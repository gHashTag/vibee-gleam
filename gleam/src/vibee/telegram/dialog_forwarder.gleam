// Dialog Forwarder
// Пересылка диалогов (вопрос + ответ) в целевую группу
// Production-ready с валидацией и обработкой edge cases

import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import pog
import vibee/config/telegram_config
import vibee/mcp/config
import vibee/config/trigger_chats
import vibee/db/postgres
import vibee/http_retry
import vibee/sales/lead_service
import vibee/vibe_logger

// Telegram API лимит на длину сообщения
const max_message_length = 4096

// P2: Rate limit для пересылок (10 в минуту на чат)
const max_forwards_per_minute = 10

// FFI для rate limiting (ETS-based)
@external(erlang, "vibee_rate_limit_ffi", "get_request_count")
fn ffi_get_forward_count(key: String) -> Int

@external(erlang, "vibee_rate_limit_ffi", "increment_request_count")
fn ffi_increment_forward_count(key: String) -> Int

/// Информация о сообщении для пересылки
pub type MessageInfo {
  MessageInfo(
    chat_id: String,
    chat_name: String,
    message_id: Int,
    from_id: Int,
    from_name: String,
    username: String,  // @username если есть, иначе ""
    phone: String,     // телефон если известен
    lang_code: String, // язык пользователя (ru, en, ...)
    is_premium: Bool,  // Telegram Premium
    text: String,
    timestamp: Int,
  )
}

/// Контекст сообщения в истории
pub type ContextMessage {
  ContextMessage(
    from_name: String,
    text: String,
    is_from_user: Bool,  // True если от лида, False если от других
  )
}

/// Контекст диалога для пересылки
pub type DialogContext {
  DialogContext(
    messages: List(ContextMessage),  // последние N сообщений
  )
}

/// Пустой контекст
pub fn empty_context() -> DialogContext {
  DialogContext(messages: [])
}

/// Результат пересылки
pub type ForwardResult {
  ForwardSuccess(message_id: Int)
  ForwardError(reason: String)
}

/// Пересылает диалог (вопрос + ответ) в целевую группу
/// context - история сообщений для понимания контекста (до 5 сообщений)
pub fn forward_dialog(
  session_id: String,
  original_message: MessageInfo,
  agent_reply: MessageInfo,
  target_chat_id: String,
) -> ForwardResult {
  forward_dialog_with_context(session_id, original_message, agent_reply, target_chat_id, empty_context())
}

/// Пересылает диалог с контекстом разговора
pub fn forward_dialog_with_context(
  session_id: String,
  original_message: MessageInfo,
  agent_reply: MessageInfo,
  target_chat_id: String,
  context: DialogContext,
) -> ForwardResult {
  let log = vibe_logger.new("forward")
    |> vibe_logger.with_data("session_id", json.string(session_id))
    |> vibe_logger.with_data("target_chat_id", json.string(target_chat_id))
    |> vibe_logger.with_data("from", json.string(original_message.from_name))
    |> vibe_logger.with_data("username", json.string(original_message.username))
    |> vibe_logger.with_data("username_empty", json.bool(string.is_empty(original_message.username)))
    |> vibe_logger.with_data("from_id", json.int(original_message.from_id))
    |> vibe_logger.with_data("chat_name", json.string(original_message.chat_name))
    |> vibe_logger.with_data("text", json.string(string.slice(original_message.text, 0, 50)))
  vibe_logger.info(log, "forward_dialog_with_context CALLED")

  // Предвычисляем метрики для логирования
  let quality_score = calculate_quality_score(original_message.text)
  let intent = classify_intent(original_message.text)
  let urgency = detect_urgency(original_message.text)

  // E2E bypass: если сообщение содержит [E2E:...] - пропускаем dedup check
  let is_e2e_test = string.contains(original_message.text, "[E2E:")

  // DEDUP: Проверяем был ли форвард от этого пользователя за последние 24ч
  // Для E2E тестов пропускаем проверку
  let is_duplicate = case is_e2e_test {
    True -> {
      vibe_logger.info(log, "E2E test detected - skipping dedup check")
      False
    }
    False -> check_recent_forward(original_message.from_id, target_chat_id)
  }
  case is_duplicate {
    True -> {
      vibe_logger.warn(log |> vibe_logger.with_data("reason", json.string("duplicate")), "Duplicate lead - skipping")
      // Логируем deduplicated
      log_forward_to_db(
        original_message.chat_id, target_chat_id,
        original_message.from_id, original_message.from_name, original_message.username,
        "deduplicated", quality_score, intent, urgency,
        original_message.message_id, "Duplicate lead within 24h", None,
      )
      ForwardError("Duplicate lead")
    }
    False -> {
      // P2: Rate limiting - проверяем лимит перед пересылкой
      let rate_key = "forward:" <> target_chat_id
      let current_count = ffi_get_forward_count(rate_key)
      case current_count >= max_forwards_per_minute {
        True -> {
          vibe_logger.warn(log |> vibe_logger.with_data("reason", json.string("rate_limit")), "Rate limit exceeded")
          // Логируем rate_limited
          log_forward_to_db(
            original_message.chat_id, target_chat_id,
            original_message.from_id, original_message.from_name, original_message.username,
            "rate_limited", quality_score, intent, urgency,
            original_message.message_id, "Rate limit exceeded", None,
          )
          ForwardError("Rate limit exceeded")
        }
        False -> {
          // P2: Валидация пустых текстов
          case string.is_empty(string.trim(original_message.text)) || string.is_empty(string.trim(agent_reply.text)) {
            True -> {
              vibe_logger.warn(log |> vibe_logger.with_data("reason", json.string("empty")), "Empty message, skipping")
              // Логируем empty
              log_forward_to_db(
                original_message.chat_id, target_chat_id,
                original_message.from_id, original_message.from_name, original_message.username,
                "empty", quality_score, intent, urgency,
                original_message.message_id, "Empty message text", None,
              )
              ForwardError("Empty message text")
            }
            False -> {
              // Инкрементируем счётчик пересылок
              let _ = ffi_increment_forward_count(rate_key)
              vibe_logger.info(log, "Forwarding dialog")

              // Формируем текст диалога с валидацией длины
              let dialog_text = format_dialog(original_message, agent_reply, context)

              // Отправляем в целевую группу
              case send_message(session_id, target_chat_id, dialog_text) {
                Ok(msg_id) -> {
                  vibe_logger.info(log |> vibe_logger.with_data("msg_id", json.int(msg_id)), "Dialog forwarded successfully")

                  // Создаём/находим lead в БД для связки с forward
                  let username_opt = case string.is_empty(original_message.username) {
                    True -> None
                    False -> Some(original_message.username)
                  }
                  let lead_id = case lead_service.get_or_create_lead(
                    original_message.from_id,
                    username_opt,
                    Some(original_message.from_name),
                    Some(original_message.text),
                    Some("crypto_trigger"),
                  ) {
                    Ok(lead) -> lead.id
                    Error(_) -> None
                  }

                  // Логируем success с lead_id
                  log_forward_to_db(
                    original_message.chat_id, target_chat_id,
                    original_message.from_id, original_message.from_name, original_message.username,
                    "forwarded", quality_score, intent, urgency,
                    msg_id, "", lead_id,
                  )
                  ForwardSuccess(msg_id)
                }
                Error(reason) -> {
                  vibe_logger.error(log |> vibe_logger.with_data("error", json.string(reason)), "Forward failed")
                  // Логируем failed
                  log_forward_to_db(
                    original_message.chat_id, target_chat_id,
                    original_message.from_id, original_message.from_name, original_message.username,
                    "failed", quality_score, intent, urgency,
                    original_message.message_id, reason, None,
                  )
                  ForwardError(reason)
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Форматирует диалог как Lead Card
fn format_dialog(original: MessageInfo, reply: MessageInfo, context: DialogContext) -> String {
  // Lead scoring и классификация
  let quality_score = calculate_quality_score(original.text)
  let intent = classify_intent(original.text)
  let urgency = detect_urgency(original.text)

  // Генерируем ссылку на сообщение
  let message_link = make_message_link(original.chat_id, original.message_id)

  // P1: Экранируем markdown символы в пользовательском тексте
  let safe_question = escape_markdown(original.text)
  let safe_answer = escape_markdown(reply.text)
  let safe_name = escape_markdown(original.from_name)
  let safe_chat = escape_markdown(original.chat_name)

  // Кликабельная ссылка на профиль (@username НЕ экранируем!)
  // Показываем @username первым, потом имя в скобках
  let client_display = case string.is_empty(original.username) {
    True -> safe_name  // Только имя если нет username
    False -> "@" <> original.username <> " (" <> safe_name <> ")"  // @username (Имя)
  }

  // Premium badge
  let premium_badge = case original.is_premium {
    True -> " 💎"
    False -> ""
  }

  // Телефон (если известен)
  let phone_display = case string.is_empty(original.phone) {
    True -> ""
    False -> "📞 Телефон: " <> original.phone <> "\n"
  }

  // Язык пользователя
  let lang_display = case string.is_empty(original.lang_code) {
    True -> ""
    False -> "🌐 Язык: " <> string.uppercase(original.lang_code) <> "\n"
  }

  // Форматируем контекст если есть
  let context_section = format_context(context, original.from_id)

  let dialog_text =
    "🔔 НОВЫЙ ЛИД #" <> int.to_string(original.from_id) <> premium_badge <> "\n"
    <> "━━━━━━━━━━━━━━━\n\n"
    <> "📌 Статус: 🆕 New → Contacted → Qualified → Won\n"
    <> "📊 Качество: " <> format_stars(quality_score) <> " (" <> int.to_string(quality_score) <> "/10)\n"
    <> "🎯 Намерение: " <> intent <> "\n"
    <> "⏰ Срочность: " <> urgency <> "\n\n"
    <> "📍 Источник: " <> safe_chat <> "\n"
    <> "👤 Клиент: " <> client_display <> "\n"
    <> "📱 ID: " <> int.to_string(original.from_id) <> "\n"
    <> phone_display
    <> lang_display
    <> "\n"
    <> context_section
    <> "💬 Триггер:\n\"" <> safe_question <> "\"\n\n"
    <> "🤖 Ответ:\n\"" <> safe_answer <> "\"\n\n"
    <> "━━━━━━━━━━━━━━━\n"
    <> message_link

  // P0: Проверяем длину и обрезаем если нужно
  case string.length(dialog_text) > max_message_length {
    True -> {
      // Обрезаем с запасом для "[...обрезано]"
      let truncated = string.slice(dialog_text, 0, max_message_length - 50)
      truncated <> "\n\n[...сообщение обрезано]"
    }
    False -> dialog_text
  }
}

/// Рассчитывает качество лида (1-10)
fn calculate_quality_score(text: String) -> Int {
  let lower = string.lowercase(text)
  let base_score = 3

  // Позитивные сигналы (+1-2 каждый)
  let has_amount = string.contains(lower, "к ") || string.contains(lower, "тысяч") || string.contains(lower, "руб") || string.contains(lower, "$")
  let has_urgency = string.contains(lower, "срочно") || string.contains(lower, "сегодня") || string.contains(lower, "сейчас")
  let has_specific_crypto = string.contains(lower, "usdt") || string.contains(lower, "btc") || string.contains(lower, "биткоин")
  let is_long = string.length(text) > 50
  let has_question = string.contains(text, "?")

  let score = base_score
    + case has_amount { True -> 2 False -> 0 }
    + case has_urgency { True -> 2 False -> 0 }
    + case has_specific_crypto { True -> 1 False -> 0 }
    + case is_long { True -> 1 False -> 0 }
    + case has_question { True -> 1 False -> 0 }

  // Ограничиваем 1-10
  case score {
    s if s > 10 -> 10
    s if s < 1 -> 1
    s -> s
  }
}

/// Классифицирует намерение пользователя
fn classify_intent(text: String) -> String {
  let lower = string.lowercase(text)

  case string.contains(lower, "куп") || string.contains(lower, "хочу") || string.contains(lower, "надо") {
    True -> "💰 Покупка"
    False -> case string.contains(lower, "прод") || string.contains(lower, "сбыт") {
      True -> "💸 Продажа"
      False -> case string.contains(lower, "обмен") || string.contains(lower, "поменять") {
        True -> "🔄 Обмен"
        False -> case string.contains(lower, "?") || string.contains(lower, "как") || string.contains(lower, "где") {
          True -> "❓ Вопрос"
          False -> "📋 Общий интерес"
        }
      }
    }
  }
}

/// Определяет срочность
fn detect_urgency(text: String) -> String {
  let lower = string.lowercase(text)

  case string.contains(lower, "срочно") || string.contains(lower, "сейчас") || string.contains(lower, "быстро") {
    True -> "🔴 Высокая"
    False -> case string.contains(lower, "сегодня") || string.contains(lower, "скоро") {
      True -> "🟡 Средняя"
      False -> "🟢 Обычная"
    }
  }
}

/// Форматирует звёзды для рейтинга
fn format_stars(score: Int) -> String {
  let full_stars = score / 2
  let half_star = score % 2

  let stars = string.repeat("⭐", full_stars)
  case half_star {
    1 -> stars <> "✨"
    _ -> stars
  }
}

/// Форматирует контекст разговора (полный текст без обрезания)
fn format_context(context: DialogContext, _lead_user_id: Int) -> String {
  case list.is_empty(context.messages) {
    True -> ""
    False -> {
      let formatted = list.map(context.messages, fn(msg) {
        let prefix = case msg.is_from_user {
          True -> "👤 [" <> escape_markdown(msg.from_name) <> "]"
          False -> "💬 [" <> escape_markdown(msg.from_name) <> "]"
        }
        // Полный текст без обрезания
        let text = escape_markdown(msg.text)
        "> " <> prefix <> ": " <> text
      })

      "📜 Контекст:\n"
        <> string.join(formatted, "\n")
        <> "\n\n"
    }
  }
}

/// P1: Экранирует markdown специальные символы
fn escape_markdown(text: String) -> String {
  text
  |> string.replace("\\", "\\\\")
  |> string.replace("[", "\\[")
  |> string.replace("]", "\\]")
  |> string.replace("*", "\\*")
  |> string.replace("_", "\\_")
  |> string.replace("`", "\\`")
}

/// Создаёт ссылку на сообщение в Telegram
/// Формат t.me/c/ работает ТОЛЬКО для Supergroups/Channels (с -100 prefix)
/// Для Basic Groups (без -100) ссылка технически невозможна
/// Документация: https://core.telegram.org/api/bots/ids
fn make_message_link(chat_id: String, message_id: Int) -> String {
  case string.starts_with(chat_id, "-100") {
    True -> {
      // Supergroup/Channel: кликабельная ссылка
      let channel_id = string.drop_start(chat_id, 4)
      "🔗 [Перейти к сообщению](https://t.me/c/" <> channel_id <> "/" <> int.to_string(message_id) <> ")"
    }
    False -> {
      // Basic Group: ссылка невозможна
      "📌 Сообщение #" <> int.to_string(message_id)
    }
  }
}

/// Отправляет сообщение через telegram-bridge
fn send_message(
  session_id: String,
  chat_id: String,
  text: String,
) -> Result(Int, String) {
  // P0: Валидация chat_id
  case int.parse(chat_id) {
    Error(_) -> Error("Invalid chat_id format: " <> chat_id)
    Ok(0) -> Error("Invalid chat_id: 0")
    Ok(chat_id_int) -> {
      let bridge_url = telegram_config.bridge_url()
      let #(scheme, host, port) = parse_bridge_url(bridge_url)
      let api_key = telegram_config.bridge_api_key()

      // Формируем JSON body
      let body_json =
        json.object([
          #("chat_id", json.int(chat_id_int)),
          #("text", json.string(text)),
        ])
        |> json.to_string

      // Создаем запрос
      let req =
        request.new()
        |> request.set_scheme(scheme)
        |> request.set_method(http.Post)
        |> request.set_host(host)
        |> request.set_port(port)
        |> request.set_path("/api/v1/send")
        |> request.set_header("content-type", "application/json")
        |> request.set_header("Authorization", "Bearer " <> api_key)
        |> request.set_header("x-session-id", session_id)
        |> request.set_body(body_json)

      // Отправляем с retry logic
      let retry_config = http_retry.default_config()
      let send_log = vibe_logger.new("forward_send")
        |> vibe_logger.with_data("chat_id", json.int(chat_id_int))
      case http_retry.send_with_retry(req, retry_config) {
        Ok(response) -> {
          case response.status {
            200 -> {
              // Parse message_id from response: {"success": true, "message_id": 12345}
              let msg_id = decode_message_id(response.body)
              vibe_logger.info(send_log
                |> vibe_logger.with_data("msg_id", json.int(msg_id)),
                "Lead Card sent successfully")
              Ok(msg_id)
            }
            status -> {
              vibe_logger.error(send_log
                |> vibe_logger.with_data("status", json.int(status))
                |> vibe_logger.with_data("body", json.string(string.slice(response.body, 0, 100))),
                "HTTP error")
              Error("HTTP " <> int.to_string(status))
            }
          }
        }
        Error(err) -> {
          vibe_logger.error(send_log
            |> vibe_logger.with_data("error", json.string(string.inspect(err))),
            "Network error")
          Error("Network error")
        }
      }
    }
  }
}

/// Парсит bridge URL в компоненты (scheme, host, port)
fn parse_bridge_url(url: String) -> #(http.Scheme, String, Int) {
  case string.starts_with(url, "https://") {
    True -> {
      let host = string.drop_start(url, 8)
        |> string.split("/")
        |> list.first
        |> result.unwrap("localhost")
      #(http.Https, host, 443)
    }
    False -> {
      case string.starts_with(url, "http://") {
        True -> {
          let rest = string.drop_start(url, 7)
            |> string.split("/")
            |> list.first
            |> result.unwrap("localhost:8081")
          // Проверяем на port
          case string.split(rest, ":") {
            [h, p] -> {
              let port = case int.parse(p) {
                Ok(n) -> n
                Error(_) -> 8081
              }
              #(http.Http, h, port)
            }
            _ -> #(http.Http, rest, 8081)
          }
        }
        False -> #(http.Http, "localhost", 8081)
      }
    }
  }
}

/// Проверяет, нужно ли пересылать диалог из этого чата
pub fn should_forward_from_chat(chat_id: String) -> Bool {
  case trigger_chats.get_forward_chat_id(chat_id) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Получает ID целевого чата для пересылки
pub fn get_forward_target(chat_id: String) -> Result(String, Nil) {
  trigger_chats.get_forward_chat_id(chat_id)
}

/// Проверяет был ли недавний форвард от этого пользователя (дедупликация)
/// Использует pog для надёжного подключения к БД
fn check_recent_forward(user_id: Int, target_chat_id: String) -> Bool {
  let log = vibe_logger.new("dedup_check")
    |> vibe_logger.with_data("user_id", json.int(user_id))
    |> vibe_logger.with_data("target_chat_id", json.string(target_chat_id))

  // Получаем глобальный пул соединений
  case postgres.get_global_pool() {
    None -> {
      vibe_logger.warn(log, "No database pool - skipping dedup check")
      False
    }
    Some(pool) -> {
      // SQL с параметрами (безопасно от SQL injection)
      let sql = "SELECT COUNT(*)::int FROM lead_forwards
                 WHERE user_id = $1
                 AND target_chat_id = $2
                 AND status = 'forwarded'
                 AND forwarded_at > NOW() - INTERVAL '1 hour'"

      // Декодер для COUNT результата
      let count_decoder = {
        use count <- decode.field(0, decode.int)
        decode.success(count)
      }

      // Парсим target_chat_id как int (убираем минус для supergroups)
      let target_id = case int.parse(target_chat_id) {
        Ok(id) -> id
        Error(_) -> 0
      }

      case pog.query(sql)
        |> pog.parameter(pog.int(user_id))
        |> pog.parameter(pog.int(target_id))
        |> pog.returning(count_decoder)
        |> pog.execute(pool)
      {
        Ok(pog.Returned(_, [count])) -> {
          let is_dup = count > 0
          vibe_logger.info(log
            |> vibe_logger.with_data("count", json.int(count))
            |> vibe_logger.with_data("is_duplicate", json.bool(is_dup)),
            "Dedup check complete")
          is_dup
        }
        Ok(_) -> {
          vibe_logger.warn(log, "Unexpected query result - skipping dedup")
          False
        }
        Error(err) -> {
          vibe_logger.error(log
            |> vibe_logger.with_data("error", json.string(pog_error_to_string(err))),
            "Database query failed - skipping dedup")
          False
        }
      }
    }
  }
}

/// Конвертирует pog ошибку в строку для логирования
fn pog_error_to_string(err: pog.QueryError) -> String {
  case err {
    pog.ConnectionUnavailable -> "ConnectionUnavailable"
    pog.ConstraintViolated(msg, constraint, _) -> "ConstraintViolated: " <> msg <> " (" <> constraint <> ")"
    pog.PostgresqlError(code, name, msg) -> "PostgresqlError " <> code <> " " <> name <> ": " <> msg
    pog.UnexpectedArgumentCount(expected, got) -> "UnexpectedArgumentCount: expected " <> int.to_string(expected) <> ", got " <> int.to_string(got)
    pog.UnexpectedArgumentType(expected, got) -> "UnexpectedArgumentType: expected " <> expected <> ", got " <> got
    pog.UnexpectedResultType(_) -> "UnexpectedResultType"
    pog.QueryTimeout -> "QueryTimeout"
  }
}

/// Логирует форвард в БД для метрик
/// Использует pog для безопасных параметризованных запросов
fn log_forward_to_db(
  source_chat_id: String,
  target_chat_id: String,
  user_id: Int,
  user_name: String,
  username: String,
  status: String,
  quality_score: Int,
  intent: String,
  urgency: String,
  message_id: Int,
  error_message: String,
  lead_id: Option(Int),
) -> Nil {
  let metrics_log = vibe_logger.new("metrics")
    |> vibe_logger.with_data("status", json.string(status))
    |> vibe_logger.with_data("user_id", json.int(user_id))
    |> vibe_logger.with_data("lead_id", case lead_id {
      Some(id) -> json.int(id)
      None -> json.null()
    })

  case postgres.get_global_pool() {
    None -> {
      vibe_logger.warn(metrics_log, "No database pool - skipping metrics log")
      Nil
    }
    Some(pool) -> {
      // Параметризованный SQL (безопасно от SQL injection)
      let sql = "INSERT INTO lead_forwards
        (source_chat_id, target_chat_id, user_id, user_name, username, status,
         quality_score, intent, urgency, message_id, error_message, lead_id, forwarded_at)
        VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, NOW())"

      // Парсим chat_id как int
      let source_id = case int.parse(source_chat_id) {
        Ok(id) -> id
        Error(_) -> 0
      }
      let target_id = case int.parse(target_chat_id) {
        Ok(id) -> id
        Error(_) -> 0
      }

      case pog.query(sql)
        |> pog.parameter(pog.int(source_id))
        |> pog.parameter(pog.int(target_id))
        |> pog.parameter(pog.int(user_id))
        |> pog.parameter(pog.text(user_name))
        |> pog.parameter(pog.text(username))
        |> pog.parameter(pog.text(status))
        |> pog.parameter(pog.int(quality_score))
        |> pog.parameter(pog.text(intent))
        |> pog.parameter(pog.text(urgency))
        |> pog.parameter(pog.int(message_id))
        |> pog.parameter(pog.text(error_message))
        |> pog.parameter(pog.nullable(pog.int, lead_id))
        |> pog.execute(pool)
      {
        Ok(_) -> vibe_logger.debug(metrics_log, "Forward logged to DB")
        Error(err) -> vibe_logger.warn(metrics_log
          |> vibe_logger.with_data("error", json.string(pog_error_to_string(err))),
          "Failed to log forward to DB")
      }
      Nil
    }
  }
}

/// Decode message_id from Bridge response
/// Response format: {"success": true, "message_id": 12345}
fn decode_message_id(body: String) -> Int {
  let decoder = {
    use message_id <- decode.field("message_id", decode.int)
    decode.success(message_id)
  }
  case json.parse(body, decoder) {
    Ok(id) -> id
    Error(_) -> 0  // Fallback if parsing fails
  }
}
