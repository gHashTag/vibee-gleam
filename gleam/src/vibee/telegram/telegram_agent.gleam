// VIBEE Telegram Agent
// Аналог TelegramService из plugin-telegram-craft
// Работает через Go bridge для MTProto

import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import shellout
import vibee/mcp/config
import vibee/config/target_chats
import vibee/config/telegram_config
import vibee/db/postgres
import vibee/logging
import vibee/mcp/session_manager

/// Конфигурация Telegram агента
pub type TelegramAgentConfig {
  TelegramAgentConfig(
    bridge_url: String,
    session_id: String,
    llm_api_key: Option(String),
    llm_model: String,
    auto_reply_enabled: Bool,
    cooldown_ms: Int,
    // Digital Twin mode settings
    digital_twin_enabled: Bool,
    owner_id: Int,
    // 0 = respond to all personal chats
  )
}

/// Digital Twin режим - отвечает на ВСЕ личные сообщения от имени пользователя
pub type DigitalTwinMode {
  TwinDisabled
  TwinAllPersonalChats
  TwinWhitelistOnly(List(Int))
}

/// Состояние агента
pub type AgentState {
  AgentState(
    config: TelegramAgentConfig,
    is_monitoring: Bool,
    total_messages: Int,
    last_reply_time: Int,
    monitored_chats: List(String),
  )
}

/// Сообщения для актора
pub type AgentMessage {
  StartMonitoring
  StopMonitoring
  ProcessMessage(chat_id: String, from_name: String, text: String, message_id: Int)
  SendReply(chat_id: String, text: String, reply_to: Option(Int))
  GetStatus
  Shutdown
}

/// Результат отправки сообщения
pub type SendResult {
  SendOk(message_id: Int)
  SendError(reason: String)
}

/// Создать конфигурацию по умолчанию (используем централизованный конфиг)
pub fn default_config() -> TelegramAgentConfig {
  // Get active session from session manager, fall back to empty string if none
  let session_id = case session_manager.get_active() {
    Some(sid) -> sid
    None -> ""
  }
  TelegramAgentConfig(
    bridge_url: telegram_config.bridge_url(),
    session_id: session_id,
    llm_api_key: None,
    llm_model: "x-ai/grok-4.1-fast",
    auto_reply_enabled: True,
    cooldown_ms: 30_000,
    // Digital Twin mode - enabled by default for owner @neuro_sage
    digital_twin_enabled: True,
    owner_id: 144_022_504,
    // @neuro_sage Telegram ID
  )
}

/// Конфигурация для Digital Twin режима
pub fn digital_twin_config(owner_telegram_id: Int) -> TelegramAgentConfig {
  let base = default_config()
  TelegramAgentConfig(
    ..base,
    digital_twin_enabled: True,
    owner_id: owner_telegram_id,
    auto_reply_enabled: True,
  )
}

/// Инициализация агента
pub fn init(config: TelegramAgentConfig) -> AgentState {
  logging.info("Telegram Agent initialized")
  logging.info("Bridge URL: " <> config.bridge_url)
  logging.info("Auto-reply: " <> case config.auto_reply_enabled {
    True -> "enabled"
    False -> "disabled"
  })

  AgentState(
    config: config,
    is_monitoring: False,
    total_messages: 0,
    last_reply_time: 0,
    monitored_chats: target_chats.target_chats,
  )
}

// Bot's user ID - skip messages from this user to prevent loops
const bot_user_id = 6_579_515_876

/// Обработка входящего сообщения
pub fn handle_incoming_message(
  state: AgentState,
  chat_id: String,
  from_id: Int,
  from_name: String,
  text: String,
  message_id: Int,
) -> AgentState {
  // Логируем в stdout для Fly.io видимости
  io.println("[MSG] chat=" <> chat_id <> " from_id=" <> int.to_string(from_id) <> " from=" <> from_name <> " text=" <> string.slice(text, 0, 50))

  // Не отвечаем на собственные сообщения (по user_id или owner_id, предотвращаем бесконечный цикл)
  case from_id == bot_user_id || from_id == state.config.owner_id {
    True -> {
      io.println("[MSG] Skipping own message from user_id: " <> int.to_string(from_id))
      state
    }
    False -> {
      // Сначала проверяем команды (работают везде, включая личные чаты)
      case parse_command(text) {
        Some(#("neurophoto", prompt)) -> {
          io.println("[CMD] /neurophoto detected! Prompt: " <> prompt)
          handle_neurophoto_command(state, chat_id, message_id, prompt)
        }
        Some(#("neuro", prompt)) -> {
          io.println("[CMD] /neuro detected! Prompt: " <> prompt)
          handle_neurophoto_command(state, chat_id, message_id, prompt)
        }
        Some(#("start", _)) -> {
          io.println("[CMD] /start detected!")
          let welcome = "Privet! Ya VIBEE - AI agent dlya generacii izobrazhenij.\n\nKomandy:\n/neurophoto <prompt> - generaciya izobrazheniya\n/neuro <prompt> - korotkaya versiya\n\nPrimer: /neurophoto cyberpunk portrait neon lights"
          let _ = send_message(state.config, chat_id, welcome, Some(message_id))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
        Some(#("help", _)) -> {
          io.println("[CMD] /help detected!")
          let help_text = "VIBEE Bot - Komandy:\n\n/neurophoto <prompt> - Generaciya izobrazheniya s FLUX LoRA\n/neuro <prompt> - Korotkaya versiya\n\nTrigger slovo NEURO_SAGE dobavlyaetsya avtomaticheski."
          let _ = send_message(state.config, chat_id, help_text, Some(message_id))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
        _ -> {
          // Digital Twin Mode - отвечаем на ВСЕ сообщения (личные + группы)
          case state.config.digital_twin_enabled {
            True -> {
              // Digital Twin отвечает на ВСЕ сообщения
              io.println("[DIGITAL_TWIN] Responding to message in chat " <> chat_id)
              process_with_digital_twin(state, chat_id, message_id, text, from_name)
            }
            False -> {
              // Обычный режим - проверяем target_chats и триггеры
              handle_normal_mode(state, chat_id, message_id, text)
            }
          }
        }
      }
    }
  }
}

/// Обработка группового сообщения (проверка триггеров)
fn handle_group_message(state: AgentState, chat_id: String, message_id: Int, text: String) -> AgentState {
  case target_chats.should_process_chat(chat_id) {
    False -> {
      io.println("[MSG] Skipping non-target group: " <> chat_id)
      state
    }
    True -> {
      case should_reply(state, text) {
        False -> {
          io.println("[MSG] No trigger in group: " <> string.slice(text, 0, 30))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
        True -> {
          io.println("[TRIGGER] Found in group! Generating reply")
          process_with_llm(state, chat_id, message_id, text)
        }
      }
    }
  }
}

/// Обработка в обычном режиме (без Digital Twin)
fn handle_normal_mode(state: AgentState, chat_id: String, message_id: Int, text: String) -> AgentState {
  case target_chats.should_process_chat(chat_id) {
    False -> {
      case int.parse(chat_id) {
        Ok(n) if n > 0 -> {
          io.println("[MSG] Personal chat, processing without triggers")
          process_with_llm(state, chat_id, message_id, text)
        }
        _ -> {
          io.println("[MSG] Skipping non-target chat: " <> chat_id)
          state
        }
      }
    }
    True -> {
      io.println("[MSG] Processing target chat: " <> chat_id)
      case should_reply(state, text) {
        False -> {
          io.println("[MSG] No trigger found in: " <> string.slice(text, 0, 30))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
        True -> {
          io.println("[TRIGGER] Found! Generating reply for: " <> string.slice(text, 0, 30))
          process_with_llm(state, chat_id, message_id, text)
        }
      }
    }
  }
}

/// Digital Twin обработка - отвечает в стиле владельца аккаунта
fn process_with_digital_twin(state: AgentState, chat_id: String, message_id: Int, text: String, from_name: String) -> AgentState {
  io.println("[TWIN] Processing message from " <> from_name <> " in chat " <> chat_id)
  case generate_digital_twin_reply(state.config, text, from_name, chat_id) {
    Ok(reply) -> {
      io.println("[TWIN] Generated reply: " <> string.slice(reply, 0, 80) <> "...")
      case send_message(state.config, chat_id, reply, Some(message_id)) {
        Ok(msg_id) -> {
          io.println("[TWIN] Message sent OK, id=" <> int.to_string(msg_id))
          // Сохраняем ответ Digital Twin в БД для RAG памяти
          let dialog_id = case int.parse(chat_id) {
            Ok(id) -> id
            Error(_) -> 0
          }
          io.println("[DB] Saving reply to dialog=" <> int.to_string(dialog_id) <> " msg_id=" <> int.to_string(msg_id))
          case postgres.insert_message_simple(dialog_id, msg_id, state.config.owner_id, "Я", reply) {
            Ok(_) -> io.println("[DB] Reply saved OK")
            Error(e) -> io.println("[DB] ERROR saving reply: " <> e)
          }
          AgentState(..state, total_messages: state.total_messages + 1)
        }
        Error(send_err) -> {
          io.println("[TWIN] SEND FAILED: " <> send_err)
          AgentState(..state, total_messages: state.total_messages + 1)
        }
      }
    }
    Error(err) -> {
      io.println("[TWIN] GENERATE FAILED: " <> err)
      state
    }
  }
}

// ============================================================
// Command Parsing and Handlers
// ============================================================

/// Парсит команду из текста сообщения
/// Возвращает Some(#(command, args)) или None
fn parse_command(text: String) -> Option(#(String, String)) {
  let trimmed = string.trim(text)
  case string.starts_with(trimmed, "/") {
    False -> None
    True -> {
      let without_slash = string.drop_start(trimmed, 1)
      case string.split(without_slash, " ") {
        [] -> None
        [cmd] -> Some(#(string.lowercase(cmd), ""))
        [cmd, ..rest] -> Some(#(string.lowercase(cmd), string.join(rest, " ")))
      }
    }
  }
}

/// Обрабатывает команду /neurophoto с FAL.ai
fn handle_neurophoto_command(
  state: AgentState,
  chat_id: String,
  message_id: Int,
  prompt: String,
) -> AgentState {
  case prompt {
    "" -> {
      // Пустой промпт - показываем подсказку
      let hint = "Ukazhite prompt dlya generacii!\n\nPrimer: /neurophoto cyberpunk portrait, neon lights, cinematic"
      let _ = send_message(state.config, chat_id, hint, Some(message_id))
      AgentState(..state, total_messages: state.total_messages + 1)
    }
    _ -> {
      // Отправляем сообщение "генерируем..."
      let _ = send_message(state.config, chat_id, "Generiruyiu izobrazhenie s NEURO_SAGE...\n\nPrompt: " <> prompt, Some(message_id))

      // Вызываем FAL.ai
      case generate_image_fal(prompt) {
        Ok(image_url) -> {
          io.println("[NEUROPHOTO] Generated: " <> image_url)
          // Отправляем изображение
          let _ = send_photo(state.config, chat_id, image_url, Some("Generated: " <> prompt))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
        Error(err) -> {
          io.println("[NEUROPHOTO ERROR] " <> err)
          let _ = send_message(state.config, chat_id, "Oshibka generacii: " <> err, Some(message_id))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
      }
    }
  }
}

/// Генерация изображения через FAL.ai FLUX LoRA
fn generate_image_fal(prompt: String) -> Result(String, String) {
  // Получаем API ключ из окружения
  let api_key = get_env("FAL_API_KEY")
  case api_key {
    "" -> Error("FAL_API_KEY not configured")
    key -> {
      // Получаем LoRA URL из окружения (или дефолт)
      let lora_url = case get_env("DEFAULT_LORA_URL") {
        "" -> "https://v3b.fal.media/files/b/elephant/YpfnIK7JlNO7vZTsGanfo_pytorch_lora_weights.safetensors"
        url -> url
      }

      // Добавляем триггер NEURO_SAGE
      let full_prompt = "NEURO_SAGE " <> prompt

      // Формируем JSON тело запроса
      let body = json.object([
        #("prompt", json.string(full_prompt)),
        #("loras", json.array([
          json.object([
            #("path", json.string(lora_url)),
            #("scale", json.float(1.0)),
          ]),
        ], fn(x) { x })),
        #("num_images", json.int(1)),
        #("image_size", json.object([
          #("width", json.int(768)),
          #("height", json.int(1365)),
        ])),
        #("enable_safety_checker", json.bool(True)),
        #("output_format", json.string("jpeg")),
        #("sync_mode", json.bool(True)),
        #("guidance_scale", json.float(3.5)),
        #("num_inference_steps", json.int(28)),
      ])
      |> json.to_string()

      io.println("[FAL] Calling FAL.ai with prompt: " <> string.slice(full_prompt, 0, 50))

      let req = request.new()
        |> request.set_scheme(http.Https)
        |> request.set_method(http.Post)
        |> request.set_host("queue.fal.run")
        |> request.set_path("/fal-ai/flux-lora")
        |> request.set_header("Authorization", "Key " <> key)
        |> request.set_header("Content-Type", "application/json")
        |> request.set_body(body)

      case httpc.send(req) {
        Ok(response) -> {
          io.println("[FAL] Response status: " <> int.to_string(response.status))
          case response.status {
            200 -> {
              // Парсим ответ и извлекаем URL изображения
              extract_image_url(response.body)
            }
            _ -> Error("FAL.ai HTTP " <> int.to_string(response.status) <> ": " <> string.slice(response.body, 0, 100))
          }
        }
        Error(_) -> Error("HTTP request to FAL.ai failed")
      }
    }
  }
}

/// Извлекает URL изображения из ответа FAL.ai
fn extract_image_url(body: String) -> Result(String, String) {
  // Формат: {"images":[{"url":"https://..."}],...}
  let pattern = "\"url\":\""
  case string.split(body, pattern) {
    [_, rest, ..] -> {
      case string.split(rest, "\"") {
        [url, ..] -> Ok(url)
        _ -> Error("Could not parse image URL")
      }
    }
    _ -> {
      // Проверяем на IN_QUEUE статус
      case string.contains(body, "IN_QUEUE") {
        True -> Error("Request queued - try again in 30 seconds")
        False -> Error("No image URL in response: " <> string.slice(body, 0, 200))
      }
    }
  }
}

/// Получает переменную окружения
/// Использует FFI wrapper для конвертации binary -> charlist -> os:getenv -> binary
@external(erlang, "vibee_ffi", "get_env")
fn get_env(name: String) -> String

/// Обработка через LLM
fn process_with_llm(state: AgentState, chat_id: String, message_id: Int, text: String) -> AgentState {
  case generate_reply(state.config, text) {
    Ok(reply) -> {
      io.println("[LLM] Reply: " <> string.slice(reply, 0, 50) <> "...")
      let _ = send_message(state.config, chat_id, reply, Some(message_id))
      AgentState(..state, total_messages: state.total_messages + 1)
    }
    Error(err) -> {
      io.println("[LLM ERROR] Failed to generate reply: " <> err)
      AgentState(..state, total_messages: state.total_messages + 1)
    }
  }
}

/// Проверяет, нужно ли отвечать на сообщение
fn should_reply(state: AgentState, text: String) -> Bool {
  case state.config.auto_reply_enabled {
    False -> False
    True -> {
      let lower_text = string.lowercase(text)
      // Проверяем триггеры
      let triggers = ["vibee", "vibe", "@vibee", "бот", "агент", "вайб"]
      list.any(triggers, fn(trigger) {
        string.contains(lower_text, trigger)
      })
    }
  }
}

/// Генерация ответа через LLM (OpenRouter)
fn generate_reply(config: TelegramAgentConfig, user_message: String) -> Result(String, String) {
  case config.llm_api_key {
    None -> {
      // Без LLM - простой fallback ответ
      Ok("Privet! Ya VIBEE agent na Gleam/BEAM. Polnyj LLM otvet budet dostupem posle nastrojki OPENROUTER_API_KEY.")
    }
    Some(api_key) -> {
      // Вызов OpenRouter API
      call_openrouter(api_key, config.llm_model, user_message)
    }
  }
}

/// Генерация ответа Digital Twin - в стиле владельца аккаунта
/// Использует улучшенный промпт с few-shot примерами и RAG контекстом
fn generate_digital_twin_reply(
  config: TelegramAgentConfig,
  user_message: String,
  from_name: String,
  chat_id: String,
) -> Result(String, String) {
  io.println("[TWIN] Generating reply for: " <> from_name <> " in chat " <> chat_id)

  // Получаем контекст из истории (RAG) - TODO: интегрировать с conversation_get_context
  let conversation_context = get_conversation_context(chat_id, user_message)
  io.println("[TWIN] Context received, length: " <> int.to_string(string.length(conversation_context)))

  // Строим улучшенный промпт с примерами и контекстом
  let system_prompt = build_digital_twin_prompt(from_name, conversation_context)
  io.println("[TWIN] Prompt built, length: " <> int.to_string(string.length(system_prompt)))

  let api_key = case config.llm_api_key {
    Some(key) -> key
    None -> get_env("OPENROUTER_API_KEY")
  }

  case api_key {
    "" -> Error("OPENROUTER_API_KEY not configured")
    key -> call_openrouter_with_system(key, config.llm_model, system_prompt, user_message)
  }
}

/// Получает контекст из истории переписки через PostgreSQL
/// Включает: последние 50 сообщений диалога + историю с этим человеком из других чатов
fn get_conversation_context(chat_id: String, _query: String) -> String {
  let db_url = config.get_env_or("DATABASE_URL", "")
  case db_url {
    "" -> {
      logging.info("[RAG] DATABASE_URL not set")
      ""
    }
    url -> {
      // SQL для последних 50 сообщений + история с этим человеком
      // Простой запрос - только сообщения из текущего диалога
      let sql =
        "SELECT CASE WHEN sender_id = 144022504 THEN 'Я' ELSE COALESCE(sender_name, 'Собеседник') END || ': ' || LEFT(text_content, 300) as msg
         FROM telegram_messages
         WHERE dialog_id = " <> chat_id <> "
           AND text_content IS NOT NULL AND text_content != ''
         ORDER BY timestamp DESC
         LIMIT 50"

      io.println("[RAG] Getting context for chat " <> chat_id <> " via psql...")
      // First check message count in DB
      let count_sql = "SELECT COUNT(*) FROM telegram_messages WHERE dialog_id = " <> chat_id
      case shellout.command(run: "psql", with: [url, "-t", "-c", count_sql], in: ".", opt: []) {
        Ok(cnt) -> io.println("[RAG] Messages in DB for dialog " <> chat_id <> ": " <> string.trim(cnt))
        Error(_) -> io.println("[RAG] Could not count messages")
      }
      case shellout.command(run: "psql", with: [url, "-t", "-c", sql], in: ".", opt: []) {
        Ok(result) -> {
          let ctx = format_context(result)
          let ctx_len = string.length(ctx)
          io.println("[RAG] Got " <> int.to_string(ctx_len) <> " chars of context")
          // Показываем превью контекста для отладки
          case ctx_len > 0 {
            True -> io.println("[RAG] Preview: " <> string.slice(ctx, 0, 150) <> "...")
            False -> io.println("[RAG] WARNING: Context is empty! No messages saved for this chat yet")
          }
          ctx
        }
        Error(#(code, err)) -> {
          io.println("[RAG] ERROR psql failed! code=" <> int.to_string(code) <> " err=" <> err)
          ""
        }
      }
    }
  }
}

/// Форматирует сырой вывод psql в читаемый контекст
fn format_context(raw: String) -> String {
  let lines =
    string.split(raw, "\n")
    |> list.filter(fn(l) { string.trim(l) != "" })
    |> list.reverse  // от старых к новым
    |> list.map(fn(l) { "- " <> string.trim(l) })
    |> string.join("\n")

  case lines {
    "" -> ""
    ctx -> "История:\n" <> ctx
  }
}

/// Строит улучшенный Digital Twin промпт с best practices
fn build_digital_twin_prompt(from_name: String, context: String) -> String {
  let base_prompt = "Ты - цифровой двойник (Digital Twin) Дмитрия Васильева (@neuro_sage).

## ИДЕНТИЧНОСТЬ

Имя: Дмитрий (Dmitrii)
Telegram: @neuro_sage
Локация: Россия → Таиланд (digital nomad)
Профессия: Вайб-кодер, AI-инженер, основатель VIBEE

## ЛИЧНОСТЬ И ЦЕННОСТИ

- Энтузиаст AI и автономных агентов
- Верю в симбиоз человека и AI
- Практикую вайб-кодинг (программирование через диалог с AI)
- Люблю эксперименты с новыми технологиями
- Ценю эффективность и автоматизацию

## ТЕКУЩИЕ ПРОЕКТЫ

1. **VIBEE** - платформа AI-агентов на Gleam/BEAM
   - Telegram боты с AI
   - Digital Twin технология
   - Генерация видео/аватаров

2. **НейроКодер** - сообщество вайб-кодеров
   - Обучение AI-программированию
   - Claude Code, Cursor, Windsurf

## СТИЛЬ ОБЩЕНИЯ

### Характерные паттерны:
- Использую микс русского и английского
- Краткие, ёмкие ответы (1-3 предложения)
- Технические термины без упрощения
- НЕ использую emoji - пишу чисто текстом
- Вариативность: начинаю каждое сообщение по-разному

### ВАЖНО - Запреты:
- НИКОГДА не начинай сообщение с \"Йо\" или \"Йо!\" - это слишком повторяется
- Используй разные приветствия: \"Прив\", \"Здарова\", \"Оп\", или сразу по делу без приветствия

### Типичные фразы:
- \"го\" вместо \"давай\"
- \"кринж\" / \"база\"
- \"вайб\" в разных контекстах
- \"залетай\" = присоединяйся
- \"тема\" = хорошая идея/проект

### Примеры моих реальных сообщений:
<examples>
User: Как дела с проектом?
Dmitrii: Движемся! Вчера затащил Digital Twin, теперь бот отвечает за меня. Тестим на проде

User: Что думаешь про новый Claude?
Dmitrii: Opus 4.5 - база. Особенно для кодинга. Я на нём весь VIBEE пилю через Claude Code

User: Можешь помочь с кодом?
Dmitrii: Го, скинь что там. Если Gleam/Elixir - вообще тема, это мой стек
</examples>

## КОНТЕКСТ РАЗГОВОРА

Собеседник: " <> from_name <> "
" <> case context {
    "" -> "История: Нет предыдущего контекста с этим человеком"
    ctx -> "Релевантный контекст из истории:\n" <> ctx
  } <> "

## ИНСТРУКЦИИ

1. Отвечай как настоящий Дмитрий - естественно, без формальностей
2. НЕ раскрывай что ты AI/бот - веди себя как человек
3. Если не знаешь точный ответ - скажи \"уточню и отпишу\"
4. На технические вопросы отвечай компетентно
5. Поддерживай дружеский тон, но без наигранности
6. Длина ответа: 1-4 предложения (как в реальной переписке)"

  base_prompt
}

/// Вызов OpenRouter с кастомным системным промптом
fn call_openrouter_with_system(
  api_key: String,
  model: String,
  system_prompt: String,
  user_message: String,
) -> Result(String, String) {
  let body = json.object([
    #("model", json.string(model)),
    #("messages", json.array([
      json.object([
        #("role", json.string("system")),
        #("content", json.string(system_prompt)),
      ]),
      json.object([
        #("role", json.string("user")),
        #("content", json.string(user_message)),
      ]),
    ], fn(x) { x })),
  ])
  |> json.to_string()

  io.println("[DIGITAL_TWIN] Calling OpenRouter with model: " <> model)

  let req = request.new()
    |> request.set_scheme(http.Https)
    |> request.set_method(http.Post)
    |> request.set_host("openrouter.ai")
    |> request.set_path("/api/v1/chat/completions")
    |> request.set_header("Authorization", "Bearer " <> api_key)
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("HTTP-Referer", "https://vibee.ai")
    |> request.set_header("X-Title", "VIBEE Digital Twin")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) -> {
      io.println("[DIGITAL_TWIN] Response status: " <> int.to_string(response.status))
      case response.status {
        200 -> {
          case extract_content_from_response(response.body) {
            Ok(content) -> Ok(content)
            Error(err) -> {
              io.println("[DIGITAL_TWIN ERROR] Parse error: " <> err)
              Error("Parse error: " <> err)
            }
          }
        }
        status -> {
          io.println("[DIGITAL_TWIN ERROR] HTTP " <> int.to_string(status))
          Error("API error: HTTP " <> int.to_string(status))
        }
      }
    }
    Error(_) -> {
      io.println("[DIGITAL_TWIN ERROR] HTTP request failed")
      Error("HTTP request failed")
    }
  }
}

/// Вызов OpenRouter API для генерации ответа
fn call_openrouter(api_key: String, model: String, user_message: String) -> Result(String, String) {
  let system_prompt = "Ты VIBEE - дружелюбный AI-агент на Gleam/BEAM платформе. Отвечай кратко и полезно на русском языке. Ты эксперт по вайбкодингу - программированию с помощью AI-ассистентов."

  let body = json.object([
    #("model", json.string(model)),
    #("messages", json.array([
      json.object([
        #("role", json.string("system")),
        #("content", json.string(system_prompt)),
      ]),
      json.object([
        #("role", json.string("user")),
        #("content", json.string(user_message)),
      ]),
    ], fn(x) { x })),
  ])
  |> json.to_string()

  logging.debug("Calling OpenRouter API with model: " <> model)

  let req = request.new()
    |> request.set_scheme(http.Https)
    |> request.set_method(http.Post)
    |> request.set_host("openrouter.ai")
    |> request.set_path("/api/v1/chat/completions")
    |> request.set_header("Authorization", "Bearer " <> api_key)
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("HTTP-Referer", "https://vibee.ai")
    |> request.set_header("X-Title", "VIBEE Agent")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) -> {
      logging.debug("OpenRouter response status: " <> int.to_string(response.status))
      // Парсим JSON ответ и извлекаем content
      case response.status {
        200 -> {
          // Ищем "content":" в ответе и извлекаем текст
          case extract_content_from_response(response.body) {
            Ok(content) -> Ok(content)
            Error(err) -> {
              logging.error("Failed to parse OpenRouter response: " <> err)
              logging.debug("Response body: " <> string.slice(response.body, 0, 200))
              Error("Parse error: " <> err)
            }
          }
        }
        status -> {
          logging.error("OpenRouter API error: HTTP " <> int.to_string(status))
          logging.debug("Error body: " <> string.slice(response.body, 0, 200))
          Error("API error: HTTP " <> int.to_string(status))
        }
      }
    }
    Error(_) -> {
      logging.error("HTTP request to OpenRouter failed")
      Error("HTTP request failed")
    }
  }
}

/// Извлекает content из JSON ответа OpenRouter
/// Формат: {"choices":[{"message":{"content":"..."}}]}
fn extract_content_from_response(body: String) -> Result(String, String) {
  // Ищем "content":" и берём текст до следующей кавычки
  let pattern = "\"content\":\""
  case string.split(body, pattern) {
    [_, rest, ..] -> {
      // Ищем закрывающую кавычку (с учётом escaped quotes)
      case find_closing_quote(rest, "", False) {
        Ok(content) -> {
          // Декодируем escaped символы
          let decoded = content
            |> string.replace("\\n", "\n")
            |> string.replace("\\\"", "\"")
            |> string.replace("\\\\", "\\")
          Ok(decoded)
        }
        Error(e) -> Error(e)
      }
    }
    _ -> Error("No content field found")
  }
}

/// Находит закрывающую кавычку с учётом escape-последовательностей
fn find_closing_quote(s: String, acc: String, escaped: Bool) -> Result(String, String) {
  case string.pop_grapheme(s) {
    Ok(#(char, rest)) -> {
      case escaped {
        True -> find_closing_quote(rest, acc <> char, False)
        False -> {
          case char {
            "\\" -> find_closing_quote(rest, acc <> char, True)
            "\"" -> Ok(acc)
            _ -> find_closing_quote(rest, acc <> char, False)
          }
        }
      }
    }
    Error(_) -> Error("Unexpected end of string")
  }
}

/// Парсит bridge URL в компоненты (scheme, host, port)
fn parse_bridge_url(url: String) -> #(http.Scheme, String, Int) {
  case string.starts_with(url, "https://") {
    True -> {
      let host = string.drop_start(url, 8)
        |> string.split("/")
        |> list.first
        |> fn(r) { case r { Ok(h) -> h _ -> "localhost" } }
      #(http.Https, host, 443)
    }
    False -> {
      case string.starts_with(url, "http://") {
        True -> {
          let rest = string.drop_start(url, 7)
            |> string.split("/")
            |> list.first
            |> fn(r) { case r { Ok(h) -> h _ -> "localhost:8081" } }
          // Проверяем на port
          case string.split(rest, ":") {
            [h, p] -> {
              let port = case int.parse(p) {
                Ok(n) -> n
                Error(_) -> 80
              }
              #(http.Http, h, port)
            }
            _ -> #(http.Http, rest, 80)
          }
        }
        False -> #(http.Http, "localhost", 8081)
      }
    }
  }
}

/// Отправка сообщения через Go bridge
pub fn send_message(
  config: TelegramAgentConfig,
  chat_id: String,
  text: String,
  reply_to: Option(Int),
) -> Result(Int, String) {
  // Parse chat_id to int for Go bridge
  let chat_id_int = case int.parse(chat_id) {
    Ok(id) -> id
    Error(_) -> 0
  }

  let body = json.object([
    #("chat_id", json.int(chat_id_int)),
    #("text", json.string(text)),
    #("reply_to", case reply_to {
      None -> json.null()
      Some(id) -> json.int(id)
    }),
  ])
  |> json.to_string()

  // Parse bridge URL dynamically
  let #(scheme, host, port) = parse_bridge_url(config.bridge_url)

  io.println("[SEND] Sending message to chat " <> chat_id <> " via " <> host <> ":" <> int.to_string(port))

  // Get bridge API key for authorization
  let api_key = telegram_config.bridge_api_key()

  let req = request.new()
    |> request.set_scheme(scheme)
    |> request.set_method(http.Post)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path("/api/v1/send")
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("Authorization", "Bearer " <> api_key)
    |> request.set_header("X-Session-ID", config.session_id)
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) -> {
      io.println("[SEND] Response status: " <> int.to_string(response.status))
      case response.status {
        200 -> {
          io.println("[SEND] Message sent successfully!")
          Ok(0)
        }
        status -> {
          let err = "HTTP " <> int.to_string(status) <> ": " <> response.body
          io.println("[SEND ERROR] " <> err)
          Error(err)
        }
      }
    }
    Error(_) -> {
      io.println("[SEND ERROR] HTTP request failed")
      Error("Network error")
    }
  }
}

/// Отправка фото через Go bridge
pub fn send_photo(
  config: TelegramAgentConfig,
  chat_id: String,
  photo_url: String,
  caption: Option(String),
) -> Result(Int, String) {
  let chat_id_int = case int.parse(chat_id) {
    Ok(id) -> id
    Error(_) -> 0
  }

  let body = json.object([
    #("chat_id", json.int(chat_id_int)),
    #("photo_url", json.string(photo_url)),
    #("caption", case caption {
      None -> json.null()
      Some(c) -> json.string(c)
    }),
  ])
  |> json.to_string()

  let #(scheme, host, port) = parse_bridge_url(config.bridge_url)

  io.println("[PHOTO] Sending photo to chat " <> chat_id <> " via " <> host)

  // Get bridge API key for authorization
  let api_key = telegram_config.bridge_api_key()

  let req = request.new()
    |> request.set_scheme(scheme)
    |> request.set_method(http.Post)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path("/api/v1/send/photo")
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("Authorization", "Bearer " <> api_key)
    |> request.set_header("X-Session-ID", config.session_id)
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) -> {
      io.println("[PHOTO] Response status: " <> int.to_string(response.status))
      case response.status {
        200 -> {
          io.println("[PHOTO] Photo sent successfully!")
          Ok(0)
        }
        status -> {
          let err = "HTTP " <> int.to_string(status) <> ": " <> response.body
          io.println("[PHOTO ERROR] " <> err)
          Error(err)
        }
      }
    }
    Error(_) -> {
      io.println("[PHOTO ERROR] HTTP request failed")
      Error("Network error")
    }
  }
}

/// Получить историю сообщений из чата
pub fn get_history(config: TelegramAgentConfig, chat_id: String, limit: Int) -> Result(String, String) {
  let url = config.bridge_url <> "/api/v1/history/" <> chat_id <> "?limit=" <> int.to_string(limit)

  let req = request.new()
    |> request.set_scheme(http.Http)
    |> request.set_method(http.Get)
    |> request.set_host("localhost")
    |> request.set_port(8081)
    |> request.set_path("/api/v1/history/" <> chat_id)
    |> request.set_header("X-Session-ID", config.session_id)

  case httpc.send(req) {
    Ok(response) -> Ok(response.body)
    Error(_) -> Error("Failed to get history")
  }
}

/// Получить список диалогов
pub fn get_dialogs(config: TelegramAgentConfig, limit: Int) -> Result(String, String) {
  let req = request.new()
    |> request.set_scheme(http.Http)
    |> request.set_method(http.Get)
    |> request.set_host("localhost")
    |> request.set_port(8081)
    |> request.set_path("/api/v1/dialogs")
    |> request.set_header("X-Session-ID", config.session_id)

  case httpc.send(req) {
    Ok(response) -> Ok(response.body)
    Error(_) -> Error("Failed to get dialogs")
  }
}
