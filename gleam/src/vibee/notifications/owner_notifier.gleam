// Owner Notifier - Real-time уведомления owner о событиях агента
// Все важные события → личный чат owner с inline кнопками

import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import pog
import vibee/config/telegram_config
import vibee/db/postgres
import vibee/http_retry
import vibee/mcp/session_manager
import vibee/vibe_logger

// =============================================================================
// Constants
// =============================================================================

/// Owner chat ID (hardcoded, можно вынести в конфиг)
const owner_chat_id = 144_022_504

/// Максимальная длина сообщения Telegram
const max_message_length = 4096

// =============================================================================
// Types
// =============================================================================

/// Тип события
pub type EventType {
  NewMessage
  AgentReply
  TriggerDetected
  NewChat
  Lead
  ErrorEvent
  SystemEvent
}

/// Уровень важности
pub type Importance {
  Low
  Medium
  High
  Critical
}

/// Событие для уведомления owner
pub type OwnerEvent {
  OwnerEvent(
    event_type: EventType,
    importance: Importance,
    chat_id: Int,
    chat_name: String,
    from_id: Int,
    from_name: String,
    username: String,
    text: String,
    timestamp: Int,
    extra: List(#(String, String)),
  )
}

/// Inline кнопка
pub type NotificationButton {
  NotificationButton(text: String, callback_data: String)
}

/// Ошибки уведомлений
pub type NotificationError {
  NotificationDisabled
  MutedChat
  BelowMinImportance
  SendError(String)
  DatabaseError(String)
}

// =============================================================================
// Public API
// =============================================================================

/// Отправить уведомление owner (с проверкой фильтров)
pub fn notify(event: OwnerEvent) -> Result(Int, NotificationError) {
  let log = vibe_logger.new("owner_notifier")
    |> vibe_logger.with_data("event_type", json.string(event_type_to_string(event.event_type)))
    |> vibe_logger.with_data("chat_id", json.int(event.chat_id))
    |> vibe_logger.with_data("from_name", json.string(event.from_name))

  case postgres.get_global_pool() {
    None -> {
      vibe_logger.warn(log, "No database pool - sending notification anyway")
      send_notification(event)
    }
    Some(pool) -> {
      case should_notify(pool, owner_chat_id, event) {
        False -> {
          vibe_logger.debug(log, "Notification filtered out")
          Error(NotificationDisabled)
        }
        True -> {
          vibe_logger.info(log, "Sending owner notification")
          case send_notification(event) {
            Ok(msg_id) -> {
              // Логируем в БД
              log_notification(pool, owner_chat_id, event, msg_id)
              Ok(msg_id)
            }
            Error(e) -> Error(e)
          }
        }
      }
    }
  }
}

/// Отправить уведомление о новом неизвестном чате
pub fn notify_new_chat(
  chat_id: Int,
  chat_type: String,
  from_id: Int,
  from_name: String,
  username: String,
  sample_message: String,
) -> Result(Int, NotificationError) {
  let event = OwnerEvent(
    event_type: NewChat,
    importance: High,
    chat_id: chat_id,
    chat_name: chat_type,
    from_id: from_id,
    from_name: from_name,
    username: username,
    text: sample_message,
    timestamp: get_timestamp(),
    extra: [],
  )
  notify(event)
}

/// Отправить уведомление о лиде
pub fn notify_lead(
  chat_id: Int,
  chat_name: String,
  from_id: Int,
  from_name: String,
  username: String,
  trigger_text: String,
  trigger_word: String,
) -> Result(Int, NotificationError) {
  let event = OwnerEvent(
    event_type: Lead,
    importance: Critical,
    chat_id: chat_id,
    chat_name: chat_name,
    from_id: from_id,
    from_name: from_name,
    username: username,
    text: trigger_text,
    timestamp: get_timestamp(),
    extra: [#("trigger", trigger_word)],
  )
  notify(event)
}

/// Отправить уведомление об ошибке
pub fn notify_error(error_type: String, error_message: String) -> Result(Int, NotificationError) {
  let event = OwnerEvent(
    event_type: ErrorEvent,
    importance: Critical,
    chat_id: 0,
    chat_name: "",
    from_id: 0,
    from_name: "System",
    username: "",
    text: error_message,
    timestamp: get_timestamp(),
    extra: [#("error_type", error_type)],
  )
  notify(event)
}

/// Проверить должен ли owner получить это уведомление
pub fn should_notify(pool: pog.Connection, check_owner_id: Int, event: OwnerEvent) -> Bool {
  // Получаем настройки из БД
  let sql = "SELECT enabled, notify_new_messages, notify_agent_replies, notify_triggers,
                    notify_new_chats, notify_leads, notify_errors, min_importance,
                    mute_until, muted_chats
             FROM owner_notification_settings WHERE owner_id = $1"

  let decoder = {
    use enabled <- decode.field(0, decode.bool)
    use notify_new_messages <- decode.field(1, decode.bool)
    use notify_agent_replies <- decode.field(2, decode.bool)
    use notify_triggers <- decode.field(3, decode.bool)
    use notify_new_chats <- decode.field(4, decode.bool)
    use notify_leads <- decode.field(5, decode.bool)
    use notify_errors <- decode.field(6, decode.bool)
    use min_importance <- decode.field(7, decode.string)
    decode.success(#(
      enabled, notify_new_messages, notify_agent_replies, notify_triggers,
      notify_new_chats, notify_leads, notify_errors, min_importance,
    ))
  }

  case pog.query(sql)
    |> pog.parameter(pog.int(check_owner_id))
    |> pog.returning(decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [settings])) -> {
      let #(enabled, notify_messages, notify_replies, notify_triggers,
            notify_chats, notify_leads_flag, notify_errors_flag, min_importance) = settings

      // Проверка: уведомления включены?
      case enabled {
        False -> False
        True -> {
          // Проверка: тип события включён?
          let type_enabled = case event.event_type {
            NewMessage -> notify_messages
            AgentReply -> notify_replies
            TriggerDetected -> notify_triggers
            NewChat -> notify_chats
            Lead -> notify_leads_flag
            ErrorEvent -> notify_errors_flag
            SystemEvent -> True
          }

          case type_enabled {
            False -> False
            True -> {
              // Проверка: важность достаточная?
              let min_level = parse_importance(min_importance)
              importance_to_int(event.importance) >= importance_to_int(min_level)
            }
          }
        }
      }
    }
    // Если настроек нет - разрешаем всё
    Ok(_) -> True
    Error(_) -> True
  }
}

// =============================================================================
// Formatting
// =============================================================================

/// Форматировать событие как Event Card
fn format_event_card(event: OwnerEvent) -> String {
  let emoji = event_type_emoji(event.event_type)
  let title = event_type_title(event.event_type)
  let importance_badge = importance_badge(event.importance)

  let header = emoji <> " " <> title <> " " <> importance_badge <> "\n"
    <> "━━━━━━━━━━━━━━━\n\n"

  let user_info = case string.is_empty(event.username) {
    True -> "👤 От: " <> escape_markdown(event.from_name) <> "\n"
    False -> "👤 От: @" <> event.username <> " (" <> escape_markdown(event.from_name) <> ")\n"
  }

  let chat_info = case event.chat_id {
    0 -> ""
    _ -> "💬 Чат: " <> escape_markdown(event.chat_name) <> "\n"
      <> "📱 ID: " <> int.to_string(event.chat_id) <> "\n"
  }

  let text_section = case string.is_empty(event.text) {
    True -> ""
    False -> "\n📝 Текст:\n\"" <> escape_markdown(truncate(event.text, 500)) <> "\"\n"
  }

  let extra_section = format_extra(event.extra)

  let timestamp_section = "\n⏰ " <> format_timestamp(event.timestamp) <> "\n"

  let footer = "\n━━━━━━━━━━━━━━━"

  let full_text = header <> user_info <> chat_info <> text_section <> extra_section <> timestamp_section <> footer

  // Обрезаем если слишком длинное
  case string.length(full_text) > max_message_length {
    True -> string.slice(full_text, 0, max_message_length - 20) <> "\n[...обрезано]"
    False -> full_text
  }
}

/// Создать inline кнопки для события
fn create_buttons(event: OwnerEvent) -> List(List(NotificationButton)) {
  case event.event_type {
    NewMessage | NewChat -> [
      [
        NotificationButton("✅ Allow", "owner:allow:" <> int.to_string(event.chat_id)),
        NotificationButton("🚫 Block", "owner:block:" <> int.to_string(event.chat_id)),
      ],
      [
        NotificationButton("💬 Reply", "owner:reply:" <> int.to_string(event.chat_id)),
        NotificationButton("🔇 Mute 1h", "owner:mute:" <> int.to_string(event.chat_id) <> ":1h"),
      ],
    ]
    TriggerDetected | Lead -> [
      [
        NotificationButton("👁 View Lead", "owner:view:" <> int.to_string(event.from_id)),
        NotificationButton("📞 Contact", "owner:contact:" <> int.to_string(event.from_id)),
      ],
      [
        NotificationButton("🚫 Block Source", "owner:block:" <> int.to_string(event.chat_id)),
      ],
    ]
    ErrorEvent -> [
      [
        NotificationButton("📋 View Logs", "owner:logs:recent"),
        NotificationButton("🔄 Retry", "owner:retry:last"),
      ],
    ]
    AgentReply -> [
      [
        NotificationButton("👁 View", "owner:view:" <> int.to_string(event.chat_id)),
      ],
    ]
    SystemEvent -> []
  }
}

// =============================================================================
// Sending
// =============================================================================

/// Отправить уведомление в Telegram
fn send_notification(event: OwnerEvent) -> Result(Int, NotificationError) {
  let text = format_event_card(event)
  let buttons = create_buttons(event)

  let log = vibe_logger.new("owner_notifier")
    |> vibe_logger.with_data("buttons_count", json.int(list.length(buttons)))
    |> vibe_logger.with_data("event_type", json.string(event_type_to_string(event.event_type)))
  vibe_logger.info(log, "Sending notification with buttons")

  send_message_with_buttons(text, buttons)
}

/// Отправить сообщение с inline кнопками
/// ВАЖНО: Inline кнопки работают ТОЛЬКО через Bot API, не через user-bot!
fn send_message_with_buttons(
  text: String,
  buttons: List(List(NotificationButton)),
) -> Result(Int, NotificationError) {
  let bridge_url = telegram_config.bridge_url()
  let #(scheme, host, port) = parse_bridge_url(bridge_url)
  let api_key = telegram_config.bridge_api_key()

  // Формируем кнопки в формате для /bot/send-buttons
  // Формат: [[{"text": "...", "callback_data": "..."}]]
  let buttons_json = list.map(buttons, fn(row) {
    json.array(row, fn(btn) {
      json.object([
        #("text", json.string(btn.text)),
        #("callback_data", json.string(btn.callback_data)),
      ])
    })
  })

  let body_json = json.object([
    #("chat_id", json.int(owner_chat_id)),
    #("text", json.string(text)),
    #("buttons", json.array(buttons_json, fn(x) { x })),
  ])
  |> json.to_string

  // ВСЕГДА используем Bot API для inline кнопок
  // User-bot НЕ поддерживает inline keyboards!
  let endpoint = "/api/v1/bot/send-buttons"

  let log2 = vibe_logger.new("owner_notifier")
    |> vibe_logger.with_data("endpoint", json.string(endpoint))
    |> vibe_logger.with_data("buttons_count", json.int(list.length(buttons)))
    |> vibe_logger.with_data("chat_id", json.int(owner_chat_id))
  vibe_logger.info(log2, "Sending notification via Bot API")

  let req =
    request.new()
    |> request.set_scheme(scheme)
    |> request.set_method(http.Post)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path(endpoint)
    |> request.set_header("content-type", "application/json")
    |> request.set_header("Authorization", "Bearer " <> api_key)
    |> request.set_body(body_json)

  let retry_config = http_retry.default_config()

  case http_retry.send_with_retry(req, retry_config) {
    Ok(response) -> {
      case response.status {
        200 -> {
          let msg_id = decode_message_id(response.body)
          vibe_logger.info(vibe_logger.new("owner_notifier")
            |> vibe_logger.with_data("message_id", json.int(msg_id)), "Notification sent successfully")
          Ok(msg_id)
        }
        status -> {
          vibe_logger.error(vibe_logger.new("owner_notifier")
            |> vibe_logger.with_data("status", json.int(status))
            |> vibe_logger.with_data("body", json.string(response.body)), "Bot API error")
          Error(SendError("HTTP " <> int.to_string(status) <> ": " <> response.body))
        }
      }
    }
    Error(_) -> {
      vibe_logger.error(vibe_logger.new("owner_notifier"), "Network error sending notification")
      Error(SendError("Network error"))
    }
  }
}

// =============================================================================
// Database
// =============================================================================

/// Логировать отправленное уведомление
fn log_notification(pool: pog.Connection, log_owner_id: Int, event: OwnerEvent, message_id: Int) -> Nil {
  let sql = "INSERT INTO owner_notification_log
             (owner_id, event_type, source_chat_id, from_user_id, message_text, telegram_message_id)
             VALUES ($1, $2, $3, $4, $5, $6)"

  let _ = pog.query(sql)
    |> pog.parameter(pog.int(log_owner_id))
    |> pog.parameter(pog.text(event_type_to_string(event.event_type)))
    |> pog.parameter(pog.int(event.chat_id))
    |> pog.parameter(pog.int(event.from_id))
    |> pog.parameter(pog.text(truncate(event.text, 500)))
    |> pog.parameter(pog.int(message_id))
    |> pog.execute(pool)

  Nil
}

// =============================================================================
// Helpers
// =============================================================================

fn event_type_to_string(et: EventType) -> String {
  case et {
    NewMessage -> "new_message"
    AgentReply -> "agent_reply"
    TriggerDetected -> "trigger_detected"
    NewChat -> "new_chat"
    Lead -> "lead"
    ErrorEvent -> "error"
    SystemEvent -> "system"
  }
}

fn event_type_emoji(et: EventType) -> String {
  case et {
    NewMessage -> "📨"
    AgentReply -> "🤖"
    TriggerDetected -> "🎯"
    NewChat -> "🆕"
    Lead -> "💰"
    ErrorEvent -> "❌"
    SystemEvent -> "⚙️"
  }
}

fn event_type_title(et: EventType) -> String {
  case et {
    NewMessage -> "НОВОЕ СООБЩЕНИЕ"
    AgentReply -> "ОТВЕТ АГЕНТА"
    TriggerDetected -> "ТРИГГЕР"
    NewChat -> "НОВЫЙ ЧАТ"
    Lead -> "НОВЫЙ ЛИД"
    ErrorEvent -> "ОШИБКА"
    SystemEvent -> "СИСТЕМА"
  }
}

fn importance_badge(imp: Importance) -> String {
  case imp {
    Low -> ""
    Medium -> "🔵"
    High -> "🟠"
    Critical -> "🔴"
  }
}

fn importance_to_int(imp: Importance) -> Int {
  case imp {
    Low -> 1
    Medium -> 2
    High -> 3
    Critical -> 4
  }
}

fn parse_importance(s: String) -> Importance {
  case string.lowercase(s) {
    "low" -> Low
    "medium" -> Medium
    "high" -> High
    "critical" -> Critical
    _ -> Medium
  }
}

fn format_extra(extra: List(#(String, String))) -> String {
  case list.is_empty(extra) {
    True -> ""
    False -> {
      let lines = list.map(extra, fn(kv) {
        "🏷 " <> kv.0 <> ": " <> kv.1
      })
      "\n" <> string.join(lines, "\n") <> "\n"
    }
  }
}

fn escape_markdown(text: String) -> String {
  text
  |> string.replace("\\", "\\\\")
  |> string.replace("[", "\\[")
  |> string.replace("]", "\\]")
  |> string.replace("*", "\\*")
  |> string.replace("_", "\\_")
  |> string.replace("`", "\\`")
}

fn truncate(s: String, max: Int) -> String {
  case string.length(s) > max {
    True -> string.slice(s, 0, max) <> "..."
    False -> s
  }
}

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

fn decode_message_id(body: String) -> Int {
  let decoder = {
    use message_id <- decode.field("message_id", decode.int)
    decode.success(message_id)
  }
  case json.parse(body, decoder) {
    Ok(id) -> id
    Error(_) -> 0
  }
}

@external(erlang, "vibee_owner_notifier_ffi", "get_timestamp")
fn get_timestamp() -> Int

@external(erlang, "vibee_owner_notifier_ffi", "format_timestamp")
fn format_timestamp(ts: Int) -> String
