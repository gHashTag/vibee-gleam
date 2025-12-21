// Dialog Forwarder
// Пересылка диалогов (вопрос + ответ) в целевую группу

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
import vibee/config/telegram_config
import vibee/config/trigger_chats
import vibee/http_retry

/// Информация о сообщении для пересылки
pub type MessageInfo {
  MessageInfo(
    chat_id: String,
    chat_name: String,
    message_id: Int,
    from_name: String,
    text: String,
    timestamp: Int,
  )
}

/// Результат пересылки
pub type ForwardResult {
  ForwardSuccess(message_id: Int)
  ForwardError(reason: String)
}

/// Пересылает диалог (вопрос + ответ) в целевую группу
pub fn forward_dialog(
  session_id: String,
  original_message: MessageInfo,
  agent_reply: MessageInfo,
  target_chat_id: String,
) -> ForwardResult {
  io.println("[FORWARD] ═══════════════════════════════════════")
  io.println("[FORWARD] 🎯 INPUT target_chat_id: " <> target_chat_id)
  io.println("[FORWARD] 🔑 session_id: " <> session_id)

  // Передаём chat_id как есть - без модификации!
  // Go Bridge сам разберется с форматом
  let full_chat_id = target_chat_id

  io.println("[FORWARD] 📤 Forwarding dialog to chat " <> full_chat_id)
  io.println("[FORWARD] Original: " <> original_message.from_name <> ": " <> string.slice(original_message.text, 0, 50))
  io.println("[FORWARD] Reply: " <> string.slice(agent_reply.text, 0, 50))
  
  // Формируем текст диалога
  let dialog_text = format_dialog(original_message, agent_reply)
  
  // Отправляем в целевую группу
  case send_message(session_id, full_chat_id, dialog_text) {
    Ok(msg_id) -> {
      io.println(
        "[FORWARD] Dialog forwarded successfully, message_id: "
        <> string.inspect(msg_id),
      )
      ForwardSuccess(msg_id)
    }
    Error(reason) -> {
      io.println("[FORWARD] Failed to forward dialog: " <> reason)
      ForwardError(reason)
    }
  }
}

/// Форматирует диалог для пересылки
fn format_dialog(original: MessageInfo, reply: MessageInfo) -> String {
  // Генерируем ссылку на сообщение (t.me/c/{channel_id}/{message_id})
  let message_link = make_message_link(original.chat_id, original.message_id)

  "🔔 НОВЫЙ ЛИД\n"
  <> "━━━━━━━━━━━━━━━\n\n"
  <> "📍 " <> original.chat_name <> "\n"
  <> "👤 " <> original.from_name <> "\n\n"
  <> "💬 Вопрос:\n" <> original.text <> "\n\n"
  <> "🤖 Ответ:\n" <> reply.text <> "\n\n"
  <> "━━━━━━━━━━━━━━━\n"
  <> message_link
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
  let bridge_url = telegram_config.bridge_url()

  io.println("[FORWARD] send_message called")
  io.println("[FORWARD] chat_id (string): " <> chat_id)
  io.println("[FORWARD] session_id: " <> session_id)
  io.println("[FORWARD] bridge_url: " <> bridge_url)

  // Парсим URL в компоненты
  let #(scheme, host, port) = parse_bridge_url(bridge_url)

  // Парсим chat_id как int - Go Bridge ожидает int!
  let chat_id_int = case int.parse(chat_id) {
    Ok(id) -> id
    Error(_) -> {
      io.println("[FORWARD] ❌ Failed to parse chat_id as int: " <> chat_id)
      0
    }
  }
  io.println("[FORWARD] chat_id_int: " <> int.to_string(chat_id_int))

  // Формируем JSON body с INT (не STRING!)
  let body_json =
    json.object([
      #("chat_id", json.int(chat_id_int)),  // ИСПРАВЛЕНО: int вместо string!
      #("text", json.string(text)),
    ])
    |> json.to_string

  io.println("[FORWARD] request body: " <> body_json)
  
  // Получаем API ключ для авторизации
  let api_key = telegram_config.bridge_api_key()

  // Создаем запрос с правильными scheme, host, port
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
  
  // Отправляем запрос с retry logic
  let retry_config = http_retry.default_config()
  case http_retry.send_with_retry(req, retry_config) {
    Ok(response) -> {
      case response.status {
        200 -> {
          io.println("[FORWARD] ✅ Message sent successfully to chat " <> chat_id)
          Ok(0)
        }
        _ -> {
          io.println("[FORWARD] ❌ HTTP error: " <> int.to_string(response.status))
          io.println("[FORWARD] Response body: " <> response.body)
          Error("HTTP " <> int.to_string(response.status))
        }
      }
    }
    Error(err) -> {
      io.println("[FORWARD] ❌ Network error after retries: " <> string.inspect(err))
      Error("Network error: " <> string.inspect(err))
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
