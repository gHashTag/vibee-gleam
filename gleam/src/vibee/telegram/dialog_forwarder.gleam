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
  // Для supergroup нужен префикс -100
  let full_chat_id = case string.starts_with(target_chat_id, "-") {
    True -> target_chat_id  // Уже с минусом
    False -> "-100" <> target_chat_id  // Добавляем префикс для supergroup
  }
  
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
  "🔥 НОВЫЙ ЛИД\n\n"
  <> "👤 Клиент: " <> original.from_name <> "\n"
  <> "💬 Вопрос: " <> original.text <> "\n\n"
  <> "✅ Ответ агента:\n"
  <> reply.text <> "\n\n"
  <> "📱 Действие: Клиент приглашён в личку"
}

/// Отправляет сообщение через telegram-bridge
fn send_message(
  session_id: String,
  chat_id: String,
  text: String,
) -> Result(Int, String) {
  let bridge_url = telegram_config.bridge_url()
  
  // Парсим URL в компоненты
  let #(scheme, host, port) = parse_bridge_url(bridge_url)
  
  // Формируем JSON body
  let body_json =
    json.object([
      #("chat_id", json.string(chat_id)),
      #("text", json.string(text)),
    ])
    |> json.to_string
  
  // Создаем запрос с правильными scheme, host, port
  let req =
    request.new()
    |> request.set_scheme(scheme)
    |> request.set_method(http.Post)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path("/api/v1/send")
    |> request.set_header("content-type", "application/json")
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
