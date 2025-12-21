// WebSocket Events Parser
// Parses JSON events from Telegram Bridge WebSocket

import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// Telegram event received via WebSocket
pub type TelegramEvent {
  NewMessage(
    chat_id: Int,
    message_id: Int,
    from_id: Int,
    from_name: String,
    text: String,
    timestamp: Int,
    is_outgoing: Bool,
  )
  EditMessage(chat_id: Int, message_id: Int, text: String, timestamp: Int)
  DeleteMessage(chat_id: Int, message_id: Int, timestamp: Int)
  Ping
  Pong
  Connected
  Disconnected(reason: String)
  Unknown(raw: String)
}

/// Parse JSON event from WebSocket
/// Format: {"type":"new_message","chat_id":-123,"message_id":456,"from_id":789,"from_name":"User","text":"Hello","timestamp":1703180400,"out":false}
pub fn parse(json_str: String) -> TelegramEvent {
  // Handle ping/pong frames
  case string.trim(json_str) {
    "" -> Ping
    "ping" -> Ping
    "pong" -> Pong
    _ -> parse_json_event(json_str)
  }
}

fn parse_json_event(json_str: String) -> TelegramEvent {
  // Extract event type
  let event_type = extract_string_field(json_str, "type")

  case event_type {
    "new_message" -> parse_new_message(json_str)
    "edit_message" -> parse_edit_message(json_str)
    "delete_message" -> parse_delete_message(json_str)
    "connected" -> Connected
    "disconnected" -> {
      let reason = extract_string_field(json_str, "reason")
      Disconnected(reason)
    }
    _ -> Unknown(json_str)
  }
}

fn parse_new_message(json_str: String) -> TelegramEvent {
  let chat_id = extract_int_field(json_str, "chat_id")
  let message_id = extract_int_field(json_str, "message_id")
  let from_id = extract_int_field(json_str, "from_id")
  let from_name = case extract_string_field(json_str, "from_name") {
    "" -> "User"
    name -> name
  }
  let text = extract_string_field(json_str, "text")
  let timestamp = extract_int_field(json_str, "timestamp")
  let is_outgoing = extract_bool_field(json_str, "out")

  NewMessage(
    chat_id: chat_id,
    message_id: message_id,
    from_id: from_id,
    from_name: from_name,
    text: text,
    timestamp: timestamp,
    is_outgoing: is_outgoing,
  )
}

fn parse_edit_message(json_str: String) -> TelegramEvent {
  let chat_id = extract_int_field(json_str, "chat_id")
  let message_id = extract_int_field(json_str, "message_id")
  let text = extract_string_field(json_str, "text")
  let timestamp = extract_int_field(json_str, "timestamp")

  EditMessage(
    chat_id: chat_id,
    message_id: message_id,
    text: text,
    timestamp: timestamp,
  )
}

fn parse_delete_message(json_str: String) -> TelegramEvent {
  let chat_id = extract_int_field(json_str, "chat_id")
  let message_id = extract_int_field(json_str, "message_id")
  let timestamp = extract_int_field(json_str, "timestamp")

  DeleteMessage(chat_id: chat_id, message_id: message_id, timestamp: timestamp)
}

/// Extract string field from JSON
fn extract_string_field(json_str: String, field: String) -> String {
  // Look for "field":"value"
  let pattern = "\"" <> field <> "\":\""
  case string.split(json_str, pattern) {
    [_, rest, ..] -> {
      // Take everything up to the next unescaped quote
      extract_until_quote(rest, "")
    }
    _ -> ""
  }
}

/// Extract characters until closing quote (handling escaped quotes)
fn extract_until_quote(s: String, acc: String) -> String {
  case string.pop_grapheme(s) {
    Error(_) -> acc
    Ok(#("\"", _)) -> acc
    Ok(#("\\", rest)) -> {
      // Escaped character - take the next char too
      case string.pop_grapheme(rest) {
        Ok(#(ch, rest2)) -> extract_until_quote(rest2, acc <> ch)
        Error(_) -> acc
      }
    }
    Ok(#(ch, rest)) -> extract_until_quote(rest, acc <> ch)
  }
}

/// Extract integer field from JSON
fn extract_int_field(json_str: String, field: String) -> Int {
  // Look for "field":number
  let pattern = "\"" <> field <> "\":"
  case string.split(json_str, pattern) {
    [_, rest, ..] -> {
      // Take digits (and minus sign) until non-digit
      let num_str =
        rest
        |> string.to_graphemes
        |> list.take_while(fn(ch) {
          ch == "-" || ch == "0" || ch == "1" || ch == "2" || ch == "3"
          || ch == "4" || ch == "5" || ch == "6" || ch == "7" || ch == "8"
          || ch == "9"
        })
        |> string.concat
      case int.parse(num_str) {
        Ok(n) -> n
        Error(_) -> 0
      }
    }
    _ -> 0
  }
}

/// Extract boolean field from JSON
fn extract_bool_field(json_str: String, field: String) -> Bool {
  // Look for "field":true or "field":false
  let pattern = "\"" <> field <> "\":"
  case string.split(json_str, pattern) {
    [_, rest, ..] -> {
      let trimmed = string.trim(rest)
      string.starts_with(trimmed, "true")
    }
    _ -> False
  }
}

/// Format event for logging
pub fn to_string(event: TelegramEvent) -> String {
  case event {
    NewMessage(chat_id, msg_id, from_id, from_name, text, _, is_out) -> {
      let direction = case is_out {
        True -> "OUT"
        False -> "IN"
      }
      "[" <> direction <> "] " <> int.to_string(chat_id) <> " " <> from_name
      <> " (id=" <> int.to_string(from_id) <> "): "
      <> string.slice(text, 0, 50)
    }
    EditMessage(chat_id, msg_id, text, _) ->
      "[EDIT] " <> int.to_string(chat_id) <> " msg=" <> int.to_string(msg_id)
      <> ": " <> string.slice(text, 0, 50)
    DeleteMessage(chat_id, msg_id, _) ->
      "[DEL] " <> int.to_string(chat_id) <> " msg=" <> int.to_string(msg_id)
    Ping -> "[PING]"
    Pong -> "[PONG]"
    Connected -> "[CONNECTED]"
    Disconnected(reason) -> "[DISCONNECTED] " <> reason
    Unknown(raw) -> "[UNKNOWN] " <> string.slice(raw, 0, 100)
  }
}
