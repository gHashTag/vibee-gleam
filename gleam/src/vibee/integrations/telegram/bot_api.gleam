// Telegram Bot API Client
// Communicates with Go telegram-bridge for Bot API operations
// Used for inline keyboard buttons that require callback queries

import gleam/http
import gleam/http/request
import gleam/http/response
import gleam/httpc
import gleam/io
import gleam/json
import gleam/dynamic/decode
import gleam/option.{type Option, None, Some}
import gleam/int
import gleam/string
import vibee/integrations/telegram/types.{
  type TelegramError,
  ConnectionError, ApiError, NetworkError,
}

/// Bot API client configuration
pub type BotApiConfig {
  BotApiConfig(
    bridge_url: String,
    api_key: Option(String),
  )
}

/// Inline keyboard button
pub type InlineButton {
  InlineButton(
    text: String,
    callback_data: String,
  )
}

/// Callback query data received from webhook
pub type CallbackQuery {
  CallbackQuery(
    query_id: String,
    chat_id: Int,
    user_id: Int,
    username: String,
    data: String,
    message_id: Int,
  )
}

/// Send result
pub type SendResult {
  SendResult(
    success: Bool,
    message_id: Int,
  )
}

/// Bot status
pub type BotStatus {
  BotStatus(
    configured: Bool,
    bot_id: Option(Int),
    username: Option(String),
  )
}

/// Create a new Bot API client
pub fn new(bridge_url: String) -> BotApiConfig {
  BotApiConfig(bridge_url: bridge_url, api_key: None)
}

/// Create a Bot API client with API key
pub fn with_key(bridge_url: String, api_key: String) -> BotApiConfig {
  BotApiConfig(bridge_url: bridge_url, api_key: Some(api_key))
}

/// Get Bot API status
pub fn get_status(config: BotApiConfig) -> Result(BotStatus, TelegramError) {
  case get(config, "/api/v1/bot/status") {
    Ok(resp) -> parse_bot_status(resp.body)
    Error(e) -> Error(e)
  }
}

/// Send a message with inline keyboard via Bot API
pub fn send_with_buttons(
  config: BotApiConfig,
  chat_id: Int,
  text: String,
  buttons: List(List(InlineButton)),
) -> Result(SendResult, TelegramError) {
  io.println("[BotAPI] send_with_buttons: chat_id=" <> int.to_string(chat_id))
  io.println("[BotAPI] text=" <> string.slice(text, 0, 50) <> "...")

  let buttons_json = json.array(buttons, fn(row) {
    json.array(row, fn(btn) {
      json.object([
        #("text", json.string(btn.text)),
        #("callback_data", json.string(btn.callback_data)),
      ])
    })
  })

  let body = json.object([
    #("chat_id", json.int(chat_id)),
    #("text", json.string(text)),
    #("buttons", buttons_json),
  ])

  case post(config, "/api/v1/bot/send-buttons", json.to_string(body)) {
    Ok(resp) -> {
      io.println("[BotAPI] ✅ Response: " <> string.slice(resp.body, 0, 200))
      parse_send_result(resp.body)
    }
    Error(e) -> {
      io.println("[BotAPI] ❌ Failed: " <> error_to_string(e))
      Error(e)
    }
  }
}

/// Answer a callback query
pub fn answer_callback(
  config: BotApiConfig,
  query_id: String,
  text: String,
  show_alert: Bool,
) -> Result(Nil, TelegramError) {
  io.println("[BotAPI] answer_callback: query_id=" <> query_id)

  let body = json.object([
    #("query_id", json.string(query_id)),
    #("text", json.string(text)),
    #("show_alert", json.bool(show_alert)),
  ])

  case post(config, "/api/v1/bot/answer", json.to_string(body)) {
    Ok(resp) -> {
      io.println("[BotAPI] ✅ Callback answered: " <> string.slice(resp.body, 0, 200))
      Ok(Nil)
    }
    Error(e) -> {
      io.println("[BotAPI] ❌ Answer failed: " <> error_to_string(e))
      Error(e)
    }
  }
}

// HTTP helpers

fn get(
  config: BotApiConfig,
  path: String,
) -> Result(response.Response(String), TelegramError) {
  let url = config.bridge_url <> path

  case request.to(url) {
    Ok(req) -> {
      let req = req
        |> request.set_method(http.Get)
        |> add_auth_header(config.api_key)

      case httpc.send(req) {
        Ok(resp) -> check_response(resp)
        Error(_) -> Error(NetworkError("Failed to send request"))
      }
    }
    Error(_) -> Error(ConnectionError("Invalid URL: " <> url))
  }
}

fn post(
  config: BotApiConfig,
  path: String,
  body: String,
) -> Result(response.Response(String), TelegramError) {
  let url = config.bridge_url <> path

  case request.to(url) {
    Ok(req) -> {
      let req = req
        |> request.set_method(http.Post)
        |> request.set_header("content-type", "application/json")
        |> add_auth_header(config.api_key)
        |> request.set_body(body)

      case httpc.send(req) {
        Ok(resp) -> check_response(resp)
        Error(_) -> Error(NetworkError("Failed to send request"))
      }
    }
    Error(_) -> Error(ConnectionError("Invalid URL: " <> url))
  }
}

fn add_auth_header(req: request.Request(String), api_key: Option(String)) -> request.Request(String) {
  case api_key {
    Some(key) -> request.set_header(req, "authorization", "Bearer " <> key)
    None -> req
  }
}

fn check_response(
  resp: response.Response(String),
) -> Result(response.Response(String), TelegramError) {
  case resp.status {
    200 -> Ok(resp)
    503 -> Error(ApiError(503, "Bot API not configured"))
    _ -> Error(ApiError(resp.status, resp.body))
  }
}

// JSON parsers

fn parse_bot_status(body: String) -> Result(BotStatus, TelegramError) {
  let decoder = {
    use configured <- decode.field("configured", decode.bool)
    use bot_id <- decode.optional_field("bot_id", None, decode.optional(decode.int))
    use username <- decode.optional_field("username", None, decode.optional(decode.string))
    decode.success(BotStatus(configured, bot_id, username))
  }

  case json.parse(body, decoder) {
    Ok(status) -> Ok(status)
    Error(_) -> Error(ApiError(0, "Failed to parse bot status"))
  }
}

fn parse_send_result(body: String) -> Result(SendResult, TelegramError) {
  let decoder = {
    use success <- decode.field("success", decode.bool)
    use message_id <- decode.field("message_id", decode.int)
    decode.success(SendResult(success, message_id))
  }

  case json.parse(body, decoder) {
    Ok(result) -> Ok(result)
    Error(_) -> Error(ApiError(0, "Failed to parse send result"))
  }
}

/// Parse callback query from webhook JSON
pub fn parse_callback_query(body: String) -> Result(CallbackQuery, TelegramError) {
  let decoder = {
    use query_id <- decode.field("query_id", decode.string)
    use chat_id <- decode.field("chat_id", decode.int)
    use user_id <- decode.field("user_id", decode.int)
    use username <- decode.optional_field("username", "", decode.string)
    use data <- decode.field("data", decode.string)
    use message_id <- decode.field("message_id", decode.int)
    decode.success(CallbackQuery(query_id, chat_id, user_id, username, data, message_id))
  }

  case json.parse(body, decoder) {
    Ok(cq) -> Ok(cq)
    Error(_) -> Error(ApiError(0, "Failed to parse callback query"))
  }
}

fn error_to_string(err: TelegramError) -> String {
  case err {
    types.ConnectionError(msg) -> "ConnectionError: " <> msg
    types.AuthError(msg) -> "AuthError: " <> msg
    types.ApiError(code, msg) -> "ApiError(" <> int.to_string(code) <> "): " <> msg
    types.NetworkError(msg) -> "NetworkError: " <> msg
    types.InvalidSession -> "InvalidSession"
    types.NotAuthorized -> "NotAuthorized"
  }
}
