// Telegram Bridge HTTP Client
// Communicates with Go telegram-bridge service

import gleam/http
import gleam/http/request
import gleam/http/response
import gleam/httpc
import gleam/json
import gleam/dynamic/decode
import gleam/result
import gleam/option.{type Option, None, Some}
import gleam/int
import gleam/string
import vibee/integrations/telegram/types.{
  type TelegramConfig, type SessionInfo, type TelegramUser,
  type TelegramDialog, type TelegramMessage, type SendMessageResult,
  type TelegramError, type DialogType,
  TelegramConfig, SessionInfo, TelegramUser, TelegramDialog,
  TelegramMessage, SendMessageResult,
  UserDialog, GroupDialog, SupergroupDialog, ChannelDialog,
  ConnectionError, AuthError, ApiError, NetworkError, InvalidSession, NotAuthorized,
}

/// Telegram Bridge client
pub type TelegramBridge {
  TelegramBridge(
    base_url: String,
    session_id: Option(String),
  )
}

/// Create a new bridge client
pub fn new(base_url: String) -> TelegramBridge {
  TelegramBridge(base_url: base_url, session_id: None)
}

/// Create a bridge client with existing session
pub fn with_session(base_url: String, session_id: String) -> TelegramBridge {
  TelegramBridge(base_url: base_url, session_id: Some(session_id))
}

/// Connect to Telegram
pub fn connect(
  bridge: TelegramBridge,
  config: TelegramConfig,
) -> Result(#(TelegramBridge, SessionInfo), TelegramError) {
  let body = json.object([
    #("app_id", json.int(config.app_id)),
    #("app_hash", json.string(config.app_hash)),
    #("phone", case config.phone {
      Some(p) -> json.string(p)
      None -> json.null()
    }),
  ])

  case post(bridge, "/api/v1/connect", json.to_string(body)) {
    Ok(resp) -> {
      case parse_session_info(resp.body) {
        Ok(info) -> {
          let new_bridge = TelegramBridge(
            base_url: bridge.base_url,
            session_id: Some(info.session_id),
          )
          Ok(#(new_bridge, info))
        }
        Error(e) -> Error(e)
      }
    }
    Error(e) -> Error(e)
  }
}

/// Send authentication code to phone
pub fn send_code(
  bridge: TelegramBridge,
  phone: String,
) -> Result(String, TelegramError) {
  let body = json.object([
    #("phone", json.string(phone)),
  ])

  case post_with_session(bridge, "/api/v1/auth/phone", json.to_string(body)) {
    Ok(resp) -> parse_code_hash(resp.body)
    Error(e) -> Error(e)
  }
}

/// Verify authentication code
pub fn verify_code(
  bridge: TelegramBridge,
  code: String,
) -> Result(TelegramUser, TelegramError) {
  let body = json.object([
    #("code", json.string(code)),
  ])

  case post_with_session(bridge, "/api/v1/auth/code", json.to_string(body)) {
    Ok(resp) -> parse_auth_result(resp.body)
    Error(e) -> Error(e)
  }
}

/// Verify 2FA password
pub fn verify_2fa(
  bridge: TelegramBridge,
  password: String,
) -> Result(TelegramUser, TelegramError) {
  let body = json.object([
    #("password", json.string(password)),
  ])

  case post_with_session(bridge, "/api/v1/auth/2fa", json.to_string(body)) {
    Ok(resp) -> parse_auth_result(resp.body)
    Error(e) -> Error(e)
  }
}

/// Get current user info
pub fn get_me(bridge: TelegramBridge) -> Result(TelegramUser, TelegramError) {
  case get_with_session(bridge, "/api/v1/me") {
    Ok(resp) -> parse_user(resp.body)
    Error(e) -> Error(e)
  }
}

/// Get list of dialogs
pub fn get_dialogs(
  bridge: TelegramBridge,
  limit: Int,
) -> Result(List(TelegramDialog), TelegramError) {
  let path = "/api/v1/dialogs?limit=" <> int.to_string(limit)
  case get_with_session(bridge, path) {
    Ok(resp) -> parse_dialogs(resp.body)
    Error(e) -> Error(e)
  }
}

/// Get chat history
pub fn get_history(
  bridge: TelegramBridge,
  chat_id: Int,
  limit: Int,
) -> Result(List(TelegramMessage), TelegramError) {
  let path = "/api/v1/history/" <> int.to_string(chat_id) <> "?limit=" <> int.to_string(limit)
  case get_with_session(bridge, path) {
    Ok(resp) -> parse_messages(resp.body)
    Error(e) -> Error(e)
  }
}

/// Send a message
pub fn send_message(
  bridge: TelegramBridge,
  chat_id: Int,
  text: String,
  reply_to: Option(Int),
) -> Result(SendMessageResult, TelegramError) {
  let body = json.object([
    #("chat_id", json.int(chat_id)),
    #("text", json.string(text)),
    #("reply_to", case reply_to {
      Some(id) -> json.int(id)
      None -> json.int(0)
    }),
  ])

  case post_with_session(bridge, "/api/v1/send", json.to_string(body)) {
    Ok(resp) -> parse_send_result(resp.body)
    Error(e) -> Error(e)
  }
}

/// Get WebSocket URL for updates
pub fn get_updates_url(bridge: TelegramBridge) -> Result(String, TelegramError) {
  case bridge.session_id {
    Some(sid) -> {
      let ws_url = string.replace(bridge.base_url, "http://", "ws://")
      let ws_url = string.replace(ws_url, "https://", "wss://")
      Ok(ws_url <> "/api/v1/updates?session_id=" <> sid)
    }
    None -> Error(InvalidSession)
  }
}

// HTTP helpers

fn post(
  bridge: TelegramBridge,
  path: String,
  body: String,
) -> Result(response.Response(String), TelegramError) {
  let url = bridge.base_url <> path

  case request.to(url) {
    Ok(req) -> {
      let req = req
        |> request.set_method(http.Post)
        |> request.set_header("content-type", "application/json")
        |> request.set_body(body)

      case httpc.send(req) {
        Ok(resp) -> Ok(resp)
        Error(_) -> Error(NetworkError("Failed to send request"))
      }
    }
    Error(_) -> Error(ConnectionError("Invalid URL: " <> url))
  }
}

fn post_with_session(
  bridge: TelegramBridge,
  path: String,
  body: String,
) -> Result(response.Response(String), TelegramError) {
  case bridge.session_id {
    None -> Error(InvalidSession)
    Some(sid) -> {
      let url = bridge.base_url <> path

      case request.to(url) {
        Ok(req) -> {
          let req = req
            |> request.set_method(http.Post)
            |> request.set_header("content-type", "application/json")
            |> request.set_header("x-session-id", sid)
            |> request.set_body(body)

          case httpc.send(req) {
            Ok(resp) -> check_response(resp)
            Error(_) -> Error(NetworkError("Failed to send request"))
          }
        }
        Error(_) -> Error(ConnectionError("Invalid URL: " <> url))
      }
    }
  }
}

fn get_with_session(
  bridge: TelegramBridge,
  path: String,
) -> Result(response.Response(String), TelegramError) {
  case bridge.session_id {
    None -> Error(InvalidSession)
    Some(sid) -> {
      let url = bridge.base_url <> path

      case request.to(url) {
        Ok(req) -> {
          let req = req
            |> request.set_method(http.Get)
            |> request.set_header("x-session-id", sid)

          case httpc.send(req) {
            Ok(resp) -> check_response(resp)
            Error(_) -> Error(NetworkError("Failed to send request"))
          }
        }
        Error(_) -> Error(ConnectionError("Invalid URL: " <> url))
      }
    }
  }
}

fn check_response(
  resp: response.Response(String),
) -> Result(response.Response(String), TelegramError) {
  case resp.status {
    200 -> Ok(resp)
    401 -> Error(NotAuthorized)
    _ -> Error(ApiError(resp.status, resp.body))
  }
}

// JSON parsers using new decode API

fn parse_session_info(body: String) -> Result(SessionInfo, TelegramError) {
  let decoder = {
    use session_id <- decode.field("session_id", decode.string)
    use authorized <- decode.field("authorized", decode.bool)
    use message <- decode.field("message", decode.string)
    decode.success(SessionInfo(session_id, authorized, message))
  }

  case json.parse(body, decoder) {
    Ok(info) -> Ok(info)
    Error(_) -> Error(ApiError(0, "Failed to parse session info"))
  }
}

fn parse_code_hash(body: String) -> Result(String, TelegramError) {
  let decoder = {
    use code_hash <- decode.field("code_hash", decode.string)
    decode.success(code_hash)
  }

  case json.parse(body, decoder) {
    Ok(hash) -> Ok(hash)
    Error(_) -> Error(ApiError(0, "Failed to parse code hash"))
  }
}

fn parse_auth_result(body: String) -> Result(TelegramUser, TelegramError) {
  // Check if 2FA required
  let status_decoder = {
    use status <- decode.field("status", decode.string)
    decode.success(status)
  }
  case json.parse(body, status_decoder) {
    Ok("2fa_required") -> Error(AuthError("2FA required"))
    _ -> parse_user_from_auth(body)
  }
}

fn parse_user_from_auth(body: String) -> Result(TelegramUser, TelegramError) {
  let decoder = {
    use user <- decode.field("user", user_decoder())
    decode.success(user)
  }
  case json.parse(body, decoder) {
    Ok(user) -> Ok(user)
    Error(_) -> Error(ApiError(0, "Failed to parse auth result"))
  }
}

fn parse_user(body: String) -> Result(TelegramUser, TelegramError) {
  case json.parse(body, user_decoder()) {
    Ok(user) -> Ok(user)
    Error(_) -> Error(ApiError(0, "Failed to parse user"))
  }
}

fn user_decoder() -> decode.Decoder(TelegramUser) {
  {
    use id <- decode.field("id", decode.int)
    use first_name <- decode.field("first_name", decode.string)
    use last_name <- decode.optional_field("last_name", None, decode.optional(decode.string))
    use username <- decode.optional_field("username", None, decode.optional(decode.string))
    use phone <- decode.optional_field("phone", None, decode.optional(decode.string))
    decode.success(TelegramUser(id, first_name, last_name, username, phone))
  }
}

fn parse_dialogs(body: String) -> Result(List(TelegramDialog), TelegramError) {
  let decoder = {
    use dialogs <- decode.field("dialogs", decode.list(dialog_decoder()))
    decode.success(dialogs)
  }
  case json.parse(body, decoder) {
    Ok(dialogs) -> Ok(dialogs)
    Error(_) -> Error(ApiError(0, "Failed to parse dialogs"))
  }
}

fn dialog_decoder() -> decode.Decoder(TelegramDialog) {
  {
    use id <- decode.field("id", decode.int)
    use title <- decode.field("title", decode.string)
    use dialog_type <- decode.field("type", dialog_type_decoder())
    use unread_count <- decode.field("unread_count", decode.int)
    use last_message <- decode.optional_field("last_message", None, decode.optional(decode.string))
    decode.success(TelegramDialog(id, title, dialog_type, unread_count, last_message))
  }
}

fn dialog_type_decoder() -> decode.Decoder(DialogType) {
  decode.then(decode.string, fn(s) {
    case s {
      "user" -> decode.success(UserDialog)
      "group" -> decode.success(GroupDialog)
      "supergroup" -> decode.success(SupergroupDialog)
      "channel" -> decode.success(ChannelDialog)
      _ -> decode.success(UserDialog) // Default
    }
  })
}

fn parse_messages(body: String) -> Result(List(TelegramMessage), TelegramError) {
  let decoder = {
    use messages <- decode.field("messages", decode.list(message_decoder()))
    decode.success(messages)
  }
  case json.parse(body, decoder) {
    Ok(messages) -> Ok(messages)
    Error(_) -> Error(ApiError(0, "Failed to parse messages"))
  }
}

fn message_decoder() -> decode.Decoder(TelegramMessage) {
  {
    use id <- decode.field("id", decode.int)
    use text <- decode.field("text", decode.string)
    use from_id <- decode.field("from_id", decode.int)
    use from_name <- decode.field("from_name", decode.string)
    use date <- decode.field("date", decode.string)
    use reply_to_id <- decode.optional_field("reply_to_id", None, decode.optional(decode.int))
    decode.success(TelegramMessage(id, text, from_id, from_name, date, reply_to_id))
  }
}

fn parse_send_result(body: String) -> Result(SendMessageResult, TelegramError) {
  let decoder = {
    use success <- decode.field("success", decode.bool)
    use message_id <- decode.field("message_id", decode.int)
    decode.success(SendMessageResult(success, message_id))
  }

  case json.parse(body, decoder) {
    Ok(result) -> Ok(result)
    Error(_) -> Error(ApiError(0, "Failed to parse send result"))
  }
}
