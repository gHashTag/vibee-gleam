// Bot Actor - Main OTP Actor for Telegram Bot
// Uses WebSocket for real-time updates from Go telegram-bridge

import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/erlang/process.{type Subject}
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/set.{type Set}
import gleam/string
import pog
import vibee/bot/router
import vibee/bot/scene.{
  type IncomingMessage, type InlineButton, type OutgoingMessage, CallbackQuery,
  Command, InlineButton, PhotoMessage, PhotosReply, TextMessage, TextReply,
  TextWithKeyboard,
}
import vibee/bot/ws_client.{type WsActorMessage, type WsConfig, type WsMessage, WsConnected, WsDisconnected, WsError, WsTextMessage}
import vibee/logging

// ============================================================
// Types
// ============================================================

pub type BotConfig {
  BotConfig(
    telegram_bridge_host: String,
    telegram_bridge_port: Int,
    session_id: String,
    fal_api_key: String,
    replicate_api_token: String,
  )
}

pub type BotState {
  BotState(
    config: BotConfig,
    db_pool: pog.Connection,
    ws_client: Option(Subject(WsActorMessage)),
    is_running: Bool,
    processed_updates: Set(String),
    messages_processed: Int,
    error_count: Int,
  )
}

pub type BotMessage {
  // Control messages
  Start
  Stop
  // WebSocket events
  BotWsConnected
  BotWsDisconnected(reason: String)
  BotWsError(error: String)
  WsUpdate(data: String)
  // Process update
  ProcessUpdate(update: Update)
  // Send response
  SendResponse(chat_id: String, response: OutgoingMessage)
  // Status
  GetStatus(reply_to: Subject(BotStatus))
}

pub type BotStatus {
  BotStatus(
    is_running: Bool,
    connected: Bool,
    processed_count: Int,
    error_count: Int,
  )
}

pub type Update {
  MessageUpdate(
    update_id: String,
    chat_id: String,
    user_id: Int,
    username: Option(String),
    text: Option(String),
    photo_file_id: Option(String),
    caption: Option(String),
  )
  CallbackUpdate(
    update_id: String,
    chat_id: String,
    user_id: Int,
    username: Option(String),
    callback_data: String,
    message_id: Int,
  )
}

// ============================================================
// Actor Setup
// ============================================================

/// Start the bot actor
pub fn start(
  config: BotConfig,
  db_pool: pog.Connection,
) -> Result(Subject(BotMessage), actor.StartError) {
  let initial_state =
    BotState(
      config: config,
      db_pool: db_pool,
      ws_client: None,
      is_running: False,
      processed_updates: set.new(),
      messages_processed: 0,
      error_count: 0,
    )

  let spec =
    actor.new(initial_state)
    |> actor.on_message(handle_message)

  case actor.start(spec) {
    Ok(started) -> Ok(started.data)
    Error(err) -> Error(err)
  }
}

/// Handle incoming messages
fn handle_message(
  state: BotState,
  message: BotMessage,
) -> actor.Next(BotState, BotMessage) {
  case message {
    Start -> {
      logging.quick_info("[BOT] Starting bot with WebSocket connection...")

      // Create WebSocket config
      let ws_config =
        ws_client.new_config(
          state.config.telegram_bridge_host,
          state.config.telegram_bridge_port,
          state.config.session_id,
        )

      // Create handler that forwards messages to this actor
      let self = process.new_subject()
      let handler = fn(msg: WsMessage) {
        case msg {
          WsConnected -> process.send(self, BotWsConnected)
          WsDisconnected(reason) -> process.send(self, BotWsDisconnected(reason))
          WsError(err) -> process.send(self, BotWsError(err))
          WsTextMessage(data) -> process.send(self, WsUpdate(data))
        }
      }

      // Start WebSocket client
      case ws_client.start(ws_config, handler) {
        Ok(ws) -> {
          // Connect to WebSocket
          ws_client.connect(ws)

          actor.continue(BotState(
            ..state,
            ws_client: Some(ws),
            is_running: True,
          ))
        }
        Error(_) -> {
          logging.quick_error("[BOT] Failed to start WebSocket client")
          actor.continue(BotState(..state, error_count: state.error_count + 1))
        }
      }
    }

    Stop -> {
      logging.quick_info("[BOT] Stopping bot...")
      case state.ws_client {
        Some(ws) -> ws_client.disconnect(ws)
        None -> Nil
      }
      actor.continue(BotState(..state, is_running: False, ws_client: None))
    }

    BotWsConnected -> {
      logging.quick_info("[BOT] WebSocket connected to telegram-bridge")
      actor.continue(state)
    }

    BotWsDisconnected(reason) -> {
      logging.quick_warn("[BOT] WebSocket disconnected: " <> reason)
      // Auto-reconnect
      case state.is_running, state.ws_client {
        True, Some(ws) -> {
          logging.quick_info("[BOT] Attempting to reconnect...")
          ws_client.connect(ws)
        }
        _, _ -> Nil
      }
      actor.continue(state)
    }

    BotWsError(error) -> {
      logging.quick_error("[BOT] WebSocket error: " <> error)
      actor.continue(BotState(..state, error_count: state.error_count + 1))
    }

    WsUpdate(data) -> {
      // Parse the update from JSON
      case parse_update(data) {
        Ok(update) -> {
          let update_id = get_update_id(update)
          case set.contains(state.processed_updates, update_id) {
            True -> actor.continue(state)
            False -> {
              // Process update
              process_update(state, update)
              actor.continue(BotState(
                ..state,
                processed_updates: set.insert(state.processed_updates, update_id),
                messages_processed: state.messages_processed + 1,
              ))
            }
          }
        }
        Error(err) -> {
          logging.quick_warn("[BOT] Failed to parse update: " <> err)
          actor.continue(state)
        }
      }
    }

    ProcessUpdate(update) -> {
      let update_id = get_update_id(update)
      case set.contains(state.processed_updates, update_id) {
        True -> actor.continue(state)
        False -> {
          process_update(state, update)
          actor.continue(
            BotState(
              ..state,
              processed_updates: set.insert(state.processed_updates, update_id),
              messages_processed: state.messages_processed + 1,
            ),
          )
        }
      }
    }

    SendResponse(chat_id, response) -> {
      let _ = send_response(state.config, chat_id, response)
      actor.continue(state)
    }

    GetStatus(reply_to) -> {
      let connected = case state.ws_client {
        Some(ws) -> ws_client.get_status(ws).connected
        None -> False
      }
      let status =
        BotStatus(
          is_running: state.is_running,
          connected: connected,
          processed_count: state.messages_processed,
          error_count: state.error_count,
        )
      process.send(reply_to, status)
      actor.continue(state)
    }
  }
}

// ============================================================
// Update Parsing
// ============================================================

fn parse_update(json_str: String) -> Result(Update, String) {
  // Parse JSON update from Go bridge
  // Format: {"type": "message", "update_id": "...", "chat_id": 123, ...}

  case string.contains(json_str, "\"type\":\"callback\"") {
    True -> parse_callback_update(json_str)
    False -> parse_message_update(json_str)
  }
}

fn parse_message_update(json_str: String) -> Result(Update, String) {
  let update_id = extract_string_field(json_str, "update_id")
  let chat_id = extract_string_field(json_str, "chat_id")
  let user_id = extract_int_field(json_str, "user_id")
  let username = case extract_string_field(json_str, "username") {
    "" -> None
    u -> Some(u)
  }
  let text = case extract_string_field(json_str, "text") {
    "" -> None
    t -> Some(t)
  }
  let photo_file_id = case extract_string_field(json_str, "photo_file_id") {
    "" -> None
    p -> Some(p)
  }
  let caption = case extract_string_field(json_str, "caption") {
    "" -> None
    c -> Some(c)
  }

  case update_id, chat_id {
    "", _ -> Error("Missing update_id")
    _, "" -> Error("Missing chat_id")
    _, _ ->
      Ok(MessageUpdate(
        update_id: update_id,
        chat_id: chat_id,
        user_id: user_id,
        username: username,
        text: text,
        photo_file_id: photo_file_id,
        caption: caption,
      ))
  }
}

fn parse_callback_update(json_str: String) -> Result(Update, String) {
  let update_id = extract_string_field(json_str, "update_id")
  let chat_id = extract_string_field(json_str, "chat_id")
  let user_id = extract_int_field(json_str, "user_id")
  let username = case extract_string_field(json_str, "username") {
    "" -> None
    u -> Some(u)
  }
  let callback_data = extract_string_field(json_str, "callback_data")
  let message_id = extract_int_field(json_str, "message_id")

  case update_id, chat_id, callback_data {
    "", _, _ -> Error("Missing update_id")
    _, "", _ -> Error("Missing chat_id")
    _, _, "" -> Error("Missing callback_data")
    _, _, _ ->
      Ok(CallbackUpdate(
        update_id: update_id,
        chat_id: chat_id,
        user_id: user_id,
        username: username,
        callback_data: callback_data,
        message_id: message_id,
      ))
  }
}

fn extract_string_field(json_str: String, field: String) -> String {
  let pattern = "\"" <> field <> "\":\""
  case string.split(json_str, pattern) {
    [_, rest, ..] -> {
      case string.split(rest, "\"") {
        [value, ..] -> value
        _ -> ""
      }
    }
    _ -> ""
  }
}

fn extract_int_field(json_str: String, field: String) -> Int {
  let pattern = "\"" <> field <> "\":"
  case string.split(json_str, pattern) {
    [_, rest, ..] -> {
      let trimmed = string.trim_start(rest)
      let digits =
        trimmed
        |> string.to_graphemes
        |> list.take_while(fn(c) { is_digit(c) })
        |> string.join("")
      case int.parse(digits) {
        Ok(n) -> n
        Error(_) -> 0
      }
    }
    _ -> 0
  }
}

fn is_digit(c: String) -> Bool {
  case c {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

// ============================================================
// Update Processing
// ============================================================

fn get_update_id(update: Update) -> String {
  case update {
    MessageUpdate(id, _, _, _, _, _, _) -> id
    CallbackUpdate(id, _, _, _, _, _) -> id
  }
}

fn process_update(state: BotState, update: Update) -> Nil {
  let #(user_id, chat_id, username, incoming_msg) = case update {
    MessageUpdate(_id, chat_id, user_id, username, text, photo_id, caption) -> {
      let msg = case photo_id {
        Some(file_id) -> PhotoMessage(file_id: file_id, caption: caption)
        None ->
          case text {
            Some(t) -> parse_text_message(t)
            None -> TextMessage(text: "")
          }
      }
      #(user_id, chat_id, username, msg)
    }
    CallbackUpdate(_id, chat_id, user_id, username, data, _msg_id) -> {
      #(user_id, chat_id, username, CallbackQuery(data: data))
    }
  }

  logging.quick_info("[BOT] Processing message from user " <> int.to_string(user_id))

  // Route message through scene system
  case router.route_message(state.db_pool, user_id, chat_id, username, incoming_msg) {
    Ok(router_result) -> {
      case router_result.response {
        Some(response) -> {
          let _ = send_response(state.config, chat_id, response)
          Nil
        }
        None -> Nil
      }
    }
    Error(err) -> {
      logging.quick_error("[BOT] Router error")
      let _ =
        send_response(
          state.config,
          chat_id,
          TextReply("Sorry, something went wrong. Please try again."),
        )
      Nil
    }
  }
}

fn parse_text_message(text: String) -> IncomingMessage {
  case string.starts_with(text, "/") {
    True -> {
      // Parse command
      let without_slash = string.drop_start(text, 1)
      case string.split(without_slash, " ") {
        [cmd] -> Command(cmd: string.lowercase(cmd), args: None)
        [cmd, ..rest] ->
          Command(
            cmd: string.lowercase(cmd),
            args: Some(string.join(rest, " ")),
          )
        _ -> TextMessage(text: text)
      }
    }
    False -> TextMessage(text: text)
  }
}

// ============================================================
// Response Sending
// ============================================================

fn send_response(
  config: BotConfig,
  chat_id: String,
  response: OutgoingMessage,
) -> Result(Nil, String) {
  case response {
    TextReply(text) -> send_text_message(config, chat_id, text, None)

    TextWithKeyboard(text, keyboard) ->
      send_text_message(config, chat_id, text, Some(keyboard))

    PhotosReply(urls, caption) -> {
      case urls {
        [url, ..] -> send_photo(config, chat_id, url, caption)
        [] -> Ok(Nil)
      }
    }

    _ -> Ok(Nil)
  }
}

fn send_text_message(
  config: BotConfig,
  chat_id: String,
  text: String,
  keyboard: Option(List(List(InlineButton))),
) -> Result(Nil, String) {
  let base_body = [
    #("chat_id", json.string(chat_id)),
    #("text", json.string(text)),
  ]

  let body_with_keyboard = case keyboard {
    Some(kb) -> {
      let inline_keyboard =
        json.array(kb, fn(row) {
          json.array(row, fn(btn) {
            json.object([
              #("text", json.string(btn.text)),
              #("callback_data", json.string(btn.callback_data)),
            ])
          })
        })
      [
        #(
          "reply_markup",
          json.object([#("inline_keyboard", inline_keyboard)]),
        ),
        ..base_body
      ]
    }
    None -> base_body
  }

  let body = json.to_string(json.object(body_with_keyboard))

  let url = "http://" <> config.telegram_bridge_host <> ":" <> int.to_string(config.telegram_bridge_port)

  let req =
    request.new()
    |> request.set_method(http.Post)
    |> request.set_host(config.telegram_bridge_host)
    |> request.set_port(config.telegram_bridge_port)
    |> request.set_path("/api/v1/send")
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("X-Session-Id", config.session_id)
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) ->
      case response.status >= 200 && response.status < 300 {
        True -> Ok(Nil)
        False -> Error("Send failed: " <> int.to_string(response.status))
      }
    Error(_) -> Error("Connection failed")
  }
}

fn send_photo(
  config: BotConfig,
  chat_id: String,
  url: String,
  caption: Option(String),
) -> Result(Nil, String) {
  let base_body = [
    #("chat_id", json.string(chat_id)),
    #("photo_url", json.string(url)),
  ]

  let body_with_caption = case caption {
    Some(c) -> [#("caption", json.string(c)), ..base_body]
    None -> base_body
  }

  let body = json.to_string(json.object(body_with_caption))

  let req =
    request.new()
    |> request.set_method(http.Post)
    |> request.set_host(config.telegram_bridge_host)
    |> request.set_port(config.telegram_bridge_port)
    |> request.set_path("/api/v1/send/photo")
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("X-Session-Id", config.session_id)
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) ->
      case response.status >= 200 && response.status < 300 {
        True -> Ok(Nil)
        False -> Error("Send photo failed: " <> int.to_string(response.status))
      }
    Error(_) -> Error("Connection failed")
  }
}

// ============================================================
// Public API
// ============================================================

/// Start the bot
pub fn start_bot(bot: Subject(BotMessage)) -> Nil {
  process.send(bot, Start)
}

/// Stop the bot
pub fn stop_bot(bot: Subject(BotMessage)) -> Nil {
  process.send(bot, Stop)
}

/// Get bot status
pub fn get_status(bot: Subject(BotMessage)) -> BotStatus {
  let reply_subject = process.new_subject()
  process.send(bot, GetStatus(reply_subject))
  case process.receive(reply_subject, 5000) {
    Ok(status) -> status
    Error(_) ->
      BotStatus(is_running: False, connected: False, processed_count: 0, error_count: 0)
  }
}

/// Send a message manually
pub fn send_message(
  bot: Subject(BotMessage),
  chat_id: String,
  response: OutgoingMessage,
) -> Nil {
  process.send(bot, SendResponse(chat_id, response))
}
