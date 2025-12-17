// VIBEE A2A Event Listener
// Real-time WebSocket client for Telegram events
// Part of the autonomous Super Agent system

import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/function
import gleam/http/request
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import gleam/uri

// ============================================================
// Types
// ============================================================

/// Telegram event types received from WebSocket
pub type TelegramEvent {
  NewMessage(message: TelegramMessage)
  EditedMessage(message: TelegramMessage)
  CallbackQuery(query: CallbackQueryData)
  ChannelPost(message: TelegramMessage)
  ConnectionEstablished
  ConnectionLost(reason: String)
  Heartbeat
  UnknownEvent(raw: String)
}

/// Telegram message structure
pub type TelegramMessage {
  TelegramMessage(
    id: Int,
    chat_id: Int,
    from_id: Int,
    from_name: String,
    text: String,
    date: String,
    reply_to_id: Option(Int),
    chat_type: String,
  )
}

/// Callback query from inline buttons
pub type CallbackQueryData {
  CallbackQueryData(
    id: String,
    chat_id: Int,
    from_id: Int,
    from_name: String,
    data: String,
    message_id: Int,
  )
}

/// Event listener configuration
pub type ListenerConfig {
  ListenerConfig(
    session_id: String,
    bridge_url: String,
    reconnect_delay_ms: Int,
    max_reconnect_attempts: Int,
  )
}

/// Event listener state
pub type ListenerState {
  ListenerState(
    config: ListenerConfig,
    connected: Bool,
    reconnect_attempts: Int,
    subscribers: List(Subject(TelegramEvent)),
    last_event_time: Int,
    events_received: Int,
  )
}

/// Messages for the event listener actor
pub type ListenerMessage {
  // Control messages
  Start
  Stop
  Subscribe(Subject(TelegramEvent))
  Unsubscribe(Subject(TelegramEvent))
  GetStatus(reply_to: Subject(ListenerStatus))

  // WebSocket messages
  WebSocketConnected
  WebSocketMessage(data: String)
  WebSocketDisconnected(reason: String)
  WebSocketError(error: String)

  // Internal
  Reconnect
  Tick
}

/// Status report for monitoring
pub type ListenerStatus {
  ListenerStatus(
    connected: Bool,
    subscribers_count: Int,
    events_received: Int,
    reconnect_attempts: Int,
    last_event_time: Int,
  )
}

// ============================================================
// Constructor Functions
// ============================================================

/// Create default configuration
pub fn default_config(session_id: String) -> ListenerConfig {
  ListenerConfig(
    session_id: session_id,
    bridge_url: "ws://localhost:8081",
    reconnect_delay_ms: 5000,
    max_reconnect_attempts: 10,
  )
}

/// Create initial state
pub fn initial_state(config: ListenerConfig) -> ListenerState {
  ListenerState(
    config: config,
    connected: False,
    reconnect_attempts: 0,
    subscribers: [],
    last_event_time: 0,
    events_received: 0,
  )
}

// ============================================================
// Actor Implementation
// ============================================================

/// Start the event listener actor
pub fn start(config: ListenerConfig) -> Result(Subject(ListenerMessage), actor.StartError) {
  actor.new(initial_state(config))
  |> actor.on_message(handle_message)
  |> actor.start
  |> result.map(fn(started) { started.data })
}

/// Handle incoming messages
fn handle_message(
  state: ListenerState,
  message: ListenerMessage,
) -> actor.Next(ListenerState, ListenerMessage) {
  case message {
    Start -> {
      // Initiate WebSocket connection
      let ws_url = build_websocket_url(state.config)
      // In real implementation, this would use a WebSocket client library
      // For now, we'll use HTTP polling as fallback
      start_polling(state.config)
      actor.continue(ListenerState(..state, reconnect_attempts: 0))
    }

    Stop -> {
      // Notify subscribers of shutdown
      broadcast_event(state.subscribers, ConnectionLost("Listener stopped"))
      actor.stop()
    }

    Subscribe(subscriber) -> {
      let new_subscribers = [subscriber, ..state.subscribers]
      actor.continue(ListenerState(..state, subscribers: new_subscribers))
    }

    Unsubscribe(subscriber) -> {
      let new_subscribers = list.filter(state.subscribers, fn(s) { s != subscriber })
      actor.continue(ListenerState(..state, subscribers: new_subscribers))
    }

    GetStatus(reply_to) -> {
      let status = ListenerStatus(
        connected: state.connected,
        subscribers_count: list.length(state.subscribers),
        events_received: state.events_received,
        reconnect_attempts: state.reconnect_attempts,
        last_event_time: state.last_event_time,
      )
      process.send(reply_to, status)
      actor.continue(state)
    }

    WebSocketConnected -> {
      broadcast_event(state.subscribers, ConnectionEstablished)
      actor.continue(ListenerState(..state, connected: True, reconnect_attempts: 0))
    }

    WebSocketMessage(data) -> {
      let event = parse_telegram_event(data)
      broadcast_event(state.subscribers, event)
      let new_state = ListenerState(
        ..state,
        events_received: state.events_received + 1,
        last_event_time: erlang_timestamp(),
      )
      actor.continue(new_state)
    }

    WebSocketDisconnected(reason) -> {
      broadcast_event(state.subscribers, ConnectionLost(reason))
      let new_state = ListenerState(
        ..state,
        connected: False,
        reconnect_attempts: state.reconnect_attempts + 1,
      )
      // Schedule reconnect if under limit
      case state.reconnect_attempts < state.config.max_reconnect_attempts {
        True -> schedule_reconnect(state.config.reconnect_delay_ms)
        False -> Nil
      }
      actor.continue(new_state)
    }

    WebSocketError(error) -> {
      broadcast_event(state.subscribers, ConnectionLost("Error: " <> error))
      actor.continue(ListenerState(..state, connected: False))
    }

    Reconnect -> {
      let ws_url = build_websocket_url(state.config)
      start_polling(state.config)
      actor.continue(state)
    }

    Tick -> {
      // Periodic health check
      broadcast_event(state.subscribers, Heartbeat)
      actor.continue(state)
    }
  }
}

// ============================================================
// WebSocket URL Building
// ============================================================

fn build_websocket_url(config: ListenerConfig) -> String {
  config.bridge_url <> "/api/v1/updates?session_id=" <> config.session_id
}

// ============================================================
// Event Parsing
// ============================================================

/// Parse raw WebSocket message into TelegramEvent
pub fn parse_telegram_event(data: String) -> TelegramEvent {
  case json.parse(data, event_decoder()) {
    Ok(event) -> event
    Error(_) -> UnknownEvent(data)
  }
}

/// JSON decoder for Telegram events - simplified approach
fn event_decoder() -> decode.Decoder(TelegramEvent) {
  use event_type <- decode.field("type", decode.string)

  case event_type {
    "new_message" | "message" -> {
      use msg <- decode.field("message", message_decoder())
      decode.success(NewMessage(msg))
    }
    "edited_message" -> {
      use msg <- decode.field("message", message_decoder())
      decode.success(EditedMessage(msg))
    }
    "channel_post" -> {
      use msg <- decode.field("message", message_decoder())
      decode.success(ChannelPost(msg))
    }
    "callback_query" -> callback_query_decoder()
    "heartbeat" | "ping" -> decode.success(Heartbeat)
    _ -> decode.success(UnknownEvent(event_type))
  }
}

fn message_decoder() -> decode.Decoder(TelegramMessage) {
  use id <- decode.field("id", decode.int)
  use chat_id <- decode.field("chat_id", decode.int)
  use from_id <- decode.optional_field("from_id", 0, decode.int)
  use from_name <- decode.optional_field("from_name", "", decode.string)
  use text <- decode.optional_field("text", "", decode.string)
  use date <- decode.optional_field("date", "", decode.string)
  use reply_to <- decode.optional_field("reply_to_id", None, decode.optional(decode.int))
  use chat_type <- decode.optional_field("chat_type", "", decode.string)

  decode.success(TelegramMessage(
    id: id,
    chat_id: chat_id,
    from_id: from_id,
    from_name: from_name,
    text: text,
    date: date,
    reply_to_id: reply_to,
    chat_type: chat_type,
  ))
}

fn callback_query_decoder() -> decode.Decoder(TelegramEvent) {
  use id <- decode.field("id", decode.string)
  use chat_id <- decode.field("chat_id", decode.int)
  use from_id <- decode.field("from_id", decode.int)
  use from_name <- decode.optional_field("from_name", "", decode.string)
  use data <- decode.field("data", decode.string)
  use message_id <- decode.field("message_id", decode.int)

  decode.success(CallbackQuery(CallbackQueryData(
    id: id,
    chat_id: chat_id,
    from_id: from_id,
    from_name: from_name,
    data: data,
    message_id: message_id,
  )))
}

// ============================================================
// Broadcasting
// ============================================================

/// Broadcast event to all subscribers
fn broadcast_event(subscribers: List(Subject(TelegramEvent)), event: TelegramEvent) -> Nil {
  list.each(subscribers, fn(sub) {
    process.send(sub, event)
  })
}

// ============================================================
// HTTP Polling Fallback
// ============================================================

/// Start HTTP polling as WebSocket fallback
fn start_polling(config: ListenerConfig) -> Nil {
  // This would spawn a polling process
  // In production, use actual WebSocket client
  Nil
}

fn schedule_reconnect(delay_ms: Int) -> Nil {
  // Schedule reconnect after delay
  Nil
}

// ============================================================
// Utilities
// ============================================================

/// Get current timestamp in seconds
@external(erlang, "vibee_timestamp_ffi", "get_timestamp_seconds")
fn erlang_timestamp() -> Int

// ============================================================
// Event Encoding (for logging/storage)
// ============================================================

/// Encode TelegramEvent to JSON
pub fn encode_event(event: TelegramEvent) -> json.Json {
  case event {
    NewMessage(msg) -> json.object([
      #("type", json.string("new_message")),
      #("message", encode_message(msg)),
    ])

    EditedMessage(msg) -> json.object([
      #("type", json.string("edited_message")),
      #("message", encode_message(msg)),
    ])

    CallbackQuery(query) -> json.object([
      #("type", json.string("callback_query")),
      #("id", json.string(query.id)),
      #("chat_id", json.int(query.chat_id)),
      #("from_id", json.int(query.from_id)),
      #("from_name", json.string(query.from_name)),
      #("data", json.string(query.data)),
      #("message_id", json.int(query.message_id)),
    ])

    ChannelPost(msg) -> json.object([
      #("type", json.string("channel_post")),
      #("message", encode_message(msg)),
    ])

    ConnectionEstablished -> json.object([
      #("type", json.string("connection_established")),
    ])

    ConnectionLost(reason) -> json.object([
      #("type", json.string("connection_lost")),
      #("reason", json.string(reason)),
    ])

    Heartbeat -> json.object([
      #("type", json.string("heartbeat")),
    ])

    UnknownEvent(raw) -> json.object([
      #("type", json.string("unknown")),
      #("raw", json.string(raw)),
    ])
  }
}

fn encode_message(msg: TelegramMessage) -> json.Json {
  json.object([
    #("id", json.int(msg.id)),
    #("chat_id", json.int(msg.chat_id)),
    #("from_id", json.int(msg.from_id)),
    #("from_name", json.string(msg.from_name)),
    #("text", json.string(msg.text)),
    #("date", json.string(msg.date)),
    #("reply_to_id", case msg.reply_to_id {
      Some(id) -> json.int(id)
      None -> json.null()
    }),
    #("chat_type", json.string(msg.chat_type)),
  ])
}

// ============================================================
// Event Classification (for Super Agent)
// ============================================================

/// Classify event for routing to appropriate agent
pub type EventClassification {
  CodeRequest(intent: String)      // Request to write/modify code
  TestRequest(intent: String)      // Request to run tests
  DebugRequest(intent: String)     // Request to debug/fix
  QueryRequest(intent: String)     // Information query
  CommandRequest(command: String)  // Direct command
  ChatMessage                      // Regular chat message
  SystemEvent                      // Internal system event
  Unknown
}

/// Classify a Telegram event for agent routing
pub fn classify_event(event: TelegramEvent) -> EventClassification {
  case event {
    NewMessage(msg) -> classify_message_text(msg.text)
    EditedMessage(msg) -> classify_message_text(msg.text)
    CallbackQuery(query) -> classify_callback(query.data)
    ChannelPost(msg) -> classify_message_text(msg.text)
    ConnectionEstablished -> SystemEvent
    ConnectionLost(_) -> SystemEvent
    Heartbeat -> SystemEvent
    UnknownEvent(_) -> Unknown
  }
}

fn classify_message_text(text: String) -> EventClassification {
  let lower = string.lowercase(text)

  // Check for code-related keywords
  let code_keywords = ["напиши", "создай", "код", "функци", "реализуй", "implement", "write", "create", "code", "function"]
  let test_keywords = ["тест", "проверь", "test", "check", "verify", "coverage"]
  let debug_keywords = ["исправь", "баг", "ошибк", "fix", "bug", "error", "debug"]
  let command_pattern = string.starts_with(text, "/")

  case command_pattern {
    True -> CommandRequest(text)
    False -> {
      case contains_any(lower, code_keywords) {
        True -> CodeRequest(text)
        False -> case contains_any(lower, test_keywords) {
          True -> TestRequest(text)
          False -> case contains_any(lower, debug_keywords) {
            True -> DebugRequest(text)
            False -> ChatMessage
          }
        }
      }
    }
  }
}

fn classify_callback(data: String) -> EventClassification {
  case string.split_once(data, ":") {
    Ok(#("code", intent)) -> CodeRequest(intent)
    Ok(#("test", intent)) -> TestRequest(intent)
    Ok(#("debug", intent)) -> DebugRequest(intent)
    Ok(#("cmd", command)) -> CommandRequest(command)
    _ -> CommandRequest(data)
  }
}

fn contains_any(text: String, keywords: List(String)) -> Bool {
  list.any(keywords, fn(kw) { string.contains(text, kw) })
}

// ============================================================
// Public API
// ============================================================

/// Subscribe to events
pub fn subscribe(listener: Subject(ListenerMessage), subscriber: Subject(TelegramEvent)) -> Nil {
  process.send(listener, Subscribe(subscriber))
}

/// Unsubscribe from events
pub fn unsubscribe(listener: Subject(ListenerMessage), subscriber: Subject(TelegramEvent)) -> Nil {
  process.send(listener, Unsubscribe(subscriber))
}

/// Start listening
pub fn start_listening(listener: Subject(ListenerMessage)) -> Nil {
  process.send(listener, Start)
}

/// Stop listening
pub fn stop_listening(listener: Subject(ListenerMessage)) -> Nil {
  process.send(listener, Stop)
}

/// Get listener status
pub fn get_status(listener: Subject(ListenerMessage)) -> ListenerStatus {
  let reply_subject = process.new_subject()
  process.send(listener, GetStatus(reply_subject))
  // In real code, would use process.receive with timeout
  ListenerStatus(
    connected: False,
    subscribers_count: 0,
    events_received: 0,
    reconnect_attempts: 0,
    last_event_time: 0,
  )
}
