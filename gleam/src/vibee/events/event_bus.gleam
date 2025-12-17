// VIBEE Event Bus - Real-time event broadcasting with PubSub pattern
// Inspired by: https://gautier.dev/articles/real-time-gleam-chat

import gleam/erlang/process.{type Subject}
import gleam/io
import gleam/int
import gleam/list
import gleam/otp/actor
import gleam/json
import gleam/string

/// Chat types for filtering
pub type ChatType {
  Private
  Group
  Supergroup
  Channel
  Unknown
}

/// Event types that can be broadcast
pub type Event {
  TelegramMessage(
    chat_id: String,
    msg_id: Int,
    sender: String,
    text: String,
    chat_type: ChatType,
    timestamp: Int,
  )
  AgentReply(
    chat_id: String,
    text: String,
    timestamp: Int,
  )
  TriggerDetected(
    chat_id: String,
    trigger: String,
    timestamp: Int,
  )
  LLMRequest(
    model: String,
    prompt_preview: String,
    timestamp: Int,
  )
  LLMResponse(
    model: String,
    response_preview: String,
    status: Int,
    timestamp: Int,
  )
  SystemEvent(
    event_type: String,
    message: String,
    timestamp: Int,
  )
  ErrorEvent(
    error_type: String,
    message: String,
    timestamp: Int,
  )
}

/// Messages for the PubSub actor
pub type PubSubMessage {
  Subscribe(client: Subject(String))
  Unsubscribe(client: Subject(String))
  Publish(event: Event)
  GetSubscriberCount(reply_to: Subject(Int))
}

/// PubSub state - list of connected clients
pub type PubSubState {
  PubSubState(clients: List(Subject(String)))
}

/// Start the event bus actor
pub fn start() -> Result(Subject(PubSubMessage), actor.StartError) {
  case actor.new(PubSubState(clients: []))
       |> actor.on_message(handle_message)
       |> actor.start {
    Ok(started) -> Ok(started.data)
    Error(e) -> Error(e)
  }
}

/// Handle incoming messages to the PubSub
fn handle_message(
  state: PubSubState,
  message: PubSubMessage,
) -> actor.Next(PubSubState, PubSubMessage) {
  case message {
    Subscribe(client) -> {
      // Add new client to the list
      let new_clients = [client, ..state.clients]
      io.println("[EventBus] +1 subscriber (total: " <> int.to_string(list.length(new_clients)) <> ")")
      actor.continue(PubSubState(clients: new_clients))
    }

    Unsubscribe(client) -> {
      // Remove client from the list
      let new_clients = list.filter(state.clients, fn(c) { c != client })
      actor.continue(PubSubState(clients: new_clients))
    }

    Publish(event) -> {
      // Convert event to JSON and broadcast to all clients
      let json_str = event_to_json(event)
      let client_count = list.length(state.clients)
      io.println("[EventBus] Publishing to " <> int.to_string(client_count) <> " clients")
      list.each(state.clients, fn(client) {
        process.send(client, json_str)
      })
      actor.continue(state)
    }

    GetSubscriberCount(reply_to) -> {
      process.send(reply_to, list.length(state.clients))
      actor.continue(state)
    }
  }
}

/// Subscribe a client to receive events
pub fn subscribe(bus: Subject(PubSubMessage), client: Subject(String)) -> Nil {
  process.send(bus, Subscribe(client))
}

/// Unsubscribe a client
pub fn unsubscribe(bus: Subject(PubSubMessage), client: Subject(String)) -> Nil {
  process.send(bus, Unsubscribe(client))
}

/// Publish an event to all subscribers
pub fn publish(bus: Subject(PubSubMessage), event: Event) -> Nil {
  process.send(bus, Publish(event))
}

/// Get the number of connected subscribers
pub fn get_subscriber_count(bus: Subject(PubSubMessage)) -> Int {
  // Simplified: return 1 for now
  // TODO: implement proper call with timeout
  let reply_subject = process.new_subject()
  process.send(bus, GetSubscriberCount(reply_subject))
  1
}

/// Helper to determine chat type from chat_id
pub fn chat_type_from_id(chat_id: String) -> ChatType {
  case string.starts_with(chat_id, "-100") {
    True -> Supergroup
    False -> case string.starts_with(chat_id, "-") {
      True -> Group
      False -> Private
    }
  }
}

/// Convert ChatType to string for JSON
fn chat_type_to_string(ct: ChatType) -> String {
  case ct {
    Private -> "private"
    Group -> "group"
    Supergroup -> "supergroup"
    Channel -> "channel"
    Unknown -> "unknown"
  }
}

/// Helper functions to create events easily
pub fn telegram_message(chat_id: String, msg_id: Int, sender: String, text: String, ts: Int) -> Event {
  TelegramMessage(
    chat_id: chat_id,
    msg_id: msg_id,
    sender: sender,
    text: truncate(text, 200),
    chat_type: chat_type_from_id(chat_id),
    timestamp: ts,
  )
}

/// Create telegram message with explicit chat type
pub fn telegram_message_with_type(chat_id: String, msg_id: Int, sender: String, text: String, ct: ChatType, ts: Int) -> Event {
  TelegramMessage(
    chat_id: chat_id,
    msg_id: msg_id,
    sender: sender,
    text: truncate(text, 200),
    chat_type: ct,
    timestamp: ts,
  )
}

pub fn agent_reply(chat_id: String, text: String, ts: Int) -> Event {
  AgentReply(
    chat_id: chat_id,
    text: truncate(text, 200),
    timestamp: ts,
  )
}

pub fn trigger_detected(chat_id: String, trigger: String, ts: Int) -> Event {
  TriggerDetected(
    chat_id: chat_id,
    trigger: trigger,
    timestamp: ts,
  )
}

pub fn llm_request(model: String, prompt: String, ts: Int) -> Event {
  LLMRequest(
    model: model,
    prompt_preview: truncate(prompt, 100),
    timestamp: ts,
  )
}

pub fn llm_response(model: String, response: String, status: Int, ts: Int) -> Event {
  LLMResponse(
    model: model,
    response_preview: truncate(response, 100),
    status: status,
    timestamp: ts,
  )
}

pub fn system_event(event_type: String, message: String, ts: Int) -> Event {
  SystemEvent(
    event_type: event_type,
    message: message,
    timestamp: ts,
  )
}

pub fn error_event(error_type: String, message: String, ts: Int) -> Event {
  ErrorEvent(
    error_type: error_type,
    message: message,
    timestamp: ts,
  )
}

/// Convert event to JSON string
fn event_to_json(event: Event) -> String {
  case event {
    TelegramMessage(chat_id, msg_id, sender, text, chat_type, timestamp) ->
      json.object([
        #("type", json.string("telegram_message")),
        #("chat_id", json.string(chat_id)),
        #("msg_id", json.int(msg_id)),
        #("sender", json.string(sender)),
        #("text", json.string(text)),
        #("chat_type", json.string(chat_type_to_string(chat_type))),
        #("timestamp", json.int(timestamp)),
      ])
      |> json.to_string()

    AgentReply(chat_id, text, timestamp) ->
      json.object([
        #("type", json.string("agent_reply")),
        #("chat_id", json.string(chat_id)),
        #("text", json.string(text)),
        #("timestamp", json.int(timestamp)),
      ])
      |> json.to_string()

    TriggerDetected(chat_id, trigger, timestamp) ->
      json.object([
        #("type", json.string("trigger_detected")),
        #("chat_id", json.string(chat_id)),
        #("trigger", json.string(trigger)),
        #("timestamp", json.int(timestamp)),
      ])
      |> json.to_string()

    LLMRequest(model, prompt_preview, timestamp) ->
      json.object([
        #("type", json.string("llm_request")),
        #("model", json.string(model)),
        #("prompt_preview", json.string(prompt_preview)),
        #("timestamp", json.int(timestamp)),
      ])
      |> json.to_string()

    LLMResponse(model, response_preview, status, timestamp) ->
      json.object([
        #("type", json.string("llm_response")),
        #("model", json.string(model)),
        #("response_preview", json.string(response_preview)),
        #("status", json.int(status)),
        #("timestamp", json.int(timestamp)),
      ])
      |> json.to_string()

    SystemEvent(event_type, message, timestamp) ->
      json.object([
        #("type", json.string("system")),
        #("event_type", json.string(event_type)),
        #("message", json.string(message)),
        #("timestamp", json.int(timestamp)),
      ])
      |> json.to_string()

    ErrorEvent(error_type, message, timestamp) ->
      json.object([
        #("type", json.string("error")),
        #("error_type", json.string(error_type)),
        #("message", json.string(message)),
        #("timestamp", json.int(timestamp)),
      ])
      |> json.to_string()
  }
}

/// Truncate string to max length
fn truncate(s: String, max: Int) -> String {
  case string.length(s) > max {
    True -> string.slice(s, 0, max) <> "..."
    False -> s
  }
}
