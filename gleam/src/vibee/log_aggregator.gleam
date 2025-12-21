// Log Aggregator - Centralized log collection and broadcasting
// Collects logs from vibe_logger and broadcasts to WebSocket clients

import gleam/erlang/process.{type Subject}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor

// =============================================================================
// Types
// =============================================================================

/// Log entry structure (matches vibe_logger output)
pub type LogEntry {
  LogEntry(
    timestamp: String,
    level: String,
    logger: String,
    message: String,
    trace_id: Option(String),
    request_id: Option(String),
    session_id: Option(String),
    span_id: Option(String),
    tool: Option(String),
    extra: List(#(String, json.Json)),
  )
}

/// Messages the aggregator can receive
pub type Message {
  /// Add a new log entry
  Log(LogEntry)
  /// Subscribe a WebSocket client to receive logs
  Subscribe(Subject(String))
  /// Unsubscribe a client
  Unsubscribe(Subject(String))
  /// Get recent logs (returns via callback)
  GetRecent(reply_to: Subject(List(LogEntry)), count: Int)
  /// Shutdown
  Shutdown
}

/// Internal state
pub type State {
  State(
    logs: List(LogEntry),
    max_logs: Int,
    subscribers: List(Subject(String)),
  )
}

// =============================================================================
// Actor
// =============================================================================

/// Start the log aggregator actor
pub fn start() -> Result(Subject(Message), actor.StartError) {
  let initial_state = State(logs: [], max_logs: 1000, subscribers: [])

  case actor.new(initial_state)
       |> actor.on_message(handle_message)
       |> actor.start {
    Ok(started) -> Ok(started.data)
    Error(e) -> Error(e)
  }
}

/// Handle incoming messages
fn handle_message(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    Log(entry) -> {
      // Add log to buffer
      let new_logs = [entry, ..state.logs]
        |> list.take(state.max_logs)

      // Broadcast to all subscribers
      let json_entry = log_entry_to_json(entry)
      list.each(state.subscribers, fn(sub) {
        process.send(sub, json_entry)
      })

      actor.continue(State(..state, logs: new_logs))
    }

    Subscribe(client) -> {
      let new_subs = [client, ..state.subscribers]
      actor.continue(State(..state, subscribers: new_subs))
    }

    Unsubscribe(client) -> {
      let new_subs = list.filter(state.subscribers, fn(s) { s != client })
      actor.continue(State(..state, subscribers: new_subs))
    }

    GetRecent(reply_to, count) -> {
      let recent = list.take(state.logs, count)
      process.send(reply_to, recent)
      actor.continue(state)
    }

    Shutdown -> {
      actor.stop()
    }
  }
}

// =============================================================================
// Public API
// =============================================================================

/// Log an entry through the aggregator
pub fn log(aggregator: Subject(Message), entry: LogEntry) -> Nil {
  process.send(aggregator, Log(entry))
}

/// Subscribe to receive log broadcasts
pub fn subscribe(aggregator: Subject(Message), client: Subject(String)) -> Nil {
  process.send(aggregator, Subscribe(client))
}

/// Unsubscribe from log broadcasts
pub fn unsubscribe(aggregator: Subject(Message), client: Subject(String)) -> Nil {
  process.send(aggregator, Unsubscribe(client))
}

/// Get recent logs synchronously
pub fn get_recent(aggregator: Subject(Message), count: Int) -> List(LogEntry) {
  // Create a subject for the reply
  let reply_subject = process.new_subject()
  // Send the request
  process.send(aggregator, GetRecent(reply_subject, count))
  // Wait for reply with 1 second timeout
  case process.receive(reply_subject, 1000) {
    Ok(logs) -> logs
    Error(_) -> []
  }
}

// =============================================================================
// Serialization
// =============================================================================

pub fn log_entry_to_json(entry: LogEntry) -> String {
  let base = [
    #("timestamp", json.string(entry.timestamp)),
    #("level", json.string(entry.level)),
    #("logger", json.string(entry.logger)),
    #("message", json.string(entry.message)),
  ]

  let with_trace = case entry.trace_id {
    Some(id) -> [#("trace_id", json.string(id)), ..base]
    None -> base
  }

  let with_request = case entry.request_id {
    Some(id) -> [#("request_id", json.string(id)), ..with_trace]
    None -> with_trace
  }

  let with_session = case entry.session_id {
    Some(id) -> [#("session_id", json.string(id)), ..with_request]
    None -> with_request
  }

  let with_span = case entry.span_id {
    Some(id) -> [#("span_id", json.string(id)), ..with_session]
    None -> with_session
  }

  let with_tool = case entry.tool {
    Some(t) -> [#("tool", json.string(t)), ..with_span]
    None -> with_span
  }

  let all_fields = list.flatten([with_tool, entry.extra])

  json.object(all_fields)
  |> json.to_string
}

// =============================================================================
// Global Instance (via process registry)
// =============================================================================

/// Start global aggregator and register it
pub fn start_global() -> Result(Subject(Message), actor.StartError) {
  case start() {
    Ok(subject) -> {
      // Register in process registry for global access
      register_aggregator(subject)
      Ok(subject)
    }
    Error(e) -> Error(e)
  }
}

/// Get the global aggregator (if registered)
pub fn get_global() -> Option(Subject(Message)) {
  lookup_aggregator()
}

// FFI for process registry
@external(erlang, "vibee_log_aggregator_ffi", "register_aggregator")
fn register_aggregator(subject: Subject(Message)) -> Nil

@external(erlang, "vibee_log_aggregator_ffi", "lookup_aggregator")
fn lookup_aggregator() -> Option(Subject(Message))
