// Event Bus for MCP Tools
// ETS-backed event system with routing and filtering

import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// Event types supported by the system
pub type EventType {
  MessageReceived
  ButtonClicked
  TaskCompleted
  AgentResponse
  ToolCall
  Error
  VoiceTranscribed
  Custom(String)
}

/// Event with metadata
pub type Event {
  Event(
    id: String,
    event_type: EventType,
    payload: json.Json,
    target: Option(String),
    timestamp: Int,
    source: String,
  )
}

/// Subscription for event routing
pub type Subscription {
  Subscription(
    id: String,
    event_types: List(EventType),
    target_filter: Option(String),
    callback_id: String,
  )
}

/// Event query options
pub type EventQuery {
  EventQuery(
    event_type: Option(EventType),
    target: Option(String),
    since_timestamp: Option(Int),
    limit: Int,
  )
}

/// Initialize the event bus (creates ETS table)
@external(erlang, "vibee_events_ffi", "init")
pub fn init() -> Nil

/// Emit an event to the bus
pub fn emit(
  event_type: EventType,
  payload: json.Json,
  target: Option(String),
  source: String,
) -> Result(String, String) {
  let event_id = generate_event_id()
  let timestamp = system_time_ms()

  let event = Event(
    id: event_id,
    event_type: event_type,
    payload: payload,
    target: target,
    timestamp: timestamp,
    source: source,
  )

  // Store in ETS
  store_event(event)

  // Notify subscribers
  notify_subscribers(event)

  Ok(event_id)
}

/// Query events from the bus
pub fn query(q: EventQuery) -> List(Event) {
  let all_events = get_all_events()

  all_events
  |> filter_by_type(q.event_type)
  |> filter_by_target(q.target)
  |> filter_by_timestamp(q.since_timestamp)
  |> list.take(q.limit)
}

/// Get recent events of a specific type
pub fn get_by_type(event_type: EventType, limit: Int) -> List(Event) {
  query(EventQuery(
    event_type: Some(event_type),
    target: None,
    since_timestamp: None,
    limit: limit,
  ))
}

/// Get events for a specific target
pub fn get_for_target(target: String, limit: Int) -> List(Event) {
  query(EventQuery(
    event_type: None,
    target: Some(target),
    since_timestamp: None,
    limit: limit,
  ))
}

/// Subscribe to events
pub fn subscribe(
  event_types: List(EventType),
  target_filter: Option(String),
  callback_id: String,
) -> String {
  let sub_id = generate_subscription_id()
  let sub = Subscription(
    id: sub_id,
    event_types: event_types,
    target_filter: target_filter,
    callback_id: callback_id,
  )
  store_subscription(sub)
  sub_id
}

/// Unsubscribe from events
pub fn unsubscribe(subscription_id: String) -> Nil {
  remove_subscription(subscription_id)
}

/// Clear old events (older than max_age_ms)
pub fn cleanup(max_age_ms: Int) -> Int {
  let cutoff = system_time_ms() - max_age_ms
  cleanup_old_events(cutoff)
}

/// Get event statistics
pub fn stats() -> EventStats {
  get_event_stats()
}

pub type EventStats {
  EventStats(
    total_events: Int,
    events_by_type: List(#(String, Int)),
    oldest_timestamp: Int,
    newest_timestamp: Int,
    active_subscriptions: Int,
  )
}

// ============================================================
// Type conversions
// ============================================================

/// Parse event type from string
pub fn parse_event_type(s: String) -> EventType {
  case string.lowercase(s) {
    "message_received" -> MessageReceived
    "button_clicked" -> ButtonClicked
    "task_completed" -> TaskCompleted
    "agent_response" -> AgentResponse
    "tool_call" -> ToolCall
    "error" -> Error
    "voice_transcribed" -> VoiceTranscribed
    other -> Custom(other)
  }
}

/// Convert event type to string
pub fn event_type_to_string(et: EventType) -> String {
  case et {
    MessageReceived -> "message_received"
    ButtonClicked -> "button_clicked"
    TaskCompleted -> "task_completed"
    AgentResponse -> "agent_response"
    ToolCall -> "tool_call"
    Error -> "error"
    VoiceTranscribed -> "voice_transcribed"
    Custom(s) -> s
  }
}

/// Encode event to JSON
pub fn encode_event(event: Event) -> json.Json {
  json.object([
    #("id", json.string(event.id)),
    #("type", json.string(event_type_to_string(event.event_type))),
    #("payload", event.payload),
    #("target", case event.target {
      Some(t) -> json.string(t)
      None -> json.null()
    }),
    #("timestamp", json.int(event.timestamp)),
    #("source", json.string(event.source)),
  ])
}

/// Get available event types
pub fn available_types() -> List(String) {
  [
    "message_received",
    "button_clicked",
    "task_completed",
    "agent_response",
    "tool_call",
    "error",
    "voice_transcribed",
  ]
}

// ============================================================
// Internal helpers
// ============================================================

fn filter_by_type(events: List(Event), event_type: Option(EventType)) -> List(Event) {
  case event_type {
    None -> events
    Some(et) -> list.filter(events, fn(e) {
      event_type_to_string(e.event_type) == event_type_to_string(et)
    })
  }
}

fn filter_by_target(events: List(Event), target: Option(String)) -> List(Event) {
  case target {
    None -> events
    Some(t) -> list.filter(events, fn(e) {
      case e.target {
        Some(et) -> et == t
        None -> False
      }
    })
  }
}

fn filter_by_timestamp(events: List(Event), since: Option(Int)) -> List(Event) {
  case since {
    None -> events
    Some(ts) -> list.filter(events, fn(e) { e.timestamp >= ts })
  }
}

fn notify_subscribers(event: Event) -> Nil {
  let subs = get_matching_subscriptions(event)
  list.each(subs, fn(sub) {
    // Call the subscriber's callback
    invoke_callback(sub.callback_id, event)
  })
}

fn get_matching_subscriptions(event: Event) -> List(Subscription) {
  get_all_subscriptions()
  |> list.filter(fn(sub) {
    // Check event type match
    let type_match = case sub.event_types {
      [] -> True  // Empty list = all types
      types -> list.any(types, fn(t) {
        event_type_to_string(t) == event_type_to_string(event.event_type)
      })
    }

    // Check target match
    let target_match = case sub.target_filter, event.target {
      None, _ -> True
      Some(filter), Some(target) -> filter == target
      Some(_), None -> False
    }

    type_match && target_match
  })
}

// ============================================================
// FFI declarations
// ============================================================

@external(erlang, "vibee_events_ffi", "store_event")
fn store_event(event: Event) -> Nil

@external(erlang, "vibee_events_ffi", "get_all_events")
fn get_all_events() -> List(Event)

@external(erlang, "vibee_events_ffi", "store_subscription")
fn store_subscription(sub: Subscription) -> Nil

@external(erlang, "vibee_events_ffi", "remove_subscription")
fn remove_subscription(id: String) -> Nil

@external(erlang, "vibee_events_ffi", "get_all_subscriptions")
fn get_all_subscriptions() -> List(Subscription)

@external(erlang, "vibee_events_ffi", "cleanup_old_events")
fn cleanup_old_events(cutoff_timestamp: Int) -> Int

@external(erlang, "vibee_events_ffi", "get_event_stats")
fn get_event_stats() -> EventStats

@external(erlang, "vibee_events_ffi", "generate_event_id")
fn generate_event_id() -> String

@external(erlang, "vibee_events_ffi", "generate_subscription_id")
fn generate_subscription_id() -> String

@external(erlang, "vibee_events_ffi", "system_time_ms")
fn system_time_ms() -> Int

@external(erlang, "vibee_events_ffi", "invoke_callback")
fn invoke_callback(callback_id: String, event: Event) -> Nil
