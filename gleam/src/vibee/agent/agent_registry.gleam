// Agent Registry - Centralized registry for all active VIBEE agents
// Uses ETS for fast in-memory storage with real-time updates

import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}

// =============================================================================
// Types
// =============================================================================

/// Type of agent
pub type AgentType {
  PollingAgent
  WebSocketAgent
  GenericAgent
  SuperAgent
  EventBusAgent
}

/// Agent status
pub type AgentStatus {
  Starting
  Running
  Paused
  Stopping
  Stopped
  Failed(String)
}

/// Agent information stored in registry
pub type AgentInfo {
  AgentInfo(
    id: String,
    agent_type: AgentType,
    status: AgentStatus,
    started_at: String,
    last_activity: String,
    message_count: Int,
    error_count: Int,
    session_id: Option(String),
    extra: List(#(String, json.Json)),
  )
}

// =============================================================================
// Registry API
// =============================================================================

/// Initialize the agent registry (must be called before use)
pub fn init() -> Nil {
  ffi_init()
}

/// Register a new agent
pub fn register(info: AgentInfo) -> Nil {
  ffi_register(
    info.id,
    agent_type_to_string(info.agent_type),
    status_to_string(info.status),
    info.started_at,
    info.last_activity,
    info.message_count,
    info.error_count,
    option.unwrap(info.session_id, ""),
  )
}

/// Unregister an agent
pub fn unregister(agent_id: String) -> Nil {
  ffi_unregister(agent_id)
}

/// Update agent status
pub fn update_status(agent_id: String, status: AgentStatus) -> Nil {
  ffi_update_status(agent_id, status_to_string(status))
}

/// Update last activity timestamp
pub fn update_activity(agent_id: String) -> Nil {
  ffi_update_activity(agent_id)
}

/// Increment message count
pub fn increment_messages(agent_id: String) -> Nil {
  ffi_increment_messages(agent_id)
}

/// Increment error count
pub fn increment_errors(agent_id: String) -> Nil {
  ffi_increment_errors(agent_id)
}

/// Reset message and error counts
pub fn reset_counts(agent_id: String) -> Nil {
  ffi_reset_counts(agent_id)
}

/// Get all registered agents
pub fn list_all() -> List(AgentInfo) {
  ffi_list_all()
  |> list.filter_map(parse_agent_tuple)
}

/// Get a specific agent by ID
pub fn get(agent_id: String) -> Option(AgentInfo) {
  case ffi_get(agent_id) {
    Some(tuple) -> option.from_result(parse_agent_tuple(tuple))
    None -> None
  }
}

/// Get count of running agents
pub fn count_running() -> Int {
  list_all()
  |> list.filter(fn(a) {
    case a.status {
      Running -> True
      _ -> False
    }
  })
  |> list.length
}

/// Get count of all agents
pub fn count_all() -> Int {
  list.length(list_all())
}

// =============================================================================
// Helpers
// =============================================================================

fn agent_type_to_string(t: AgentType) -> String {
  case t {
    PollingAgent -> "polling"
    WebSocketAgent -> "websocket"
    GenericAgent -> "generic"
    SuperAgent -> "super"
    EventBusAgent -> "event_bus"
  }
}

fn string_to_agent_type(s: String) -> AgentType {
  case s {
    "polling" -> PollingAgent
    "websocket" -> WebSocketAgent
    "generic" -> GenericAgent
    "super" -> SuperAgent
    "event_bus" -> EventBusAgent
    _ -> GenericAgent
  }
}

fn status_to_string(s: AgentStatus) -> String {
  case s {
    Starting -> "starting"
    Running -> "running"
    Paused -> "paused"
    Stopping -> "stopping"
    Stopped -> "stopped"
    Failed(msg) -> "error:" <> msg
  }
}

fn string_to_status(s: String) -> AgentStatus {
  case s {
    "starting" -> Starting
    "running" -> Running
    "paused" -> Paused
    "stopping" -> Stopping
    "stopped" -> Stopped
    _ ->
      case s {
        "error:" <> msg -> Failed(msg)
        _ -> Stopped
      }
  }
}

fn parse_agent_tuple(
  tuple: #(String, String, String, String, String, Int, Int, String),
) -> Result(AgentInfo, Nil) {
  let #(id, agent_type, status, started_at, last_activity, msg_count, err_count, session_id) =
    tuple
  Ok(AgentInfo(
    id: id,
    agent_type: string_to_agent_type(agent_type),
    status: string_to_status(status),
    started_at: started_at,
    last_activity: last_activity,
    message_count: msg_count,
    error_count: err_count,
    session_id: case session_id {
      "" -> None
      s -> Some(s)
    },
    extra: [],
  ))
}

/// Convert agent info to JSON
pub fn to_json(info: AgentInfo) -> json.Json {
  let base = [
    #("id", json.string(info.id)),
    #("type", json.string(agent_type_to_string(info.agent_type))),
    #("status", json.string(status_to_string(info.status))),
    #("started_at", json.string(info.started_at)),
    #("last_activity", json.string(info.last_activity)),
    #("message_count", json.int(info.message_count)),
    #("error_count", json.int(info.error_count)),
  ]

  let with_session = case info.session_id {
    Some(sid) -> [#("session_id", json.string(sid)), ..base]
    None -> base
  }

  json.object(with_session)
}

// =============================================================================
// FFI - Erlang functions
// =============================================================================

@external(erlang, "vibee_agent_registry_ffi", "init")
fn ffi_init() -> Nil

@external(erlang, "vibee_agent_registry_ffi", "register_agent")
fn ffi_register(
  id: String,
  agent_type: String,
  status: String,
  started_at: String,
  last_activity: String,
  message_count: Int,
  error_count: Int,
  session_id: String,
) -> Nil

@external(erlang, "vibee_agent_registry_ffi", "unregister_agent")
fn ffi_unregister(id: String) -> Nil

@external(erlang, "vibee_agent_registry_ffi", "update_status")
fn ffi_update_status(id: String, status: String) -> Nil

@external(erlang, "vibee_agent_registry_ffi", "update_activity")
fn ffi_update_activity(id: String) -> Nil

@external(erlang, "vibee_agent_registry_ffi", "increment_messages")
fn ffi_increment_messages(id: String) -> Nil

@external(erlang, "vibee_agent_registry_ffi", "increment_errors")
fn ffi_increment_errors(id: String) -> Nil

@external(erlang, "vibee_agent_registry_ffi", "list_all")
fn ffi_list_all() -> List(#(String, String, String, String, String, Int, Int, String))

@external(erlang, "vibee_agent_registry_ffi", "get_agent")
fn ffi_get(
  id: String,
) -> Option(#(String, String, String, String, String, Int, Int, String))

@external(erlang, "vibee_agent_registry_ffi", "reset_counts")
fn ffi_reset_counts(id: String) -> Nil
