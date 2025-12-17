// VIBEE A2A Protocol
// Agent-to-Agent communication protocol based on Google A2A
// Enables autonomous agent collaboration

import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string

// ============================================================
// Core Types (Google A2A Inspired)
// ============================================================

/// Agent Card - describes agent capabilities (A2A spec)
pub type AgentCard {
  AgentCard(
    id: String,
    name: String,
    description: String,
    version: String,
    capabilities: List(String),
    endpoint: String,
    status: AgentStatus,
    metadata: Dict(String, String),
  )
}

/// Agent operational status
pub type AgentStatus {
  Online
  Offline
  Busy
  InError(message: String)
}

/// A2A Message Types
pub type A2AMessage {
  // Task lifecycle
  TaskRequest(request: TaskRequestData)
  TaskResponse(response: TaskResponseData)
  TaskProgress(progress: TaskProgressData)
  TaskCancel(task_id: String, reason: String)

  // Discovery
  CapabilityQuery(from: String, capability: String)
  CapabilityResponse(capabilities: List(AgentCard))

  // Health
  Heartbeat(from: String, timestamp: Int)
  HeartbeatAck(from: String, timestamp: Int)

  // Error
  ErrorMessage(from: String, error: A2AError)
}

/// Task request from one agent to another
pub type TaskRequestData {
  TaskRequestData(
    id: String,
    from_agent: String,
    to_agent: String,
    capability: String,
    input: json.Json,
    priority: TaskPriority,
    timeout_ms: Int,
    context: Dict(String, String),
  )
}

/// Task response
pub type TaskResponseData {
  TaskResponseData(
    task_id: String,
    from_agent: String,
    to_agent: String,
    status: TaskStatus,
    output: Option(json.Json),
    error: Option(A2AError),
    duration_ms: Int,
  )
}

/// Task progress update
pub type TaskProgressData {
  TaskProgressData(
    task_id: String,
    from_agent: String,
    progress_percent: Int,
    message: String,
    intermediate_result: Option(json.Json),
  )
}

/// Task priority levels
pub type TaskPriority {
  Critical
  High
  Normal
  Low
  Background
}

/// Task execution status
pub type TaskStatus {
  Pending
  Running
  Completed
  Failed
  Cancelled
  TimedOut
}

/// A2A Error
pub type A2AError {
  A2AError(
    code: String,
    message: String,
    details: Option(String),
  )
}

// ============================================================
// Agent Registry
// ============================================================

/// Registry state for managing agents
pub type RegistryState {
  RegistryState(
    agents: Dict(String, AgentCard),
    tasks: Dict(String, PendingTask),
    subscribers: List(Subject(RegistryEvent)),
  )
}

/// Pending task tracking
pub type PendingTask {
  PendingTask(
    request: TaskRequestData,
    status: TaskStatus,
    created_at: Int,
    updated_at: Int,
    retries: Int,
  )
}

/// Registry events for subscribers
pub type RegistryEvent {
  AgentRegistered(card: AgentCard)
  AgentUpdated(card: AgentCard)
  AgentRemoved(agent_id: String)
  TaskCreated(task_id: String, capability: String)
  TaskCompleted(task_id: String, success: Bool)
}

/// Registry actor messages
pub type RegistryMessage {
  Register(card: AgentCard, reply_to: Subject(Result(Nil, String)))
  Unregister(agent_id: String)
  UpdateStatus(agent_id: String, status: AgentStatus)
  Discover(capability: String, reply_to: Subject(List(AgentCard)))
  GetAgent(agent_id: String, reply_to: Subject(Option(AgentCard)))
  ListAgents(reply_to: Subject(List(AgentCard)))

  // Task management
  SubmitTask(request: TaskRequestData, reply_to: Subject(Result(String, String)))
  CompleteTask(task_id: String, response: TaskResponseData)
  CancelTask(task_id: String, reason: String)
  GetTaskStatus(task_id: String, reply_to: Subject(Option(PendingTask)))

  // Subscription
  Subscribe(Subject(RegistryEvent))
  Unsubscribe(Subject(RegistryEvent))
}

// ============================================================
// Registry Implementation
// ============================================================

/// Create initial registry state
pub fn initial_registry_state() -> RegistryState {
  RegistryState(
    agents: dict.new(),
    tasks: dict.new(),
    subscribers: [],
  )
}

/// Start the agent registry
pub fn start_registry() -> Result(Subject(RegistryMessage), actor.StartError) {
  actor.new(initial_registry_state())
  |> actor.on_message(handle_registry_message)
  |> actor.start
  |> result.map(fn(started) { started.data })
}

/// Handle registry messages
fn handle_registry_message(
  state: RegistryState,
  message: RegistryMessage,
) -> actor.Next(RegistryState, RegistryMessage) {
  case message {
    Register(card, reply_to) -> {
      case dict.has_key(state.agents, card.id) {
        True -> {
          process.send(reply_to, Error("Agent already registered: " <> card.id))
          actor.continue(state)
        }
        False -> {
          let new_agents = dict.insert(state.agents, card.id, card)
          broadcast_registry_event(state.subscribers, AgentRegistered(card))
          process.send(reply_to, Ok(Nil))
          actor.continue(RegistryState(..state, agents: new_agents))
        }
      }
    }

    Unregister(agent_id) -> {
      let new_agents = dict.delete(state.agents, agent_id)
      broadcast_registry_event(state.subscribers, AgentRemoved(agent_id))
      actor.continue(RegistryState(..state, agents: new_agents))
    }

    UpdateStatus(agent_id, status) -> {
      case dict.get(state.agents, agent_id) {
        Ok(card) -> {
          let updated_card = AgentCard(..card, status: status)
          let new_agents = dict.insert(state.agents, agent_id, updated_card)
          broadcast_registry_event(state.subscribers, AgentUpdated(updated_card))
          actor.continue(RegistryState(..state, agents: new_agents))
        }
        Error(Nil) -> actor.continue(state)
      }
    }

    Discover(capability, reply_to) -> {
      let matching = dict.values(state.agents)
        |> list.filter(fn(card) {
          list.contains(card.capabilities, capability) &&
          card.status == Online
        })
      process.send(reply_to, matching)
      actor.continue(state)
    }

    GetAgent(agent_id, reply_to) -> {
      let result = dict.get(state.agents, agent_id)
        |> result.map(Some)
        |> result.unwrap(None)
      process.send(reply_to, result)
      actor.continue(state)
    }

    ListAgents(reply_to) -> {
      process.send(reply_to, dict.values(state.agents))
      actor.continue(state)
    }

    SubmitTask(request, reply_to) -> {
      let task = PendingTask(
        request: request,
        status: Pending,
        created_at: erlang_timestamp(),
        updated_at: erlang_timestamp(),
        retries: 0,
      )
      let new_tasks = dict.insert(state.tasks, request.id, task)
      broadcast_registry_event(state.subscribers, TaskCreated(request.id, request.capability))
      process.send(reply_to, Ok(request.id))
      actor.continue(RegistryState(..state, tasks: new_tasks))
    }

    CompleteTask(task_id, response) -> {
      case dict.get(state.tasks, task_id) {
        Ok(task) -> {
          let updated_task = PendingTask(
            ..task,
            status: response.status,
            updated_at: erlang_timestamp(),
          )
          let new_tasks = dict.insert(state.tasks, task_id, updated_task)
          let success = response.status == Completed
          broadcast_registry_event(state.subscribers, TaskCompleted(task_id, success))
          actor.continue(RegistryState(..state, tasks: new_tasks))
        }
        Error(Nil) -> actor.continue(state)
      }
    }

    CancelTask(task_id, _reason) -> {
      case dict.get(state.tasks, task_id) {
        Ok(task) -> {
          let updated_task = PendingTask(..task, status: Cancelled, updated_at: erlang_timestamp())
          let new_tasks = dict.insert(state.tasks, task_id, updated_task)
          broadcast_registry_event(state.subscribers, TaskCompleted(task_id, False))
          actor.continue(RegistryState(..state, tasks: new_tasks))
        }
        Error(Nil) -> actor.continue(state)
      }
    }

    GetTaskStatus(task_id, reply_to) -> {
      let result = dict.get(state.tasks, task_id)
        |> result.map(Some)
        |> result.unwrap(None)
      process.send(reply_to, result)
      actor.continue(state)
    }

    Subscribe(subscriber) -> {
      let new_subscribers = [subscriber, ..state.subscribers]
      actor.continue(RegistryState(..state, subscribers: new_subscribers))
    }

    Unsubscribe(subscriber) -> {
      let new_subscribers = list.filter(state.subscribers, fn(s) { s != subscriber })
      actor.continue(RegistryState(..state, subscribers: new_subscribers))
    }
  }
}

fn broadcast_registry_event(subscribers: List(Subject(RegistryEvent)), event: RegistryEvent) -> Nil {
  list.each(subscribers, fn(sub) { process.send(sub, event) })
}

// ============================================================
// Predefined Agent Types
// ============================================================

/// Create a Code Agent card
pub fn code_agent_card(id: String) -> AgentCard {
  AgentCard(
    id: id,
    name: "CodeAgent",
    description: "Generates and refactors code",
    version: "1.0.0",
    capabilities: ["code_generate", "code_refactor", "code_explain", "code_review"],
    endpoint: "local://" <> id,
    status: Online,
    metadata: dict.new(),
  )
}

/// Create a Test Agent card
pub fn test_agent_card(id: String) -> AgentCard {
  AgentCard(
    id: id,
    name: "TestAgent",
    description: "Runs tests and validates code",
    version: "1.0.0",
    capabilities: ["test_run", "test_create", "test_coverage", "test_validate"],
    endpoint: "local://" <> id,
    status: Online,
    metadata: dict.new(),
  )
}

/// Create a Debug Agent card
pub fn debug_agent_card(id: String) -> AgentCard {
  AgentCard(
    id: id,
    name: "DebugAgent",
    description: "Debugs and fixes code issues",
    version: "1.0.0",
    capabilities: ["debug_build", "debug_analyze", "debug_fix", "debug_heal"],
    endpoint: "local://" <> id,
    status: Online,
    metadata: dict.new(),
  )
}

/// Create a Telegram Agent card
pub fn telegram_agent_card(id: String) -> AgentCard {
  AgentCard(
    id: id,
    name: "TelegramAgent",
    description: "Monitors and responds to Telegram messages",
    version: "1.0.0",
    capabilities: ["telegram_monitor", "telegram_respond", "telegram_analyze"],
    endpoint: "local://" <> id,
    status: Online,
    metadata: dict.new(),
  )
}

// ============================================================
// Task Creation Helpers
// ============================================================

/// Create a new task request
pub fn create_task_request(
  from: String,
  to: String,
  capability: String,
  input: json.Json,
) -> TaskRequestData {
  TaskRequestData(
    id: generate_task_id(),
    from_agent: from,
    to_agent: to,
    capability: capability,
    input: input,
    priority: Normal,
    timeout_ms: 30_000,
    context: dict.new(),
  )
}

/// Create task with priority
pub fn create_priority_task(
  from: String,
  to: String,
  capability: String,
  input: json.Json,
  priority: TaskPriority,
) -> TaskRequestData {
  TaskRequestData(
    ..create_task_request(from, to, capability, input),
    priority: priority,
  )
}

fn generate_task_id() -> String {
  "task_" <> int.to_string(erlang_timestamp()) <> "_" <> random_suffix()
}

fn random_suffix() -> String {
  int.to_string(erlang_unique_integer())
}

// ============================================================
// JSON Encoding
// ============================================================

/// Encode AgentCard to JSON
pub fn encode_agent_card(card: AgentCard) -> json.Json {
  json.object([
    #("id", json.string(card.id)),
    #("name", json.string(card.name)),
    #("description", json.string(card.description)),
    #("version", json.string(card.version)),
    #("capabilities", json.array(card.capabilities, json.string)),
    #("endpoint", json.string(card.endpoint)),
    #("status", encode_agent_status(card.status)),
    #("metadata", encode_metadata(card.metadata)),
  ])
}

fn encode_agent_status(status: AgentStatus) -> json.Json {
  case status {
    Online -> json.string("online")
    Offline -> json.string("offline")
    Busy -> json.string("busy")
    InError(msg) -> json.object([
      #("status", json.string("error")),
      #("message", json.string(msg)),
    ])
  }
}

fn encode_metadata(metadata: Dict(String, String)) -> json.Json {
  dict.to_list(metadata)
  |> list.map(fn(pair) { #(pair.0, json.string(pair.1)) })
  |> json.object()
}

/// Encode A2A message
pub fn encode_a2a_message(message: A2AMessage) -> json.Json {
  case message {
    TaskRequest(req) -> json.object([
      #("type", json.string("task_request")),
      #("request", encode_task_request(req)),
    ])

    TaskResponse(resp) -> json.object([
      #("type", json.string("task_response")),
      #("response", encode_task_response(resp)),
    ])

    TaskProgress(prog) -> json.object([
      #("type", json.string("task_progress")),
      #("task_id", json.string(prog.task_id)),
      #("from_agent", json.string(prog.from_agent)),
      #("progress_percent", json.int(prog.progress_percent)),
      #("message", json.string(prog.message)),
    ])

    TaskCancel(task_id, reason) -> json.object([
      #("type", json.string("task_cancel")),
      #("task_id", json.string(task_id)),
      #("reason", json.string(reason)),
    ])

    CapabilityQuery(from, capability) -> json.object([
      #("type", json.string("capability_query")),
      #("from", json.string(from)),
      #("capability", json.string(capability)),
    ])

    CapabilityResponse(capabilities) -> json.object([
      #("type", json.string("capability_response")),
      #("capabilities", json.array(capabilities, encode_agent_card)),
    ])

    Heartbeat(from, timestamp) -> json.object([
      #("type", json.string("heartbeat")),
      #("from", json.string(from)),
      #("timestamp", json.int(timestamp)),
    ])

    HeartbeatAck(from, timestamp) -> json.object([
      #("type", json.string("heartbeat_ack")),
      #("from", json.string(from)),
      #("timestamp", json.int(timestamp)),
    ])

    ErrorMessage(from, error) -> json.object([
      #("type", json.string("error")),
      #("from", json.string(from)),
      #("error", encode_a2a_error(error)),
    ])
  }
}

fn encode_task_request(req: TaskRequestData) -> json.Json {
  json.object([
    #("id", json.string(req.id)),
    #("from_agent", json.string(req.from_agent)),
    #("to_agent", json.string(req.to_agent)),
    #("capability", json.string(req.capability)),
    #("input", req.input),
    #("priority", encode_priority(req.priority)),
    #("timeout_ms", json.int(req.timeout_ms)),
  ])
}

fn encode_task_response(resp: TaskResponseData) -> json.Json {
  json.object([
    #("task_id", json.string(resp.task_id)),
    #("from_agent", json.string(resp.from_agent)),
    #("to_agent", json.string(resp.to_agent)),
    #("status", encode_task_status(resp.status)),
    #("output", case resp.output {
      Some(out) -> out
      None -> json.null()
    }),
    #("duration_ms", json.int(resp.duration_ms)),
  ])
}

fn encode_priority(priority: TaskPriority) -> json.Json {
  case priority {
    Critical -> json.string("critical")
    High -> json.string("high")
    Normal -> json.string("normal")
    Low -> json.string("low")
    Background -> json.string("background")
  }
}

fn encode_task_status(status: TaskStatus) -> json.Json {
  case status {
    Pending -> json.string("pending")
    Running -> json.string("running")
    Completed -> json.string("completed")
    Failed -> json.string("failed")
    Cancelled -> json.string("cancelled")
    TimedOut -> json.string("timed_out")
  }
}

fn encode_a2a_error(error: A2AError) -> json.Json {
  json.object([
    #("code", json.string(error.code)),
    #("message", json.string(error.message)),
    #("details", case error.details {
      Some(d) -> json.string(d)
      None -> json.null()
    }),
  ])
}

// ============================================================
// JSON Decoding
// ============================================================

/// Decode AgentCard from JSON
pub fn decode_agent_card(data: json.Json) -> Result(AgentCard, String) {
  case json.parse(json.to_string(data), agent_card_decoder()) {
    Ok(card) -> Ok(card)
    Error(_err) -> Error("Failed to decode AgentCard")
  }
}

fn agent_card_decoder() -> decode.Decoder(AgentCard) {
  use id <- decode.field("id", decode.string)
  use name <- decode.field("name", decode.string)
  use description <- decode.field("description", decode.string)
  use version <- decode.field("version", decode.string)
  use capabilities <- decode.field("capabilities", decode.list(decode.string))
  use endpoint <- decode.field("endpoint", decode.string)

  decode.success(AgentCard(
    id: id,
    name: name,
    description: description,
    version: version,
    capabilities: capabilities,
    endpoint: endpoint,
    status: Online,
    metadata: dict.new(),
  ))
}

// ============================================================
// Utilities
// ============================================================

/// Get current timestamp in seconds
@external(erlang, "vibee_timestamp_ffi", "get_timestamp_seconds")
fn erlang_timestamp() -> Int

@external(erlang, "erlang", "unique_integer")
fn erlang_unique_integer() -> Int

// ============================================================
// Public API Functions
// ============================================================

/// Register an agent with the registry
pub fn register_agent(
  registry: Subject(RegistryMessage),
  card: AgentCard,
) -> Result(Nil, String) {
  let reply_subject = process.new_subject()
  process.send(registry, Register(card, reply_subject))
  // In production, use process.receive with timeout
  Ok(Nil)
}

/// Discover agents by capability
pub fn discover_agents(
  registry: Subject(RegistryMessage),
  capability: String,
) -> List(AgentCard) {
  let reply_subject = process.new_subject()
  process.send(registry, Discover(capability, reply_subject))
  // In production, use process.receive with timeout
  []
}

/// Submit a task to an agent
pub fn submit_task(
  registry: Subject(RegistryMessage),
  request: TaskRequestData,
) -> Result(String, String) {
  let reply_subject = process.new_subject()
  process.send(registry, SubmitTask(request, reply_subject))
  // In production, use process.receive with timeout
  Ok(request.id)
}

/// List all registered agents
pub fn list_all_agents(registry: Subject(RegistryMessage)) -> List(AgentCard) {
  let reply_subject = process.new_subject()
  process.send(registry, ListAgents(reply_subject))
  // In production, use process.receive with timeout
  []
}
