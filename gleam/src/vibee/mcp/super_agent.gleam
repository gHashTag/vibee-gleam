// VIBEE Super Agent
// Autonomous coordinator for A2A agent system
// Handles event loop, decision making, and agent orchestration

import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string

import vibee/mcp/a2a_protocol.{
  type AgentCard, type A2AMessage, type RegistryMessage, type TaskRequestData,
  type TaskResponseData, type TaskStatus, AgentCard, Normal, Online, Completed, Failed,
  TaskRequest, TaskResponse,
}
import vibee/mcp/agent_memory.{
  type MemoryMessage, type Episode, type Reflection, type Outcome,
  Success, Failure, Partial,
}
import vibee/mcp/event_listener.{
  type TelegramEvent, type EventClassification, type ListenerMessage,
  NewMessage, EditedMessage, CallbackQuery, ChannelPost, ConnectionEstablished,
  ConnectionLost, Heartbeat, UnknownEvent, CodeRequest, TestRequest, DebugRequest,
  QueryRequest, CommandRequest, ChatMessage, SystemEvent, Unknown,
}

// ============================================================
// Types
// ============================================================

/// Super Agent configuration
pub type SuperAgentConfig {
  SuperAgentConfig(
    id: String,
    name: String,
    telegram_session_id: String,
    target_chats: List(Int),            // Chats to respond to
    trigger_words: List(String),        // Words that trigger response
    confidence_threshold: Float,         // Min confidence for auto-action
    max_task_retries: Int,
    task_timeout_ms: Int,
  )
}

/// Super Agent state
pub type SuperAgentState {
  SuperAgentState(
    config: SuperAgentConfig,
    status: SuperAgentStatus,
    registry: Option(Subject(RegistryMessage)),
    memory: Option(Subject(MemoryMessage)),
    listener: Option(Subject(ListenerMessage)),
    active_tasks: Dict(String, ActiveTask),
    stats: SuperAgentStats,
    confidence_scores: Dict(String, Float),  // Per-capability confidence
  )
}

pub type SuperAgentStatus {
  Initializing
  Running
  Paused
  Stopping
  Stopped
  ErrorStatus(message: String)
}

/// Active task tracking
pub type ActiveTask {
  ActiveTask(
    id: String,
    capability: String,
    assigned_agent: String,
    chat_id: Int,
    original_message: String,
    created_at: Int,
    status: TaskStatus,
    retries: Int,
  )
}

/// Statistics
pub type SuperAgentStats {
  SuperAgentStats(
    events_processed: Int,
    tasks_created: Int,
    tasks_completed: Int,
    tasks_failed: Int,
    messages_sent: Int,
    uptime_seconds: Int,
    started_at: Int,
  )
}

/// Super Agent messages
pub type SuperAgentMessage {
  // Lifecycle
  Start
  Stop
  Pause
  Resume
  GetStatus(reply_to: Subject(SuperAgentStatusReport))

  // Event handling
  HandleTelegramEvent(event: TelegramEvent)
  HandleA2AMessage(message: A2AMessage)

  // Task management
  CreateTask(capability: String, input: json.Json, chat_id: Int, original_msg: String)
  TaskCompleted(task_id: String, response: TaskResponseData)
  TaskFailed(task_id: String, reason: String)
  RetryTask(task_id: String)

  // Decision making
  Decide(classification: EventClassification, context: DecisionContext)
  ExecuteDecision(decision: Decision)

  // Learning
  Reflect(task_id: String, outcome: Outcome)
  UpdateConfidence(capability: String, adjustment: Float)

  // Internal
  Tick
  Cleanup
}

pub type DecisionContext {
  DecisionContext(
    chat_id: Int,
    user_id: Int,
    user_name: String,
    message_text: String,
    timestamp: Int,
  )
}

pub type Decision {
  Decision(
    action: DecisionAction,
    confidence: Float,
    reasoning: String,
    capability: Option(String),
  )
}

pub type DecisionAction {
  DelegateToAgent(agent_type: String)
  RespondDirectly(response: String)
  AskForClarification(question: String)
  IgnoreEvent
  EscalateToHuman(reason: String)
}

pub type SuperAgentStatusReport {
  SuperAgentStatusReport(
    id: String,
    status: SuperAgentStatus,
    stats: SuperAgentStats,
    active_tasks_count: Int,
    registered_agents: Int,
    confidence_scores: Dict(String, Float),
  )
}

// ============================================================
// Constructor Functions
// ============================================================

/// Create default configuration
pub fn default_config(telegram_session_id: String) -> SuperAgentConfig {
  SuperAgentConfig(
    id: "super_agent_1",
    name: "VIBEE Super Agent",
    telegram_session_id: telegram_session_id,
    target_chats: [],  // Empty = respond to all
    trigger_words: ["vibee", "vibe", "бот", "агент", "помоги", "сделай", "напиши"],
    confidence_threshold: 0.7,
    max_task_retries: 3,
    task_timeout_ms: 60_000,
  )
}

/// Create initial state
pub fn initial_state(config: SuperAgentConfig) -> SuperAgentState {
  SuperAgentState(
    config: config,
    status: Initializing,
    registry: None,
    memory: None,
    listener: None,
    active_tasks: dict.new(),
    stats: initial_stats(),
    confidence_scores: default_confidence_scores(),
  )
}

fn initial_stats() -> SuperAgentStats {
  SuperAgentStats(
    events_processed: 0,
    tasks_created: 0,
    tasks_completed: 0,
    tasks_failed: 0,
    messages_sent: 0,
    uptime_seconds: 0,
    started_at: erlang_timestamp(),
  )
}

fn default_confidence_scores() -> Dict(String, Float) {
  dict.new()
  |> dict.insert("code_generate", 0.8)
  |> dict.insert("code_refactor", 0.75)
  |> dict.insert("test_run", 0.85)
  |> dict.insert("test_create", 0.7)
  |> dict.insert("debug_build", 0.9)
  |> dict.insert("debug_analyze", 0.75)
  |> dict.insert("debug_fix", 0.65)
}

// ============================================================
// Actor Implementation
// ============================================================

/// Start the Super Agent
pub fn start(config: SuperAgentConfig) -> Result(Subject(SuperAgentMessage), actor.StartError) {
  actor.new(initial_state(config))
  |> actor.on_message(handle_message)
  |> actor.start
  |> result.map(fn(started) { started.data })
}

/// Handle incoming messages
fn handle_message(
  state: SuperAgentState,
  message: SuperAgentMessage,
) -> actor.Next(SuperAgentState, SuperAgentMessage) {
  case message {
    Start -> handle_start(state)
    Stop -> handle_stop(state)
    Pause -> actor.continue(SuperAgentState(..state, status: Paused))
    Resume -> actor.continue(SuperAgentState(..state, status: Running))

    GetStatus(reply_to) -> {
      let report = create_status_report(state)
      process.send(reply_to, report)
      actor.continue(state)
    }

    HandleTelegramEvent(event) -> handle_telegram_event(event, state)
    HandleA2AMessage(msg) -> handle_a2a_message(msg, state)

    CreateTask(capability, input, chat_id, original_msg) ->
      handle_create_task(capability, input, chat_id, original_msg, state)

    TaskCompleted(task_id, response) -> handle_task_completed(task_id, response, state)
    TaskFailed(task_id, reason) -> handle_task_failed(task_id, reason, state)
    RetryTask(task_id) -> handle_retry_task(task_id, state)

    Decide(classification, context) -> handle_decide(classification, context, state)
    ExecuteDecision(decision) -> handle_execute_decision(decision, state)

    Reflect(task_id, outcome) -> handle_reflect(task_id, outcome, state)
    UpdateConfidence(capability, adjustment) -> {
      let current = dict.get(state.confidence_scores, capability)
        |> result.unwrap(0.5)
      let new_confidence = float.clamp(current +. adjustment, 0.0, 1.0)
      let new_scores = dict.insert(state.confidence_scores, capability, new_confidence)
      actor.continue(SuperAgentState(..state, confidence_scores: new_scores))
    }

    Tick -> {
      // Periodic maintenance
      let new_stats = SuperAgentStats(
        ..state.stats,
        uptime_seconds: erlang_timestamp() - state.stats.started_at,
      )
      actor.continue(SuperAgentState(..state, stats: new_stats))
    }

    Cleanup -> {
      // Clean up timed-out tasks
      let now = erlang_timestamp()
      let timeout_threshold = state.config.task_timeout_ms / 1000
      let cleaned_tasks = dict.filter(state.active_tasks, fn(_k, v) {
        now - v.created_at < timeout_threshold
      })
      actor.continue(SuperAgentState(..state, active_tasks: cleaned_tasks))
    }
  }
}

// ============================================================
// Handler Functions
// ============================================================

fn handle_start(state: SuperAgentState) -> actor.Next(SuperAgentState, SuperAgentMessage) {
  // Start subsystems
  // In production, would start registry, memory, and listener here
  let new_state = SuperAgentState(
    ..state,
    status: Running,
    stats: SuperAgentStats(..state.stats, started_at: erlang_timestamp()),
  )
  actor.continue(new_state)
}

fn handle_stop(_state: SuperAgentState) -> actor.Next(SuperAgentState, SuperAgentMessage) {
  // Stop subsystems gracefully
  actor.stop()
}

fn handle_telegram_event(
  event: TelegramEvent,
  state: SuperAgentState,
) -> actor.Next(SuperAgentState, SuperAgentMessage) {
  // Only process if running
  case state.status {
    Running -> {
      let new_stats = SuperAgentStats(
        ..state.stats,
        events_processed: state.stats.events_processed + 1,
      )
      let new_state = SuperAgentState(..state, stats: new_stats)

      // Classify and decide
      let classification = event_listener.classify_event(event)
      let context = extract_context(event)

      case context {
        Some(ctx) -> {
          // Check if we should respond
          case should_respond(event, state.config) {
            True -> {
              let decision = make_decision(classification, ctx, state)
              // Execute immediately if confident enough
              case decision.confidence >=. state.config.confidence_threshold {
                True -> execute_decision(decision, ctx, new_state)
                False -> {
                  // Low confidence - maybe ask for clarification
                  actor.continue(new_state)
                }
              }
            }
            False -> actor.continue(new_state)
          }
        }
        None -> actor.continue(new_state)
      }
    }
    _ -> actor.continue(state)
  }
}

fn handle_a2a_message(
  message: A2AMessage,
  state: SuperAgentState,
) -> actor.Next(SuperAgentState, SuperAgentMessage) {
  case message {
    TaskResponse(response) -> {
      case response.status {
        Completed -> handle_task_completed(response.task_id, response, state)
        Failed -> handle_task_failed(response.task_id, "Task failed", state)
        _ -> actor.continue(state)
      }
    }
    _ -> actor.continue(state)
  }
}

fn handle_create_task(
  capability: String,
  input: json.Json,
  chat_id: Int,
  original_msg: String,
  state: SuperAgentState,
) -> actor.Next(SuperAgentState, SuperAgentMessage) {
  let task_id = generate_task_id()

  // Find agent for capability
  let agent_id = find_agent_for_capability(capability)

  let task = ActiveTask(
    id: task_id,
    capability: capability,
    assigned_agent: agent_id,
    chat_id: chat_id,
    original_message: original_msg,
    created_at: erlang_timestamp(),
    status: a2a_protocol.Pending,
    retries: 0,
  )

  let new_tasks = dict.insert(state.active_tasks, task_id, task)
  let new_stats = SuperAgentStats(
    ..state.stats,
    tasks_created: state.stats.tasks_created + 1,
  )

  // In production, would delegate via A2A protocol here
  // For now, simulate immediate handling

  actor.continue(SuperAgentState(..state, active_tasks: new_tasks, stats: new_stats))
}

fn handle_task_completed(
  task_id: String,
  response: TaskResponseData,
  state: SuperAgentState,
) -> actor.Next(SuperAgentState, SuperAgentMessage) {
  case dict.get(state.active_tasks, task_id) {
    Ok(task) -> {
      // Update stats
      let new_stats = SuperAgentStats(
        ..state.stats,
        tasks_completed: state.stats.tasks_completed + 1,
      )

      // Remove from active tasks
      let new_tasks = dict.delete(state.active_tasks, task_id)

      // Record episode in memory
      // In production, would send to memory actor

      // Send response to Telegram
      // In production, would use telegram_send_message tool

      actor.continue(SuperAgentState(..state, active_tasks: new_tasks, stats: new_stats))
    }
    Error(_) -> actor.continue(state)
  }
}

fn handle_task_failed(
  task_id: String,
  reason: String,
  state: SuperAgentState,
) -> actor.Next(SuperAgentState, SuperAgentMessage) {
  case dict.get(state.active_tasks, task_id) {
    Ok(task) -> {
      // Check if we should retry
      case task.retries < state.config.max_task_retries {
        True -> {
          // Retry with updated count
          let updated_task = ActiveTask(..task, retries: task.retries + 1)
          let new_tasks = dict.insert(state.active_tasks, task_id, updated_task)
          actor.continue(SuperAgentState(..state, active_tasks: new_tasks))
        }
        False -> {
          // Max retries reached, mark as failed
          let new_stats = SuperAgentStats(
            ..state.stats,
            tasks_failed: state.stats.tasks_failed + 1,
          )
          let new_tasks = dict.delete(state.active_tasks, task_id)

          // Update confidence for this capability
          let capability = task.capability
          let current_conf = dict.get(state.confidence_scores, capability)
            |> result.unwrap(0.5)
          let new_conf = float.max(0.0, current_conf -. 0.05)
          let new_scores = dict.insert(state.confidence_scores, capability, new_conf)

          actor.continue(SuperAgentState(
            ..state,
            active_tasks: new_tasks,
            stats: new_stats,
            confidence_scores: new_scores,
          ))
        }
      }
    }
    Error(_) -> actor.continue(state)
  }
}

fn handle_retry_task(
  task_id: String,
  state: SuperAgentState,
) -> actor.Next(SuperAgentState, SuperAgentMessage) {
  case dict.get(state.active_tasks, task_id) {
    Ok(task) -> {
      let updated_task = ActiveTask(
        ..task,
        retries: task.retries + 1,
        status: a2a_protocol.Pending,
      )
      let new_tasks = dict.insert(state.active_tasks, task_id, updated_task)
      // In production, would re-delegate to agent
      actor.continue(SuperAgentState(..state, active_tasks: new_tasks))
    }
    Error(_) -> actor.continue(state)
  }
}

fn handle_decide(
  classification: EventClassification,
  context: DecisionContext,
  state: SuperAgentState,
) -> actor.Next(SuperAgentState, SuperAgentMessage) {
  let decision = make_decision(classification, context, state)
  execute_decision(decision, context, state)
}

fn handle_execute_decision(
  decision: Decision,
  state: SuperAgentState,
) -> actor.Next(SuperAgentState, SuperAgentMessage) {
  // In production, would actually execute the decision
  actor.continue(state)
}

fn handle_reflect(
  task_id: String,
  outcome: Outcome,
  state: SuperAgentState,
) -> actor.Next(SuperAgentState, SuperAgentMessage) {
  // Create reflection
  case dict.get(state.active_tasks, task_id) {
    Ok(task) -> {
      let confidence_adj = case outcome {
        Success(score) -> score *. 0.1
        Failure(_, score) -> score *. -0.1
        Partial(progress, _) -> { progress -. 0.5 } *. 0.05
      }

      let capability = task.capability
      let current = dict.get(state.confidence_scores, capability)
        |> result.unwrap(0.5)
      let new_conf = float.clamp(current +. confidence_adj, 0.0, 1.0)
      let new_scores = dict.insert(state.confidence_scores, capability, new_conf)

      // In production, would store reflection in memory

      actor.continue(SuperAgentState(..state, confidence_scores: new_scores))
    }
    Error(_) -> actor.continue(state)
  }
}

// ============================================================
// Decision Making
// ============================================================

fn make_decision(
  classification: EventClassification,
  context: DecisionContext,
  state: SuperAgentState,
) -> Decision {
  case classification {
    CodeRequest(intent) -> {
      let conf = dict.get(state.confidence_scores, "code_generate")
        |> result.unwrap(0.7)
      Decision(
        action: DelegateToAgent("CodeAgent"),
        confidence: conf,
        reasoning: "Code generation request detected: " <> intent,
        capability: Some("code_generate"),
      )
    }

    TestRequest(intent) -> {
      let conf = dict.get(state.confidence_scores, "test_run")
        |> result.unwrap(0.8)
      Decision(
        action: DelegateToAgent("TestAgent"),
        confidence: conf,
        reasoning: "Test request detected: " <> intent,
        capability: Some("test_run"),
      )
    }

    DebugRequest(intent) -> {
      let conf = dict.get(state.confidence_scores, "debug_analyze")
        |> result.unwrap(0.75)
      Decision(
        action: DelegateToAgent("DebugAgent"),
        confidence: conf,
        reasoning: "Debug request detected: " <> intent,
        capability: Some("debug_analyze"),
      )
    }

    QueryRequest(intent) -> {
      Decision(
        action: RespondDirectly("Searching knowledge base..."),
        confidence: 0.6,
        reasoning: "Query request: " <> intent,
        capability: Some("knowledge_search"),
      )
    }

    CommandRequest(command) -> {
      Decision(
        action: RespondDirectly("Processing command: " <> command),
        confidence: 0.9,
        reasoning: "Direct command received",
        capability: None,
      )
    }

    ChatMessage -> {
      Decision(
        action: IgnoreEvent,
        confidence: 0.95,
        reasoning: "Regular chat message, no action needed",
        capability: None,
      )
    }

    SystemEvent -> {
      Decision(
        action: IgnoreEvent,
        confidence: 1.0,
        reasoning: "System event, handled internally",
        capability: None,
      )
    }

    Unknown -> {
      Decision(
        action: AskForClarification("Не понял запрос. Уточни, пожалуйста."),
        confidence: 0.3,
        reasoning: "Could not classify the request",
        capability: None,
      )
    }
  }
}

fn execute_decision(
  decision: Decision,
  context: DecisionContext,
  state: SuperAgentState,
) -> actor.Next(SuperAgentState, SuperAgentMessage) {
  case decision.action {
    DelegateToAgent(agent_type) -> {
      // Create task for the agent
      let capability = option.unwrap(decision.capability, "unknown")
      let input = json.object([
        #("message", json.string(context.message_text)),
        #("chat_id", json.int(context.chat_id)),
        #("user_id", json.int(context.user_id)),
      ])

      let task_id = generate_task_id()
      let task = ActiveTask(
        id: task_id,
        capability: capability,
        assigned_agent: agent_type,
        chat_id: context.chat_id,
        original_message: context.message_text,
        created_at: erlang_timestamp(),
        status: a2a_protocol.Pending,
        retries: 0,
      )

      let new_tasks = dict.insert(state.active_tasks, task_id, task)
      let new_stats = SuperAgentStats(
        ..state.stats,
        tasks_created: state.stats.tasks_created + 1,
      )

      actor.continue(SuperAgentState(..state, active_tasks: new_tasks, stats: new_stats))
    }

    RespondDirectly(response) -> {
      // Send response to Telegram
      // In production, would use telegram_send_message
      let new_stats = SuperAgentStats(
        ..state.stats,
        messages_sent: state.stats.messages_sent + 1,
      )
      actor.continue(SuperAgentState(..state, stats: new_stats))
    }

    AskForClarification(question) -> {
      // Send clarification request
      let new_stats = SuperAgentStats(
        ..state.stats,
        messages_sent: state.stats.messages_sent + 1,
      )
      actor.continue(SuperAgentState(..state, stats: new_stats))
    }

    IgnoreEvent -> actor.continue(state)

    EscalateToHuman(reason) -> {
      // Log escalation
      actor.continue(state)
    }
  }
}

// ============================================================
// Helper Functions
// ============================================================

fn extract_context(event: TelegramEvent) -> Option(DecisionContext) {
  case event {
    NewMessage(msg) -> Some(DecisionContext(
      chat_id: msg.chat_id,
      user_id: msg.from_id,
      user_name: msg.from_name,
      message_text: msg.text,
      timestamp: erlang_timestamp(),
    ))
    EditedMessage(msg) -> Some(DecisionContext(
      chat_id: msg.chat_id,
      user_id: msg.from_id,
      user_name: msg.from_name,
      message_text: msg.text,
      timestamp: erlang_timestamp(),
    ))
    CallbackQuery(query) -> Some(DecisionContext(
      chat_id: query.chat_id,
      user_id: query.from_id,
      user_name: query.from_name,
      message_text: query.data,
      timestamp: erlang_timestamp(),
    ))
    ChannelPost(msg) -> Some(DecisionContext(
      chat_id: msg.chat_id,
      user_id: 0,
      user_name: "channel",
      message_text: msg.text,
      timestamp: erlang_timestamp(),
    ))
    _ -> None
  }
}

fn should_respond(event: TelegramEvent, config: SuperAgentConfig) -> Bool {
  case event {
    NewMessage(msg) -> {
      // Check if chat is in target list (empty = all)
      let chat_allowed = case config.target_chats {
        [] -> True
        chats -> list.contains(chats, msg.chat_id)
      }

      // Check for trigger words
      let has_trigger = list.any(config.trigger_words, fn(word) {
        string.contains(string.lowercase(msg.text), string.lowercase(word))
      })

      chat_allowed && has_trigger
    }
    CallbackQuery(_) -> True  // Always respond to button callbacks
    _ -> False
  }
}

fn find_agent_for_capability(capability: String) -> String {
  case string.starts_with(capability, "code_") {
    True -> "CodeAgent"
    False -> case string.starts_with(capability, "test_") {
      True -> "TestAgent"
      False -> case string.starts_with(capability, "debug_") {
        True -> "DebugAgent"
        False -> "GeneralAgent"
      }
    }
  }
}

fn generate_task_id() -> String {
  "task_" <> int.to_string(erlang_timestamp()) <> "_" <> int.to_string(erlang_unique_integer())
}

fn create_status_report(state: SuperAgentState) -> SuperAgentStatusReport {
  SuperAgentStatusReport(
    id: state.config.id,
    status: state.status,
    stats: state.stats,
    active_tasks_count: dict.size(state.active_tasks),
    registered_agents: 0,  // Would query registry
    confidence_scores: state.confidence_scores,
  )
}

// ============================================================
// JSON Encoding
// ============================================================

pub fn encode_status_report(report: SuperAgentStatusReport) -> json.Json {
  json.object([
    #("id", json.string(report.id)),
    #("status", encode_status(report.status)),
    #("stats", encode_stats(report.stats)),
    #("active_tasks_count", json.int(report.active_tasks_count)),
    #("registered_agents", json.int(report.registered_agents)),
    #("confidence_scores", encode_confidence_scores(report.confidence_scores)),
  ])
}

fn encode_status(status: SuperAgentStatus) -> json.Json {
  case status {
    Initializing -> json.string("initializing")
    Running -> json.string("running")
    Paused -> json.string("paused")
    Stopping -> json.string("stopping")
    Stopped -> json.string("stopped")
    ErrorStatus(msg) -> json.object([
      #("status", json.string("error")),
      #("message", json.string(msg)),
    ])
  }
}

fn encode_stats(stats: SuperAgentStats) -> json.Json {
  json.object([
    #("events_processed", json.int(stats.events_processed)),
    #("tasks_created", json.int(stats.tasks_created)),
    #("tasks_completed", json.int(stats.tasks_completed)),
    #("tasks_failed", json.int(stats.tasks_failed)),
    #("messages_sent", json.int(stats.messages_sent)),
    #("uptime_seconds", json.int(stats.uptime_seconds)),
    #("started_at", json.int(stats.started_at)),
  ])
}

fn encode_confidence_scores(scores: Dict(String, Float)) -> json.Json {
  dict.to_list(scores)
  |> list.map(fn(pair) { #(pair.0, json.float(pair.1)) })
  |> json.object()
}

pub fn encode_decision(decision: Decision) -> json.Json {
  json.object([
    #("action", encode_decision_action(decision.action)),
    #("confidence", json.float(decision.confidence)),
    #("reasoning", json.string(decision.reasoning)),
    #("capability", case decision.capability {
      Some(c) -> json.string(c)
      None -> json.null()
    }),
  ])
}

fn encode_decision_action(action: DecisionAction) -> json.Json {
  case action {
    DelegateToAgent(agent) -> json.object([
      #("type", json.string("delegate")),
      #("agent", json.string(agent)),
    ])
    RespondDirectly(response) -> json.object([
      #("type", json.string("respond")),
      #("response", json.string(response)),
    ])
    AskForClarification(question) -> json.object([
      #("type", json.string("clarify")),
      #("question", json.string(question)),
    ])
    IgnoreEvent -> json.object([
      #("type", json.string("ignore")),
    ])
    EscalateToHuman(reason) -> json.object([
      #("type", json.string("escalate")),
      #("reason", json.string(reason)),
    ])
  }
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
// Public API
// ============================================================

/// Send a Telegram event to the Super Agent
pub fn send_event(agent: Subject(SuperAgentMessage), event: TelegramEvent) -> Nil {
  process.send(agent, HandleTelegramEvent(event))
}

/// Start the Super Agent
pub fn start_agent(agent: Subject(SuperAgentMessage)) -> Nil {
  process.send(agent, Start)
}

/// Stop the Super Agent
pub fn stop_agent(agent: Subject(SuperAgentMessage)) -> Nil {
  process.send(agent, Stop)
}

/// Get status report
pub fn get_status(agent: Subject(SuperAgentMessage)) -> SuperAgentStatusReport {
  let reply_subject = process.new_subject()
  process.send(agent, GetStatus(reply_subject))
  // In production, would use process.receive with timeout
  SuperAgentStatusReport(
    id: "unknown",
    status: Stopped,
    stats: initial_stats(),
    active_tasks_count: 0,
    registered_agents: 0,
    confidence_scores: dict.new(),
  )
}

/// Trigger reflection for a task
pub fn reflect_on_task(agent: Subject(SuperAgentMessage), task_id: String, outcome: Outcome) -> Nil {
  process.send(agent, Reflect(task_id, outcome))
}
