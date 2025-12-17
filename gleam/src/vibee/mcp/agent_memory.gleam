// VIBEE Agent Memory System
// Hierarchical memory for autonomous agents
// Implements Working, Episodic, Semantic, and Procedural memory
// Plus Reflexion pattern for self-improvement

import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string

// ============================================================
// Memory Types
// ============================================================

/// Memory entry with metadata
pub type MemoryEntry {
  MemoryEntry(
    id: String,
    content: json.Json,
    memory_type: MemoryType,
    created_at: Int,
    accessed_at: Int,
    access_count: Int,
    importance: Float,
    tags: List(String),
    associations: List(String),  // IDs of related memories
  )
}

/// Types of memory in the hierarchy
pub type MemoryType {
  Working    // Current task context, short-lived
  Episodic   // Specific events and experiences
  Semantic   // General knowledge and facts
  Procedural // How to do things, patterns
}

/// Working memory for current task
pub type WorkingMemory {
  WorkingMemory(
    task_id: String,
    context: Dict(String, json.Json),
    goals: List(String),
    current_step: Int,
    intermediate_results: List(json.Json),
    created_at: Int,
    ttl_ms: Int,
  )
}

/// Episodic memory entry - specific event
pub type Episode {
  Episode(
    id: String,
    event_type: String,
    timestamp: Int,
    actors: List(String),        // Agent IDs involved
    input: json.Json,
    output: json.Json,
    outcome: Outcome,
    duration_ms: Int,
    context: Dict(String, String),
  )
}

/// Outcome of an action
pub type Outcome {
  Success(score: Float)
  Failure(reason: String, score: Float)
  Partial(progress: Float, notes: String)
}

/// Semantic memory - factual knowledge
pub type SemanticKnowledge {
  SemanticKnowledge(
    id: String,
    topic: String,
    content: String,
    confidence: Float,
    source: String,
    created_at: Int,
    updated_at: Int,
    usage_count: Int,
  )
}

/// Procedural memory - how to do things
pub type Procedure {
  Procedure(
    id: String,
    name: String,
    description: String,
    capability: String,
    steps: List(ProcedureStep),
    success_rate: Float,
    avg_duration_ms: Int,
    execution_count: Int,
    last_used: Int,
  )
}

pub type ProcedureStep {
  ProcedureStep(
    order: Int,
    action: String,
    parameters: Dict(String, String),
    expected_outcome: String,
    fallback_action: Option(String),
  )
}

/// Reflection entry (Reflexion pattern)
pub type Reflection {
  Reflection(
    id: String,
    task_id: String,
    timestamp: Int,
    what_happened: String,
    what_worked: List(String),
    what_failed: List(String),
    lessons_learned: List(String),
    recommendations: List(String),
    confidence_adjustment: Float,
  )
}

// ============================================================
// Memory Store State
// ============================================================

pub type MemoryState {
  MemoryState(
    working: Dict(String, WorkingMemory),
    episodes: List(Episode),
    semantic: Dict(String, SemanticKnowledge),
    procedures: Dict(String, Procedure),
    reflections: List(Reflection),
    max_episodes: Int,
    max_working_items: Int,
  )
}

/// Memory actor messages
pub type MemoryMessage {
  // Working memory
  CreateWorkingMemory(task_id: String, context: Dict(String, json.Json))
  UpdateWorkingMemory(task_id: String, key: String, value: json.Json)
  GetWorkingMemory(task_id: String, reply_to: Subject(Option(WorkingMemory)))
  ClearWorkingMemory(task_id: String)
  AddIntermediateResult(task_id: String, result: json.Json)

  // Episodic memory
  RecordEpisode(episode: Episode)
  QueryEpisodes(query: EpisodeQuery, reply_to: Subject(List(Episode)))
  GetSimilarEpisodes(event_type: String, limit: Int, reply_to: Subject(List(Episode)))

  // Semantic memory
  StoreKnowledge(knowledge: SemanticKnowledge)
  QueryKnowledge(topic: String, reply_to: Subject(List(SemanticKnowledge)))
  UpdateKnowledgeConfidence(id: String, confidence: Float)

  // Procedural memory
  StoreProcedure(procedure: Procedure)
  GetProcedure(capability: String, reply_to: Subject(Option(Procedure)))
  UpdateProcedureStats(id: String, success: Bool, duration_ms: Int)

  // Reflection
  AddReflection(reflection: Reflection)
  GetReflections(task_id: String, reply_to: Subject(List(Reflection)))
  GetRecentReflections(limit: Int, reply_to: Subject(List(Reflection)))

  // Utility
  GetStats(reply_to: Subject(MemoryStats))
  Cleanup
}

pub type EpisodeQuery {
  EpisodeQuery(
    event_type: Option(String),
    actor: Option(String),
    outcome_type: Option(String),
    time_from: Option(Int),
    time_to: Option(Int),
    limit: Int,
  )
}

pub type MemoryStats {
  MemoryStats(
    working_count: Int,
    episode_count: Int,
    semantic_count: Int,
    procedure_count: Int,
    reflection_count: Int,
    total_size_estimate: Int,
  )
}

// ============================================================
// Memory Store Implementation
// ============================================================

/// Create initial memory state
pub fn initial_memory_state() -> MemoryState {
  MemoryState(
    working: dict.new(),
    episodes: [],
    semantic: dict.new(),
    procedures: dict.new(),
    reflections: [],
    max_episodes: 1000,
    max_working_items: 100,
  )
}

/// Start the memory store
pub fn start_memory() -> Result(Subject(MemoryMessage), actor.StartError) {
  actor.new(initial_memory_state())
  |> actor.on_message(handle_memory_message)
  |> actor.start
  |> result.map(fn(started) { started.data })
}

fn handle_memory_message(
  state: MemoryState,
  message: MemoryMessage,
) -> actor.Next(MemoryState, MemoryMessage) {
  case message {
    // Working Memory
    CreateWorkingMemory(task_id, context) -> {
      let wm = WorkingMemory(
        task_id: task_id,
        context: context,
        goals: [],
        current_step: 0,
        intermediate_results: [],
        created_at: erlang_timestamp(),
        ttl_ms: 3_600_000,  // 1 hour default
      )
      let new_working = dict.insert(state.working, task_id, wm)
      actor.continue(MemoryState(..state, working: new_working))
    }

    UpdateWorkingMemory(task_id, key, value) -> {
      case dict.get(state.working, task_id) {
        Ok(wm) -> {
          let new_context = dict.insert(wm.context, key, value)
          let updated_wm = WorkingMemory(..wm, context: new_context)
          let new_working = dict.insert(state.working, task_id, updated_wm)
          actor.continue(MemoryState(..state, working: new_working))
        }
        Error(_) -> actor.continue(state)
      }
    }

    GetWorkingMemory(task_id, reply_to) -> {
      let result = dict.get(state.working, task_id)
        |> result.map(Some)
        |> result.unwrap(None)
      process.send(reply_to, result)
      actor.continue(state)
    }

    ClearWorkingMemory(task_id) -> {
      let new_working = dict.delete(state.working, task_id)
      actor.continue(MemoryState(..state, working: new_working))
    }

    AddIntermediateResult(task_id, result) -> {
      case dict.get(state.working, task_id) {
        Ok(wm) -> {
          let new_results = list.append(wm.intermediate_results, [result])
          let updated_wm = WorkingMemory(..wm, intermediate_results: new_results)
          let new_working = dict.insert(state.working, task_id, updated_wm)
          actor.continue(MemoryState(..state, working: new_working))
        }
        Error(_) -> actor.continue(state)
      }
    }

    // Episodic Memory
    RecordEpisode(episode) -> {
      let new_episodes = [episode, ..state.episodes]
      // Trim if over limit
      let trimmed = case list.length(new_episodes) > state.max_episodes {
        True -> list.take(new_episodes, state.max_episodes)
        False -> new_episodes
      }
      actor.continue(MemoryState(..state, episodes: trimmed))
    }

    QueryEpisodes(query, reply_to) -> {
      let results = filter_episodes(state.episodes, query)
      process.send(reply_to, results)
      actor.continue(state)
    }

    GetSimilarEpisodes(event_type, limit, reply_to) -> {
      let similar = state.episodes
        |> list.filter(fn(ep) { ep.event_type == event_type })
        |> list.take(limit)
      process.send(reply_to, similar)
      actor.continue(state)
    }

    // Semantic Memory
    StoreKnowledge(knowledge) -> {
      let new_semantic = dict.insert(state.semantic, knowledge.id, knowledge)
      actor.continue(MemoryState(..state, semantic: new_semantic))
    }

    QueryKnowledge(topic, reply_to) -> {
      let results = dict.values(state.semantic)
        |> list.filter(fn(k) { string.contains(string.lowercase(k.topic), string.lowercase(topic)) })
      process.send(reply_to, results)
      actor.continue(state)
    }

    UpdateKnowledgeConfidence(id, confidence) -> {
      case dict.get(state.semantic, id) {
        Ok(k) -> {
          let updated = SemanticKnowledge(..k, confidence: confidence, updated_at: erlang_timestamp())
          let new_semantic = dict.insert(state.semantic, id, updated)
          actor.continue(MemoryState(..state, semantic: new_semantic))
        }
        Error(_) -> actor.continue(state)
      }
    }

    // Procedural Memory
    StoreProcedure(procedure) -> {
      let new_procedures = dict.insert(state.procedures, procedure.capability, procedure)
      actor.continue(MemoryState(..state, procedures: new_procedures))
    }

    GetProcedure(capability, reply_to) -> {
      let result = dict.get(state.procedures, capability)
        |> result.map(Some)
        |> result.unwrap(None)
      process.send(reply_to, result)
      actor.continue(state)
    }

    UpdateProcedureStats(id, success, duration_ms) -> {
      case dict.get(state.procedures, id) {
        Ok(p) -> {
          let new_count = p.execution_count + 1
          let new_success_rate = case success {
            True -> { p.success_rate *. int.to_float(p.execution_count) +. 1.0 } /. int.to_float(new_count)
            False -> { p.success_rate *. int.to_float(p.execution_count) } /. int.to_float(new_count)
          }
          let new_avg = { p.avg_duration_ms * p.execution_count + duration_ms } / new_count
          let updated = Procedure(
            ..p,
            success_rate: new_success_rate,
            avg_duration_ms: new_avg,
            execution_count: new_count,
            last_used: erlang_timestamp(),
          )
          let new_procedures = dict.insert(state.procedures, id, updated)
          actor.continue(MemoryState(..state, procedures: new_procedures))
        }
        Error(_) -> actor.continue(state)
      }
    }

    // Reflections
    AddReflection(reflection) -> {
      let new_reflections = [reflection, ..state.reflections]
      actor.continue(MemoryState(..state, reflections: new_reflections))
    }

    GetReflections(task_id, reply_to) -> {
      let results = list.filter(state.reflections, fn(r) { r.task_id == task_id })
      process.send(reply_to, results)
      actor.continue(state)
    }

    GetRecentReflections(limit, reply_to) -> {
      let results = list.take(state.reflections, limit)
      process.send(reply_to, results)
      actor.continue(state)
    }

    // Utility
    GetStats(reply_to) -> {
      let stats = MemoryStats(
        working_count: dict.size(state.working),
        episode_count: list.length(state.episodes),
        semantic_count: dict.size(state.semantic),
        procedure_count: dict.size(state.procedures),
        reflection_count: list.length(state.reflections),
        total_size_estimate: estimate_memory_size(state),
      )
      process.send(reply_to, stats)
      actor.continue(state)
    }

    Cleanup -> {
      // Remove expired working memory
      let now = erlang_timestamp()
      let cleaned_working = dict.filter(state.working, fn(_k, v) {
        v.created_at + v.ttl_ms / 1000 > now
      })
      actor.continue(MemoryState(..state, working: cleaned_working))
    }
  }
}

fn filter_episodes(episodes: List(Episode), query: EpisodeQuery) -> List(Episode) {
  episodes
  |> list.filter(fn(ep) {
    let type_match = case query.event_type {
      Some(t) -> ep.event_type == t
      None -> True
    }
    let actor_match = case query.actor {
      Some(a) -> list.contains(ep.actors, a)
      None -> True
    }
    let time_from_match = case query.time_from {
      Some(t) -> ep.timestamp >= t
      None -> True
    }
    let time_to_match = case query.time_to {
      Some(t) -> ep.timestamp <= t
      None -> True
    }
    type_match && actor_match && time_from_match && time_to_match
  })
  |> list.take(query.limit)
}

fn estimate_memory_size(state: MemoryState) -> Int {
  // Rough estimate in bytes
  let working_size = dict.size(state.working) * 1000
  let episode_size = list.length(state.episodes) * 500
  let semantic_size = dict.size(state.semantic) * 2000
  let procedure_size = dict.size(state.procedures) * 1500
  let reflection_size = list.length(state.reflections) * 800
  working_size + episode_size + semantic_size + procedure_size + reflection_size
}

// ============================================================
// Reflexion Pattern Implementation
// ============================================================

/// Create a reflection from task outcome
pub fn create_reflection(
  task_id: String,
  outcome: Outcome,
  what_happened: String,
  analysis: ReflectionAnalysis,
) -> Reflection {
  Reflection(
    id: "refl_" <> int.to_string(erlang_timestamp()),
    task_id: task_id,
    timestamp: erlang_timestamp(),
    what_happened: what_happened,
    what_worked: analysis.what_worked,
    what_failed: analysis.what_failed,
    lessons_learned: analysis.lessons,
    recommendations: analysis.recommendations,
    confidence_adjustment: calculate_confidence_adjustment(outcome),
  )
}

pub type ReflectionAnalysis {
  ReflectionAnalysis(
    what_worked: List(String),
    what_failed: List(String),
    lessons: List(String),
    recommendations: List(String),
  )
}

fn calculate_confidence_adjustment(outcome: Outcome) -> Float {
  case outcome {
    Success(score) -> score *. 0.1  // Increase confidence by up to 10%
    Failure(_, score) -> score *. -0.1  // Decrease confidence
    Partial(progress, _) -> { progress -. 0.5 } *. 0.05  // Small adjustment
  }
}

/// Analyze episodes to generate reflection analysis
pub fn analyze_for_reflection(episodes: List(Episode)) -> ReflectionAnalysis {
  let successful = list.filter(episodes, fn(ep) {
    case ep.outcome {
      Success(_) -> True
      _ -> False
    }
  })
  let failed = list.filter(episodes, fn(ep) {
    case ep.outcome {
      Failure(_, _) -> True
      _ -> False
    }
  })

  let what_worked = successful
    |> list.map(fn(ep) { ep.event_type <> " succeeded" })

  let what_failed = failed
    |> list.map(fn(ep) {
      case ep.outcome {
        Failure(reason, _) -> ep.event_type <> ": " <> reason
        _ -> ep.event_type
      }
    })

  let lessons = generate_lessons(successful, failed)
  let recommendations = generate_recommendations(successful, failed)

  ReflectionAnalysis(
    what_worked: what_worked,
    what_failed: what_failed,
    lessons: lessons,
    recommendations: recommendations,
  )
}

fn generate_lessons(successful: List(Episode), failed: List(Episode)) -> List(String) {
  let success_count = list.length(successful)
  let fail_count = list.length(failed)

  case success_count, fail_count {
    s, f if s > f * 2 -> ["Current approach is working well", "Continue with similar patterns"]
    s, f if f > s * 2 -> ["Need to revise approach", "Consider alternative strategies"]
    _, _ -> ["Mixed results - need more data", "Try incremental improvements"]
  }
}

fn generate_recommendations(successful: List(Episode), failed: List(Episode)) -> List(String) {
  case list.length(failed) > 0 {
    True -> [
      "Review failed cases for common patterns",
      "Add more validation before execution",
      "Consider breaking down complex tasks",
    ]
    False -> [
      "Maintain current patterns",
      "Document successful approaches",
      "Explore optimizations",
    ]
  }
}

// ============================================================
// JSON Encoding
// ============================================================

pub fn encode_reflection(r: Reflection) -> json.Json {
  json.object([
    #("id", json.string(r.id)),
    #("task_id", json.string(r.task_id)),
    #("timestamp", json.int(r.timestamp)),
    #("what_happened", json.string(r.what_happened)),
    #("what_worked", json.array(r.what_worked, json.string)),
    #("what_failed", json.array(r.what_failed, json.string)),
    #("lessons_learned", json.array(r.lessons_learned, json.string)),
    #("recommendations", json.array(r.recommendations, json.string)),
    #("confidence_adjustment", json.float(r.confidence_adjustment)),
  ])
}

pub fn encode_episode(ep: Episode) -> json.Json {
  json.object([
    #("id", json.string(ep.id)),
    #("event_type", json.string(ep.event_type)),
    #("timestamp", json.int(ep.timestamp)),
    #("actors", json.array(ep.actors, json.string)),
    #("input", ep.input),
    #("output", ep.output),
    #("outcome", encode_outcome(ep.outcome)),
    #("duration_ms", json.int(ep.duration_ms)),
  ])
}

fn encode_outcome(outcome: Outcome) -> json.Json {
  case outcome {
    Success(score) -> json.object([
      #("type", json.string("success")),
      #("score", json.float(score)),
    ])
    Failure(reason, score) -> json.object([
      #("type", json.string("failure")),
      #("reason", json.string(reason)),
      #("score", json.float(score)),
    ])
    Partial(progress, notes) -> json.object([
      #("type", json.string("partial")),
      #("progress", json.float(progress)),
      #("notes", json.string(notes)),
    ])
  }
}

pub fn encode_memory_stats(stats: MemoryStats) -> json.Json {
  json.object([
    #("working_count", json.int(stats.working_count)),
    #("episode_count", json.int(stats.episode_count)),
    #("semantic_count", json.int(stats.semantic_count)),
    #("procedure_count", json.int(stats.procedure_count)),
    #("reflection_count", json.int(stats.reflection_count)),
    #("total_size_estimate", json.int(stats.total_size_estimate)),
  ])
}

pub fn encode_procedure(p: Procedure) -> json.Json {
  json.object([
    #("id", json.string(p.id)),
    #("name", json.string(p.name)),
    #("description", json.string(p.description)),
    #("capability", json.string(p.capability)),
    #("steps", json.array(p.steps, encode_procedure_step)),
    #("success_rate", json.float(p.success_rate)),
    #("avg_duration_ms", json.int(p.avg_duration_ms)),
    #("execution_count", json.int(p.execution_count)),
    #("last_used", json.int(p.last_used)),
  ])
}

fn encode_procedure_step(step: ProcedureStep) -> json.Json {
  json.object([
    #("order", json.int(step.order)),
    #("action", json.string(step.action)),
    #("expected_outcome", json.string(step.expected_outcome)),
    #("fallback_action", case step.fallback_action {
      Some(a) -> json.string(a)
      None -> json.null()
    }),
  ])
}

// ============================================================
// Utilities
// ============================================================

/// Get current timestamp in seconds
@external(erlang, "vibee_timestamp_ffi", "get_timestamp_seconds")
fn erlang_timestamp() -> Int

// ============================================================
// Public API
// ============================================================

/// Store an episode
pub fn record_episode(memory: Subject(MemoryMessage), episode: Episode) -> Nil {
  process.send(memory, RecordEpisode(episode))
}

/// Add a reflection
pub fn add_reflection(memory: Subject(MemoryMessage), reflection: Reflection) -> Nil {
  process.send(memory, AddReflection(reflection))
}

/// Store a procedure
pub fn store_procedure(memory: Subject(MemoryMessage), procedure: Procedure) -> Nil {
  process.send(memory, StoreProcedure(procedure))
}

/// Store semantic knowledge
pub fn store_knowledge(memory: Subject(MemoryMessage), knowledge: SemanticKnowledge) -> Nil {
  process.send(memory, StoreKnowledge(knowledge))
}

/// Create working memory for a task
pub fn create_task_context(
  memory: Subject(MemoryMessage),
  task_id: String,
  context: Dict(String, json.Json),
) -> Nil {
  process.send(memory, CreateWorkingMemory(task_id, context))
}

/// Update working memory
pub fn update_task_context(
  memory: Subject(MemoryMessage),
  task_id: String,
  key: String,
  value: json.Json,
) -> Nil {
  process.send(memory, UpdateWorkingMemory(task_id, key, value))
}

/// Cleanup expired memory
pub fn cleanup_memory(memory: Subject(MemoryMessage)) -> Nil {
  process.send(memory, Cleanup)
}

/// Create episode from event
pub fn create_episode(
  event_type: String,
  actors: List(String),
  input: json.Json,
  output: json.Json,
  outcome: Outcome,
  duration_ms: Int,
) -> Episode {
  Episode(
    id: "ep_" <> int.to_string(erlang_timestamp()),
    event_type: event_type,
    timestamp: erlang_timestamp(),
    actors: actors,
    input: input,
    output: output,
    outcome: outcome,
    duration_ms: duration_ms,
    context: dict.new(),
  )
}
