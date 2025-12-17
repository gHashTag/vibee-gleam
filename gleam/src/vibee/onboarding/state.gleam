// Onboarding State Types for VIBEE MCP
// Manages client onboarding flow after MCP connection

import gleam/option.{type Option}
import gleam/dynamic.{type Dynamic}
import gleam/json

/// Onboarding status enum
pub type OnboardingStatus {
  StatusPending
  StatusScanning
  StatusWaitingSelection
  StatusEmbedding
  StatusCompleted
  StatusFailed
}

/// Progress tracking
pub type OnboardingProgress {
  OnboardingProgress(
    dialogs_found: Int,
    dialogs_scanned: Int,
    messages_parsed: Int,
    embeddings_generated: Int,
    current_dialog: Option(String),
  )
}

/// Dialog info for selection
pub type DialogInfo {
  DialogInfo(
    id: Int,
    title: String,
    dialog_type: String,
    messages_count: Int,
    username: Option(String),
  )
}

/// Main onboarding state
pub type OnboardingState {
  OnboardingState(
    id: String,
    session_id: String,
    phone: String,
    status: OnboardingStatus,
    auto_select: Bool,
    dialogs: List(DialogInfo),
    selected_dialog_ids: List(Int),
    progress: OnboardingProgress,
    started_at: Int,
    completed_at: Option(Int),
    error: Option(String),
  )
}

/// Convert status to string
pub fn status_to_string(status: OnboardingStatus) -> String {
  case status {
    StatusPending -> "pending"
    StatusScanning -> "scanning"
    StatusWaitingSelection -> "waiting_selection"
    StatusEmbedding -> "embedding"
    StatusCompleted -> "completed"
    StatusFailed -> "failed"
  }
}

/// Parse status from string
pub fn status_from_string(s: String) -> OnboardingStatus {
  case s {
    "pending" -> StatusPending
    "scanning" -> StatusScanning
    "waiting_selection" -> StatusWaitingSelection
    "embedding" -> StatusEmbedding
    "completed" -> StatusCompleted
    "failed" -> StatusFailed
    _ -> StatusPending
  }
}

/// Convert progress to JSON
pub fn progress_to_json(p: OnboardingProgress) -> json.Json {
  json.object([
    #("dialogs_found", json.int(p.dialogs_found)),
    #("dialogs_scanned", json.int(p.dialogs_scanned)),
    #("messages_parsed", json.int(p.messages_parsed)),
    #("embeddings_generated", json.int(p.embeddings_generated)),
    #("current_dialog", case p.current_dialog {
      option.Some(d) -> json.string(d)
      option.None -> json.null()
    }),
  ])
}

/// Convert dialog info to JSON
pub fn dialog_to_json(d: DialogInfo) -> json.Json {
  json.object([
    #("id", json.int(d.id)),
    #("title", json.string(d.title)),
    #("type", json.string(d.dialog_type)),
    #("messages_count", json.int(d.messages_count)),
    #("username", case d.username {
      option.Some(u) -> json.string(u)
      option.None -> json.null()
    }),
  ])
}

/// Convert state to JSON for MCP response
pub fn state_to_json(state: OnboardingState) -> json.Json {
  json.object([
    #("id", json.string(state.id)),
    #("session_id", json.string(state.session_id)),
    #("status", json.string(status_to_string(state.status))),
    #("progress", progress_to_json(state.progress)),
    #("dialogs", json.array(state.dialogs, dialog_to_json)),
    #("selected_dialog_ids", json.array(state.selected_dialog_ids, json.int)),
    #("started_at", json.int(state.started_at)),
    #("completed_at", case state.completed_at {
      option.Some(t) -> json.int(t)
      option.None -> json.null()
    }),
    #("error", case state.error {
      option.Some(e) -> json.string(e)
      option.None -> json.null()
    }),
  ])
}

/// Create initial state
pub fn new(id: String, session_id: String, phone: String, auto_select: Bool, started_at: Int) -> OnboardingState {
  OnboardingState(
    id: id,
    session_id: session_id,
    phone: phone,
    status: StatusPending,
    auto_select: auto_select,
    dialogs: [],
    selected_dialog_ids: [],
    progress: OnboardingProgress(
      dialogs_found: 0,
      dialogs_scanned: 0,
      messages_parsed: 0,
      embeddings_generated: 0,
      current_dialog: option.None,
    ),
    started_at: started_at,
    completed_at: option.None,
    error: option.None,
  )
}

// FFI bindings to Erlang ETS module
@external(erlang, "vibee_onboarding_ffi", "init")
pub fn init_store() -> Nil

@external(erlang, "vibee_onboarding_ffi", "generate_id")
pub fn generate_id() -> String

@external(erlang, "vibee_onboarding_ffi", "get_timestamp")
pub fn get_timestamp() -> Int

@external(erlang, "vibee_onboarding_ffi", "create")
pub fn create_in_store(session_id: String, phone: String, auto_select: Bool) -> #(atom, String)

@external(erlang, "vibee_onboarding_ffi", "get")
pub fn get_from_store(id: String) -> Dynamic

@external(erlang, "vibee_onboarding_ffi", "update_status")
pub fn update_status_in_store(id: String, status: Atom) -> Dynamic

@external(erlang, "vibee_onboarding_ffi", "update_progress")
pub fn update_progress_in_store(id: String, dialogs_scanned: Int, messages_parsed: Int, current_dialog: Dynamic) -> Dynamic

@external(erlang, "vibee_onboarding_ffi", "set_dialogs_list")
pub fn set_dialogs_in_store(id: String, dialogs: Dynamic) -> Dynamic

@external(erlang, "vibee_onboarding_ffi", "set_selected_dialogs")
pub fn set_selected_in_store(id: String, dialog_ids: List(Int)) -> Dynamic

@external(erlang, "vibee_onboarding_ffi", "complete")
pub fn complete_in_store(id: String, embeddings_count: Int) -> Dynamic

@external(erlang, "vibee_onboarding_ffi", "fail")
pub fn fail_in_store(id: String, error: String) -> Dynamic

@external(erlang, "vibee_onboarding_ffi", "list_active")
pub fn list_active_in_store() -> Dynamic

@external(erlang, "vibee_onboarding_ffi", "delete")
pub fn delete_from_store(id: String) -> Bool

// Helper type for atom
pub type Atom
