// Session Manager for VIBEE Multi-Account Telegram Support
// ETS-backed storage for active session management

import gleam/option.{type Option, None, Some}

/// Session information stored in ETS
pub type SessionInfo {
  SessionInfo(
    session_id: String,
    phone: Option(String),
    username: Option(String),
    authorized: Bool,
    created_at: Int,
  )
}

/// Initialize the session store (call once at startup)
@external(erlang, "vibee_session_ffi", "init")
pub fn init() -> Nil

/// Get active session ID
@external(erlang, "vibee_session_ffi", "get_active")
pub fn get_active() -> Option(String)

/// Set active session
@external(erlang, "vibee_session_ffi", "set_active")
pub fn set_active(session_id: String) -> Nil

/// Clear active session
@external(erlang, "vibee_session_ffi", "clear_active")
pub fn clear_active() -> Nil

/// Internal: upsert session raw
@external(erlang, "vibee_session_ffi", "upsert_session")
fn upsert_session_raw(
  session_id: String,
  phone: Option(String),
  username: Option(String),
  authorized: Bool,
  created_at: Int,
) -> Nil

/// Upsert session info
pub fn upsert(info: SessionInfo) -> Nil {
  upsert_session_raw(
    info.session_id,
    info.phone,
    info.username,
    info.authorized,
    info.created_at,
  )
}

/// Internal: get session raw
@external(erlang, "vibee_session_ffi", "get_session")
fn get_session_raw(
  session_id: String,
) -> Option(#(String, Option(String), Option(String), Bool, Int))

/// Get session by ID
pub fn get(session_id: String) -> Option(SessionInfo) {
  case get_session_raw(session_id) {
    Some(#(sid, phone, username, authorized, created_at)) ->
      Some(SessionInfo(sid, phone, username, authorized, created_at))
    None -> None
  }
}

/// Internal: list sessions raw
@external(erlang, "vibee_session_ffi", "list_sessions")
fn list_sessions_raw() -> List(
  #(String, Option(String), Option(String), Bool, Int),
)

/// List all sessions
pub fn list_all() -> List(SessionInfo) {
  list_sessions_raw()
  |> list_map(fn(tuple) {
    let #(sid, phone, username, authorized, created_at) = tuple
    SessionInfo(sid, phone, username, authorized, created_at)
  })
}

fn list_map(
  list: List(a),
  mapper: fn(a) -> b,
) -> List(b) {
  case list {
    [] -> []
    [first, ..rest] -> [mapper(first), ..list_map(rest, mapper)]
  }
}

/// Remove session by ID
@external(erlang, "vibee_session_ffi", "remove_session")
pub fn remove(session_id: String) -> Bool

/// Resolve session ID: use provided or fall back to active
/// Returns Error if no session available
pub fn resolve_session(provided: Option(String)) -> Result(String, String) {
  case provided {
    Some(sid) -> Ok(sid)
    None ->
      case get_active() {
        Some(active_sid) -> Ok(active_sid)
        None ->
          Error(
            "No session_id provided and no active session set. "
            <> "Use session_list to see available sessions, "
            <> "session_set_active to set default, "
            <> "or pass session_id explicitly.",
          )
      }
  }
}

/// Check if session_id is the active one
pub fn is_active(session_id: String) -> Bool {
  case get_active() {
    Some(active) -> active == session_id
    None -> False
  }
}
