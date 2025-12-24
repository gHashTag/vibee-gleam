// Session Database Module
// CRUD operations for telegram_sessions table
// Database-driven session management instead of ENV variables

import gleam/dynamic/decode.{type Decoder}
import gleam/list
import gleam/option.{type Option, None, Some}
import pog
import vibee/db/postgres.{type DbError, type DbPool, DbQueryError, DbNotFound}

// =============================================================================
// Types
// =============================================================================

/// Telegram session record from database
pub type TelegramSession {
  TelegramSession(
    session_id: String,
    phone: Option(String),
    username: Option(String),
    polling_enabled: Bool,
    owner_id: Int,
  )
}

// =============================================================================
// Private Helpers
// =============================================================================

fn add_parameters(query: pog.Query(t), params: List(pog.Value)) -> pog.Query(t) {
  list.fold(params, query, fn(q, p) { pog.parameter(q, p) })
}

fn decode_session() -> Decoder(TelegramSession) {
  use session_id <- decode.field(0, decode.string)
  use phone <- decode.field(1, decode.optional(decode.string))
  use username <- decode.field(2, decode.optional(decode.string))
  use polling_enabled <- decode.field(3, decode.bool)
  use owner_id <- decode.field(4, decode.int)
  decode.success(TelegramSession(
    session_id: session_id,
    phone: phone,
    username: username,
    polling_enabled: polling_enabled,
    owner_id: owner_id,
  ))
}

// =============================================================================
// CRUD Operations
// =============================================================================

/// Get all sessions with polling_enabled = true
pub fn get_active_sessions(pool: DbPool) -> Result(List(TelegramSession), DbError) {
  let sql = "SELECT session_id, phone, username, polling_enabled, owner_id
             FROM telegram_sessions WHERE polling_enabled = true
             ORDER BY created_at ASC"
  case pog.query(sql) |> pog.returning(decode_session()) |> pog.execute(pool) {
    Ok(pog.Returned(_, sessions)) -> Ok(sessions)
    Error(_) -> Error(DbQueryError("Failed to get active sessions"))
  }
}

/// Get all sessions (including disabled)
pub fn get_all_sessions(pool: DbPool) -> Result(List(TelegramSession), DbError) {
  let sql = "SELECT session_id, phone, username, polling_enabled, owner_id
             FROM telegram_sessions ORDER BY created_at ASC"
  case pog.query(sql) |> pog.returning(decode_session()) |> pog.execute(pool) {
    Ok(pog.Returned(_, sessions)) -> Ok(sessions)
    Error(_) -> Error(DbQueryError("Failed to get all sessions"))
  }
}

/// Get a single session by ID
pub fn get_session(pool: DbPool, session_id: String) -> Result(TelegramSession, DbError) {
  let sql = "SELECT session_id, phone, username, polling_enabled, owner_id
             FROM telegram_sessions WHERE session_id = $1"
  let params = [pog.text(session_id)]
  case pog.query(sql) |> add_parameters(params) |> pog.returning(decode_session()) |> pog.execute(pool) {
    Ok(pog.Returned(_, [session])) -> Ok(session)
    Ok(pog.Returned(_, [])) -> Error(DbNotFound)
    Ok(_) -> Error(DbQueryError("Multiple sessions found"))
    Error(_) -> Error(DbQueryError("Failed to get session"))
  }
}

/// Insert or update a session
pub fn upsert_session(pool: DbPool, session: TelegramSession) -> Result(Nil, DbError) {
  let sql = "INSERT INTO telegram_sessions (session_id, phone, username, polling_enabled, owner_id)
             VALUES ($1, $2, $3, $4, $5)
             ON CONFLICT (session_id) DO UPDATE SET
               phone = EXCLUDED.phone,
               username = EXCLUDED.username,
               polling_enabled = EXCLUDED.polling_enabled,
               owner_id = EXCLUDED.owner_id,
               updated_at = NOW()"
  let params = [
    pog.text(session.session_id),
    pog.nullable(pog.text, session.phone),
    pog.nullable(pog.text, session.username),
    pog.bool(session.polling_enabled),
    pog.int(session.owner_id),
  ]
  case pog.query(sql) |> add_parameters(params) |> pog.execute(pool) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error(DbQueryError("Failed to upsert session"))
  }
}

/// Enable or disable polling for a session
pub fn set_polling_enabled(pool: DbPool, session_id: String, enabled: Bool) -> Result(Nil, DbError) {
  let sql = "UPDATE telegram_sessions SET polling_enabled = $2, updated_at = NOW()
             WHERE session_id = $1"
  let params = [pog.text(session_id), pog.bool(enabled)]
  case pog.query(sql) |> add_parameters(params) |> pog.execute(pool) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error(DbQueryError("Failed to update polling status"))
  }
}

/// Delete a session
pub fn delete_session(pool: DbPool, session_id: String) -> Result(Nil, DbError) {
  let sql = "DELETE FROM telegram_sessions WHERE session_id = $1"
  let params = [pog.text(session_id)]
  case pog.query(sql) |> add_parameters(params) |> pog.execute(pool) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error(DbQueryError("Failed to delete session"))
  }
}
