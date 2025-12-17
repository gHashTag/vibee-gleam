// Audit Logging for MCP Tool Calls
// Records all tool invocations with timing, results, and sanitized args

import gleam/json
import gleam/string

/// Audit log entry
pub type AuditEntry {
  AuditEntry(
    timestamp: Int,
    session_id: String,
    tool_name: String,
    arguments_hash: String,  // SHA256 of args for privacy
    result_status: ResultStatus,
    duration_ms: Int,
    error_message: String,
  )
}

/// Result status for audit
pub type ResultStatus {
  Success
  Failure
  RateLimited
  CircuitOpen
}

/// Start timing a tool call
pub fn start_timer() -> Int {
  system_time_ms()
}

/// Calculate duration from start time
pub fn get_duration(start_time: Int) -> Int {
  system_time_ms() - start_time
}

/// Log a tool call (audit entry)
pub fn log_tool_call(
  tool_name: String,
  args: json.Json,
  status: ResultStatus,
  duration_ms: Int,
  error_msg: String,
) -> Nil {
  let entry = AuditEntry(
    timestamp: system_time_ms(),
    session_id: get_session_id(),
    tool_name: tool_name,
    arguments_hash: hash_arguments(args),
    result_status: status,
    duration_ms: duration_ms,
    error_message: sanitize_error(error_msg),
  )

  write_audit_entry(entry)
}

/// Log a successful tool call
pub fn log_success(tool_name: String, args: json.Json, duration_ms: Int) -> Nil {
  log_tool_call(tool_name, args, Success, duration_ms, "")
}

/// Log a failed tool call
pub fn log_failure(tool_name: String, args: json.Json, duration_ms: Int, error: String) -> Nil {
  log_tool_call(tool_name, args, Failure, duration_ms, error)
}

/// Log a rate-limited request
pub fn log_rate_limited(tool_name: String, args: json.Json) -> Nil {
  log_tool_call(tool_name, args, RateLimited, 0, "Rate limit exceeded")
}

/// Log a circuit-open rejection
pub fn log_circuit_open(tool_name: String, args: json.Json, service: String) -> Nil {
  log_tool_call(tool_name, args, CircuitOpen, 0, "Circuit open for " <> service)
}

/// Convert result status to string
pub fn status_to_string(status: ResultStatus) -> String {
  case status {
    Success -> "success"
    Failure -> "failure"
    RateLimited -> "rate_limited"
    CircuitOpen -> "circuit_open"
  }
}

/// Encode audit entry to JSON
pub fn encode_entry(entry: AuditEntry) -> json.Json {
  json.object([
    #("timestamp", json.int(entry.timestamp)),
    #("session_id", json.string(entry.session_id)),
    #("tool_name", json.string(entry.tool_name)),
    #("arguments_hash", json.string(entry.arguments_hash)),
    #("result_status", json.string(status_to_string(entry.result_status))),
    #("duration_ms", json.int(entry.duration_ms)),
    #("error_message", json.string(entry.error_message)),
  ])
}

// ============================================================
// Internal helpers
// ============================================================

/// Hash arguments for privacy (don't log raw args)
fn hash_arguments(args: json.Json) -> String {
  let raw = json.to_string(args)
  hash_string(raw)
}

/// Sanitize error message (remove sensitive info)
fn sanitize_error(msg: String) -> String {
  msg
  |> string.replace("password", "***")
  |> string.replace("token", "***")
  |> string.replace("secret", "***")
  |> string.replace("key", "***")
  |> truncate(500)
}

/// Truncate string to max length
fn truncate(s: String, max: Int) -> String {
  case string.length(s) <= max {
    True -> s
    False -> string.slice(s, 0, max) <> "..."
  }
}

/// Write entry to audit log file
fn write_audit_entry(entry: AuditEntry) -> Nil {
  let line = json.to_string(encode_entry(entry))
  append_to_audit_log(line)
}

// ============================================================
// FFI for timing and file operations
// ============================================================

@external(erlang, "vibee_audit_ffi", "system_time_ms")
fn system_time_ms() -> Int

@external(erlang, "vibee_audit_ffi", "get_session_id")
fn get_session_id() -> String

@external(erlang, "vibee_audit_ffi", "hash_string")
fn hash_string(s: String) -> String

@external(erlang, "vibee_audit_ffi", "append_to_audit_log")
fn append_to_audit_log(line: String) -> Nil
