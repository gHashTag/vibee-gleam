// Input validation for security and data integrity
// Prevents path traversal, shell injection, and other attacks

import gleam/string
import gleam/list
import gleam/int
import gleam/option.{type Option, None, Some}
import vibee/mcp/session_manager

/// Validation error types
pub type ValidationError {
  EmptyValue(field: String)
  InvalidFormat(field: String, expected: String)
  PathTraversal(path: String)
  UnsafeCommand(command: String)
  ValueTooLong(field: String, max: Int)
  InvalidCharacters(field: String, chars: String)
}

/// Convert validation error to user-friendly string
pub fn error_to_string(err: ValidationError) -> String {
  case err {
    EmptyValue(f) -> "Required field is empty: " <> f
    InvalidFormat(f, e) -> "Invalid format for '" <> f <> "', expected: " <> e
    PathTraversal(p) -> "Invalid path (potential traversal attack): " <> p
    UnsafeCommand(c) -> "Command not allowed: " <> c
    ValueTooLong(f, m) -> "Value too long for '" <> f <> "', max: " <> int.to_string(m)
    InvalidCharacters(f, c) -> "Invalid characters in '" <> f <> "': " <> c
  }
}

// ============================================================
// String Validation
// ============================================================

/// Validate non-empty string
pub fn validate_required(value: String, field: String) -> Result(String, ValidationError) {
  case string.trim(value) {
    "" -> Error(EmptyValue(field))
    v -> Ok(v)
  }
}

/// Validate string with max length
pub fn validate_max_length(value: String, field: String, max: Int) -> Result(String, ValidationError) {
  case string.length(value) <= max {
    True -> Ok(value)
    False -> Error(ValueTooLong(field, max))
  }
}

/// Validate optional string (returns Option if valid)
pub fn validate_optional(value: Option(String)) -> Result(Option(String), ValidationError) {
  case value {
    None -> Ok(None)
    Some("") -> Ok(None)
    Some(v) -> Ok(Some(v))
  }
}

// ============================================================
// Path Validation
// ============================================================

/// Dangerous patterns for path traversal attacks
const dangerous_path_patterns = [
  "..",      // Parent directory traversal
  "//",      // Double slash
  "~",       // Home directory expansion
  "$",       // Variable expansion
  "`",       // Command substitution
  "|",       // Pipe
  ";",       // Command separator
  "&",       // Background/AND
  ">",       // Redirect output
  "<",       // Redirect input
  "$(", // Command substitution
  "${",      // Variable expansion
  "\n",      // Newline injection
  "\r",      // Carriage return
]

/// Validate file path (prevent traversal attacks)
pub fn validate_path(path: String) -> Result(String, ValidationError) {
  let trimmed = string.trim(path)

  case trimmed {
    "" -> Error(EmptyValue("path"))
    p -> {
      let has_dangerous = list.any(dangerous_path_patterns, fn(pattern) {
        string.contains(p, pattern)
      })

      case has_dangerous {
        True -> Error(PathTraversal(p))
        False -> Ok(p)
      }
    }
  }
}

/// Validate path with additional check for absolute paths only
pub fn validate_absolute_path(path: String) -> Result(String, ValidationError) {
  case validate_path(path) {
    Error(e) -> Error(e)
    Ok(p) -> {
      case string.starts_with(p, "/") {
        True -> Ok(p)
        False -> Error(InvalidFormat("path", "absolute path starting with /"))
      }
    }
  }
}

// ============================================================
// Command Validation
// ============================================================

/// Whitelist of allowed shell commands
const allowed_commands = [
  "ls", "cat", "head", "tail", "grep", "wc", "echo", "date", "pwd",
  "find", "sort", "uniq", "tr", "cut", "diff", "stat", "file",
  "gleam", "git", "whisper",  // Project-specific
]

/// Validate shell command against whitelist
pub fn validate_command(command: String) -> Result(String, ValidationError) {
  let parts = string.split(command, " ")
  let cmd_name = case list.first(parts) {
    Ok(name) -> name
    Error(_) -> ""
  }

  case list.contains(allowed_commands, cmd_name) {
    True -> Ok(command)
    False -> Error(UnsafeCommand(cmd_name))
  }
}

/// Sanitize string for safe shell usage (single-quote escaping)
pub fn sanitize_for_shell(s: String) -> String {
  // Wrap in single quotes and escape internal single quotes
  // This is the safest way to pass arguments to shell
  "'" <> string.replace(s, "'", "'\\''") <> "'"
}

/// Sanitize string by removing dangerous characters
pub fn sanitize_string(s: String) -> String {
  s
  |> string.replace("`", "")
  |> string.replace("$", "")
  |> string.replace(";", "")
  |> string.replace("|", "")
  |> string.replace("&", "")
  |> string.replace(">", "")
  |> string.replace("<", "")
  |> string.replace("\n", " ")
  |> string.replace("\r", "")
}

// ============================================================
// Phone Number Validation
// ============================================================

/// Validate phone number format (+XXXXXXXXXXX)
pub fn validate_phone(phone: String) -> Result(String, ValidationError) {
  let cleaned = phone
    |> string.replace(" ", "")
    |> string.replace("-", "")
    |> string.replace("(", "")
    |> string.replace(")", "")

  case string.starts_with(cleaned, "+") {
    True -> {
      let digits = string.drop_start(cleaned, 1)
      let len = string.length(digits)

      case len >= 10 && len <= 15 && is_digits_only(digits) {
        True -> Ok(cleaned)
        False -> Error(InvalidFormat("phone", "+XXXXXXXXXXX (10-15 digits)"))
      }
    }
    False -> Error(InvalidFormat("phone", "+XXXXXXXXXXX (must start with +)"))
  }
}

/// Check if string contains only digits
fn is_digits_only(s: String) -> Bool {
  let chars = string.to_graphemes(s)
  list.all(chars, fn(c) {
    case c {
      "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
      _ -> False
    }
  })
}

// ============================================================
// Session ID Validation
// ============================================================

/// Validate session ID format (for raw string)
pub fn validate_session_id_string(sid: String) -> Result(String, ValidationError) {
  let trimmed = string.trim(sid)
  let len = string.length(trimmed)

  case len > 0 && len <= 128 {
    True -> {
      // Check for safe characters only (alphanumeric, underscore, dash)
      case is_safe_identifier(trimmed) {
        True -> Ok(trimmed)
        False -> Error(InvalidCharacters("session_id", "only alphanumeric, _ and - allowed"))
      }
    }
    False -> Error(InvalidFormat("session_id", "non-empty string, max 128 chars"))
  }
}

/// Resolve and validate session ID
/// If session_id is None, tries to get active session from session_manager
/// Then validates the resolved session ID
pub fn validate_session_id(sid: Option(String)) -> Result(String, ValidationError) {
  case session_manager.resolve_session(sid) {
    Error(err) -> Error(InvalidFormat("session_id", err))
    Ok(resolved_sid) -> validate_session_id_string(resolved_sid)
  }
}

/// Check if string is a safe identifier
fn is_safe_identifier(s: String) -> Bool {
  let chars = string.to_graphemes(s)
  list.all(chars, fn(c) {
    is_alphanumeric(c) || c == "_" || c == "-"
  })
}

/// Check if character is alphanumeric
fn is_alphanumeric(c: String) -> Bool {
  case c {
    "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" |
    "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" |
    "u" | "v" | "w" | "x" | "y" | "z" |
    "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" |
    "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" |
    "U" | "V" | "W" | "X" | "Y" | "Z" |
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

// ============================================================
// Chat ID Validation
// ============================================================

/// Validate Telegram chat_id (numeric or @username)
pub fn validate_chat_id(chat_id: String) -> Result(String, ValidationError) {
  let trimmed = string.trim(chat_id)

  case trimmed {
    "" -> Error(EmptyValue("chat_id"))
    cid -> {
      // Chat ID can be:
      // - Numeric (positive or negative)
      // - @username format
      case string.starts_with(cid, "@") {
        True -> {
          let username = string.drop_start(cid, 1)
          case string.length(username) >= 5 && is_safe_identifier(username) {
            True -> Ok(cid)
            False -> Error(InvalidFormat("chat_id", "@username (min 5 chars, alphanumeric)"))
          }
        }
        False -> {
          // Should be numeric (possibly with leading minus)
          let to_check = case string.starts_with(cid, "-") {
            True -> string.drop_start(cid, 1)
            False -> cid
          }
          case is_digits_only(to_check) {
            True -> Ok(cid)
            False -> Error(InvalidFormat("chat_id", "numeric ID or @username"))
          }
        }
      }
    }
  }
}

// ============================================================
// Enum Validation
// ============================================================

/// Validate build target (erlang/javascript)
pub fn validate_build_target(target: Option(String)) -> String {
  case target {
    Some("javascript") -> "javascript"
    Some("js") -> "javascript"
    _ -> "erlang"  // Default to erlang
  }
}

/// Validate log level
pub fn validate_log_level(level: String) -> Result(String, ValidationError) {
  case string.lowercase(level) {
    "debug" | "info" | "warn" | "warning" | "error" -> Ok(string.lowercase(level))
    _ -> Error(InvalidFormat("level", "debug, info, warn, or error"))
  }
}

/// Validate comparison type for test validation
pub fn validate_comparison_type(comparison: Option(String)) -> String {
  case comparison {
    Some("exact") -> "exact"
    Some("contains") -> "contains"
    Some("regex") -> "regex"
    Some("json_equal") -> "json_equal"
    _ -> "exact"  // Default
  }
}

/// Validate agent status
pub fn validate_agent_status(status: String) -> Result(String, ValidationError) {
  case string.lowercase(status) {
    "start" | "progress" | "success" | "error" -> Ok(string.lowercase(status))
    _ -> Error(InvalidFormat("status", "start, progress, success, or error"))
  }
}

/// Validate refactoring type
pub fn validate_refactoring_type(refactoring: String) -> Result(String, ValidationError) {
  case string.lowercase(refactoring) {
    "rename" | "extract_function" | "inline" | "move" | "simplify" ->
      Ok(string.lowercase(refactoring))
    _ -> Error(InvalidFormat("refactoring", "rename, extract_function, inline, move, or simplify"))
  }
}

/// Validate bot analysis depth
pub fn validate_analysis_depth(depth: Option(String)) -> String {
  case depth {
    Some("quick") -> "quick"
    Some("deep") -> "deep"
    _ -> "standard"  // Default
  }
}

/// Validate monitor action
pub fn validate_monitor_action(action: String) -> Result(String, ValidationError) {
  case string.lowercase(action) {
    "start" | "stop" | "status" | "report" -> Ok(string.lowercase(action))
    _ -> Error(InvalidFormat("action", "start, stop, status, or report"))
  }
}

// ============================================================
// Rate Limiting
// ============================================================

/// Rate limit result
pub type RateLimitResult {
  Allowed
  RateLimited(retry_after_seconds: Int)
}

/// Rate limit error for validation
pub type RateLimitError {
  TooManyRequests(tool: String, limit: Int, window_seconds: Int)
  CircuitOpen(service: String, retry_after: Int)
}

/// Convert rate limit error to string
pub fn rate_limit_error_to_string(err: RateLimitError) -> String {
  case err {
    TooManyRequests(tool, limit, window) ->
      "Rate limit exceeded for '" <> tool <> "': " <>
      int.to_string(limit) <> " requests per " <>
      int.to_string(window) <> " seconds"
    CircuitOpen(service, retry) ->
      "Service '" <> service <> "' is temporarily unavailable. " <>
      "Retry after " <> int.to_string(retry) <> " seconds"
  }
}

/// Check if request is allowed based on rate limit
/// Uses sliding window algorithm via FFI to ETS
pub fn check_rate_limit(tool_name: String, limit_per_minute: Int) -> RateLimitResult {
  let current_count = get_request_count(tool_name)
  case current_count < limit_per_minute {
    True -> {
      increment_request_count(tool_name)
      Allowed
    }
    False -> {
      let retry_after = get_window_reset_seconds(tool_name)
      RateLimited(retry_after)
    }
  }
}

/// Get tool-specific rate limit (some tools have different limits)
pub fn get_tool_rate_limit(tool_name: String, default_limit: Int) -> Int {
  // External API tools have lower limits
  let external_api_tools = [
    "telegram_send_message", "telegram_send_photo", "telegram_send_buttons",
    "system_exec", "agent_spawn", "knowledge_embed",
  ]

  // High-frequency read tools have higher limits
  let high_frequency_tools = [
    "file_read", "file_list", "event_list", "agent_status",
  ]

  case list.contains(external_api_tools, tool_name) {
    True -> default_limit / 2  // Half the default limit for external APIs
    False -> {
      case list.contains(high_frequency_tools, tool_name) {
        True -> default_limit * 2  // Double for read-heavy tools
        False -> default_limit
      }
    }
  }
}

// ============================================================
// Circuit Breaker
// ============================================================

/// Circuit breaker state
pub type CircuitState {
  Closed  // Normal operation
  Open    // Failing, reject requests
  HalfOpen  // Testing if service recovered
}

/// Check circuit breaker for external service
pub fn check_circuit(service_name: String) -> Result(Nil, RateLimitError) {
  let state = get_circuit_state(service_name)
  case state {
    Closed -> Ok(Nil)
    HalfOpen -> Ok(Nil)  // Allow one request to test
    Open -> {
      let retry_after = get_circuit_retry_seconds(service_name)
      Error(CircuitOpen(service_name, retry_after))
    }
  }
}

/// Record success for circuit breaker
pub fn record_success(service_name: String) -> Nil {
  let state = get_circuit_state(service_name)
  case state {
    HalfOpen -> close_circuit(service_name)
    _ -> Nil
  }
}

/// Record failure for circuit breaker
pub fn record_failure(service_name: String) -> Nil {
  let failures = increment_failure_count(service_name)
  let threshold = 5  // Open after 5 consecutive failures
  case failures >= threshold {
    True -> open_circuit(service_name)
    False -> Nil
  }
}

// ============================================================
// FFI for Rate Limiting (ETS-backed)
// ============================================================

@external(erlang, "vibee_rate_limit_ffi", "get_request_count")
fn get_request_count(tool_name: String) -> Int

@external(erlang, "vibee_rate_limit_ffi", "increment_request_count")
fn increment_request_count(tool_name: String) -> Int

@external(erlang, "vibee_rate_limit_ffi", "get_window_reset_seconds")
fn get_window_reset_seconds(tool_name: String) -> Int

@external(erlang, "vibee_rate_limit_ffi", "get_circuit_state")
fn get_circuit_state(service_name: String) -> CircuitState

@external(erlang, "vibee_rate_limit_ffi", "get_circuit_retry_seconds")
fn get_circuit_retry_seconds(service_name: String) -> Int

@external(erlang, "vibee_rate_limit_ffi", "increment_failure_count")
fn increment_failure_count(service_name: String) -> Int

@external(erlang, "vibee_rate_limit_ffi", "open_circuit")
fn open_circuit(service_name: String) -> Nil

@external(erlang, "vibee_rate_limit_ffi", "close_circuit")
fn close_circuit(service_name: String) -> Nil
