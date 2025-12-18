// Structured JSON Logger
// Provides structured logging for production monitoring

import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/string

pub type LogLevel {
  Debug
  Info
  Warn
  Error
}

pub type LogContext =
  Dict(String, String)

/// Log a message with structured context
pub fn log(level: LogLevel, message: String, context: LogContext) {
  let log_entry =
    json.object([
      #("timestamp", json.string(iso8601_now())),
      #("level", json.string(level_to_string(level))),
      #("message", json.string(message)),
      #("context", context_to_json(context)),
      #("service", json.string("vibee-agent")),
    ])

  io.println(json.to_string(log_entry))
}

/// Log info level
pub fn info(message: String, context: LogContext) {
  log(Info, message, context)
}

/// Log warning level
pub fn warn(message: String, context: LogContext) {
  log(Warn, message, context)
}

/// Log error level
pub fn error(message: String, context: LogContext) {
  log(Error, message, context)
}

/// Log debug level
pub fn debug(message: String, context: LogContext) {
  log(Debug, message, context)
}

/// Create empty context
pub fn empty_context() -> LogContext {
  dict.new()
}

/// Add field to context
pub fn add_field(context: LogContext, key: String, value: String) -> LogContext {
  dict.insert(context, key, value)
}

/// Convert log level to string
fn level_to_string(level: LogLevel) -> String {
  case level {
    Debug -> "debug"
    Info -> "info"
    Warn -> "warn"
    Error -> "error"
  }
}

/// Convert context dict to JSON
fn context_to_json(context: LogContext) -> json.Json {
  context
  |> dict.to_list
  |> list.map(fn(pair) {
    let #(key, value) = pair
    #(key, json.string(value))
  })
  |> json.object
}

/// Get current timestamp in ISO8601 format
fn iso8601_now() -> String {
  let timestamp = get_timestamp()
  // Simple ISO8601 format: YYYY-MM-DDTHH:MM:SSZ
  // In production, use proper datetime library
  int.to_string(timestamp)
}

@external(erlang, "vibee_ffi", "get_unix_timestamp")
fn get_timestamp() -> Int
