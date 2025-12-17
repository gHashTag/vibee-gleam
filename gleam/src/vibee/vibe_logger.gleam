// Vibe Logger - Structured logging with context and tracing
// Uses stderr to avoid mixing with MCP JSON-RPC responses
// Also sends to log_aggregator for WebSocket streaming

import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import vibee/log_aggregator

// =============================================================================
// Types
// =============================================================================

/// Log level
pub type LogLevel {
  Trace
  Debug
  Info
  Warn
  ErrorLevel
  Fatal
}

/// Context for request tracing
pub type LogContext {
  LogContext(
    trace_id: Option(String),
    request_id: Option(String),
    session_id: Option(String),
    user_id: Option(Int),
    tool_name: Option(String),
    span_id: Option(String),
    extra: List(#(String, json.Json)),
  )
}

/// Logger with context
pub type VibeLogger {
  VibeLogger(name: String, level: LogLevel, context: LogContext)
}

// =============================================================================
// Constructors
// =============================================================================

/// Create empty context
pub fn empty_context() -> LogContext {
  LogContext(
    trace_id: None,
    request_id: None,
    session_id: None,
    user_id: None,
    tool_name: None,
    span_id: None,
    extra: [],
  )
}

/// Create new logger
pub fn new(name: String) -> VibeLogger {
  VibeLogger(name: name, level: Debug, context: empty_context())
}

/// Create logger with minimum level
pub fn with_level(logger: VibeLogger, level: LogLevel) -> VibeLogger {
  VibeLogger(..logger, level: level)
}

/// Add context to logger
pub fn with_context(logger: VibeLogger, context: LogContext) -> VibeLogger {
  VibeLogger(..logger, context: context)
}

/// Add trace ID to context
pub fn with_trace_id(logger: VibeLogger, trace_id: String) -> VibeLogger {
  VibeLogger(
    ..logger,
    context: LogContext(..logger.context, trace_id: Some(trace_id)),
  )
}

/// Add request ID to context
pub fn with_request_id(logger: VibeLogger, request_id: String) -> VibeLogger {
  VibeLogger(
    ..logger,
    context: LogContext(..logger.context, request_id: Some(request_id)),
  )
}

/// Add session ID to context
pub fn with_session_id(logger: VibeLogger, session_id: String) -> VibeLogger {
  VibeLogger(
    ..logger,
    context: LogContext(..logger.context, session_id: Some(session_id)),
  )
}

/// Add tool name to context
pub fn with_tool(logger: VibeLogger, tool_name: String) -> VibeLogger {
  VibeLogger(
    ..logger,
    context: LogContext(..logger.context, tool_name: Some(tool_name)),
  )
}

/// Add span ID to context
pub fn with_span_id(logger: VibeLogger, span_id: String) -> VibeLogger {
  VibeLogger(
    ..logger,
    context: LogContext(..logger.context, span_id: Some(span_id)),
  )
}

/// Add extra data to context
pub fn with_data(
  logger: VibeLogger,
  key: String,
  value: json.Json,
) -> VibeLogger {
  VibeLogger(
    ..logger,
    context: LogContext(
      ..logger.context,
      extra: [#(key, value), ..logger.context.extra],
    ),
  )
}

// =============================================================================
// Logging Functions
// =============================================================================

/// Log at TRACE level
pub fn trace(logger: VibeLogger, message: String) -> Nil {
  log(logger, Trace, message)
}

/// Log at DEBUG level
pub fn debug(logger: VibeLogger, message: String) -> Nil {
  log(logger, Debug, message)
}

/// Log at INFO level
pub fn info(logger: VibeLogger, message: String) -> Nil {
  log(logger, Info, message)
}

/// Log at WARN level
pub fn warn(logger: VibeLogger, message: String) -> Nil {
  log(logger, Warn, message)
}

/// Log at ERROR level
pub fn error(logger: VibeLogger, message: String) -> Nil {
  log(logger, ErrorLevel, message)
}

/// Log at FATAL level
pub fn fatal(logger: VibeLogger, message: String) -> Nil {
  log(logger, Fatal, message)
}

/// Main log function
pub fn log(logger: VibeLogger, level: LogLevel, message: String) -> Nil {
  // Check if level is enabled
  case should_log(logger.level, level) {
    False -> Nil
    True -> {
      let timestamp = get_iso_timestamp()
      let log_json = format_json_with_timestamp(logger, level, message, timestamp)

      // Output to stderr (MCP-safe!)
      print_stderr(log_json)

      // Also send to aggregator for WebSocket streaming
      case log_aggregator.get_global() {
        Some(aggregator) -> {
          let entry = log_aggregator.LogEntry(
            timestamp: timestamp,
            level: level_to_string(level),
            logger: logger.name,
            message: message,
            trace_id: logger.context.trace_id,
            request_id: logger.context.request_id,
            session_id: logger.context.session_id,
            span_id: logger.context.span_id,
            tool: logger.context.tool_name,
            extra: logger.context.extra,
          )
          log_aggregator.log(aggregator, entry)
        }
        None -> Nil
      }
    }
  }
}

// =============================================================================
// Formatting
// =============================================================================

fn format_json_with_timestamp(
  logger: VibeLogger,
  level: LogLevel,
  message: String,
  timestamp: String,
) -> String {
  let ctx = logger.context

  let base_fields = [
    #("timestamp", json.string(timestamp)),
    #("level", json.string(level_to_string(level))),
    #("logger", json.string(logger.name)),
    #("message", json.string(message)),
  ]

  let context_fields =
    [
      option_field("trace_id", ctx.trace_id),
      option_field("request_id", ctx.request_id),
      option_field("session_id", ctx.session_id),
      option_int_field("user_id", ctx.user_id),
      option_field("tool", ctx.tool_name),
      option_field("span_id", ctx.span_id),
    ]
    |> list.filter_map(fn(x) { x })

  let all_fields =
    list.flatten([base_fields, context_fields, ctx.extra])

  json.object(all_fields)
  |> json.to_string
}

fn option_field(
  key: String,
  value: Option(String),
) -> Result(#(String, json.Json), Nil) {
  case value {
    Some(v) -> Ok(#(key, json.string(v)))
    None -> Error(Nil)
  }
}

fn option_int_field(
  key: String,
  value: Option(Int),
) -> Result(#(String, json.Json), Nil) {
  case value {
    Some(v) -> Ok(#(key, json.int(v)))
    None -> Error(Nil)
  }
}

fn level_to_string(level: LogLevel) -> String {
  case level {
    Trace -> "TRACE"
    Debug -> "DEBUG"
    Info -> "INFO"
    Warn -> "WARN"
    ErrorLevel -> "ERROR"
    Fatal -> "FATAL"
  }
}

fn level_to_int(level: LogLevel) -> Int {
  case level {
    Trace -> 0
    Debug -> 1
    Info -> 2
    Warn -> 3
    ErrorLevel -> 4
    Fatal -> 5
  }
}

fn should_log(min_level: LogLevel, log_level: LogLevel) -> Bool {
  level_to_int(log_level) >= level_to_int(min_level)
}

// =============================================================================
// FFI - Erlang functions
// =============================================================================

@external(erlang, "vibee_vibe_logger_ffi", "print_stderr")
fn print_stderr(message: String) -> Nil

@external(erlang, "vibee_vibe_logger_ffi", "get_iso_timestamp")
fn get_iso_timestamp() -> String

@external(erlang, "vibee_vibe_logger_ffi", "generate_trace_id")
pub fn generate_trace_id() -> String

@external(erlang, "vibee_vibe_logger_ffi", "generate_span_id")
pub fn generate_span_id() -> String

// =============================================================================
// Convenience Functions
// =============================================================================

/// Quick log to info level (for migration from old logging module)
pub fn log_info(message: String) -> Nil {
  new("vibee")
  |> info(message)
}

/// Quick log to error level
pub fn log_error(message: String) -> Nil {
  new("vibee")
  |> error(message)
}

/// Quick log to debug level
pub fn log_debug(message: String) -> Nil {
  new("vibee")
  |> debug(message)
}

/// Quick log to warn level
pub fn log_warn(message: String) -> Nil {
  new("vibee")
  |> warn(message)
}

/// Create logger for MCP operations
pub fn mcp_logger(request_id: String) -> VibeLogger {
  new("mcp")
  |> with_request_id(request_id)
  |> with_trace_id(generate_trace_id())
}

/// Create logger for tool execution
pub fn tool_logger(tool_name: String, parent: VibeLogger) -> VibeLogger {
  VibeLogger(
    ..parent,
    name: "tool",
    context: LogContext(..parent.context, tool_name: Some(tool_name)),
  )
}
