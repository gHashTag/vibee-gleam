// VIBEE Advanced Logging System
// Structured logging with context, metrics, and anomaly detection

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// Log levels with priority
pub type LogLevel {
  Trace    // 0 - Very detailed, performance impact
  Debug    // 1 - Development debugging
  Info     // 2 - General information
  Warn     // 3 - Warning, potential issues
  Error    // 4 - Error, needs attention
  Critical // 5 - Critical, immediate action required
}

/// Log categories for filtering
pub type LogCategory {
  System
  Telegram
  Database
  API
  WebSocket
  Agent
  Trigger
  Filter
  Performance
  Security
  Anomaly
}

/// Structured log context
pub type LogContext {
  LogContext(
    category: LogCategory,
    component: String,
    operation: Option(String),
    chat_id: Option(String),
    user_id: Option(String),
    duration_ms: Option(Int),
    error_code: Option(String),
  )
}

/// Create empty context
pub fn context() -> LogContext {
  LogContext(
    category: System,
    component: "unknown",
    operation: None,
    chat_id: None,
    user_id: None,
    duration_ms: None,
    error_code: None,
  )
}

/// Builder pattern for context
pub fn with_category(ctx: LogContext, category: LogCategory) -> LogContext {
  LogContext(..ctx, category: category)
}

pub fn with_component(ctx: LogContext, component: String) -> LogContext {
  LogContext(..ctx, component: component)
}

pub fn with_operation(ctx: LogContext, operation: String) -> LogContext {
  LogContext(..ctx, operation: Some(operation))
}

pub fn with_chat(ctx: LogContext, chat_id: String) -> LogContext {
  LogContext(..ctx, chat_id: Some(chat_id))
}

pub fn with_user(ctx: LogContext, user_id: String) -> LogContext {
  LogContext(..ctx, user_id: Some(user_id))
}

pub fn with_duration(ctx: LogContext, duration_ms: Int) -> LogContext {
  LogContext(..ctx, duration_ms: Some(duration_ms))
}

pub fn with_error(ctx: LogContext, error_code: String) -> LogContext {
  LogContext(..ctx, error_code: Some(error_code))
}

/// Convert level to string with emoji
fn level_to_string(level: LogLevel) -> String {
  case level {
    Trace -> "🔍 [TRACE]"
    Debug -> "🐛 [DEBUG]"
    Info -> "ℹ️  [INFO]"
    Warn -> "⚠️  [WARN]"
    Error -> "❌ [ERROR]"
    Critical -> "🚨 [CRITICAL]"
  }
}

/// Convert category to string
fn category_to_string(category: LogCategory) -> String {
  case category {
    System -> "SYS"
    Telegram -> "TG"
    Database -> "DB"
    API -> "API"
    WebSocket -> "WS"
    Agent -> "AGENT"
    Trigger -> "TRIGGER"
    Filter -> "FILTER"
    Performance -> "PERF"
    Security -> "SEC"
    Anomaly -> "ANOMALY"
  }
}

/// Format context as string
fn format_context(ctx: LogContext) -> String {
  let parts = [
    "[" <> category_to_string(ctx.category) <> ":" <> ctx.component <> "]",
  ]
  
  let parts = case ctx.operation {
    Some(op) -> list.append(parts, ["op=" <> op])
    None -> parts
  }
  
  let parts = case ctx.chat_id {
    Some(id) -> list.append(parts, ["chat=" <> id])
    None -> parts
  }
  
  let parts = case ctx.user_id {
    Some(id) -> list.append(parts, ["user=" <> id])
    None -> parts
  }
  
  let parts = case ctx.duration_ms {
    Some(ms) -> list.append(parts, ["duration=" <> int.to_string(ms) <> "ms"])
    None -> parts
  }
  
  let parts = case ctx.error_code {
    Some(code) -> list.append(parts, ["error=" <> code])
    None -> parts
  }
  
  string.join(parts, " ")
}

/// Main logging function with context
pub fn log(level: LogLevel, ctx: LogContext, message: String) -> Nil {
  let level_str = level_to_string(level)
  let ctx_str = format_context(ctx)
  let line = level_str <> " " <> ctx_str <> " " <> message
  io.println(line)
}

/// Convenience functions with context
pub fn trace(ctx: LogContext, message: String) -> Nil {
  log(Trace, ctx, message)
}

pub fn debug(ctx: LogContext, message: String) -> Nil {
  log(Debug, ctx, message)
}

pub fn info(ctx: LogContext, message: String) -> Nil {
  log(Info, ctx, message)
}

pub fn warn(ctx: LogContext, message: String) -> Nil {
  log(Warn, ctx, message)
}

pub fn error(ctx: LogContext, message: String) -> Nil {
  log(Error, ctx, message)
}

pub fn critical(ctx: LogContext, message: String) -> Nil {
  log(Critical, ctx, message)
}

/// Quick logging without context (legacy compatibility)
pub fn quick_info(message: String) -> Nil {
  info(context(), message)
}

pub fn quick_warn(message: String) -> Nil {
  warn(context(), message)
}

pub fn quick_error(message: String) -> Nil {
  error(context(), message)
}

/// Domain-specific logging functions

/// Log Telegram message
pub fn telegram_message(chat_id: String, sender: String, text: String) -> Nil {
  let ctx = context()
    |> with_category(Telegram)
    |> with_component("message")
    |> with_chat(chat_id)
  
  let msg = sender <> ": " <> string.slice(text, 0, 100)
  info(ctx, msg)
}

/// Log Telegram message processing
pub fn telegram_processing(chat_id: String, msg_id: Int, from_id: Int, text: String) -> Nil {
  let ctx = context()
    |> with_category(Telegram)
    |> with_component("processing")
    |> with_chat(chat_id)
    |> with_user(int.to_string(from_id))
  
  let msg = "msg_id=" <> int.to_string(msg_id) <> " text=" <> string.slice(text, 0, 50)
  debug(ctx, msg)
}

/// Log filter decision
pub fn filter_decision(chat_id: String, decision: String, reason: String) -> Nil {
  let ctx = context()
    |> with_category(Filter)
    |> with_component("chat_filter")
    |> with_chat(chat_id)
  
  let msg = decision <> " - " <> reason
  debug(ctx, msg)
}

/// Log trigger detection
pub fn trigger_detected(chat_id: String, trigger: String, matched: Bool) -> Nil {
  let ctx = context()
    |> with_category(Trigger)
    |> with_component("detection")
    |> with_chat(chat_id)
  
  let status = case matched {
    True -> "✅ MATCHED"
    False -> "❌ NO MATCH"
  }
  let msg = status <> " trigger='" <> trigger <> "'"
  
  case matched {
    True -> info(ctx, msg)
    False -> debug(ctx, msg)
  }
}

/// Log API request
pub fn api_request(method: String, path: String, status: Int) -> Nil {
  let ctx = context()
    |> with_category(API)
    |> with_component("request")
    |> with_operation(method <> " " <> path)
  
  let msg = "status=" <> int.to_string(status)
  
  case status >= 400 {
    True -> warn(ctx, msg)
    False -> debug(ctx, msg)
  }
}

/// Log database operation
pub fn db_operation(operation: String, table: String, duration_ms: Int, success: Bool) -> Nil {
  let ctx = context()
    |> with_category(Database)
    |> with_component(table)
    |> with_operation(operation)
    |> with_duration(duration_ms)
  
  let status = case success {
    True -> "✅ OK"
    False -> "❌ FAILED"
  }
  let msg = status
  
  case success {
    True -> {
      case duration_ms > 1000 {
        True -> warn(ctx, msg <> " - SLOW QUERY")
        False -> debug(ctx, msg)
      }
    }
    False -> error(ctx, msg)
  }
}

/// Log performance metric
pub fn performance(component: String, operation: String, duration_ms: Int) -> Nil {
  let ctx = context()
    |> with_category(Performance)
    |> with_component(component)
    |> with_operation(operation)
    |> with_duration(duration_ms)
  
  let msg = ""
  
  case duration_ms {
    ms if ms > 5000 -> critical(ctx, msg <> " - CRITICAL SLOWNESS")
    ms if ms > 2000 -> warn(ctx, msg <> " - SLOW")
    ms if ms > 1000 -> info(ctx, msg <> " - ACCEPTABLE")
    _ -> debug(ctx, msg)
  }
}

/// Log security event
pub fn security_event(event_type: String, details: String, severity: LogLevel) -> Nil {
  let ctx = context()
    |> with_category(Security)
    |> with_component("security")
    |> with_operation(event_type)
  
  log(severity, ctx, details)
}

/// Log anomaly detection
pub fn anomaly(component: String, anomaly_type: String, details: String) -> Nil {
  let ctx = context()
    |> with_category(Anomaly)
    |> with_component(component)
    |> with_operation(anomaly_type)
  
  warn(ctx, details)
}

/// Log WebSocket event
pub fn ws_event(event: String, details: String) -> Nil {
  let ctx = context()
    |> with_category(WebSocket)
    |> with_component("event")
    |> with_operation(event)
  
  debug(ctx, details)
}

/// Log agent state change
pub fn agent_state(state: String, details: String) -> Nil {
  let ctx = context()
    |> with_category(Agent)
    |> with_component("state")
    |> with_operation(state)
  
  info(ctx, details)
}
