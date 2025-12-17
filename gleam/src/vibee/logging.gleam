// VIBEE Logging Module
// Logs to stdout for Fly.io compatibility

import gleam/io
import gleam/string

/// Log levels
pub type LogLevel {
  Info
  Warn
  Error
  Debug
}

fn level_to_string(level: LogLevel) -> String {
  case level {
    Info -> "[info]"
    Warn -> "[warn]"
    Error -> "[error]"
    Debug -> "[debug]"
  }
}

/// Write a log message to stdout for Fly.io
pub fn log(level: LogLevel, message: String) -> Nil {
  let level_str = level_to_string(level)
  let line = level_str <> message
  io.println(line)
}

/// Convenience functions
pub fn info(message: String) -> Nil {
  log(Info, message)
}

pub fn warn(message: String) -> Nil {
  log(Warn, message)
}

pub fn error(message: String) -> Nil {
  log(Error, message)
}

pub fn debug(message: String) -> Nil {
  log(Debug, message)
}

/// Log Telegram message
pub fn telegram_message(group: String, sender: String, text: String) -> Nil {
  let msg = "[TG:" <> group <> "] " <> sender <> ": " <> string.slice(text, 0, 100)
  log(Info, msg)
}

/// Log API request
pub fn api_request(method: String, path: String) -> Nil {
  let msg = "[API] " <> method <> " " <> path
  log(Debug, msg)
}

/// Log WebSocket event
pub fn ws_event(event: String, details: String) -> Nil {
  let msg = "[WS] " <> event <> " - " <> details
  log(Debug, msg)
}
