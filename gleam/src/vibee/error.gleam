// Error types for VIBEE Agent Framework

/// All errors that can occur in VIBEE
pub type VibeeError {
  /// Agent with given ID was not found
  AgentNotFound(agent_id: String)

  /// Failed to start an agent
  AgentStartFailed(reason: String)

  /// Request timed out
  MessageTimeout(request_id: String, timeout_ms: Int)

  /// Database/persistence error
  PersistenceError(operation: String, details: String)

  /// Telegram API error
  TelegramError(code: Int, message: String)

  /// Telegram bridge connection error
  TelegramBridgeError(message: String)

  /// Input validation error
  ValidationError(field: String, message: String)

  /// JSON parsing error
  JsonError(message: String)

  /// HTTP request error
  HttpError(status: Int, message: String)

  /// Configuration error
  ConfigError(message: String)

  /// Internal error
  InternalError(message: String)
}

/// Convert error to human-readable string
pub fn to_string(error: VibeeError) -> String {
  case error {
    AgentNotFound(id) -> "Agent not found: " <> id
    AgentStartFailed(reason) -> "Failed to start agent: " <> reason
    MessageTimeout(req_id, ms) ->
      "Request " <> req_id <> " timed out after " <> int_to_string(ms) <> "ms"
    PersistenceError(op, details) ->
      "Persistence error in " <> op <> ": " <> details
    TelegramError(code, msg) ->
      "Telegram error " <> int_to_string(code) <> ": " <> msg
    TelegramBridgeError(msg) -> "Telegram bridge error: " <> msg
    ValidationError(field, msg) -> "Validation error on " <> field <> ": " <> msg
    JsonError(msg) -> "JSON error: " <> msg
    HttpError(status, msg) ->
      "HTTP error " <> int_to_string(status) <> ": " <> msg
    ConfigError(msg) -> "Configuration error: " <> msg
    InternalError(msg) -> "Internal error: " <> msg
  }
}

/// Helper to convert int to string
fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ -> do_int_to_string(n, "")
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        9 -> "9"
        _ -> "?"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}
