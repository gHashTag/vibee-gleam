// MCP Configuration - ENV-based settings

/// Configuration for MCP server
pub type Config {
  Config(
    bridge_url: String,
    bridge_port: Int,
    data_dir: String,
    log_level: LogLevel,
    rate_limit_per_minute: Int,
    api_key: String,
  )
}

pub type LogLevel {
  LogDebug
  LogInfo
  LogWarn
  LogErr
}

/// Get configuration from environment or defaults
pub fn get_config() -> Config {
  Config(
    bridge_url: get_env_with_default("VIBEE_BRIDGE_URL", "localhost"),
    bridge_port: get_env_int("VIBEE_BRIDGE_PORT", 8081),
    data_dir: get_env_with_default("VIBEE_DATA_DIR", "/tmp/vibee"),
    log_level: parse_log_level(get_env_with_default("VIBEE_LOG_LEVEL", "info")),
    rate_limit_per_minute: get_env_int("VIBEE_RATE_LIMIT", 100),
    api_key: get_env_with_default("VIBEE_API_KEY", ""),
  )
}

/// Get bridge URL with port
pub fn bridge_base_url(config: Config) -> String {
  // If bridge_url already has a scheme (https://), use it directly without port
  case has_scheme(config.bridge_url) {
    True -> config.bridge_url
    False -> "http://" <> config.bridge_url <> ":" <> int_to_string(config.bridge_port)
  }
}

/// Check if URL already has http:// or https:// scheme
fn has_scheme(url: String) -> Bool {
  case url {
    "http://" <> _ -> True
    "https://" <> _ -> True
    _ -> False
  }
}

/// Get bridge host
pub fn bridge_host(config: Config) -> String {
  config.bridge_url
}

/// Get bridge port
pub fn bridge_port(config: Config) -> Int {
  config.bridge_port
}

/// Get data directory
pub fn data_dir(config: Config) -> String {
  config.data_dir
}

/// Get API key
pub fn api_key(config: Config) -> String {
  config.api_key
}

/// Get environment variable with default
pub fn get_env(name: String) -> String {
  case get_env_internal(name) {
    Ok(value) -> value
    Error(Nil) -> ""
  }
}

/// Get environment variable with fallback default
pub fn get_env_or(name: String, default: String) -> String {
  case get_env_internal(name) {
    Ok(value) -> value
    Error(Nil) -> default
  }
}

// Internal helpers

fn parse_log_level(s: String) -> LogLevel {
  case s {
    "debug" -> LogDebug
    "warn" -> LogWarn
    "error" -> LogErr
    _ -> LogInfo
  }
}

// FFI for environment variables
@external(erlang, "vibee_config_ffi", "get_env")
fn get_env_internal(name: String) -> Result(String, Nil)

fn get_env_with_default(name: String, default: String) -> String {
  case get_env_internal(name) {
    Ok(value) -> value
    Error(Nil) -> default
  }
}

fn get_env_int(name: String, default: Int) -> Int {
  case get_env_internal(name) {
    Ok(value) -> {
      case parse_int(value) {
        Ok(n) -> n
        Error(Nil) -> default
      }
    }
    Error(Nil) -> default
  }
}

@external(erlang, "erlang", "integer_to_list")
fn int_to_charlist(n: Int) -> List(Int)

fn int_to_string(n: Int) -> String {
  charlist_to_string(int_to_charlist(n))
}

@external(erlang, "erlang", "list_to_binary")
fn charlist_to_string(chars: List(Int)) -> String

fn parse_int(s: String) -> Result(Int, Nil) {
  case s {
    "" -> Error(Nil)
    _ -> Ok(parse_int_unsafe(s))
  }
}

@external(erlang, "erlang", "binary_to_integer")
fn parse_int_unsafe(s: String) -> Int
