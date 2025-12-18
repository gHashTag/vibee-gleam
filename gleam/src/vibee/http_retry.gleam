// HTTP Retry Logic with Exponential Backoff
// Provides resilient HTTP requests with automatic retries

import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/httpc
import gleam/int
import gleam/io

pub type RetryConfig {
  RetryConfig(
    max_attempts: Int,
    initial_delay_ms: Int,
    max_delay_ms: Int,
    backoff_multiplier: Float,
  )
}

/// Default retry configuration
pub fn default_config() -> RetryConfig {
  RetryConfig(
    max_attempts: 3,
    initial_delay_ms: 1000,
    max_delay_ms: 10_000,
    backoff_multiplier: 2.0,
  )
}

/// Send HTTP request with retry logic
pub fn send_with_retry(
  req: Request(String),
  config: RetryConfig,
) -> Result(Response(String), httpc.HttpError) {
  send_with_retry_internal(req, config, 1, config.initial_delay_ms)
}

/// Internal retry implementation
fn send_with_retry_internal(
  req: Request(String),
  config: RetryConfig,
  attempt: Int,
  delay_ms: Int,
) -> Result(Response(String), httpc.HttpError) {
  io.println(
    "[RETRY] Attempt "
    <> int.to_string(attempt)
    <> "/"
    <> int.to_string(config.max_attempts),
  )

  case httpc.send(req) {
    Ok(response) -> {
      io.println("[RETRY] Success on attempt " <> int.to_string(attempt))
      Ok(response)
    }
    Error(err) -> {
      case attempt < config.max_attempts {
        True -> {
          io.println(
            "[RETRY] Failed, retrying in " <> int.to_string(delay_ms) <> "ms",
          )
          process.sleep(delay_ms)

          // Calculate next delay with exponential backoff
          let next_delay =
            float_to_int(int.to_float(delay_ms) *. config.backoff_multiplier)
          let capped_delay = int.min(next_delay, config.max_delay_ms)

          send_with_retry_internal(req, config, attempt + 1, capped_delay)
        }
        False -> {
          io.println(
            "[RETRY] All attempts failed after "
            <> int.to_string(config.max_attempts)
            <> " tries",
          )
          Error(err)
        }
      }
    }
  }
}

/// Convert float to int (truncate)
fn float_to_int(f: Float) -> Int {
  case f >=. 0.0 {
    True -> truncate_positive(f)
    False -> 0 - truncate_positive(0.0 -. f)
  }
}

@external(erlang, "erlang", "trunc")
fn truncate_positive(f: Float) -> Int
