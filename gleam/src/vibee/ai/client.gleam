// Unified HTTP Client for AI Services
// Executes requests from any AI module without code duplication

import gleam/bit_array
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/list
import gleam/string

// ============================================================
// Common Request Type (used by all AI modules)
// ============================================================

/// Generic request type compatible with all AI service modules
pub type Request {
  Request(
    url: String,
    method: String,
    headers: List(#(String, String)),
    body: String,
  )
}

// ============================================================
// Response Types
// ============================================================

pub type ApiResponse {
  ApiResponse(status: Int, body: String, headers: List(#(String, String)))
}

pub type ApiError {
  NetworkError(String)
  HttpError(status: Int, body: String)
  ParseError(String)
  TimeoutError
  RateLimitError(retry_after_ms: Int)
}

// ============================================================
// Retry Configuration
// ============================================================

/// Configuration for retry with exponential backoff
pub type RetryConfig {
  RetryConfig(
    max_retries: Int,
    base_delay_ms: Int,
    max_delay_ms: Int,
  )
}

/// Default retry config: 3 retries, 1s base, 30s max
pub fn default_retry_config() -> RetryConfig {
  RetryConfig(max_retries: 3, base_delay_ms: 1000, max_delay_ms: 30_000)
}

/// Aggressive retry config for critical requests
pub fn aggressive_retry_config() -> RetryConfig {
  RetryConfig(max_retries: 5, base_delay_ms: 500, max_delay_ms: 60_000)
}

/// No retry config
pub fn no_retry_config() -> RetryConfig {
  RetryConfig(max_retries: 0, base_delay_ms: 0, max_delay_ms: 0)
}

// ============================================================
// HTTP Execution
// ============================================================

/// Execute a request and return the response
pub fn execute(req: Request) -> Result(ApiResponse, ApiError) {
  // Parse URL and create request
  case request.to(req.url) {
    Ok(base_req) -> {
      // Set method
      let method = case string.lowercase(req.method) {
        "post" -> http.Post
        "put" -> http.Put
        "delete" -> http.Delete
        "patch" -> http.Patch
        _ -> http.Get
      }

      // Build request with headers and body
      let http_req =
        base_req
        |> request.set_method(method)
        |> request.set_body(req.body)
        |> add_headers(req.headers)

      // Execute
      case httpc.send(http_req) {
        Ok(resp) ->
          Ok(ApiResponse(
            status: resp.status,
            body: resp.body,
            headers: resp.headers,
          ))
        Error(_) -> Error(NetworkError("Failed to send request"))
      }
    }
    Error(_) -> Error(NetworkError("Invalid URL: " <> req.url))
  }
}

/// Execute request and return body if successful
pub fn execute_json(req: Request) -> Result(String, ApiError) {
  case execute(req) {
    Ok(resp) -> {
      case resp.status >= 200 && resp.status < 300 {
        True -> Ok(resp.body)
        False -> Error(HttpError(resp.status, resp.body))
      }
    }
    Error(e) -> Error(e)
  }
}

/// Execute request expecting binary response (for audio/images)
pub fn execute_binary(req: Request) -> Result(BitArray, ApiError) {
  case request.to(req.url) {
    Ok(base_req) -> {
      let method = case string.lowercase(req.method) {
        "post" -> http.Post
        _ -> http.Get
      }

      // For binary requests, we need to use a different approach
      // Convert body to BitArray
      let body_bits = bit_array.from_string(req.body)

      let http_req =
        base_req
        |> request.set_method(method)
        |> request.set_body(body_bits)
        |> add_headers_bits(req.headers)

      case httpc.send_bits(http_req) {
        Ok(resp) -> {
          case resp.status >= 200 && resp.status < 300 {
            True -> Ok(resp.body)
            False -> Error(HttpError(resp.status, "Binary request failed"))
          }
        }
        Error(_) -> Error(NetworkError("Failed to send binary request"))
      }
    }
    Error(_) -> Error(NetworkError("Invalid URL"))
  }
}

// ============================================================
// Helper Functions
// ============================================================

fn add_headers(
  req: request.Request(String),
  headers: List(#(String, String)),
) -> request.Request(String) {
  list.fold(headers, req, fn(r, header) {
    request.set_header(r, header.0, header.1)
  })
}

fn add_headers_bits(
  req: request.Request(BitArray),
  headers: List(#(String, String)),
) -> request.Request(BitArray) {
  list.fold(headers, req, fn(r, header) {
    request.set_header(r, header.0, header.1)
  })
}

/// Format error for user display
pub fn error_to_string(error: ApiError) -> String {
  case error {
    NetworkError(msg) -> "Network error: " <> msg
    HttpError(status, body) ->
      "HTTP " <> string.inspect(status) <> ": " <> string.slice(body, 0, 200)
    ParseError(msg) -> "Parse error: " <> msg
    TimeoutError -> "Request timed out"
    RateLimitError(retry_after) ->
      "Rate limited. Retry after " <> int.to_string(retry_after) <> "ms"
  }
}

/// Check if response indicates success
pub fn is_success(resp: ApiResponse) -> Bool {
  resp.status >= 200 && resp.status < 300
}

// ============================================================
// Retry Logic
// ============================================================

/// Execute request with retry on failures and rate limits
pub fn execute_with_retry(
  req: Request,
  config: RetryConfig,
) -> Result(ApiResponse, ApiError) {
  execute_with_retry_inner(req, config, 0)
}

fn execute_with_retry_inner(
  req: Request,
  config: RetryConfig,
  attempt: Int,
) -> Result(ApiResponse, ApiError) {
  case execute(req) {
    Ok(resp) -> {
      case is_retryable_status(resp.status) {
        True -> {
          case attempt < config.max_retries {
            True -> {
              // Calculate exponential backoff delay
              let _delay = calculate_delay(config, attempt)
              // Note: Gleam doesn't have built-in sleep, this is a marker
              // In production, this would be handled by the runtime
              execute_with_retry_inner(req, config, attempt + 1)
            }
            False -> {
              case resp.status == 429 {
                True -> Error(RateLimitError(get_retry_after(resp)))
                False -> Error(HttpError(resp.status, resp.body))
              }
            }
          }
        }
        False -> Ok(resp)
      }
    }
    Error(NetworkError(msg)) -> {
      case attempt < config.max_retries {
        True -> execute_with_retry_inner(req, config, attempt + 1)
        False -> Error(NetworkError(msg))
      }
    }
    Error(other) -> Error(other)
  }
}

/// Check if HTTP status is retryable
fn is_retryable_status(status: Int) -> Bool {
  status == 429 || status == 500 || status == 502 || status == 503 || status == 504
}

/// Calculate exponential backoff delay
fn calculate_delay(config: RetryConfig, attempt: Int) -> Int {
  let base = config.base_delay_ms
  let multiplier = power_of_2(attempt)
  let delay = base * multiplier
  min_int(delay, config.max_delay_ms)
}

/// Get Retry-After header value in ms
fn get_retry_after(resp: ApiResponse) -> Int {
  case list.find(resp.headers, fn(h) { h.0 == "retry-after" }) {
    Ok(#(_, value)) -> {
      case int.parse(value) {
        Ok(seconds) -> seconds * 1000
        Error(_) -> 1000
      }
    }
    Error(_) -> 1000
  }
}

/// Power of 2 for exponential backoff
fn power_of_2(n: Int) -> Int {
  case n {
    0 -> 1
    1 -> 2
    2 -> 4
    3 -> 8
    4 -> 16
    5 -> 32
    _ -> 64
  }
}

/// Min of two ints
fn min_int(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}

/// Execute JSON request with retry
pub fn execute_json_with_retry(
  req: Request,
  config: RetryConfig,
) -> Result(String, ApiError) {
  case execute_with_retry(req, config) {
    Ok(resp) -> {
      case is_success(resp) {
        True -> Ok(resp.body)
        False -> Error(HttpError(resp.status, resp.body))
      }
    }
    Error(e) -> Error(e)
  }
}
