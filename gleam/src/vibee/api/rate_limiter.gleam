// Rate Limiter - Token Bucket per IP
// Limits GraphQL requests to prevent abuse

import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result

/// Rate limit configuration
pub type RateLimitConfig {
  RateLimitConfig(
    requests_per_minute: Int,  // Max requests per minute (default: 60)
    burst_size: Int,           // Allow burst above limit (default: 10)
  )
}

/// Token bucket for rate limiting
pub type TokenBucket {
  TokenBucket(
    tokens: Int,           // Available tokens
    last_refill: Int,      // Last refill timestamp (ms)
  )
}

/// Rate limit result
pub type RateLimitResult {
  Allowed(remaining: Int)
  RateLimited(retry_after: Int)  // Seconds until next allowed request
}

/// Default config: 60 requests/min with burst of 10
pub fn default_config() -> RateLimitConfig {
  RateLimitConfig(
    requests_per_minute: 60,
    burst_size: 10,
  )
}

/// Check rate limit for an IP address
/// Uses ETS-backed storage via FFI
pub fn check(ip: String, config: RateLimitConfig) -> RateLimitResult {
  let now = get_current_time_ms()

  // Get or create bucket for IP
  let bucket = case ffi_get_bucket(ip) {
    Some(b) -> b
    None -> TokenBucket(tokens: config.requests_per_minute + config.burst_size, last_refill: now)
  }

  // Calculate token refill
  let elapsed_ms = now - bucket.last_refill
  let refill_rate = config.requests_per_minute * 1000 / 60  // tokens per second * 1000
  let new_tokens = case refill_rate > 0 {
    True -> elapsed_ms * config.requests_per_minute / 60000  // tokens to add
    False -> 0
  }

  let max_tokens = config.requests_per_minute + config.burst_size
  let refilled_tokens = int.min(bucket.tokens + new_tokens, max_tokens)

  case refilled_tokens >= 1 {
    True -> {
      // Consume one token
      let new_bucket = TokenBucket(tokens: refilled_tokens - 1, last_refill: now)
      ffi_set_bucket(ip, new_bucket)
      Allowed(remaining: refilled_tokens - 1)
    }
    False -> {
      // Calculate retry-after in seconds
      let tokens_needed = 1
      let ms_per_token = 60000 / config.requests_per_minute
      let retry_after_ms = tokens_needed * ms_per_token
      let retry_after_sec = { retry_after_ms + 999 } / 1000  // Round up
      RateLimited(retry_after: retry_after_sec)
    }
  }
}

/// Get current time in milliseconds
fn get_current_time_ms() -> Int {
  ffi_get_time_ms()
}

// FFI declarations for ETS-backed storage
@external(erlang, "vibee_rate_limiter_ffi", "get_bucket")
fn ffi_get_bucket(ip: String) -> Option(TokenBucket)

@external(erlang, "vibee_rate_limiter_ffi", "set_bucket")
fn ffi_set_bucket(ip: String, bucket: TokenBucket) -> Nil

@external(erlang, "vibee_rate_limiter_ffi", "get_time_ms")
fn ffi_get_time_ms() -> Int

@external(erlang, "vibee_rate_limiter_ffi", "init")
pub fn init() -> Nil
