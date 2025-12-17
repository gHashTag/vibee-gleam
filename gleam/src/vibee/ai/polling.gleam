// Polling Utility for AI Services
// Provides types and configurations for polling async task status

import gleam/json
import gleam/option.{type Option, None, Some}

// ============================================================
// Types
// ============================================================

/// Generic poll status for async operations
pub type PollStatus {
  /// Task is still queued
  Pending
  /// Task is actively being processed
  Processing
  /// Task completed successfully with result
  Complete(result: json.Json)
  /// Task failed with error message
  Failed(error: String)
  /// Task was cancelled
  Cancelled
  /// Task timed out
  Timeout
}

/// Configuration for polling behavior
pub type PollConfig {
  PollConfig(
    /// Interval between polls in milliseconds
    interval_ms: Int,
    /// Maximum total wait time in milliseconds
    max_wait_ms: Int,
    /// Initial delay before first poll in milliseconds
    initial_delay_ms: Option(Int),
    /// Whether to use exponential backoff
    exponential_backoff: Bool,
    /// Maximum interval when using exponential backoff
    max_interval_ms: Option(Int),
    /// Backoff multiplier (default 2.0)
    backoff_multiplier: Option(Float),
  )
}

/// Result of a poll operation
pub type PollResult {
  PollResult(
    status: PollStatus,
    attempts: Int,
    elapsed_ms: Int,
    last_response: Option(json.Json),
  )
}

/// Service-specific polling parameters
pub type ServicePollParams {
  ServicePollParams(
    task_id: String,
    service_name: String,
    status_endpoint: String,
    success_status: List(String),
    failed_status: List(String),
    result_field: String,
    error_field: String,
  )
}

// ============================================================
// Default Configurations
// ============================================================

/// Default polling config - 2 second interval, 5 minute max wait
pub fn default_config() -> PollConfig {
  PollConfig(
    interval_ms: 2000,
    max_wait_ms: 300_000,
    initial_delay_ms: None,
    exponential_backoff: False,
    max_interval_ms: None,
    backoff_multiplier: None,
  )
}

/// Fast polling config - 1 second interval, 2 minute max wait
pub fn fast_config() -> PollConfig {
  PollConfig(
    interval_ms: 1000,
    max_wait_ms: 120_000,
    initial_delay_ms: None,
    exponential_backoff: False,
    max_interval_ms: None,
    backoff_multiplier: None,
  )
}

/// Slow polling config - 5 second interval, 10 minute max wait
pub fn slow_config() -> PollConfig {
  PollConfig(
    interval_ms: 5000,
    max_wait_ms: 600_000,
    initial_delay_ms: None,
    exponential_backoff: False,
    max_interval_ms: None,
    backoff_multiplier: None,
  )
}

/// Exponential backoff config - starts at 1 second, max 30 seconds
pub fn backoff_config() -> PollConfig {
  PollConfig(
    interval_ms: 1000,
    max_wait_ms: 300_000,
    initial_delay_ms: Some(500),
    exponential_backoff: True,
    max_interval_ms: Some(30_000),
    backoff_multiplier: Some(1.5),
  )
}

/// Video generation config - longer waits for video services
pub fn video_config() -> PollConfig {
  PollConfig(
    interval_ms: 5000,
    max_wait_ms: 1_800_000,
    // 30 minutes
    initial_delay_ms: Some(3000),
    exponential_backoff: True,
    max_interval_ms: Some(60_000),
    backoff_multiplier: Some(1.3),
  )
}

/// Image generation config - moderate waits
pub fn image_config() -> PollConfig {
  PollConfig(
    interval_ms: 2000,
    max_wait_ms: 300_000,
    // 5 minutes
    initial_delay_ms: Some(1000),
    exponential_backoff: True,
    max_interval_ms: Some(15_000),
    backoff_multiplier: Some(1.5),
  )
}

// ============================================================
// Service-Specific Poll Params
// ============================================================

/// BFL (FLUX) polling parameters
pub fn bfl_poll_params(task_id: String) -> ServicePollParams {
  ServicePollParams(
    task_id: task_id,
    service_name: "BFL",
    status_endpoint: "https://api.bfl.ml/v1/get_result?id=" <> task_id,
    success_status: ["Ready"],
    failed_status: ["Error", "Failed"],
    result_field: "result.sample",
    error_field: "error",
  )
}

/// Hedra polling parameters
pub fn hedra_poll_params(job_id: String) -> ServicePollParams {
  ServicePollParams(
    task_id: job_id,
    service_name: "Hedra",
    status_endpoint: "https://api.hedra.com/v1/characters/" <> job_id,
    success_status: ["completed", "done"],
    failed_status: ["failed", "error"],
    result_field: "videoUrl",
    error_field: "error",
  )
}

/// HeyGen polling parameters
pub fn heygen_poll_params(video_id: String) -> ServicePollParams {
  ServicePollParams(
    task_id: video_id,
    service_name: "HeyGen",
    status_endpoint: "https://api.heygen.com/v1/video_status.get?video_id="
      <> video_id,
    success_status: ["completed"],
    failed_status: ["failed", "error"],
    result_field: "data.video_url",
    error_field: "error.message",
  )
}

/// Kling polling parameters
pub fn kling_poll_params(task_id: String) -> ServicePollParams {
  ServicePollParams(
    task_id: task_id,
    service_name: "Kling",
    status_endpoint: "https://api.klingai.com/v1/videos/text2video/" <> task_id,
    success_status: ["succeed"],
    failed_status: ["failed"],
    result_field: "data.task_result.videos",
    error_field: "data.task_status_msg",
  )
}

/// KIE AI (Veo3) polling parameters
pub fn kieai_poll_params(task_id: String) -> ServicePollParams {
  ServicePollParams(
    task_id: task_id,
    service_name: "KIE AI",
    status_endpoint: "https://api.kie.ai/api/v1/veo/record-info?taskId="
      <> task_id,
    success_status: ["1"],
    // successFlag = 1
    failed_status: ["2", "3"],
    // 2 = failed, 3 = error
    result_field: "data.outputFiles",
    error_field: "message",
  )
}

/// Replicate polling parameters
pub fn replicate_poll_params(prediction_id: String) -> ServicePollParams {
  ServicePollParams(
    task_id: prediction_id,
    service_name: "Replicate",
    status_endpoint: "https://api.replicate.com/v1/predictions/" <> prediction_id,
    success_status: ["succeeded"],
    failed_status: ["failed", "canceled"],
    result_field: "output",
    error_field: "error",
  )
}

// ============================================================
// Helper Functions
// ============================================================

/// Calculate next poll interval with optional exponential backoff
pub fn next_interval(config: PollConfig, current_interval: Int, attempt: Int) -> Int {
  case config.exponential_backoff {
    True -> {
      let multiplier = case config.backoff_multiplier {
        Some(m) -> m
        None -> 2.0
      }
      let max_interval = case config.max_interval_ms {
        Some(max) -> max
        None -> config.max_wait_ms
      }
      // Calculate exponential backoff
      let base = config.interval_ms
      // Simple exponential: base * multiplier^attempt
      let exp_interval =
        base
        * float_to_int(pow(multiplier, int_to_float(attempt)))
      // Cap at max interval
      min(exp_interval, max_interval)
    }
    False -> config.interval_ms
  }
}

/// Check if polling should continue
pub fn should_continue(config: PollConfig, elapsed_ms: Int, status: PollStatus) -> Bool {
  case status {
    Pending | Processing -> elapsed_ms < config.max_wait_ms
    Complete(_) | Failed(_) | Cancelled | Timeout -> False
  }
}

/// Get estimated time remaining based on typical service times
pub fn estimated_time_remaining(
  service_name: String,
  elapsed_ms: Int,
) -> Option(Int) {
  let typical_time = case service_name {
    "BFL" -> 30_000
    // 30 seconds for images
    "Hedra" -> 120_000
    // 2 minutes for avatar video
    "HeyGen" -> 180_000
    // 3 minutes for avatar video
    "Kling" -> 300_000
    // 5 minutes for video
    "KIE AI" -> 120_000
    // 2 minutes for video
    "Replicate" -> 60_000
    // 1 minute average
    _ -> 60_000
  }
  case typical_time > elapsed_ms {
    True -> Some(typical_time - elapsed_ms)
    False -> None
  }
}

/// Create initial poll result
pub fn initial_result() -> PollResult {
  PollResult(status: Pending, attempts: 0, elapsed_ms: 0, last_response: None)
}

/// Update poll result with new status
pub fn update_result(
  result: PollResult,
  status: PollStatus,
  interval_ms: Int,
  response: Option(json.Json),
) -> PollResult {
  PollResult(
    status: status,
    attempts: result.attempts + 1,
    elapsed_ms: result.elapsed_ms + interval_ms,
    last_response: response,
  )
}

// ============================================================
// Internal Helper Functions
// ============================================================

fn min(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}

fn pow(base: Float, exp: Int) -> Float {
  case exp <= 0 {
    True -> 1.0
    False -> base *. pow(base, exp - 1)
  }
}

fn float_to_int(f: Float) -> Int {
  // Gleam's float truncation
  case f <. 0.0 {
    True -> 0 - truncate(0.0 -. f)
    False -> truncate(f)
  }
}

fn truncate(f: Float) -> Int {
  // External function would be needed for proper truncation
  // This is a placeholder - in real code use erlang:trunc/1
  0
}

fn int_to_float(i: Int) -> Int {
  i
}

// ============================================================
// Status Parsing Helpers
// ============================================================

/// Check if status string indicates completion
pub fn is_complete_status(status: String, success_statuses: List(String)) -> Bool {
  list_contains(success_statuses, status)
}

/// Check if status string indicates failure
pub fn is_failed_status(status: String, failed_statuses: List(String)) -> Bool {
  list_contains(failed_statuses, status)
}

fn list_contains(list: List(String), item: String) -> Bool {
  case list {
    [] -> False
    [head, ..tail] ->
      case head == item {
        True -> True
        False -> list_contains(tail, item)
      }
  }
}

/// Parse common status response patterns
pub fn parse_status_response(response: json.Json, params: ServicePollParams) -> PollStatus {
  // This would need actual JSON parsing - placeholder for now
  // In real implementation, use gleam/json decoders
  Pending
}
