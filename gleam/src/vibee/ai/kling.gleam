// Kling AI Video Generation Integration
// API Documentation: https://docs.klingai.com

import gleam/bit_array
import gleam/crypto
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/string

// ============================================================
// Types
// ============================================================

pub type Config {
  Config(access_key: String, secret_key: String)
}

pub type VideoRequest {
  VideoRequest(
    prompt: String,
    mode: String,
    duration: String,
    aspect_ratio: Option(String),
  )
}

pub type ImageToVideoRequest {
  ImageToVideoRequest(
    image_url: String,
    prompt: Option(String),
    duration: String,
  )
}

pub type Request {
  Request(
    url: String,
    method: String,
    headers: List(#(String, String)),
    body: String,
  )
}

pub type TaskStatus {
  Pending
  Processing
  Completed(video_url: String)
  Failed(error: String)
}

pub type Task {
  Task(task_id: String, status: TaskStatus)
}

// ============================================================
// JWT Token Generation
// ============================================================

/// Generate a JWT token for Kling API authentication
/// Uses HS256 algorithm per Kling API documentation
pub fn generate_jwt_token(config: Config, timestamp: Int) -> String {
  // JWT Header: {"alg": "HS256", "typ": "JWT"}
  let header = "{\"alg\":\"HS256\",\"typ\":\"JWT\"}"
  let header_b64 = base64url_encode(header)

  // JWT Payload with claims
  let exp = timestamp + 1800  // 30 minutes
  let nbf = timestamp - 5  // 5 seconds before
  let payload =
    "{\"iss\":\""
    <> config.access_key
    <> "\",\"exp\":"
    <> int.to_string(exp)
    <> ",\"nbf\":"
    <> int.to_string(nbf)
    <> "}"
  let payload_b64 = base64url_encode(payload)

  // Create signature input
  let signing_input = header_b64 <> "." <> payload_b64

  // Sign with HMAC-SHA256
  let signature =
    crypto.hmac(
      bit_array.from_string(signing_input),
      crypto.Sha256,
      bit_array.from_string(config.secret_key),
    )
  let signature_b64 = base64url_encode_bits(signature)

  // Return complete JWT
  signing_input <> "." <> signature_b64
}

/// Generate JWT token with current timestamp
/// Note: Caller should provide actual timestamp from runtime
pub fn generate_jwt_token_now(config: Config) -> String {
  // Placeholder timestamp - in real usage, pass actual Unix timestamp
  // from Erlang runtime: erlang.system_time(Second)
  generate_jwt_token(config, 0)
}

/// Get Authorization header value
fn get_auth_header(config: Config, timestamp: Int) -> String {
  "Bearer " <> generate_jwt_token(config, timestamp)
}

// ============================================================
// Base64URL Encoding
// ============================================================

/// Base64URL encode a string
fn base64url_encode(input: String) -> String {
  input
  |> bit_array.from_string
  |> base64url_encode_bits
}

/// Base64URL encode raw bits
fn base64url_encode_bits(input: BitArray) -> String {
  input
  |> bit_array.base64_encode(True)
  |> string.replace("+", "-")
  |> string.replace("/", "_")
  |> string.replace("=", "")
}

// ============================================================
// Request Builders
// ============================================================

/// Create a request to generate a video from text
/// timestamp: Unix timestamp in seconds (use erlang.system_time(Second))
pub fn create_video_request(
  config: Config,
  req: VideoRequest,
  timestamp: Int,
) -> Request {
  let body_parts = [
    #("prompt", json.string(req.prompt)),
    #("mode", json.string(req.mode)),
    #("duration", json.string(req.duration)),
  ]

  let body_with_ratio = case req.aspect_ratio {
    Some(ratio) -> [#("aspect_ratio", json.string(ratio)), ..body_parts]
    None -> body_parts
  }

  let body = json.to_string(json.object(body_with_ratio))

  Request(
    url: "https://api.klingai.com/v1/videos/text2video",
    method: "POST",
    headers: [
      #("Authorization", get_auth_header(config, timestamp)),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create a request to generate a video from an image
pub fn image_to_video_request(
  config: Config,
  req: ImageToVideoRequest,
  timestamp: Int,
) -> Request {
  let body_parts = [
    #("image_url", json.string(req.image_url)),
    #("duration", json.string(req.duration)),
  ]

  let body_with_prompt = case req.prompt {
    Some(p) -> [#("prompt", json.string(p)), ..body_parts]
    None -> body_parts
  }

  let body = json.to_string(json.object(body_with_prompt))

  Request(
    url: "https://api.klingai.com/v1/videos/image2video",
    method: "POST",
    headers: [
      #("Authorization", get_auth_header(config, timestamp)),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create a request to check task status
pub fn get_task_request(config: Config, task_id: String, timestamp: Int) -> Request {
  Request(
    url: "https://api.klingai.com/v1/videos/tasks/" <> task_id,
    method: "GET",
    headers: [
      #("Authorization", get_auth_header(config, timestamp)),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Create a request to list all tasks
pub fn list_tasks_request(config: Config, timestamp: Int) -> Request {
  Request(
    url: "https://api.klingai.com/v1/videos/tasks",
    method: "GET",
    headers: [
      #("Authorization", get_auth_header(config, timestamp)),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

// ============================================================
// Available Options
// ============================================================

pub fn supported_modes() -> List(String) {
  ["std", "pro"]
}

pub fn mode_descriptions() -> List(#(String, String)) {
  [
    #("std", "Standard mode - Faster, good quality"),
    #("pro", "Professional mode - Higher quality, slower"),
  ]
}

pub fn supported_durations() -> List(String) {
  ["5", "10"]
}

pub fn supported_aspect_ratios() -> List(String) {
  ["16:9", "9:16", "1:1"]
}
