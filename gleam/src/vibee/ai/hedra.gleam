// Hedra Avatar Integration
// API Documentation: https://hedra.com/api

import gleam/json
import gleam/option.{type Option, None, Some}

// ============================================================
// Types
// ============================================================

pub type Config {
  Config(api_key: String)
}

pub type AvatarRequest {
  AvatarRequest(
    audio_url: String,
    image_url: String,
    aspect_ratio: Option(String),
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

pub type JobStatus {
  Pending
  Processing
  Completed(result_url: String)
  Failed(error: String)
}

pub type Job {
  Job(job_id: String, status: JobStatus, created_at: String)
}

// ============================================================
// Request Builders
// ============================================================

/// Create a request to generate an avatar video
pub fn create_avatar_request(config: Config, req: AvatarRequest) -> Request {
  let body_parts = [
    #("audio_url", json.string(req.audio_url)),
    #("image_url", json.string(req.image_url)),
  ]

  let body_with_ratio = case req.aspect_ratio {
    Some(ratio) ->
      [#("aspect_ratio", json.string(ratio)), ..body_parts]
    None -> body_parts
  }

  let body = json.to_string(json.object(body_with_ratio))

  Request(
    url: "https://api.hedra.com/v1/characters",
    method: "POST",
    headers: [
      #("X-API-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create a request to check job status
pub fn get_status_request(config: Config, job_id: String) -> Request {
  Request(
    url: "https://api.hedra.com/v1/characters/" <> job_id,
    method: "GET",
    headers: [
      #("X-API-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Create a request to list all jobs
pub fn list_jobs_request(config: Config) -> Request {
  Request(
    url: "https://api.hedra.com/v1/characters",
    method: "GET",
    headers: [
      #("X-API-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Create a request to cancel a job
pub fn cancel_job_request(config: Config, job_id: String) -> Request {
  Request(
    url: "https://api.hedra.com/v1/characters/" <> job_id <> "/cancel",
    method: "POST",
    headers: [
      #("X-API-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

// ============================================================
// Available Options
// ============================================================

pub fn supported_aspect_ratios() -> List(String) {
  ["1:1", "16:9", "9:16", "4:3", "3:4"]
}

// ============================================================
// Asset Types (Full API)
// ============================================================

pub type AssetType {
  Audio
  Image
}

pub type CreateAssetRequest {
  CreateAssetRequest(name: String, asset_type: AssetType)
}

pub type GenerationRequest {
  GenerationRequest(
    image_asset_id: String,
    audio_asset_id: String,
    resolution: Option(String),
    aspect_ratio: Option(String),
  )
}

// ============================================================
// Asset Upload API
// ============================================================

/// Create a request to create an asset (step 1 of upload)
pub fn create_asset_request(config: Config, req: CreateAssetRequest) -> Request {
  let type_str = case req.asset_type {
    Audio -> "audio"
    Image -> "image"
  }

  let body =
    json.to_string(json.object([
      #("name", json.string(req.name)),
      #("type", json.string(type_str)),
    ]))

  Request(
    url: "https://api.hedra.com/v1/assets",
    method: "POST",
    headers: [
      #("X-API-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create a request to upload asset content (step 2 of upload)
/// asset_id: ID returned from create_asset_request
/// upload_url: URL from the asset content
pub fn upload_asset_request(config: Config, asset_id: String, url: String) -> Request {
  let body =
    json.to_string(json.object([
      #("url", json.string(url)),
    ]))

  Request(
    url: "https://api.hedra.com/v1/assets/" <> asset_id <> "/upload",
    method: "POST",
    headers: [
      #("X-API-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create a request to start generation with uploaded assets
pub fn start_generation_request(config: Config, req: GenerationRequest) -> Request {
  let base_parts = [
    #("imageAssetId", json.string(req.image_asset_id)),
    #("audioAssetId", json.string(req.audio_asset_id)),
    #("aiModelId", json.string(ai_model_id())),
  ]

  let with_resolution = case req.resolution {
    Some(r) -> [#("resolution", json.string(r)), ..base_parts]
    None -> base_parts
  }

  let with_ratio = case req.aspect_ratio {
    Some(ar) -> [#("aspectRatio", json.string(ar)), ..with_resolution]
    None -> with_resolution
  }

  let body = json.to_string(json.object(with_ratio))

  Request(
    url: "https://api.hedra.com/v1/characters",
    method: "POST",
    headers: [
      #("X-API-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Get asset details
pub fn get_asset_request(config: Config, asset_id: String) -> Request {
  Request(
    url: "https://api.hedra.com/v1/assets/" <> asset_id,
    method: "GET",
    headers: [
      #("X-API-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Delete an asset
pub fn delete_asset_request(config: Config, asset_id: String) -> Request {
  Request(
    url: "https://api.hedra.com/v1/assets/" <> asset_id,
    method: "DELETE",
    headers: [
      #("X-API-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

// ============================================================
// Reference Data
// ============================================================

/// Hedra AI Model ID for character generation
pub fn ai_model_id() -> String {
  "d1dd37a3-e39a-4854-a298-6510289f9cf2"
}

/// Supported resolutions
pub fn supported_resolutions() -> List(String) {
  ["540p", "720p"]
}

/// Resolution descriptions
pub fn resolution_descriptions() -> List(#(String, String)) {
  [
    #("540p", "Standard resolution, faster generation"),
    #("720p", "HD resolution, higher quality"),
  ]
}

/// Create default config
pub fn default_config(api_key: String) -> Config {
  Config(api_key: api_key)
}
