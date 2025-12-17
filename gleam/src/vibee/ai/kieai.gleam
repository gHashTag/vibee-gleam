// KIE AI (Veo3) Video Generation Integration
// API Documentation: https://api.kie.ai/api/v1

import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}

// ============================================================
// Types
// ============================================================

pub type Config {
  Config(api_key: String)
}

pub type VideoRequest {
  VideoRequest(
    prompt: String,
    seeds: Int,
    model: Option(String),
    aspect_ratio: Option(String),
  )
}

pub type ImageToVideoRequest {
  ImageToVideoRequest(
    prompt: Option(String),
    image_url: String,
    seeds: Int,
    model: Option(String),
    duration: Option(Int),
  )
}

pub type VeedFabricRequest {
  VeedFabricRequest(
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

pub type TaskStatus {
  Processing
  Success(video_urls: List(String))
  Failed(error: String)
}

pub type Task {
  Task(task_id: String, status: TaskStatus)
}

// ============================================================
// Constants
// ============================================================

const base_url = "https://api.kie.ai/api/v1"

// ============================================================
// Request Builders
// ============================================================

/// Create a request to generate a video from prompt
pub fn create_video_request(config: Config, req: VideoRequest) -> Request {
  let model = case req.model {
    Some(m) -> m
    None -> "veo3_fast"
  }

  let aspect_ratio = case req.aspect_ratio {
    Some(ar) -> ar
    None -> "9:16"
  }

  let body =
    json.to_string(json.object([
      #("prompt", json.string(req.prompt)),
      #("seeds", json.int(req.seeds)),
      #("model", json.string(model)),
      #("aspectRatio", json.string(aspect_ratio)),
    ]))

  Request(
    url: base_url <> "/veo/generate",
    method: "POST",
    headers: [
      #("Authorization", "Bearer " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create a request to check task status
pub fn get_task_status_request(config: Config, task_id: String) -> Request {
  Request(
    url: base_url <> "/veo/record-info?taskId=" <> task_id,
    method: "GET",
    headers: [
      #("Authorization", "Bearer " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Create a request to list all videos
pub fn list_videos_request(config: Config, limit: Int, offset: Int) -> Request {
  Request(
    url: base_url
      <> "/veo/records?limit="
      <> int.to_string(limit)
      <> "&offset="
      <> int.to_string(offset),
    method: "GET",
    headers: [
      #("Authorization", "Bearer " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Create a request to generate video from image (I2V)
pub fn create_image_to_video_request(
  config: Config,
  req: ImageToVideoRequest,
) -> Request {
  let model = case req.model {
    Some(m) -> m
    None -> "wan-2.5-i2v"
  }

  let duration = case req.duration {
    Some(d) -> d
    None -> 5
  }

  let base_parts = [
    #("imageUrl", json.string(req.image_url)),
    #("seeds", json.int(req.seeds)),
    #("model", json.string(model)),
    #("duration", json.int(duration)),
  ]

  let final_parts = case req.prompt {
    Some(p) -> [#("prompt", json.string(p)), ..base_parts]
    None -> base_parts
  }

  let body = json.to_string(json.object(final_parts))

  Request(
    url: base_url <> "/veo/generate",
    method: "POST",
    headers: [
      #("Authorization", "Bearer " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create a request to generate Veed Fabric lip-sync video
pub fn create_veed_fabric_request(
  config: Config,
  req: VeedFabricRequest,
) -> Request {
  let aspect_ratio = case req.aspect_ratio {
    Some(ar) -> ar
    None -> "9:16"
  }

  let body =
    json.to_string(json.object([
      #("audioUrl", json.string(req.audio_url)),
      #("imageUrl", json.string(req.image_url)),
      #("aspectRatio", json.string(aspect_ratio)),
    ]))

  Request(
    url: base_url <> "/veed-fabric",
    method: "POST",
    headers: [
      #("Authorization", "Bearer " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

// ============================================================
// Helper Functions
// ============================================================

/// Create default config with API key
pub fn default_config(api_key: String) -> Config {
  Config(api_key: api_key)
}

/// Create simple video request with default settings
pub fn simple_video_request(prompt: String) -> VideoRequest {
  VideoRequest(
    prompt: prompt,
    seeds: generate_seed(),
    model: None,
    aspect_ratio: None,
  )
}

/// Create video request with specific aspect ratio
pub fn video_request_with_ratio(
  prompt: String,
  aspect_ratio: String,
) -> VideoRequest {
  VideoRequest(
    prompt: prompt,
    seeds: generate_seed(),
    model: None,
    aspect_ratio: Some(aspect_ratio),
  )
}

/// Create video request with all options
pub fn video_request_full(
  prompt: String,
  seeds: Int,
  model: String,
  aspect_ratio: String,
) -> VideoRequest {
  VideoRequest(
    prompt: prompt,
    seeds: seeds,
    model: Some(model),
    aspect_ratio: Some(aspect_ratio),
  )
}

/// Create simple image-to-video request
pub fn simple_image_to_video(image_url: String) -> ImageToVideoRequest {
  ImageToVideoRequest(
    prompt: None,
    image_url: image_url,
    seeds: generate_seed(),
    model: None,
    duration: None,
  )
}

/// Create image-to-video with prompt
pub fn image_to_video_with_prompt(
  image_url: String,
  prompt: String,
) -> ImageToVideoRequest {
  ImageToVideoRequest(
    prompt: Some(prompt),
    image_url: image_url,
    seeds: generate_seed(),
    model: None,
    duration: None,
  )
}

/// Create full image-to-video request
pub fn image_to_video_full(
  image_url: String,
  prompt: Option(String),
  seeds: Int,
  model: String,
  duration: Int,
) -> ImageToVideoRequest {
  ImageToVideoRequest(
    prompt: prompt,
    image_url: image_url,
    seeds: seeds,
    model: Some(model),
    duration: Some(duration),
  )
}

/// Create simple Veed Fabric lip-sync request
pub fn simple_veed_fabric(
  audio_url: String,
  image_url: String,
) -> VeedFabricRequest {
  VeedFabricRequest(
    audio_url: audio_url,
    image_url: image_url,
    aspect_ratio: None,
  )
}

/// Create Veed Fabric request with aspect ratio
pub fn veed_fabric_with_ratio(
  audio_url: String,
  image_url: String,
  aspect_ratio: String,
) -> VeedFabricRequest {
  VeedFabricRequest(
    audio_url: audio_url,
    image_url: image_url,
    aspect_ratio: Some(aspect_ratio),
  )
}

/// Generate random seed (placeholder - should use actual random)
fn generate_seed() -> Int {
  // In production, use proper random number generation
  // For now, return a fixed value that can be overridden
  50000
}

// ============================================================
// Available Options
// ============================================================

pub fn supported_models() -> List(String) {
  ["veo3_fast", "veo3", "wan-2.5-t2v", "wan-2.5-i2v"]
}

pub fn model_descriptions() -> List(#(String, String)) {
  [
    #("veo3_fast", "Veo3 Fast - Quick generation, good quality"),
    #("veo3", "Veo3 - Higher quality, slower generation"),
    #("wan-2.5-t2v", "WAN 2.5 Text-to-Video - High quality T2V"),
    #("wan-2.5-i2v", "WAN 2.5 Image-to-Video - Animate still images"),
  ]
}

pub fn supported_aspect_ratios() -> List(String) {
  ["9:16", "16:9", "1:1"]
}

pub fn aspect_ratio_descriptions() -> List(#(String, String)) {
  [
    #("9:16", "Portrait (TikTok, Instagram Reels)"),
    #("16:9", "Landscape (YouTube)"),
    #("1:1", "Square (Instagram Posts)"),
  ]
}

/// Valid seed range: 10000-99999
pub fn seed_range() -> #(Int, Int) {
  #(10_000, 99_999)
}

// ============================================================
// Status Parsing
// ============================================================

/// Parse successFlag from API response
/// -1: processing, 1: success, 2: failed, 3: error
pub fn parse_success_flag(flag: Int) -> TaskStatus {
  case flag {
    1 -> Success([])
    2 -> Failed("Generation failed")
    3 -> Failed("Generation error")
    _ -> Processing
  }
}
