// FAL.ai Image Generation Integration
// API Documentation: https://fal.ai/docs
// Supports: FLUX LoRA, Nano Banana Pro, Flux Kontext

import gleam/json
import gleam/option.{type Option, None, Some}

// ============================================================
// Types
// ============================================================

pub type Config {
  Config(api_key: String)
}

pub type Request {
  Request(
    url: String,
    method: String,
    headers: List(#(String, String)),
    body: String,
  )
}

// ============================================================
// NeuroPhoto with LoRA Types
// ============================================================

pub type LoraWeight {
  LoraWeight(path: String, scale: Float)
}

pub type NeuroPhotoRequest {
  NeuroPhotoRequest(
    prompt: String,
    lora_url: String,
    num_images: Int,
    image_size: ImageSize,
    seed: Option(Int),
    guidance_scale: Option(Float),
    num_inference_steps: Option(Int),
    enable_safety_checker: Bool,
  )
}

pub type ImageSize {
  ImageSize(width: Int, height: Int)
}

// ============================================================
// Nano Banana Pro Types
// ============================================================

pub type NanoBananaRequest {
  NanoBananaRequest(
    prompt: String,
    num_images: Int,
    aspect_ratio: String,
    resolution: String,
    seed: Option(Int),
  )
}

// ============================================================
// Flux Kontext Types
// ============================================================

pub type FluxKontextMode {
  Quick
  Single
  Multi
  PortraitSeries
  Haircut
  Landmarks
  Headshot
}

pub type FluxKontextRequest {
  FluxKontextRequest(
    prompt: String,
    input_image_url: String,
    model_type: String,
    mode: FluxKontextMode,
    aspect_ratio: Option(String),
    seed: Option(Int),
  )
}

// ============================================================
// NeuroPhoto Request Builder (FAL FLUX LoRA)
// ============================================================

/// Create a NeuroPhoto request with NEURO_SAGE LoRA
pub fn neuro_photo_request(config: Config, req: NeuroPhotoRequest) -> Request {
  let loras = json.array(
    [
      json.object([
        #("path", json.string(req.lora_url)),
        #("scale", json.float(1.0)),
      ]),
    ],
    fn(x) { x },
  )

  let image_size =
    json.object([
      #("width", json.int(req.image_size.width)),
      #("height", json.int(req.image_size.height)),
    ])

  let base_parts = [
    #("prompt", json.string(req.prompt)),
    #("loras", loras),
    #("num_images", json.int(req.num_images)),
    #("image_size", image_size),
    #("enable_safety_checker", json.bool(req.enable_safety_checker)),
    #("output_format", json.string("jpeg")),
    #("sync_mode", json.bool(True)),
  ]

  let with_seed = case req.seed {
    Some(s) -> [#("seed", json.int(s)), ..base_parts]
    None -> base_parts
  }

  let with_guidance = case req.guidance_scale {
    Some(g) -> [#("guidance_scale", json.float(g)), ..with_seed]
    None -> with_seed
  }

  let final_parts = case req.num_inference_steps {
    Some(s) -> [#("num_inference_steps", json.int(s)), ..with_guidance]
    None -> with_guidance
  }

  let body = json.to_string(json.object(final_parts))

  Request(
    url: "https://queue.fal.run/fal-ai/flux-lora",
    method: "POST",
    headers: [
      #("Authorization", "Key " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

// ============================================================
// Nano Banana Pro Request Builder
// ============================================================

/// Create a Google Nano Banana Pro request
pub fn nano_banana_request(config: Config, req: NanoBananaRequest) -> Request {
  let base_parts = [
    #("prompt", json.string(req.prompt)),
    #("num_images", json.int(req.num_images)),
    #("aspect_ratio", json.string(req.aspect_ratio)),
    #("resolution", json.string(req.resolution)),
    #("sync_mode", json.bool(True)),
  ]

  let final_parts = case req.seed {
    Some(s) -> [#("seed", json.int(s)), ..base_parts]
    None -> base_parts
  }

  let body = json.to_string(json.object(final_parts))

  Request(
    url: "https://queue.fal.run/fal-ai/nano-banana-pro",
    method: "POST",
    headers: [
      #("Authorization", "Key " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

// ============================================================
// Flux Kontext Request Builder
// ============================================================

/// Create a FLUX Kontext request (Pro or Max)
pub fn flux_kontext_request(config: Config, req: FluxKontextRequest) -> Request {
  let model_url = case req.model_type {
    "max" -> "https://queue.fal.run/black-forest-labs/flux-kontext-max"
    _ -> "https://queue.fal.run/black-forest-labs/flux-kontext-pro"
  }

  let base_parts = [
    #("prompt", json.string(req.prompt)),
    #("input_image_url", json.string(req.input_image_url)),
    #("sync_mode", json.bool(True)),
    #("output_format", json.string("jpeg")),
  ]

  let with_aspect = case req.aspect_ratio {
    Some(ar) -> [#("aspect_ratio", json.string(ar)), ..base_parts]
    None -> base_parts
  }

  let final_parts = case req.seed {
    Some(s) -> [#("seed", json.int(s)), ..with_aspect]
    None -> with_aspect
  }

  let body = json.to_string(json.object(final_parts))

  Request(
    url: model_url,
    method: "POST",
    headers: [
      #("Authorization", "Key " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

// ============================================================
// Queue Status Request
// ============================================================

/// Get the status of a queued request
pub fn get_queue_status(config: Config, request_id: String) -> Request {
  Request(
    url: "https://queue.fal.run/requests/" <> request_id <> "/status",
    method: "GET",
    headers: [
      #("Authorization", "Key " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Get the result of a completed request
pub fn get_queue_result(config: Config, request_id: String) -> Request {
  Request(
    url: "https://queue.fal.run/requests/" <> request_id,
    method: "GET",
    headers: [
      #("Authorization", "Key " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

// ============================================================
// Helper Functions
// ============================================================

/// Create a simple NeuroPhoto request with defaults
pub fn simple_neuro_photo(prompt: String, lora_url: String) -> NeuroPhotoRequest {
  NeuroPhotoRequest(
    prompt: prompt,
    lora_url: lora_url,
    num_images: 1,
    image_size: ImageSize(width: 768, height: 1365),
    seed: None,
    guidance_scale: Some(3.5),
    num_inference_steps: Some(28),
    enable_safety_checker: True,
  )
}

/// Create a NeuroPhoto request with NEURO_SAGE trigger word
pub fn neuro_sage_request(base_prompt: String, lora_url: String) -> NeuroPhotoRequest {
  let full_prompt = "NEURO_SAGE " <> base_prompt
  simple_neuro_photo(full_prompt, lora_url)
}

/// Create a simple Nano Banana request
pub fn simple_nano_banana(prompt: String) -> NanoBananaRequest {
  NanoBananaRequest(
    prompt: prompt,
    num_images: 1,
    aspect_ratio: "9:16",
    resolution: "1K",
    seed: None,
  )
}

/// Create a simple Flux Kontext request
pub fn simple_flux_kontext(
  prompt: String,
  image_url: String,
  model: String,
) -> FluxKontextRequest {
  FluxKontextRequest(
    prompt: prompt,
    input_image_url: image_url,
    model_type: model,
    mode: Single,
    aspect_ratio: None,
    seed: None,
  )
}

/// Create default config
pub fn default_config(api_key: String) -> Config {
  Config(api_key: api_key)
}

// ============================================================
// Preset Image Sizes
// ============================================================

/// 9:16 Portrait (vertical)
pub fn portrait_size() -> ImageSize {
  ImageSize(width: 768, height: 1365)
}

/// 16:9 Landscape (horizontal)
pub fn landscape_size() -> ImageSize {
  ImageSize(width: 1365, height: 768)
}

/// 1:1 Square
pub fn square_size() -> ImageSize {
  ImageSize(width: 1024, height: 1024)
}

/// 4:3 Standard
pub fn standard_size() -> ImageSize {
  ImageSize(width: 1152, height: 864)
}

// ============================================================
// Supported Models and Modes
// ============================================================

/// Supported Flux Kontext models
pub fn kontext_models() -> List(#(String, String)) {
  [
    #("pro", "FLUX Kontext Pro - Fast, high quality"),
    #("max", "FLUX Kontext Max - Highest quality, slower"),
  ]
}

/// Supported aspect ratios
pub fn supported_aspect_ratios() -> List(String) {
  ["1:1", "16:9", "9:16", "4:3", "3:4", "21:9", "9:21"]
}

/// Supported Nano Banana resolutions
pub fn supported_resolutions() -> List(#(String, String)) {
  [
    #("1K", "1024px - Fast generation"),
    #("2K", "2048px - Balanced quality"),
    #("4K", "4096px - Highest quality, slower"),
  ]
}

/// Kontext mode to string for API
pub fn mode_to_string(mode: FluxKontextMode) -> String {
  case mode {
    Quick -> "quick"
    Single -> "single"
    Multi -> "multi"
    PortraitSeries -> "portrait_series"
    Haircut -> "haircut"
    Landmarks -> "landmarks"
    Headshot -> "headshot"
  }
}

// ============================================================
// Pricing Information
// ============================================================

/// Get pricing info for FAL models
pub fn pricing() -> List(#(String, String)) {
  [
    #("flux-lora", "$0.025 per image (NeuroPhoto)"),
    #("nano-banana-pro", "$0.0398 per image (~25 images per $1)"),
    #("flux-kontext-pro", "$0.035 per image"),
    #("flux-kontext-max", "$0.045 per image"),
    #("veed-fabric-1.0-480p", "$0.08 per second"),
    #("veed-fabric-1.0-720p", "$0.15 per second"),
  ]
}

// ============================================================
// VEED Fabric 1.0 (Lipsync) Types
// ============================================================

pub type FabricResolution {
  Resolution480p
  Resolution720p
}

pub type FabricLipsyncRequest {
  FabricLipsyncRequest(
    image_url: String,
    audio_url: String,
    resolution: FabricResolution,
  )
}

// ============================================================
// VEED Fabric 1.0 Lipsync Request Builder
// ============================================================

/// Create a VEED Fabric 1.0 lipsync request
/// Turns an image + audio into a talking head video
pub fn fabric_lipsync_request(config: Config, req: FabricLipsyncRequest) -> Request {
  let resolution_str = case req.resolution {
    Resolution480p -> "480p"
    Resolution720p -> "720p"
  }

  let body = json.to_string(json.object([
    #("image_url", json.string(req.image_url)),
    #("audio_url", json.string(req.audio_url)),
    #("resolution", json.string(resolution_str)),
  ]))

  Request(
    url: "https://queue.fal.run/veed/fabric-1.0",
    method: "POST",
    headers: [
      #("Authorization", "Key " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create a simple lipsync request with 720p default
pub fn simple_fabric_lipsync(image_url: String, audio_url: String) -> FabricLipsyncRequest {
  FabricLipsyncRequest(
    image_url: image_url,
    audio_url: audio_url,
    resolution: Resolution720p,
  )
}

/// Create a lipsync request with 480p for faster/cheaper generation
pub fn fast_fabric_lipsync(image_url: String, audio_url: String) -> FabricLipsyncRequest {
  FabricLipsyncRequest(
    image_url: image_url,
    audio_url: audio_url,
    resolution: Resolution480p,
  )
}
