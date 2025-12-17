// AI Image Editing Integration
// Supports: SeeDream-4.5 (ByteDance), Qwen Image Edit (Alibaba)
// API: Replicate

import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}

// ============================================================
// Types
// ============================================================

pub type Config {
  Config(api_token: String)
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
// SeeDream-4.5 Types
// ============================================================

pub type SeeDreamSize {
  K1
  K2
  K4
  Custom(width: Int, height: Int)
}

pub type SeeDreamRequest {
  SeeDreamRequest(
    prompt: String,
    size: SeeDreamSize,
    max_images: Int,
    image_input: Option(List(String)),
    aspect_ratio: Option(String),
  )
}

// ============================================================
// Qwen Image Edit Types
// ============================================================

pub type QwenEditMode {
  Semantic
  Appearance
  Auto
}

pub type QwenImageEditRequest {
  QwenImageEditRequest(
    prompt: String,
    image: String,
    editing_mode: QwenEditMode,
    preserve_quality: Bool,
    seed: Option(Int),
    output_format: String,
  )
}

// ============================================================
// SeeDream-4.5 Request Builders
// ============================================================

/// Create a SeeDream-4.5 prediction request
pub fn create_seedream_request(
  config: Config,
  req: SeeDreamRequest,
) -> Request {
  // Get dimensions based on size
  let #(width, height) = case req.size {
    K1 -> #(1024, 1536)
    K2 -> #(1365, 2048)
    K4 -> #(2731, 4096)
    Custom(w, h) -> #(w, h)
  }

  let mut_input_parts = [
    #("prompt", json.string(req.prompt)),
    #("width", json.int(width)),
    #("height", json.int(height)),
    #("max_images", json.int(req.max_images)),
  ]

  // Add image_input if provided
  let input_parts = case req.image_input {
    Some(images) -> {
      let images_json = json.array(images, fn(url) { json.string(url) })
      [#("image_input", images_json), ..mut_input_parts]
    }
    None -> mut_input_parts
  }

  // Add aspect_ratio if provided
  let final_input = case req.aspect_ratio {
    Some(ar) -> [#("aspect_ratio", json.string(ar)), ..input_parts]
    None -> input_parts
  }

  let body =
    json.to_string(json.object([
      #("version", json.string("bytedance/seedream-4.5")),
      #("input", json.object(final_input)),
    ]))

  Request(
    url: "https://api.replicate.com/v1/predictions",
    method: "POST",
    headers: [
      #("Authorization", "Bearer " <> config.api_token),
      #("Content-Type", "application/json"),
      #("Prefer", "wait"),
    ],
    body: body,
  )
}

// ============================================================
// Qwen Image Edit Request Builders
// ============================================================

/// Create a Qwen Image Edit prediction request
pub fn create_qwen_edit_request(
  config: Config,
  req: QwenImageEditRequest,
) -> Request {
  let mode_string = case req.editing_mode {
    Semantic -> "semantic"
    Appearance -> "appearance"
    Auto -> "auto"
  }

  let mut_input_parts = [
    #("prompt", json.string(req.prompt)),
    #("image", json.string(req.image)),
    #("editing_mode", json.string(mode_string)),
    #("preserve_quality", json.bool(req.preserve_quality)),
    #("output_format", json.string(req.output_format)),
  ]

  let input_parts = case req.seed {
    Some(s) -> [#("seed", json.int(s)), ..mut_input_parts]
    None -> mut_input_parts
  }

  let body =
    json.to_string(json.object([
      #("version", json.string("qwen/qwen-image-edit")),
      #("input", json.object(input_parts)),
    ]))

  Request(
    url: "https://api.replicate.com/v1/predictions",
    method: "POST",
    headers: [
      #("Authorization", "Bearer " <> config.api_token),
      #("Content-Type", "application/json"),
      #("Prefer", "wait"),
    ],
    body: body,
  )
}

/// Get prediction status
pub fn get_prediction_status(config: Config, prediction_id: String) -> Request {
  Request(
    url: "https://api.replicate.com/v1/predictions/" <> prediction_id,
    method: "GET",
    headers: [
      #("Authorization", "Bearer " <> config.api_token),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

// ============================================================
// Helper Functions
// ============================================================

/// Create default config
pub fn default_config(api_token: String) -> Config {
  Config(api_token: api_token)
}

/// Create simple SeeDream request
pub fn simple_seedream(prompt: String) -> SeeDreamRequest {
  SeeDreamRequest(
    prompt: prompt,
    size: K1,
    max_images: 1,
    image_input: None,
    aspect_ratio: None,
  )
}

/// Create SeeDream request with specific size
pub fn seedream_with_size(prompt: String, size: SeeDreamSize) -> SeeDreamRequest {
  SeeDreamRequest(
    prompt: prompt,
    size: size,
    max_images: 1,
    image_input: None,
    aspect_ratio: None,
  )
}

/// Create SeeDream request with image input (for image editing)
pub fn seedream_with_image(
  prompt: String,
  image_url: String,
) -> SeeDreamRequest {
  SeeDreamRequest(
    prompt: prompt,
    size: K1,
    max_images: 1,
    image_input: Some([image_url]),
    aspect_ratio: None,
  )
}

/// Create simple Qwen edit request
pub fn simple_qwen_edit(prompt: String, image_url: String) -> QwenImageEditRequest {
  QwenImageEditRequest(
    prompt: prompt,
    image: image_url,
    editing_mode: Auto,
    preserve_quality: True,
    seed: None,
    output_format: "png",
  )
}

/// Create Qwen edit with specific mode
pub fn qwen_edit_with_mode(
  prompt: String,
  image_url: String,
  mode: QwenEditMode,
) -> QwenImageEditRequest {
  QwenImageEditRequest(
    prompt: prompt,
    image: image_url,
    editing_mode: mode,
    preserve_quality: True,
    seed: None,
    output_format: "png",
  )
}

/// Create full Qwen edit request
pub fn qwen_edit_full(
  prompt: String,
  image_url: String,
  mode: QwenEditMode,
  preserve_quality: Bool,
  seed: Option(Int),
  output_format: String,
) -> QwenImageEditRequest {
  QwenImageEditRequest(
    prompt: prompt,
    image: image_url,
    editing_mode: mode,
    preserve_quality: preserve_quality,
    seed: seed,
    output_format: output_format,
  )
}

// ============================================================
// Model Information
// ============================================================

pub fn supported_seedream_sizes() -> List(#(String, String)) {
  [
    #("1K", "1024x1536 - Fast generation"),
    #("2K", "1365x2048 - Balanced quality"),
    #("4K", "2731x4096 - Highest quality"),
    #("custom", "Custom dimensions (1024-4096px)"),
  ]
}

pub fn supported_qwen_modes() -> List(#(String, String)) {
  [
    #("auto", "Automatic - Detect best editing mode"),
    #("semantic", "Semantic - Restructure and modify scene"),
    #("appearance", "Appearance - Local adjustments and details"),
  ]
}

pub fn supported_output_formats() -> List(String) {
  ["png", "jpg", "webp"]
}

pub fn model_info() -> List(#(String, String)) {
  [
    #("SeeDream-4.5", "ByteDance - Up to 4K image generation"),
    #("Qwen Image Edit", "Alibaba - 20B parameter image editing"),
  ]
}

// ============================================================
// Pricing Information
// ============================================================

pub fn pricing() -> List(#(String, String)) {
  [
    #("SeeDream-4.5 1K", "$0.015 per image"),
    #("SeeDream-4.5 2K", "$0.025 per image"),
    #("SeeDream-4.5 4K", "$0.050 per image"),
    #("Qwen Image Edit", "$0.025 per edit"),
  ]
}
