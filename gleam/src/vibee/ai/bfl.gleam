// BFL (Black Forest Labs) Image Generation Integration
// API Documentation: https://api.bfl.ml/docs

import gleam/json
import gleam/option.{type Option, None, Some}

// ============================================================
// Types
// ============================================================

pub type Config {
  Config(api_key: String)
}

pub type ImageRequest {
  ImageRequest(
    prompt: String,
    model: String,
    width: Option(Int),
    height: Option(Int),
    steps: Option(Int),
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
  Ready(image_url: String)
  Failed(error: String)
}

pub type Task {
  Task(task_id: String, status: TaskStatus)
}

// ============================================================
// Request Builders
// ============================================================

/// Create a request to generate an image
pub fn generate_image_request(config: Config, req: ImageRequest) -> Request {
  let base_parts = [#("prompt", json.string(req.prompt))]

  let with_width = case req.width {
    Some(w) -> [#("width", json.int(w)), ..base_parts]
    None -> base_parts
  }

  let with_height = case req.height {
    Some(h) -> [#("height", json.int(h)), ..with_width]
    None -> with_width
  }

  let with_steps = case req.steps {
    Some(s) -> [#("num_steps", json.int(s)), ..with_height]
    None -> with_height
  }

  let body = json.to_string(json.object(with_steps))

  Request(
    url: "https://api.bfl.ml/v1/" <> req.model,
    method: "POST",
    headers: [
      #("X-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create a request to get the result of a task
pub fn get_result_request(config: Config, task_id: String) -> Request {
  Request(
    url: "https://api.bfl.ml/v1/get_result?id=" <> task_id,
    method: "GET",
    headers: [
      #("X-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

// ============================================================
// Available Models
// ============================================================

pub fn supported_models() -> List(String) {
  ["flux-pro-1.1", "flux-pro", "flux-dev", "flux-pro-1.1-ultra"]
}

pub fn model_descriptions() -> List(#(String, String)) {
  [
    #("flux-pro-1.1", "Latest FLUX Pro - Highest quality, fastest"),
    #("flux-pro", "FLUX Pro - High quality"),
    #("flux-dev", "FLUX Dev - Development model, free tier"),
    #("flux-pro-1.1-ultra", "FLUX Pro Ultra - Highest resolution"),
  ]
}

// ============================================================
// Helper Functions
// ============================================================

/// Create a simple image request with defaults
pub fn simple_request(prompt: String) -> ImageRequest {
  ImageRequest(
    prompt: prompt,
    model: "flux-pro-1.1",
    width: None,
    height: None,
    steps: None,
  )
}

/// Create a request with specific dimensions
pub fn request_with_size(
  prompt: String,
  width: Int,
  height: Int,
) -> ImageRequest {
  ImageRequest(
    prompt: prompt,
    model: "flux-pro-1.1",
    width: Some(width),
    height: Some(height),
    steps: None,
  )
}

// ============================================================
// Extended Image Request Types (Full API Support)
// ============================================================

/// Full image request with all BFL options
pub type ImageRequestFull {
  ImageRequestFull(
    prompt: String,
    model: String,
    width: Option(Int),
    height: Option(Int),
    steps: Option(Int),
    seed: Option(Int),
    guidance_scale: Option(Float),
    safety_tolerance: Option(Int),
    prompt_upsampling: Option(Bool),
    output_format: Option(String),
  )
}

/// Image-to-image request (for supported models)
pub type ImageToImageRequest {
  ImageToImageRequest(
    prompt: String,
    model: String,
    image_url: String,
    strength: Option(Float),
    seed: Option(Int),
    guidance_scale: Option(Float),
    steps: Option(Int),
  )
}

/// Inpainting request
pub type InpaintingRequest {
  InpaintingRequest(
    prompt: String,
    model: String,
    image_url: String,
    mask_url: String,
    seed: Option(Int),
    guidance_scale: Option(Float),
    steps: Option(Int),
  )
}

/// ControlNet request
pub type ControlNetRequest {
  ControlNetRequest(
    prompt: String,
    model: String,
    control_image_url: String,
    control_type: String,
    control_strength: Option(Float),
    seed: Option(Int),
    guidance_scale: Option(Float),
    steps: Option(Int),
  )
}

// ============================================================
// Extended Request Builders
// ============================================================

/// Create a full image generation request with all options
pub fn generate_image_full_request(
  config: Config,
  req: ImageRequestFull,
) -> Request {
  let base_parts = [#("prompt", json.string(req.prompt))]

  let with_width = case req.width {
    Some(w) -> [#("width", json.int(w)), ..base_parts]
    None -> base_parts
  }

  let with_height = case req.height {
    Some(h) -> [#("height", json.int(h)), ..with_width]
    None -> with_width
  }

  let with_steps = case req.steps {
    Some(s) -> [#("num_steps", json.int(s)), ..with_height]
    None -> with_height
  }

  let with_seed = case req.seed {
    Some(s) -> [#("seed", json.int(s)), ..with_steps]
    None -> with_steps
  }

  let with_guidance = case req.guidance_scale {
    Some(g) -> [#("guidance", json.float(g)), ..with_seed]
    None -> with_seed
  }

  let with_safety = case req.safety_tolerance {
    Some(s) -> [#("safety_tolerance", json.int(s)), ..with_guidance]
    None -> with_guidance
  }

  let with_upsampling = case req.prompt_upsampling {
    Some(u) -> [#("prompt_upsampling", json.bool(u)), ..with_safety]
    None -> with_safety
  }

  let final_parts = case req.output_format {
    Some(f) -> [#("output_format", json.string(f)), ..with_upsampling]
    None -> with_upsampling
  }

  let body = json.to_string(json.object(final_parts))

  Request(
    url: "https://api.bfl.ml/v1/" <> req.model,
    method: "POST",
    headers: [
      #("X-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create an image-to-image request
pub fn image_to_image_request(
  config: Config,
  req: ImageToImageRequest,
) -> Request {
  let base_parts = [
    #("prompt", json.string(req.prompt)),
    #("image_url", json.string(req.image_url)),
  ]

  let with_strength = case req.strength {
    Some(s) -> [#("strength", json.float(s)), ..base_parts]
    None -> base_parts
  }

  let with_seed = case req.seed {
    Some(s) -> [#("seed", json.int(s)), ..with_strength]
    None -> with_strength
  }

  let with_guidance = case req.guidance_scale {
    Some(g) -> [#("guidance", json.float(g)), ..with_seed]
    None -> with_seed
  }

  let final_parts = case req.steps {
    Some(s) -> [#("num_steps", json.int(s)), ..with_guidance]
    None -> with_guidance
  }

  let body = json.to_string(json.object(final_parts))

  Request(
    url: "https://api.bfl.ml/v1/" <> req.model <> "/image-to-image",
    method: "POST",
    headers: [
      #("X-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create an inpainting request
pub fn inpainting_request(config: Config, req: InpaintingRequest) -> Request {
  let base_parts = [
    #("prompt", json.string(req.prompt)),
    #("image_url", json.string(req.image_url)),
    #("mask_url", json.string(req.mask_url)),
  ]

  let with_seed = case req.seed {
    Some(s) -> [#("seed", json.int(s)), ..base_parts]
    None -> base_parts
  }

  let with_guidance = case req.guidance_scale {
    Some(g) -> [#("guidance", json.float(g)), ..with_seed]
    None -> with_seed
  }

  let final_parts = case req.steps {
    Some(s) -> [#("num_steps", json.int(s)), ..with_guidance]
    None -> with_guidance
  }

  let body = json.to_string(json.object(final_parts))

  Request(
    url: "https://api.bfl.ml/v1/" <> req.model <> "/inpaint",
    method: "POST",
    headers: [
      #("X-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create a ControlNet request
pub fn controlnet_request(config: Config, req: ControlNetRequest) -> Request {
  let base_parts = [
    #("prompt", json.string(req.prompt)),
    #("control_image", json.string(req.control_image_url)),
    #("control_type", json.string(req.control_type)),
  ]

  let with_strength = case req.control_strength {
    Some(s) -> [#("control_strength", json.float(s)), ..base_parts]
    None -> base_parts
  }

  let with_seed = case req.seed {
    Some(s) -> [#("seed", json.int(s)), ..with_strength]
    None -> with_strength
  }

  let with_guidance = case req.guidance_scale {
    Some(g) -> [#("guidance", json.float(g)), ..with_seed]
    None -> with_seed
  }

  let final_parts = case req.steps {
    Some(s) -> [#("num_steps", json.int(s)), ..with_guidance]
    None -> with_guidance
  }

  let body = json.to_string(json.object(final_parts))

  Request(
    url: "https://api.bfl.ml/v1/" <> req.model <> "/controlnet",
    method: "POST",
    headers: [
      #("X-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

// ============================================================
// Helper Functions for Extended API
// ============================================================

/// Create a full request with seed for reproducibility
pub fn request_with_seed(prompt: String, seed: Int) -> ImageRequestFull {
  ImageRequestFull(
    prompt: prompt,
    model: "flux-pro-1.1",
    width: None,
    height: None,
    steps: None,
    seed: Some(seed),
    guidance_scale: None,
    safety_tolerance: None,
    prompt_upsampling: None,
    output_format: None,
  )
}

/// Create a request with custom guidance scale
pub fn request_with_guidance(
  prompt: String,
  guidance_scale: Float,
) -> ImageRequestFull {
  ImageRequestFull(
    prompt: prompt,
    model: "flux-pro-1.1",
    width: None,
    height: None,
    steps: None,
    seed: None,
    guidance_scale: Some(guidance_scale),
    safety_tolerance: None,
    prompt_upsampling: None,
    output_format: None,
  )
}

/// Create a full request with all custom parameters
pub fn full_request(
  prompt: String,
  model: String,
  width: Int,
  height: Int,
  steps: Int,
  seed: Int,
  guidance_scale: Float,
) -> ImageRequestFull {
  ImageRequestFull(
    prompt: prompt,
    model: model,
    width: Some(width),
    height: Some(height),
    steps: Some(steps),
    seed: Some(seed),
    guidance_scale: Some(guidance_scale),
    safety_tolerance: None,
    prompt_upsampling: None,
    output_format: None,
  )
}

/// Create image-to-image helper
pub fn simple_img2img(
  prompt: String,
  image_url: String,
  strength: Float,
) -> ImageToImageRequest {
  ImageToImageRequest(
    prompt: prompt,
    model: "flux-pro-1.1",
    image_url: image_url,
    strength: Some(strength),
    seed: None,
    guidance_scale: None,
    steps: None,
  )
}

/// Create inpainting helper
pub fn simple_inpaint(
  prompt: String,
  image_url: String,
  mask_url: String,
) -> InpaintingRequest {
  InpaintingRequest(
    prompt: prompt,
    model: "flux-pro-1.1",
    image_url: image_url,
    mask_url: mask_url,
    seed: None,
    guidance_scale: None,
    steps: None,
  )
}

/// Create ControlNet helper
pub fn simple_controlnet(
  prompt: String,
  control_image_url: String,
  control_type: String,
) -> ControlNetRequest {
  ControlNetRequest(
    prompt: prompt,
    model: "flux-pro-1.1",
    control_image_url: control_image_url,
    control_type: control_type,
    control_strength: None,
    seed: None,
    guidance_scale: None,
    steps: None,
  )
}

// ============================================================
// Available Options
// ============================================================

/// Supported output formats
pub fn supported_output_formats() -> List(String) {
  ["jpeg", "png", "webp"]
}

/// Supported ControlNet types
pub fn supported_control_types() -> List(#(String, String)) {
  [
    #("canny", "Edge detection - extracts edges from image"),
    #("depth", "Depth estimation - uses depth map"),
    #("soft_edge", "Soft edge detection - HED style"),
    #("pose", "Pose estimation - OpenPose style"),
  ]
}

/// Safety tolerance levels (0-6, higher = less strict)
pub fn safety_tolerance_levels() -> List(#(Int, String)) {
  [
    #(0, "Most strict - block most potentially unsafe content"),
    #(2, "Moderate - balanced filtering"),
    #(4, "Lenient - allow more artistic content"),
    #(6, "Least strict - minimal filtering"),
  ]
}

/// Recommended steps by model
pub fn recommended_steps() -> List(#(String, Int)) {
  [
    #("flux-pro-1.1", 28),
    #("flux-pro", 25),
    #("flux-dev", 20),
    #("flux-pro-1.1-ultra", 40),
  ]
}

/// Guidance scale recommendations
pub fn guidance_recommendations() -> List(#(String, String)) {
  [
    #("1.5-3.0", "Low guidance - more creative, less prompt adherence"),
    #("3.5-5.0", "Balanced - good prompt adherence with creativity"),
    #("5.5-7.5", "High guidance - strong prompt adherence"),
    #("8.0+", "Very high - strictly follows prompt, may reduce quality"),
  ]
}

/// Common aspect ratios as width/height pairs
pub fn common_aspect_ratios() -> List(#(String, #(Int, Int))) {
  [
    #("1:1 Square", #(1024, 1024)),
    #("16:9 Landscape", #(1344, 768)),
    #("9:16 Portrait", #(768, 1344)),
    #("4:3 Standard", #(1152, 864)),
    #("3:4 Portrait", #(864, 1152)),
    #("21:9 Ultrawide", #(1536, 640)),
  ]
}

/// Create default config with API key
pub fn default_config(api_key: String) -> Config {
  Config(api_key: api_key)
}
