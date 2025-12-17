// Replicate API Integration
// API Documentation: https://replicate.com/docs/reference/http

import gleam/json
import gleam/option.{type Option, None, Some}

// ============================================================
// Types
// ============================================================

pub type Config {
  Config(api_token: String)
}

pub type PredictionRequest {
  PredictionRequest(
    model: String,
    version: Option(String),
    input: List(#(String, json.Json)),
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

pub type PredictionStatus {
  Starting
  Processing
  Succeeded(output: String)
  Failed(error: String)
  Canceled
}

pub type Prediction {
  Prediction(id: String, status: PredictionStatus, model: String)
}

// ============================================================
// Request Builders
// ============================================================

/// Create a prediction request
pub fn create_prediction_request(
  config: Config,
  req: PredictionRequest,
) -> Request {
  let input_json = json.object(req.input)

  let body_parts = case req.version {
    Some(v) -> [
      #("version", json.string(v)),
      #("input", input_json),
    ]
    None -> [
      #("model", json.string(req.model)),
      #("input", input_json),
    ]
  }

  let body = json.to_string(json.object(body_parts))

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

/// Create a request to get prediction status
pub fn get_prediction_request(config: Config, prediction_id: String) -> Request {
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

/// Create a request to cancel a prediction
pub fn cancel_prediction_request(
  config: Config,
  prediction_id: String,
) -> Request {
  Request(
    url: "https://api.replicate.com/v1/predictions/" <> prediction_id <> "/cancel",
    method: "POST",
    headers: [
      #("Authorization", "Bearer " <> config.api_token),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Create a request to list predictions
pub fn list_predictions_request(config: Config) -> Request {
  Request(
    url: "https://api.replicate.com/v1/predictions",
    method: "GET",
    headers: [
      #("Authorization", "Bearer " <> config.api_token),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Create a request to get a model
pub fn get_model_request(config: Config, owner: String, name: String) -> Request {
  Request(
    url: "https://api.replicate.com/v1/models/" <> owner <> "/" <> name,
    method: "GET",
    headers: [
      #("Authorization", "Bearer " <> config.api_token),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

// ============================================================
// Popular Models
// ============================================================

pub fn popular_models() -> List(#(String, String)) {
  [
    #("stability-ai/sdxl", "Stable Diffusion XL - High quality images"),
    #("lucataco/sdxl-lightning-4step", "SDXL Lightning - Fast generation"),
    #("black-forest-labs/flux-schnell", "FLUX Schnell - Fast, high quality"),
    #("black-forest-labs/flux-dev", "FLUX Dev - Development model"),
    #("meta/llama-3.1-405b-instruct", "Llama 3.1 405B - Large language model"),
    #("openai/whisper", "Whisper - Speech to text"),
    #("cjwbw/rembg", "RemBG - Background removal"),
    #("nightmareai/real-esrgan", "Real-ESRGAN - Image upscaling"),
    #("lucataco/animate-diff", "AnimateDiff - Image animation"),
    #("fofr/face-to-sticker", "Face to Sticker - Convert faces"),
  ]
}

// ============================================================
// Helper Functions
// ============================================================

/// Create a simple image generation request
pub fn simple_image_request(prompt: String) -> PredictionRequest {
  PredictionRequest(
    model: "stability-ai/sdxl",
    version: None,
    input: [#("prompt", json.string(prompt))],
  )
}

/// Create a request for FLUX model
pub fn flux_request(prompt: String) -> PredictionRequest {
  PredictionRequest(
    model: "black-forest-labs/flux-schnell",
    version: None,
    input: [
      #("prompt", json.string(prompt)),
      #("num_outputs", json.int(1)),
    ],
  )
}

/// Create a Whisper transcription request
pub fn whisper_request(audio_url: String) -> PredictionRequest {
  PredictionRequest(
    model: "openai/whisper",
    version: None,
    input: [#("audio", json.string(audio_url))],
  )
}

/// Create a background removal request
pub fn rembg_request(image_url: String) -> PredictionRequest {
  PredictionRequest(
    model: "cjwbw/rembg",
    version: None,
    input: [#("image", json.string(image_url))],
  )
}

/// Create an upscaling request
pub fn upscale_request(image_url: String, scale: Int) -> PredictionRequest {
  PredictionRequest(
    model: "nightmareai/real-esrgan",
    version: None,
    input: [
      #("image", json.string(image_url)),
      #("scale", json.int(scale)),
    ],
  )
}

/// Create a Clarity Upscaler request (philz1337x/clarity-upscaler)
pub fn clarity_upscale_request(
  image_url: String,
  scale: Int,
  creativity: Float,
) -> PredictionRequest {
  PredictionRequest(
    model: "philz1337x/clarity-upscaler",
    version: None,
    input: [
      #("image", json.string(image_url)),
      #("scale", json.int(scale)),
      #("creativity", json.float(creativity)),
      #("resemblance", json.float(0.6)),
      #("hdr", json.int(0)),
    ],
  )
}

/// Create a simple Clarity Upscaler request with defaults
pub fn simple_clarity_upscale(image_url: String) -> PredictionRequest {
  clarity_upscale_request(image_url, 2, 0.35)
}

// ============================================================
// LoRA Training Types & Requests
// ============================================================

pub type LoraTrainingRequest {
  LoraTrainingRequest(
    input_images: String,
    trigger_word: String,
    steps: Option(Int),
    lora_rank: Option(Int),
    optimizer: Option(String),
    batch_size: Option(Int),
    resolution: Option(String),
    autocaption: Option(Bool),
    caption_prefix: Option(String),
  )
}

/// Create a training request for FLUX LoRA
pub fn create_training_request(
  config: Config,
  req: LoraTrainingRequest,
) -> Request {
  let base_input = [
    #("input_images", json.string(req.input_images)),
    #("trigger_word", json.string(req.trigger_word)),
  ]

  let with_steps = case req.steps {
    Some(s) -> [#("steps", json.int(s)), ..base_input]
    None -> base_input
  }

  let with_rank = case req.lora_rank {
    Some(r) -> [#("lora_rank", json.int(r)), ..with_steps]
    None -> with_steps
  }

  let with_optimizer = case req.optimizer {
    Some(o) -> [#("optimizer", json.string(o)), ..with_rank]
    None -> with_rank
  }

  let with_batch = case req.batch_size {
    Some(b) -> [#("batch_size", json.int(b)), ..with_optimizer]
    None -> with_optimizer
  }

  let with_resolution = case req.resolution {
    Some(r) -> [#("resolution", json.string(r)), ..with_batch]
    None -> with_batch
  }

  let with_autocaption = case req.autocaption {
    Some(a) -> [#("autocaption", json.bool(a)), ..with_resolution]
    None -> with_resolution
  }

  let final_input = case req.caption_prefix {
    Some(p) -> [#("caption_prefix", json.string(p)), ..with_autocaption]
    None -> with_autocaption
  }

  let body = json.to_string(json.object([
    #("destination", json.string("your-username/your-model")),
    #("input", json.object(final_input)),
    #("model", json.string("ostris/flux-dev-lora-trainer")),
  ]))

  Request(
    url: "https://api.replicate.com/v1/trainings",
    method: "POST",
    headers: [
      #("Authorization", "Bearer " <> config.api_token),
      #("Content-Type", "application/json"),
      #("Prefer", "wait"),
    ],
    body: body,
  )
}

/// Get training status
pub fn get_training_request(config: Config, training_id: String) -> Request {
  Request(
    url: "https://api.replicate.com/v1/trainings/" <> training_id,
    method: "GET",
    headers: [
      #("Authorization", "Bearer " <> config.api_token),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Cancel a training
pub fn cancel_training_request(config: Config, training_id: String) -> Request {
  Request(
    url: "https://api.replicate.com/v1/trainings/" <> training_id <> "/cancel",
    method: "POST",
    headers: [
      #("Authorization", "Bearer " <> config.api_token),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Create a simple LoRA training request
pub fn simple_lora_training(
  images_url: String,
  trigger_word: String,
) -> LoraTrainingRequest {
  LoraTrainingRequest(
    input_images: images_url,
    trigger_word: trigger_word,
    steps: Some(1000),
    lora_rank: Some(16),
    optimizer: Some("adamw8bit"),
    batch_size: Some(1),
    resolution: Some("512,768,1024"),
    autocaption: Some(True),
    caption_prefix: None,
  )
}

/// Create a full LoRA training request with all options
pub fn full_lora_training(
  images_url: String,
  trigger_word: String,
  steps: Int,
  lora_rank: Int,
  optimizer: String,
  batch_size: Int,
  resolution: String,
  autocaption: Bool,
  caption_prefix: String,
) -> LoraTrainingRequest {
  LoraTrainingRequest(
    input_images: images_url,
    trigger_word: trigger_word,
    steps: Some(steps),
    lora_rank: Some(lora_rank),
    optimizer: Some(optimizer),
    batch_size: Some(batch_size),
    resolution: Some(resolution),
    autocaption: Some(autocaption),
    caption_prefix: Some(caption_prefix),
  )
}

/// Create default config with API token
pub fn default_config(api_token: String) -> Config {
  Config(api_token: api_token)
}

// ============================================================
// LoRA Training Information
// ============================================================

pub fn supported_optimizers() -> List(#(String, String)) {
  [
    #("adamw8bit", "AdamW 8-bit - Memory efficient"),
    #("adam8bit", "Adam 8-bit - Alternative optimizer"),
    #("lion8bit", "Lion 8-bit - Fast convergence"),
    #("prodigy", "Prodigy - Adaptive learning rate"),
  ]
}

pub fn supported_resolutions() -> List(String) {
  ["512", "768", "1024", "512,768", "512,768,1024"]
}

pub fn training_models() -> List(#(String, String)) {
  [
    #("ostris/flux-dev-lora-trainer", "FLUX Dev LoRA Trainer"),
    #("ostris/flux-schnell-lora-trainer", "FLUX Schnell LoRA Trainer"),
  ]
}
