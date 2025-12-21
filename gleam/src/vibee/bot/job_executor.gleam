// Job Executor - Async execution of AI generation jobs
// Handles FAL.ai, Kling, and other AI service calls

import gleam/dict.{type Dict}
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/ai/fal
import vibee/ai/replicate
import vibee/bot/scene_handler.{
  type JobType, JobAvatarVideo, JobBRoll, JobImageToVideo, JobLoraTraining,
  JobMorphing, JobNeuroPhoto, JobTextToVideo, JobVoiceClone,
}
import vibee/mcp/config

// ============================================================
// Types
// ============================================================

pub type ExecutorConfig {
  ExecutorConfig(
    fal_api_key: String,
    replicate_api_token: String,
    bridge_url: String,
    bridge_api_key: String,
    session_id: String,
  )
}

pub type ExecutionResult {
  ExecutionSuccess(url: String)
  ExecutionPending(job_id: String)
  ExecutionFailed(error: String)
}

// ============================================================
// Main Executor
// ============================================================

/// Execute a job and send result to chat
pub fn execute(
  job_type: JobType,
  params: Dict(String, String),
  chat_id: String,
) -> Result(Nil, String) {
  // Get config from environment
  let executor_config = get_config()

  case executor_config {
    Error(e) -> Error(e)
    Ok(cfg) -> {
      // Execute job based on type
      let result = case job_type {
        JobNeuroPhoto -> execute_neuro_photo(cfg, params)
        JobTextToVideo -> execute_text_to_video(cfg, params)
        JobImageToVideo -> execute_image_to_video(cfg, params)
        JobMorphing -> execute_morphing(cfg, params)
        JobBRoll -> execute_broll(cfg, params)
        JobAvatarVideo -> execute_avatar_video(cfg, params)
        JobVoiceClone -> execute_voice_clone(cfg, params)
        JobLoraTraining -> execute_lora_training(cfg, params)
      }

      // Send result to chat
      case result {
        Ok(ExecutionSuccess(url)) -> {
          // Determine if it's a photo or video
          case job_type {
            JobNeuroPhoto -> send_photo(cfg, chat_id, url, Some("Your image is ready!"))
            _ -> send_video(cfg, chat_id, url, Some("Your video is ready!"))
          }
        }
        Ok(ExecutionPending(job_id)) -> {
          send_text(cfg, chat_id, "Generation started. Job ID: " <> job_id <> "\n\nI'll notify you when it's ready.")
        }
        Ok(ExecutionFailed(error)) -> {
          send_text(cfg, chat_id, "Generation failed: " <> error)
        }
        Error(e) -> {
          send_text(cfg, chat_id, "Error: " <> e)
        }
      }
    }
  }
}

/// Get executor config from environment
fn get_config() -> Result(ExecutorConfig, String) {
  let fal_key = config.get_env("FAL_API_KEY")
  let replicate_token = config.get_env("REPLICATE_API_TOKEN")
  let bridge_url = config.get_env_or("VIBEE_BRIDGE_URL", "https://vibee-telegram-bridge.fly.dev")
  let bridge_api_key = config.get_env_or("VIBEE_API_KEY", "")
  let session_id = config.get_env_or("TELEGRAM_SESSION_ID", "")

  case fal_key, replicate_token {
    "", _ -> Error("FAL_API_KEY not configured")
    _, "" -> Error("REPLICATE_API_TOKEN not configured")
    fal, rep ->
      Ok(ExecutorConfig(
        fal_api_key: fal,
        replicate_api_token: rep,
        bridge_url: bridge_url,
        bridge_api_key: bridge_api_key,
        session_id: session_id,
      ))
  }
}

// ============================================================
// NeuroPhoto Execution (FAL.ai)
// ============================================================

fn execute_neuro_photo(
  cfg: ExecutorConfig,
  params: Dict(String, String),
) -> Result(ExecutionResult, String) {
  let prompt = dict.get(params, "prompt") |> result.unwrap("")
  let model = dict.get(params, "model") |> result.unwrap("nano-banana")

  case prompt {
    "" -> Error("Prompt is required")
    _ -> {
      case model {
        "nano-banana" -> execute_nano_banana(cfg, prompt)
        "flux-lora" -> execute_flux_lora(cfg, prompt)
        "flux-kontext" -> execute_flux_kontext(cfg, prompt, params)
        _ -> execute_nano_banana(cfg, prompt)
      }
    }
  }
}

/// Execute Nano Banana Pro generation
fn execute_nano_banana(
  cfg: ExecutorConfig,
  prompt: String,
) -> Result(ExecutionResult, String) {
  let fal_config = fal.Config(api_key: cfg.fal_api_key)

  let req = fal.NanoBananaRequest(
    prompt: prompt,
    num_images: 1,
    aspect_ratio: "9:16",
    resolution: "1K",
    seed: None,
  )

  let fal_request = fal.nano_banana_request(fal_config, req)

  execute_fal_request(fal_request)
}

/// Execute FLUX LoRA generation
fn execute_flux_lora(
  cfg: ExecutorConfig,
  prompt: String,
) -> Result(ExecutionResult, String) {
  let fal_config = fal.Config(api_key: cfg.fal_api_key)

  let lora_url = "https://huggingface.co/alvdansen/flux-koda/resolve/main/flux_koda.safetensors"

  let req = fal.NeuroPhotoRequest(
    prompt: prompt,
    lora_url: lora_url,
    num_images: 1,
    image_size: fal.ImageSize(width: 1024, height: 1024),
    seed: None,
    guidance_scale: Some(3.5),
    num_inference_steps: Some(28),
    enable_safety_checker: False,
  )

  let fal_request = fal.neuro_photo_request(fal_config, req)

  execute_fal_request(fal_request)
}

/// Execute FLUX Kontext (image-to-image)
fn execute_flux_kontext(
  cfg: ExecutorConfig,
  prompt: String,
  params: Dict(String, String),
) -> Result(ExecutionResult, String) {
  let image_url = dict.get(params, "image_url") |> result.unwrap("")

  case image_url {
    "" -> Error("Image URL is required for Kontext")
    url -> {
      let fal_config = fal.Config(api_key: cfg.fal_api_key)

      let req = fal.FluxKontextRequest(
        prompt: prompt,
        input_image_url: url,
        model_type: "pro",
        mode: fal.Single,
        aspect_ratio: None,
        seed: None,
      )

      let fal_request = fal.flux_kontext_request(fal_config, req)

      execute_fal_request(fal_request)
    }
  }
}

/// Execute FAL.ai HTTP request
fn execute_fal_request(req: fal.Request) -> Result(ExecutionResult, String) {
  let http_req =
    request.new()
    |> request.set_method(case req.method {
      "POST" -> http.Post
      "GET" -> http.Get
      _ -> http.Post
    })
    |> request.set_scheme(http.Https)
    |> request.set_host(extract_host(req.url))
    |> request.set_path(extract_path(req.url))
    |> request.set_body(req.body)
    |> add_headers(req.headers)

  case httpc.send(http_req) {
    Ok(response) -> {
      case response.status {
        200 -> parse_fal_response(response.body)
        202 -> parse_fal_pending(response.body)
        status -> Error("FAL.ai error: HTTP " <> int.to_string(status) <> " - " <> response.body)
      }
    }
    Error(_) -> Error("Failed to connect to FAL.ai")
  }
}

fn parse_fal_response(body: String) -> Result(ExecutionResult, String) {
  // Parse JSON response - look for images array
  case string.contains(body, "\"images\"") {
    True -> {
      case extract_image_url(body) {
        "" -> Error("No image URL in response")
        url -> Ok(ExecutionSuccess(url: url))
      }
    }
    False -> Error("Invalid FAL.ai response: " <> string.slice(body, 0, 200))
  }
}

fn parse_fal_pending(body: String) -> Result(ExecutionResult, String) {
  // Parse request_id from 202 response
  case string.contains(body, "\"request_id\"") {
    True -> {
      let request_id = extract_field(body, "request_id")
      Ok(ExecutionPending(job_id: request_id))
    }
    False -> Error("No request_id in pending response")
  }
}

fn extract_image_url(json_str: String) -> String {
  // Find "url":"..." in images array
  case string.split(json_str, "\"url\":\"") {
    [_, rest, ..] -> {
      case string.split(rest, "\"") {
        [url, ..] -> url
        _ -> ""
      }
    }
    _ -> ""
  }
}

fn extract_field(json_str: String, field: String) -> String {
  case string.split(json_str, "\"" <> field <> "\":\"") {
    [_, rest, ..] -> {
      case string.split(rest, "\"") {
        [value, ..] -> value
        _ -> ""
      }
    }
    _ -> ""
  }
}

fn extract_host(url: String) -> String {
  case string.split(url, "//") {
    [_, rest, ..] -> {
      case string.split(rest, "/") {
        [host, ..] -> host
        _ -> ""
      }
    }
    _ -> ""
  }
}

fn extract_path(url: String) -> String {
  case string.split(url, "//") {
    [_, rest, ..] -> {
      case string.split(rest, "/") {
        [_host, ..path_parts] -> "/" <> string.join(path_parts, "/")
        _ -> "/"
      }
    }
    _ -> "/"
  }
}

fn add_headers(
  req: request.Request(String),
  headers: List(#(String, String)),
) -> request.Request(String) {
  case headers {
    [] -> req
    [#(name, value), ..rest] ->
      add_headers(request.set_header(req, name, value), rest)
  }
}

// ============================================================
// Replicate Video Generation
// ============================================================

/// Text-to-video using Replicate's minimax/video-01
fn execute_text_to_video(
  cfg: ExecutorConfig,
  params: Dict(String, String),
) -> Result(ExecutionResult, String) {
  let prompt = dict.get(params, "prompt") |> result.unwrap("")

  case prompt {
    "" -> Error("Prompt is required for text-to-video")
    _ -> {
      let replicate_cfg = replicate.Config(api_token: cfg.replicate_api_token)

      let req = replicate.PredictionRequest(
        model: "minimax/video-01",
        version: None,
        input: [
          #("prompt", json.string(prompt)),
          #("prompt_optimizer", json.bool(True)),
        ],
      )

      let replicate_request = replicate.create_prediction_request(replicate_cfg, req)
      execute_replicate_with_polling(cfg, replicate_request)
    }
  }
}

/// Image-to-video using Replicate's minimax/video-01
fn execute_image_to_video(
  cfg: ExecutorConfig,
  params: Dict(String, String),
) -> Result(ExecutionResult, String) {
  let image_url = dict.get(params, "image_url") |> result.unwrap("")
  let prompt = dict.get(params, "prompt") |> result.unwrap("")

  case image_url {
    "" -> Error("Image URL is required for image-to-video")
    _ -> {
      let replicate_cfg = replicate.Config(api_token: cfg.replicate_api_token)

      let input = case prompt {
        "" -> [
          #("first_frame_image", json.string(image_url)),
        ]
        p -> [
          #("first_frame_image", json.string(image_url)),
          #("prompt", json.string(p)),
          #("prompt_optimizer", json.bool(True)),
        ]
      }

      let req = replicate.PredictionRequest(
        model: "minimax/video-01",
        version: None,
        input: input,
      )

      let replicate_request = replicate.create_prediction_request(replicate_cfg, req)
      execute_replicate_with_polling(cfg, replicate_request)
    }
  }
}

/// Morphing between two faces using Replicate
fn execute_morphing(
  cfg: ExecutorConfig,
  params: Dict(String, String),
) -> Result(ExecutionResult, String) {
  let source_url = dict.get(params, "source_url") |> result.unwrap("")
  let target_url = dict.get(params, "target_url") |> result.unwrap("")

  case source_url, target_url {
    "", _ -> Error("Source image URL is required for morphing")
    _, "" -> Error("Target image URL is required for morphing")
    source, target -> {
      let replicate_cfg = replicate.Config(api_token: cfg.replicate_api_token)

      // Using fofr/face-to-many for face morphing
      let req = replicate.PredictionRequest(
        model: "fofr/face-to-many",
        version: None,
        input: [
          #("image", json.string(source)),
          #("style", json.string("Video game")),
          #("style_name", json.string(target)),
          #("negative_prompt", json.string("")),
          #("prompt_strength", json.float(4.5)),
          #("denoising_strength", json.float(0.65)),
          #("instant_id_strength", json.float(1.0)),
        ],
      )

      let replicate_request = replicate.create_prediction_request(replicate_cfg, req)
      execute_replicate_with_polling(cfg, replicate_request)
    }
  }
}

/// Avatar video using Replicate's SadTalker
fn execute_avatar_video(
  cfg: ExecutorConfig,
  params: Dict(String, String),
) -> Result(ExecutionResult, String) {
  let image_url = dict.get(params, "image_url") |> result.unwrap("")
  let audio_url = dict.get(params, "audio_url") |> result.unwrap("")

  case image_url, audio_url {
    "", _ -> Error("Portrait image URL is required for avatar video")
    _, "" -> Error("Audio URL is required for avatar video")
    img, audio -> {
      let replicate_cfg = replicate.Config(api_token: cfg.replicate_api_token)

      // Using lucataco/sadtalker for talking head generation
      let req = replicate.PredictionRequest(
        model: "lucataco/sadtalker",
        version: None,
        input: [
          #("source_image", json.string(img)),
          #("driven_audio", json.string(audio)),
          #("preprocess", json.string("crop")),
          #("still_mode", json.bool(False)),
        ],
      )

      let replicate_request = replicate.create_prediction_request(replicate_cfg, req)
      execute_replicate_with_polling(cfg, replicate_request)
    }
  }
}

fn execute_broll(
  _cfg: ExecutorConfig,
  _params: Dict(String, String),
) -> Result(ExecutionResult, String) {
  // TODO: Implement B-Roll
  Error("B-Roll not yet implemented in job executor")
}

fn execute_voice_clone(
  _cfg: ExecutorConfig,
  _params: Dict(String, String),
) -> Result(ExecutionResult, String) {
  // TODO: Implement voice clone
  Error("Voice clone not yet implemented in job executor")
}

fn execute_lora_training(
  _cfg: ExecutorConfig,
  _params: Dict(String, String),
) -> Result(ExecutionResult, String) {
  // TODO: Implement LoRA training
  Error("LoRA training not yet implemented in job executor")
}

// ============================================================
// Replicate HTTP Execution
// ============================================================

/// Execute Replicate request with polling for completion
fn execute_replicate_with_polling(
  cfg: ExecutorConfig,
  req: replicate.Request,
) -> Result(ExecutionResult, String) {
  let http_req =
    request.new()
    |> request.set_method(case req.method {
      "POST" -> http.Post
      "GET" -> http.Get
      _ -> http.Post
    })
    |> request.set_scheme(http.Https)
    |> request.set_host("api.replicate.com")
    |> request.set_path(extract_replicate_path(req.url))
    |> request.set_body(req.body)
    |> add_headers(req.headers)

  case httpc.send(http_req) {
    Ok(response) -> {
      case response.status {
        200 | 201 -> {
          // Check if output is ready (synchronous with Prefer: wait)
          parse_replicate_response(response.body)
        }
        202 -> {
          // Async - need to poll
          case extract_prediction_id(response.body) {
            "" -> Error("No prediction ID in response")
            prediction_id -> poll_replicate(cfg, prediction_id, 0)
          }
        }
        status -> Error("Replicate error: HTTP " <> int.to_string(status) <> " - " <> response.body)
      }
    }
    Error(_) -> Error("Failed to connect to Replicate API")
  }
}

/// Poll Replicate prediction until complete (max 60 attempts, 5 sec each = 5 min)
fn poll_replicate(
  cfg: ExecutorConfig,
  prediction_id: String,
  attempt: Int,
) -> Result(ExecutionResult, String) {
  case attempt >= 60 {
    True -> Error("Replicate video generation timed out after 5 minutes")
    False -> {
      // Wait 5 seconds between polls
      sleep(5000)

      let replicate_cfg = replicate.Config(api_token: cfg.replicate_api_token)
      let status_req = replicate.get_prediction_request(replicate_cfg, prediction_id)

      let http_req =
        request.new()
        |> request.set_method(http.Get)
        |> request.set_scheme(http.Https)
        |> request.set_host("api.replicate.com")
        |> request.set_path("/v1/predictions/" <> prediction_id)
        |> add_headers(status_req.headers)

      case httpc.send(http_req) {
        Ok(response) -> {
          case response.status {
            200 -> {
              case extract_replicate_status(response.body) {
                "succeeded" -> parse_replicate_response(response.body)
                "failed" -> Error("Replicate generation failed: " <> extract_replicate_error(response.body))
                "canceled" -> Error("Replicate generation was canceled")
                _ -> poll_replicate(cfg, prediction_id, attempt + 1)
              }
            }
            status -> Error("Replicate poll error: HTTP " <> int.to_string(status))
          }
        }
        Error(_) -> poll_replicate(cfg, prediction_id, attempt + 1)
      }
    }
  }
}

fn parse_replicate_response(body: String) -> Result(ExecutionResult, String) {
  // Look for output in the response
  let output = extract_replicate_output(body)
  case output {
    "" -> Error("No output URL in Replicate response")
    url -> Ok(ExecutionSuccess(url: url))
  }
}

fn extract_prediction_id(json_str: String) -> String {
  extract_field(json_str, "id")
}

fn extract_replicate_status(json_str: String) -> String {
  extract_field(json_str, "status")
}

fn extract_replicate_error(json_str: String) -> String {
  case extract_field(json_str, "error") {
    "" -> "Unknown error"
    e -> e
  }
}

fn extract_replicate_output(json_str: String) -> String {
  // Output can be a string or array of strings
  // First try "output":"url"
  case string.split(json_str, "\"output\":\"") {
    [_, rest, ..] -> {
      case string.split(rest, "\"") {
        [url, ..] -> url
        _ -> try_extract_array_output(json_str)
      }
    }
    _ -> try_extract_array_output(json_str)
  }
}

fn try_extract_array_output(json_str: String) -> String {
  // Try "output":["url"]
  case string.split(json_str, "\"output\":[\"") {
    [_, rest, ..] -> {
      case string.split(rest, "\"") {
        [url, ..] -> url
        _ -> ""
      }
    }
    _ -> ""
  }
}

fn extract_replicate_path(url: String) -> String {
  case string.split(url, "api.replicate.com") {
    [_, path, ..] -> path
    _ -> "/v1/predictions"
  }
}

@external(erlang, "timer", "sleep")
fn sleep(ms: Int) -> Nil

// ============================================================
// Telegram Message Sending
// ============================================================

fn send_text(
  cfg: ExecutorConfig,
  chat_id: String,
  text: String,
) -> Result(Nil, String) {
  let body = json.to_string(json.object([
    #("session_id", json.string(cfg.session_id)),
    #("chat_id", json.string(chat_id)),
    #("text", json.string(text)),
  ]))

  let req =
    request.new()
    |> request.set_method(http.Post)
    |> request.set_scheme(http.Https)
    |> request.set_host(extract_bridge_host(cfg.bridge_url))
    |> request.set_path("/send")
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("Authorization", "Bearer " <> cfg.bridge_api_key)
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) ->
      case response.status >= 200 && response.status < 300 {
        True -> Ok(Nil)
        False -> Error("Send failed: " <> int.to_string(response.status))
      }
    Error(_) -> Error("Connection to bridge failed")
  }
}

fn send_photo(
  cfg: ExecutorConfig,
  chat_id: String,
  url: String,
  caption: Option(String),
) -> Result(Nil, String) {
  let base_body = [
    #("session_id", json.string(cfg.session_id)),
    #("chat_id", json.string(chat_id)),
    #("photo_url", json.string(url)),
  ]

  let body_with_caption = case caption {
    Some(c) -> [#("caption", json.string(c)), ..base_body]
    None -> base_body
  }

  let body = json.to_string(json.object(body_with_caption))

  let req =
    request.new()
    |> request.set_method(http.Post)
    |> request.set_scheme(http.Https)
    |> request.set_host(extract_bridge_host(cfg.bridge_url))
    |> request.set_path("/send/photo")
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("Authorization", "Bearer " <> cfg.bridge_api_key)
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) ->
      case response.status >= 200 && response.status < 300 {
        True -> Ok(Nil)
        False -> Error("Send photo failed: " <> int.to_string(response.status))
      }
    Error(_) -> Error("Connection to bridge failed")
  }
}

fn send_video(
  cfg: ExecutorConfig,
  chat_id: String,
  url: String,
  caption: Option(String),
) -> Result(Nil, String) {
  let base_body = [
    #("session_id", json.string(cfg.session_id)),
    #("chat_id", json.string(chat_id)),
    #("video_url", json.string(url)),
  ]

  let body_with_caption = case caption {
    Some(c) -> [#("caption", json.string(c)), ..base_body]
    None -> base_body
  }

  let body = json.to_string(json.object(body_with_caption))

  let req =
    request.new()
    |> request.set_method(http.Post)
    |> request.set_scheme(http.Https)
    |> request.set_host(extract_bridge_host(cfg.bridge_url))
    |> request.set_path("/send/video")
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("Authorization", "Bearer " <> cfg.bridge_api_key)
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) ->
      case response.status >= 200 && response.status < 300 {
        True -> Ok(Nil)
        False -> Error("Send video failed: " <> int.to_string(response.status))
      }
    Error(_) -> Error("Connection to bridge failed")
  }
}

fn extract_bridge_host(url: String) -> String {
  // Remove protocol prefix
  let without_protocol = case string.split(url, "://") {
    [_, rest, ..] -> rest
    _ -> url
  }
  // Remove trailing path
  case string.split(without_protocol, "/") {
    [host, ..] -> host
    _ -> without_protocol
  }
}
