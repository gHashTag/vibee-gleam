// NeuroPhoto Scene - AI Image Generation
// Integrates with FAL.ai for FLUX LoRA, Nano Banana Pro

import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import pog
import vibee/ai/fal
import vibee/bot/scene.{
  type OutgoingMessage, type Scene, type UserSession, NeuroPhoto,
  NeuroPhotoEnterPrompt, NeuroPhotoGenerating, NeuroPhotoResult,
  NeuroPhotoSelectModel, PhotosReply, TextReply, TextWithKeyboard, UserSession,
  button,
}
import vibee/bot/session_store

// ============================================================
// Types
// ============================================================

pub type NeuroPhotoConfig {
  NeuroPhotoConfig(
    fal_api_key: String,
    default_lora_url: Option(String),
  )
}

pub type GenerationResult {
  GenerationSuccess(images: List(String), seed: Int)
  GenerationPending(request_id: String)
  GenerationFailed(error: String)
}

// ============================================================
// Model Definitions
// ============================================================

pub type Model {
  FluxLora
  NanoBanana
  FluxKontext
}

pub fn model_from_string(s: String) -> Model {
  case s {
    "flux-lora" -> FluxLora
    "nano-banana" -> NanoBanana
    "flux-kontext" -> FluxKontext
    _ -> FluxLora
  }
}

pub fn model_to_string(m: Model) -> String {
  case m {
    FluxLora -> "flux-lora"
    NanoBanana -> "nano-banana"
    FluxKontext -> "flux-kontext"
  }
}

pub fn model_display_name(m: Model) -> String {
  case m {
    FluxLora -> "FLUX LoRA"
    NanoBanana -> "Nano Banana Pro"
    FluxKontext -> "FLUX Kontext"
  }
}

// ============================================================
// Generation Functions
// ============================================================

/// Generate image with FLUX LoRA
pub fn generate_flux_lora(
  config: NeuroPhotoConfig,
  prompt: String,
  lora_url: Option(String),
) -> Result(GenerationResult, String) {
  let actual_lora = case lora_url {
    Some(url) -> url
    None ->
      case config.default_lora_url {
        Some(url) -> url
        None ->
          "https://huggingface.co/alvdansen/flux-koda/resolve/main/flux_koda.safetensors"
      }
  }

  let fal_config = fal.Config(api_key: config.fal_api_key)

  let req =
    fal.NeuroPhotoRequest(
      prompt: prompt,
      lora_url: actual_lora,
      num_images: 1,
      image_size: fal.ImageSize(width: 1024, height: 1024),
      seed: None,
      guidance_scale: Some(3.5),
      num_inference_steps: Some(28),
      enable_safety_checker: False,
    )

  let fal_request = fal.neuro_photo_request(fal_config, req)

  // Execute HTTP request
  execute_fal_request(fal_request)
}

/// Generate image with Nano Banana Pro
pub fn generate_nano_banana(
  config: NeuroPhotoConfig,
  prompt: String,
  aspect_ratio: String,
) -> Result(GenerationResult, String) {
  let fal_config = fal.Config(api_key: config.fal_api_key)

  let req =
    fal.NanoBananaRequest(
      prompt: prompt,
      num_images: 1,
      aspect_ratio: aspect_ratio,
      resolution: "1K",
      seed: None,
    )

  let fal_request = fal.nano_banana_request(fal_config, req)

  execute_fal_request(fal_request)
}

/// Execute FAL.ai request and parse response
fn execute_fal_request(req: fal.Request) -> Result(GenerationResult, String) {
  // Build HTTP request
  let http_req =
    request.new()
    |> request.set_method(case req.method {
      "POST" -> http.Post
      "GET" -> http.Get
      _ -> http.Post
    })
    |> request.set_host(extract_host(req.url))
    |> request.set_path(extract_path(req.url))
    |> request.set_body(req.body)
    |> add_headers(req.headers)

  // Send request
  case httpc.send(http_req) {
    Ok(response) -> {
      case response.status {
        200 -> parse_fal_response(response.body)
        status -> Error("FAL.ai error: HTTP " <> int.to_string(status))
      }
    }
    Error(_) -> Error("Failed to connect to FAL.ai")
  }
}

fn parse_fal_response(body: String) -> Result(GenerationResult, String) {
  // Parse JSON response
  // Expected format: {"images": [{"url": "..."}], "seed": 12345}
  case string.contains(body, "\"images\"") {
    True -> {
      // Extract image URLs
      let urls = extract_image_urls(body)
      let seed = extract_seed(body)
      Ok(GenerationSuccess(images: urls, seed: seed))
    }
    False ->
      case string.contains(body, "\"request_id\"") {
        True -> {
          let request_id = extract_field(body, "request_id")
          Ok(GenerationPending(request_id: request_id))
        }
        False -> Error("Invalid FAL.ai response: " <> body)
      }
  }
}

fn extract_image_urls(json_str: String) -> List(String) {
  // Simple extraction: find all "url":"..." patterns
  case string.split(json_str, "\"url\":\"") {
    [_, rest, ..] -> {
      case string.split(rest, "\"") {
        [url, ..] -> [url]
        _ -> []
      }
    }
    _ -> []
  }
}

fn extract_seed(json_str: String) -> Int {
  case string.split(json_str, "\"seed\":") {
    [_, rest, ..] -> {
      let digits =
        rest
        |> string.trim
        |> string.to_graphemes
        |> take_while_digit
        |> string.join("")
      case int.parse(digits) {
        Ok(n) -> n
        Error(_) -> 0
      }
    }
    _ -> 0
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

fn take_while_digit(chars: List(String)) -> List(String) {
  case chars {
    [] -> []
    [c, ..rest] ->
      case is_digit(c) {
        True -> [c, ..take_while_digit(rest)]
        False -> []
      }
  }
}

fn is_digit(c: String) -> Bool {
  case c {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
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
// Scene Responses
// ============================================================

pub fn select_model_response() -> OutgoingMessage {
  TextWithKeyboard(
    "NeuroPhoto - AI Image Generation\n\n"
    <> "Choose a model for your image:",
    [
      [button("FLUX LoRA (Best Quality)", "np_model_flux-lora")],
      [button("Nano Banana Pro (Fast)", "np_model_nano-banana")],
      [button("FLUX Kontext (Transform)", "np_model_flux-kontext")],
      [button("Back to Menu", "back_menu")],
    ],
  )
}

pub fn enter_prompt_response(model: String) -> OutgoingMessage {
  let model_name = model_display_name(model_from_string(model))
  TextWithKeyboard(
    "Selected: " <> model_name <> "\n\n"
    <> "Now enter your prompt for the image.\n\n"
    <> "Tips:\n"
    <> "- Be specific and descriptive\n"
    <> "- Include style keywords (cinematic, portrait, etc)\n"
    <> "- Mention lighting and mood",
    [[button("Back", "np_back_select")]],
  )
}

pub fn generating_response(model: String, prompt: String) -> OutgoingMessage {
  TextReply(
    "Generating with " <> model_display_name(model_from_string(model)) <> "...\n\n"
    <> "Prompt: " <> prompt <> "\n\n"
    <> "This may take 10-30 seconds.",
  )
}

pub fn result_response(images: List(String)) -> OutgoingMessage {
  case images {
    [] -> TextReply("Generation failed. Please try again.")
    [url] ->
      TextWithKeyboard(
        "Here's your image!",
        [
          [button("Generate Again", "np_again")],
          [button("Change Model", "np_change_model")],
          [button("Menu", "back_menu")],
        ],
      )
    urls ->
      PhotosReply(
        urls: urls,
        caption: Some("Here are your images!"),
      )
  }
}

pub fn error_response(error: String) -> OutgoingMessage {
  TextWithKeyboard(
    "Error: " <> error <> "\n\nPlease try again.",
    [[button("Try Again", "np_again")], [button("Menu", "back_menu")]],
  )
}

// ============================================================
// Helpers for HTTP
// ============================================================

import gleam/http
