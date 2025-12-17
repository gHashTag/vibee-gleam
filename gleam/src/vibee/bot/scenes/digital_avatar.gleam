// Digital Avatar Scene - LoRA Training and Generation
// Integrates with Replicate for FLUX LoRA training
// and FAL.ai for image generation with trained model

import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import pog
import vibee/ai/fal
import vibee/ai/replicate
import vibee/bot/scene.{
  type OutgoingMessage, type Scene, type UserSession, Avatar,
  AvatarEnterPrompt, AvatarEnterTriggerWord, AvatarGenerating, AvatarResult,
  AvatarStart, AvatarTrainingComplete, AvatarTrainingStarted,
  AvatarUploadPhotos, TextReply, TextWithKeyboard, UserSession, button,
}
import vibee/bot/session_store

// ============================================================
// Types
// ============================================================

pub type AvatarConfig {
  AvatarConfig(
    replicate_api_token: String,
    fal_api_key: String,
    min_photos: Int,
    max_photos: Int,
    default_steps: Int,
  )
}

pub type TrainingResult {
  TrainingStarted(training_id: String)
  TrainingInProgress(training_id: String, progress: Int)
  TrainingCompleted(training_id: String, lora_url: String)
  TrainingFailed(error: String)
}

pub type PhotoCollection {
  PhotoCollection(
    user_id: Int,
    session_key: String,
    photos: List(PhotoInfo),
  )
}

pub type PhotoInfo {
  PhotoInfo(
    file_id: String,
    file_path: Option(String),
    file_url: Option(String),
  )
}

// ============================================================
// Default Config
// ============================================================

pub fn default_config(
  replicate_token: String,
  fal_key: String,
) -> AvatarConfig {
  AvatarConfig(
    replicate_api_token: replicate_token,
    fal_api_key: fal_key,
    min_photos: 5,
    max_photos: 20,
    default_steps: 1000,
  )
}

// ============================================================
// Training Functions
// ============================================================

/// Start LoRA training on Replicate
pub fn start_training(
  config: AvatarConfig,
  images_zip_url: String,
  trigger_word: String,
  steps: Option(Int),
) -> Result(TrainingResult, String) {
  let replicate_config = replicate.Config(api_token: config.replicate_api_token)

  let actual_steps = option.unwrap(steps, config.default_steps)

  let req = replicate.LoraTrainingRequest(
    input_images: images_zip_url,
    trigger_word: trigger_word,
    steps: Some(actual_steps),
    lora_rank: Some(16),
    optimizer: Some("adamw8bit"),
    batch_size: Some(1),
    resolution: Some("512,768,1024"),
    autocaption: Some(True),
    caption_prefix: None,
  )

  let training_request = replicate.create_training_request(replicate_config, req)

  execute_training_request(training_request)
}

/// Check training status
pub fn check_training_status(
  config: AvatarConfig,
  training_id: String,
) -> Result(TrainingResult, String) {
  let replicate_config = replicate.Config(api_token: config.replicate_api_token)
  let status_request = replicate.get_training_request(replicate_config, training_id)

  execute_status_request(status_request)
}

/// Cancel training
pub fn cancel_training(
  config: AvatarConfig,
  training_id: String,
) -> Result(Nil, String) {
  let replicate_config = replicate.Config(api_token: config.replicate_api_token)
  let cancel_request = replicate.cancel_training_request(replicate_config, training_id)

  case execute_cancel_request(cancel_request) {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(e)
  }
}

// ============================================================
// Generation with Trained LoRA
// ============================================================

/// Generate image using trained LoRA
pub fn generate_with_avatar(
  config: AvatarConfig,
  lora_url: String,
  trigger_word: String,
  prompt: String,
) -> Result(List(String), String) {
  let fal_config = fal.Config(api_key: config.fal_api_key)

  // Ensure trigger word is in prompt
  let full_prompt = case string.contains(prompt, trigger_word) {
    True -> prompt
    False -> trigger_word <> " " <> prompt
  }

  let req = fal.NeuroPhotoRequest(
    prompt: full_prompt,
    lora_url: lora_url,
    num_images: 1,
    image_size: fal.ImageSize(width: 1024, height: 1024),
    seed: None,
    guidance_scale: Some(3.5),
    num_inference_steps: Some(28),
    enable_safety_checker: False,
  )

  let fal_request = fal.neuro_photo_request(fal_config, req)

  execute_generation_request(fal_request)
}

// ============================================================
// Photo Management
// ============================================================

/// Save photo to database
pub fn save_photo(
  pool: pog.Connection,
  user_id: Int,
  session_key: String,
  file_id: String,
  position: Int,
) -> Result(Nil, String) {
  let query =
    "INSERT INTO user_photos (user_id, session_key, file_id, position)
     VALUES ($1, $2, $3, $4)"

  let result =
    pog.query(query)
    |> pog.parameter(pog.int(user_id))
    |> pog.parameter(pog.text(session_key))
    |> pog.parameter(pog.text(file_id))
    |> pog.parameter(pog.int(position))
    |> pog.execute(pool)

  case result {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error("Failed to save photo")
  }
}

/// Get collected photos count
pub fn get_photos_count(
  pool: pog.Connection,
  user_id: Int,
  session_key: String,
) -> Result(Int, String) {
  let query =
    "SELECT COUNT(*) FROM user_photos
     WHERE user_id = $1 AND session_key = $2"

  let result =
    pog.query(query)
    |> pog.parameter(pog.int(user_id))
    |> pog.parameter(pog.text(session_key))
    |> pog.execute(pool)

  case result {
    Ok(_) -> Ok(0)
    // TODO: proper count parsing
    Error(_) -> Error("Failed to count photos")
  }
}

/// Clear photos for session
pub fn clear_photos(
  pool: pog.Connection,
  user_id: Int,
  session_key: String,
) -> Result(Nil, String) {
  let query =
    "DELETE FROM user_photos
     WHERE user_id = $1 AND session_key = $2"

  let result =
    pog.query(query)
    |> pog.parameter(pog.int(user_id))
    |> pog.parameter(pog.text(session_key))
    |> pog.execute(pool)

  case result {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Failed to clear photos")
  }
}

/// Save trained LoRA model
pub fn save_lora_model(
  pool: pog.Connection,
  user_id: Int,
  name: String,
  trigger_word: String,
  lora_url: String,
  training_id: String,
  photos_count: Int,
  steps: Int,
) -> Result(Nil, String) {
  let query =
    "INSERT INTO user_lora_models
     (user_id, name, trigger_word, lora_url, training_id, photos_count, steps)
     VALUES ($1, $2, $3, $4, $5, $6, $7)"

  let result =
    pog.query(query)
    |> pog.parameter(pog.int(user_id))
    |> pog.parameter(pog.text(name))
    |> pog.parameter(pog.text(trigger_word))
    |> pog.parameter(pog.text(lora_url))
    |> pog.parameter(pog.text(training_id))
    |> pog.parameter(pog.int(photos_count))
    |> pog.parameter(pog.int(steps))
    |> pog.execute(pool)

  case result {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Failed to save LoRA model")
  }
}

/// Get user's LoRA models
pub fn get_user_models(
  pool: pog.Connection,
  user_id: Int,
) -> Result(List(#(String, String, String)), String) {
  // Returns list of (name, trigger_word, lora_url)
  let query =
    "SELECT name, trigger_word, lora_url FROM user_lora_models
     WHERE user_id = $1
     ORDER BY created_at DESC"

  // TODO: proper result parsing
  Ok([])
}

// ============================================================
// HTTP Helpers
// ============================================================

fn execute_training_request(
  req: replicate.Request,
) -> Result(TrainingResult, String) {
  case send_request(req) {
    Ok(response) -> parse_training_response(response)
    Error(e) -> Error(e)
  }
}

fn execute_status_request(
  req: replicate.Request,
) -> Result(TrainingResult, String) {
  case send_request(req) {
    Ok(response) -> parse_status_response(response)
    Error(e) -> Error(e)
  }
}

fn execute_cancel_request(req: replicate.Request) -> Result(String, String) {
  send_request(req)
}

fn execute_generation_request(req: fal.Request) -> Result(List(String), String) {
  case send_fal_request(req) {
    Ok(response) -> parse_generation_response(response)
    Error(e) -> Error(e)
  }
}

fn send_request(req: replicate.Request) -> Result(String, String) {
  let http_req =
    request.new()
    |> request.set_method(case req.method {
      "POST" -> http.Post
      "GET" -> http.Get
      _ -> http.Post
    })
    |> request.set_host("api.replicate.com")
    |> request.set_path(extract_path(req.url))
    |> request.set_body(req.body)
    |> add_headers(req.headers)

  case httpc.send(http_req) {
    Ok(response) ->
      case response.status >= 200 && response.status < 300 {
        True -> Ok(response.body)
        False ->
          Error(
            "Replicate error: HTTP " <> int.to_string(response.status),
          )
      }
    Error(_) -> Error("Failed to connect to Replicate")
  }
}

fn send_fal_request(req: fal.Request) -> Result(String, String) {
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

  case httpc.send(http_req) {
    Ok(response) ->
      case response.status >= 200 && response.status < 300 {
        True -> Ok(response.body)
        False -> Error("FAL.ai error: HTTP " <> int.to_string(response.status))
      }
    Error(_) -> Error("Failed to connect to FAL.ai")
  }
}

fn parse_training_response(body: String) -> Result(TrainingResult, String) {
  // Parse training ID from response
  case extract_field(body, "id") {
    "" -> Error("No training ID in response")
    id -> Ok(TrainingStarted(training_id: id))
  }
}

fn parse_status_response(body: String) -> Result(TrainingResult, String) {
  let status = extract_field(body, "status")
  let id = extract_field(body, "id")

  case status {
    "starting" | "processing" -> {
      let progress = extract_progress(body)
      Ok(TrainingInProgress(training_id: id, progress: progress))
    }
    "succeeded" -> {
      let output = extract_field(body, "output")
      // Output contains the LoRA URL
      Ok(TrainingCompleted(training_id: id, lora_url: output))
    }
    "failed" -> {
      let error = extract_field(body, "error")
      Ok(TrainingFailed(error: error))
    }
    "canceled" -> Ok(TrainingFailed(error: "Training was canceled"))
    _ -> Error("Unknown training status: " <> status)
  }
}

fn parse_generation_response(body: String) -> Result(List(String), String) {
  // Extract image URLs from response
  let urls = extract_image_urls(body)
  case urls {
    [] -> Error("No images in response")
    _ -> Ok(urls)
  }
}

fn extract_field(json_str: String, field: String) -> String {
  case string.split(json_str, "\"" <> field <> "\":") {
    [_, rest, ..] -> {
      let trimmed = string.trim_start(rest)
      case string.starts_with(trimmed, "\"") {
        True -> {
          let without_quote = string.drop_start(trimmed, 1)
          case string.split(without_quote, "\"") {
            [value, ..] -> value
            _ -> ""
          }
        }
        False -> {
          // Might be a number or null
          case string.split(trimmed, ",") {
            [value, ..] -> string.trim(value)
            _ -> ""
          }
        }
      }
    }
    _ -> ""
  }
}

fn extract_progress(body: String) -> Int {
  // Try to extract progress percentage
  0
}

fn extract_image_urls(json_str: String) -> List(String) {
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

pub fn start_response() -> OutgoingMessage {
  TextWithKeyboard(
    "Digital Avatar Training\n\n"
    <> "Create your personal AI model!\n\n"
    <> "How it works:\n"
    <> "1. Upload 10-20 photos of yourself\n"
    <> "2. Choose a trigger word\n"
    <> "3. Wait for training (~15 min)\n"
    <> "4. Generate images with your avatar!\n\n"
    <> "Requirements:\n"
    <> "- Clear, well-lit photos\n"
    <> "- Different angles and expressions\n"
    <> "- No group photos",
    [[button("Start Training", "avatar_start")], [button("Menu", "back_menu")]],
  )
}

pub fn upload_photos_response(collected: Int, required: Int) -> OutgoingMessage {
  TextWithKeyboard(
    "Upload your photos\n\n"
    <> "Uploaded: "
    <> int.to_string(collected)
    <> "/"
    <> int.to_string(required)
    <> "\n\n"
    <> "Send photos one by one or as an album.",
    case collected >= 5 {
      True -> [
        [button("Done (" <> int.to_string(collected) <> " photos)", "done_photos")],
        [button("Cancel", "cancel")],
      ]
      False -> [[button("Cancel", "cancel")]]
    },
  )
}

pub fn enter_trigger_response() -> OutgoingMessage {
  TextReply(
    "All photos received!\n\n"
    <> "Now enter a trigger word for your avatar.\n"
    <> "This word will activate your style in prompts.\n\n"
    <> "Examples:\n"
    <> "- MYAVATAR\n"
    <> "- JOHNSTYLE\n"
    <> "- SARAFACE\n\n"
    <> "Use uppercase letters, 3+ characters:",
  )
}

pub fn training_started_response(trigger_word: String) -> OutgoingMessage {
  TextReply(
    "Training started!\n\n"
    <> "Trigger word: "
    <> trigger_word
    <> "\n\n"
    <> "This will take about 10-20 minutes.\n"
    <> "I'll notify you when it's ready!",
  )
}

pub fn training_progress_response(progress: Int) -> OutgoingMessage {
  TextReply("Training in progress: " <> int.to_string(progress) <> "%")
}

pub fn training_complete_response(trigger_word: String) -> OutgoingMessage {
  TextWithKeyboard(
    "Training complete!\n\n"
    <> "Your avatar model is ready.\n"
    <> "Trigger word: "
    <> trigger_word
    <> "\n\n"
    <> "You can now generate images with your avatar!",
    [
      [button("Generate Image", "avatar_generate")],
      [button("Menu", "back_menu")],
    ],
  )
}

pub fn enter_prompt_response(trigger_word: String) -> OutgoingMessage {
  TextReply(
    "Enter a prompt for your avatar.\n\n"
    <> "Use your trigger word: "
    <> trigger_word
    <> "\n\n"
    <> "Examples:\n"
    <> "- portrait of "
    <> trigger_word
    <> " as a superhero\n"
    <> "- "
    <> trigger_word
    <> " in a fantasy forest\n"
    <> "- professional headshot of "
    <> trigger_word,
  )
}

pub fn generating_response(prompt: String) -> OutgoingMessage {
  TextReply("Generating your avatar image...\n\nPrompt: " <> prompt)
}

pub fn result_response(images: List(String)) -> OutgoingMessage {
  case images {
    [] ->
      TextWithKeyboard(
        "Generation failed. Please try again.",
        [[button("Try Again", "avatar_generate")], [button("Menu", "back_menu")]],
      )
    _ ->
      TextWithKeyboard(
        "Here's your avatar image!",
        [
          [button("Generate More", "avatar_generate")],
          [button("Menu", "back_menu")],
        ],
      )
  }
}

pub fn error_response(error: String) -> OutgoingMessage {
  TextWithKeyboard(
    "Error: " <> error,
    [[button("Try Again", "avatar_start")], [button("Menu", "back_menu")]],
  )
}
