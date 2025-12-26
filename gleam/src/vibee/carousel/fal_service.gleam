// Carousel FAL.ai Image Generation Service
// Uses FLUX-LoRA for high-quality slide backgrounds with custom LoRA

import gleam/dynamic/decode
import gleam/erlang/process
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/ai/fal
import vibee/ai/client as ai_client
import vibee/carousel/types.{type AspectRatio}
import vibee/mcp/config

// ============================================================
// Constants
// ============================================================

/// Default LoRA model URL
const default_lora_url = "https://v3b.fal.media/files/b/0a86e4c0/4TISTwQk8pxuu7ZTXsbHF_pytorch_lora_weights.safetensors"

// ============================================================
// Types
// ============================================================

pub type ImageResult {
  ImageResult(image_url: String, request_id: String)
}

pub type TaskStatus {
  TaskPending
  TaskProcessing
  TaskReady(image_url: String)
  TaskFailed(error: String)
}

// ============================================================
// Image Generation with LoRA
// ============================================================

/// Generate a slide background image using FAL.ai FLUX-LoRA
/// With sync_mode=true, returns image URL immediately or request_id for polling
pub fn generate_with_lora(
  prompt: String,
  art_style: String,
  aspect_ratio: AspectRatio,
  lora_url: Option(String),
) -> Result(ImageResult, String) {
  let api_key = config.get_env_or("FAL_API_KEY", "")
  case api_key {
    "" -> Error("FAL_API_KEY not set")
    key -> {
      let fal_config = fal.default_config(key)

      // Get image size for aspect ratio
      let image_size = aspect_ratio_to_fal_size(aspect_ratio)

      // Build full prompt with style
      let full_prompt = build_full_prompt(prompt, art_style)

      // Use provided LoRA or default
      let lora = case lora_url {
        Some(url) -> url
        None -> default_lora_url
      }

      io.println(
        "[Carousel FAL] Generating image with LoRA: "
        <> string.slice(full_prompt, 0, 50)
        <> "...",
      )
      io.println(
        "[Carousel FAL] Size: "
        <> string.inspect(image_size.width)
        <> "x"
        <> string.inspect(image_size.height),
      )

      let request = fal.NeuroPhotoRequest(
        prompt: full_prompt,
        lora_url: lora,
        num_images: 1,
        image_size: image_size,
        seed: None,
        guidance_scale: Some(3.5),
        num_inference_steps: Some(28),
        enable_safety_checker: True,
      )

      let fal_request = fal.neuro_photo_request(fal_config, request)
      let http_request = convert_request(fal_request)

      case ai_client.execute_json(http_request) {
        Ok(response_body) -> parse_fal_response(response_body)
        Error(e) ->
          Error("FAL request failed: " <> ai_client.error_to_string(e))
      }
    }
  }
}

/// Convert fal.Request to ai_client.Request
fn convert_request(req: fal.Request) -> ai_client.Request {
  ai_client.Request(
    url: req.url,
    method: req.method,
    headers: req.headers,
    body: req.body,
  )
}

/// Get FAL image size from aspect ratio
fn aspect_ratio_to_fal_size(ratio: AspectRatio) -> fal.ImageSize {
  case ratio {
    types.Square -> fal.ImageSize(width: 1024, height: 1024)
    types.Portrait -> fal.ImageSize(width: 864, height: 1080)
    types.Story -> fal.ImageSize(width: 768, height: 1365)
  }
}

/// Build full prompt with art style and safety hints
fn build_full_prompt(prompt: String, art_style: String) -> String {
  let style_hints = case art_style {
    "PHOTOREALISM" ->
      "ultra-realistic photography, 8k, professional lighting, sharp focus, cinematic"
    "CYBERPUNK" ->
      "cyberpunk aesthetic, neon lights, futuristic city, rain, dark atmosphere, holographic elements"
    "ACID_GRAPHICS" ->
      "bold vibrant colors, abstract geometric shapes, 90s acid graphics, maximalist design"
    "3D_ABSTRACT" ->
      "3D rendered abstract forms, smooth gradients, depth of field, modern CGI"
    "DARK_NOIR" ->
      "film noir style, high contrast, dramatic shadows, moody atmosphere, black and white accents"
    "OIL_PAINTING" ->
      "classical oil painting style, textured brush strokes, rich colors, museum quality"
    "MINIMALIST" ->
      "minimalist design, clean composition, negative space, subtle colors, zen aesthetic"
    _ -> "high quality, professional"
  }

  prompt
  <> ". Style: "
  <> style_hints
  <> ". No text, no watermarks. Safe for work, family friendly."
}

/// Parse FAL response - either immediate result (sync) or request_id for polling
fn parse_fal_response(response_body: String) -> Result(ImageResult, String) {
  // First try to parse as immediate result (sync_mode response)
  // FAL returns {"images": [{"url": "..."}]} on success
  let images_decoder = {
    use images <- decode.field("images", decode.list({
      use url <- decode.field("url", decode.string)
      decode.success(url)
    }))
    decode.success(images)
  }

  case json.parse(response_body, images_decoder) {
    Ok(urls) -> {
      case urls {
        [url, ..] -> {
          io.println("[Carousel FAL] Image ready (sync): " <> url)
          Ok(ImageResult(image_url: url, request_id: "sync"))
        }
        [] -> Error("No images in FAL response")
      }
    }
    Error(_) -> {
      // Try to parse as async response with request_id
      let request_id_decoder = {
        use id <- decode.field("request_id", decode.string)
        decode.success(id)
      }
      case json.parse(response_body, request_id_decoder) {
        Ok(request_id) -> {
          io.println("[Carousel FAL] Got request_id (async): " <> request_id)
          // Need to poll for result
          poll_and_return(request_id)
        }
        Error(_) -> Error("Failed to parse FAL response: " <> response_body)
      }
    }
  }
}

/// Poll for result and return as ImageResult
fn poll_and_return(request_id: String) -> Result(ImageResult, String) {
  case poll_until_ready(request_id, 60, 3000) {
    Ok(url) -> Ok(ImageResult(image_url: url, request_id: request_id))
    Error(e) -> Error(e)
  }
}

// ============================================================
// Polling for Result
// ============================================================

/// Poll for image generation result
pub fn poll_result(request_id: String) -> Result(TaskStatus, String) {
  let api_key = config.get_env_or("FAL_API_KEY", "")
  case api_key {
    "" -> Error("FAL_API_KEY not set")
    key -> {
      let fal_config = fal.default_config(key)
      let fal_request = fal.get_queue_result(fal_config, request_id)
      let http_request = convert_request(fal_request)

      case ai_client.execute_json(http_request) {
        Ok(response_body) -> parse_status_from_json(response_body)
        Error(e) -> Error("FAL poll failed: " <> ai_client.error_to_string(e))
      }
    }
  }
}

/// Parse status from FAL result JSON response
fn parse_status_from_json(response_body: String) -> Result(TaskStatus, String) {
  // Try to get images first (completed result)
  let images_decoder = {
    use images <- decode.field("images", decode.list({
      use url <- decode.field("url", decode.string)
      decode.success(url)
    }))
    decode.success(images)
  }

  case json.parse(response_body, images_decoder) {
    Ok(urls) -> {
      case urls {
        [url, ..] -> Ok(TaskReady(url))
        [] -> Ok(TaskProcessing)
      }
    }
    Error(_) -> {
      // Check for status field
      let status_decoder = {
        use status <- decode.field("status", decode.string)
        decode.success(status)
      }
      case json.parse(response_body, status_decoder) {
        Ok(status) -> {
          case string.lowercase(status) {
            "completed" -> {
              // Try to get output.images[0].url
              let output_decoder = {
                use output <- decode.field("output", {
                  use images <- decode.field("images", decode.list({
                    use url <- decode.field("url", decode.string)
                    decode.success(url)
                  }))
                  decode.success(images)
                })
                decode.success(output)
              }
              case json.parse(response_body, output_decoder) {
                Ok([url, ..]) -> Ok(TaskReady(url))
                _ -> Ok(TaskProcessing)
              }
            }
            "in_queue" | "in_progress" | "pending" -> Ok(TaskPending)
            "processing" -> Ok(TaskProcessing)
            "failed" -> Ok(TaskFailed("Generation failed"))
            _ -> Ok(TaskProcessing)
          }
        }
        Error(_) -> Ok(TaskProcessing)
      }
    }
  }
}

/// Poll until image is ready (with timeout)
pub fn poll_until_ready(
  request_id: String,
  max_attempts: Int,
  delay_ms: Int,
) -> Result(String, String) {
  poll_loop(request_id, 0, max_attempts, delay_ms)
}

fn poll_loop(
  request_id: String,
  attempt: Int,
  max_attempts: Int,
  delay_ms: Int,
) -> Result(String, String) {
  case attempt >= max_attempts {
    True -> Error("Timeout waiting for FAL image generation")
    False -> {
      case poll_result(request_id) {
        Ok(TaskReady(url)) -> {
          io.println("[Carousel FAL] Image ready: " <> url)
          Ok(url)
        }
        Ok(TaskFailed(err)) -> Error("FAL generation failed: " <> err)
        Ok(TaskPending) | Ok(TaskProcessing) -> {
          io.println(
            "[Carousel FAL] Poll "
            <> string.inspect(attempt + 1)
            <> "/"
            <> string.inspect(max_attempts)
            <> " - still processing...",
          )
          process.sleep(delay_ms)
          poll_loop(request_id, attempt + 1, max_attempts, delay_ms)
        }
        Error(e) -> Error(e)
      }
    }
  }
}

// ============================================================
// Convenience Functions
// ============================================================

/// Generate image with LoRA and wait for result (blocking)
/// Uses default LoRA model
pub fn generate_and_wait(
  prompt: String,
  art_style: String,
  aspect_ratio: AspectRatio,
) -> Result(String, String) {
  case generate_with_lora(prompt, art_style, aspect_ratio, None) {
    Ok(result) -> Ok(result.image_url)
    Error(e) -> Error(e)
  }
}

/// Generate with custom LoRA and wait
pub fn generate_with_custom_lora(
  prompt: String,
  art_style: String,
  aspect_ratio: AspectRatio,
  lora_url: String,
) -> Result(String, String) {
  case generate_with_lora(prompt, art_style, aspect_ratio, Some(lora_url)) {
    Ok(result) -> Ok(result.image_url)
    Error(e) -> Error(e)
  }
}

/// Generate multiple images in parallel (batch)
pub fn generate_batch(
  prompts: List(#(Int, String)),
  art_style: String,
  aspect_ratio: AspectRatio,
  lora_url: Option(String),
) -> List(#(Int, Result(String, String))) {
  prompts
  |> list.map(fn(item) {
    let #(index, prompt) = item
    let result = case generate_with_lora(prompt, art_style, aspect_ratio, lora_url) {
      Ok(r) -> Ok(r.image_url)
      Error(e) -> Error(e)
    }
    #(index, result)
  })
}

/// Regenerate a specific slide image with LoRA
pub fn regenerate_image(
  prompt: String,
  art_style: String,
  aspect_ratio: AspectRatio,
  lora_url: Option(String),
) -> Result(ImageResult, String) {
  generate_with_lora(prompt, art_style, aspect_ratio, lora_url)
}

// ============================================================
// Default LoRA Access
// ============================================================

/// Get the default LoRA URL
pub fn get_default_lora() -> String {
  default_lora_url
}
