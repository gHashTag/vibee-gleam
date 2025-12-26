// Carousel BFL Image Generation Service
// Uses FLUX models for high-quality slide backgrounds

import gleam/dynamic/decode
import gleam/erlang/process
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/ai/bfl
import vibee/ai/client as ai_client
import vibee/carousel/types.{type AspectRatio}
import vibee/mcp/config

// ============================================================
// Types
// ============================================================

pub type ImageResult {
  ImageResult(image_url: String, task_id: String)
}

pub type TaskStatus {
  TaskPending
  TaskProcessing
  TaskReady(image_url: String)
  TaskFailed(error: String)
}

// ============================================================
// Image Generation
// ============================================================

/// Generate a slide background image
/// Returns immediately with task_id for polling
pub fn generate_slide_image(
  prompt: String,
  art_style: String,
  aspect_ratio: AspectRatio,
) -> Result(String, String) {
  let api_key = config.get_env_or("BFL_API_KEY", "")
  case api_key {
    "" -> Error("BFL_API_KEY not set")
    key -> {
      let bfl_config = bfl.default_config(key)

      // Get dimensions for aspect ratio
      let #(width, height) = types.aspect_ratio_for_bfl(aspect_ratio)

      // Build full prompt with style
      let full_prompt = build_full_prompt(prompt, art_style)

      io.println(
        "[Carousel BFL] Generating image: "
        <> string.slice(full_prompt, 0, 50)
        <> "...",
      )
      io.println(
        "[Carousel BFL] Size: "
        <> string.inspect(width)
        <> "x"
        <> string.inspect(height),
      )

      let request =
        bfl.ImageRequest(
          prompt: full_prompt,
          model: "flux-pro-1.1",
          width: Some(width),
          height: Some(height),
          steps: None,
        )

      let bfl_request = bfl.generate_image_request(bfl_config, request)
      // Convert bfl.Request to ai_client.Request
      let http_request = convert_request(bfl_request)

      case ai_client.execute_json(http_request) {
        Ok(response_body) -> parse_task_id_from_json(response_body)
        Error(e) ->
          Error("BFL request failed: " <> ai_client.error_to_string(e))
      }
    }
  }
}

/// Convert bfl.Request to ai_client.Request
fn convert_request(req: bfl.Request) -> ai_client.Request {
  ai_client.Request(
    url: req.url,
    method: req.method,
    headers: req.headers,
    body: req.body,
  )
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

/// Parse task ID from BFL JSON response
fn parse_task_id_from_json(response_body: String) -> Result(String, String) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    decode.success(id)
  }

  case json.parse(response_body, decoder) {
    Ok(id) -> {
      io.println("[Carousel BFL] Got task ID: " <> id)
      Ok(id)
    }
    Error(_) -> Error("Failed to parse BFL task ID from response")
  }
}

// ============================================================
// Polling for Result
// ============================================================

/// Poll for image generation result
pub fn poll_result(task_id: String) -> Result(TaskStatus, String) {
  let api_key = config.get_env_or("BFL_API_KEY", "")
  case api_key {
    "" -> Error("BFL_API_KEY not set")
    key -> {
      let bfl_config = bfl.default_config(key)
      let bfl_request = bfl.get_result_request(bfl_config, task_id)
      let http_request = convert_request(bfl_request)

      case ai_client.execute_json(http_request) {
        Ok(response_body) -> parse_status_from_json(response_body)
        Error(e) -> Error("BFL poll failed: " <> ai_client.error_to_string(e))
      }
    }
  }
}

/// Parse status from BFL result JSON response
fn parse_status_from_json(response_body: String) -> Result(TaskStatus, String) {
  // First try to decode status
  let status_decoder = {
    use status <- decode.field("status", decode.string)
    decode.success(status)
  }

  case json.parse(response_body, status_decoder) {
    Ok(status) -> {
      case status {
        "Ready" -> {
          // Try to get the result URL using nested field access
          let url_decoder = {
            use sample <- decode.field("result", {
              use sample <- decode.field("sample", decode.string)
              decode.success(sample)
            })
            decode.success(sample)
          }
          case json.parse(response_body, url_decoder) {
            Ok(url) -> Ok(TaskReady(url))
            Error(_) -> Error("Result ready but no URL found")
          }
        }
        "Pending" -> Ok(TaskPending)
        "Processing" -> Ok(TaskProcessing)
        "Task not found" -> Ok(TaskProcessing)
        "Failed" | "Error" | "Content Moderated" -> {
          // Try to get error message
          let error_decoder = {
            use err <- decode.optional_field("error", "", decode.string)
            decode.success(err)
          }
          case json.parse(response_body, error_decoder) {
            Ok("") -> Ok(TaskFailed("Generation failed: " <> status))
            Ok(err) -> Ok(TaskFailed(err))
            _ -> Ok(TaskFailed("Generation failed: " <> status))
          }
        }
        _ -> Ok(TaskProcessing)
      }
    }
    Error(_) -> Error("Failed to parse status from response")
  }
}

/// Poll until image is ready (with timeout)
/// Returns the image URL
pub fn poll_until_ready(
  task_id: String,
  max_attempts: Int,
  delay_ms: Int,
) -> Result(String, String) {
  poll_loop(task_id, 0, max_attempts, delay_ms)
}

fn poll_loop(
  task_id: String,
  attempt: Int,
  max_attempts: Int,
  delay_ms: Int,
) -> Result(String, String) {
  case attempt >= max_attempts {
    True -> Error("Timeout waiting for image generation")
    False -> {
      case poll_result(task_id) {
        Ok(TaskReady(url)) -> {
          io.println("[Carousel BFL] Image ready: " <> url)
          Ok(url)
        }
        Ok(TaskFailed(err)) -> Error("Image generation failed: " <> err)
        Ok(TaskPending) | Ok(TaskProcessing) -> {
          io.println(
            "[Carousel BFL] Poll "
            <> string.inspect(attempt + 1)
            <> "/"
            <> string.inspect(max_attempts)
            <> " - still processing...",
          )
          process.sleep(delay_ms)
          poll_loop(task_id, attempt + 1, max_attempts, delay_ms)
        }
        Error(e) -> Error(e)
      }
    }
  }
}

// ============================================================
// Convenience Functions
// ============================================================

/// Generate image and wait for result (blocking)
/// Polls every 3 seconds for up to 2 minutes
pub fn generate_and_wait(
  prompt: String,
  art_style: String,
  aspect_ratio: AspectRatio,
) -> Result(String, String) {
  case generate_slide_image(prompt, art_style, aspect_ratio) {
    Ok(task_id) -> poll_until_ready(task_id, 40, 3000)
    Error(e) -> Error(e)
  }
}

/// Generate multiple images in parallel (batch)
/// Returns list of (slide_index, task_id) pairs
pub fn generate_batch(
  prompts: List(#(Int, String)),
  art_style: String,
  aspect_ratio: AspectRatio,
) -> List(#(Int, Result(String, String))) {
  prompts
  |> list.map(fn(item) {
    let #(index, prompt) = item
    let result = generate_slide_image(prompt, art_style, aspect_ratio)
    #(index, result)
  })
}

/// Poll multiple tasks and get results
pub fn poll_batch(
  tasks: List(#(Int, String)),
  max_attempts: Int,
  delay_ms: Int,
) -> List(#(Int, Result(String, String))) {
  tasks
  |> list.map(fn(item) {
    let #(index, task_id) = item
    let result = poll_until_ready(task_id, max_attempts, delay_ms)
    #(index, result)
  })
}

// ============================================================
// Image Regeneration
// ============================================================

/// Regenerate a specific slide image with a new prompt
pub fn regenerate_image(
  prompt: String,
  art_style: String,
  aspect_ratio: AspectRatio,
) -> Result(ImageResult, String) {
  case generate_slide_image(prompt, art_style, aspect_ratio) {
    Ok(task_id) -> {
      case poll_until_ready(task_id, 40, 3000) {
        Ok(url) -> Ok(ImageResult(image_url: url, task_id: task_id))
        Error(e) -> Error(e)
      }
    }
    Error(e) -> Error(e)
  }
}

// ============================================================
// Upscale (if needed)
// ============================================================

/// Upscale image to higher resolution (for export)
/// Uses FLUX Ultra model
pub fn upscale_image(
  image_url: String,
  _target_width: Int,
  _target_height: Int,
) -> Result(String, String) {
  let api_key = config.get_env_or("BFL_API_KEY", "")
  case api_key {
    "" -> Error("BFL_API_KEY not set")
    key -> {
      let bfl_config = bfl.default_config(key)

      let request =
        bfl.ImageToImageRequest(
          prompt: "Upscale this image, maintain quality and details",
          model: "flux-pro-1.1-ultra",
          image_url: image_url,
          strength: Some(0.3),
          // Low strength = mostly preserve original
          seed: None,
          guidance_scale: None,
          steps: None,
        )

      let bfl_request = bfl.image_to_image_request(bfl_config, request)
      let http_request = convert_request(bfl_request)

      case ai_client.execute_json(http_request) {
        Ok(response_body) -> {
          case parse_task_id_from_json(response_body) {
            Ok(task_id) -> poll_until_ready(task_id, 60, 3000)
            Error(e) -> Error(e)
          }
        }
        Error(e) ->
          Error("Upscale request failed: " <> ai_client.error_to_string(e))
      }
    }
  }
}
