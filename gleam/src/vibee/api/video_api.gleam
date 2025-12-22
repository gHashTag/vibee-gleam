// Video API Handlers
// Auto Video Pipeline endpoints for fully automated video rendering
//
// Endpoints:
// POST /api/video/auto-render - Start pipeline
// GET /api/video/status/:pipeline_id - Check status
// POST /api/video/ai-reels/template1 - AI Reels Template 1

import gleam/bytes_tree
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/string
import gleam/list
import gleam/float
import mist.{type Connection, type ResponseData}
import vibee/video/pipeline
import vibee/video/broll_prompts
import vibee/video/ai_reels_template1

// ============================================================
// Handlers
// ============================================================

/// POST /api/video/auto-render
/// Start fully automated video pipeline
pub fn auto_render_handler(req: Request(Connection)) -> Response(ResponseData) {
  // Read request body
  case mist.read_body(req, 1024 * 1024) {
    Ok(body_req) -> {
      let body_str = case bit_array_to_string(body_req.body) {
        Ok(s) -> s
        Error(_) -> ""
      }

      // Parse JSON request manually
      case parse_auto_render_request(body_str) {
        Ok(render_req) -> {
          // Get config (hardcoded for test mode)
          let config = get_pipeline_config()

          // Build pipeline request
          let pipeline_req = pipeline.PipelineRequest(
            photo_url: render_req.photo_url,
            script_text: render_req.script_text,
            voice_id: render_req.voice_id,
            webhook_url: render_req.webhook_url,
            test_mode: render_req.test_mode,
          )

          // Start pipeline (test mode for now)
          case pipeline.start_test_pipeline(config, pipeline_req) {
            Ok(job) -> {
              let body = pipeline.job_to_json(job) |> json.to_string()

              response.new(202)
              |> response.set_header("content-type", "application/json")
              |> response.set_header("location", "/api/video/status/" <> job.id)
              |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
            }
            Error(err) -> {
              let error_msg = pipeline_error_to_string(err)
              let body = json.object([
                #("error", json.string(error_msg)),
                #("status", json.string("failed")),
              ]) |> json.to_string()

              response.new(500)
              |> response.set_header("content-type", "application/json")
              |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
            }
          }
        }
        Error(err) -> {
          let body = json.object([
            #("error", json.string("Invalid request: " <> err)),
            #("status", json.string("failed")),
          ]) |> json.to_string()

          response.new(400)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
        }
      }
    }
    Error(_) -> {
      let body = json.object([
        #("error", json.string("Failed to read request body")),
        #("status", json.string("failed")),
      ]) |> json.to_string()

      response.new(400)
      |> response.set_header("content-type", "application/json")
      |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
    }
  }
}

/// POST /api/video/ai-render
/// Start AI-powered video pipeline with dynamic B-roll generation
pub fn ai_render_handler(req: Request(Connection)) -> Response(ResponseData) {
  // Read request body
  case mist.read_body(req, 1024 * 1024) {
    Ok(body_req) -> {
      let body_str = case bit_array_to_string(body_req.body) {
        Ok(s) -> s
        Error(_) -> ""
      }

      // Parse JSON request manually
      case parse_auto_render_request(body_str) {
        Ok(render_req) -> {
          // Get config
          let config = get_pipeline_config()

          // Build pipeline request
          let pipeline_req = pipeline.PipelineRequest(
            photo_url: render_req.photo_url,
            script_text: render_req.script_text,
            voice_id: render_req.voice_id,
            webhook_url: render_req.webhook_url,
            test_mode: render_req.test_mode,
          )

          // Start AI pipeline (uses OpenRouter for B-roll generation)
          case pipeline.start_ai_pipeline(config, pipeline_req) {
            Ok(job) -> {
              let body = pipeline.job_to_json(job) |> json.to_string()

              response.new(202)
              |> response.set_header("content-type", "application/json")
              |> response.set_header("location", "/api/video/status/" <> job.id)
              |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
            }
            Error(err) -> {
              let error_msg = pipeline_error_to_string(err)
              let body = json.object([
                #("error", json.string(error_msg)),
                #("status", json.string("failed")),
              ]) |> json.to_string()

              response.new(500)
              |> response.set_header("content-type", "application/json")
              |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
            }
          }
        }
        Error(err) -> {
          let body = json.object([
            #("error", json.string("Invalid request: " <> err)),
            #("status", json.string("failed")),
          ]) |> json.to_string()

          response.new(400)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
        }
      }
    }
    Error(_) -> {
      let body = json.object([
        #("error", json.string("Failed to read request body")),
        #("status", json.string("failed")),
      ]) |> json.to_string()

      response.new(400)
      |> response.set_header("content-type", "application/json")
      |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
    }
  }
}

/// POST /api/video/ai-reels/template1
/// AI Reels Template 1 - Full emulation with test mode
pub fn template1_handler(req: Request(Connection)) -> Response(ResponseData) {
  case mist.read_body(req, 1024 * 1024) {
    Ok(body_req) -> {
      let body_str = case bit_array_to_string(body_req.body) {
        Ok(s) -> s
        Error(_) -> ""
      }

      // Parse JSON request
      case parse_template1_request(body_str) {
        Ok(template_req) -> {
          // Run Template 1 pipeline
          case ai_reels_template1.run_template1(template_req) {
            Ok(resp) -> {
              let body = ai_reels_template1.response_to_json(resp) |> json.to_string()

              response.new(200)
              |> response.set_header("content-type", "application/json")
              |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
            }
            Error(err) -> {
              let body = json.object([
                #("error", json.string(err)),
                #("status", json.string("failed")),
              ]) |> json.to_string()

              response.new(500)
              |> response.set_header("content-type", "application/json")
              |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
            }
          }
        }
        Error(err) -> {
          let body = json.object([
            #("error", json.string("Invalid request: " <> err)),
            #("status", json.string("failed")),
          ]) |> json.to_string()

          response.new(400)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
        }
      }
    }
    Error(_) -> {
      let body = json.object([
        #("error", json.string("Failed to read request body")),
        #("status", json.string("failed")),
      ]) |> json.to_string()

      response.new(400)
      |> response.set_header("content-type", "application/json")
      |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
    }
  }
}

/// GET /api/video/status/:pipeline_id
/// Check pipeline status
pub fn status_handler(pipeline_id: String) -> Response(ResponseData) {
  // TODO: Implement proper status lookup from ETS/database
  // For now, return placeholder response
  let body = json.object([
    #("id", json.string(pipeline_id)),
    #("status", json.string("pending")),
    #("message", json.string("Pipeline status lookup not yet implemented. Pipeline was started successfully.")),
  ]) |> json.to_string()

  response.new(200)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

/// GET /api/video/prompts/preview
/// Preview B-roll prompts for a script (no render)
pub fn prompts_preview_handler(req: Request(Connection)) -> Response(ResponseData) {
  // Get script from query params
  let query = case req.query {
    Some(q) -> q
    None -> ""
  }

  // Parse script parameter
  let script = case parse_query_param(query, "script") {
    Some(s) -> s
    None -> "business presentation"
  }

  let duration_str = case parse_query_param(query, "duration") {
    Some(d) -> d
    None -> "25.92"
  }

  let duration = case float.parse(duration_str) {
    Ok(d) -> d
    Error(_) -> 25.92
  }

  // Generate prompts
  let prompts = broll_prompts.get_preset_prompts(script, duration)

  let body = json.object([
    #("script", json.string(script)),
    #("duration", json.float(duration)),
    #("prompts", broll_prompts.prompts_to_json(prompts)),
    #("count", json.int(list.length(prompts))),
  ]) |> json.to_string()

  response.new(200)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

// ============================================================
// Types
// ============================================================

pub type AutoRenderRequest {
  AutoRenderRequest(
    photo_url: String,
    script_text: String,
    voice_id: Option(String),
    webhook_url: Option(String),
    test_mode: Bool,
  )
}

// ============================================================
// Helpers
// ============================================================

fn get_pipeline_config() -> pipeline.PipelineConfig {
  // For test mode, use hardcoded values
  // In production, read from environment
  pipeline.PipelineConfig(
    elevenlabs_api_key: "",
    fal_api_key: "",
    remotion_url: "https://vibee-remotion.fly.dev",
    test_assets_url: "https://vibee-remotion.fly.dev/public",
  )
}

/// Simple JSON parsing for auto-render request
fn parse_auto_render_request(body: String) -> Result(AutoRenderRequest, String) {
  case extract_json_string(body, "photo_url") {
    None -> Error("Missing photo_url")
    Some(photo_url) ->
      case extract_json_string(body, "script_text") {
        None -> Error("Missing script_text")
        Some(script_text) -> {
          // Extract optional fields
          let voice_id = extract_json_string(body, "voice_id")
          let webhook_url = extract_json_string(body, "webhook_url")

          // Extract test_mode (default true)
          let test_mode = case extract_json_bool(body, "test_mode") {
            Some(b) -> b
            None -> True
          }

          Ok(AutoRenderRequest(
            photo_url: photo_url,
            script_text: script_text,
            voice_id: voice_id,
            webhook_url: webhook_url,
            test_mode: test_mode,
          ))
        }
      }
  }
}

/// Extract string value from JSON (simple pattern matching)
fn extract_json_string(json_str: String, key: String) -> Option(String) {
  let pattern = "\"" <> key <> "\":"
  case string.split(json_str, pattern) {
    [_, rest] -> {
      // Find the value - it starts with " and ends with "
      let trimmed = string.trim_start(rest)
      case string.starts_with(trimmed, "\"") {
        True -> {
          // Remove leading quote
          let without_start = string.drop_start(trimmed, 1)
          // Find closing quote
          case string.split(without_start, "\"") {
            [value, ..] -> Some(value)
            _ -> None
          }
        }
        False -> None
      }
    }
    _ -> None
  }
}

/// Extract boolean value from JSON
fn extract_json_bool(json_str: String, key: String) -> Option(Bool) {
  let pattern = "\"" <> key <> "\":"
  case string.split(json_str, pattern) {
    [_, rest] -> {
      let trimmed = string.trim_start(rest)
      case string.starts_with(trimmed, "true") {
        True -> Some(True)
        False ->
          case string.starts_with(trimmed, "false") {
            True -> Some(False)
            False -> None
          }
      }
    }
    _ -> None
  }
}

/// Parse Template 1 request from JSON
fn parse_template1_request(body: String) -> Result(ai_reels_template1.Template1Request, String) {
  case extract_json_string(body, "photo_url") {
    None -> Error("Missing photo_url")
    Some(photo_url) ->
      case extract_json_string(body, "text") {
        None -> Error("Missing text")
        Some(text) -> {
          // Extract optional fields
          let voice_id = extract_json_string(body, "voice_id")
          let telegram_id = case extract_json_string(body, "telegram_id") {
            Some(id) -> id
            None -> "0"
          }

          // Extract test_mode (default true)
          let test_mode = case extract_json_bool(body, "test_mode") {
            Some(b) -> b
            None -> True
          }

          Ok(ai_reels_template1.Template1Request(
            photo_url: photo_url,
            text: text,
            voice_id: voice_id,
            telegram_id: telegram_id,
            test_mode: test_mode,
          ))
        }
      }
  }
}

fn parse_query_param(query: String, param: String) -> Option(String) {
  query
  |> string.split("&")
  |> list.filter_map(fn(pair) {
    case string.split(pair, "=") {
      [key, value] if key == param -> Ok(value)
      _ -> Error(Nil)
    }
  })
  |> list.first()
  |> option.from_result()
}

@external(erlang, "unicode", "characters_to_list")
fn unicode_to_list(bits: BitArray) -> Result(String, Nil)

fn bit_array_to_string(bits: BitArray) -> Result(String, Nil) {
  // First try UTF-8 binary_to_list
  case erlang_binary_to_list(bits) {
    Ok(s) -> Ok(s)
    Error(_) -> Error(Nil)
  }
}

@external(erlang, "erlang", "binary_to_list")
fn erlang_binary_to_list(bits: BitArray) -> Result(String, Nil)

fn pipeline_error_to_string(err: pipeline.PipelineError) -> String {
  case err {
    pipeline.TTSError(msg) -> "TTS Error: " <> msg
    pipeline.LipsyncError(msg) -> "Lipsync Error: " <> msg
    pipeline.RenderError(msg) -> "Render Error: " <> msg
    pipeline.ConfigError(msg) -> "Config Error: " <> msg
    pipeline.NetworkError(msg) -> "Network Error: " <> msg
    pipeline.BRollError(msg) -> "B-Roll Error: " <> msg
  }
}
