// MCP AI Tool Handlers
// Simplified version - connects AI tools to API clients
// All HTTP execution goes through client.gleam (NO code duplication)

import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/ai/bfl
import vibee/ai/client
import vibee/ai/elevenlabs
import vibee/ai/fal
import vibee/ai/hedra
import vibee/ai/heygen
import vibee/ai/kieai
import vibee/ai/kling
import vibee/ai/openai
import vibee/ai/replicate
import vibee/ai/s3
import vibee/mcp/cache
import vibee/mcp/config
import vibee/mcp/protocol
import vibee/mcp/types.{type ToolResult, TextContent, ToolResult}

// ============================================================
// Response Helpers
// ============================================================

fn success_response(data: json.Json) -> json.Json {
  json.object([
    #("success", json.bool(True)),
    #("data", data),
  ])
}

fn error_response(message: String) -> json.Json {
  json.object([
    #("success", json.bool(False)),
    #("error", json.string(message)),
  ])
}

fn not_configured(service: String) -> json.Json {
  error_response(service <> " API key not configured")
}

// ============================================================
// Timestamp Helper
// ============================================================

/// Get current Unix timestamp in seconds
@external(erlang, "erlang", "system_time")
fn system_time_native() -> Int

/// Get current Unix timestamp in seconds
fn get_current_timestamp() -> Int {
  system_time_native() / 1_000_000_000
}

// ============================================================
// JSON Parsing Helpers
// ============================================================

fn json_get_string(j: json.Json, key: String) -> Result(String, Nil) {
  let decoder = {
    use v <- decode.field(key, decode.string)
    decode.success(v)
  }
  let json_str = json.to_string(j)
  case json.parse(json_str, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

fn json_get_optional_string(j: json.Json, key: String) -> Option(String) {
  case json_get_string(j, key) {
    Ok(v) -> Some(v)
    Error(_) -> None
  }
}

fn json_get_int(j: json.Json, key: String) -> Result(Int, Nil) {
  let decoder = {
    use v <- decode.field(key, decode.int)
    decode.success(v)
  }
  let json_str = json.to_string(j)
  case json.parse(json_str, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

fn json_get_optional_int(j: json.Json, key: String) -> Option(Int) {
  case json_get_int(j, key) {
    Ok(v) -> Some(v)
    Error(_) -> None
  }
}

fn json_get_float(j: json.Json, key: String) -> Result(Float, Nil) {
  let decoder = {
    use v <- decode.field(key, decode.float)
    decode.success(v)
  }
  let json_str = json.to_string(j)
  case json.parse(json_str, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

fn json_get_optional_float(j: json.Json, key: String) -> Option(Float) {
  case json_get_float(j, key) {
    Ok(v) -> Some(v)
    Error(_) -> None
  }
}

// ============================================================
// ElevenLabs FFI Functions
// ============================================================

/// Download file from URL
@external(erlang, "vibee_http_ffi", "download_file")
fn download_file_ffi(url: String) -> Result(BitArray, String)

/// Clone voice with multipart upload
@external(erlang, "vibee_http_ffi", "clone_voice")
fn clone_voice_ffi(
  api_key: String,
  name: String,
  description: String,
  audio_data: BitArray,
) -> Result(String, String)

// ============================================================
// ElevenLabs Handlers
// ============================================================

pub fn handle_elevenlabs_tts(text: String, voice_id: Option(String)) -> json.Json {
  let api_key = config.get_env("ELEVENLABS_API_KEY")
  case api_key {
    "" -> not_configured("ElevenLabs")
    key -> {
      // Create config - use provided voice_id or default
      let vid = case voice_id {
        Some(v) -> v
        None -> "EXAVITQu4vr4xnSDxMaL"
      }
      let conf = elevenlabs.Config(
        api_key: key,
        voice_id: vid,
        model: "eleven_multilingual_v2",
      )
      let req = elevenlabs.create_tts_request(conf, text)
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_binary(client_req) {
        Ok(_) -> success_response(json.object([
          #("status", json.string("completed")),
          #("message", json.string("Audio generated")),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

pub fn handle_elevenlabs_list_voices() -> json.Json {
  let api_key = config.get_env("ELEVENLABS_API_KEY")
  case api_key {
    "" -> not_configured("ElevenLabs")
    key -> {
      let conf = elevenlabs.Config(
        api_key: key,
        voice_id: "EXAVITQu4vr4xnSDxMaL",
        model: "eleven_multilingual_v2",
      )
      let req = elevenlabs.list_voices_request(conf)
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("voices", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

/// Clone voice from audio URL using ElevenLabs Instant Voice Cloning
/// Downloads the audio from URL and uploads via multipart/form-data
pub fn handle_elevenlabs_clone_voice(
  name: String,
  audio_url: String,
  description: Option(String),
) -> json.Json {
  let api_key = config.get_env("ELEVENLABS_API_KEY")
  case api_key {
    "" -> not_configured("ElevenLabs")
    key -> {
      // Step 1: Download audio from URL
      case download_file_ffi(audio_url) {
        Error(e) -> error_response("Failed to download audio: " <> e)
        Ok(audio_data) -> {
          // Step 2: Clone voice via multipart upload
          let desc = case description {
            Some(d) -> d
            None -> "Voice cloned via Vibee from audio URL"
          }
          case clone_voice_ffi(key, name, desc, audio_data) {
            Ok(response_body) -> {
              // Parse voice_id from response
              // Response format: {"voice_id": "xxx", "name": "yyy"}
              success_response(json.object([
                #("result", json.string(response_body)),
                #("message", json.string("Voice cloned successfully")),
              ]))
            }
            Error(e) -> error_response("Voice cloning failed: " <> e)
          }
        }
      }
    }
  }
}

/// Delete a cloned voice by voice_id
pub fn handle_elevenlabs_delete_voice(voice_id: String) -> json.Json {
  let api_key = config.get_env("ELEVENLABS_API_KEY")
  case api_key {
    "" -> not_configured("ElevenLabs")
    key -> {
      let conf = elevenlabs.Config(
        api_key: key,
        voice_id: voice_id,
        model: "eleven_multilingual_v2",
      )
      let req = elevenlabs.delete_voice_request(conf, voice_id)
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute(client_req) {
        Ok(resp) -> {
          case client.is_success(resp) {
            True -> success_response(json.object([
              #("message", json.string("Voice deleted successfully")),
              #("voice_id", json.string(voice_id)),
            ]))
            False -> error_response("Failed to delete voice: " <> resp.body)
          }
        }
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

// ============================================================
// OpenAI Handlers
// ============================================================

pub fn handle_openai_chat(message: String, system_prompt: Option(String)) -> json.Json {
  let api_key = config.get_env("OPENAI_API_KEY")
  case api_key {
    "" -> not_configured("OpenAI")
    key -> {
      let conf = openai.default_config(key)
      let chat_req = case system_prompt {
        Some(sys) -> openai.chat_with_system(sys, message)
        None -> openai.simple_chat(message)
      }
      let req = openai.create_chat_request(conf, chat_req)
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("response", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

pub fn handle_openai_embedding(text: String) -> json.Json {
  let api_key = config.get_env("OPENAI_API_KEY")
  case api_key {
    "" -> not_configured("OpenAI")
    key -> {
      let conf = openai.default_config(key)
      let req = openai.create_embedding_request(conf, openai.simple_embedding(text))
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("embedding", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

// ============================================================
// Replicate Handlers
// ============================================================

pub fn handle_replicate_run(model: String, prompt: Option(String)) -> json.Json {
  let api_key = config.get_env("REPLICATE_API_TOKEN")
  case api_key {
    "" -> not_configured("Replicate")
    key -> {
      let conf = replicate.default_config(key)
      let input = case prompt {
        Some(p) -> [#("prompt", json.string(p))]
        None -> []
      }
      let req = replicate.create_prediction_request(conf, replicate.PredictionRequest(
        model: model,
        version: None,
        input: input,
      ))
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("prediction", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

pub fn handle_replicate_get_prediction(prediction_id: String) -> json.Json {
  let api_key = config.get_env("REPLICATE_API_TOKEN")
  case api_key {
    "" -> not_configured("Replicate")
    key -> {
      let conf = replicate.default_config(key)
      let req = replicate.get_prediction_request(conf, prediction_id)
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("prediction", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

// ============================================================
// S3 Handlers
// ============================================================

pub fn handle_s3_upload(key: String, content: String, content_type: Option(String)) -> json.Json {
  let access_key = config.get_env("AWS_ACCESS_KEY_ID")
  let secret_key = config.get_env("AWS_SECRET_ACCESS_KEY")
  let bucket = config.get_env("AWS_S3_BUCKET")
  case access_key, secret_key, bucket {
    "", _, _ -> not_configured("AWS S3")
    _, "", _ -> not_configured("AWS S3")
    _, _, "" -> not_configured("AWS S3")
    ak, sk, b -> {
      let conf = s3.default_config(ak, sk, b)
      let ct = case content_type {
        Some(t) -> t
        None -> s3.content_type_binary()
      }
      let req = s3.put_object_request(conf, s3.UploadRequest(
        key: key,
        content_type: ct,
        body: content,
      ))
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute(client_req) {
        Ok(resp) -> {
          case client.is_success(resp) {
            True -> success_response(json.object([
              #("url", json.string(s3.get_object_url(conf, key))),
              #("key", json.string(key)),
            ]))
            False -> error_response("Upload failed: " <> resp.body)
          }
        }
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

pub fn handle_s3_download(key: String) -> json.Json {
  let access_key = config.get_env("AWS_ACCESS_KEY_ID")
  let secret_key = config.get_env("AWS_SECRET_ACCESS_KEY")
  let bucket = config.get_env("AWS_S3_BUCKET")
  case access_key, secret_key, bucket {
    "", _, _ -> not_configured("AWS S3")
    _, "", _ -> not_configured("AWS S3")
    _, _, "" -> not_configured("AWS S3")
    ak, sk, b -> {
      let conf = s3.default_config(ak, sk, b)
      let req = s3.get_object_request(conf, key)
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute(client_req) {
        Ok(resp) -> {
          case client.is_success(resp) {
            True -> success_response(json.object([
              #("content", json.string(resp.body)),
              #("key", json.string(key)),
            ]))
            False -> error_response("Download failed")
          }
        }
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

// ============================================================
// BFL (FLUX) Handlers - Placeholder
// ============================================================

pub fn handle_bfl_generate_image(prompt: String) -> json.Json {
  let api_key = config.get_env("BFL_API_KEY")
  case api_key {
    "" -> not_configured("BFL")
    key -> {
      let conf = bfl.Config(api_key: key)
      // Use simple_request to create ImageRequest, then generate_image_request
      let image_req = bfl.simple_request(prompt)
      let req = bfl.generate_image_request(conf, image_req)
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("task", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

pub fn handle_bfl_get_result(task_id: String) -> json.Json {
  let api_key = config.get_env("BFL_API_KEY")
  case api_key {
    "" -> not_configured("BFL")
    key -> {
      let conf = bfl.Config(api_key: key)
      let req = bfl.get_result_request(conf, task_id)
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("result", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

// ============================================================
// Hedra Handlers - Placeholder
// ============================================================

pub fn handle_hedra_create_avatar(audio_url: String, image_url: String) -> json.Json {
  let api_key = config.get_env("HEDRA_API_KEY")
  case api_key {
    "" -> not_configured("Hedra")
    key -> {
      let conf = hedra.Config(api_key: key)
      let req = hedra.create_avatar_request(conf, hedra.AvatarRequest(
        audio_url: audio_url,
        image_url: image_url,
        aspect_ratio: None,
      ))
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("job", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

pub fn handle_hedra_get_status(job_id: String) -> json.Json {
  let api_key = config.get_env("HEDRA_API_KEY")
  case api_key {
    "" -> not_configured("Hedra")
    key -> {
      let conf = hedra.Config(api_key: key)
      let req = hedra.get_status_request(conf, job_id)
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("status", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

pub fn handle_hedra_list_jobs() -> json.Json {
  let api_key = config.get_env("HEDRA_API_KEY")
  case api_key {
    "" -> not_configured("Hedra")
    key -> {
      let conf = hedra.Config(api_key: key)
      let req = hedra.list_jobs_request(conf)
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("jobs", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

// ============================================================
// Kling Handlers - Placeholder
// ============================================================

pub fn handle_kling_create_video(
  prompt: String,
  duration: Option(String),
  mode: Option(String),
  aspect_ratio: Option(String),
) -> json.Json {
  let access = config.get_env("KLING_ACCESS_KEY")
  let secret = config.get_env("KLING_SECRET_KEY")
  case access, secret {
    "", _ -> not_configured("Kling")
    _, "" -> not_configured("Kling")
    a, s -> {
      let conf = kling.Config(access_key: a, secret_key: s)
      // Create VideoRequest with provided or default values
      let video_req = kling.VideoRequest(
        prompt: prompt,
        mode: option.unwrap(mode, "std"),
        duration: option.unwrap(duration, "5"),
        aspect_ratio: aspect_ratio,
      )
      // Use current timestamp (Unix seconds) - in real usage, get from erlang:system_time
      let timestamp = get_current_timestamp()
      let req = kling.create_video_request(conf, video_req, timestamp)
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("task", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

pub fn handle_kling_get_task(task_id: String) -> json.Json {
  let access = config.get_env("KLING_ACCESS_KEY")
  let secret = config.get_env("KLING_SECRET_KEY")
  case access, secret {
    "", _ -> not_configured("Kling")
    _, "" -> not_configured("Kling")
    a, s -> {
      let conf = kling.Config(access_key: a, secret_key: s)
      let timestamp = get_current_timestamp()
      let req = kling.get_task_request(conf, task_id, timestamp)
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("status", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

pub fn handle_kling_image_to_video(
  image_url: String,
  prompt: Option(String),
  duration: Option(String),
) -> json.Json {
  let access = config.get_env("KLING_ACCESS_KEY")
  let secret = config.get_env("KLING_SECRET_KEY")
  case access, secret {
    "", _ -> not_configured("Kling")
    _, "" -> not_configured("Kling")
    a, s -> {
      let conf = kling.Config(access_key: a, secret_key: s)
      let timestamp = get_current_timestamp()
      let req = kling.image_to_video_request(
        conf,
        kling.ImageToVideoRequest(
          image_url: image_url,
          prompt: prompt,
          duration: option.unwrap(duration, "5"),
        ),
        timestamp,
      )
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("task", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

pub fn handle_kling_list_tasks() -> json.Json {
  let access = config.get_env("KLING_ACCESS_KEY")
  let secret = config.get_env("KLING_SECRET_KEY")
  case access, secret {
    "", _ -> not_configured("Kling")
    _, "" -> not_configured("Kling")
    a, s -> {
      let conf = kling.Config(access_key: a, secret_key: s)
      let timestamp = get_current_timestamp()
      let req = kling.list_tasks_request(conf, timestamp)
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("tasks", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

// ============================================================
// KIE.ai (Veo3) Handlers
// ============================================================

pub fn handle_kieai_create_video(
  prompt: String,
  model: Option(String),
  aspect_ratio: Option(String),
  seeds: Option(Int),
) -> json.Json {
  let api_key = config.get_env("KIEAI_API_KEY")
  case api_key {
    "" -> not_configured("KIE.ai")
    key -> {
      let conf = kieai.Config(api_key: key)
      let seed_value = case seeds {
        Some(s) -> s
        None -> 50_000
      }
      let req = kieai.create_video_request(
        conf,
        kieai.VideoRequest(
          prompt: prompt,
          seeds: seed_value,
          model: model,
          aspect_ratio: aspect_ratio,
        ),
      )
      let client_req =
        client.Request(
          url: req.url,
          method: req.method,
          headers: req.headers,
          body: req.body,
        )
      case client.execute_json(client_req) {
        Ok(body) ->
          success_response(json.object([#("task", json.string(body))]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

pub fn handle_kieai_get_status(task_id: String) -> json.Json {
  let api_key = config.get_env("KIEAI_API_KEY")
  case api_key {
    "" -> not_configured("KIE.ai")
    key -> {
      let conf = kieai.Config(api_key: key)
      let req = kieai.get_task_status_request(conf, task_id)
      let client_req =
        client.Request(
          url: req.url,
          method: req.method,
          headers: req.headers,
          body: req.body,
        )
      case client.execute_json(client_req) {
        Ok(body) ->
          success_response(json.object([#("status", json.string(body))]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

pub fn handle_kieai_list_videos(limit: Int, offset: Int) -> json.Json {
  let api_key = config.get_env("KIEAI_API_KEY")
  case api_key {
    "" -> not_configured("KIE.ai")
    key -> {
      let conf = kieai.Config(api_key: key)
      let req = kieai.list_videos_request(conf, limit, offset)
      let client_req =
        client.Request(
          url: req.url,
          method: req.method,
          headers: req.headers,
          body: req.body,
        )
      case client.execute_json(client_req) {
        Ok(body) ->
          success_response(json.object([#("videos", json.string(body))]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

// ============================================================
// HeyGen Handlers - Placeholder
// ============================================================

pub fn handle_heygen_create_video(avatar_id: String, script: String) -> json.Json {
  let api_key = config.get_env("HEYGEN_API_KEY")
  case api_key {
    "" -> not_configured("HeyGen")
    key -> {
      let conf = heygen.Config(api_key: key)
      let req = heygen.create_video_request(conf, heygen.VideoRequest(
        avatar_id: avatar_id,
        script: script,
        voice_id: None,
        background_url: None,
      ))
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("video", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

pub fn handle_heygen_list_avatars() -> json.Json {
  let api_key = config.get_env("HEYGEN_API_KEY")
  case api_key {
    "" -> not_configured("HeyGen")
    key -> {
      let conf = heygen.Config(api_key: key)
      let req = heygen.list_avatars_request(conf)
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("avatars", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

pub fn handle_heygen_get_video_status(video_id: String) -> json.Json {
  let api_key = config.get_env("HEYGEN_API_KEY")
  case api_key {
    "" -> not_configured("HeyGen")
    key -> {
      let conf = heygen.Config(api_key: key)
      let req = heygen.get_video_status_request(conf, video_id)
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("status", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

pub fn handle_heygen_list_voices() -> json.Json {
  let api_key = config.get_env("HEYGEN_API_KEY")
  case api_key {
    "" -> not_configured("HeyGen")
    key -> {
      let conf = heygen.Config(api_key: key)
      let req = heygen.list_voices_request(conf)
      let client_req = client.Request(
        url: req.url, method: req.method, headers: req.headers, body: req.body
      )
      case client.execute_json(client_req) {
        Ok(body) -> success_response(json.object([
          #("voices", json.string(body)),
        ]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

// ============================================================
// FAL.ai Handlers (NeuroPhoto, Nano Banana, Flux Kontext)
// ============================================================

/// Generate NeuroPhoto image with FLUX LoRA
pub fn handle_fal_neuro_photo(
  prompt: String,
  lora_url: String,
  num_images: Option(Int),
  width: Option(Int),
  height: Option(Int),
  seed: Option(Int),
  guidance_scale: Option(Float),
  num_inference_steps: Option(Int),
) -> json.Json {
  let api_key = config.get_env("FAL_API_KEY")
  case api_key {
    "" -> not_configured("FAL.ai")
    key -> {
      let conf = fal.Config(api_key: key)
      let n_images = case num_images {
        Some(n) -> n
        None -> 1
      }
      let w = case width {
        Some(v) -> v
        None -> 768
      }
      let h = case height {
        Some(v) -> v
        None -> 1365
      }
      let req = fal.neuro_photo_request(
        conf,
        fal.NeuroPhotoRequest(
          prompt: prompt,
          lora_url: lora_url,
          num_images: n_images,
          image_size: fal.ImageSize(width: w, height: h),
          seed: seed,
          guidance_scale: guidance_scale,
          num_inference_steps: num_inference_steps,
          enable_safety_checker: True,
        ),
      )
      let client_req =
        client.Request(
          url: req.url,
          method: req.method,
          headers: req.headers,
          body: req.body,
        )
      case client.execute_json(client_req) {
        Ok(body) ->
          success_response(json.object([#("result", json.string(body))]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

/// Generate image with Google Nano Banana Pro
pub fn handle_fal_nano_banana(
  prompt: String,
  num_images: Option(Int),
  aspect_ratio: Option(String),
  resolution: Option(String),
  seed: Option(Int),
) -> json.Json {
  let api_key = config.get_env("FAL_API_KEY")
  case api_key {
    "" -> not_configured("FAL.ai")
    key -> {
      let conf = fal.Config(api_key: key)
      let n_images = case num_images {
        Some(n) -> n
        None -> 1
      }
      let ar = case aspect_ratio {
        Some(a) -> a
        None -> "9:16"
      }
      let res = case resolution {
        Some(r) -> r
        None -> "1K"
      }
      let req = fal.nano_banana_request(
        conf,
        fal.NanoBananaRequest(
          prompt: prompt,
          num_images: n_images,
          aspect_ratio: ar,
          resolution: res,
          seed: seed,
        ),
      )
      let client_req =
        client.Request(
          url: req.url,
          method: req.method,
          headers: req.headers,
          body: req.body,
        )
      case client.execute_json(client_req) {
        Ok(body) ->
          success_response(json.object([#("result", json.string(body))]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

/// Edit image with FLUX Kontext (Pro or Max)
pub fn handle_fal_flux_kontext(
  prompt: String,
  input_image_url: String,
  model_type: Option(String),
  aspect_ratio: Option(String),
  seed: Option(Int),
) -> json.Json {
  let api_key = config.get_env("FAL_API_KEY")
  case api_key {
    "" -> not_configured("FAL.ai")
    key -> {
      let conf = fal.Config(api_key: key)
      let model = case model_type {
        Some(m) -> m
        None -> "pro"
      }
      let req = fal.flux_kontext_request(
        conf,
        fal.FluxKontextRequest(
          prompt: prompt,
          input_image_url: input_image_url,
          model_type: model,
          mode: fal.Single,
          aspect_ratio: aspect_ratio,
          seed: seed,
        ),
      )
      let client_req =
        client.Request(
          url: req.url,
          method: req.method,
          headers: req.headers,
          body: req.body,
        )
      case client.execute_json(client_req) {
        Ok(body) ->
          success_response(json.object([#("result", json.string(body))]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

/// Get FAL.ai queue status
pub fn handle_fal_get_status(request_id: String) -> json.Json {
  let api_key = config.get_env("FAL_API_KEY")
  case api_key {
    "" -> not_configured("FAL.ai")
    key -> {
      let conf = fal.Config(api_key: key)
      let req = fal.get_queue_status(conf, request_id)
      let client_req =
        client.Request(
          url: req.url,
          method: req.method,
          headers: req.headers,
          body: req.body,
        )
      case client.execute_json(client_req) {
        Ok(body) ->
          success_response(json.object([#("status", json.string(body))]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

/// Get FAL.ai queue result
pub fn handle_fal_get_result(request_id: String) -> json.Json {
  let api_key = config.get_env("FAL_API_KEY")
  case api_key {
    "" -> not_configured("FAL.ai")
    key -> {
      let conf = fal.Config(api_key: key)
      let req = fal.get_queue_result(conf, request_id)
      let client_req =
        client.Request(
          url: req.url,
          method: req.method,
          headers: req.headers,
          body: req.body,
        )
      case client.execute_json(client_req) {
        Ok(body) ->
          success_response(json.object([#("result", json.string(body))]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

// ============================================================
// Replicate LipSync Handler (Kling LipSync)
// ============================================================

/// Generate lip-sync video using Kling model via Replicate
pub fn handle_replicate_lipsync(video_url: String, audio_url: String) -> json.Json {
  let api_key = config.get_env("REPLICATE_API_TOKEN")
  case api_key {
    "" -> not_configured("Replicate")
    key -> {
      let conf = replicate.default_config(key)
      let input = [
        #("video_url", json.string(video_url)),
        #("audio_url", json.string(audio_url)),
      ]
      let req =
        replicate.create_prediction_request(
          conf,
          replicate.PredictionRequest(
            model: "kwaivgi/kling-lip-sync",
            version: None,
            input: input,
          ),
        )
      let client_req =
        client.Request(
          url: req.url,
          method: req.method,
          headers: req.headers,
          body: req.body,
        )
      case client.execute_json(client_req) {
        Ok(body) ->
          success_response(json.object([#("prediction", json.string(body))]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

// ============================================================
// Replicate Morphing Handler (Kling Morphing)
// ============================================================

/// Create morphing video between two images using Kling model via Replicate
pub fn handle_replicate_morphing(
  start_image_url: String,
  end_image_url: String,
  prompt: Option(String),
  duration: Option(String),
) -> json.Json {
  let api_key = config.get_env("REPLICATE_API_TOKEN")
  case api_key {
    "" -> not_configured("Replicate")
    key -> {
      let conf = replicate.default_config(key)
      let default_prompt =
        "smooth cinematic transition, elegant morphing between frames, constant camera movement, soft cinematic lighting, professional cinematography, motion blur, 4k quality"
      let morph_prompt = case prompt {
        Some(p) -> p
        None -> default_prompt
      }
      let dur = case duration {
        Some(d) -> d
        None -> "5"
      }
      let input = [
        #("start_image", json.string(start_image_url)),
        #("end_image", json.string(end_image_url)),
        #("prompt", json.string(morph_prompt)),
        #("duration", json.string(dur)),
      ]
      let req =
        replicate.create_prediction_request(
          conf,
          replicate.PredictionRequest(
            model: "kwaivgi/kling-v2.1-pro",
            version: None,
            input: input,
          ),
        )
      let client_req =
        client.Request(
          url: req.url,
          method: req.method,
          headers: req.headers,
          body: req.body,
        )
      case client.execute_json(client_req) {
        Ok(body) ->
          success_response(json.object([#("prediction", json.string(body))]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

// ============================================================
// FFmpeg FFI Functions (Video Editing)
// ============================================================

/// Concatenate multiple video files
@external(erlang, "vibee_ffmpeg_ffi", "concat_videos")
fn ffmpeg_concat_ffi(
  input_files: List(String),
  output_path: String,
) -> Result(String, String)

/// Add audio track to video
@external(erlang, "vibee_ffmpeg_ffi", "add_audio")
fn ffmpeg_add_audio_ffi(
  video_path: String,
  audio_path: String,
  output_path: String,
  options: List(#(String, String)),
) -> Result(String, String)

/// Add watermark to video
@external(erlang, "vibee_ffmpeg_ffi", "add_watermark")
fn ffmpeg_watermark_ffi(
  video_path: String,
  watermark_path: String,
  output_path: String,
  options: List(#(String, String)),
) -> Result(String, String)

/// Trim video
@external(erlang, "vibee_ffmpeg_ffi", "trim_video")
fn ffmpeg_trim_ffi(
  video_path: String,
  output_path: String,
  start_time: String,
  duration: String,
) -> Result(String, String)

/// Get video info
@external(erlang, "vibee_ffmpeg_ffi", "get_video_info")
fn ffmpeg_info_ffi(video_path: String) -> Result(String, String)

/// Extract audio from video
@external(erlang, "vibee_ffmpeg_ffi", "extract_audio")
fn ffmpeg_extract_audio_ffi(
  video_path: String,
  output_path: String,
) -> Result(String, String)

// ============================================================
// Video Editing Handlers
// ============================================================

/// Generate unique output filename
fn generate_output_path(prefix: String, extension: String) -> String {
  let timestamp = get_current_timestamp()
  "/tmp/" <> prefix <> "_" <> string.inspect(timestamp) <> "." <> extension
}

/// Concatenate multiple videos into one
pub fn handle_video_concat(video_urls: List(String)) -> json.Json {
  let output_path = generate_output_path("concat", "mp4")
  case ffmpeg_concat_ffi(video_urls, output_path) {
    Ok(path) ->
      success_response(json.object([
        #("output_path", json.string(path)),
        #("message", json.string("Videos concatenated successfully")),
      ]))
    Error(e) -> error_response("FFmpeg concat error: " <> e)
  }
}

fn wrap_video_concat(args: json.Json) -> ToolResult {
  let decoder = {
    use v <- decode.field("video_urls", decode.list(decode.string))
    decode.success(v)
  }
  let json_str = json.to_string(args)
  case json.parse(json_str, decoder) {
    Ok(urls) -> json_to_result(handle_video_concat(urls))
    Error(_) -> protocol.error_result("Missing required field: video_urls (array of URLs)")
  }
}

/// Add audio track to video
pub fn handle_video_add_audio(
  video_url: String,
  audio_url: String,
  volume: Option(Float),
  replace_audio: Option(Bool),
) -> json.Json {
  let output_path = generate_output_path("audio_added", "mp4")
  let vol = case volume {
    Some(v) -> v
    None -> 1.0
  }
  let replace = case replace_audio {
    Some(r) -> r
    None -> False
  }
  let options = [
    #("volume", string.inspect(vol)),
    #("replace", case replace { True -> "true" False -> "false" }),
  ]
  case ffmpeg_add_audio_ffi(video_url, audio_url, output_path, options) {
    Ok(path) ->
      success_response(json.object([
        #("output_path", json.string(path)),
        #("message", json.string("Audio added successfully")),
      ]))
    Error(e) -> error_response("FFmpeg add_audio error: " <> e)
  }
}

fn wrap_video_add_audio(args: json.Json) -> ToolResult {
  let video_url = json_get_string(args, "video_url")
  let audio_url = json_get_string(args, "audio_url")
  let volume = json_get_optional_float(args, "volume")
  let replace = case json_get_string(args, "replace_audio") {
    Ok("true") -> Some(True)
    Ok("false") -> Some(False)
    _ -> None
  }
  case video_url, audio_url {
    Ok(v), Ok(a) -> json_to_result(handle_video_add_audio(v, a, volume, replace))
    _, _ -> protocol.error_result("Missing required fields: video_url, audio_url")
  }
}

/// Add watermark to video
pub fn handle_video_watermark(
  video_url: String,
  watermark_url: String,
  position: Option(String),
  opacity: Option(Float),
  scale: Option(Float),
) -> json.Json {
  let output_path = generate_output_path("watermarked", "mp4")
  let pos = case position {
    Some(p) -> p
    None -> "bottomright"
  }
  let op = case opacity {
    Some(o) -> o
    None -> 0.7
  }
  let sc = case scale {
    Some(s) -> s
    None -> 0.15
  }
  let options = [
    #("position", pos),
    #("opacity", string.inspect(op)),
    #("scale", string.inspect(sc)),
  ]
  case ffmpeg_watermark_ffi(video_url, watermark_url, output_path, options) {
    Ok(path) ->
      success_response(json.object([
        #("output_path", json.string(path)),
        #("message", json.string("Watermark added successfully")),
      ]))
    Error(e) -> error_response("FFmpeg watermark error: " <> e)
  }
}

fn wrap_video_watermark(args: json.Json) -> ToolResult {
  let video_url = json_get_string(args, "video_url")
  let watermark_url = json_get_string(args, "watermark_url")
  let position = json_get_optional_string(args, "position")
  let opacity = json_get_optional_float(args, "opacity")
  let scale = json_get_optional_float(args, "scale")
  case video_url, watermark_url {
    Ok(v), Ok(w) -> json_to_result(handle_video_watermark(v, w, position, opacity, scale))
    _, _ -> protocol.error_result("Missing required fields: video_url, watermark_url")
  }
}

/// Trim video to specific duration
pub fn handle_video_trim(
  video_url: String,
  start_time: String,
  duration: String,
) -> json.Json {
  let output_path = generate_output_path("trimmed", "mp4")
  case ffmpeg_trim_ffi(video_url, output_path, start_time, duration) {
    Ok(path) ->
      success_response(json.object([
        #("output_path", json.string(path)),
        #("message", json.string("Video trimmed successfully")),
      ]))
    Error(e) -> error_response("FFmpeg trim error: " <> e)
  }
}

fn wrap_video_trim(args: json.Json) -> ToolResult {
  let video_url = json_get_string(args, "video_url")
  let start_time = json_get_string(args, "start_time")
  let duration = json_get_string(args, "duration")
  case video_url, start_time, duration {
    Ok(v), Ok(s), Ok(d) -> json_to_result(handle_video_trim(v, s, d))
    _, _, _ -> protocol.error_result("Missing required fields: video_url, start_time, duration")
  }
}

/// Get video information
pub fn handle_video_info(video_url: String) -> json.Json {
  case ffmpeg_info_ffi(video_url) {
    Ok(info) ->
      success_response(json.object([
        #("info", json.string(info)),
      ]))
    Error(e) -> error_response("FFprobe error: " <> e)
  }
}

fn wrap_video_info(args: json.Json) -> ToolResult {
  let video_url = json_get_string(args, "video_url")
  case video_url {
    Ok(v) -> json_to_result(handle_video_info(v))
    Error(_) -> protocol.error_result("Missing required field: video_url")
  }
}

/// Extract audio from video
pub fn handle_video_extract_audio(video_url: String) -> json.Json {
  let output_path = generate_output_path("extracted", "mp3")
  case ffmpeg_extract_audio_ffi(video_url, output_path) {
    Ok(path) ->
      success_response(json.object([
        #("output_path", json.string(path)),
        #("message", json.string("Audio extracted successfully")),
      ]))
    Error(e) -> error_response("FFmpeg extract error: " <> e)
  }
}

fn wrap_video_extract_audio(args: json.Json) -> ToolResult {
  let video_url = json_get_string(args, "video_url")
  case video_url {
    Ok(v) -> json_to_result(handle_video_extract_audio(v))
    Error(_) -> protocol.error_result("Missing required field: video_url")
  }
}

// ============================================================
// B-Roll Generation Handler
// ============================================================

import vibee/ai/broll_templates

/// Generate B-Roll video using AI
pub fn handle_broll_generate(
  category: String,
  template_name: Option(String),
  custom_prompt: Option(String),
  model: Option(String),
) -> json.Json {
  // Determine which prompt to use
  let prompt = case custom_prompt {
    Some(p) -> p
    None -> {
      case template_name {
        Some(name) -> {
          case broll_templates.find_by_name(name) {
            Ok(template) -> {
              let m = case model {
                Some(mod) -> mod
                None -> "kling"
              }
              broll_templates.get_model_prompt(template, m)
            }
            Error(_) -> {
              // Fall back to category random
              let cat = broll_templates.string_to_category(category)
              case broll_templates.random_from_category(cat, get_current_timestamp()) {
                Ok(template) -> template.prompt
                Error(_) -> "Abstract particles flowing in space, seamless loop, 4K"
              }
            }
          }
        }
        None -> {
          let cat = broll_templates.string_to_category(category)
          case broll_templates.random_from_category(cat, get_current_timestamp()) {
            Ok(template) -> template.prompt
            Error(_) -> "Abstract particles flowing in space, seamless loop, 4K"
          }
        }
      }
    }
  }

  // Use Kling or KIE.ai based on model preference
  let target_model = case model {
    Some("veo3") | Some("kieai") -> "kieai"
    Some("kling") -> "kling"
    Some(other) -> other
    None -> "kling"  // Default to Kling
  }

  case target_model {
    "kieai" -> {
      // Use KIE.ai (Veo3)
      handle_kieai_create_video(prompt, Some("veo3_fast"), None, None)
    }
    _ -> {
      // Use Kling
      handle_kling_create_video(prompt, None, None, None)
    }
  }
}

fn wrap_broll_generate(args: json.Json) -> ToolResult {
  let category = case json_get_string(args, "category") {
    Ok(c) -> c
    Error(_) -> "abstract"
  }
  let template_name = json_get_optional_string(args, "template_name")
  let custom_prompt = json_get_optional_string(args, "custom_prompt")
  let model = json_get_optional_string(args, "model")
  json_to_result(handle_broll_generate(category, template_name, custom_prompt, model))
}

/// List available B-Roll templates
pub fn handle_broll_list_templates(category: Option(String)) -> json.Json {
  let templates = case category {
    Some(cat) -> broll_templates.get_by_category(broll_templates.string_to_category(cat))
    None -> broll_templates.all_templates()
  }

  let template_list = list.map(templates, fn(t) {
    json.object([
      #("name", json.string(t.name)),
      #("category", json.string(broll_templates.category_to_string(t.category))),
      #("prompt", json.string(t.prompt)),
      #("style", json.string(t.style)),
      #("loop_friendly", json.bool(t.loop_friendly)),
      #("duration_hint", json.string(t.duration_hint)),
    ])
  })

  success_response(json.object([
    #("templates", json.array(template_list, fn(x) { x })),
    #("count", json.int(list.length(templates))),
  ]))
}

fn wrap_broll_list_templates(args: json.Json) -> ToolResult {
  let category = json_get_optional_string(args, "category")
  json_to_result(handle_broll_list_templates(category))
}

// ============================================================
// Get all AI handlers for registration
// ============================================================

/// Get all AI tool handlers as (name, handler) pairs for registration
pub fn get_ai_handlers() -> List(#(String, fn(json.Json) -> ToolResult)) {
  [
    #("ai_elevenlabs_tts", wrap_elevenlabs_tts),
    #("ai_elevenlabs_list_voices", wrap_elevenlabs_list_voices),
    #("ai_elevenlabs_clone_voice", wrap_elevenlabs_clone_voice),
    #("ai_elevenlabs_delete_voice", wrap_elevenlabs_delete_voice),
    #("ai_hedra_create_avatar", wrap_hedra_create_avatar),
    #("ai_hedra_get_status", wrap_hedra_get_status),
    #("ai_hedra_list_jobs", wrap_hedra_list_jobs),
    #("ai_bfl_generate_image", wrap_bfl_generate_image),
    #("ai_bfl_get_result", wrap_bfl_get_result),
    #("ai_kling_create_video", wrap_kling_create_video),
    #("ai_kling_image_to_video", wrap_kling_image_to_video),
    #("ai_kling_get_task", wrap_kling_get_task),
    #("ai_kling_list_tasks", wrap_kling_list_tasks),
    #("ai_heygen_create_video", wrap_heygen_create_video),
    #("ai_heygen_list_avatars", wrap_heygen_list_avatars),
    #("ai_heygen_get_video_status", wrap_heygen_get_video_status),
    #("ai_heygen_list_voices", wrap_heygen_list_voices),
    // KIE.ai (Veo3)
    #("ai_kieai_create_video", wrap_kieai_create_video),
    #("ai_kieai_get_status", wrap_kieai_get_status),
    #("ai_kieai_list_videos", wrap_kieai_list_videos),
    // FAL.ai (NeuroPhoto, Nano Banana, Flux Kontext)
    #("ai_fal_neuro_photo", wrap_fal_neuro_photo),
    #("ai_fal_nano_banana", wrap_fal_nano_banana),
    #("ai_fal_flux_kontext", wrap_fal_flux_kontext),
    #("ai_fal_get_status", wrap_fal_get_status),
    #("ai_fal_get_result", wrap_fal_get_result),
    // Replicate LipSync, Morphing & Face Swap
    #("ai_replicate_lipsync", wrap_replicate_lipsync),
    #("ai_replicate_morphing", wrap_replicate_morphing),
    #("ai_replicate_faceswap", wrap_replicate_faceswap),
    // OpenAI Tools (Whisper, Vision, Improve Prompt)
    #("ai_openai_transcribe", wrap_openai_transcribe),
    #("ai_openai_vision", wrap_openai_vision),
    #("ai_openai_improve_prompt", wrap_openai_improve_prompt),
    // Replicate Upscaler & LoRA Training
    #("ai_replicate_upscale", wrap_replicate_upscale),
    #("ai_replicate_train_lora", wrap_replicate_train_lora),
    // Video Editing (FFmpeg)
    #("video_concat", wrap_video_concat),
    #("video_add_audio", wrap_video_add_audio),
    #("video_watermark", wrap_video_watermark),
    #("video_trim", wrap_video_trim),
    #("video_info", wrap_video_info),
    #("video_extract_audio", wrap_video_extract_audio),
    // B-Roll Generation
    #("broll_generate", wrap_broll_generate),
    #("broll_list_templates", wrap_broll_list_templates),
  ]
}

// ============================================================
// Wrapper functions for MCP tool handlers
// ============================================================

fn wrap_elevenlabs_tts(args: json.Json) -> ToolResult {
  let text = json_get_string(args, "text")
  let voice_id = json_get_optional_string(args, "voice_id")
  case text {
    Ok(t) -> json_to_result(handle_elevenlabs_tts(t, voice_id))
    Error(_) -> protocol.error_result("Missing required field: text")
  }
}

fn wrap_elevenlabs_list_voices(args: json.Json) -> ToolResult {
  let cached = cache.with_cache("ai_elevenlabs_list_voices", args, fn() {
    json.to_string(handle_elevenlabs_list_voices())
  })
  protocol.text_result(cached)
}

fn wrap_elevenlabs_clone_voice(args: json.Json) -> ToolResult {
  let name = json_get_string(args, "name")
  let audio_url = json_get_string(args, "audio_url")
  let description = json_get_optional_string(args, "description")
  case name, audio_url {
    Ok(n), Ok(a) -> json_to_result(handle_elevenlabs_clone_voice(n, a, description))
    _, _ -> protocol.error_result("Missing required fields: name, audio_url")
  }
}

fn wrap_elevenlabs_delete_voice(args: json.Json) -> ToolResult {
  let voice_id = json_get_string(args, "voice_id")
  case voice_id {
    Ok(v) -> json_to_result(handle_elevenlabs_delete_voice(v))
    Error(_) -> protocol.error_result("Missing required field: voice_id")
  }
}

fn wrap_hedra_create_avatar(args: json.Json) -> ToolResult {
  let audio_url = json_get_string(args, "audio_url")
  let image_url = json_get_string(args, "image_url")
  case audio_url, image_url {
    Ok(a), Ok(i) -> json_to_result(handle_hedra_create_avatar(a, i))
    _, _ -> protocol.error_result("Missing required fields: audio_url, image_url")
  }
}

fn wrap_hedra_get_status(args: json.Json) -> ToolResult {
  let job_id = json_get_string(args, "job_id")
  case job_id {
    Ok(j) -> {
      let cached = cache.with_cache("ai_hedra_get_status", args, fn() {
        json.to_string(handle_hedra_get_status(j))
      })
      protocol.text_result(cached)
    }
    Error(_) -> protocol.error_result("Missing required field: job_id")
  }
}

fn wrap_hedra_list_jobs(args: json.Json) -> ToolResult {
  let cached = cache.with_cache("ai_hedra_list_jobs", args, fn() {
    json.to_string(handle_hedra_list_jobs())
  })
  protocol.text_result(cached)
}

fn wrap_bfl_generate_image(args: json.Json) -> ToolResult {
  let prompt = json_get_string(args, "prompt")
  case prompt {
    Ok(p) -> json_to_result(handle_bfl_generate_image(p))
    Error(_) -> protocol.error_result("Missing required field: prompt")
  }
}

fn wrap_bfl_get_result(args: json.Json) -> ToolResult {
  let task_id = json_get_string(args, "task_id")
  case task_id {
    Ok(t) -> {
      let cached = cache.with_cache("ai_bfl_get_result", args, fn() {
        json.to_string(handle_bfl_get_result(t))
      })
      protocol.text_result(cached)
    }
    Error(_) -> protocol.error_result("Missing required field: task_id")
  }
}

fn wrap_kling_create_video(args: json.Json) -> ToolResult {
  let prompt = json_get_string(args, "prompt")
  case prompt {
    Ok(p) -> {
      let duration = json_get_optional_string(args, "duration")
      let mode = json_get_optional_string(args, "mode")
      let aspect_ratio = json_get_optional_string(args, "aspect_ratio")
      json_to_result(handle_kling_create_video(p, duration, mode, aspect_ratio))
    }
    Error(_) -> protocol.error_result("Missing required field: prompt")
  }
}

fn wrap_kling_image_to_video(args: json.Json) -> ToolResult {
  let image_url = json_get_string(args, "image_url")
  case image_url {
    Ok(i) -> {
      let prompt = json_get_optional_string(args, "prompt")
      let duration = json_get_optional_string(args, "duration")
      json_to_result(handle_kling_image_to_video(i, prompt, duration))
    }
    Error(_) -> protocol.error_result("Missing required field: image_url")
  }
}

fn wrap_kling_get_task(args: json.Json) -> ToolResult {
  let task_id = json_get_string(args, "task_id")
  case task_id {
    Ok(t) -> {
      let cached = cache.with_cache("ai_kling_get_task", args, fn() {
        json.to_string(handle_kling_get_task(t))
      })
      protocol.text_result(cached)
    }
    Error(_) -> protocol.error_result("Missing required field: task_id")
  }
}

fn wrap_kling_list_tasks(args: json.Json) -> ToolResult {
  let cached = cache.with_cache("ai_kling_list_tasks", args, fn() {
    json.to_string(handle_kling_list_tasks())
  })
  protocol.text_result(cached)
}

fn wrap_heygen_create_video(args: json.Json) -> ToolResult {
  let avatar_id = json_get_string(args, "avatar_id")
  let script = json_get_string(args, "script")
  case avatar_id, script {
    Ok(a), Ok(s) -> json_to_result(handle_heygen_create_video(a, s))
    _, _ -> protocol.error_result("Missing required fields: avatar_id, script")
  }
}

fn wrap_heygen_list_avatars(args: json.Json) -> ToolResult {
  let cached = cache.with_cache("ai_heygen_list_avatars", args, fn() {
    json.to_string(handle_heygen_list_avatars())
  })
  protocol.text_result(cached)
}

fn wrap_heygen_get_video_status(args: json.Json) -> ToolResult {
  let video_id = json_get_string(args, "video_id")
  case video_id {
    Ok(v) -> {
      let cached = cache.with_cache("ai_heygen_get_video_status", args, fn() {
        json.to_string(handle_heygen_get_video_status(v))
      })
      protocol.text_result(cached)
    }
    Error(_) -> protocol.error_result("Missing required field: video_id")
  }
}

fn wrap_heygen_list_voices(args: json.Json) -> ToolResult {
  let cached = cache.with_cache("ai_heygen_list_voices", args, fn() {
    json.to_string(handle_heygen_list_voices())
  })
  protocol.text_result(cached)
}

// ============================================================
// KIE.ai Wrapper Functions
// ============================================================

fn wrap_kieai_create_video(args: json.Json) -> ToolResult {
  let prompt = json_get_string(args, "prompt")
  let model = json_get_optional_string(args, "model")
  let aspect_ratio = json_get_optional_string(args, "aspect_ratio")
  let seeds = json_get_optional_int(args, "seeds")
  case prompt {
    Ok(p) -> json_to_result(handle_kieai_create_video(p, model, aspect_ratio, seeds))
    Error(_) -> protocol.error_result("Missing required field: prompt")
  }
}

fn wrap_kieai_get_status(args: json.Json) -> ToolResult {
  let task_id = json_get_string(args, "task_id")
  case task_id {
    Ok(t) -> {
      let cached = cache.with_cache("ai_kieai_get_status", args, fn() {
        json.to_string(handle_kieai_get_status(t))
      })
      protocol.text_result(cached)
    }
    Error(_) -> protocol.error_result("Missing required field: task_id")
  }
}

fn wrap_kieai_list_videos(args: json.Json) -> ToolResult {
  let limit = case json_get_optional_int(args, "limit") {
    Some(l) -> l
    None -> 20
  }
  let offset = case json_get_optional_int(args, "offset") {
    Some(o) -> o
    None -> 0
  }
  json_to_result(handle_kieai_list_videos(limit, offset))
}

// ============================================================
// FAL.ai Wrapper Functions
// ============================================================

fn wrap_fal_neuro_photo(args: json.Json) -> ToolResult {
  let prompt = json_get_string(args, "prompt")
  let lora_url = json_get_string(args, "lora_url")
  let num_images = json_get_optional_int(args, "num_images")
  let width = json_get_optional_int(args, "width")
  let height = json_get_optional_int(args, "height")
  let seed = json_get_optional_int(args, "seed")
  let guidance_scale = json_get_optional_float(args, "guidance_scale")
  let num_inference_steps = json_get_optional_int(args, "num_inference_steps")
  case prompt, lora_url {
    Ok(p), Ok(l) ->
      json_to_result(handle_fal_neuro_photo(
        p,
        l,
        num_images,
        width,
        height,
        seed,
        guidance_scale,
        num_inference_steps,
      ))
    _, _ -> protocol.error_result("Missing required fields: prompt, lora_url")
  }
}

fn wrap_fal_nano_banana(args: json.Json) -> ToolResult {
  let prompt = json_get_string(args, "prompt")
  let num_images = json_get_optional_int(args, "num_images")
  let aspect_ratio = json_get_optional_string(args, "aspect_ratio")
  let resolution = json_get_optional_string(args, "resolution")
  let seed = json_get_optional_int(args, "seed")
  case prompt {
    Ok(p) ->
      json_to_result(handle_fal_nano_banana(
        p,
        num_images,
        aspect_ratio,
        resolution,
        seed,
      ))
    Error(_) -> protocol.error_result("Missing required field: prompt")
  }
}

fn wrap_fal_flux_kontext(args: json.Json) -> ToolResult {
  let prompt = json_get_string(args, "prompt")
  let input_image_url = json_get_string(args, "input_image_url")
  let model_type = json_get_optional_string(args, "model_type")
  let aspect_ratio = json_get_optional_string(args, "aspect_ratio")
  let seed = json_get_optional_int(args, "seed")
  case prompt, input_image_url {
    Ok(p), Ok(i) ->
      json_to_result(handle_fal_flux_kontext(
        p,
        i,
        model_type,
        aspect_ratio,
        seed,
      ))
    _, _ ->
      protocol.error_result("Missing required fields: prompt, input_image_url")
  }
}

fn wrap_fal_get_status(args: json.Json) -> ToolResult {
  let request_id = json_get_string(args, "request_id")
  case request_id {
    Ok(r) -> json_to_result(handle_fal_get_status(r))
    Error(_) -> protocol.error_result("Missing required field: request_id")
  }
}

fn wrap_fal_get_result(args: json.Json) -> ToolResult {
  let request_id = json_get_string(args, "request_id")
  case request_id {
    Ok(r) -> json_to_result(handle_fal_get_result(r))
    Error(_) -> protocol.error_result("Missing required field: request_id")
  }
}

// ============================================================
// Replicate LipSync & Morphing Wrapper Functions
// ============================================================

fn wrap_replicate_lipsync(args: json.Json) -> ToolResult {
  let video_url = json_get_string(args, "video_url")
  let audio_url = json_get_string(args, "audio_url")
  case video_url, audio_url {
    Ok(v), Ok(a) -> json_to_result(handle_replicate_lipsync(v, a))
    _, _ ->
      protocol.error_result("Missing required fields: video_url, audio_url")
  }
}

fn wrap_replicate_morphing(args: json.Json) -> ToolResult {
  let start_image_url = json_get_string(args, "start_image_url")
  let end_image_url = json_get_string(args, "end_image_url")
  let prompt = json_get_optional_string(args, "prompt")
  let duration = json_get_optional_string(args, "duration")
  case start_image_url, end_image_url {
    Ok(s), Ok(e) ->
      json_to_result(handle_replicate_morphing(s, e, prompt, duration))
    _, _ ->
      protocol.error_result(
        "Missing required fields: start_image_url, end_image_url",
      )
  }
}

// ============================================================
// Replicate Face Swap Handler
// ============================================================

/// Handle face swap using Replicate codeplugtech/face-swap model
pub fn handle_replicate_faceswap(
  target_image_url: String,
  swap_image_url: String,
) -> json.Json {
  let api_key = config.get_env("REPLICATE_API_TOKEN")
  case api_key {
    "" -> not_configured("Replicate")
    key -> {
      let conf = replicate.default_config(key)
      let input = [
        #("input_image", json.string(target_image_url)),
        #("swap_image", json.string(swap_image_url)),
      ]
      let req =
        replicate.create_prediction_request(
          conf,
          replicate.PredictionRequest(
            model: "codeplugtech/face-swap",
            version: Some("278a81e7ebb22db98bcba54de985d22cc1abeead2754eb1f2af717247be69b34"),
            input: input,
          ),
        )
      let client_req =
        client.Request(
          url: req.url,
          method: req.method,
          headers: req.headers,
          body: req.body,
        )
      case client.execute_json(client_req) {
        Ok(body) ->
          success_response(json.object([#("prediction", json.string(body))]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

fn wrap_replicate_faceswap(args: json.Json) -> ToolResult {
  let target_image_url = json_get_string(args, "target_image_url")
  let swap_image_url = json_get_string(args, "swap_image_url")
  case target_image_url, swap_image_url {
    Ok(t), Ok(s) -> json_to_result(handle_replicate_faceswap(t, s))
    _, _ ->
      protocol.error_result(
        "Missing required fields: target_image_url, swap_image_url",
      )
  }
}

// ============================================================
// OpenAI Transcribe (Whisper) Handler
// ============================================================

/// Transcribe audio using OpenAI Whisper API
pub fn handle_openai_transcribe(
  audio_url: String,
  _language: Option(String),
  _response_format: Option(String),
) -> json.Json {
  // Note: Whisper requires multipart/form-data with file upload
  // For URL-based audio, we use Replicate Whisper model
  let replicate_key = config.get_env("REPLICATE_API_TOKEN")
  case replicate_key {
    "" -> not_configured("Replicate (for Whisper)")
    rkey -> {
      let client_req = client.Request(
        url: "https://api.replicate.com/v1/predictions",
        method: "POST",
        headers: [
          #("Authorization", "Bearer " <> rkey),
          #("Content-Type", "application/json"),
          #("Prefer", "wait"),
        ],
        body: json.to_string(json.object([
          #("model", json.string("openai/whisper")),
          #("input", json.object([
            #("audio", json.string(audio_url)),
          ])),
        ])),
      )
      case client.execute_json(client_req) {
        Ok(body) ->
          success_response(json.object([#("transcription", json.string(body))]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

fn wrap_openai_transcribe(args: json.Json) -> ToolResult {
  let audio_url = json_get_string(args, "audio_url")
  let language = json_get_optional_string(args, "language")
  let response_format = json_get_optional_string(args, "response_format")
  case audio_url {
    Ok(a) -> json_to_result(handle_openai_transcribe(a, language, response_format))
    Error(_) -> protocol.error_result("Missing required field: audio_url")
  }
}

// ============================================================
// OpenAI Vision (Image to Prompt) Handler
// ============================================================

/// Describe an image using OpenAI Vision API (GPT-4o)
pub fn handle_openai_vision(
  image_url: String,
  _prompt: Option(String),
) -> json.Json {
  let api_key = config.get_env("OPENAI_API_KEY")
  case api_key {
    "" -> not_configured("OpenAI")
    key -> {
      let conf = openai.default_config(key)
      let req = openai.create_image_to_prompt_request(conf, image_url)
      let client_req = client.Request(
        url: req.url,
        method: req.method,
        headers: req.headers,
        body: req.body,
      )
      case client.execute_json(client_req) {
        Ok(body) ->
          success_response(json.object([#("description", json.string(body))]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

fn wrap_openai_vision(args: json.Json) -> ToolResult {
  let image_url = json_get_string(args, "image_url")
  let prompt = json_get_optional_string(args, "prompt")
  case image_url {
    Ok(i) -> json_to_result(handle_openai_vision(i, prompt))
    Error(_) -> protocol.error_result("Missing required field: image_url")
  }
}

// ============================================================
// OpenAI Improve Prompt Handler
// ============================================================

/// Improve/enhance a prompt for AI generation using GPT
pub fn handle_openai_improve_prompt(
  original_prompt: String,
  style: Option(String),
) -> json.Json {
  let api_key = config.get_env("OPENAI_API_KEY")
  case api_key {
    "" -> not_configured("OpenAI")
    key -> {
      let conf = openai.default_config(key)
      let req = openai.create_improve_prompt_request(conf, original_prompt, style)
      let client_req = client.Request(
        url: req.url,
        method: req.method,
        headers: req.headers,
        body: req.body,
      )
      case client.execute_json(client_req) {
        Ok(body) ->
          success_response(json.object([#("improved_prompt", json.string(body))]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

fn wrap_openai_improve_prompt(args: json.Json) -> ToolResult {
  let prompt = json_get_string(args, "prompt")
  let style = json_get_optional_string(args, "style")
  case prompt {
    Ok(p) -> json_to_result(handle_openai_improve_prompt(p, style))
    Error(_) -> protocol.error_result("Missing required field: prompt")
  }
}

// ============================================================
// Replicate Upscale (Clarity Upscaler) Handler
// ============================================================

/// Upscale image using Clarity Upscaler via Replicate
pub fn handle_replicate_upscale(
  image_url: String,
  scale: Option(Int),
  creativity: Option(Float),
) -> json.Json {
  let api_key = config.get_env("REPLICATE_API_TOKEN")
  case api_key {
    "" -> not_configured("Replicate")
    key -> {
      let conf = replicate.default_config(key)
      let scale_value = case scale {
        Some(s) -> s
        None -> 2
      }
      let creativity_value = case creativity {
        Some(c) -> c
        None -> 0.35
      }
      let input = [
        #("image", json.string(image_url)),
        #("upscale", json.int(scale_value)),
        #("creativity", json.float(creativity_value)),
        #("resemblance", json.float(0.6)),
        #("output_format", json.string("png")),
      ]
      let req = replicate.create_prediction_request(
        conf,
        replicate.PredictionRequest(
          model: "philz1337x/clarity-upscaler",
          version: None,
          input: input,
        ),
      )
      let client_req = client.Request(
        url: req.url,
        method: req.method,
        headers: req.headers,
        body: req.body,
      )
      case client.execute_json(client_req) {
        Ok(body) ->
          success_response(json.object([#("prediction", json.string(body))]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

fn wrap_replicate_upscale(args: json.Json) -> ToolResult {
  let image_url = json_get_string(args, "image_url")
  let scale = json_get_optional_int(args, "scale")
  let creativity = json_get_optional_float(args, "creativity")
  case image_url {
    Ok(i) -> json_to_result(handle_replicate_upscale(i, scale, creativity))
    Error(_) -> protocol.error_result("Missing required field: image_url")
  }
}

// ============================================================
// Replicate Train LoRA Handler
// ============================================================

/// Train a LoRA model using Replicate Training API
pub fn handle_replicate_train_lora(
  images_zip_url: String,
  trigger_word: String,
  model_name: String,
  steps: Option(Int),
) -> json.Json {
  let api_key = config.get_env("REPLICATE_API_TOKEN")
  case api_key {
    "" -> not_configured("Replicate")
    key -> {
      let steps_value = case steps {
        Some(s) -> s
        None -> 1000
      }
      // Use the FLUX LoRA training model
      let body = json.to_string(json.object([
        #("destination", json.string("vibee/" <> model_name)),
        #("input", json.object([
          #("input_images", json.string(images_zip_url)),
          #("trigger_word", json.string(trigger_word)),
          #("steps", json.int(steps_value)),
          #("lora_rank", json.int(16)),
          #("optimizer", json.string("adamw8bit")),
          #("batch_size", json.int(1)),
          #("resolution", json.string("512,768,1024")),
          #("autocaption", json.bool(True)),
          #("autocaption_prefix", json.string(trigger_word <> ", ")),
        ])),
      ]))
      let client_req = client.Request(
        url: "https://api.replicate.com/v1/models/ostris/flux-dev-lora-trainer/versions/d995297071a44dcb72244e6c19462111649ec86a9646c32df56daa7f14801944/trainings",
        method: "POST",
        headers: [
          #("Authorization", "Bearer " <> key),
          #("Content-Type", "application/json"),
        ],
        body: body,
      )
      case client.execute_json(client_req) {
        Ok(body) ->
          success_response(json.object([#("training", json.string(body))]))
        Error(e) -> error_response(client.error_to_string(e))
      }
    }
  }
}

fn wrap_replicate_train_lora(args: json.Json) -> ToolResult {
  let images_zip_url = json_get_string(args, "images_zip_url")
  let trigger_word = json_get_string(args, "trigger_word")
  let model_name = json_get_string(args, "model_name")
  let steps = json_get_optional_int(args, "steps")
  case images_zip_url, trigger_word, model_name {
    Ok(i), Ok(t), Ok(m) ->
      json_to_result(handle_replicate_train_lora(i, t, m, steps))
    _, _, _ ->
      protocol.error_result(
        "Missing required fields: images_zip_url, trigger_word, model_name",
      )
  }
}

/// Convert JSON response to ToolResult
fn json_to_result(response: json.Json) -> ToolResult {
  ToolResult(
    content: [TextContent(text: json.to_string(response))],
    is_error: False,
  )
}

// ============================================================
// Main Handler Router
// ============================================================

pub fn handle_ai_tool(tool_name: String, _args: json.Json) -> json.Json {
  // For now, return placeholder - full implementation requires JSON arg parsing
  case tool_name {
    "ai_elevenlabs_tts" -> error_response("Use handle_elevenlabs_tts directly")
    "ai_elevenlabs_list_voices" -> handle_elevenlabs_list_voices()
    "ai_openai_chat" -> error_response("Use handle_openai_chat directly")
    "ai_openai_embedding" -> error_response("Use handle_openai_embedding directly")
    "ai_replicate_run" -> error_response("Use handle_replicate_run directly")
    "ai_replicate_get_prediction" -> error_response("Use handle_replicate_get_prediction directly")
    "ai_s3_upload" -> error_response("Use handle_s3_upload directly")
    "ai_s3_download" -> error_response("Use handle_s3_download directly")
    "ai_bfl_generate_image" -> error_response("Use handle_bfl_generate_image directly")
    "ai_bfl_get_result" -> error_response("Use handle_bfl_get_result directly")
    "ai_hedra_create_avatar" -> error_response("Use handle_hedra_create_avatar directly")
    "ai_hedra_get_status" -> error_response("Use handle_hedra_get_status directly")
    "ai_kling_create_video" -> error_response("Use handle_kling_create_video directly")
    "ai_kling_get_task" -> error_response("Use handle_kling_get_task directly")
    "ai_heygen_create_video" -> error_response("Use handle_heygen_create_video directly")
    "ai_heygen_list_avatars" -> handle_heygen_list_avatars()
    _ -> error_response("Unknown AI tool: " <> tool_name)
  }
}
