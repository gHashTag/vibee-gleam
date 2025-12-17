// Gemini Client for VIBEE
// Multimodal analysis using Google Gemini 2.5 Pro Preview API
// Supports: images, videos, audio transcription

import gleam/dynamic/decode.{type Decoder}
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile
import vibee/mcp/config

// =============================================================================
// Types
// =============================================================================

/// Gemini configuration
pub type GeminiConfig {
  GeminiConfig(
    api_key: String,
    model: String,
    max_output_tokens: Int,
  )
}

/// Analysis result from Gemini
pub type AnalysisResult {
  AnalysisResult(
    description: String,
    transcription: Option(String),
    ocr_text: Option(String),
    detected_objects: List(String),
    emotions: Option(String),
    key_phrases: List(String),
    timestamps: List(TimestampEvent),
    speakers: List(String),
  )
}

/// Timestamp event for video analysis
pub type TimestampEvent {
  TimestampEvent(
    time_seconds: Int,
    description: String,
  )
}

/// Gemini error types
pub type GeminiError {
  GeminiApiError(String)
  GeminiFileError(String)
  GeminiRateLimited
  GeminiInvalidResponse(String)
  GeminiUnsupportedMedia(String)
}

/// File upload result from Gemini Files API
pub type FileUploadResult {
  FileUploadResult(
    uri: String,
    mime_type: String,
    size_bytes: Int,
    state: String,
  )
}

// =============================================================================
// Configuration
// =============================================================================

/// Default Gemini configuration
pub fn default_config() -> GeminiConfig {
  GeminiConfig(
    api_key: config.get_env("GEMINI_API_KEY"),
    model: "gemini-2.5-pro-preview-06-05",
    max_output_tokens: 8192,
  )
}

/// Create config with custom model
pub fn with_model(cfg: GeminiConfig, model: String) -> GeminiConfig {
  GeminiConfig(..cfg, model: model)
}

/// Check if config is valid
pub fn is_configured(cfg: GeminiConfig) -> Bool {
  cfg.api_key != ""
}

// =============================================================================
// Image Analysis
// =============================================================================

/// Analyze an image file
pub fn analyze_image(
  file_path: String,
  context: String,
  cfg: GeminiConfig,
) -> Result(AnalysisResult, GeminiError) {
  case is_configured(cfg) {
    False -> Error(GeminiApiError("GEMINI_API_KEY not set"))
    True -> {
      // Read file and convert to base64
      case read_file_base64(file_path) {
        Error(e) -> Error(GeminiFileError("Cannot read file: " <> e))
        Ok(#(base64_data, mime_type)) -> {
          let prompt = build_image_prompt(context)
          call_gemini_with_inline_data(base64_data, mime_type, prompt, cfg)
        }
      }
    }
  }
}

/// Build image analysis prompt
fn build_image_prompt(context: String) -> String {
  "Analyze this image in detail. " <> case context {
    "" -> ""
    ctx -> "Context: " <> ctx <> ". "
  } <> "
Provide a JSON response with:
{
  \"description\": \"detailed description of what you see\",
  \"ocr_text\": \"any text visible in the image or null\",
  \"detected_objects\": [\"list\", \"of\", \"objects\"],
  \"emotions\": \"emotional tone if people are present or null\",
  \"key_phrases\": [\"relevant\", \"keywords\"]
}
Only respond with valid JSON, no additional text."
}

// =============================================================================
// Audio Analysis (Transcription)
// =============================================================================

/// Transcribe audio file
pub fn analyze_audio(
  file_path: String,
  language: String,
  cfg: GeminiConfig,
) -> Result(AnalysisResult, GeminiError) {
  case is_configured(cfg) {
    False -> Error(GeminiApiError("GEMINI_API_KEY not set"))
    True -> {
      // For audio, we may need to use Files API for large files
      case get_file_size(file_path) {
        Error(e) -> Error(GeminiFileError("Cannot get file size: " <> e))
        Ok(size) -> {
          case size > 20_000_000 {
            // > 20MB, use Files API
            True -> analyze_audio_large(file_path, language, cfg)
            False -> {
              // Small file, inline
              case read_file_base64(file_path) {
                Error(e) -> Error(GeminiFileError("Cannot read file: " <> e))
                Ok(#(base64_data, mime_type)) -> {
                  let prompt = build_audio_prompt(language)
                  call_gemini_audio(base64_data, mime_type, prompt, cfg)
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Build audio transcription prompt
fn build_audio_prompt(language: String) -> String {
  let lang_hint = case language {
    "" -> ""
    lang -> "The audio is in " <> lang <> ". "
  }
  lang_hint <> "Transcribe this audio message accurately.
Provide a JSON response with:
{
  \"transcription\": \"full text transcription\",
  \"speakers\": [\"speaker names/labels if multiple\"],
  \"emotions\": \"emotional tone of the speaker(s)\",
  \"key_phrases\": [\"important\", \"keywords\"]
}
Only respond with valid JSON, no additional text."
}

/// Analyze large audio file using Files API
fn analyze_audio_large(
  file_path: String,
  language: String,
  cfg: GeminiConfig,
) -> Result(AnalysisResult, GeminiError) {
  // Upload file first
  case upload_file(file_path, cfg) {
    Error(e) -> Error(e)
    Ok(upload_result) -> {
      let prompt = build_audio_prompt(language)
      call_gemini_with_file_uri(upload_result.uri, upload_result.mime_type, prompt, cfg)
    }
  }
}

// =============================================================================
// Video Analysis
// =============================================================================

/// Analyze video file
pub fn analyze_video(
  file_path: String,
  context: String,
  cfg: GeminiConfig,
) -> Result(AnalysisResult, GeminiError) {
  case is_configured(cfg) {
    False -> Error(GeminiApiError("GEMINI_API_KEY not set"))
    True -> {
      // Videos are usually large, use Files API
      case upload_file(file_path, cfg) {
        Error(e) -> Error(e)
        Ok(upload_result) -> {
          let prompt = build_video_prompt(context)
          call_gemini_with_file_uri(upload_result.uri, upload_result.mime_type, prompt, cfg)
        }
      }
    }
  }
}

/// Build video analysis prompt
fn build_video_prompt(context: String) -> String {
  "Analyze this video in detail. " <> case context {
    "" -> ""
    ctx -> "Context: " <> ctx <> ". "
  } <> "
Provide a JSON response with:
{
  \"description\": \"overall description of the video content\",
  \"transcription\": \"any speech in the video or null\",
  \"detected_objects\": [\"objects\", \"appearing\", \"in\", \"video\"],
  \"emotions\": \"emotional tone if people are present\",
  \"key_phrases\": [\"important\", \"keywords\"],
  \"timestamps\": [
    {\"time_seconds\": 0, \"description\": \"what happens at this time\"}
  ]
}
Only respond with valid JSON, no additional text."
}

// =============================================================================
// Gemini API Calls
// =============================================================================

/// Call Gemini with inline base64 data
fn call_gemini_with_inline_data(
  base64_data: String,
  mime_type: String,
  prompt: String,
  cfg: GeminiConfig,
) -> Result(AnalysisResult, GeminiError) {
  let url = "https://generativelanguage.googleapis.com/v1beta/models/"
    <> cfg.model
    <> ":generateContent?key="
    <> cfg.api_key

  let body = json.object([
    #("contents", json.array([
      json.object([
        #("parts", json.array([
          json.object([
            #("inline_data", json.object([
              #("mime_type", json.string(mime_type)),
              #("data", json.string(base64_data)),
            ])),
          ]),
          json.object([
            #("text", json.string(prompt)),
          ]),
        ], fn(x) { x })),
      ]),
    ], fn(x) { x })),
    #("generationConfig", json.object([
      #("maxOutputTokens", json.int(cfg.max_output_tokens)),
      #("temperature", json.float(0.1)),
    ])),
  ])
  |> json.to_string

  case https_post(url, body) {
    Error(e) -> Error(GeminiApiError("Request failed: " <> e))
    Ok(response) -> parse_gemini_response(response)
  }
}

/// Call Gemini for audio with inline data
fn call_gemini_audio(
  base64_data: String,
  mime_type: String,
  prompt: String,
  cfg: GeminiConfig,
) -> Result(AnalysisResult, GeminiError) {
  // Same as inline data call
  call_gemini_with_inline_data(base64_data, mime_type, prompt, cfg)
}

/// Call Gemini with file URI (for large files)
fn call_gemini_with_file_uri(
  file_uri: String,
  mime_type: String,
  prompt: String,
  cfg: GeminiConfig,
) -> Result(AnalysisResult, GeminiError) {
  let url = "https://generativelanguage.googleapis.com/v1beta/models/"
    <> cfg.model
    <> ":generateContent?key="
    <> cfg.api_key

  let body = json.object([
    #("contents", json.array([
      json.object([
        #("parts", json.array([
          json.object([
            #("file_data", json.object([
              #("mime_type", json.string(mime_type)),
              #("file_uri", json.string(file_uri)),
            ])),
          ]),
          json.object([
            #("text", json.string(prompt)),
          ]),
        ], fn(x) { x })),
      ]),
    ], fn(x) { x })),
    #("generationConfig", json.object([
      #("maxOutputTokens", json.int(cfg.max_output_tokens)),
      #("temperature", json.float(0.1)),
    ])),
  ])
  |> json.to_string

  case https_post(url, body) {
    Error(e) -> Error(GeminiApiError("Request failed: " <> e))
    Ok(response) -> parse_gemini_response(response)
  }
}

// =============================================================================
// Files API (for large files > 20MB)
// =============================================================================

/// Upload a file to Gemini Files API
pub fn upload_file(
  file_path: String,
  cfg: GeminiConfig,
) -> Result(FileUploadResult, GeminiError) {
  case is_configured(cfg) {
    False -> Error(GeminiApiError("GEMINI_API_KEY not set"))
    True -> {
      case read_file_base64(file_path) {
        Error(e) -> Error(GeminiFileError("Cannot read file: " <> e))
        Ok(#(base64_data, mime_type)) -> {
          // Start resumable upload
          let url = "https://generativelanguage.googleapis.com/upload/v1beta/files?key="
            <> cfg.api_key

          let metadata = json.object([
            #("file", json.object([
              #("display_name", json.string(get_filename(file_path))),
            ])),
          ])
          |> json.to_string

          // For simplicity, we'll use a single POST with the file data
          // In production, you'd use resumable upload for very large files
          case upload_file_simple(url, base64_data, mime_type, metadata, cfg) {
            Error(e) -> Error(e)
            Ok(result) -> Ok(result)
          }
        }
      }
    }
  }
}

/// Simple file upload (for files that fit in memory)
fn upload_file_simple(
  url: String,
  base64_data: String,
  mime_type: String,
  _metadata: String,
  _cfg: GeminiConfig,
) -> Result(FileUploadResult, GeminiError) {
  // Create multipart body (simplified - in practice you'd use proper multipart)
  let body = json.object([
    #("file", json.object([
      #("mimeType", json.string(mime_type)),
      #("data", json.string(base64_data)),
    ])),
  ])
  |> json.to_string

  case https_post(url, body) {
    Error(e) -> Error(GeminiApiError("Upload failed: " <> e))
    Ok(response) -> parse_upload_response(response)
  }
}

// =============================================================================
// Response Parsing
// =============================================================================

/// Parse Gemini generateContent response
fn parse_gemini_response(response: String) -> Result(AnalysisResult, GeminiError) {
  // First extract the text from Gemini response
  let text_decoder = {
    use candidates <- decode.field("candidates", decode.list({
      use content <- decode.field("content", {
        use parts <- decode.field("parts", decode.list({
          use text <- decode.field("text", decode.string)
          decode.success(text)
        }))
        decode.success(parts)
      })
      decode.success(content)
    }))
    decode.success(candidates)
  }

  case json.parse(response, text_decoder) {
    Error(_) -> {
      // Try to parse error response
      parse_gemini_error(response)
    }
    Ok([]) -> Error(GeminiInvalidResponse("Empty candidates"))
    Ok([[text, ..], ..]) -> {
      // Parse the JSON from the text
      parse_analysis_json(text)
    }
    Ok([[],..]) -> Error(GeminiInvalidResponse("Empty parts"))
    Ok(_) -> Error(GeminiInvalidResponse("Unexpected response structure"))
  }
}

/// Parse the JSON analysis result from text
fn parse_analysis_json(text: String) -> Result(AnalysisResult, GeminiError) {
  // Clean up the text - remove markdown code blocks if present
  let cleaned = text
    |> string.replace("```json", "")
    |> string.replace("```", "")
    |> string.trim

  // Try to parse as our expected format
  let result_decoder = {
    use description <- decode.field("description", decode.string)
    use transcription <- decode.optional_field("transcription", "", decode.string)
    use ocr_text <- decode.optional_field("ocr_text", "", decode.string)
    use detected_objects <- decode.optional_field("detected_objects", [], decode.list(decode.string))
    use emotions <- decode.optional_field("emotions", "", decode.string)
    use key_phrases <- decode.optional_field("key_phrases", [], decode.list(decode.string))
    use timestamps <- decode.optional_field("timestamps", [], decode.list(timestamp_decoder()))
    use speakers <- decode.optional_field("speakers", [], decode.list(decode.string))

    decode.success(AnalysisResult(
      description: description,
      transcription: string_to_option(transcription),
      ocr_text: string_to_option(ocr_text),
      detected_objects: detected_objects,
      emotions: string_to_option(emotions),
      key_phrases: key_phrases,
      timestamps: timestamps,
      speakers: speakers,
    ))
  }

  case json.parse(cleaned, result_decoder) {
    Ok(result) -> Ok(result)
    Error(_) -> {
      // If parsing fails, return a basic result with the raw text
      Ok(AnalysisResult(
        description: cleaned,
        transcription: None,
        ocr_text: None,
        detected_objects: [],
        emotions: None,
        key_phrases: [],
        timestamps: [],
        speakers: [],
      ))
    }
  }
}

/// Decoder for timestamp events
fn timestamp_decoder() -> Decoder(TimestampEvent) {
  use time_seconds <- decode.field("time_seconds", decode.int)
  use description <- decode.field("description", decode.string)
  decode.success(TimestampEvent(time_seconds: time_seconds, description: description))
}

/// Convert empty string to None
fn string_to_option(s: String) -> Option(String) {
  case s {
    "" -> None
    "null" -> None
    str -> Some(str)
  }
}

/// Parse Gemini error response
fn parse_gemini_error(response: String) -> Result(AnalysisResult, GeminiError) {
  let error_decoder = {
    use error <- decode.field("error", {
      use message <- decode.field("message", decode.string)
      use code <- decode.optional_field("code", 0, decode.int)
      decode.success(#(message, code))
    })
    decode.success(error)
  }

  case json.parse(response, error_decoder) {
    Ok(#(message, 429)) -> Error(GeminiRateLimited)
    Ok(#(message, _)) -> Error(GeminiApiError(message))
    Error(_) -> Error(GeminiInvalidResponse(string.slice(response, 0, 200)))
  }
}

/// Parse file upload response
fn parse_upload_response(response: String) -> Result(FileUploadResult, GeminiError) {
  let file_decoder = {
    use file <- decode.field("file", {
      use uri <- decode.field("uri", decode.string)
      use mime_type <- decode.field("mimeType", decode.string)
      use size_bytes <- decode.optional_field("sizeBytes", 0, decode.int)
      use state <- decode.field("state", decode.string)
      decode.success(FileUploadResult(
        uri: uri,
        mime_type: mime_type,
        size_bytes: size_bytes,
        state: state,
      ))
    })
    decode.success(file)
  }

  case json.parse(response, file_decoder) {
    Ok(result) -> Ok(result)
    Error(_) -> Error(GeminiInvalidResponse("Invalid upload response: " <> string.slice(response, 0, 200)))
  }
}

// =============================================================================
// File Utilities
// =============================================================================

/// Read file and convert to base64
fn read_file_base64(file_path: String) -> Result(#(String, String), String) {
  case simplifile.read_bits(file_path) {
    Error(_) -> Error("Cannot read file")
    Ok(bits) -> {
      let base64 = base64_encode(bits)
      let mime = get_mime_type(file_path)
      Ok(#(base64, mime))
    }
  }
}

/// Get file size
fn get_file_size(file_path: String) -> Result(Int, String) {
  case simplifile.file_info(file_path) {
    Error(_) -> Error("Cannot get file info")
    Ok(info) -> Ok(info.size)
  }
}

/// Get filename from path
fn get_filename(file_path: String) -> String {
  file_path
  |> string.split("/")
  |> list.last
  |> result.unwrap("file")
}

/// Determine MIME type from file extension
fn get_mime_type(file_path: String) -> String {
  let ext = file_path
    |> string.split(".")
    |> list.last
    |> result.unwrap("")
    |> string.lowercase

  case ext {
    "jpg" | "jpeg" -> "image/jpeg"
    "png" -> "image/png"
    "gif" -> "image/gif"
    "webp" -> "image/webp"
    "mp3" -> "audio/mpeg"
    "ogg" | "oga" -> "audio/ogg"
    "wav" -> "audio/wav"
    "m4a" -> "audio/mp4"
    "mp4" -> "video/mp4"
    "webm" -> "video/webm"
    "mov" -> "video/quicktime"
    "avi" -> "video/x-msvideo"
    _ -> "application/octet-stream"
  }
}

// =============================================================================
// HTTP Utilities
// =============================================================================

/// HTTPS POST request
fn https_post(url: String, body: String) -> Result(String, String) {
  // Parse URL for host/path
  let #(host, path_with_query) = parse_https_url(url)

  let req = request.new()
    |> request.set_scheme(http.Https)
    |> request.set_method(http.Post)
    |> request.set_host(host)
    |> request.set_path(path_with_query)
    |> request.set_header("Content-Type", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) -> Ok(response.body)
    Error(_) -> Error("HTTPS request failed")
  }
}

/// Parse HTTPS URL into host and path
fn parse_https_url(url: String) -> #(String, String) {
  let without_scheme = string.replace(url, "https://", "")

  case string.split_once(without_scheme, "/") {
    Ok(#(host, path)) -> #(host, "/" <> path)
    Error(_) -> #(without_scheme, "/")
  }
}

// =============================================================================
// Base64 Encoding (Erlang FFI)
// =============================================================================

@external(erlang, "base64", "encode")
fn base64_encode(data: BitArray) -> String

// =============================================================================
// Result Serialization
// =============================================================================

/// Convert analysis result to JSON
pub fn result_to_json(result: AnalysisResult) -> String {
  json.object([
    #("description", json.string(result.description)),
    #("transcription", option_to_json(result.transcription)),
    #("ocr_text", option_to_json(result.ocr_text)),
    #("detected_objects", json.array(result.detected_objects, json.string)),
    #("emotions", option_to_json(result.emotions)),
    #("key_phrases", json.array(result.key_phrases, json.string)),
    #("timestamps", json.array(result.timestamps, timestamp_to_json)),
    #("speakers", json.array(result.speakers, json.string)),
  ])
  |> json.to_string
}

fn option_to_json(opt: Option(String)) -> json.Json {
  case opt {
    Some(s) -> json.string(s)
    None -> json.null()
  }
}

fn timestamp_to_json(ts: TimestampEvent) -> json.Json {
  json.object([
    #("time_seconds", json.int(ts.time_seconds)),
    #("description", json.string(ts.description)),
  ])
}

/// Convert error to string
pub fn error_to_string(err: GeminiError) -> String {
  case err {
    GeminiApiError(msg) -> "API Error: " <> msg
    GeminiFileError(msg) -> "File Error: " <> msg
    GeminiRateLimited -> "Rate limited - please try again later"
    GeminiInvalidResponse(msg) -> "Invalid Response: " <> msg
    GeminiUnsupportedMedia(msg) -> "Unsupported Media: " <> msg
  }
}
