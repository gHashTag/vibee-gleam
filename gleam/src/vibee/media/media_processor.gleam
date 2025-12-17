// Media Processor for VIBEE
// Downloads and analyzes Telegram media using Gemini 2.5 Pro
// Saves results to PostgreSQL for RAG search

import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/llm/gemini_client.{
  type AnalysisResult, type GeminiConfig, type GeminiError,
}
import vibee/mcp/config

// =============================================================================
// Types
// =============================================================================

/// Media item from database
pub type MediaItem {
  MediaItem(
    id: Int,
    message_id: Int,
    dialog_id: Int,
    media_type: String,
    file_id: String,
    mime_type: String,
    file_size: Int,
    duration_seconds: Int,
  )
}

/// Download result from Go Bridge
pub type DownloadResult {
  DownloadResult(
    success: Bool,
    file_path: String,
    file_size: Int,
    mime_type: String,
    error: String,
  )
}

/// Processing result
pub type ProcessingResult {
  ProcessingResult(
    media_id: Int,
    success: Bool,
    transcription: Option(String),
    description: Option(String),
    ocr_text: Option(String),
    detected_objects: List(String),
    emotions: Option(String),
    key_phrases: List(String),
    error: Option(String),
  )
}

/// Batch processing result
pub type BatchResult {
  BatchResult(
    total: Int,
    processed: Int,
    failed: Int,
    results: List(ProcessingResult),
  )
}

/// Processing error
pub type ProcessingError {
  DownloadError(String)
  AnalysisError(String)
  DatabaseError(String)
  UnsupportedMediaType(String)
}

// =============================================================================
// Configuration
// =============================================================================

/// Get Go Bridge URL from environment
fn bridge_url() -> String {
  config.get_env_or("TELEGRAM_BRIDGE_URL", "http://localhost:8081")
}

/// Get Gemini config
fn gemini_config() -> GeminiConfig {
  gemini_client.default_config()
}

// =============================================================================
// Single Media Processing
// =============================================================================

/// Process a single media item
pub fn process_media(
  media: MediaItem,
  session_id: String,
) -> Result(ProcessingResult, ProcessingError) {
  // Download media from Telegram
  case download_media(media.dialog_id, media.message_id, session_id) {
    Error(e) -> Error(DownloadError(e))
    Ok(download) -> {
      case download.success {
        False -> Error(DownloadError(download.error))
        True -> {
          // Analyze based on media type
          let cfg = gemini_config()
          case analyze_media(download.file_path, media.media_type, cfg) {
            Error(e) -> Error(AnalysisError(gemini_client.error_to_string(e)))
            Ok(analysis) -> {
              // Clean up temp file
              delete_temp_file(download.file_path)

              // Create result
              Ok(ProcessingResult(
                media_id: media.id,
                success: True,
                transcription: analysis.transcription,
                description: Some(analysis.description),
                ocr_text: analysis.ocr_text,
                detected_objects: analysis.detected_objects,
                emotions: analysis.emotions,
                key_phrases: analysis.key_phrases,
                error: None,
              ))
            }
          }
        }
      }
    }
  }
}

/// Analyze media based on type
fn analyze_media(
  file_path: String,
  media_type: String,
  cfg: GeminiConfig,
) -> Result(AnalysisResult, GeminiError) {
  case media_type {
    "photo" -> gemini_client.analyze_image(file_path, "", cfg)
    "voice" | "audio" -> gemini_client.analyze_audio(file_path, "ru", cfg)
    "video" | "video_note" -> gemini_client.analyze_video(file_path, "", cfg)
    "document" -> {
      // Try to detect type from file extension
      let ext = get_file_extension(file_path)
      case ext {
        "jpg" | "jpeg" | "png" | "gif" | "webp" -> gemini_client.analyze_image(file_path, "", cfg)
        "mp4" | "mov" | "webm" | "avi" -> gemini_client.analyze_video(file_path, "", cfg)
        "mp3" | "ogg" | "wav" | "m4a" -> gemini_client.analyze_audio(file_path, "", cfg)
        _ -> Error(gemini_client.GeminiUnsupportedMedia("Unknown document type: " <> ext))
      }
    }
    _ -> Error(gemini_client.GeminiUnsupportedMedia(media_type))
  }
}

// =============================================================================
// Batch Processing
// =============================================================================

/// Process multiple media items
pub fn process_batch(
  items: List(MediaItem),
  session_id: String,
) -> BatchResult {
  let results = list.map(items, fn(item) {
    case process_media(item, session_id) {
      Ok(result) -> result
      Error(e) -> ProcessingResult(
        media_id: item.id,
        success: False,
        transcription: None,
        description: None,
        ocr_text: None,
        detected_objects: [],
        emotions: None,
        key_phrases: [],
        error: Some(error_to_string(e)),
      )
    }
  })

  let processed = list.filter(results, fn(r) { r.success })
  let failed = list.filter(results, fn(r) { !r.success })

  BatchResult(
    total: list.length(items),
    processed: list.length(processed),
    failed: list.length(failed),
    results: results,
  )
}

/// Process pending media by type
pub fn process_pending_by_type(
  _media_type: String,
  _batch_size: Int,
  _session_id: String,
) -> Result(BatchResult, ProcessingError) {
  // This would query DB for pending items - for now return stub
  // In real implementation, query telegram_media WHERE process_status = 'pending' AND media_type = ?
  Ok(BatchResult(
    total: 0,
    processed: 0,
    failed: 0,
    results: [],
  ))
}

// =============================================================================
// Go Bridge Communication
// =============================================================================

/// Download media from Telegram via Go Bridge
fn download_media(
  chat_id: Int,
  message_id: Int,
  session_id: String,
) -> Result(DownloadResult, String) {
  let url = bridge_url() <> "/api/v1/download"

  let body = json.object([
    #("chat_id", json.int(chat_id)),
    #("message_id", json.int(message_id)),
  ])
  |> json.to_string

  case http_post_with_session(url, session_id, body) {
    Error(e) -> Error(e)
    Ok(response) -> parse_download_response(response)
  }
}

/// Parse download response from Go Bridge
fn parse_download_response(response: String) -> Result(DownloadResult, String) {
  let decoder = {
    use success <- decode.field("success", decode.bool)
    use file_path <- decode.optional_field("file_path", "", decode.string)
    use file_size <- decode.optional_field("file_size", 0, decode.int)
    use mime_type <- decode.optional_field("mime_type", "", decode.string)
    use error <- decode.optional_field("error", "", decode.string)
    decode.success(DownloadResult(
      success: success,
      file_path: file_path,
      file_size: file_size,
      mime_type: mime_type,
      error: error,
    ))
  }

  case json.parse(response, decoder) {
    Ok(result) -> Ok(result)
    Error(_) -> Error("Invalid download response: " <> string.slice(response, 0, 100))
  }
}

// =============================================================================
// HTTP Utilities
// =============================================================================

/// HTTP POST with session ID header
fn http_post_with_session(
  url: String,
  session_id: String,
  body: String,
) -> Result(String, String) {
  let #(host, port, path) = parse_url(url)

  let req = request.new()
    |> request.set_scheme(http.Http)
    |> request.set_method(http.Post)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path(path)
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("X-Session-ID", session_id)
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) -> Ok(response.body)
    Error(_) -> Error("HTTP request failed")
  }
}

/// Parse URL into host, port, path
fn parse_url(url: String) -> #(String, Int, String) {
  let without_scheme = url
    |> string.replace("http://", "")
    |> string.replace("https://", "")

  case string.split_once(without_scheme, "/") {
    Ok(#(host_port, path)) -> {
      let #(host, port) = parse_host_port(host_port)
      #(host, port, "/" <> path)
    }
    Error(_) -> {
      let #(host, port) = parse_host_port(without_scheme)
      #(host, port, "/")
    }
  }
}

/// Parse host:port string
fn parse_host_port(host_port: String) -> #(String, Int) {
  case string.split_once(host_port, ":") {
    Ok(#(host, port_str)) -> {
      case int.parse(port_str) {
        Ok(port) -> #(host, port)
        Error(_) -> #(host, 80)
      }
    }
    Error(_) -> #(host_port, 80)
  }
}

// =============================================================================
// File Utilities
// =============================================================================

/// Get file extension from path (lowercase)
fn get_file_extension(path: String) -> String {
  path
  |> string.split(".")
  |> list.last
  |> result.unwrap("")
  |> string.lowercase
}

/// Delete temporary file (best effort)
fn delete_temp_file(path: String) -> Nil {
  // Use simplifile to delete - ignore errors
  let _ = simplifile_delete(path)
  Nil
}

@external(erlang, "file", "delete")
fn simplifile_delete(path: String) -> Result(Nil, Nil)

// =============================================================================
// Error Handling
// =============================================================================

/// Convert processing error to string
pub fn error_to_string(err: ProcessingError) -> String {
  case err {
    DownloadError(msg) -> "Download error: " <> msg
    AnalysisError(msg) -> "Analysis error: " <> msg
    DatabaseError(msg) -> "Database error: " <> msg
    UnsupportedMediaType(t) -> "Unsupported media type: " <> t
  }
}

// =============================================================================
// Result Serialization
// =============================================================================

/// Convert processing result to JSON
pub fn result_to_json(result: ProcessingResult) -> String {
  json.object([
    #("media_id", json.int(result.media_id)),
    #("success", json.bool(result.success)),
    #("transcription", option_to_json(result.transcription)),
    #("description", option_to_json(result.description)),
    #("ocr_text", option_to_json(result.ocr_text)),
    #("detected_objects", json.array(result.detected_objects, json.string)),
    #("emotions", option_to_json(result.emotions)),
    #("key_phrases", json.array(result.key_phrases, json.string)),
    #("error", option_to_json(result.error)),
  ])
  |> json.to_string
}

/// Convert batch result to JSON
pub fn batch_result_to_json(result: BatchResult) -> String {
  json.object([
    #("total", json.int(result.total)),
    #("processed", json.int(result.processed)),
    #("failed", json.int(result.failed)),
    #("results", json.array(result.results, fn(r) {
      json.object([
        #("media_id", json.int(r.media_id)),
        #("success", json.bool(r.success)),
        #("error", option_to_json(r.error)),
      ])
    })),
  ])
  |> json.to_string
}

fn option_to_json(opt: Option(String)) -> json.Json {
  case opt {
    Some(s) -> json.string(s)
    None -> json.null()
  }
}
