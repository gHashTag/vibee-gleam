// Storage MCP Tools
// Provides MCP tools for S3/Tigris file storage via render-server proxy

import gleam/dynamic
import gleam/dynamic/decode
import gleam/json
import vibee/mcp/types.{
  type Tool, type ToolResult, TextContent, Tool, ToolResult,
}

// ============================================================
// FFI Imports
// ============================================================

/// Upload to S3 via render-server proxy
@external(erlang, "vibee_storage_ffi", "upload_to_s3")
fn ffi_upload(
  data: BitArray,
  filename: String,
  content_type: String,
) -> Result(dynamic.Dynamic, String)

/// List assets from S3
@external(erlang, "vibee_storage_ffi", "list_assets")
fn ffi_list_assets() -> Result(String, String)

/// Get storage config
@external(erlang, "vibee_storage_ffi", "get_config")
fn ffi_get_config() -> dynamic.Dynamic

// ============================================================
// Tool Definitions
// ============================================================

/// Tool: storage_upload - Upload file to S3/Tigris
pub fn storage_upload_tool() -> Tool {
  Tool(
    name: "storage_upload",
    description: "Upload a file to S3/Tigris cloud storage. Accepts base64-encoded file data. Returns public URL of uploaded file.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "data",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("Base64-encoded file content"),
              ),
            ]),
          ),
          #(
            "filename",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("Filename with extension (e.g., 'video.mp4', 'image.png')"),
              ),
            ]),
          ),
          #(
            "content_type",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("MIME type (e.g., 'video/mp4', 'image/png', 'audio/mpeg')"),
              ),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["data", "filename"], json.string)),
    ]),
  )
}

/// Tool: storage_list - List files in S3/Tigris
pub fn storage_list_tool() -> Tool {
  Tool(
    name: "storage_list",
    description: "List all uploaded files in S3/Tigris storage. Returns array of files with URLs and metadata.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
    ]),
  )
}

/// Tool: storage_config - Get storage configuration
pub fn storage_config_tool() -> Tool {
  Tool(
    name: "storage_config",
    description: "Get current storage configuration (S3 endpoint, bucket, public URL).",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
    ]),
  )
}

// ============================================================
// All Tools
// ============================================================

/// Get all Storage tools
pub fn all_tools() -> List(Tool) {
  [storage_upload_tool(), storage_list_tool(), storage_config_tool()]
}

// ============================================================
// Tool Handlers
// ============================================================

fn text_result(content: String) -> ToolResult {
  ToolResult(content: [TextContent(content)], is_error: False)
}

fn error_result(message: String) -> ToolResult {
  ToolResult(content: [TextContent("Error: " <> message)], is_error: True)
}

/// Handle storage_upload - uploads base64 data to S3 via render-server
pub fn handle_upload(
  data: String,
  filename: String,
  content_type: String,
) -> ToolResult {
  // Convert base64 string to BitArray
  let data_bits = <<data:utf8>>

  // Determine content type if not provided
  let ct = case content_type {
    "" -> guess_content_type(filename)
    _ -> content_type
  }

  case ffi_upload(data_bits, filename, ct) {
    Ok(result) -> {
      // Extract URL from result map
      let url_decoder = decode.at(["url"], decode.string)
      let key_decoder = decode.at(["key"], decode.string)

      let url = case decode.run(result, url_decoder) {
        Ok(u) -> u
        Error(_) -> "unknown"
      }
      let key = case decode.run(result, key_decoder) {
        Ok(k) -> k
        Error(_) -> ""
      }
      text_result(
        "Uploaded successfully!\nURL: " <> url <> "\nKey: " <> key
      )
    }
    Error(err) -> error_result(err)
  }
}

/// Handle storage_list - lists all assets from S3
pub fn handle_list() -> ToolResult {
  case ffi_list_assets() {
    Ok(json_str) -> text_result(json_str)
    Error(err) -> error_result(err)
  }
}

/// Handle storage_config - returns storage configuration
pub fn handle_config() -> ToolResult {
  let config = ffi_get_config()

  let render_url = case decode.run(config, decode.at(["render_server_url"], decode.string)) {
    Ok(u) -> u
    Error(_) -> "unknown"
  }
  let s3_endpoint = case decode.run(config, decode.at(["s3_endpoint"], decode.string)) {
    Ok(e) -> e
    Error(_) -> "unknown"
  }
  let bucket = case decode.run(config, decode.at(["s3_bucket"], decode.string)) {
    Ok(b) -> b
    Error(_) -> "unknown"
  }
  let public_url = case decode.run(config, decode.at(["s3_public_url"], decode.string)) {
    Ok(p) -> p
    Error(_) -> "unknown"
  }

  text_result(
    "Storage Configuration:\n" <>
    "Render Server: " <> render_url <> "\n" <>
    "S3 Endpoint: " <> s3_endpoint <> "\n" <>
    "Bucket: " <> bucket <> "\n" <>
    "Public URL: " <> public_url
  )
}

/// Guess content type from filename extension
fn guess_content_type(filename: String) -> String {
  let ext = get_extension(filename)
  case ext {
    ".mp4" -> "video/mp4"
    ".webm" -> "video/webm"
    ".mov" -> "video/quicktime"
    ".mp3" -> "audio/mpeg"
    ".wav" -> "audio/wav"
    ".png" -> "image/png"
    ".jpg" -> "image/jpeg"
    ".jpeg" -> "image/jpeg"
    ".gif" -> "image/gif"
    ".webp" -> "image/webp"
    _ -> "application/octet-stream"
  }
}

/// Get file extension (lowercase)
@external(erlang, "vibee_storage_ffi", "get_extension")
fn get_extension(filename: String) -> String
