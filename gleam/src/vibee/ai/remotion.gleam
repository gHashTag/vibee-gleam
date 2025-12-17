// Remotion Lambda Integration
// API Documentation: https://www.remotion.dev/docs/lambda

import gleam/dynamic
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}

// ============================================================
// Types
// ============================================================

pub type Config {
  Config(
    region: String,
    function_name: String,
    serve_url: String,
  )
}

pub type RenderRequest {
  RenderRequest(
    composition_id: String,
    input_props: json.Json,
    codec: Option(String),
    output_bucket: String,
  )
}

pub type StillRequest {
  StillRequest(
    composition_id: String,
    input_props: json.Json,
    frame: Int,
    output_bucket: String,
  )
}

pub type RenderProgressRequest {
  RenderProgressRequest(
    render_id: String,
    bucket_name: String,
  )
}

pub type Request {
  Request(
    url: String,
    method: String,
    headers: List(#(String, String)),
    body: String,
  )
}

pub type RenderStatus {
  Pending
  Rendering(progress: Float)
  Done(output_url: String)
  Failed(error: String)
}

pub type RenderProgress {
  RenderProgress(
    render_id: String,
    status: RenderStatus,
    overall_progress: Float,
    frames_rendered: Int,
    total_frames: Int,
  )
}

// ============================================================
// Constants
// ============================================================

const api_version = "2023-12-24"

// ============================================================
// Config Helpers
// ============================================================

/// Create default config with common settings
pub fn default_config(region: String, function_name: String, serve_url: String) -> Config {
  Config(
    region: region,
    function_name: function_name,
    serve_url: serve_url,
  )
}

/// Create config from environment-style values
pub fn config_from_env(
  aws_region: String,
  remotion_function: String,
  remotion_serve_url: String,
) -> Config {
  Config(
    region: aws_region,
    function_name: remotion_function,
    serve_url: remotion_serve_url,
  )
}

// ============================================================
// Request Builders
// ============================================================

/// Create a render video request payload
/// This creates the JSON payload to send to Remotion Lambda
pub fn render_video_payload(config: Config, req: RenderRequest) -> String {
  let codec = case req.codec {
    Some(c) -> c
    None -> "h264"
  }

  json.to_string(json.object([
    #("type", json.string("start")),
    #("serveUrl", json.string(config.serve_url)),
    #("composition", json.string(req.composition_id)),
    #("inputProps", req.input_props),
    #("codec", json.string(codec)),
    #("outName", json.string("video.mp4")),
    #("privacy", json.string("public")),
  ]))
}

/// Create a render still request payload
pub fn render_still_payload(config: Config, req: StillRequest) -> String {
  json.to_string(json.object([
    #("type", json.string("still")),
    #("serveUrl", json.string(config.serve_url)),
    #("composition", json.string(req.composition_id)),
    #("inputProps", req.input_props),
    #("frame", json.int(req.frame)),
    #("imageFormat", json.string("png")),
    #("privacy", json.string("public")),
  ]))
}

/// Create a get render progress request payload
pub fn get_progress_payload(req: RenderProgressRequest) -> String {
  json.to_string(json.object([
    #("type", json.string("status")),
    #("renderId", json.string(req.render_id)),
    #("bucketName", json.string(req.bucket_name)),
  ]))
}

// ============================================================
// Simple Request Builders
// ============================================================

/// Create a simple render request with default settings
pub fn simple_render_request(
  composition_id: String,
  output_bucket: String,
) -> RenderRequest {
  RenderRequest(
    composition_id: composition_id,
    input_props: json.object([]),
    codec: None,
    output_bucket: output_bucket,
  )
}

/// Create a render request with custom props
pub fn render_request_with_props(
  composition_id: String,
  props: List(#(String, json.Json)),
  output_bucket: String,
) -> RenderRequest {
  RenderRequest(
    composition_id: composition_id,
    input_props: json.object(props),
    codec: None,
    output_bucket: output_bucket,
  )
}

/// Create a TextOverlay render request
pub fn text_overlay_request(
  title: String,
  subtitle: String,
  output_bucket: String,
) -> RenderRequest {
  RenderRequest(
    composition_id: "TextOverlay",
    input_props: json.object([
      #("title", json.string(title)),
      #("subtitle", json.string(subtitle)),
      #("backgroundColor", json.string("#1a1a2e")),
      #("textColor", json.string("#ffffff")),
      #("accentColor", json.string("#e94560")),
    ]),
    codec: Some("h264"),
    output_bucket: output_bucket,
  )
}

/// Create a VideoIntro render request
pub fn video_intro_request(
  brand_name: String,
  tagline: String,
  output_bucket: String,
) -> RenderRequest {
  RenderRequest(
    composition_id: "VideoIntro",
    input_props: json.object([
      #("brandName", json.string(brand_name)),
      #("tagline", json.string(tagline)),
      #("logoUrl", json.string("")),
      #("primaryColor", json.string("#6c5ce7")),
      #("secondaryColor", json.string("#00cec9")),
    ]),
    codec: Some("h264"),
    output_bucket: output_bucket,
  )
}

/// Create a DynamicVideo render request
pub fn dynamic_video_request(
  user_name: String,
  message: String,
  items: List(#(String, String)),
  theme: String,
  output_bucket: String,
) -> RenderRequest {
  let items_json = json.array(items, fn(item) {
    let #(title, value) = item
    json.object([
      #("title", json.string(title)),
      #("value", json.string(value)),
    ])
  })

  RenderRequest(
    composition_id: "DynamicVideo",
    input_props: json.object([
      #("userName", json.string(user_name)),
      #("message", json.string(message)),
      #("avatarUrl", json.string("")),
      #("items", items_json),
      #("theme", json.string(theme)),
    ]),
    codec: Some("h264"),
    output_bucket: output_bucket,
  )
}

// ============================================================
// Available Compositions
// ============================================================

/// List all available compositions
pub fn available_compositions() -> List(String) {
  ["TextOverlay", "VideoIntro", "DynamicVideo"]
}

/// Get composition descriptions
pub fn composition_descriptions() -> List(#(String, String)) {
  [
    #("TextOverlay", "Animated text overlay with title and subtitle. 9:16 vertical format."),
    #("VideoIntro", "Brand intro animation with logo and tagline. 16:9 horizontal format."),
    #("DynamicVideo", "Data-driven video with user message and items. 9:16 vertical format."),
  ]
}

/// Get available codecs
pub fn available_codecs() -> List(String) {
  ["h264", "h265", "vp8", "vp9", "prores", "gif"]
}

/// Get codec descriptions
pub fn codec_descriptions() -> List(#(String, String)) {
  [
    #("h264", "Most compatible, works everywhere"),
    #("h265", "Better compression, modern devices"),
    #("vp8", "WebM format, good for web"),
    #("vp9", "Better WebM, smaller files"),
    #("prores", "High quality, large files"),
    #("gif", "Animated GIF format"),
  ]
}
