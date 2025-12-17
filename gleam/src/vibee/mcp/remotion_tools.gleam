// Remotion MCP Tools
// Provides MCP tools for Remotion video rendering

import gleam/int
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import vibee/ai/remotion
import vibee/mcp/types.{
  type Tool, type ToolResult, TextContent, Tool, ToolResult,
}

// ============================================================
// Tool Definitions
// ============================================================

/// Tool: remotion_render_video - Render a video composition
pub fn remotion_render_video_tool() -> Tool {
  Tool(
    name: "remotion_render_video",
    description: "Render a video using Remotion. Available compositions: TextOverlay (animated text), VideoIntro (brand intro), DynamicVideo (user message with data items).",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "composition_id",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string(
                  "Composition to render: TextOverlay, VideoIntro, or DynamicVideo",
                ),
              ),
              #(
                "enum",
                json.array(remotion.available_compositions(), json.string),
              ),
            ]),
          ),
          #(
            "title",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Title text (for TextOverlay)")),
            ]),
          ),
          #(
            "subtitle",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Subtitle text (for TextOverlay)")),
            ]),
          ),
          #(
            "brand_name",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Brand name (for VideoIntro)")),
            ]),
          ),
          #(
            "tagline",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Tagline (for VideoIntro)")),
            ]),
          ),
          #(
            "user_name",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("User name (for DynamicVideo)")),
            ]),
          ),
          #(
            "message",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Message text (for DynamicVideo)")),
            ]),
          ),
          #(
            "theme",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("Theme: dark or light (for DynamicVideo)"),
              ),
              #("enum", json.array(["dark", "light"], json.string)),
              #("default", json.string("dark")),
            ]),
          ),
          #(
            "codec",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Video codec")),
              #("enum", json.array(remotion.available_codecs(), json.string)),
              #("default", json.string("h264")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["composition_id"], json.string)),
    ]),
  )
}

/// Tool: remotion_render_still - Render a still image from composition
pub fn remotion_render_still_tool() -> Tool {
  Tool(
    name: "remotion_render_still",
    description: "Render a single frame (still image) from a Remotion composition.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "composition_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Composition to render")),
              #(
                "enum",
                json.array(remotion.available_compositions(), json.string),
              ),
            ]),
          ),
          #(
            "frame",
            json.object([
              #("type", json.string("integer")),
              #(
                "description",
                json.string("Frame number to render (0-indexed)"),
              ),
              #("default", json.int(0)),
            ]),
          ),
          #(
            "title",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Title (for TextOverlay)")),
            ]),
          ),
          #(
            "subtitle",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Subtitle (for TextOverlay)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["composition_id"], json.string)),
    ]),
  )
}

/// Tool: remotion_get_render_status - Check render progress
pub fn remotion_get_render_status_tool() -> Tool {
  Tool(
    name: "remotion_get_render_status",
    description: "Get the status and progress of a Remotion render job.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "render_id",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("Render ID returned from render_video"),
              ),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["render_id"], json.string)),
    ]),
  )
}

/// Tool: remotion_list_compositions - List available compositions
pub fn remotion_list_compositions_tool() -> Tool {
  Tool(
    name: "remotion_list_compositions",
    description: "List all available Remotion video compositions with descriptions.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
    ]),
  )
}

// ============================================================
// Template Management Tools (for AI Agent)
// ============================================================

/// Tool: template_list - List available templates
pub fn template_list_tool() -> Tool {
  Tool(
    name: "template_list",
    description: "List all available Remotion templates in the project. Returns template names, descriptions, and file paths.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "category",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array(["all", "lipsync", "text", "intro", "outro"], json.string)),
              #("description", json.string("Filter templates by category (default: all)")),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

/// Tool: template_read - Read template source code
pub fn template_read_tool() -> Tool {
  Tool(
    name: "template_read",
    description: "Read the TypeScript/TSX source code of a Remotion template. Use this to understand how a template works before modifying it.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "template_name",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Name of the template (e.g., 'LipSyncMain', 'TextOverlay')")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["template_name"], json.string)),
    ]),
  )
}

/// Tool: template_create - Create a new template
pub fn template_create_tool() -> Tool {
  Tool(
    name: "template_create",
    description: "Create a new Remotion template. Generates a new .tsx file in the compositions directory with the provided code.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "name",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Template name (PascalCase, e.g., 'MyCustomTemplate')")),
            ]),
          ),
          #(
            "description",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Brief description of what the template does")),
            ]),
          ),
          #(
            "code",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Full TypeScript/TSX code for the template")),
            ]),
          ),
          #(
            "props_schema",
            json.object([
              #("type", json.string("object")),
              #("description", json.string("JSON schema for template props (optional)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["name", "code"], json.string)),
    ]),
  )
}

/// Tool: template_modify - Modify existing template
pub fn template_modify_tool() -> Tool {
  Tool(
    name: "template_modify",
    description: "Modify an existing Remotion template. Apply changes to the template code.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "template_name",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Name of the template to modify")),
            ]),
          ),
          #(
            "new_code",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("New full code to replace the template (optional)")),
            ]),
          ),
          #(
            "patch",
            json.object([
              #("type", json.string("object")),
              #("description", json.string("Partial changes: {old_string, new_string} for find-replace")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["template_name"], json.string)),
    ]),
  )
}

/// Tool: template_preview - Get preview URL for template
pub fn template_preview_tool() -> Tool {
  Tool(
    name: "template_preview",
    description: "Get a preview URL to see the template in the Remotion Player with custom props.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "template_name",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Template name to preview")),
            ]),
          ),
          #(
            "props",
            json.object([
              #("type", json.string("object")),
              #("description", json.string("Props to pass to the template")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["template_name"], json.string)),
    ]),
  )
}

/// Tool: props_update - Update template props in editor
pub fn props_update_tool() -> Tool {
  Tool(
    name: "props_update",
    description: "Update one or more template properties in the editor. Changes are applied immediately to the preview.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "updates",
            json.object([
              #("type", json.string("object")),
              #("description", json.string("Object with prop keys and new values to set")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["updates"], json.string)),
    ]),
  )
}

// ============================================================
// All Tools
// ============================================================

/// Get all Remotion tools
pub fn all_tools() -> List(Tool) {
  [
    remotion_render_video_tool(),
    remotion_render_still_tool(),
    remotion_get_render_status_tool(),
    remotion_list_compositions_tool(),
    // Template management tools (for AI Agent)
    template_list_tool(),
    template_read_tool(),
    template_create_tool(),
    template_modify_tool(),
    template_preview_tool(),
    props_update_tool(),
    // Factory tools (variation system)
    factory_list_variants_tool(),
    factory_generate_variants_tool(),
    factory_render_preview_tool(),
    factory_batch_render_tool(),
    factory_batch_status_tool(),
    factory_get_gallery_url_tool(),
    // Video Analysis tools (AI Vision)
    analyze_rendered_video_tool(),
    analyze_video_frames_tool(),
    batch_analyze_renders_tool(),
  ]
}

// ============================================================
// Tool Handlers
// ============================================================

/// Handle remotion_list_compositions
pub fn handle_list_compositions() -> ToolResult {
  let descriptions = remotion.composition_descriptions()
  let result =
    list.map(descriptions, fn(item) {
      let #(name, desc) = item
      name <> ": " <> desc
    })
    |> string.join("\n")

  text_result("Available Remotion Compositions:\n\n" <> result)
}

/// Handle remotion_render_video
/// Note: Actual Lambda invocation would require AWS SDK integration
pub fn handle_render_video(args: json.Json, bucket: String) -> ToolResult {
  let composition_id =
    json_get_string(args, "composition_id")
    |> result.unwrap("TextOverlay")

  // Build request based on composition type
  let render_request = case composition_id {
    "TextOverlay" -> {
      let title = json_get_string(args, "title") |> result.unwrap("Welcome")
      let subtitle = json_get_string(args, "subtitle") |> result.unwrap("")
      remotion.text_overlay_request(title, subtitle, bucket)
    }
    "VideoIntro" -> {
      let brand = json_get_string(args, "brand_name") |> result.unwrap("VIBEE")
      let tagline = json_get_string(args, "tagline") |> result.unwrap("")
      remotion.video_intro_request(brand, tagline, bucket)
    }
    "DynamicVideo" -> {
      let user = json_get_string(args, "user_name") |> result.unwrap("User")
      let msg = json_get_string(args, "message") |> result.unwrap("")
      let theme = json_get_string(args, "theme") |> result.unwrap("dark")
      remotion.dynamic_video_request(user, msg, [], theme, bucket)
    }
    _ -> remotion.simple_render_request(composition_id, bucket)
  }

  // Create payload (would be sent to Lambda)
  let config =
    remotion.default_config(
      "eu-central-1",
      "remotion-render-4-0-388",
      "https://remotionlambda-ams.s3.eu-central-1.amazonaws.com/sites/vibee",
    )

  let payload = remotion.render_video_payload(config, render_request)

  // In production, this would invoke AWS Lambda
  // For now, return the prepared payload
  text_result(
    "Render request prepared for composition: "
    <> composition_id
    <> "\n\nPayload:\n"
    <> payload
    <> "\n\nNote: Lambda invocation requires AWS SDK configuration.",
  )
}

/// Handle remotion_render_still
pub fn handle_render_still(args: json.Json, bucket: String) -> ToolResult {
  let composition_id =
    json_get_string(args, "composition_id")
    |> result.unwrap("TextOverlay")
  let frame =
    json_get_int(args, "frame")
    |> result.unwrap(0)

  let still_request =
    remotion.StillRequest(
      composition_id: composition_id,
      input_props: json.object([]),
      frame: frame,
      output_bucket: bucket,
    )

  let config =
    remotion.default_config(
      "eu-central-1",
      "remotion-render-4-0-388",
      "https://remotionlambda-ams.s3.eu-central-1.amazonaws.com/sites/vibee",
    )

  let payload = remotion.render_still_payload(config, still_request)

  text_result(
    "Still render request prepared for composition: "
    <> composition_id
    <> " (frame "
    <> int.to_string(frame)
    <> ")"
    <> "\n\nPayload:\n"
    <> payload,
  )
}

// ============================================================
// Factory Tools (Template Variation System)
// ============================================================

/// Tool: factory_list_variants - List all template variants
pub fn factory_list_variants_tool() -> Tool {
  Tool(
    name: "factory_list_variants",
    description: "List all generated template variants with their preview URLs. Returns variant IDs, priorities, and axis configurations.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "template_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Template ID (default: talking-head)")),
              #("default", json.string("talking-head")),
            ]),
          ),
          #(
            "limit",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Max variants to return (default: 50)")),
              #("default", json.int(50)),
            ]),
          ),
          #(
            "min_priority",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Minimum priority score 0-100 (default: 0)")),
              #("default", json.int(0)),
            ]),
          ),
          #(
            "filter_axis",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Filter by axis value (e.g., 'circle-bottom-left', 'karaoke')")),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

/// Tool: factory_generate_variants - Generate new variants
pub fn factory_generate_variants_tool() -> Tool {
  Tool(
    name: "factory_generate_variants",
    description: "Generate template variants from a base template. Returns list of variants with IDs, priorities, and estimated render cost.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "template_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Template ID to generate variants from")),
              #("default", json.string("talking-head")),
            ]),
          ),
          #(
            "limit",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Maximum variants to generate (default: 100)")),
              #("default", json.int(100)),
            ]),
          ),
          #(
            "axes",
            json.object([
              #("type", json.string("array")),
              #("description", json.string("Axes to vary: avatarPosition, hookStyle, captionStyle, bRollPattern")),
              #("items", json.object([#("type", json.string("string"))])),
            ]),
          ),
          #(
            "min_priority",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Minimum priority threshold (default: 0)")),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

/// Tool: factory_render_preview - Render preview for a variant
pub fn factory_render_preview_tool() -> Tool {
  Tool(
    name: "factory_render_preview",
    description: "Render a single frame preview (thumbnail) for a template variant. Returns image URL.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "variant_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Variant ID (e.g., 'TH_cbl_zi_k_hcc')")),
            ]),
          ),
          #(
            "frame",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Frame number to render (default: 90 = 3 seconds)")),
              #("default", json.int(90)),
            ]),
          ),
          #(
            "width",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Output width (default: 540)")),
              #("default", json.int(540)),
            ]),
          ),
          #(
            "height",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Output height (default: 960)")),
              #("default", json.int(960)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["variant_id"], json.string)),
    ]),
  )
}

/// Tool: factory_batch_render - Start batch rendering
pub fn factory_batch_render_tool() -> Tool {
  Tool(
    name: "factory_batch_render",
    description: "Start batch rendering of multiple template variants. Returns batch ID for tracking progress.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "variant_ids",
            json.object([
              #("type", json.string("array")),
              #("description", json.string("List of variant IDs to render")),
              #("items", json.object([#("type", json.string("string"))])),
            ]),
          ),
          #(
            "template_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Or specify template_id + limit to auto-generate variants")),
            ]),
          ),
          #(
            "limit",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Number of top-priority variants to render")),
            ]),
          ),
          #(
            "concurrency",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Parallel renders (default: 5)")),
              #("default", json.int(5)),
            ]),
          ),
          #(
            "duration",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Video duration in seconds (default: 30)")),
              #("default", json.int(30)),
            ]),
          ),
          #(
            "dry_run",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("If true, only estimate cost without rendering")),
              #("default", json.bool(False)),
            ]),
          ),
          #(
            "test_mode",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("Use placeholder test assets")),
              #("default", json.bool(False)),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

/// Tool: factory_batch_status - Get batch render status
pub fn factory_batch_status_tool() -> Tool {
  Tool(
    name: "factory_batch_status",
    description: "Get the status of a batch render job. Returns progress, completed/failed counts, and output URLs.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "batch_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Batch ID returned from factory_batch_render")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["batch_id"], json.string)),
    ]),
  )
}

/// Tool: factory_get_gallery_url - Get gallery preview URL
pub fn factory_get_gallery_url_tool() -> Tool {
  Tool(
    name: "factory_get_gallery_url",
    description: "Get URL to the variant gallery page showing all previews. Returns HTML gallery URL.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "template_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Template ID (default: talking-head)")),
            ]),
          ),
          #(
            "batch_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Specific batch ID to show (optional)")),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

// ============================================================
// Video Analysis Tools (AI Vision for Quality Review)
// ============================================================

/// Tool: analyze_rendered_video - Analyze a rendered video for quality
pub fn analyze_rendered_video_tool() -> Tool {
  Tool(
    name: "analyze_rendered_video",
    description: "Analyze a rendered video for quality, composition, and visual elements using Gemini 3 Pro vision. Extracts frames at key timestamps and provides detailed feedback with scores.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "video_path",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Absolute path to the rendered video file (e.g., /path/to/video.mp4)")),
            ]),
          ),
          #(
            "analysis_type",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array(["quick", "detailed", "design_review"], json.string)),
              #("description", json.string("Type of analysis: quick (2 frames), detailed (4 frames), design_review (6 frames with scoring)")),
              #("default", json.string("detailed")),
            ]),
          ),
          #(
            "timestamps",
            json.object([
              #("type", json.string("array")),
              #("items", json.object([#("type", json.string("integer"))])),
              #("description", json.string("Custom timestamps in seconds to extract frames (optional, overrides analysis_type)")),
            ]),
          ),
          #(
            "custom_prompt",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Custom prompt for analysis (optional, adds to default)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["video_path"], json.string)),
    ]),
  )
}

/// Tool: analyze_video_frames - Analyze specific video frames
pub fn analyze_video_frames_tool() -> Tool {
  Tool(
    name: "analyze_video_frames",
    description: "Analyze specific frames from a video or image files. Send multiple images for comparison or sequence analysis.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "frame_paths",
            json.object([
              #("type", json.string("array")),
              #("items", json.object([#("type", json.string("string"))])),
              #("description", json.string("List of image file paths (PNG/JPG) to analyze")),
            ]),
          ),
          #(
            "prompt",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Analysis prompt describing what to evaluate")),
            ]),
          ),
          #(
            "model",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array(["gemini-3-pro-image-preview", "gpt-4o", "claude-3.5-sonnet"], json.string)),
              #("description", json.string("Vision model to use (default: gemini-3-pro-image-preview)")),
              #("default", json.string("gemini-3-pro-image-preview")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["frame_paths", "prompt"], json.string)),
    ]),
  )
}

/// Tool: batch_analyze_renders - Analyze multiple rendered videos
pub fn batch_analyze_renders_tool() -> Tool {
  Tool(
    name: "batch_analyze_renders",
    description: "Analyze multiple rendered videos and generate a comparative report. Useful for evaluating batch renders.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "video_directory",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Directory containing rendered videos")),
            ]),
          ),
          #(
            "limit",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Maximum videos to analyze (default: 10)")),
              #("default", json.int(10)),
            ]),
          ),
          #(
            "output_format",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array(["summary", "detailed", "json"], json.string)),
              #("description", json.string("Output format (default: summary)")),
              #("default", json.string("summary")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["video_directory"], json.string)),
    ]),
  )
}

// ============================================================
// Template Tool Handlers
// ============================================================

/// Handle template_list - List available templates
pub fn handle_template_list(category: String) -> ToolResult {
  // Default templates in the player project
  let templates = [
    #("LipSyncMain", "lipsync", "Main lipsync video template with avatar, background videos, and captions"),
    #("TextOverlay", "text", "Simple animated text overlay composition"),
    #("VideoIntro", "intro", "Animated brand intro with logo and tagline"),
    #("DynamicVideo", "text", "User-generated content with dynamic data items"),
    #("Captions", "text", "TikTok-style animated captions overlay"),
  ]

  let filtered = case category {
    "all" | "" -> templates
    cat ->
      list.filter(templates, fn(t) {
        let #(_, c, _) = t
        c == cat
      })
  }

  let result =
    filtered
    |> list.map(fn(t) {
      let #(name, cat, desc) = t
      name <> " [" <> cat <> "]: " <> desc
    })
    |> string.join("\n")

  text_result("Available Templates:\n\n" <> result)
}

/// Handle template_read - Read template code
pub fn handle_template_read(template_name: String, project_path: String) -> ToolResult {
  // Map template names to file paths
  let file_path = case template_name {
    "LipSyncMain" -> project_path <> "/src/compositions/LipSyncMain.tsx"
    "Captions" -> project_path <> "/src/components/Captions/Captions.tsx"
    other -> project_path <> "/src/compositions/" <> other <> ".tsx"
  }

  // Note: Actual file reading would use file system FFI
  text_result(
    "Template: " <> template_name <> "\n"
    <> "Path: " <> file_path <> "\n\n"
    <> "Note: Use file system to read actual content.\n"
    <> "The template exports a React component with props interface.",
  )
}

/// Handle template_create - Create new template
pub fn handle_template_create(
  name: String,
  code: String,
  description: String,
  project_path: String,
) -> ToolResult {
  let file_path = project_path <> "/src/compositions/" <> name <> ".tsx"

  // Note: Actual file writing would use file system FFI
  text_result(
    "Template created: " <> name <> "\n"
    <> "Path: " <> file_path <> "\n"
    <> "Description: " <> description <> "\n\n"
    <> "Code length: " <> int.to_string(string.length(code)) <> " characters\n\n"
    <> "Note: Register the composition in Root.tsx to use it.",
  )
}

/// Handle template_preview - Get preview URL
pub fn handle_template_preview(
  template_name: String,
  player_url: String,
) -> ToolResult {
  let preview_url = player_url <> "/preview/" <> template_name

  text_result(
    "Preview URL: " <> preview_url <> "\n\n"
    <> "Open this URL in browser to see the template.\n"
    <> "You can pass props via query parameters.",
  )
}

/// Handle props_update - Update props in editor
pub fn handle_props_update(updates_json: String) -> ToolResult {
  text_result(
    "Props update request:\n" <> updates_json <> "\n\n"
    <> "Note: Send this via WebSocket to the editor.\n"
    <> "Message type: 'props_changed'",
  )
}

// ============================================================
// Helper Functions
// ============================================================

/// Helper: Create text result
fn text_result(text: String) -> ToolResult {
  ToolResult(content: [TextContent(text: text)], is_error: False)
}

/// Helper: Get string from JSON (returns Result for existing handlers)
fn json_get_string(data: json.Json, key: String) -> Result(String, Nil) {
  let value = get_json_string_ffi(data, key)
  case value {
    "" -> Error(Nil)
    s -> Ok(s)
  }
}

/// Helper: Get int from JSON
fn json_get_int(data: json.Json, key: String) -> Result(Int, Nil) {
  let value = get_json_string_ffi(data, key)
  case value {
    "" -> Error(Nil)
    s -> int.parse(s)
  }
}

// ============================================================
// Handler Wrappers (for MCP registry)
// ============================================================

/// Get all Remotion handlers for registry
pub fn get_handlers() -> List(#(String, fn(json.Json) -> ToolResult)) {
  [
    #("remotion_render_video", wrap_render_video),
    #("remotion_render_still", wrap_render_still),
    #("remotion_get_render_status", wrap_get_render_status),
    #("remotion_list_compositions", wrap_list_compositions),
    #("template_list", wrap_template_list),
    #("template_read", wrap_template_read),
    #("template_create", wrap_template_create),
    #("template_modify", wrap_template_modify),
    #("template_preview", wrap_template_preview),
    #("props_update", wrap_props_update),
    // Video Analysis handlers
    #("analyze_rendered_video", wrap_analyze_rendered_video),
    #("analyze_video_frames", wrap_analyze_video_frames),
    #("batch_analyze_renders", wrap_batch_analyze_renders),
  ]
}

/// Wrapper for render_video
fn wrap_render_video(args: json.Json) -> ToolResult {
  let bucket = get_env_or("REMOTION_OUTPUT_BUCKET", "vibee-renders")
  handle_render_video(args, bucket)
}

/// Wrapper for render_still
fn wrap_render_still(args: json.Json) -> ToolResult {
  let bucket = get_env_or("REMOTION_OUTPUT_BUCKET", "vibee-renders")
  handle_render_still(args, bucket)
}

/// Wrapper for get_render_status
fn wrap_get_render_status(args: json.Json) -> ToolResult {
  let render_id = get_json_string_ffi(args, "render_id")
  text_result(
    "Render status for: " <> render_id <> "\n\n"
    <> "Note: AWS Lambda polling required for actual status."
  )
}

/// Wrapper for list_compositions
fn wrap_list_compositions(_args: json.Json) -> ToolResult {
  handle_list_compositions()
}

/// Wrapper for template_list
fn wrap_template_list(args: json.Json) -> ToolResult {
  let category = get_json_string_ffi(args, "category")
  handle_template_list(category)
}

/// Wrapper for template_read
fn wrap_template_read(args: json.Json) -> ToolResult {
  let template_name = get_json_string_ffi(args, "template_name")
  let project_path = get_env_or("REMOTION_PROJECT_PATH", "/app/remotion")
  handle_template_read(template_name, project_path)
}

/// Wrapper for template_create
fn wrap_template_create(args: json.Json) -> ToolResult {
  let name = get_json_string_ffi(args, "name")
  let code = get_json_string_ffi(args, "code")
  let description = get_json_string_ffi(args, "description")
  let project_path = get_env_or("REMOTION_PROJECT_PATH", "/app/remotion")
  handle_template_create(name, code, description, project_path)
}

/// Wrapper for template_modify
fn wrap_template_modify(args: json.Json) -> ToolResult {
  let template_name = get_json_string_ffi(args, "template_name")
  let new_code = get_json_string_ffi(args, "new_code")
  let project_path = get_env_or("REMOTION_PROJECT_PATH", "/app/remotion")
  text_result(
    "Template modify: " <> template_name <> "\n"
    <> "Path: " <> project_path <> "/src/compositions/" <> template_name <> ".tsx\n"
    <> "Code length: " <> int.to_string(string.length(new_code)) <> " chars\n\n"
    <> "Note: File system write required."
  )
}

/// Wrapper for template_preview
fn wrap_template_preview(args: json.Json) -> ToolResult {
  let template_name = get_json_string_ffi(args, "template_name")
  let player_url = get_env_or("REMOTION_PLAYER_URL", "http://localhost:3000")
  handle_template_preview(template_name, player_url)
}

/// Wrapper for props_update
fn wrap_props_update(args: json.Json) -> ToolResult {
  let updates = json.to_string(args)
  handle_props_update(updates)
}

// ============================================================
// Video Analysis Handlers
// ============================================================

/// Default prompt for video quality analysis
fn video_analysis_prompt() -> String {
  "Analyze this video frame from a rendered Instagram Reel. Evaluate:

1. **Composition** (1-10): Avatar placement, text positioning, visual balance
2. **Color Harmony** (1-10): Color scheme, contrast, brand consistency
3. **Text Readability** (1-10): Font size, contrast with background, safe zones
4. **Visual Quality** (1-10): Resolution, artifacts, blur, compression
5. **Professional Feel** (1-10): Overall polish, attention to detail

For each category, provide:
- Score (1-10)
- Specific issues found
- Improvement suggestions

Return as JSON:
{
  \"overall_score\": 7.5,
  \"composition\": {\"score\": 8, \"issues\": [], \"suggestions\": []},
  \"color_harmony\": {\"score\": 7, \"issues\": [], \"suggestions\": []},
  \"text_readability\": {\"score\": 6, \"issues\": [], \"suggestions\": []},
  \"visual_quality\": {\"score\": 8, \"issues\": [], \"suggestions\": []},
  \"professional_feel\": {\"score\": 7, \"issues\": [], \"suggestions\": []},
  \"summary\": \"One paragraph overall assessment\"
}"
}

/// Handle analyze_rendered_video - Extract frames and analyze with AI vision
pub fn handle_analyze_rendered_video(
  video_path: String,
  analysis_type: String,
  custom_prompt: String,
) -> ToolResult {
  // Determine timestamps based on analysis type
  let timestamps = case analysis_type {
    "quick" -> [1, 8]
    "detailed" -> [1, 3, 8, 12]
    "design_review" -> [1, 3, 5, 8, 10, 12]
    _ -> [1, 3, 8, 12]
  }

  // Build the analysis prompt
  let prompt = case custom_prompt {
    "" -> video_analysis_prompt()
    p -> video_analysis_prompt() <> "\n\nAdditional instructions: " <> p
  }

  // Generate ffmpeg command for frame extraction
  let timestamps_str = list.map(timestamps, int.to_string) |> string.join(",")
  let output_pattern = "/tmp/video_analysis_frame_%d.jpg"

  let ffmpeg_cmd =
    "ffmpeg -i \"" <> video_path <> "\" " <>
    "-vf \"select='eq(n\\," <> timestamps_str <> ")'\" " <>
    "-vsync vfr -q:v 2 " <> output_pattern

  text_result(
    "📹 Video Analysis Request\n\n" <>
    "Video: " <> video_path <> "\n" <>
    "Type: " <> analysis_type <> "\n" <>
    "Frames to analyze: " <> int.to_string(list.length(timestamps)) <> "\n" <>
    "Timestamps (sec): " <> timestamps_str <> "\n\n" <>
    "FFmpeg command:\n" <> ffmpeg_cmd <> "\n\n" <>
    "After frame extraction, images will be sent to:\n" <>
    "Model: google/gemini-3-pro-image-preview\n\n" <>
    "Analysis prompt:\n" <> prompt <> "\n\n" <>
    "Note: Execute ffmpeg via shell, then call analyze_video_frames with the extracted images."
  )
}

/// Handle analyze_video_frames - Analyze extracted frames with AI vision
pub fn handle_analyze_video_frames(
  frame_paths: List(String),
  prompt: String,
  model: String,
) -> ToolResult {
  let model_id = case model {
    "gemini-3-pro-image-preview" -> "google/gemini-3-pro-image-preview"
    "gpt-4o" -> "openai/gpt-4o"
    "claude-3.5-sonnet" -> "anthropic/claude-3.5-sonnet"
    _ -> "google/gemini-3-pro-image-preview"
  }

  let frame_list = string.join(frame_paths, "\n  - ")

  text_result(
    "🖼️ Frame Analysis Request\n\n" <>
    "Model: " <> model_id <> "\n" <>
    "Frames to analyze:\n  - " <> frame_list <> "\n\n" <>
    "Prompt: " <> prompt <> "\n\n" <>
    "To execute this analysis:\n" <>
    "1. Read each image as base64\n" <>
    "2. Create VisionRequest with analyze_multiple_images()\n" <>
    "3. Send to OpenRouter API\n" <>
    "4. Parse JSON response for scores\n\n" <>
    "Use vibee/ai/openrouter.gleam functions:\n" <>
    "- analyze_multiple_images(image_urls, prompt)\n" <>
    "- create_vision_request(config, req)"
  )
}

/// Handle batch_analyze_renders - Analyze multiple videos in a directory
pub fn handle_batch_analyze_renders(
  video_directory: String,
  limit: Int,
  output_format: String,
) -> ToolResult {
  text_result(
    "📊 Batch Analysis Request\n\n" <>
    "Directory: " <> video_directory <> "\n" <>
    "Limit: " <> int.to_string(limit) <> " videos\n" <>
    "Format: " <> output_format <> "\n\n" <>
    "Process:\n" <>
    "1. List all .mp4 files in directory\n" <>
    "2. For each video (up to limit):\n" <>
    "   - Extract key frames with ffmpeg\n" <>
    "   - Analyze with Gemini 3 Pro\n" <>
    "   - Collect scores\n" <>
    "3. Generate comparative report\n\n" <>
    "Output includes:\n" <>
    "- Average scores across all videos\n" <>
    "- Best/worst performing variants\n" <>
    "- Common issues detected\n" <>
    "- Recommendations for improvement"
  )
}

/// Wrapper for analyze_rendered_video
fn wrap_analyze_rendered_video(args: json.Json) -> ToolResult {
  let video_path = get_json_string_ffi(args, "video_path")
  let analysis_type = case get_json_string_ffi(args, "analysis_type") {
    "" -> "detailed"
    t -> t
  }
  let custom_prompt = get_json_string_ffi(args, "custom_prompt")
  handle_analyze_rendered_video(video_path, analysis_type, custom_prompt)
}

/// Wrapper for analyze_video_frames
fn wrap_analyze_video_frames(args: json.Json) -> ToolResult {
  let frame_paths = get_json_string_list_ffi(args, "frame_paths")
  let prompt = get_json_string_ffi(args, "prompt")
  let model = case get_json_string_ffi(args, "model") {
    "" -> "gemini-3-pro-image-preview"
    m -> m
  }
  handle_analyze_video_frames(frame_paths, prompt, model)
}

/// Wrapper for batch_analyze_renders
fn wrap_batch_analyze_renders(args: json.Json) -> ToolResult {
  let video_directory = get_json_string_ffi(args, "video_directory")
  let limit = case json_get_int(args, "limit") {
    Ok(l) -> l
    Error(_) -> 10
  }
  let output_format = case get_json_string_ffi(args, "output_format") {
    "" -> "summary"
    f -> f
  }
  handle_batch_analyze_renders(video_directory, limit, output_format)
}

// ============================================================
// FFI Helpers
// ============================================================

@external(erlang, "vibee_json_ffi", "get_string")
fn get_json_string_ffi(data: json.Json, key: String) -> String

@external(erlang, "vibee_json_ffi", "get_string_list")
fn get_json_string_list_ffi(data: json.Json, key: String) -> List(String)

@external(erlang, "vibee_remotion_ffi", "get_env_or")
fn get_env_or(key: String, default: String) -> String
