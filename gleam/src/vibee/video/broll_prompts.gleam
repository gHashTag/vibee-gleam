// B-Roll Prompt Generator for Veo 3.1
// Generates video prompts from script text following Veo 3.1 best practices
//
// Veo 3.1 Prompt Structure:
// Subject + Action + Setting + Style + Camera + Lighting + Duration

import gleam/dynamic/decode
import gleam/float
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/mcp/config

// ============================================================
// Types
// ============================================================

pub type BRollPrompt {
  BRollPrompt(
    segment_index: Int,
    prompt: String,
    duration_seconds: Int,
    start_seconds: Float,
    camera: String,
    style: String,
  )
}

pub type BRollStyle {
  Corporate
  Casual
  Cinematic
  Documentary
  Social
}

pub type CameraMove {
  Static
  SlowPan
  DollyForward
  TrackingShot
  AerialView
  CloseUp
  MediumShot
  WideShot
}

// ============================================================
// Preset B-Roll Templates (for test mode)
// ============================================================

/// Get preset B-roll prompts for common topics
pub fn get_preset_prompts(topic: String, total_duration: Float) -> List(BRollPrompt) {
  let t = string.lowercase(topic)
  let prompts = detect_topic_prompts(t)

  // Distribute prompts across video duration
  distribute_prompts(prompts, total_duration)
}

/// Detect topic and return appropriate prompts
fn detect_topic_prompts(topic: String) -> List(String) {
  case
    string.contains(topic, "бизнес") || string.contains(topic, "business")
  {
    True -> business_prompts()
    False ->
      case
        string.contains(topic, "технолог") || string.contains(topic, "tech")
      {
        True -> tech_prompts()
        False ->
          case
            string.contains(topic, "здоров") || string.contains(topic, "health")
          {
            True -> health_prompts()
            False ->
              case
                string.contains(topic, "финанс")
                || string.contains(topic, "money")
              {
                True -> finance_prompts()
                False -> generic_prompts()
              }
          }
      }
  }
}

/// Business-themed B-roll prompts
fn business_prompts() -> List(String) {
  [
    "A confident business professional presenting data on a laptop screen, modern minimalist office with floor-to-ceiling windows, corporate clean style, medium shot slight dolly forward, soft natural light from windows warm tone, 6 seconds",
    "Hands typing on a sleek laptop keyboard with charts visible on screen, contemporary coworking space, professional aesthetic, close-up shot static, ambient office lighting, 4 seconds",
    "Team meeting in glass-walled conference room, diverse professionals discussing strategy, modern corporate environment, wide shot slow pan left, even diffused lighting, 6 seconds",
    "Business person walking through bright modern office lobby, confident purposeful stride, architectural interior design, tracking shot following subject, natural daylight streaming in, 4 seconds",
    "Close-up of handshake between business partners, successful deal celebration, professional setting, extreme close-up static, warm key light, 4 seconds",
  ]
}

/// Tech-themed B-roll prompts
fn tech_prompts() -> List(String) {
  [
    "Software developer coding on multiple monitors with colorful syntax highlighting, modern tech startup office, cyberpunk aesthetic subtle, medium shot static, blue-tinted ambient lighting, 6 seconds",
    "Hands interacting with futuristic holographic interface, clean white technology lab, sci-fi minimalist style, close-up dolly forward, soft neon accents, 4 seconds",
    "Server room with blinking lights and cable management, high-tech data center, documentary style, slow pan across racks, cool blue LED lighting, 6 seconds",
    "Team brainstorming around whiteboard with diagrams and flowcharts, creative tech workspace, startup culture aesthetic, wide shot, natural light mixed with warm lamps, 4 seconds",
  ]
}

/// Health-themed B-roll prompts
fn health_prompts() -> List(String) {
  [
    "Person meditating in peaceful morning light, serene minimalist room, wellness lifestyle aesthetic, medium shot static, soft golden hour lighting, 6 seconds",
    "Fresh healthy meal preparation with colorful vegetables, bright modern kitchen, lifestyle documentary style, close-up overhead shot, bright natural light, 4 seconds",
    "Person jogging through beautiful park at sunrise, active lifestyle scene, cinematic style, tracking shot following runner, warm sunrise lighting, 6 seconds",
    "Yoga pose in studio with plants and natural materials, zen wellness environment, calm aesthetic, wide shot slow push in, soft diffused daylight, 4 seconds",
  ]
}

/// Finance-themed B-roll prompts
fn finance_prompts() -> List(String) {
  [
    "Stock market charts and graphs on professional trading screens, financial district office, corporate serious tone, close-up pan across monitors, blue and green accent lighting, 6 seconds",
    "Business person reviewing financial documents with calculator, executive office setting, professional atmosphere, medium shot static, warm desk lamp lighting, 4 seconds",
    "Coins and currency in artistic arrangement, wealth concept visualization, elegant minimal style, extreme close-up slow dolly, dramatic side lighting, 4 seconds",
    "Modern cityscape with financial district skyscrapers, establishing shot of business hub, cinematic urban aesthetic, wide aerial view, golden hour lighting, 6 seconds",
  ]
}

/// Generic B-roll prompts for any topic
fn generic_prompts() -> List(String) {
  [
    "Person working thoughtfully at desk with notebook and laptop, modern home office, lifestyle aesthetic, medium shot static, soft natural window light, 6 seconds",
    "Hands writing notes with elegant pen, creative process visualization, minimal aesthetic, close-up static, warm ambient lighting, 4 seconds",
    "Peaceful nature scene with gentle movement, calming backdrop, documentary style, wide shot slow pan, natural daylight, 6 seconds",
    "Abstract motion graphics with flowing shapes and colors, modern digital aesthetic, contemporary style, full frame, vibrant colors, 4 seconds",
  ]
}

// ============================================================
// Prompt Distribution
// ============================================================

/// Distribute prompts evenly across video duration
fn distribute_prompts(prompts: List(String), total_duration: Float) -> List(BRollPrompt) {
  let prompt_count = list.length(prompts)
  let segment_duration = total_duration /. int.to_float(prompt_count)

  prompts
  |> list.index_map(fn(prompt, idx) {
    let start = int.to_float(idx) *. segment_duration
    let duration = case idx {
      _ if idx == prompt_count - 1 -> 6  // Last segment slightly longer
      _ -> 4  // Standard duration
    }

    BRollPrompt(
      segment_index: idx,
      prompt: prompt,
      duration_seconds: duration,
      start_seconds: start,
      camera: extract_camera(prompt),
      style: extract_style(prompt),
    )
  })
}

/// Extract camera movement from prompt
fn extract_camera(prompt: String) -> String {
  let p = string.lowercase(prompt)
  case string.contains(p, "dolly") {
    True -> "dolly"
    False ->
      case string.contains(p, "pan") {
        True -> "pan"
        False ->
          case string.contains(p, "tracking") {
            True -> "tracking"
            False ->
              case string.contains(p, "aerial") {
                True -> "aerial"
                False ->
                  case string.contains(p, "close-up") {
                    True -> "close-up"
                    False ->
                      case string.contains(p, "wide shot") {
                        True -> "wide"
                        False -> "static"
                      }
                  }
              }
          }
      }
  }
}

/// Extract style from prompt
fn extract_style(prompt: String) -> String {
  let p = string.lowercase(prompt)
  case string.contains(p, "cinematic") {
    True -> "cinematic"
    False ->
      case string.contains(p, "documentary") {
        True -> "documentary"
        False ->
          case string.contains(p, "corporate") {
            True -> "corporate"
            False ->
              case string.contains(p, "minimal") {
                True -> "minimal"
                False -> "lifestyle"
              }
          }
      }
  }
}

// ============================================================
// JSON Serialization
// ============================================================

/// Convert B-roll prompt to JSON
pub fn prompt_to_json(prompt: BRollPrompt) -> json.Json {
  json.object([
    #("segment_index", json.int(prompt.segment_index)),
    #("prompt", json.string(prompt.prompt)),
    #("duration_seconds", json.int(prompt.duration_seconds)),
    #("start_seconds", json.float(prompt.start_seconds)),
    #("camera", json.string(prompt.camera)),
    #("style", json.string(prompt.style)),
  ])
}

/// Convert list of prompts to JSON array
pub fn prompts_to_json(prompts: List(BRollPrompt)) -> json.Json {
  json.array(prompts, prompt_to_json)
}

// ============================================================
// Test Mode Assets Mapping
// ============================================================

/// Map B-roll prompts to existing assets (for test mode)
/// Returns list of (prompt_index, asset_url) pairs
pub fn map_to_test_assets(prompts: List(BRollPrompt), base_url: String) -> List(#(Int, String)) {
  let assets = [
    "/backgrounds/business/00.mp4",
    "/backgrounds/business/01.mp4",
    "/backgrounds/business/02.mp4",
    "/backgrounds/business/03.mp4",
    "/backgrounds/business/04.mp4",
  ]

  prompts
  |> list.index_map(fn(prompt, idx) {
    let asset_idx = idx % list.length(assets)
    let asset = case get_at_index(assets, asset_idx) {
      Ok(a) -> a
      Error(_) -> "/backgrounds/business/00.mp4"
    }
    #(prompt.segment_index, base_url <> asset)
  })
}

/// Get element at index from list
fn get_at_index(lst: List(a), idx: Int) -> Result(a, Nil) {
  lst
  |> list.drop(idx)
  |> list.first()
}

// ============================================================
// AI-Powered B-Roll Generation (OpenRouter/Gemini)
// ============================================================

/// Generate B-roll prompts from transcription text using AI
/// Similar to render-api-v3/broll_prompts_from_transcription.py
pub fn generate_broll_prompts_ai(
  text: String,
  start_time: Float,
  end_time: Float,
) -> Result(List(BRollPrompt), String) {
  let api_key = config.get_env("OPENROUTER_API_KEY")
  case api_key {
    "" -> Error("OPENROUTER_API_KEY not set")
    key -> call_openrouter_for_brolls(text, start_time, end_time, key)
  }
}

/// Call OpenRouter API to generate B-roll segments
fn call_openrouter_for_brolls(
  text: String,
  start_time: Float,
  end_time: Float,
  api_key: String,
) -> Result(List(BRollPrompt), String) {
  let duration = end_time -. start_time

  // Calculate segment constraints (3-8 seconds each)
  let min_segments = max_int(1, float.ceiling(duration /. 8.0) |> float.round)
  let max_segments = max_int(min_segments, float.floor(duration /. 3.0) |> float.round)

  let system_prompt = build_broll_system_prompt(duration, start_time, end_time, min_segments, max_segments)
  let user_prompt = build_broll_user_prompt(text, duration, start_time, end_time, min_segments, max_segments)

  let url = "https://openrouter.ai/api/v1/chat/completions"

  let body = json.object([
    #("model", json.string("google/gemini-3-flash-preview")),
    #("messages", json.array([
      json.object([
        #("role", json.string("system")),
        #("content", json.string(system_prompt)),
      ]),
      json.object([
        #("role", json.string("user")),
        #("content", json.string(user_prompt)),
      ]),
    ], fn(x) { x })),
    #("temperature", json.float(0.7)),
    #("max_tokens", json.int(4096)),
    #("response_format", json.object([
      #("type", json.string("json_object")),
    ])),
  ])

  io.println("[BROLL AI] Calling OpenRouter for B-roll generation...")
  io.println("[BROLL AI] Text length: " <> int.to_string(string.length(text)))
  io.println("[BROLL AI] Duration: " <> float.to_string(duration) <> "s")

  case request.to(url) {
    Error(_) -> Error("Invalid URL")
    Ok(req) -> {
      let req = req
        |> request.set_method(http.Post)
        |> request.set_header("content-type", "application/json")
        |> request.set_header("authorization", "Bearer " <> api_key)
        |> request.set_header("http-referer", "https://vibee-mcp.fly.dev")
        |> request.set_header("x-title", "VIBEE B-Roll Generator")
        |> request.set_body(json.to_string(body))

      case httpc.send(req) {
        Error(e) -> Error("HTTP error: " <> string.inspect(e))
        Ok(resp) -> {
          io.println("[BROLL AI] Response status: " <> int.to_string(resp.status))
          case resp.status {
            200 -> parse_broll_ai_response(resp.body)
            429 -> Error("Rate limited - try again later")
            status -> Error("API error: " <> int.to_string(status) <> " - " <> resp.body)
          }
        }
      }
    }
  }
}

/// Build system prompt for B-roll generation
fn build_broll_system_prompt(
  duration: Float,
  start_time: Float,
  end_time: Float,
  min_segments: Int,
  max_segments: Int,
) -> String {
  "You are a creative director and expert visual strategist for generating sequential b-roll segments for vertical video content (9:16 format).

**YOUR TASK:**
Analyze the provided text and create a sequence of " <> int.to_string(min_segments) <> "-" <> int.to_string(max_segments) <> " b-roll video segments that will play back-to-back within a " <> float.to_string(duration) <> " second time window (from " <> float.to_string(start_time) <> "s to " <> float.to_string(end_time) <> "s).

**CRITICAL REQUIREMENTS:**
1. **Sequential Timing:** Each segment must have a precise start and end time. Segments should be sequential and non-overlapping.
2. **Duration Constraints:** Each segment MUST be between 3.0 and 8.0 seconds long.
3. **Total Duration:** All segments combined must fit exactly within the " <> float.to_string(duration) <> " second window.
4. **Segment Count:** Create between " <> int.to_string(min_segments) <> " and " <> int.to_string(max_segments) <> " segments.
5. **Cinematic Quality:** Use descriptive, cinematic language (slow motion, close-up, tracking shot, rack focus, etc.)
6. **Vertical Format:** Optimize all descriptions for 9:16 vertical video.
7. **Visual Metaphors:** Translate abstract concepts into powerful visual metaphors.

**OUTPUT FORMAT:**
Return a valid JSON object with this exact structure:
{
  \"segments\": [
    {
      \"start\": <float - start time in seconds>,
      \"end\": <float - end time in seconds>,
      \"prompt\": \"<highly descriptive cinematic prompt for video generation>\"
    }
  ]
}

Do NOT include any markdown formatting or explanatory text - only the JSON object."
}

/// Build user prompt for B-roll generation
fn build_broll_user_prompt(
  text: String,
  duration: Float,
  start_time: Float,
  end_time: Float,
  min_segments: Int,
  max_segments: Int,
) -> String {
  "Time window: " <> float.to_string(start_time) <> "s to " <> float.to_string(end_time) <> "s (total: " <> float.to_string(duration) <> "s)

Transcription text:
" <> text <> "

Generate " <> int.to_string(min_segments) <> "-" <> int.to_string(max_segments) <> " sequential b-roll segments with exact timing and cinematic prompts."
}

/// Parse AI response into BRollPrompt list
fn parse_broll_ai_response(body: String) -> Result(List(BRollPrompt), String) {
  // First, extract the content from OpenRouter response
  let content_decoder = {
    use choices <- decode.field("choices", decode.list({
      use message <- decode.field("message", {
        use content <- decode.field("content", decode.string)
        decode.success(content)
      })
      decode.success(message)
    }))
    decode.success(choices)
  }

  case json.parse(body, content_decoder) {
    Error(_) -> Error("Failed to parse OpenRouter response")
    Ok([]) -> Error("No choices in response")
    Ok([content, ..]) -> {
      io.println("[BROLL AI] Got content, parsing segments...")
      parse_segments_json(content)
    }
  }
}

/// Parse the segments JSON from AI response
fn parse_segments_json(content: String) -> Result(List(BRollPrompt), String) {
  let segment_decoder = {
    use start <- decode.field("start", decode.float)
    use end <- decode.field("end", decode.float)
    use prompt <- decode.field("prompt", decode.string)
    decode.success(#(start, end, prompt))
  }

  let segments_decoder = {
    use segments <- decode.field("segments", decode.list(segment_decoder))
    decode.success(segments)
  }

  case json.parse(content, segments_decoder) {
    Error(_) -> {
      io.println("[BROLL AI] Failed to parse segments JSON: " <> string.slice(content, 0, 200))
      Error("Failed to parse segments JSON")
    }
    Ok(segments) -> {
      io.println("[BROLL AI] Parsed " <> int.to_string(list.length(segments)) <> " segments")
      let prompts = list.index_map(segments, fn(seg, idx) {
        let #(start, end, prompt) = seg
        let duration_secs = end -. start
        BRollPrompt(
          segment_index: idx,
          prompt: prompt,
          duration_seconds: float.round(duration_secs),
          start_seconds: start,
          camera: extract_camera(prompt),
          style: extract_style(prompt),
        )
      })
      Ok(prompts)
    }
  }
}

/// Helper: max of two integers
fn max_int(a: Int, b: Int) -> Int {
  case a > b {
    True -> a
    False -> b
  }
}
