// AI Generation Workflows and Pipelines
// High-level orchestration of AI services for complex tasks
// Based on patterns from ai-server

import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// ============================================================
// Generation Modes
// ============================================================

/// Available generation modes
pub type GenerationMode {
  NeuroPhoto
  TextToVideo
  ImageToVideo
  Morphing
  BRoll
  AvatarVideo
  VoiceClone
}

/// Get mode display name
pub fn mode_name(mode: GenerationMode, is_ru: Bool) -> String {
  case mode, is_ru {
    NeuroPhoto, True -> "Нейрофото"
    NeuroPhoto, False -> "NeuroPhoto"
    TextToVideo, True -> "Текст в видео"
    TextToVideo, False -> "Text to Video"
    ImageToVideo, True -> "Изображение в видео"
    ImageToVideo, False -> "Image to Video"
    Morphing, True -> "Морфинг"
    Morphing, False -> "Morphing"
    BRoll, True -> "B-Roll генерация"
    BRoll, False -> "B-Roll Generation"
    AvatarVideo, True -> "Аватар видео"
    AvatarVideo, False -> "Avatar Video"
    VoiceClone, True -> "Клонирование голоса"
    VoiceClone, False -> "Voice Clone"
  }
}

// ============================================================
// Aspect Ratios
// ============================================================

/// Supported aspect ratios
pub type AspectRatio {
  Square
  // 1:1
  Landscape
  // 16:9
  Portrait
  // 9:16
  Standard4x3
  // 4:3
  Ultrawide
  // 21:9
}

/// Parse aspect ratio from string
pub fn parse_aspect_ratio(s: String) -> AspectRatio {
  case s {
    "1:1" -> Square
    "16:9" -> Landscape
    "9:16" -> Portrait
    "4:3" -> Standard4x3
    "21:9" -> Ultrawide
    _ -> Square
  }
}

/// Get dimensions for aspect ratio
pub fn aspect_ratio_dimensions(ratio: AspectRatio) -> #(Int, Int) {
  case ratio {
    Square -> #(1024, 1024)
    Landscape -> #(1344, 768)
    Portrait -> #(768, 1344)
    Standard4x3 -> #(1152, 864)
    Ultrawide -> #(1536, 640)
  }
}

/// Aspect ratio to string
pub fn aspect_ratio_string(ratio: AspectRatio) -> String {
  case ratio {
    Square -> "1:1"
    Landscape -> "16:9"
    Portrait -> "9:16"
    Standard4x3 -> "4:3"
    Ultrawide -> "21:9"
  }
}

// ============================================================
// NeuroPhoto Types and Prompts
// ============================================================

/// Gender for prompt customization
pub type Gender {
  Male
  Female
  Unknown
}

/// Parse gender from string
pub fn parse_gender(s: String) -> Gender {
  case string.lowercase(s) {
    "male" | "m" | "мужской" -> Male
    "female" | "f" | "женский" -> Female
    _ -> Unknown
  }
}

/// NeuroPhoto generation request
pub type NeuroPhotoRequest {
  NeuroPhotoRequest(
    prompt: String,
    model_url: String,
    num_images: Int,
    aspect_ratio: AspectRatio,
    gender: Option(Gender),
    seed: Option(Int),
  )
}

/// Build enhanced prompt for NeuroPhoto
pub fn build_neuro_photo_prompt(base_prompt: String, gender: Gender) -> String {
  let gender_desc = case gender {
    Male -> "handsome man, masculine features"
    Female -> "beautiful woman, feminine features"
    Unknown -> "person"
  }

  "Fashionable "
  <> gender_desc
  <> ": "
  <> base_prompt
  <> ". Cinematic Lighting, realistic, intricate details, extremely detailed, incredible details, full colored, complex details, insanely detailed and intricate, hypermaximalist, extremely detailed with rich colors. Masterpiece, best quality, HDR, UHD, unreal engine, fair skin, beautiful face, Rich in details, high quality, gorgeous, glamorous, 8K, super detail, gorgeous light and shadow, detailed decoration, detailed lines."
}

/// Default NeuroPhoto negative prompt
pub fn neuro_photo_negative_prompt() -> String {
  "nsfw, erotic, violence, bad anatomy, bad hands, missing fingers, extra digit, fewer digits, cropped, worst quality, low quality, normal quality, jpeg artifacts, signature, watermark, username, blurry, artist name, poorly drawn face, bad face, fused face, cloned face, big face, long face, bad eyes, fused eyes, partially rendered objects, deformed or partially rendered eyes, deformed body, deformed hands, deformed legs"
}

/// Default NeuroPhoto input parameters
pub fn neuro_photo_default_input(
  prompt: String,
  gender: Gender,
  aspect_ratio: AspectRatio,
) -> json.Json {
  let #(width, height) = aspect_ratio_dimensions(aspect_ratio)
  let enhanced_prompt = build_neuro_photo_prompt(prompt, gender)

  json.object([
    #("prompt", json.string(enhanced_prompt)),
    #("negative_prompt", json.string(neuro_photo_negative_prompt())),
    #("num_inference_steps", json.int(40)),
    #("guidance_scale", json.float(3.0)),
    #("lora_scale", json.float(1.0)),
    #("megapixels", json.string("1")),
    #("output_quality", json.int(80)),
    #("prompt_strength", json.float(0.8)),
    #("extra_lora_scale", json.float(1.0)),
    #("go_fast", json.bool(False)),
    #("width", json.int(width)),
    #("height", json.int(height)),
    #("sampler", json.string("flowmatch")),
    #("num_outputs", json.int(1)),
    #("aspect_ratio", json.string(aspect_ratio_string(aspect_ratio))),
  ])
}

// ============================================================
// Morphing Types
// ============================================================

/// Morphing type
pub type MorphingType {
  /// Smooth transitions between images
  Seamless
  /// Loop back to first image
  Loop
}

/// Morphing request
pub type MorphingRequest {
  MorphingRequest(
    images: List(String),
    morphing_type: MorphingType,
    model: String,
  )
}

/// Morphing job status
pub type MorphingStatus {
  MorphPending
  MorphProcessing(current_pair: Int, total_pairs: Int)
  MorphConcatenating
  MorphComplete(video_url: String)
  MorphFailed(error: String)
}

/// Calculate number of pairs for morphing
pub fn morphing_pairs_count(
  image_count: Int,
  morphing_type: MorphingType,
) -> Int {
  case morphing_type {
    Seamless -> image_count - 1
    Loop ->
      case image_count > 2 {
        True -> image_count
        // includes last->first pair
        False -> image_count - 1
      }
  }
}

/// Estimate morphing processing time in minutes
pub fn estimate_morphing_time(pairs: Int) -> #(Int, Int) {
  // Each pair takes 5-15 minutes
  #(pairs * 5, pairs * 15)
}

// ============================================================
// B-Roll Generation
// ============================================================

/// Transcription word with timing
pub type TranscriptionWord {
  TranscriptionWord(text: String, start: Float, end: Float)
}

/// Transcription segment
pub type TranscriptionSegment {
  TranscriptionSegment(text: String, start: Float, end: Float)
}

/// Full transcription result
pub type Transcription {
  Transcription(
    id: String,
    text: String,
    segments: List(TranscriptionSegment),
    words: List(TranscriptionWord),
    language_code: Option(String),
    confidence: Option(Float),
  )
}

/// B-Roll segment with video prompt
pub type BRollSegment {
  BRollSegment(
    id: String,
    layer_id: Option(String),
    start: Float,
    end: Float,
    veo3_prompt: String,
  )
}

/// B-Roll generation result
pub type BRollResult {
  BRollResult(segments: List(BRollSegment), transcription_id: String)
}

/// System prompt for B-Roll AI generation
pub fn broll_system_prompt() -> String {
  "You are a creative director and expert visual strategist specializing in generating b-roll concepts for short-form video content (9:16 vertical format).

Your task is to analyze an input text and break it down into key moments with strong visual potential. For each moment, you will create a descriptive prompt for a text-to-video AI model (like Google Veo) to generate a compelling b-roll clip.

**GUIDELINES:**
1. **Identify 3-5 Core Concepts:** Do not create a new idea for every sentence. Instead, group related sentences into larger, more impactful visual concepts.
2. **Be Cinematic:** Use cinematic language like \"slow motion,\" \"extreme close-up,\" \"dynamic tracking shot,\" \"cinematic lighting,\" \"rack focus,\" \"hyperlapse,\" etc.
3. **Think Metaphorically:** Translate abstract concepts into powerful visual metaphors.
4. **Optimize for Vertical:** Ensure the descriptions work well in a 9:16 aspect ratio.

**OUTPUT FORMAT:**
Return a JSON array of objects with:
- \"text_part\": The exact segment of original text
- \"veo3_prompt\": A highly descriptive, cinematic prompt for video generation"
}

/// Build cinematic B-Roll prompt from text segment
pub fn build_broll_prompt(text_segment: String) -> String {
  "Generate a cinematic b-roll video scene for: " <> text_segment
}

/// Find matching transcription segment for text
pub fn find_matching_segment(
  text_part: String,
  segments: List(TranscriptionSegment),
) -> Option(TranscriptionSegment) {
  let text_lower = string.lowercase(string.trim(text_part))

  // First try exact substring match
  let exact_match =
    list.find(segments, fn(seg) {
      let seg_text = string.lowercase(string.trim(seg.text))
      string.contains(seg_text, text_lower)
    })

  case exact_match {
    Ok(seg) -> Some(seg)
    Error(_) -> {
      // Try word-based fuzzy matching
      let text_words = string.split(text_lower, " ")
      let text_word_count = list.length(text_words)

      let best =
        list.fold(segments, #(None, 0), fn(acc, seg) {
          let seg_text = string.lowercase(string.trim(seg.text))
          let seg_words = string.split(seg_text, " ")

          // Count matching words
          let matches =
            list.fold(text_words, 0, fn(count, word) {
              case list.contains(seg_words, word) {
                True -> count + 1
                False -> count
              }
            })

          // Require at least 50% match
          case matches > acc.1 && matches >= text_word_count / 2 {
            True -> #(Some(seg), matches)
            False -> acc
          }
        })

      best.0
    }
  }
}

// ============================================================
// Image-to-Video Types
// ============================================================

/// Image to video request
pub type ImageToVideoRequest {
  ImageToVideoRequest(
    image_url: String,
    prompt: String,
    model: String,
    aspect_ratio: AspectRatio,
  )
}

/// Morphing image pair request
pub type MorphingPairRequest {
  MorphingPairRequest(
    image_a_url: String,
    image_b_url: String,
    model: String,
  )
}

/// Text to video request
pub type TextToVideoRequest {
  TextToVideoRequest(prompt: String, model: String, aspect_ratio: AspectRatio)
}

// ============================================================
// Video Model Configuration
// ============================================================

/// Video model info
pub type VideoModelInfo {
  VideoModelInfo(
    id: String,
    name: String,
    description: String,
    supports_image_input: Bool,
    supports_morphing: Bool,
    typical_duration_sec: Int,
  )
}

/// Available video models
pub fn video_models() -> List(VideoModelInfo) {
  [
    VideoModelInfo(
      id: "minimax",
      name: "MiniMax",
      description: "Fast video generation",
      supports_image_input: True,
      supports_morphing: True,
      typical_duration_sec: 5,
    ),
    VideoModelInfo(
      id: "kling",
      name: "Kling AI",
      description: "High quality video with motion",
      supports_image_input: True,
      supports_morphing: True,
      typical_duration_sec: 5,
    ),
    VideoModelInfo(
      id: "runway",
      name: "Runway Gen-3",
      description: "Cinematic quality",
      supports_image_input: True,
      supports_morphing: False,
      typical_duration_sec: 4,
    ),
    VideoModelInfo(
      id: "luma",
      name: "Luma Dream Machine",
      description: "Creative video generation",
      supports_image_input: True,
      supports_morphing: True,
      typical_duration_sec: 5,
    ),
  ]
}

/// Find video model by ID
pub fn find_video_model(id: String) -> Option(VideoModelInfo) {
  list.find(video_models(), fn(m) { m.id == id })
  |> option.from_result
}

// ============================================================
// Avatar Video Types
// ============================================================

/// Avatar video request combining TTS + Avatar
pub type AvatarVideoRequest {
  AvatarVideoRequest(
    text: String,
    voice_id: String,
    avatar_id: String,
    avatar_service: AvatarService,
    aspect_ratio: AspectRatio,
  )
}

/// Avatar service provider
pub type AvatarService {
  HedraAvatar
  HeyGenAvatar
}

/// Avatar video pipeline steps
pub type AvatarPipelineStep {
  GeneratingAudio
  UploadingAudio
  GeneratingVideo
  ProcessingResult
  Complete
}

// ============================================================
// Workflow Result Types
// ============================================================

/// Generic workflow result
pub type WorkflowResult {
  WorkflowResult(
    success: Bool,
    mode: GenerationMode,
    result_url: Option(String),
    error: Option(String),
    processing_time_ms: Int,
    metadata: Option(json.Json),
  )
}

/// Create success result
pub fn workflow_success(
  mode: GenerationMode,
  url: String,
  time_ms: Int,
) -> WorkflowResult {
  WorkflowResult(
    success: True,
    mode: mode,
    result_url: Some(url),
    error: None,
    processing_time_ms: time_ms,
    metadata: None,
  )
}

/// Create failure result
pub fn workflow_failure(
  mode: GenerationMode,
  error: String,
  time_ms: Int,
) -> WorkflowResult {
  WorkflowResult(
    success: False,
    mode: mode,
    result_url: None,
    error: Some(error),
    processing_time_ms: time_ms,
    metadata: None,
  )
}

// ============================================================
// Cost Calculation (for reference)
// ============================================================

/// Base costs per generation mode (in stars)
pub fn base_cost(mode: GenerationMode) -> Float {
  case mode {
    NeuroPhoto -> 1.0
    TextToVideo -> 10.0
    ImageToVideo -> 8.0
    Morphing -> 15.0
    BRoll -> 12.0
    AvatarVideo -> 20.0
    VoiceClone -> 5.0
  }
}

/// Calculate total cost for batch generation
pub fn calculate_cost(mode: GenerationMode, count: Int) -> Float {
  base_cost(mode) *. int_to_float(count)
}

fn int_to_float(i: Int) -> Float {
  case i {
    0 -> 0.0
    1 -> 1.0
    2 -> 2.0
    3 -> 3.0
    4 -> 4.0
    5 -> 5.0
    _ -> int_to_float(i - 1) +. 1.0
  }
}
