// HeyGen Avatar Video Integration
// API Documentation: https://docs.heygen.com/reference/create-an-avatar-video-v2

import gleam/json
import gleam/option.{type Option, None, Some}

// ============================================================
// Types
// ============================================================

pub type Config {
  Config(api_key: String)
}

pub type Request {
  Request(
    url: String,
    method: String,
    headers: List(#(String, String)),
    body: String,
  )
}

// ============================================================
// Video Request Types (v2 API)
// ============================================================

/// Voice settings for video generation
pub type VoiceSettings {
  VoiceSettings(
    voice_id: String,
    input_text: String,
    speed: Option(Float),
    pitch: Option(Int),
    emotion: Option(String),
  )
}

/// Character/Avatar settings
pub type CharacterSettings {
  CharacterSettings(
    avatar_id: String,
    avatar_style: Option(String),
    scale: Option(Float),
  )
}

/// Background configuration
pub type Background {
  ColorBackground(color: String)
  ImageBackground(url: String)
  VideoBackground(url: String)
}

/// Single video input combining character, voice, and background
pub type VideoInput {
  VideoInput(
    character: CharacterSettings,
    voice: VoiceSettings,
    background: Option(Background),
  )
}

/// Video dimensions
pub type Dimension {
  Dimension(width: Int, height: Int)
}

/// Full v2 video creation request
pub type CreateVideoRequestV2 {
  CreateVideoRequestV2(
    video_inputs: List(VideoInput),
    dimension: Option(Dimension),
    caption: Option(Bool),
    title: Option(String),
    callback_id: Option(String),
    callback_url: Option(String),
  )
}

/// Simple video request (backward compatible)
pub type VideoRequest {
  VideoRequest(
    avatar_id: String,
    script: String,
    voice_id: Option(String),
    background_url: Option(String),
  )
}

// ============================================================
// Response Types
// ============================================================

pub type VideoStatus {
  Pending
  Processing
  Completed(video_url: String)
  Failed(error: String)
}

pub type Video {
  Video(video_id: String, status: VideoStatus)
}

pub type Avatar {
  Avatar(
    avatar_id: String,
    name: String,
    preview_url: String,
    gender: String,
  )
}

pub type Voice {
  Voice(voice_id: String, name: String, language: String, gender: String)
}

// ============================================================
// Request Builders
// ============================================================

/// Create a full v2 video generation request with all options
pub fn create_video_v2_request(
  config: Config,
  req: CreateVideoRequestV2,
) -> Request {
  let video_inputs_json =
    json.array(req.video_inputs, fn(input) { encode_video_input(input) })

  let base_parts = [#("video_inputs", video_inputs_json)]

  let with_dimension = case req.dimension {
    Some(dim) -> [
      #(
        "dimension",
        json.object([
          #("width", json.int(dim.width)),
          #("height", json.int(dim.height)),
        ]),
      ),
      ..base_parts
    ]
    None -> base_parts
  }

  let with_caption = case req.caption {
    Some(c) -> [#("caption", json.bool(c)), ..with_dimension]
    None -> with_dimension
  }

  let with_title = case req.title {
    Some(t) -> [#("title", json.string(t)), ..with_caption]
    None -> with_caption
  }

  let with_callback_id = case req.callback_id {
    Some(id) -> [#("callback_id", json.string(id)), ..with_title]
    None -> with_title
  }

  let final_parts = case req.callback_url {
    Some(url) -> [#("callback_url", json.string(url)), ..with_callback_id]
    None -> with_callback_id
  }

  let body = json.to_string(json.object(final_parts))

  Request(
    url: "https://api.heygen.com/v2/video/generate",
    method: "POST",
    headers: [
      #("X-Api-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Encode a single video input to JSON
fn encode_video_input(input: VideoInput) -> json.Json {
  let character_parts = [
    #("type", json.string("avatar")),
    #("avatar_id", json.string(input.character.avatar_id)),
  ]

  let character_with_style = case input.character.avatar_style {
    Some(style) -> [#("avatar_style", json.string(style)), ..character_parts]
    None -> character_parts
  }

  let character_final = case input.character.scale {
    Some(s) -> [#("scale", json.float(s)), ..character_with_style]
    None -> character_with_style
  }

  let voice_parts = [
    #("type", json.string("text")),
    #("voice_id", json.string(input.voice.voice_id)),
    #("input_text", json.string(input.voice.input_text)),
  ]

  let voice_with_speed = case input.voice.speed {
    Some(s) -> [#("speed", json.float(s)), ..voice_parts]
    None -> voice_parts
  }

  let voice_with_pitch = case input.voice.pitch {
    Some(p) -> [#("pitch", json.int(p)), ..voice_with_speed]
    None -> voice_with_speed
  }

  let voice_final = case input.voice.emotion {
    Some(e) -> [#("emotion", json.string(e)), ..voice_with_pitch]
    None -> voice_with_pitch
  }

  let base_input = [
    #("character", json.object(character_final)),
    #("voice", json.object(voice_final)),
  ]

  let with_background = case input.background {
    Some(bg) -> [#("background", encode_background(bg)), ..base_input]
    None -> base_input
  }

  json.object(with_background)
}

/// Encode background to JSON
fn encode_background(bg: Background) -> json.Json {
  case bg {
    ColorBackground(color) ->
      json.object([#("type", json.string("color")), #("value", json.string(color))])
    ImageBackground(url) ->
      json.object([#("type", json.string("image")), #("url", json.string(url))])
    VideoBackground(url) ->
      json.object([#("type", json.string("video")), #("url", json.string(url))])
  }
}

/// Create a request to generate a video with an avatar (simple v1-style API)
pub fn create_video_request(config: Config, req: VideoRequest) -> Request {
  let input_text =
    json.object([
      #("type", json.string("text")),
      #("input_text", json.string(req.script)),
    ])

  let voice = case req.voice_id {
    Some(v) -> json.object([#("voice_id", json.string(v))])
    None ->
      json.object([#("type", json.string("text")), #("method", json.string("auto"))])
  }

  let video_inputs =
    json.array(
      [
        json.object([
          #(
            "character",
            json.object([
              #("type", json.string("avatar")),
              #("avatar_id", json.string(req.avatar_id)),
            ]),
          ),
          #("voice", voice),
          #("script", input_text),
        ]),
      ],
      fn(x) { x },
    )

  let body_parts = [#("video_inputs", video_inputs)]

  let body_with_bg = case req.background_url {
    Some(bg) -> [
      #(
        "background",
        json.object([
          #("type", json.string("image")),
          #("url", json.string(bg)),
        ]),
      ),
      ..body_parts
    ]
    None -> body_parts
  }

  let body = json.to_string(json.object(body_with_bg))

  Request(
    url: "https://api.heygen.com/v2/video/generate",
    method: "POST",
    headers: [
      #("X-Api-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create a request to list available avatars
pub fn list_avatars_request(config: Config) -> Request {
  Request(
    url: "https://api.heygen.com/v2/avatars",
    method: "GET",
    headers: [
      #("X-Api-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Create a request to check video status
pub fn get_video_status_request(config: Config, video_id: String) -> Request {
  Request(
    url: "https://api.heygen.com/v1/video_status.get?video_id=" <> video_id,
    method: "GET",
    headers: [
      #("X-Api-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Create a request to list available voices
pub fn list_voices_request(config: Config) -> Request {
  Request(
    url: "https://api.heygen.com/v2/voices",
    method: "GET",
    headers: [
      #("X-Api-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Create a request to get video details
pub fn get_video_request(config: Config, video_id: String) -> Request {
  Request(
    url: "https://api.heygen.com/v1/video/" <> video_id,
    method: "GET",
    headers: [
      #("X-Api-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Create a request to get avatar details
pub fn get_avatar_details_request(config: Config, avatar_id: String) -> Request {
  Request(
    url: "https://api.heygen.com/v2/avatars/" <> avatar_id,
    method: "GET",
    headers: [
      #("X-Api-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Delete a video
pub fn delete_video_request(config: Config, video_id: String) -> Request {
  Request(
    url: "https://api.heygen.com/v1/video/" <> video_id,
    method: "DELETE",
    headers: [
      #("X-Api-Key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

// ============================================================
// Helper Functions
// ============================================================

/// Create a simple video request with minimal options
pub fn simple_video_request(avatar_id: String, script: String) -> VideoRequest {
  VideoRequest(
    avatar_id: avatar_id,
    script: script,
    voice_id: None,
    background_url: None,
  )
}

/// Create simple voice settings
pub fn simple_voice(voice_id: String, text: String) -> VoiceSettings {
  VoiceSettings(
    voice_id: voice_id,
    input_text: text,
    speed: None,
    pitch: None,
    emotion: None,
  )
}

/// Create voice settings with all options
pub fn voice_with_options(
  voice_id: String,
  text: String,
  speed: Float,
  pitch: Int,
  emotion: String,
) -> VoiceSettings {
  VoiceSettings(
    voice_id: voice_id,
    input_text: text,
    speed: Some(speed),
    pitch: Some(pitch),
    emotion: Some(emotion),
  )
}

/// Create simple character settings
pub fn simple_character(avatar_id: String) -> CharacterSettings {
  CharacterSettings(avatar_id: avatar_id, avatar_style: None, scale: None)
}

/// Create character settings with style
pub fn character_with_style(
  avatar_id: String,
  style: String,
  scale: Float,
) -> CharacterSettings {
  CharacterSettings(
    avatar_id: avatar_id,
    avatar_style: Some(style),
    scale: Some(scale),
  )
}

/// Create a video input from character and voice
pub fn create_video_input(
  character: CharacterSettings,
  voice: VoiceSettings,
) -> VideoInput {
  VideoInput(character: character, voice: voice, background: None)
}

/// Create a video input with background
pub fn create_video_input_with_bg(
  character: CharacterSettings,
  voice: VoiceSettings,
  background: Background,
) -> VideoInput {
  VideoInput(character: character, voice: voice, background: Some(background))
}

/// Create a simple v2 request from a single video input
pub fn simple_v2_request(video_input: VideoInput) -> CreateVideoRequestV2 {
  CreateVideoRequestV2(
    video_inputs: [video_input],
    dimension: None,
    caption: None,
    title: None,
    callback_id: None,
    callback_url: None,
  )
}

/// Create a v2 request with dimension
pub fn v2_request_with_dimension(
  video_inputs: List(VideoInput),
  width: Int,
  height: Int,
) -> CreateVideoRequestV2 {
  CreateVideoRequestV2(
    video_inputs: video_inputs,
    dimension: Some(Dimension(width: width, height: height)),
    caption: None,
    title: None,
    callback_id: None,
    callback_url: None,
  )
}

/// Create default config
pub fn default_config(api_key: String) -> Config {
  Config(api_key: api_key)
}

// ============================================================
// Available Options
// ============================================================

/// Supported avatar styles
pub fn supported_avatar_styles() -> List(String) {
  ["normal", "circle", "closeUp"]
}

/// Supported emotions
pub fn supported_emotions() -> List(String) {
  ["default", "excited", "friendly", "serious", "soothing", "broadcaster"]
}

/// Common video dimensions
pub fn common_dimensions() -> List(#(String, Dimension)) {
  [
    #("1080p", Dimension(width: 1920, height: 1080)),
    #("720p", Dimension(width: 1280, height: 720)),
    #("square", Dimension(width: 1080, height: 1080)),
    #("portrait", Dimension(width: 1080, height: 1920)),
  ]
}
