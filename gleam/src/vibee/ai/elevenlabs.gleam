// ElevenLabs TTS Integration
// API Documentation: https://elevenlabs.io/docs/api-reference

import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}

// ============================================================
// Types
// ============================================================

pub type Config {
  Config(api_key: String, voice_id: String, model: String)
}

pub type VoiceSettings {
  VoiceSettings(stability: Float, similarity_boost: Float)
}

pub type Request {
  Request(
    url: String,
    method: String,
    headers: List(#(String, String)),
    body: String,
  )
}

pub type Voice {
  Voice(
    voice_id: String,
    name: String,
    category: String,
    labels: List(String),
  )
}

// ============================================================
// Config Functions
// ============================================================

/// Create a default config with the given API key
pub fn default_config(api_key: String) -> Config {
  Config(
    api_key: api_key,
    voice_id: "EXAVITQu4vr4xnSDxMaL",
    // Default: Sarah
    model: "eleven_multilingual_v2",
  )
}

// ============================================================
// Request Builders
// ============================================================

/// Create a text-to-speech request
pub fn create_tts_request(config: Config, text: String) -> Request {
  create_tts_request_with_settings(config, text, None)
}

/// Create a text-to-speech request with voice settings
pub fn create_tts_request_with_settings(
  config: Config,
  text: String,
  settings: Option(VoiceSettings),
) -> Request {
  let url =
    "https://api.elevenlabs.io/v1/text-to-speech/" <> config.voice_id

  let body_parts = [
    #("text", json.string(text)),
    #("model_id", json.string(config.model)),
  ]

  let body_with_settings = case settings {
    Some(s) -> {
      let voice_settings =
        json.object([
          #("stability", json.float(s.stability)),
          #("similarity_boost", json.float(s.similarity_boost)),
        ])
      list.append(body_parts, [#("voice_settings", voice_settings)])
    }
    None -> body_parts
  }

  let body = json.to_string(json.object(body_with_settings))

  Request(
    url: url,
    method: "POST",
    headers: [
      #("xi-api-key", config.api_key),
      #("Content-Type", "application/json"),
      #("Accept", "audio/mpeg"),
    ],
    body: body,
  )
}

/// Create a request to list available voices
pub fn list_voices_request(config: Config) -> Request {
  Request(
    url: "https://api.elevenlabs.io/v1/voices",
    method: "GET",
    headers: [
      #("xi-api-key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Create a request to get voice details
pub fn get_voice_request(config: Config, voice_id: String) -> Request {
  Request(
    url: "https://api.elevenlabs.io/v1/voices/" <> voice_id,
    method: "GET",
    headers: [
      #("xi-api-key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Create a request to clone a voice
pub fn clone_voice_request(
  config: Config,
  name: String,
  description: String,
  files_base64: List(String),
) -> Request {
  let files_json =
    json.array(files_base64, fn(f) { json.string(f) })

  let body =
    json.to_string(json.object([
      #("name", json.string(name)),
      #("description", json.string(description)),
      #("files", files_json),
    ]))

  Request(
    url: "https://api.elevenlabs.io/v1/voices/add",
    method: "POST",
    headers: [
      #("xi-api-key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

// ============================================================
// Voice Cloning from URL
// ============================================================

/// Voice cloning request with audio URL
pub type VoiceCloneRequest {
  VoiceCloneRequest(
    name: String,
    description: String,
    audio_url: String,
    labels: Option(List(#(String, String))),
  )
}

/// Create voice cloning request from audio URL
/// Note: Returns JSON representation. Actual implementation needs
/// to download the file and send as multipart/form-data
pub fn clone_voice_from_url_request(
  config: Config,
  req: VoiceCloneRequest,
) -> Request {
  let labels_json = case req.labels {
    Some(labels_list) -> {
      let labels_obj =
        json.object(
          list.map(labels_list, fn(pair) {
            let #(key, value) = pair
            #(key, json.string(value))
          }),
        )
      Some(labels_obj)
    }
    None -> None
  }

  let base_parts = [
    #("name", json.string(req.name)),
    #("description", json.string(req.description)),
    #("audio_url", json.string(req.audio_url)),
  ]

  let final_parts = case labels_json {
    Some(labels) -> [#("labels", labels), ..base_parts]
    None -> base_parts
  }

  let body = json.to_string(json.object(final_parts))

  Request(
    url: "https://api.elevenlabs.io/v1/voices/add",
    method: "POST",
    headers: [
      #("xi-api-key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create simple voice clone request with defaults
pub fn simple_voice_clone(
  name: String,
  audio_url: String,
) -> VoiceCloneRequest {
  VoiceCloneRequest(
    name: name,
    description: "Voice created from Telegram voice message",
    audio_url: audio_url,
    labels: Some([#("accent", "neutral")]),
  )
}

/// Create voice clone with custom description
pub fn voice_clone_with_description(
  name: String,
  audio_url: String,
  description: String,
) -> VoiceCloneRequest {
  VoiceCloneRequest(
    name: name,
    description: description,
    audio_url: audio_url,
    labels: Some([#("accent", "neutral")]),
  )
}

/// Create full voice clone request
pub fn voice_clone_full(
  name: String,
  audio_url: String,
  description: String,
  labels: List(#(String, String)),
) -> VoiceCloneRequest {
  VoiceCloneRequest(
    name: name,
    description: description,
    audio_url: audio_url,
    labels: Some(labels),
  )
}

// ============================================================
// Available Voices (common ones)
// ============================================================

pub fn available_voices() -> List(#(String, String)) {
  [
    #("EXAVITQu4vr4xnSDxMaL", "Sarah - Soft, warm female voice"),
    #("21m00Tcm4TlvDq8ikWAM", "Rachel - Calm, clear female voice"),
    #("AZnzlk1XvdvUeBnXmlld", "Domi - Strong, confident female voice"),
    #("MF3mGyEYCl7XYWbV9V6O", "Elli - Friendly, expressive female voice"),
    #("TxGEqnHWrfWFTfGW9XjX", "Josh - Deep, warm male voice"),
    #("VR6AewLTigWG4xSOukaG", "Arnold - Strong, authoritative male voice"),
    #("pNInz6obpgDQGcFmaJgB", "Adam - Clear, professional male voice"),
    #("yoZ06aMxZJJ28mfd3POQ", "Sam - Friendly, conversational male voice"),
  ]
}

// ============================================================
// Available Models
// ============================================================

pub fn available_models() -> List(#(String, String)) {
  [
    #("eleven_multilingual_v2", "Multilingual v2 - Best quality, 29 languages"),
    #("eleven_turbo_v2", "Turbo v2 - Fast, English optimized"),
    #("eleven_monolingual_v1", "Monolingual v1 - English only, legacy"),
  ]
}

// ============================================================
// Speech-to-Text (Transcription) Types
// ============================================================

/// Transcription request for speech-to-text
pub type TranscriptionRequest {
  TranscriptionRequest(
    audio_url: Option(String),
    model: Option(String),
    language: Option(String),
    tag_audio_events: Option(Bool),
    num_speakers: Option(Int),
    timestamps_granularity: Option(String),
  )
}

/// A single word with timing information
pub type TranscriptionWord {
  TranscriptionWord(text: String, start: Float, end: Float)
}

/// Transcription response structure
pub type TranscriptionResponse {
  TranscriptionResponse(
    text: String,
    language_code: String,
    words: Option(List(TranscriptionWord)),
    confidence: Option(Float),
  )
}

/// Transcription status enum
pub type TranscriptionStatus {
  TranscriptionPending
  TranscriptionProcessing
  TranscriptionCompleted(result: TranscriptionResponse)
  TranscriptionFailed(error: String)
}

// ============================================================
// Speech-to-Text Request Builders
// ============================================================

/// Create a simple transcription request with just the model
pub fn simple_transcription_request() -> TranscriptionRequest {
  TranscriptionRequest(
    audio_url: None,
    model: None,
    language: None,
    tag_audio_events: None,
    num_speakers: None,
    timestamps_granularity: None,
  )
}

/// Create a transcription request with language hint
pub fn transcription_request_with_language(language: String) -> TranscriptionRequest {
  TranscriptionRequest(
    audio_url: None,
    model: None,
    language: Some(language),
    tag_audio_events: None,
    num_speakers: None,
    timestamps_granularity: None,
  )
}

/// Create a full transcription request with all options
pub fn transcription_request_full(
  model: String,
  language: String,
  tag_audio_events: Bool,
  num_speakers: Int,
  timestamps_granularity: String,
) -> TranscriptionRequest {
  TranscriptionRequest(
    audio_url: None,
    model: Some(model),
    language: Some(language),
    tag_audio_events: Some(tag_audio_events),
    num_speakers: Some(num_speakers),
    timestamps_granularity: Some(timestamps_granularity),
  )
}

/// Create a speech-to-text transcription request
/// Note: This returns form-data structure info since actual multipart
/// encoding needs to be done by the HTTP client
pub fn create_transcription_request(
  config: Config,
  req: TranscriptionRequest,
) -> Request {
  // Build form data JSON representation
  // The actual HTTP client will need to handle multipart/form-data encoding
  let model = case req.model {
    Some(m) -> m
    None -> "scribe_v1"
  }

  let body_parts = [#("model_id", json.string(model))]

  let with_language = case req.language {
    Some(lang) -> [#("language_code", json.string(lang)), ..body_parts]
    None -> body_parts
  }

  let with_tag_events = case req.tag_audio_events {
    Some(tag) -> [#("tag_audio_events", json.bool(tag)), ..with_language]
    None -> with_language
  }

  let with_num_speakers = case req.num_speakers {
    Some(n) -> [
      #("num_speakers", json.int(n)),
      ..with_tag_events
    ]
    None -> with_tag_events
  }

  let final_parts = case req.timestamps_granularity {
    Some(g) -> [#("timestamps_granularity", json.string(g)), ..with_num_speakers]
    None -> with_num_speakers
  }

  let body = json.to_string(json.object(final_parts))

  Request(
    url: "https://api.elevenlabs.io/v1/speech-to-text",
    method: "POST",
    headers: [
      #("xi-api-key", config.api_key),
      // Note: Content-Type should be multipart/form-data with audio file
      // This is a placeholder - actual implementation needs form encoding
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create a transcription request with audio URL
/// ElevenLabs also supports audio_url parameter for remote files
pub fn create_transcription_from_url_request(
  config: Config,
  audio_url: String,
  req: TranscriptionRequest,
) -> Request {
  let model = case req.model {
    Some(m) -> m
    None -> "scribe_v1"
  }

  let body_parts = [
    #("model_id", json.string(model)),
    #("audio_url", json.string(audio_url)),
  ]

  let with_language = case req.language {
    Some(lang) -> [#("language_code", json.string(lang)), ..body_parts]
    None -> body_parts
  }

  let with_tag_events = case req.tag_audio_events {
    Some(tag) -> [#("tag_audio_events", json.bool(tag)), ..with_language]
    None -> with_language
  }

  let with_num_speakers = case req.num_speakers {
    Some(n) -> [
      #("num_speakers", json.int(n)),
      ..with_tag_events
    ]
    None -> with_tag_events
  }

  let final_parts = case req.timestamps_granularity {
    Some(g) -> [#("timestamps_granularity", json.string(g)), ..with_num_speakers]
    None -> with_num_speakers
  }

  let body = json.to_string(json.object(final_parts))

  Request(
    url: "https://api.elevenlabs.io/v1/speech-to-text",
    method: "POST",
    headers: [
      #("xi-api-key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

// ============================================================
// Extended Voice Settings (Full API Support)
// ============================================================

/// Full voice settings with all ElevenLabs options
pub type VoiceSettingsFull {
  VoiceSettingsFull(
    stability: Float,
    similarity_boost: Float,
    style: Option(Float),
    use_speaker_boost: Option(Bool),
    speed: Option(Float),
  )
}

/// Create default voice settings
pub fn default_voice_settings() -> VoiceSettings {
  VoiceSettings(stability: 0.5, similarity_boost: 0.75)
}

/// Create full voice settings with all options
pub fn full_voice_settings(
  stability: Float,
  similarity_boost: Float,
  style: Float,
  use_speaker_boost: Bool,
  speed: Float,
) -> VoiceSettingsFull {
  VoiceSettingsFull(
    stability: stability,
    similarity_boost: similarity_boost,
    style: Some(style),
    use_speaker_boost: Some(use_speaker_boost),
    speed: Some(speed),
  )
}

/// Create TTS request with full voice settings
pub fn create_tts_request_full(
  config: Config,
  text: String,
  settings: VoiceSettingsFull,
  output_format: Option(String),
  seed: Option(Int),
) -> Request {
  let url =
    "https://api.elevenlabs.io/v1/text-to-speech/" <> config.voice_id

  let voice_settings_parts = [
    #("stability", json.float(settings.stability)),
    #("similarity_boost", json.float(settings.similarity_boost)),
  ]

  let with_style = case settings.style {
    Some(s) -> [#("style", json.float(s)), ..voice_settings_parts]
    None -> voice_settings_parts
  }

  let with_speaker_boost = case settings.use_speaker_boost {
    Some(b) -> [#("use_speaker_boost", json.bool(b)), ..with_style]
    None -> with_style
  }

  let voice_settings_final = case settings.speed {
    Some(sp) -> [#("speed", json.float(sp)), ..with_speaker_boost]
    None -> with_speaker_boost
  }

  let voice_settings_json = json.object(voice_settings_final)

  let body_parts = [
    #("text", json.string(text)),
    #("model_id", json.string(config.model)),
    #("voice_settings", voice_settings_json),
  ]

  let with_output_format = case output_format {
    Some(fmt) -> [#("output_format", json.string(fmt)), ..body_parts]
    None -> body_parts
  }

  let final_parts = case seed {
    Some(s) -> [#("seed", json.int(s)), ..with_output_format]
    None -> with_output_format
  }

  let body = json.to_string(json.object(final_parts))

  Request(
    url: url,
    method: "POST",
    headers: [
      #("xi-api-key", config.api_key),
      #("Content-Type", "application/json"),
      #("Accept", "audio/mpeg"),
    ],
    body: body,
  )
}

// ============================================================
// Available STT Models
// ============================================================

/// Available speech-to-text models
pub fn available_stt_models() -> List(#(String, String)) {
  [
    #("scribe_v1", "Scribe v1 - Default STT model with word timestamps"),
    #("scribe_v1_turbo", "Scribe v1 Turbo - Faster transcription"),
  ]
}

/// Supported timestamp granularities
pub fn timestamp_granularities() -> List(String) {
  ["word", "sentence", "segment"]
}

/// Supported output formats for TTS
pub fn available_output_formats() -> List(#(String, String)) {
  [
    #("mp3_44100_128", "MP3 44.1kHz 128kbps - Default"),
    #("mp3_22050_32", "MP3 22.05kHz 32kbps - Low quality"),
    #("pcm_16000", "PCM 16kHz - Raw audio"),
    #("pcm_22050", "PCM 22.05kHz - Raw audio"),
    #("pcm_24000", "PCM 24kHz - Raw audio"),
    #("pcm_44100", "PCM 44.1kHz - Raw audio"),
    #("ulaw_8000", "μ-law 8kHz - Telephony"),
  ]
}

// ============================================================
// User/Account Info
// ============================================================

/// Create a request to get user subscription info
pub fn get_subscription_request(config: Config) -> Request {
  Request(
    url: "https://api.elevenlabs.io/v1/user/subscription",
    method: "GET",
    headers: [
      #("xi-api-key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Create a request to get user info
pub fn get_user_request(config: Config) -> Request {
  Request(
    url: "https://api.elevenlabs.io/v1/user",
    method: "GET",
    headers: [
      #("xi-api-key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Create a request to delete a voice
pub fn delete_voice_request(config: Config, voice_id: String) -> Request {
  Request(
    url: "https://api.elevenlabs.io/v1/voices/" <> voice_id,
    method: "DELETE",
    headers: [
      #("xi-api-key", config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}
