// AI Services Tests - TDD approach
// These tests are written FIRST, implementation follows
// Run with: gleam test

import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import vibee/ai/elevenlabs
import vibee/ai/hedra
import vibee/ai/bfl
import vibee/ai/kling
import vibee/ai/heygen

pub fn main() {
  gleeunit.main()
}

// ============================================================
// ElevenLabs TTS Tests
// ============================================================

pub fn elevenlabs_config_creation_test() {
  let config =
    elevenlabs.Config(
      api_key: "test_api_key",
      voice_id: "EXAVITQu4vr4xnSDxMaL",
      model: "eleven_multilingual_v2",
    )

  should.equal(config.api_key, "test_api_key")
  should.equal(config.voice_id, "EXAVITQu4vr4xnSDxMaL")
  should.equal(config.model, "eleven_multilingual_v2")
}

pub fn elevenlabs_default_config_test() {
  let config = elevenlabs.default_config("my_api_key")

  should.equal(config.api_key, "my_api_key")
  // Should have default voice
  should.be_true(string.length(config.voice_id) > 0)
  // Should have default model
  should.be_true(string.contains(config.model, "eleven"))
}

pub fn elevenlabs_create_tts_request_test() {
  let config =
    elevenlabs.Config(
      api_key: "test_key",
      voice_id: "voice_123",
      model: "eleven_multilingual_v2",
    )

  let request = elevenlabs.create_tts_request(config, "Hello world")

  // Should create correct URL
  should.be_true(string.contains(
    request.url,
    "https://api.elevenlabs.io/v1/text-to-speech",
  ))
  should.be_true(string.contains(request.url, "voice_123"))

  // Should have API key header
  should.be_true(list.any(request.headers, fn(h) {
    case h {
      #("xi-api-key", "test_key") -> True
      _ -> False
    }
  }))

  // Should have correct content type
  should.be_true(list.any(request.headers, fn(h) {
    case h {
      #("Content-Type", "application/json") -> True
      _ -> False
    }
  }))

  // Body should contain text and model
  should.be_true(string.contains(request.body, "Hello world"))
  should.be_true(string.contains(request.body, "eleven_multilingual_v2"))
}

pub fn elevenlabs_voice_settings_test() {
  let settings = elevenlabs.VoiceSettings(stability: 0.5, similarity_boost: 0.8)

  should.equal(settings.stability, 0.5)
  should.equal(settings.similarity_boost, 0.8)
}

pub fn elevenlabs_create_tts_request_with_settings_test() {
  let config =
    elevenlabs.Config(
      api_key: "test_key",
      voice_id: "voice_123",
      model: "eleven_multilingual_v2",
    )

  let settings =
    Some(elevenlabs.VoiceSettings(stability: 0.7, similarity_boost: 0.9))

  let request =
    elevenlabs.create_tts_request_with_settings(config, "Test text", settings)

  // Body should contain voice settings
  should.be_true(string.contains(request.body, "voice_settings"))
  should.be_true(string.contains(request.body, "stability"))
}

pub fn elevenlabs_list_voices_request_test() {
  let config = elevenlabs.default_config("test_key")
  let request = elevenlabs.list_voices_request(config)

  should.equal(request.url, "https://api.elevenlabs.io/v1/voices")
  should.be_true(list.any(request.headers, fn(h) {
    case h {
      #("xi-api-key", "test_key") -> True
      _ -> False
    }
  }))
}

// ============================================================
// Hedra Avatar Tests
// ============================================================

pub fn hedra_config_creation_test() {
  let config = hedra.Config(api_key: "hedra_test_key")
  should.equal(config.api_key, "hedra_test_key")
}

pub fn hedra_avatar_request_creation_test() {
  let request =
    hedra.AvatarRequest(
      audio_url: "https://example.com/audio.mp3",
      image_url: "https://example.com/photo.jpg",
      aspect_ratio: Some("16:9"),
    )

  should.equal(request.audio_url, "https://example.com/audio.mp3")
  should.equal(request.image_url, "https://example.com/photo.jpg")
  should.equal(request.aspect_ratio, Some("16:9"))
}

pub fn hedra_create_avatar_request_test() {
  let config = hedra.Config(api_key: "hedra_key")
  let avatar_req =
    hedra.AvatarRequest(
      audio_url: "https://example.com/audio.mp3",
      image_url: "https://example.com/photo.jpg",
      aspect_ratio: None,
    )

  let request = hedra.create_avatar_request(config, avatar_req)

  // Should use correct API endpoint
  should.be_true(string.contains(request.url, "api.hedra.com"))
  should.be_true(string.contains(request.url, "characters"))

  // Should have authorization header
  should.be_true(list.any(request.headers, fn(h) {
    case h {
      #("X-API-Key", "hedra_key") -> True
      _ -> False
    }
  }))

  // Body should contain URLs
  should.be_true(string.contains(request.body, "audio.mp3"))
  should.be_true(string.contains(request.body, "photo.jpg"))
}

pub fn hedra_get_status_request_test() {
  let config = hedra.Config(api_key: "hedra_key")
  let request = hedra.get_status_request(config, "job_12345")

  // Should have job_id in URL
  should.be_true(string.contains(request.url, "job_12345"))
  should.be_true(string.contains(request.url, "api.hedra.com"))
}

pub fn hedra_job_status_types_test() {
  let pending = hedra.Pending
  let processing = hedra.Processing
  let completed = hedra.Completed("https://result.com/video.mp4")
  let failed = hedra.Failed("Error message")

  should.be_true(case pending {
    hedra.Pending -> True
    _ -> False
  })
  should.be_true(case processing {
    hedra.Processing -> True
    _ -> False
  })
  should.be_true(case completed {
    hedra.Completed(_) -> True
    _ -> False
  })
  should.be_true(case failed {
    hedra.Failed(_) -> True
    _ -> False
  })
}

// ============================================================
// BFL Image Generation Tests
// ============================================================

pub fn bfl_config_creation_test() {
  let config = bfl.Config(api_key: "bfl_test_key")
  should.equal(config.api_key, "bfl_test_key")
}

pub fn bfl_image_request_creation_test() {
  let request =
    bfl.ImageRequest(
      prompt: "A beautiful sunset over mountains",
      model: "flux-pro-1.1",
      width: Some(1024),
      height: Some(768),
      steps: Some(30),
    )

  should.equal(request.prompt, "A beautiful sunset over mountains")
  should.equal(request.model, "flux-pro-1.1")
  should.equal(request.width, Some(1024))
}

pub fn bfl_generate_image_request_test() {
  let config = bfl.Config(api_key: "bfl_key")
  let img_req =
    bfl.ImageRequest(
      prompt: "A cat in space",
      model: "flux-pro-1.1",
      width: None,
      height: None,
      steps: None,
    )

  let request = bfl.generate_image_request(config, img_req)

  // Should use BFL API
  should.be_true(string.contains(request.url, "api.bfl.ml"))
  should.be_true(string.contains(request.url, "flux-pro"))

  // Should have API key header
  should.be_true(list.any(request.headers, fn(h) {
    case h {
      #("X-Key", "bfl_key") -> True
      _ -> False
    }
  }))

  // Body should contain prompt
  should.be_true(string.contains(request.body, "A cat in space"))
}

pub fn bfl_get_result_request_test() {
  let config = bfl.Config(api_key: "bfl_key")
  let request = bfl.get_result_request(config, "task_abc123")

  should.be_true(string.contains(request.url, "task_abc123"))
  should.be_true(string.contains(request.url, "get_result"))
}

pub fn bfl_supported_models_test() {
  let models = bfl.supported_models()

  should.be_true(list.contains(models, "flux-pro-1.1"))
  should.be_true(list.contains(models, "flux-dev"))
  should.be_true(list.length(models) >= 2)
}

// ============================================================
// Kling AI Video Tests
// ============================================================

pub fn kling_config_creation_test() {
  let config =
    kling.Config(access_key: "kling_access", secret_key: "kling_secret")

  should.equal(config.access_key, "kling_access")
  should.equal(config.secret_key, "kling_secret")
}

pub fn kling_video_request_creation_test() {
  let request =
    kling.VideoRequest(
      prompt: "A cat walking on the moon",
      mode: "std",
      duration: "5",
      aspect_ratio: Some("16:9"),
    )

  should.equal(request.prompt, "A cat walking on the moon")
  should.equal(request.mode, "std")
  should.equal(request.duration, "5")
}

pub fn kling_create_video_request_test() {
  let config =
    kling.Config(access_key: "kling_access", secret_key: "kling_secret")

  let video_req =
    kling.VideoRequest(
      prompt: "Ocean waves",
      mode: "std",
      duration: "5",
      aspect_ratio: None,
    )

  // Use test timestamp
  let timestamp = 1_700_000_000
  let request = kling.create_video_request(config, video_req, timestamp)

  // Should use Kling API
  should.be_true(string.contains(request.url, "api.klingai.com"))
  should.be_true(string.contains(request.url, "text2video"))

  // Should have JWT auth header (Kling uses JWT)
  should.be_true(list.any(request.headers, fn(h) {
    case h {
      #("Authorization", auth) -> string.starts_with(auth, "Bearer ")
      _ -> False
    }
  }))

  // Body should contain prompt
  should.be_true(string.contains(request.body, "Ocean waves"))
}

pub fn kling_image_to_video_request_test() {
  let config =
    kling.Config(access_key: "kling_access", secret_key: "kling_secret")

  let i2v_req =
    kling.ImageToVideoRequest(
      image_url: "https://example.com/image.jpg",
      prompt: Some("Make the image come alive"),
      duration: "5",
    )

  let timestamp = 1_700_000_000
  let request = kling.image_to_video_request(config, i2v_req, timestamp)

  // Should use image2video endpoint
  should.be_true(string.contains(request.url, "image2video"))
  should.be_true(string.contains(request.body, "image.jpg"))
}

pub fn kling_get_task_request_test() {
  let config =
    kling.Config(access_key: "kling_access", secret_key: "kling_secret")

  let timestamp = 1_700_000_000
  let request = kling.get_task_request(config, "task_xyz789", timestamp)

  should.be_true(string.contains(request.url, "task_xyz789"))
}

pub fn kling_supported_modes_test() {
  let modes = kling.supported_modes()

  should.be_true(list.contains(modes, "std"))
  should.be_true(list.contains(modes, "pro"))
}

// ============================================================
// HeyGen Avatar Tests
// ============================================================

pub fn heygen_config_creation_test() {
  let config = heygen.Config(api_key: "heygen_test_key")
  should.equal(config.api_key, "heygen_test_key")
}

pub fn heygen_video_request_creation_test() {
  let request =
    heygen.VideoRequest(
      avatar_id: "avatar_123",
      script: "Hello, this is a test video",
      voice_id: Some("voice_456"),
      background_url: None,
    )

  should.equal(request.avatar_id, "avatar_123")
  should.equal(request.script, "Hello, this is a test video")
}

pub fn heygen_create_video_request_test() {
  let config = heygen.Config(api_key: "heygen_key")
  let video_req =
    heygen.VideoRequest(
      avatar_id: "avatar_123",
      script: "Test script",
      voice_id: None,
      background_url: None,
    )

  let request = heygen.create_video_request(config, video_req)

  // Should use HeyGen API
  should.be_true(string.contains(request.url, "api.heygen.com"))
  should.be_true(string.contains(request.url, "video"))

  // Should have API key header
  should.be_true(list.any(request.headers, fn(h) {
    case h {
      #("X-Api-Key", "heygen_key") -> True
      _ -> False
    }
  }))

  // Body should contain avatar_id and script
  should.be_true(string.contains(request.body, "avatar_123"))
  should.be_true(string.contains(request.body, "Test script"))
}

pub fn heygen_list_avatars_request_test() {
  let config = heygen.Config(api_key: "heygen_key")
  let request = heygen.list_avatars_request(config)

  should.be_true(string.contains(request.url, "avatars"))
  should.be_true(string.contains(request.url, "api.heygen.com"))
}

pub fn heygen_get_video_status_request_test() {
  let config = heygen.Config(api_key: "heygen_key")
  let request = heygen.get_video_status_request(config, "video_abc")

  should.be_true(string.contains(request.url, "video_abc"))
}
