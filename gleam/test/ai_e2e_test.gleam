// AI Services E2E Tests
// Comprehensive end-to-end tests for all AI service modules
// Tests request building, body generation, headers, and URLs

import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import vibee/ai/bfl
import vibee/ai/elevenlabs
import vibee/ai/hedra
import vibee/ai/heygen
import vibee/ai/kieai
import vibee/ai/kling
import vibee/ai/openai
import vibee/ai/polling
import vibee/ai/replicate
import vibee/ai/workflows

pub fn main() {
  gleeunit.main()
}

// ============================================================
// ElevenLabs E2E Tests
// ============================================================

pub fn elevenlabs_e2e_tts_request_test() {
  let config = elevenlabs.default_config("sk-test-key-123")
  let request = elevenlabs.create_tts_request(config, "Hello, this is a test")

  // Verify URL structure
  should.be_true(string.starts_with(
    request.url,
    "https://api.elevenlabs.io/v1/text-to-speech/",
  ))

  // Verify headers
  should.be_true(has_header(request.headers, "xi-api-key", "sk-test-key-123"))
  should.be_true(has_header(request.headers, "Content-Type", "application/json"))
  should.be_true(has_header(request.headers, "Accept", "audio/mpeg"))

  // Verify body contains required fields
  should.be_true(string.contains(request.body, "\"text\""))
  should.be_true(string.contains(request.body, "Hello, this is a test"))
  should.be_true(string.contains(request.body, "\"model_id\""))
}

pub fn elevenlabs_e2e_transcription_request_test() {
  let config = elevenlabs.default_config("sk-test-key")
  let trans_req =
    elevenlabs.transcription_request_full(
      "scribe_v1",
      "en",
      True,
      2,
      "word",
    )

  let request =
    elevenlabs.create_transcription_from_url_request(
      config,
      "https://example.com/audio.mp3",
      trans_req,
    )

  should.equal(request.url, "https://api.elevenlabs.io/v1/speech-to-text")
  should.be_true(string.contains(request.body, "audio.mp3"))
  should.be_true(string.contains(request.body, "scribe_v1"))
}

pub fn elevenlabs_e2e_voice_settings_test() {
  let config = elevenlabs.default_config("sk-test")
  let settings =
    elevenlabs.full_voice_settings(0.5, 0.75, 0.3, True, 1.1)

  let request =
    elevenlabs.create_tts_request_full(
      config,
      "Test text",
      settings,
      Some("mp3_44100_128"),
      Some(12345),
    )

  should.be_true(string.contains(request.body, "stability"))
  should.be_true(string.contains(request.body, "similarity_boost"))
  should.be_true(string.contains(request.body, "style"))
  should.be_true(string.contains(request.body, "use_speaker_boost"))
  should.be_true(string.contains(request.body, "speed"))
  should.be_true(string.contains(request.body, "mp3_44100_128"))
  should.be_true(string.contains(request.body, "12345"))
}

// ============================================================
// BFL (FLUX) E2E Tests
// ============================================================

pub fn bfl_e2e_image_request_test() {
  let config = bfl.Config(api_key: "bfl-api-key-456")
  let img_req =
    bfl.ImageRequest(
      prompt: "A beautiful sunset over mountains",
      model: "flux-pro-1.1",
      width: Some(1024),
      height: Some(768),
      steps: Some(30),
    )

  let request = bfl.generate_image_request(config, img_req)

  // Verify URL
  should.equal(request.url, "https://api.bfl.ml/v1/flux-pro-1.1")
  should.equal(request.method, "POST")

  // Verify headers
  should.be_true(has_header(request.headers, "X-Key", "bfl-api-key-456"))
  should.be_true(has_header(request.headers, "Content-Type", "application/json"))

  // Verify body
  should.be_true(string.contains(request.body, "A beautiful sunset"))
  should.be_true(string.contains(request.body, "1024"))
  should.be_true(string.contains(request.body, "768"))
}

pub fn bfl_e2e_full_request_test() {
  let config = bfl.Config(api_key: "bfl-key")
  let full_req =
    bfl.full_request(
      "Cyberpunk city",
      "flux-pro-1.1-ultra",
      1536,
      640,
      40,
      42,
      5.5,
    )

  let request = bfl.generate_image_full_request(config, full_req)

  should.be_true(string.contains(request.url, "flux-pro-1.1-ultra"))
  should.be_true(string.contains(request.body, "Cyberpunk city"))
  should.be_true(string.contains(request.body, "1536"))
  should.be_true(string.contains(request.body, "\"seed\""))
  should.be_true(string.contains(request.body, "42"))
  should.be_true(string.contains(request.body, "\"guidance\""))
}

pub fn bfl_e2e_img2img_request_test() {
  let config = bfl.Config(api_key: "bfl-key")
  let i2i_req =
    bfl.simple_img2img(
      "Add snow to the scene",
      "https://example.com/image.jpg",
      0.7,
    )

  let request = bfl.image_to_image_request(config, i2i_req)

  should.be_true(string.contains(request.url, "image-to-image"))
  should.be_true(string.contains(request.body, "Add snow"))
  should.be_true(string.contains(request.body, "image.jpg"))
  should.be_true(string.contains(request.body, "strength"))
}

pub fn bfl_e2e_controlnet_request_test() {
  let config = bfl.Config(api_key: "bfl-key")
  let cn_req =
    bfl.simple_controlnet(
      "A portrait photo",
      "https://example.com/control.jpg",
      "canny",
    )

  let request = bfl.controlnet_request(config, cn_req)

  should.be_true(string.contains(request.url, "controlnet"))
  should.be_true(string.contains(request.body, "A portrait photo"))
  should.be_true(string.contains(request.body, "canny"))
  should.be_true(string.contains(request.body, "control_image"))
}

pub fn bfl_e2e_get_result_test() {
  let config = bfl.Config(api_key: "bfl-key")
  let request = bfl.get_result_request(config, "task-id-xyz")

  should.equal(
    request.url,
    "https://api.bfl.ml/v1/get_result?id=task-id-xyz",
  )
  should.equal(request.method, "GET")
}

// ============================================================
// Hedra E2E Tests
// ============================================================

pub fn hedra_e2e_avatar_request_test() {
  let config = hedra.Config(api_key: "hedra-api-key")
  let avatar_req =
    hedra.AvatarRequest(
      audio_url: "https://example.com/speech.mp3",
      image_url: "https://example.com/avatar.jpg",
      aspect_ratio: Some("16:9"),
    )

  let request = hedra.create_avatar_request(config, avatar_req)

  should.be_true(string.contains(request.url, "api.hedra.com"))
  should.be_true(string.contains(request.url, "characters"))
  should.be_true(has_header(request.headers, "X-API-Key", "hedra-api-key"))
  should.be_true(string.contains(request.body, "speech.mp3"))
  should.be_true(string.contains(request.body, "avatar.jpg"))
}

pub fn hedra_e2e_asset_upload_test() {
  let config = hedra.Config(api_key: "hedra-key")

  // Test audio asset creation
  let audio_req =
    hedra.CreateAssetRequest(name: "my_audio", asset_type: hedra.Audio)
  let request = hedra.create_asset_request(config, audio_req)

  should.be_true(string.contains(request.url, "assets"))
  should.be_true(string.contains(request.body, "my_audio"))
  should.be_true(string.contains(request.body, "audio"))
}

pub fn hedra_e2e_generation_request_test() {
  let config = hedra.Config(api_key: "hedra-key")
  let gen_req =
    hedra.GenerationRequest(
      image_asset_id: "img-asset-id",
      audio_asset_id: "audio-asset-id",
      resolution: Some("720p"),
      aspect_ratio: Some("16:9"),
    )
  let request = hedra.start_generation_request(config, gen_req)

  should.be_true(string.contains(request.url, "characters"))
  should.be_true(string.contains(request.body, "img-asset-id"))
  should.be_true(string.contains(request.body, "audio-asset-id"))
  should.be_true(string.contains(request.body, "720p"))
}

pub fn hedra_e2e_status_request_test() {
  let config = hedra.Config(api_key: "hedra-key")
  let request = hedra.get_status_request(config, "job-123")

  should.be_true(string.contains(request.url, "job-123"))
  should.be_true(string.contains(request.url, "api.hedra.com"))
}

// ============================================================
// HeyGen E2E Tests
// ============================================================

pub fn heygen_e2e_video_request_test() {
  let config = heygen.Config(api_key: "heygen-api-key")
  let video_req =
    heygen.VideoRequest(
      avatar_id: "avatar_james_001",
      script: "Hello, welcome to our service!",
      voice_id: Some("voice_english_male"),
      background_url: Some("https://example.com/bg.jpg"),
    )

  let request = heygen.create_video_request(config, video_req)

  should.be_true(string.contains(request.url, "api.heygen.com"))
  should.be_true(string.contains(request.url, "video"))
  should.be_true(has_header(request.headers, "X-Api-Key", "heygen-api-key"))
  should.be_true(string.contains(request.body, "avatar_james_001"))
  should.be_true(string.contains(request.body, "Hello, welcome"))
}

pub fn heygen_e2e_list_avatars_test() {
  let config = heygen.Config(api_key: "heygen-key")
  let request = heygen.list_avatars_request(config)

  should.be_true(string.contains(request.url, "avatars"))
  should.equal(request.method, "GET")
}

pub fn heygen_e2e_video_status_test() {
  let config = heygen.Config(api_key: "heygen-key")
  let request = heygen.get_video_status_request(config, "video-abc-123")

  should.be_true(string.contains(request.url, "video-abc-123"))
}

pub fn heygen_e2e_v2_video_request_test() {
  let config = heygen.Config(api_key: "heygen-key")
  let character =
    heygen.CharacterSettings(
      avatar_id: "avatar_001",
      avatar_style: Some("normal"),
      scale: Some(1.0),
    )

  let voice =
    heygen.VoiceSettings(
      voice_id: "voice_001",
      input_text: "Hello world",
      speed: Some(1.0),
      pitch: Some(0),
      emotion: Some("friendly"),
    )

  let background = heygen.ColorBackground("#ffffff")

  let video_input =
    heygen.VideoInput(
      character: character,
      voice: voice,
      background: Some(background),
    )

  let dimension = heygen.Dimension(width: 1080, height: 1920)

  let v2_req =
    heygen.CreateVideoRequestV2(
      video_inputs: [video_input],
      dimension: Some(dimension),
      caption: None,
      title: None,
      callback_id: None,
      callback_url: None,
    )

  let request = heygen.create_video_v2_request(config, v2_req)

  should.be_true(string.contains(request.url, "v2"))
  should.be_true(string.contains(request.url, "video"))
  should.be_true(string.contains(request.body, "avatar_001"))
  should.be_true(string.contains(request.body, "Hello world"))
  should.be_true(string.contains(request.body, "1080"))
  should.be_true(string.contains(request.body, "1920"))
}

// ============================================================
// Kling E2E Tests
// ============================================================

pub fn kling_e2e_video_request_test() {
  let config = kling.Config(access_key: "kling-access", secret_key: "kling-secret")
  let video_req =
    kling.VideoRequest(
      prompt: "A cat playing piano",
      mode: "std",
      duration: "5",
      aspect_ratio: Some("16:9"),
    )

  let timestamp = 1_700_000_000
  let request = kling.create_video_request(config, video_req, timestamp)

  should.be_true(string.contains(request.url, "api.klingai.com"))
  should.be_true(string.contains(request.url, "text2video"))

  // Should have JWT Bearer token
  should.be_true(list.any(request.headers, fn(h) {
    case h {
      #("Authorization", auth) -> string.starts_with(auth, "Bearer ")
      _ -> False
    }
  }))

  should.be_true(string.contains(request.body, "A cat playing piano"))
}

pub fn kling_e2e_image_to_video_test() {
  let config =
    kling.Config(access_key: "kling-access", secret_key: "kling-secret")
  let i2v_req =
    kling.ImageToVideoRequest(
      image_url: "https://example.com/cat.jpg",
      prompt: Some("Make it dance"),
      duration: "5",
    )

  let timestamp = 1_700_000_000
  let request = kling.image_to_video_request(config, i2v_req, timestamp)

  should.be_true(string.contains(request.url, "image2video"))
  should.be_true(string.contains(request.body, "cat.jpg"))
  should.be_true(string.contains(request.body, "Make it dance"))
}

pub fn kling_e2e_task_status_test() {
  let config =
    kling.Config(access_key: "kling-access", secret_key: "kling-secret")
  let timestamp = 1_700_000_000
  let request = kling.get_task_request(config, "task-xyz-789", timestamp)

  should.be_true(string.contains(request.url, "task-xyz-789"))
}

// ============================================================
// KIE AI (Veo3) E2E Tests
// ============================================================

pub fn kieai_e2e_video_request_test() {
  let config = kieai.Config(api_key: "kieai-api-key")
  let video_req =
    kieai.VideoRequest(
      prompt: "A drone flying over mountains",
      seeds: 12345,
      model: Some("veo3_fast"),
      aspect_ratio: Some("16:9"),
    )

  let request = kieai.create_video_request(config, video_req)

  should.be_true(string.contains(request.url, "api.kie.ai"))
  should.be_true(string.contains(request.url, "veo"))
  should.be_true(has_header_prefix(request.headers, "Authorization", "Bearer "))
  should.be_true(string.contains(request.body, "A drone flying"))
  should.be_true(string.contains(request.body, "veo3_fast"))
  should.be_true(string.contains(request.body, "16:9"))
}

pub fn kieai_e2e_simple_request_test() {
  let simple = kieai.simple_video_request("Ocean waves crashing")

  should.equal(simple.prompt, "Ocean waves crashing")
  // seeds is auto-generated, model and aspect_ratio are None
  should.equal(simple.model, None)
  should.equal(simple.aspect_ratio, None)
}

pub fn kieai_e2e_task_status_test() {
  let config = kieai.Config(api_key: "kieai-key")
  let request = kieai.get_task_status_request(config, "task-veo-123")

  should.be_true(string.contains(request.url, "record-info"))
  should.be_true(string.contains(request.url, "task-veo-123"))
}

pub fn kieai_e2e_list_videos_test() {
  let config = kieai.Config(api_key: "kieai-key")
  let request = kieai.list_videos_request(config, 20, 0)

  should.be_true(string.contains(request.url, "records"))
}

// ============================================================
// OpenAI E2E Tests
// ============================================================

pub fn openai_e2e_chat_request_test() {
  let config = openai.default_config("sk-openai-key")
  let chat_req =
    openai.ChatRequest(
      model: "gpt-4o-mini",
      messages: [
        openai.ChatMessage(role: "system", content: "You are a helpful assistant"),
        openai.ChatMessage(role: "user", content: "What is 2+2?"),
      ],
      temperature: None,
      max_tokens: None,
    )

  let request = openai.create_chat_request(config, chat_req)

  should.be_true(string.contains(request.url, "api.openai.com"))
  should.be_true(string.contains(request.url, "chat/completions"))
  should.be_true(has_header_prefix(request.headers, "Authorization", "Bearer "))
  should.be_true(string.contains(request.body, "helpful assistant"))
  should.be_true(string.contains(request.body, "What is 2+2"))
}

pub fn openai_e2e_chat_with_tools_test() {
  let config = openai.default_config("sk-openai-key")

  let tool =
    openai.function_tool(
      "get_weather",
      "Get weather for a location",
      json.object([
        #("type", json.string("object")),
        #(
          "properties",
          json.object([
            #("location", json.object([#("type", json.string("string"))])),
          ]),
        ),
      ]),
    )

  let chat_req =
    openai.ChatRequestWithTools(
      model: "gpt-4",
      messages: [openai.UserMessage("What's the weather in NYC?")],
      tools: Some([tool]),
      tool_choice: Some(openai.ToolChoiceAuto),
      temperature: None,
      max_tokens: None,
      response_format: None,
    )

  let request = openai.create_chat_with_tools_request(config, chat_req)

  should.be_true(string.contains(request.body, "get_weather"))
  should.be_true(string.contains(request.body, "function"))
  should.be_true(string.contains(request.body, "tools"))
}

pub fn openai_e2e_embedding_request_test() {
  let config = openai.default_config("sk-openai-key")
  let emb_req = openai.simple_embedding("Hello world")

  let request = openai.create_embedding_request(config, emb_req)

  should.be_true(string.contains(request.url, "embeddings"))
  should.be_true(string.contains(request.body, "Hello world"))
}

// ============================================================
// Replicate E2E Tests
// ============================================================

pub fn replicate_e2e_prediction_request_test() {
  let config = replicate.Config(api_token: "r8-replicate-key")

  let pred_req =
    replicate.PredictionRequest(
      model: "stability-ai/sdxl",
      version: Some("v1.0"),
      input: [#("prompt", json.string("A happy dog"))],
    )

  let request = replicate.create_prediction_request(config, pred_req)

  should.be_true(string.contains(request.url, "api.replicate.com"))
  should.be_true(string.contains(request.url, "predictions"))
  should.be_true(has_header_prefix(request.headers, "Authorization", "Bearer "))
  should.be_true(string.contains(request.body, "A happy dog"))
}

pub fn replicate_e2e_get_prediction_test() {
  let config = replicate.Config(api_token: "r8-key")
  let request = replicate.get_prediction_request(config, "pred-abc-123")

  should.be_true(string.contains(request.url, "pred-abc-123"))
  should.equal(request.method, "GET")
}

pub fn replicate_e2e_cancel_prediction_test() {
  let config = replicate.Config(api_token: "r8-key")
  let request = replicate.cancel_prediction_request(config, "pred-to-cancel")

  should.be_true(string.contains(request.url, "cancel"))
  should.equal(request.method, "POST")
}

pub fn replicate_e2e_list_predictions_test() {
  let config = replicate.Config(api_token: "r8-key")
  let request = replicate.list_predictions_request(config)

  should.be_true(string.contains(request.url, "predictions"))
  should.equal(request.method, "GET")
}

// ============================================================
// Polling E2E Tests
// ============================================================

pub fn polling_e2e_configs_test() {
  let default = polling.default_config()
  should.equal(default.interval_ms, 2000)
  should.equal(default.max_wait_ms, 300_000)

  let fast = polling.fast_config()
  should.equal(fast.interval_ms, 1000)
  should.equal(fast.max_wait_ms, 120_000)

  let video = polling.video_config()
  should.equal(video.max_wait_ms, 1_800_000)
  should.be_true(video.exponential_backoff)
}

pub fn polling_e2e_service_params_test() {
  let bfl_params = polling.bfl_poll_params("task-123")
  should.equal(bfl_params.task_id, "task-123")
  should.equal(bfl_params.service_name, "BFL")
  should.be_true(string.contains(bfl_params.status_endpoint, "task-123"))

  let hedra_params = polling.hedra_poll_params("job-456")
  should.equal(hedra_params.service_name, "Hedra")

  let kling_params = polling.kling_poll_params("kling-task")
  should.equal(kling_params.service_name, "Kling")

  let kieai_params = polling.kieai_poll_params("veo-task")
  should.equal(kieai_params.service_name, "KIE AI")
}

pub fn polling_e2e_next_interval_test() {
  // Use config WITHOUT exponential backoff (default_config has exponential_backoff: False)
  let config = polling.default_config()

  // Without backoff, interval should always be the configured interval_ms
  let first = polling.next_interval(config, 1000, 0)
  should.equal(first, 2000)
  // default_config has interval_ms: 2000

  // Even with more attempts, interval stays the same without backoff
  let second = polling.next_interval(config, 1000, 5)
  should.equal(second, 2000)
}

pub fn polling_e2e_should_continue_test() {
  let config = polling.default_config()

  // Should continue when pending and under time limit
  should.be_true(polling.should_continue(config, 10_000, polling.Pending))
  should.be_true(polling.should_continue(config, 10_000, polling.Processing))

  // Should stop when time limit exceeded
  should.be_false(polling.should_continue(config, 500_000, polling.Pending))

  // Should stop when complete or failed
  should.be_false(polling.should_continue(
    config,
    10_000,
    polling.Complete(json.null()),
  ))
  should.be_false(polling.should_continue(
    config,
    10_000,
    polling.Failed("error"),
  ))
}

pub fn polling_e2e_estimated_time_test() {
  let bfl_time = polling.estimated_time_remaining("BFL", 10_000)
  should.be_true(case bfl_time {
    Some(t) -> t > 0
    None -> False
  })

  let hedra_time = polling.estimated_time_remaining("Hedra", 10_000)
  should.be_true(case hedra_time {
    Some(t) -> t > 0
    None -> False
  })

  // If elapsed exceeds typical time, should return None
  let overtime = polling.estimated_time_remaining("BFL", 100_000)
  should.equal(overtime, None)
}

// ============================================================
// Workflows E2E Tests
// ============================================================

pub fn workflows_e2e_neuro_photo_test() {
  let request =
    workflows.NeuroPhotoRequest(
      prompt: "Professional headshot",
      model_url: "https://replicate.com/model/lora",
      num_images: 4,
      aspect_ratio: workflows.Portrait,
      gender: Some(workflows.Female),
      seed: Some(42),
    )

  should.equal(request.num_images, 4)
  should.equal(request.aspect_ratio, workflows.Portrait)

  // Test prompt building
  let enhanced =
    workflows.build_neuro_photo_prompt("in a suit", workflows.Male)
  should.be_true(string.contains(enhanced, "handsome man"))
  should.be_true(string.contains(enhanced, "8K"))
  should.be_true(string.contains(enhanced, "Cinematic"))

  // Test negative prompt
  let negative = workflows.neuro_photo_negative_prompt()
  should.be_true(string.contains(negative, "nsfw"))
  should.be_true(string.contains(negative, "watermark"))
}

pub fn workflows_e2e_morphing_test() {
  // Test seamless morphing
  let pairs_seamless = workflows.morphing_pairs_count(5, workflows.Seamless)
  should.equal(pairs_seamless, 4)

  // Test loop morphing
  let pairs_loop = workflows.morphing_pairs_count(5, workflows.Loop)
  should.equal(pairs_loop, 5)

  // Test time estimation
  let #(min_time, max_time) = workflows.estimate_morphing_time(4)
  should.equal(min_time, 20)
  should.equal(max_time, 60)
}

pub fn workflows_e2e_broll_test() {
  let prompt = workflows.broll_system_prompt()
  should.be_true(string.contains(prompt, "b-roll"))
  should.be_true(string.contains(prompt, "9:16"))

  let segment =
    workflows.BRollSegment(
      id: "seg_001",
      layer_id: Some("layer_1"),
      start: 0.0,
      end: 5.0,
      veo3_prompt: "Cinematic drone shot of mountains",
    )

  should.equal(segment.id, "seg_001")
  should.be_true(string.contains(segment.veo3_prompt, "Cinematic"))
}

pub fn workflows_e2e_transcription_matching_test() {
  let segments = [
    workflows.TranscriptionSegment(text: "Hello world", start: 0.0, end: 1.0),
    workflows.TranscriptionSegment(
      text: "This is a test",
      start: 1.0,
      end: 2.0,
    ),
    workflows.TranscriptionSegment(text: "Goodbye", start: 2.0, end: 3.0),
  ]

  // Exact match
  let found = workflows.find_matching_segment("hello world", segments)
  should.be_true(case found {
    Some(seg) -> seg.text == "Hello world"
    None -> False
  })

  // No match
  let not_found = workflows.find_matching_segment("random text", segments)
  should.equal(not_found, None)
}

pub fn workflows_e2e_video_models_test() {
  let models = workflows.video_models()
  should.be_true(list.length(models) >= 4)

  let minimax = workflows.find_video_model("minimax")
  should.be_true(case minimax {
    Some(m) -> m.supports_image_input && m.supports_morphing
    None -> False
  })

  let runway = workflows.find_video_model("runway")
  should.be_true(case runway {
    Some(m) -> m.supports_image_input && !m.supports_morphing
    None -> False
  })
}

pub fn workflows_e2e_cost_calculation_test() {
  should.equal(workflows.base_cost(workflows.NeuroPhoto), 1.0)
  should.equal(workflows.base_cost(workflows.TextToVideo), 10.0)
  should.equal(workflows.base_cost(workflows.AvatarVideo), 20.0)

  should.equal(workflows.calculate_cost(workflows.NeuroPhoto, 5), 5.0)
  should.equal(workflows.calculate_cost(workflows.TextToVideo, 3), 30.0)
}

pub fn workflows_e2e_result_types_test() {
  let success =
    workflows.workflow_success(
      workflows.NeuroPhoto,
      "https://result.com/image.jpg",
      5000,
    )

  should.be_true(success.success)
  should.equal(success.result_url, Some("https://result.com/image.jpg"))
  should.equal(success.error, None)
  should.equal(success.processing_time_ms, 5000)

  let failure =
    workflows.workflow_failure(workflows.TextToVideo, "API timeout", 30_000)

  should.be_false(failure.success)
  should.equal(failure.result_url, None)
  should.equal(failure.error, Some("API timeout"))
}

// ============================================================
// Helper Functions
// ============================================================

fn has_header(headers: List(#(String, String)), key: String, value: String) -> Bool {
  list.any(headers, fn(h) {
    case h {
      #(k, v) -> k == key && v == value
    }
  })
}

fn has_header_prefix(
  headers: List(#(String, String)),
  key: String,
  prefix: String,
) -> Bool {
  list.any(headers, fn(h) {
    case h {
      #(k, v) -> k == key && string.starts_with(v, prefix)
    }
  })
}
