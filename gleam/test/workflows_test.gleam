// AI Workflows Tests
// Tests for generation pipelines, prompts, and helpers

import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import vibee/ai/workflows

pub fn main() {
  gleeunit.main()
}

// ============================================================
// Generation Mode Tests
// ============================================================

pub fn mode_name_english_test() {
  workflows.mode_name(workflows.NeuroPhoto, False)
  |> should.equal("NeuroPhoto")

  workflows.mode_name(workflows.TextToVideo, False)
  |> should.equal("Text to Video")

  workflows.mode_name(workflows.ImageToVideo, False)
  |> should.equal("Image to Video")

  workflows.mode_name(workflows.Morphing, False)
  |> should.equal("Morphing")

  workflows.mode_name(workflows.BRoll, False)
  |> should.equal("B-Roll Generation")

  workflows.mode_name(workflows.AvatarVideo, False)
  |> should.equal("Avatar Video")

  workflows.mode_name(workflows.VoiceClone, False)
  |> should.equal("Voice Clone")
}

pub fn mode_name_russian_test() {
  workflows.mode_name(workflows.NeuroPhoto, True)
  |> should.equal("Нейрофото")

  workflows.mode_name(workflows.TextToVideo, True)
  |> should.equal("Текст в видео")

  workflows.mode_name(workflows.Morphing, True)
  |> should.equal("Морфинг")
}

// ============================================================
// Aspect Ratio Tests
// ============================================================

pub fn parse_aspect_ratio_test() {
  workflows.parse_aspect_ratio("1:1")
  |> should.equal(workflows.Square)

  workflows.parse_aspect_ratio("16:9")
  |> should.equal(workflows.Landscape)

  workflows.parse_aspect_ratio("9:16")
  |> should.equal(workflows.Portrait)

  workflows.parse_aspect_ratio("4:3")
  |> should.equal(workflows.Standard4x3)

  workflows.parse_aspect_ratio("21:9")
  |> should.equal(workflows.Ultrawide)

  // Unknown defaults to Square
  workflows.parse_aspect_ratio("unknown")
  |> should.equal(workflows.Square)
}

pub fn aspect_ratio_dimensions_test() {
  workflows.aspect_ratio_dimensions(workflows.Square)
  |> should.equal(#(1024, 1024))

  workflows.aspect_ratio_dimensions(workflows.Landscape)
  |> should.equal(#(1344, 768))

  workflows.aspect_ratio_dimensions(workflows.Portrait)
  |> should.equal(#(768, 1344))
}

pub fn aspect_ratio_string_test() {
  workflows.aspect_ratio_string(workflows.Square)
  |> should.equal("1:1")

  workflows.aspect_ratio_string(workflows.Landscape)
  |> should.equal("16:9")

  workflows.aspect_ratio_string(workflows.Portrait)
  |> should.equal("9:16")
}

// ============================================================
// Gender Parsing Tests
// ============================================================

pub fn parse_gender_test() {
  workflows.parse_gender("male")
  |> should.equal(workflows.Male)

  workflows.parse_gender("Male")
  |> should.equal(workflows.Male)

  workflows.parse_gender("M")
  |> should.equal(workflows.Male)

  workflows.parse_gender("мужской")
  |> should.equal(workflows.Male)

  workflows.parse_gender("female")
  |> should.equal(workflows.Female)

  workflows.parse_gender("F")
  |> should.equal(workflows.Female)

  workflows.parse_gender("женский")
  |> should.equal(workflows.Female)

  workflows.parse_gender("other")
  |> should.equal(workflows.Unknown)
}

// ============================================================
// NeuroPhoto Prompt Tests
// ============================================================

pub fn build_neuro_photo_prompt_male_test() {
  let prompt = workflows.build_neuro_photo_prompt("in a suit", workflows.Male)

  // Should contain gender descriptor
  should.be_true(string.contains(prompt, "handsome man"))
  should.be_true(string.contains(prompt, "masculine features"))

  // Should contain base prompt
  should.be_true(string.contains(prompt, "in a suit"))

  // Should contain quality enhancers
  should.be_true(string.contains(prompt, "Cinematic Lighting"))
  should.be_true(string.contains(prompt, "8K"))
}

pub fn build_neuro_photo_prompt_female_test() {
  let prompt =
    workflows.build_neuro_photo_prompt("on the beach", workflows.Female)

  // Should contain gender descriptor
  should.be_true(string.contains(prompt, "beautiful woman"))
  should.be_true(string.contains(prompt, "feminine features"))

  // Should contain base prompt
  should.be_true(string.contains(prompt, "on the beach"))
}

pub fn build_neuro_photo_prompt_unknown_test() {
  let prompt =
    workflows.build_neuro_photo_prompt("in nature", workflows.Unknown)

  // Should use neutral descriptor
  should.be_true(string.contains(prompt, "person"))
  should.be_true(string.contains(prompt, "in nature"))
}

pub fn neuro_photo_negative_prompt_test() {
  let negative = workflows.neuro_photo_negative_prompt()

  // Should contain NSFW filter
  should.be_true(string.contains(negative, "nsfw"))

  // Should contain quality filters
  should.be_true(string.contains(negative, "bad anatomy"))
  should.be_true(string.contains(negative, "low quality"))
  should.be_true(string.contains(negative, "watermark"))
}

// ============================================================
// Morphing Tests
// ============================================================

pub fn morphing_pairs_count_seamless_test() {
  // 3 images = 2 pairs (1→2, 2→3)
  workflows.morphing_pairs_count(3, workflows.Seamless)
  |> should.equal(2)

  // 5 images = 4 pairs
  workflows.morphing_pairs_count(5, workflows.Seamless)
  |> should.equal(4)
}

pub fn morphing_pairs_count_loop_test() {
  // 3 images = 3 pairs (1→2, 2→3, 3→1)
  workflows.morphing_pairs_count(3, workflows.Loop)
  |> should.equal(3)

  // 5 images = 5 pairs (includes last→first)
  workflows.morphing_pairs_count(5, workflows.Loop)
  |> should.equal(5)

  // 2 images = 1 pair (not enough for loop)
  workflows.morphing_pairs_count(2, workflows.Loop)
  |> should.equal(1)
}

pub fn estimate_morphing_time_test() {
  // 3 pairs = 15-45 minutes
  let #(min, max) = workflows.estimate_morphing_time(3)
  should.equal(min, 15)
  should.equal(max, 45)

  // 5 pairs = 25-75 minutes
  let #(min2, max2) = workflows.estimate_morphing_time(5)
  should.equal(min2, 25)
  should.equal(max2, 75)
}

// ============================================================
// Transcription Types Tests
// ============================================================

pub fn transcription_word_test() {
  let word = workflows.TranscriptionWord(text: "hello", start: 0.0, end: 0.5)

  should.equal(word.text, "hello")
  should.equal(word.start, 0.0)
  should.equal(word.end, 0.5)
}

pub fn transcription_segment_test() {
  let segment =
    workflows.TranscriptionSegment(
      text: "Hello world",
      start: 0.0,
      end: 1.5,
    )

  should.equal(segment.text, "Hello world")
  should.equal(segment.start, 0.0)
  should.equal(segment.end, 1.5)
}

// ============================================================
// B-Roll Tests
// ============================================================

pub fn broll_system_prompt_test() {
  let prompt = workflows.broll_system_prompt()

  // Should mention B-roll
  should.be_true(string.contains(prompt, "b-roll"))

  // Should mention vertical format
  should.be_true(string.contains(prompt, "9:16"))

  // Should mention cinematic
  should.be_true(string.contains(prompt, "cinematic"))

  // Should specify output format
  should.be_true(string.contains(prompt, "JSON"))
}

pub fn build_broll_prompt_test() {
  let prompt = workflows.build_broll_prompt("A person walking in the city")

  should.be_true(string.contains(prompt, "cinematic"))
  should.be_true(string.contains(prompt, "A person walking in the city"))
}

pub fn find_matching_segment_exact_test() {
  let segments = [
    workflows.TranscriptionSegment(
      text: "Hello world",
      start: 0.0,
      end: 1.0,
    ),
    workflows.TranscriptionSegment(
      text: "This is a test",
      start: 1.0,
      end: 2.0,
    ),
    workflows.TranscriptionSegment(
      text: "Goodbye everyone",
      start: 2.0,
      end: 3.0,
    ),
  ]

  // Exact match
  let result = workflows.find_matching_segment("hello world", segments)
  should.be_true(case result {
    Some(seg) -> seg.text == "Hello world"
    None -> False
  })
}

pub fn find_matching_segment_not_found_test() {
  let segments = [
    workflows.TranscriptionSegment(
      text: "Hello world",
      start: 0.0,
      end: 1.0,
    ),
  ]

  // No match
  let result = workflows.find_matching_segment("completely different", segments)
  should.equal(result, None)
}

// ============================================================
// Video Model Tests
// ============================================================

pub fn video_models_test() {
  let models = workflows.video_models()

  // Should have at least 4 models
  should.be_true(case models {
    [_, _, _, _, ..] -> True
    _ -> False
  })
}

pub fn find_video_model_test() {
  // Find existing model
  let minimax = workflows.find_video_model("minimax")
  should.be_true(case minimax {
    Some(m) -> m.name == "MiniMax"
    None -> False
  })

  // Non-existing model
  let unknown = workflows.find_video_model("nonexistent")
  should.equal(unknown, None)
}

// ============================================================
// Workflow Result Tests
// ============================================================

pub fn workflow_success_test() {
  let result =
    workflows.workflow_success(
      workflows.NeuroPhoto,
      "https://example.com/image.jpg",
      5000,
    )

  should.be_true(result.success)
  should.equal(result.mode, workflows.NeuroPhoto)
  should.equal(result.result_url, Some("https://example.com/image.jpg"))
  should.equal(result.error, None)
  should.equal(result.processing_time_ms, 5000)
}

pub fn workflow_failure_test() {
  let result =
    workflows.workflow_failure(workflows.TextToVideo, "API error", 3000)

  should.be_false(result.success)
  should.equal(result.mode, workflows.TextToVideo)
  should.equal(result.result_url, None)
  should.equal(result.error, Some("API error"))
  should.equal(result.processing_time_ms, 3000)
}

// ============================================================
// Cost Calculation Tests
// ============================================================

pub fn base_cost_test() {
  workflows.base_cost(workflows.NeuroPhoto)
  |> should.equal(1.0)

  workflows.base_cost(workflows.TextToVideo)
  |> should.equal(10.0)

  workflows.base_cost(workflows.AvatarVideo)
  |> should.equal(20.0)
}

pub fn calculate_cost_test() {
  // 3 NeuroPhoto images
  workflows.calculate_cost(workflows.NeuroPhoto, 3)
  |> should.equal(3.0)

  // 2 TextToVideo
  workflows.calculate_cost(workflows.TextToVideo, 2)
  |> should.equal(20.0)
}

// ============================================================
// NeuroPhoto Request Type Test
// ============================================================

pub fn neuro_photo_request_test() {
  let request =
    workflows.NeuroPhotoRequest(
      prompt: "Professional photo",
      model_url: "https://model.com/lora",
      num_images: 4,
      aspect_ratio: workflows.Portrait,
      gender: Some(workflows.Female),
      seed: Some(12345),
    )

  should.equal(request.prompt, "Professional photo")
  should.equal(request.num_images, 4)
  should.equal(request.aspect_ratio, workflows.Portrait)
  should.equal(request.gender, Some(workflows.Female))
  should.equal(request.seed, Some(12345))
}

// ============================================================
// BRoll Segment Test
// ============================================================

pub fn broll_segment_test() {
  let segment =
    workflows.BRollSegment(
      id: "seg_001",
      layer_id: Some("layer_1"),
      start: 5.0,
      end: 10.0,
      veo3_prompt: "Cinematic shot of a sunset",
    )

  should.equal(segment.id, "seg_001")
  should.equal(segment.layer_id, Some("layer_1"))
  should.equal(segment.start, 5.0)
  should.equal(segment.end, 10.0)
  should.be_true(string.contains(segment.veo3_prompt, "sunset"))
}

// ============================================================
// Avatar Video Types Test
// ============================================================

pub fn avatar_video_request_test() {
  let request =
    workflows.AvatarVideoRequest(
      text: "Hello, this is a test",
      voice_id: "voice_123",
      avatar_id: "avatar_456",
      avatar_service: workflows.HedraAvatar,
      aspect_ratio: workflows.Portrait,
    )

  should.equal(request.text, "Hello, this is a test")
  should.equal(request.voice_id, "voice_123")
  should.equal(request.avatar_id, "avatar_456")

  should.be_true(case request.avatar_service {
    workflows.HedraAvatar -> True
    _ -> False
  })
}

pub fn avatar_service_types_test() {
  let hedra = workflows.HedraAvatar
  let heygen = workflows.HeyGenAvatar

  should.be_true(case hedra {
    workflows.HedraAvatar -> True
    _ -> False
  })

  should.be_true(case heygen {
    workflows.HeyGenAvatar -> True
    _ -> False
  })
}
