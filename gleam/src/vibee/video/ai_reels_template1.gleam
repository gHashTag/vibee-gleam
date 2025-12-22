// AI Reels Template 1 Implementation
// Replicates the logic from 999-multibots-telegraf ai-reels-wizard.ts
// With full test mode emulation (no real API calls)
//
// Steps:
// 1. Text → ElevenLabs TTS → audio_url
// 2. photo + audio → Fal Veed Fabric → lipsync_video_url
// 3. image → Google Veo 3.1 → reference_video_url
// 4. Merge videos → final_video
//
// Cost: 240⭐ (fixed)

import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}

// ============================================================
// Types
// ============================================================

pub type Template1Request {
  Template1Request(
    photo_url: String,
    text: String,
    voice_id: Option(String),
    telegram_id: String,
    test_mode: Bool,
  )
}

pub type Template1Response {
  Template1Response(
    status: String,
    audio_url: Option(String),
    lipsync_url: Option(String),
    veo_url: Option(String),
    merged_url: Option(String),
    cost: Int,
    duration_seconds: Float,
    steps_completed: List(String),
    test_mode: Bool,
  )
}

pub type Template1State {
  Pending
  GeneratingAudio
  GeneratingLipsync
  GeneratingVeo31
  MergingVideos
  Completed(video_url: String)
  Failed(error: String)
}

pub type Template1Config {
  Template1Config(
    test_assets_base_url: String,
    cost_stars: Int,
  )
}

// ============================================================
// Constants
// ============================================================

const cost_stars = 240

const test_lipsync_url = "https://vibee-remotion.fly.dev/public/lipsync/lipsync.mp4"

const test_veo_url = "https://vibee-remotion.fly.dev/public/backgrounds/business/00.mp4"

const test_audio_url = "https://vibee-remotion.fly.dev/public/lipsync/audio.mp3"

const test_duration_seconds = 25.92

// Mock delays in milliseconds
const tts_delay_ms = 1000

const lipsync_delay_ms = 2000

const veo31_delay_ms = 2000

const merge_delay_ms = 1000

// ============================================================
// Main Entry Point
// ============================================================

/// Run Template 1 pipeline with full test mode emulation
pub fn run_template1(req: Template1Request) -> Result(Template1Response, String) {
  case req.test_mode {
    True -> run_test_mode(req)
    False -> Error("Production mode not yet implemented. Use test_mode=true")
  }
}

/// Run in test mode with mock delays and test URLs
fn run_test_mode(req: Template1Request) -> Result(Template1Response, String) {
  // Step 1: Mock TTS generation
  let audio_result = mock_tts_generation(req.text)

  case audio_result {
    Error(err) -> Error("TTS failed: " <> err)
    Ok(audio_url) -> {
      // Step 2: Mock Lipsync generation
      let lipsync_result = mock_lipsync_generation(req.photo_url, audio_url)

      case lipsync_result {
        Error(err) -> Error("Lipsync failed: " <> err)
        Ok(lipsync_url) -> {
          // Step 3: Mock Veo 3.1 generation
          let veo_result = mock_veo31_generation(req.photo_url)

          case veo_result {
            Error(err) -> Error("Veo 3.1 failed: " <> err)
            Ok(veo_url) -> {
              // Step 4: Mock video merge
              let merge_result = mock_video_merge(lipsync_url, veo_url)

              case merge_result {
                Error(err) -> Error("Merge failed: " <> err)
                Ok(merged_url) -> {
                  Ok(Template1Response(
                    status: "success",
                    audio_url: Some(audio_url),
                    lipsync_url: Some(lipsync_url),
                    veo_url: Some(veo_url),
                    merged_url: Some(merged_url),
                    cost: cost_stars,
                    duration_seconds: test_duration_seconds,
                    steps_completed: ["tts", "lipsync", "veo31", "merge"],
                    test_mode: True,
                  ))
                }
              }
            }
          }
        }
      }
    }
  }
}

// ============================================================
// Mock Functions (Test Mode)
// ============================================================

/// Mock ElevenLabs TTS generation
fn mock_tts_generation(_text: String) -> Result(String, String) {
  // Simulate API delay
  process.sleep(tts_delay_ms)

  // Return test audio URL
  Ok(test_audio_url)
}

/// Mock Fal Veed Fabric lip-sync generation
fn mock_lipsync_generation(
  _image_url: String,
  _audio_url: String,
) -> Result(String, String) {
  // Simulate API delay
  process.sleep(lipsync_delay_ms)

  // Return test lipsync video URL
  Ok(test_lipsync_url)
}

/// Mock Google Veo 3.1 reference-to-video generation
fn mock_veo31_generation(_image_url: String) -> Result(String, String) {
  // Simulate API delay
  process.sleep(veo31_delay_ms)

  // Return test Veo 3.1 video URL
  Ok(test_veo_url)
}

/// Mock FFmpeg video merge
fn mock_video_merge(
  _lipsync_url: String,
  _veo_url: String,
) -> Result(String, String) {
  // Simulate processing delay
  process.sleep(merge_delay_ms)

  // In real mode, this would merge the two videos
  // For test mode, just return the lipsync video as the "merged" result
  Ok(test_lipsync_url)
}

// ============================================================
// JSON Serialization
// ============================================================

/// Convert response to JSON for API
pub fn response_to_json(resp: Template1Response) -> json.Json {
  json.object([
    #("status", json.string(resp.status)),
    #("audio_url", json.nullable(resp.audio_url, json.string)),
    #("lipsync_url", json.nullable(resp.lipsync_url, json.string)),
    #("veo_url", json.nullable(resp.veo_url, json.string)),
    #("merged_url", json.nullable(resp.merged_url, json.string)),
    #("cost", json.int(resp.cost)),
    #("duration_seconds", json.float(resp.duration_seconds)),
    #(
      "steps_completed",
      json.array(resp.steps_completed, json.string),
    ),
    #("test_mode", json.bool(resp.test_mode)),
  ])
}

/// Convert state to string for status API
pub fn state_to_string(state: Template1State) -> String {
  case state {
    Pending -> "pending"
    GeneratingAudio -> "generating_audio"
    GeneratingLipsync -> "generating_lipsync"
    GeneratingVeo31 -> "generating_veo31"
    MergingVideos -> "merging_videos"
    Completed(_) -> "completed"
    Failed(_) -> "failed"
  }
}

// ============================================================
// Helper Functions
// ============================================================

/// Get default config
pub fn default_config() -> Template1Config {
  Template1Config(
    test_assets_base_url: "https://vibee-remotion.fly.dev/public",
    cost_stars: cost_stars,
  )
}

/// Calculate estimated time for test mode
pub fn estimated_time_ms() -> Int {
  tts_delay_ms + lipsync_delay_ms + veo31_delay_ms + merge_delay_ms
}

/// Generate unique job ID
pub fn generate_job_id() -> String {
  let timestamp = erlang_now()
  let random = erlang_unique_integer()
  "aiреels_" <> int.to_string(timestamp) <> "_" <> int.to_string(random)
}

@external(erlang, "os", "system_time")
fn erlang_now() -> Int

@external(erlang, "erlang", "unique_integer")
fn erlang_unique_integer() -> Int
