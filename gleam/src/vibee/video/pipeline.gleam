// Auto Video Pipeline Orchestrator
// Fully automated video generation from photo + script
//
// Pipeline steps:
// 1. Text → ElevenLabs TTS → audio_url
// 2. Photo + audio → fal.ai Fabric → lipsync_video_url
// 3. Script → LLM → B-roll prompts (Veo 3.1 style)
// 4. [TEST MODE] Select from existing B-roll assets
// 5. Build segments + captions
// 6. POST /render/template → final video

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
import vibee/video/broll_prompts

// ============================================================
// Types
// ============================================================

pub type PipelineRequest {
  PipelineRequest(
    photo_url: String,
    script_text: String,
    voice_id: Option(String),
    webhook_url: Option(String),
    test_mode: Bool,
    quick_test: Bool,
  )
}

pub type PipelineState {
  Pending
  GeneratingTTS
  GeneratingLipsync
  GeneratingBRoll
  BuildingSegments
  Rendering
  Completed(video_url: String)
  Failed(error: String)
}

pub type PipelineJob {
  PipelineJob(
    id: String,
    request: PipelineRequest,
    state: PipelineState,
    progress: Int,
    current_step: String,
    audio_url: Option(String),
    lipsync_url: Option(String),
    broll_prompts: List(broll_prompts.BRollPrompt),
    render_id: Option(String),
    created_at: Int,
  )
}

pub type PipelineConfig {
  PipelineConfig(
    elevenlabs_api_key: String,
    fal_api_key: String,
    remotion_url: String,
    test_assets_url: String,
  )
}

pub type PipelineError {
  TTSError(String)
  LipsyncError(String)
  RenderError(String)
  ConfigError(String)
  NetworkError(String)
  BRollError(String)
}

// ============================================================
// Pipeline Execution (Test Mode)
// ============================================================

/// Start pipeline in test mode (uses existing assets)
pub fn start_test_pipeline(
  config: PipelineConfig,
  req: PipelineRequest,
) -> Result(PipelineJob, PipelineError) {
  let pipeline_id = generate_id()

  let job = PipelineJob(
    id: pipeline_id,
    request: req,
    state: Pending,
    progress: 0,
    current_step: "Initializing",
    audio_url: None,
    lipsync_url: None,
    broll_prompts: [],
    render_id: None,
    created_at: erlang_now(),
  )

  // In test mode, skip TTS and lipsync generation
  // Use existing test assets
  case req.test_mode {
    True -> run_test_mode_pipeline(config, job)
    False -> Error(ConfigError("Production mode not yet implemented"))
  }
}

/// Start pipeline with AI-generated B-roll prompts
pub fn start_ai_pipeline(
  config: PipelineConfig,
  req: PipelineRequest,
) -> Result(PipelineJob, PipelineError) {
  let pipeline_id = generate_id()

  let job = PipelineJob(
    id: pipeline_id,
    request: req,
    state: GeneratingBRoll,
    progress: 0,
    current_step: "Generating B-roll with AI",
    audio_url: None,
    lipsync_url: None,
    broll_prompts: [],
    render_id: None,
    created_at: erlang_now(),
  )

  run_ai_pipeline(config, job)
}

/// Run pipeline with AI-generated B-roll prompts
fn run_ai_pipeline(
  config: PipelineConfig,
  job: PipelineJob,
) -> Result(PipelineJob, PipelineError) {
  io.println("[AI PIPELINE] Starting AI B-roll generation...")

  // Step 1: Use existing lipsync video (test asset)
  let lipsync_url = config.test_assets_url <> "/lipsync/lipsync.mp4"
  let total_duration = 25.92  // Duration of test lipsync video

  // Step 2: Generate B-roll prompts using AI
  let prompts_result = broll_prompts.generate_broll_prompts_ai(
    job.request.script_text,
    0.0,
    total_duration,
  )

  case prompts_result {
    Error(err) -> {
      io.println("[AI PIPELINE] Failed to generate B-roll prompts: " <> err)
      Error(BRollError(err))
    }
    Ok(prompts) -> {
      io.println("[AI PIPELINE] Generated " <> int.to_string(list.length(prompts)) <> " B-roll prompts")

      // Step 3: Map AI prompts to test assets (for now)
      // In production, these would be sent to Veo 3.1
      let asset_mapping = broll_prompts.map_to_test_assets(prompts, config.test_assets_url)

      // Step 4: Build segments for render
      let segments = build_segments_from_broll_prompts(prompts, asset_mapping)

      // Step 5: Call render API
      let render_result = call_render_api(
        config.remotion_url,
        lipsync_url,
        segments,
        job.request.webhook_url,
      )

      case render_result {
        Ok(render_response) -> {
          let updated_job = PipelineJob(
            ..job,
            state: Rendering,
            progress: 80,
            current_step: "Rendering video",
            lipsync_url: Some(lipsync_url),
            broll_prompts: prompts,
            render_id: Some(render_response.render_id),
          )
          Ok(updated_job)
        }
        Error(err) -> Error(RenderError(err))
      }
    }
  }
}

/// Build segments from AI-generated B-roll prompts with asset mapping
fn build_segments_from_broll_prompts(
  prompts: List(broll_prompts.BRollPrompt),
  asset_mapping: List(#(Int, String)),
) -> List(Segment) {
  // Create a lookup map from segment index to asset URL
  prompts
  |> list.index_map(fn(prompt, idx) {
    // Find corresponding asset URL
    let asset_url = case list.find(asset_mapping, fn(m) { m.0 == prompt.segment_index }) {
      Ok(#(_, url)) -> Some(url)
      Error(_) -> None
    }

    // Alternate between split and fullscreen based on index
    let seg_type = case idx % 2 {
      0 -> "split"
      _ -> "fullscreen"
    }

    Segment(
      segment_type: seg_type,
      start_seconds: prompt.start_seconds,
      duration_seconds: int.to_float(prompt.duration_seconds),
      broll_url: asset_url,
    )
  })
}

/// Run pipeline with test assets (no API calls for generation)
fn run_test_mode_pipeline(
  config: PipelineConfig,
  job: PipelineJob,
) -> Result(PipelineJob, PipelineError) {
  // Step 1: Use existing lipsync video
  let lipsync_url = config.test_assets_url <> "/lipsync/lipsync.mp4"

  // Step 2: Check for quick test mode (5 sec, 150 frames, 1 segment)
  case job.request.quick_test {
    True -> {
      io.println("[PIPELINE] Quick test mode - 5 sec video with 1 segment")
      // Quick test: single fullscreen segment with static image
      let segments = [
        Segment(
          segment_type: "fullscreen",
          start_seconds: 0.0,
          duration_seconds: 5.0,
          broll_url: Some(config.test_assets_url <> "/covers/cover.jpeg"),
        )
      ]

      let render_result = call_render_api_quick(
        config.remotion_url,
        lipsync_url,
        segments,
        job.request.webhook_url,
      )

      case render_result {
        Ok(render_response) -> {
          let updated_job = PipelineJob(
            ..job,
            state: Rendering,
            progress: 80,
            current_step: "Rendering quick test video",
            lipsync_url: Some(lipsync_url),
            broll_prompts: [],
            render_id: Some(render_response.render_id),
          )
          Ok(updated_job)
        }
        Error(err) -> Error(RenderError(err))
      }
    }
    False -> {
      // Full test: 4 segments with B-roll videos
      let prompts = broll_prompts.get_preset_prompts(
        job.request.script_text,
        25.92,  // Duration of test lipsync video
      )

      let asset_mapping = broll_prompts.map_to_test_assets(prompts, config.test_assets_url)
      let segments = build_segments_from_assets(asset_mapping, 25.92)

      let render_result = call_render_api(
        config.remotion_url,
        lipsync_url,
        segments,
        job.request.webhook_url,
      )

      case render_result {
        Ok(render_response) -> {
          let updated_job = PipelineJob(
            ..job,
            state: Rendering,
            progress: 80,
            current_step: "Rendering video",
            lipsync_url: Some(lipsync_url),
            broll_prompts: prompts,
            render_id: Some(render_response.render_id),
          )
          Ok(updated_job)
        }
        Error(err) -> Error(RenderError(err))
      }
    }
  }
}

// ============================================================
// Segment Building
// ============================================================

pub type Segment {
  Segment(
    segment_type: String,
    start_seconds: Float,
    duration_seconds: Float,
    broll_url: Option(String),
  )
}

/// Build segments from asset mapping
fn build_segments_from_assets(
  assets: List(#(Int, String)),
  total_duration: Float,
) -> List(Segment) {
  let segment_count = list.length(assets)
  let segment_duration = total_duration /. int.to_float(segment_count)

  assets
  |> list.index_map(fn(asset, idx) {
    let #(_, url) = asset
    let start = int.to_float(idx) *. segment_duration

    // Alternate between split and fullscreen
    let seg_type = case idx % 3 {
      0 -> "split"
      1 -> "fullscreen"
      _ -> "split"
    }

    Segment(
      segment_type: seg_type,
      start_seconds: start,
      duration_seconds: segment_duration,
      broll_url: Some(url),
    )
  })
}

// ============================================================
// Render API Call
// ============================================================

pub type RenderResponse {
  RenderResponse(
    success: Bool,
    render_id: String,
    status_url: String,
  )
}

/// Call Remotion render API for quick test mode (5 sec, 150 frames)
fn call_render_api_quick(
  remotion_url: String,
  lipsync_url: String,
  segments: List(Segment),
  _webhook_url: Option(String),
) -> Result(RenderResponse, String) {
  // Quick test: 150 frames (5 sec), single segment with image, no face detection
  let segments_json = json.array(segments, fn(seg) {
    json.object([
      #("bRollUrl", json.string(option.unwrap(seg.broll_url, ""))),
      #("bRollType", json.string("image")),
      #("type", json.string(seg.segment_type)),
      #("startFrame", json.int(0)),
      #("durationFrames", json.int(150)),
      #("caption", json.string("")),
    ])
  })

  let body = json.object([
    #("type", json.string("video")),
    #("compositionId", json.string("SplitTalkingHead")),
    #("inputProps", json.object([
      #("lipSyncVideo", json.string(lipsync_url)),
      #("quickTest", json.bool(True)),
      #("durationFrames", json.int(150)),
      #("segments", segments_json),
      #("faceOffsetX", json.float(0.0)),
      #("faceOffsetY", json.float(0.0)),
      #("faceScale", json.float(1.0)),
      #("backgroundMusic", json.string("/audio/music/phonk_01.mp3")),
      #("musicVolume", json.float(0.10)),
      #("captions", json.array([], fn(x) { x })),
      #("showCaptions", json.bool(False)),
    ])),
  ])

  let body_str = json.to_string(body)
  io.println("[RENDER API QUICK] Sending quick test request to: " <> remotion_url <> "/render")
  io.println("[RENDER API QUICK] Body: " <> body_str)

  let host = case string.split(remotion_url, "://") {
    [_, rest] -> case string.split(rest, "/") {
      [h, ..] -> h
      _ -> rest
    }
    _ -> remotion_url
  }

  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_scheme(http.Https)
    |> request.set_host(host)
    |> request.set_path("/render")
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body_str)

  case httpc.send(req) {
    Ok(resp) -> {
      io.println("[RENDER API QUICK] Response status: " <> int.to_string(resp.status))
      io.println("[RENDER API QUICK] Response body: " <> resp.body)
      parse_render_response(resp.body)
    }
    Error(e) -> {
      io.println("[RENDER API QUICK] HTTP error: " <> string.inspect(e))
      Error("Failed to connect to Remotion: " <> string.inspect(e))
    }
  }
}

/// Call Remotion render API with real HTTP request
fn call_render_api(
  remotion_url: String,
  lipsync_url: String,
  segments: List(Segment),
  _webhook_url: Option(String),
) -> Result(RenderResponse, String) {
  // Convert segments to JSON format for SplitTalkingHead
  let segments_json = json.array(segments, fn(seg) {
    let start_frame = float.round(seg.start_seconds *. 30.0)
    let duration_frames = float.round(seg.duration_seconds *. 30.0)

    let base_fields = [
      #("type", json.string(seg.segment_type)),
      #("startFrame", json.int(start_frame)),
      #("durationFrames", json.int(duration_frames)),
      #("caption", json.string("")),
    ]

    let fields = case seg.broll_url {
      Some(url) -> [
        #("bRollUrl", json.string(url)),
        #("bRollType", json.string("video")),
        ..base_fields
      ]
      None -> base_fields
    }

    json.object(fields)
  })

  let body = json.object([
    #("type", json.string("video")),
    #("compositionId", json.string("SplitTalkingHead")),
    #("inputProps", json.object([
      #("lipSyncVideo", json.string(lipsync_url)),
      #("segments", segments_json),
      #("backgroundMusic", json.string("/audio/music/phonk_01.mp3")),
      #("musicVolume", json.float(0.10)),
      #("captions", json.array([], fn(x) { x })),
      #("showCaptions", json.bool(False)),
    ])),
  ])

  let body_str = json.to_string(body)
  io.println("[RENDER API] Sending request to: " <> remotion_url <> "/render")
  io.println("[RENDER API] Body: " <> body_str)

  // Parse URL to get host
  let host = case string.split(remotion_url, "://") {
    [_, rest] -> case string.split(rest, "/") {
      [h, ..] -> h
      _ -> rest
    }
    _ -> remotion_url
  }

  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_scheme(http.Https)
    |> request.set_host(host)
    |> request.set_path("/render")
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body_str)

  case httpc.send(req) {
    Ok(resp) -> {
      io.println("[RENDER API] Response status: " <> int.to_string(resp.status))
      io.println("[RENDER API] Response body: " <> resp.body)
      parse_render_response(resp.body)
    }
    Error(e) -> {
      io.println("[RENDER API] HTTP error: " <> string.inspect(e))
      Error("Failed to connect to Remotion: " <> string.inspect(e))
    }
  }
}

/// Parse render API response
fn parse_render_response(body: String) -> Result(RenderResponse, String) {
  let decoder = {
    use success <- decode.field("success", decode.bool)
    use render_id <- decode.field("renderId", decode.string)
    decode.success(RenderResponse(
      success: success,
      render_id: render_id,
      status_url: "/renders/" <> render_id,
    ))
  }

  case json.parse(body, decoder) {
    Ok(response) -> Ok(response)
    Error(_) -> {
      // Try to extract error message
      let error_decoder = {
        use error_msg <- decode.field("error", decode.string)
        decode.success(error_msg)
      }
      case json.parse(body, error_decoder) {
        Ok(error_msg) -> Error("Render API error: " <> error_msg)
        Error(_) -> Error("Failed to parse render response: " <> body)
      }
    }
  }
}

// ============================================================
// Helpers
// ============================================================

fn generate_id() -> String {
  let timestamp = erlang_now()
  let random = erlang_unique_integer()
  "pipeline_" <> int.to_string(timestamp) <> "_" <> int.to_string(random)
}

@external(erlang, "os", "system_time")
fn erlang_now() -> Int

@external(erlang, "erlang", "unique_integer")
fn erlang_unique_integer() -> Int

// ============================================================
// JSON Serialization
// ============================================================

/// Convert pipeline job to JSON for API response
pub fn job_to_json(job: PipelineJob) -> json.Json {
  let state_str = case job.state {
    Pending -> "pending"
    GeneratingTTS -> "generating_tts"
    GeneratingLipsync -> "generating_lipsync"
    GeneratingBRoll -> "generating_broll"
    BuildingSegments -> "building_segments"
    Rendering -> "rendering"
    Completed(_) -> "completed"
    Failed(_) -> "failed"
  }

  let video_url = case job.state {
    Completed(url) -> Some(url)
    _ -> None
  }

  let error = case job.state {
    Failed(err) -> Some(err)
    _ -> None
  }

  json.object([
    #("id", json.string(job.id)),
    #("status", json.string(state_str)),
    #("progress", json.int(job.progress)),
    #("current_step", json.string(job.current_step)),
    #("lipsync_url", json.nullable(job.lipsync_url, json.string)),
    #("render_id", json.nullable(job.render_id, json.string)),
    #("video_url", json.nullable(video_url, json.string)),
    #("error", json.nullable(error, json.string)),
    #("broll_prompts", broll_prompts.prompts_to_json(job.broll_prompts)),
  ])
}
