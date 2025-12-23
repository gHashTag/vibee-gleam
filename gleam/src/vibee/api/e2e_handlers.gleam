// E2E Testing Handlers
// Rainbow Bridge automated testing via MCP tools internally

import gleam/bytes_tree
import gleam/http/response.{type Response}
import gleam/io
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/list
import gleam/erlang/process
import mist.{type ResponseData}
import vibee/integrations/telegram/client
import vibee/integrations/telegram/types.{type TelegramMessage, type TelegramError}
import vibee/config/dynamic_config
import vibee/config/trigger_chats
import vibee/db/postgres
import vibee/mcp/config
import vibee/vibe_logger
import vibee/api/e2e_test_runner
import vibee/video/pipeline as vibee_pipeline
import gleam/http as gleam_http
import gleam/http/request as gleam_http_request
import gleam/httpc as gleam_httpc

/// E2E test result for a single command
pub type TestResult {
  TestResult(
    command: String,
    expected: String,
    passed: Bool,
    response: String,
    duration_ms: Int,
  )
}

/// Overall E2E test results
pub type E2EResults {
  E2EResults(
    tester_session: String,
    tester_username: String,
    bot_username: String,
    bot_chat_id: Int,
    tests: List(TestResult),
    total: Int,
    passed: Int,
    failed: Int,
  )
}

/// Run E2E tests handler (ASYNC) - GET /api/e2e/run
/// Returns 202 Accepted with test_run_id immediately
/// Poll /api/e2e/status/{test_run_id} for results
pub fn run_handler() -> Response(ResponseData) {
  // Initialize ETS table FIRST (critical for async)
  e2e_test_runner.init()

  // Define tests to run - NLP ONLY! No /commands for Digital Twin (user-bot)
  let tests = [
    // NLP pricing - natural language triggers
    e2e_test_runner.E2ETest("покажи тарифы", "JUNIOR|MIDDLE|Тариф|$99"),
    // NLP video - natural language
    e2e_test_runner.E2ETest("хочу создать видео", "Kling|Minimax|Выбери|провайдер|видео"),
    // NLP reels - ElizaOS action
    e2e_test_runner.E2ETest("создай рилс про продуктивность", "Создаю рилс|Генерация|TTS|рилс|продуктивность"),
    // P2P lead trigger - crypto keywords
    e2e_test_runner.E2ETest("хочу купить крипту", "личку|напиши|помогу"),
  ]

  // Start async tests
  let test_run_id = e2e_test_runner.start_async(tests)

  // Return 202 Accepted immediately
  let body = json.object([
    #("status", json.string("running")),
    #("test_run_id", json.string(test_run_id)),
    #("message", json.string("Tests started. Poll /api/e2e/status/" <> test_run_id <> " for results.")),
  ]) |> json.to_string()

  response.new(202)
  |> response.set_header("content-type", "application/json")
  |> response.set_header("location", "/api/e2e/status/" <> test_run_id)
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

/// Legacy synchronous E2E handler (for backwards compatibility)
/// GET /api/e2e/run-sync - WARNING: May timeout on Fly.io
pub fn run_sync_handler() -> Response(ResponseData) {
  case run_e2e_tests() {
    Ok(results) -> {
      let body = encode_results(results) |> json.to_string()
      response.new(200)
      |> response.set_header("content-type", "application/json")
      |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
    }
    Error(err) -> {
      let body = json.object([
        #("error", json.string(err)),
        #("status", json.string("failed")),
      ]) |> json.to_string()
      response.new(500)
      |> response.set_header("content-type", "application/json")
      |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
    }
  }
}

/// Simple ETS test - GET /api/e2e/test-ets
pub fn test_ets_handler() -> Response(ResponseData) {
  // Test basic ETS functionality
  e2e_test_runner.init()

  // Test 1: Simple key
  let test_id = "test_" <> int.to_string(e2e_test_runner.current_time_ms())
  let test_run = e2e_test_runner.E2ETestRun(
    id: test_id,
    status: e2e_test_runner.Running,
    tests: [],
    started_at: e2e_test_runner.current_time_ms(),
    completed_at: None,
    tester_session: "test",
    bot_chat_id: 0,
  )
  e2e_test_runner.save(test_run)
  let result1 = case e2e_test_runner.get_status(test_id) {
    Some(_) -> "OK"
    None -> "FAIL"
  }

  // Test 2: Use generate_id() like start_async does
  let e2e_id = e2e_test_runner.generate_id()
  let e2e_run = e2e_test_runner.E2ETestRun(
    id: e2e_id,
    status: e2e_test_runner.Running,
    tests: [],
    started_at: e2e_test_runner.current_time_ms(),
    completed_at: None,
    tester_session: "test_e2e",
    bot_chat_id: config.get_env_int_or("VIBEE_AGENT_USER_ID", 0),
  )
  e2e_test_runner.save(e2e_run)
  let result2 = case e2e_test_runner.get_status(e2e_id) {
    Some(_) -> "OK"
    None -> "FAIL"
  }

  // Test 3: Call start_async and check immediately
  let async_id = e2e_test_runner.start_async([])
  let result3 = case e2e_test_runner.get_status(async_id) {
    Some(_) -> "OK"
    None -> "FAIL"
  }

  let body = json.object([
    #("test_simple_key", json.string(test_id <> " -> " <> result1)),
    #("test_e2e_id", json.string(e2e_id <> " -> " <> result2)),
    #("test_start_async", json.string(async_id <> " -> " <> result3)),
  ]) |> json.to_string()

  response.new(200)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

/// Get E2E test run status - GET /api/e2e/status/{test_run_id}
pub fn status_handler(test_run_id: String) -> Response(ResponseData) {
  // Ensure ETS table exists
  e2e_test_runner.init()

  case e2e_test_runner.get_status(test_run_id) {
    Some(run) -> {
      let body = e2e_test_runner.encode_run(run) |> json.to_string()
      response.new(200)
      |> response.set_header("content-type", "application/json")
      |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
    }
    None -> {
      let body = json.object([
        #("error", json.string("Test run not found: " <> test_run_id)),
        #("status", json.string("not_found")),
      ]) |> json.to_string()
      response.new(404)
      |> response.set_header("content-type", "application/json")
      |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
    }
  }
}

/// Test single /neuro command - GET /api/e2e/neuro
/// Quick test for /neuro with 60s timeout
pub fn neuro_test_handler() -> Response(ResponseData) {
  io.println("[E2E-NEURO] 🧪 HANDLER STARTED")
  let logger = vibe_logger.new("E2E-NEURO")
  vibe_logger.info(logger, "🧪 NEURO TEST STARTING")

  let bridge_url = get_bridge_url()
  let api_key = config.require_env("VIBEE_API_KEY")
  let tester_session = config.require_env("TELEGRAM_SESSION_ID_TESTER")
  let bot_chat_id = config.get_env_int_or("VIBEE_AGENT_USER_ID", 0)

  let bridge = client.with_session_and_key(bridge_url, tester_session, api_key)

  // Send NLP neuro trigger (no /command!)
  vibe_logger.info(logger, "📤 Sending NLP neuro trigger to bot...")
  case client.send_message(bridge, bot_chat_id, "сгенерируй фото кота в космосе", None) {
    Error(err) -> {
      let err_str = telegram_error_to_string(err)
      vibe_logger.error(logger |> vibe_logger.with_data("error", json.string(err_str)), "Failed to send")
      let body = json.object([
        #("status", json.string("failed")),
        #("error", json.string(err_str)),
      ]) |> json.to_string()
      response.new(500)
      |> response.set_header("content-type", "application/json")
      |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
    }
    Ok(_) -> {
      vibe_logger.info(logger, "✅ Command sent! Waiting 30s for FAL.ai...")
      // Wait for FAL.ai processing (30 seconds - faster for E2E)
      process.sleep(30000)

      vibe_logger.info(logger, "📥 Fetching history...")
      case client.get_history(bridge, bot_chat_id, 10) {
        Error(err) -> {
          let err_str = telegram_error_to_string(err)
          let body = json.object([
            #("status", json.string("failed")),
            #("error", json.string("Failed to get history: " <> err_str)),
          ]) |> json.to_string()
          response.new(500)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
        }
        Ok(messages) -> {
          vibe_logger.info(logger |> vibe_logger.with_data("count", json.int(list.length(messages))), "📬 Got messages")

          // Find photo or image response from bot
          let vibee_agent_id = config.get_env_int_or("VIBEE_AGENT_USER_ID", 0)
          let bot_responses = list.filter(messages, fn(m) { m.from_id == vibee_agent_id })

          let has_image = list.any(bot_responses, fn(m) {
            string.contains(string.lowercase(m.text), "generated") ||
            string.contains(m.text, "fal.media") ||
            string.contains(m.text, "Промпт:") ||
            string.contains(m.text, "Prompt:") ||
            string.contains(m.text, "Генерирую")
          })

          let has_error = list.any(bot_responses, fn(m) {
            string.contains(m.text, "Ошибка") ||
            string.contains(string.lowercase(m.text), "error") ||
            string.contains(m.text, "Таймаут")
          })

          let latest_response = case list.first(bot_responses) {
            Ok(m) -> m.text
            Error(_) -> "No bot response"
          }

          let passed = has_image && !has_error
          vibe_logger.info(logger
            |> vibe_logger.with_data("passed", json.bool(passed))
            |> vibe_logger.with_data("has_image", json.bool(has_image))
            |> vibe_logger.with_data("has_error", json.bool(has_error))
            |> vibe_logger.with_data("response", json.string(string.slice(latest_response, 0, 100))),
            case passed { True -> "✅ NEURO TEST PASSED" False -> "❌ NEURO TEST FAILED" })

          let body = json.object([
            #("status", json.string(case passed { True -> "passed" False -> "failed" })),
            #("has_image", json.bool(has_image)),
            #("has_error", json.bool(has_error)),
            #("response", json.string(string.slice(latest_response, 0, 500))),
            #("messages_count", json.int(list.length(bot_responses))),
          ]) |> json.to_string()

          response.new(200)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
        }
      }
    }
  }
}

/// P2P Lead Forward E2E test handler - GET /api/e2e/p2p
/// Tests the full P2P lead forwarding flow:
/// 1. Send trigger message to Aimly.io dev group
/// 2. Agent responds with "напиши в личку"
/// 3. Lead Card is forwarded to Leads group
/// Returns 202 Accepted with test_run_id for polling
pub fn p2p_handler() -> Response(ResponseData) {
  io.println("[E2E-P2P] 🔄 P2P LEAD FORWARD TEST STARTING")

  // Initialize ETS
  e2e_test_runner.init()

  let test_run_id = e2e_test_runner.generate_id()
  let started_at = e2e_test_runner.current_time_ms()

  let tester_session = config.require_env("TELEGRAM_SESSION_ID_TESTER")
  let bot_chat_id = config.get_env_int_or("VIBEE_AGENT_USER_ID", 0)

  // Create initial running state
  let initial_run = e2e_test_runner.E2ETestRun(
    id: test_run_id,
    status: e2e_test_runner.Running,
    tests: [],
    started_at: started_at,
    completed_at: None,
    tester_session: tester_session,
    bot_chat_id: bot_chat_id,
  )
  e2e_test_runner.save(initial_run)

  // Spawn background process for P2P test
  let id_copy = test_run_id
  ffi_spawn_async(fn() {
    run_p2p_test_background(id_copy)
  })

  // Return 202 Accepted immediately
  let body = json.object([
    #("status", json.string("running")),
    #("test_run_id", json.string(test_run_id)),
    #("test_type", json.string("p2p_lead_forward")),
    #("message", json.string("P2P Lead Forward test started. Poll /api/e2e/status/" <> test_run_id <> " for results.")),
  ]) |> json.to_string()

  response.new(202)
  |> response.set_header("content-type", "application/json")
  |> response.set_header("location", "/api/e2e/status/" <> test_run_id)
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

/// Run P2P test in background
fn run_p2p_test_background(run_id: String) -> Nil {
  io.println("[E2E-P2P-BG] Starting P2P test for " <> run_id)

  let bridge_url = get_bridge_url()
  let api_key = config.require_env("VIBEE_API_KEY")
  let tester_session = config.require_env("TELEGRAM_SESSION_ID_TESTER")
  let bridge = client.with_session_and_key(bridge_url, tester_session, api_key)

  // Run the lead forward test
  let test_result = run_lead_forward_test(bridge)

  // Convert local TestResult to e2e_test_runner.TestResult
  let runner_result = e2e_test_runner.TestResult(
    command: test_result.command,
    expected: test_result.expected,
    passed: test_result.passed,
    response: test_result.response,
    duration_ms: test_result.duration_ms,
  )

  // Update ETS with results
  let final_status = case test_result.passed {
    True -> e2e_test_runner.Completed
    False -> e2e_test_runner.Failed("P2P lead forward test failed")
  }

  let updated_run = e2e_test_runner.E2ETestRun(
    id: run_id,
    status: final_status,
    tests: [runner_result],
    started_at: case e2e_test_runner.get_status(run_id) {
      Some(r) -> r.started_at
      None -> e2e_test_runner.current_time_ms()
    },
    completed_at: Some(e2e_test_runner.current_time_ms()),
    tester_session: tester_session,
    bot_chat_id: config.get_env_int_or("VIBEE_AGENT_USER_ID", 0),
  )

  io.println("[E2E-P2P-BG] Saving results for " <> run_id)
  e2e_test_runner.save(updated_run)
  io.println("[E2E-P2P-BG] Test complete: " <> case test_result.passed { True -> "PASSED" False -> "FAILED" })

  Nil
}

// FFI for spawning async process
@external(erlang, "vibee_e2e_runner_ffi", "spawn_async")
fn ffi_spawn_async(f: fn() -> Nil) -> Nil

/// Video E2E tests handler - GET /api/e2e/video
/// Tests video commands to verify they respond correctly
/// NOTE: Full video generation with button clicks not supported for user-bots (MTProto)
///       because MessagesGetBotCallbackAnswer only works for actual Telegram bots.
/// Returns 202 Accepted with test_run_id for polling
pub fn video_handler() -> Response(ResponseData) {
  io.println("[E2E-VIDEO] 🎬 VIDEO E2E HANDLER STARTED")

  // Initialize ETS
  e2e_test_runner.init()

  // Test video NLP triggers - verify they show provider selection
  // NOTE: Cannot click inline buttons on user-bot messages (MTProto limitation)
  // The Digital Twin is a user-bot, not a Telegram Bot API bot
  // NLP ONLY - no /commands!
  let tests = [
    // NLP video trigger
    e2e_test_runner.E2ETest("сделай видео", "Kling|Veo3|provider|выбери|choose|видео"),
    // NLP morph trigger
    e2e_test_runner.E2ETest("сделай морфинг", "стиль|style|morph|face-to-many"),
    // NLP animate trigger
    e2e_test_runner.E2ETest("анимируй фото", "анимац|animate|image-to-video"),
  ]

  // Start async tests
  let test_run_id = e2e_test_runner.start_async(tests)

  // Return 202 Accepted immediately
  let body = json.object([
    #("status", json.string("running")),
    #("test_run_id", json.string(test_run_id)),
    #("test_type", json.string("video_commands")),
    #("timeout_seconds", json.int(60)),
    #("message", json.string("Video command tests started. Poll /api/e2e/status/" <> test_run_id <> " for results.")),
    #("note", json.string("Full video generation with button clicks not supported - user-bot limitation.")),
  ]) |> json.to_string()

  response.new(202)
  |> response.set_header("content-type", "application/json")
  |> response.set_header("location", "/api/e2e/status/" <> test_run_id)
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

/// Reels Full Flow E2E test handler - GET /api/e2e/reels-flow
/// Tests the complete /reels flow including button clicks and video generation
/// Uses MultiStepTest for sequential command + button interaction
pub fn reels_flow_handler() -> Response(ResponseData) {
  io.println("[E2E-REELS] 🎬 REELS FULL FLOW E2E HANDLER STARTED")

  // Initialize ETS
  e2e_test_runner.init()

  // Define multi-step test for full reels flow - NLP triggered
  let reels_test = e2e_test_runner.MultiStepTest(
    name: "reels_full_flow",
    steps: [
      // Step 1: Send NLP reels trigger (no /command!)
      e2e_test_runner.SendCommand("хочу создать рилс"),
      // Step 2: Wait for template selection keyboard
      e2e_test_runner.Wait(2000),
      // Step 3: Click Split Talking Head template button
      e2e_test_runner.ClickButton("reels:template:split-talking-head", 3000),
      // Step 4: Enter idea text
      e2e_test_runner.SendCommand("5 способов повысить продуктивность"),
      // Step 5: Wait for niche selection
      e2e_test_runner.Wait(2000),
      // Step 6: Click Business niche button
      e2e_test_runner.ClickButton("reels:niche:business", 3000),
      // Step 7: Skip product description
      e2e_test_runner.ClickButton("reels:skip_product", 3000),
      // Step 8: Confirm and start generation
      e2e_test_runner.ClickButton("reels:confirm", 5000),
      // Step 9: Wait for generation to complete (up to 5 minutes)
      e2e_test_runner.WaitForResponse("генерация|готов|video|mp4|рилс|Error", 300000),
    ],
    final_pattern: "готов|video|mp4|Error|failed",
    timeout_ms: 360000,  // 6 minutes total timeout
  )

  // Start async multi-step test
  let test_run_id = e2e_test_runner.start_async_multi([reels_test])

  // Return 202 Accepted immediately
  let body = json.object([
    #("status", json.string("running")),
    #("test_run_id", json.string(test_run_id)),
    #("test_type", json.string("reels_full_flow")),
    #("timeout_seconds", json.int(360)),
    #("message", json.string("Reels full flow test started. Poll /api/e2e/status/" <> test_run_id <> " for results.")),
    #("note", json.string("This test performs the complete reels NLP flow: trigger → template → idea → niche → confirm → generate.")),
    #("steps", json.array([
      json.string("1. Send NLP trigger: хочу создать рилс"),
      json.string("2. Click Split Talking Head template"),
      json.string("3. Enter idea: 5 способов повысить продуктивность"),
      json.string("4. Select Business niche"),
      json.string("5. Skip product"),
      json.string("6. Confirm and generate"),
      json.string("7. Wait for video"),
    ], fn(x) { x })),
  ]) |> json.to_string()

  response.new(202)
  |> response.set_header("content-type", "application/json")
  |> response.set_header("location", "/api/e2e/status/" <> test_run_id)
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

/// Reels Pipeline E2E test handler - GET /api/e2e/reels-pipeline
/// Tests the pipeline DIRECTLY without Telegram UI or buttons
/// This bypasses MTProto button click issues
pub fn reels_pipeline_handler() -> Response(ResponseData) {
  io.println("[E2E-REELS-PIPELINE] 🎬 DIRECT PIPELINE TEST STARTED")

  // Get config from environment
  let remotion_url = config.get_env_or("REMOTION_URL", "https://vibee-remotion.fly.dev")
  let test_assets = config.get_env_or("TEST_ASSETS_URL", "https://vibee-remotion.fly.dev/public")
  let elevenlabs_key = config.get_env_or("ELEVENLABS_API_KEY", "")
  let fal_key = config.get_env_or("FAL_API_KEY", "")

  // Configure pipeline
  let pipeline_config = vibee_pipeline.PipelineConfig(
    elevenlabs_api_key: elevenlabs_key,
    fal_api_key: fal_key,
    remotion_url: remotion_url,
    test_assets_url: test_assets,
  )

  // Test request with test photo and script
  let test_request = vibee_pipeline.PipelineRequest(
    photo_url: test_assets <> "/photos/avatar_test.jpg",
    script_text: "Привет! Это тестовое видео для проверки pipeline. Продуктивность - это важно!",
    voice_id: None,
    webhook_url: None,
    test_mode: True,  // Use test mode for faster execution
  )

  io.println("[E2E-REELS-PIPELINE] Starting test pipeline...")
  io.println("[E2E-REELS-PIPELINE] Remotion URL: " <> remotion_url)
  io.println("[E2E-REELS-PIPELINE] Test assets: " <> test_assets)

  // Run the pipeline directly
  case vibee_pipeline.start_test_pipeline(pipeline_config, test_request) {
    Ok(job) -> {
      io.println("[E2E-REELS-PIPELINE] ✅ Pipeline started, job_id: " <> job.id)

      // Return success with job info
      let body = json.object([
        #("status", json.string("success")),
        #("job_id", json.string(job.id)),
        #("pipeline_state", json.string(pipeline_state_to_string(job.state))),
        #("progress", json.int(job.progress)),
        #("current_step", json.string(job.current_step)),
        #("message", json.string("Pipeline test started successfully. Check job status for video_url.")),
        #("test_config", json.object([
          #("remotion_url", json.string(remotion_url)),
          #("test_assets_url", json.string(test_assets)),
          #("test_mode", json.bool(True)),
        ])),
      ]) |> json.to_string()

      response.new(200)
      |> response.set_header("content-type", "application/json")
      |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
    }
    Error(err) -> {
      let err_str = pipeline_error_to_string(err)
      io.println("[E2E-REELS-PIPELINE] ❌ Pipeline error: " <> err_str)

      let body = json.object([
        #("status", json.string("error")),
        #("error", json.string(err_str)),
        #("message", json.string("Pipeline test failed. Check configuration and API keys.")),
      ]) |> json.to_string()

      response.new(500)
      |> response.set_header("content-type", "application/json")
      |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
    }
  }
}

/// Convert pipeline state to string
fn pipeline_state_to_string(state: vibee_pipeline.PipelineState) -> String {
  case state {
    vibee_pipeline.Pending -> "pending"
    vibee_pipeline.GeneratingTTS -> "generating_tts"
    vibee_pipeline.GeneratingLipsync -> "generating_lipsync"
    vibee_pipeline.GeneratingBRoll -> "generating_broll"
    vibee_pipeline.BuildingSegments -> "building_segments"
    vibee_pipeline.Rendering -> "rendering"
    vibee_pipeline.Completed(url) -> "completed:" <> url
    vibee_pipeline.Failed(err) -> "failed:" <> err
  }
}

/// Convert pipeline error to string
fn pipeline_error_to_string(err: vibee_pipeline.PipelineError) -> String {
  case err {
    vibee_pipeline.TTSError(msg) -> "TTS error: " <> msg
    vibee_pipeline.LipsyncError(msg) -> "Lipsync error: " <> msg
    vibee_pipeline.RenderError(msg) -> "Render error: " <> msg
    vibee_pipeline.ConfigError(msg) -> "Config error: " <> msg
    vibee_pipeline.NetworkError(msg) -> "Network error: " <> msg
    vibee_pipeline.BRollError(msg) -> "B-roll error: " <> msg
  }
}

/// Run AI E2E tests handler - GET /api/e2e/ai
/// Tests AI commands like /neuro, /voice, /video
pub fn ai_handler() -> Response(ResponseData) {
  case run_ai_tests() {
    Ok(results) -> {
      let body = encode_results(results) |> json.to_string()
      response.new(200)
      |> response.set_header("content-type", "application/json")
      |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
    }
    Error(err) -> {
      let body = json.object([
        #("error", json.string(err)),
        #("status", json.string("failed")),
      ]) |> json.to_string()
      response.new(500)
      |> response.set_header("content-type", "application/json")
      |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
    }
  }
}

/// Run AI function tests
pub fn run_ai_tests() -> Result(E2EResults, String) {
  let logger = vibe_logger.new("E2E-AI")
    |> vibe_logger.with_data("test_type", json.string("ai_functions"))

  let bridge_url = get_bridge_url()
  let api_key = config.require_env("VIBEE_API_KEY")

  vibe_logger.info(logger, "🤖 AI FUNCTION E2E TESTS STARTING")

  let tester_session = config.require_env("TELEGRAM_SESSION_ID_TESTER")
  let bot_username = "vibee_agent"
  let bot_chat_id = config.get_env_int_or("VIBEE_AGENT_USER_ID", 0)

  let bridge = client.with_session_and_key(bridge_url, tester_session, api_key)

  case client.get_me(bridge) {
    Error(err) -> {
      let err_str = telegram_error_to_string(err)
      Error("Failed to verify tester session: " <> err_str)
    }
    Ok(me) -> {
      let tester_username = me.username |> option.unwrap("unknown")

      // AI test cases - NLP triggers only! No /commands for Digital Twin
      // Pattern: NLP trigger, expected pattern in response
      let ai_test_cases = [
        // NLP neuro - генерация изображений (FAL.ai)
        #("сгенерируй фото кота в космосе", "генерир|создаю|processing|fal.ai|изображение"),
        // NLP voice - голосовой клон (ElevenLabs)
        #("озвучь текст Привет мир", "голос|voice|elevenlabs|audio|синтез"),
        // NLP video - видео генерация (Kling)
        #("создай видео про закат над океаном", "видео|video|kling|генерир|processing"),
      ]

      let tests = list.map(ai_test_cases, fn(tc) {
        let #(command, expected) = tc
        run_ai_test(bridge, bot_chat_id, command, expected)
      })

      let passed = list.count(tests, fn(t) { t.passed })
      let failed = list.length(tests) - passed

      Ok(E2EResults(
        tester_session: tester_session,
        tester_username: tester_username,
        bot_username: bot_username,
        bot_chat_id: bot_chat_id,
        tests: tests,
        total: list.length(tests),
        passed: passed,
        failed: failed,
      ))
    }
  }
}

/// Run a single AI test with longer timeout
fn run_ai_test(
  bridge: client.TelegramBridge,
  chat_id: Int,
  command: String,
  expected_pattern: String,
) -> TestResult {
  let start = erlang_monotonic_time()
  io.println("")
  io.println("[E2E-AI] ═══════════════════════════════════════")
  io.println("[E2E-AI] 🤖 Testing: " <> command)

  case client.send_message(bridge, chat_id, command, None) {
    Error(err) -> {
      let err_str = telegram_error_to_string(err)
      io.println("[E2E-AI] ❌ Failed to send: " <> err_str)
      TestResult(
        command: command,
        expected: expected_pattern,
        passed: False,
        response: "Failed to send: " <> err_str,
        duration_ms: 0,
      )
    }
    Ok(_) -> {
      io.println("[E2E-AI] ✅ Command sent!")
      io.println("[E2E-AI] ⏳ Waiting 30s for AI processing...")

      // Longer wait for AI processing
      process.sleep(30000)

      case client.get_history(bridge, chat_id, 15) {
        Error(err) -> {
          let err_str = telegram_error_to_string(err)
          TestResult(
            command: command,
            expected: expected_pattern,
            passed: False,
            response: "Failed to get history: " <> err_str,
            duration_ms: elapsed_ms(start),
          )
        }
        Ok(messages) -> {
          io.println("[E2E-AI] 📬 Got " <> int.to_string(list.length(messages)) <> " messages")
          let #(response_text, passed) = find_matching_response(messages, expected_pattern)

          // Also check for error messages
          let has_error = pattern_matches(response_text, "error|ошибка|недоступ|not available|missing key")
          let final_passed = passed && !has_error

          io.println("[E2E-AI] 💬 Response: " <> string.slice(response_text, 0, 100))
          io.println("[E2E-AI] " <> case final_passed {
            True -> "✅ PASS"
            False -> case has_error {
              True -> "⚠️ ERROR RESPONSE"
              False -> "❌ FAIL"
            }
          })

          TestResult(
            command: command,
            expected: expected_pattern,
            passed: final_passed,
            response: string.slice(response_text, 0, 300),
            duration_ms: elapsed_ms(start),
          )
        }
      }
    }
  }
}

/// Run E2E tests - can be called from HTTP handler or MCP tool
pub fn run_e2e_tests() -> Result(E2EResults, String) {
  // Create structured logger for E2E tests
  let logger = vibe_logger.new("E2E")
    |> vibe_logger.with_data("test_type", json.string("rainbow_bridge"))

  // Get bridge URL and API key
  let bridge_url = get_bridge_url()
  let api_key = config.require_env("VIBEE_API_KEY")

  vibe_logger.info(logger, "🌈 RAINBOW BRIDGE E2E TESTS STARTING")
  vibe_logger.info(logger
    |> vibe_logger.with_data("bridge_url", json.string(bridge_url))
    |> vibe_logger.with_data("api_key", json.string(string.slice(api_key, 0, 10) <> "...")),
    "Configuration loaded")

  // Test accounts loaded from environment (see .env.example)
  let tester_session = config.require_env("TELEGRAM_SESSION_ID_TESTER")
  let bot_username = "vibee_agent"
  let bot_chat_id = config.get_env_int_or("VIBEE_AGENT_USER_ID", 0)

  vibe_logger.info(logger
    |> vibe_logger.with_session_id(tester_session)
    |> vibe_logger.with_data("bot_chat_id", json.int(bot_chat_id)),
    "Test accounts configured")

  // Create bridge client with tester session and API key
  let bridge = client.with_session_and_key(bridge_url, tester_session, api_key)

  vibe_logger.debug(logger, "Verifying tester session...")

  // Verify tester session
  case client.get_me(bridge) {
    Error(err) -> {
      let err_str = telegram_error_to_string(err)
      vibe_logger.error(logger
        |> vibe_logger.with_data("error", json.string(err_str)),
        "get_me failed - tester session verification failed")
      Error("Failed to verify tester session @neuro_sage: " <> err_str)
    }
    Ok(me) -> {
      let tester_username = me.username |> option.unwrap("unknown")

      // Define NLP test cases - no /commands for Digital Twin!
      let test_cases = [
        #("что ты умеешь", "neurophoto|video|возможност|умею"),
        #("покажи тарифы", "JUNIOR|MIDDLE|Тариф|$99"),
        #("хочу создать видео", "Kling|Minimax|Выбери|провайдер|видео"),
        #("создай рилс про бизнес", "рилс|шаблон|Split|Talking|template|бизнес"),
      ]

      // Run bot command tests
      let command_tests = list.map(test_cases, fn(tc) {
        let #(command, expected) = tc
        run_single_test(bridge, bot_chat_id, command, expected)
      })

      // Run lead forwarding test
      let lead_test = run_lead_forward_test(bridge)

      // Combine all tests
      let tests = list.append(command_tests, [lead_test])

      let passed = list.count(tests, fn(t) { t.passed })
      let failed = list.length(tests) - passed

      Ok(E2EResults(
        tester_session: tester_session,
        tester_username: tester_username,
        bot_username: bot_username,
        bot_chat_id: bot_chat_id,
        tests: tests,
        total: list.length(tests),
        passed: passed,
        failed: failed,
      ))
    }
  }
}

/// Run a single test
fn run_single_test(
  bridge: client.TelegramBridge,
  chat_id: Int,
  command: String,
  expected_pattern: String,
) -> TestResult {
  let start = erlang_monotonic_time()
  io.println("")
  io.println("[E2E] ═══════════════════════════════════════")
  io.println("[E2E] 📤 Sending: " <> command)
  io.println("[E2E] 📍 To chat_id: " <> int.to_string(chat_id))

  // Send command
  case client.send_message(bridge, chat_id, command, None) {
    Error(err) -> {
      let err_str = telegram_error_to_string(err)
      io.println("[E2E] ❌ Failed to send: " <> err_str)
      TestResult(
        command: command,
        expected: expected_pattern,
        passed: False,
        response: "Failed to send message: " <> err_str,
        duration_ms: 0,
      )
    }
    Ok(send_result) -> {
      let sent_msg_id = send_result.message_id
      io.println("[E2E] ✅ Message sent! msg_id=" <> int.to_string(sent_msg_id))
      io.println("[E2E] Waiting 15s for bot response...")

      // Wait for response (polling cycle 5s + LLM processing time)
      process.sleep(15000)

      io.println("[E2E] 📥 Fetching history...")
      // Get history
      case client.get_history(bridge, chat_id, 10) {
        Error(err) -> {
          let err_str = telegram_error_to_string(err)
          io.println("[E2E] ❌ Failed to get history: " <> err_str)
          TestResult(
            command: command,
            expected: expected_pattern,
            passed: False,
            response: "Failed to get history: " <> err_str,
            duration_ms: elapsed_ms(start),
          )
        }
        Ok(messages) -> {
          io.println("[E2E] 📬 Got " <> int.to_string(list.length(messages)) <> " messages")
          // Find any bot response that matches the expected pattern
          let #(response_text, passed) = find_matching_response(messages, expected_pattern)
          io.println("[E2E] 💬 Response: " <> string.slice(response_text, 0, 80) <> "...")
          io.println("[E2E] " <> case passed { True -> "✅ PASS" False -> "❌ FAIL" } <> " (expected: " <> expected_pattern <> ")")

          TestResult(
            command: command,
            expected: expected_pattern,
            passed: passed,
            response: string.slice(response_text, 0, 200),
            duration_ms: elapsed_ms(start),
          )
        }
      }
    }
  }
}

/// Test lead forwarding: trigger in Aimly group → agent reply → forward to leads group
/// NOTE: This test is OPTIONAL - requires tester to be member of both trigger and leads groups
/// If access is not available, test is SKIPPED (passed=True)
fn run_lead_forward_test(bridge: client.TelegramBridge) -> TestResult {
  let start = erlang_monotonic_time()

  io.println("")
  io.println("[E2E] ═══════════════════════════════════════")
  io.println("[E2E] 🔄 LEAD FORWARD TEST (Optional)")

  // SINGLE SOURCE OF TRUTH: Get chat IDs from trigger_chats.gleam config
  let trigger_configs = trigger_chats.get_trigger_chats()
  case list.first(trigger_configs) {
    Error(_) -> {
      io.println("[E2E] ℹ️ No trigger chat configs - SKIPPING")
      TestResult(
        command: "lead_forward",
        expected: "optional",
        passed: True,  // Skip = pass
        response: "SKIPPED: No trigger chat configs",
        duration_ms: 0,
      )
    }
    Ok(config) -> {
      // Normalize chat IDs - remove -100 prefix if present
      let trigger_chat_id = normalize_chat_id_for_api(config.chat_id)
      let leads_chat_id = normalize_chat_id_for_api(config.forward_chat_id)

      io.println("[E2E] 📋 Config:")
      io.println("[E2E]    trigger_chat: " <> config.chat_name)
      io.println("[E2E]    config chat_id: " <> config.chat_id)
      io.println("[E2E]    normalized: " <> int.to_string(trigger_chat_id))
      io.println("[E2E]    leads_chat: " <> int.to_string(leads_chat_id))

      // Check if tester can access trigger chat
      io.println("[E2E] 🔍 Checking tester access...")
      case client.get_history(bridge, trigger_chat_id, 1) {
        Error(err) -> {
          let err_str = telegram_error_to_string(err)
          io.println("[E2E] ⚠️ No access to trigger chat: " <> err_str)
          io.println("[E2E] ℹ️ SKIPPING - add @neuro_sage to " <> config.chat_name)
          TestResult(
            command: "lead_forward",
            expected: "optional",
            passed: True,
            response: "SKIPPED: Tester not in " <> config.chat_name,
            duration_ms: elapsed_ms(start),
          )
        }
        Ok(_) -> {
          io.println("[E2E] ✅ Tester has access to trigger chat")

          case client.get_history(bridge, leads_chat_id, 1) {
            Error(err) -> {
              let err_str = telegram_error_to_string(err)
              io.println("[E2E] ⚠️ No access to leads chat - SKIPPING")
              TestResult(
                command: "lead_forward",
                expected: "optional",
                passed: True,
                response: "SKIPPED: Tester not in leads group",
                duration_ms: elapsed_ms(start),
              )
            }
            Ok(_) -> {
              io.println("[E2E] ✅ Tester has access to leads chat")

              // Add unique timestamp to trigger message to ensure it's always a NEW message
              // This prevents false negatives due to duplicate text matching old seen_ids
              let base_trigger = case list.first(config.custom_triggers) {
                Ok(t) -> t
                Error(_) -> "тест"
              }
              let timestamp = int.to_string(elapsed_ms(0))  // Current unix millis
              let trigger_message = base_trigger <> " [E2E:" <> timestamp <> "]"

              let expected_response = config.expected_response_pattern
              let expected_forward = config.expected_forward_pattern

              io.println("[E2E]    trigger: " <> trigger_message)
              io.println("[E2E]    unique_id: " <> timestamp)

              run_lead_forward_test_impl(bridge, start, trigger_chat_id, leads_chat_id,
                config.chat_name, trigger_message, expected_response, expected_forward)
            }
          }
        }
      }
    }
  }
}

/// Parse chat_id for API calls - Go Bridge handles format
fn normalize_chat_id_for_api(chat_id: String) -> Int {
  // Pass chat_id as-is to Go Bridge
  // Bridge handles both regular groups (-xxx) and supergroups (-100xxx)
  case int.parse(chat_id) {
    Ok(id) -> id
    Error(_) -> 0
  }
}

/// Implementation of lead forward test
fn run_lead_forward_test_impl(
  bridge: client.TelegramBridge,
  start: Int,
  aimly_chat_id: Int,
  leads_chat_id: Int,
  chat_name: String,
  trigger_message: String,
  expected_response: String,
  expected_forward: String,
) -> TestResult {

  io.println("")
  io.println("[E2E] ═══════════════════════════════════════")
  io.println("[E2E] 🔄 LEAD FORWARD TEST")

  // Step 0: Sync dialogs to ensure channels are cached
  io.println("[E2E] 📋 Syncing dialogs for tester session...")
  case client.get_dialogs(bridge, 100) {
    Error(err) -> {
      let err_str = telegram_error_to_string(err)
      io.println("[E2E] ⚠️ Failed to sync dialogs: " <> err_str)
    }
    Ok(dialogs) -> {
      io.println("[E2E] ✅ Synced " <> int.to_string(list.length(dialogs)) <> " dialogs")
      // Log dialogs to help debug
      list.each(dialogs, fn(d) {
        io.println("[E2E] 📱 Dialog: " <> d.title <> " (id=" <> int.to_string(d.id) <> ")")
      })
    }
  }

  io.println("[E2E] 📤 Sending trigger to " <> chat_name <> ": " <> trigger_message)
  io.println("[E2E] 🎯 aimly_chat_id=" <> int.to_string(aimly_chat_id))
  io.println("[E2E] 🎯 leads_chat_id=" <> int.to_string(leads_chat_id))
  io.println("[E2E] 🔑 Session: " <> option.unwrap(bridge.session_id, "NONE"))
  io.println("[E2E] 🌐 Bridge URL: " <> bridge.base_url)

  // Step 0.5: Get last msg_id in BOTH groups BEFORE sending trigger
  // This allows us to check for NEW messages only (both response and Lead Card)
  let last_aimly_msg_id = case client.get_history(bridge, aimly_chat_id, 1) {
    Ok(msgs) -> case list.first(msgs) {
      Ok(m) -> {
        io.println("[E2E] 📊 Last aimly msg_id before trigger: " <> int.to_string(m.id))
        m.id
      }
      Error(_) -> 0
    }
    Error(_) -> 0
  }

  let last_leads_msg_id = case client.get_history(bridge, leads_chat_id, 1) {
    Ok(msgs) -> case list.first(msgs) {
      Ok(m) -> {
        io.println("[E2E] 📊 Last leads msg_id before trigger: " <> int.to_string(m.id))
        m.id
      }
      Error(_) -> 0
    }
    Error(_) -> 0
  }

  // Step 1: Send trigger to Aimly group
  case client.send_message(bridge, aimly_chat_id, trigger_message, None) {
    Error(err) -> {
      let err_str = telegram_error_to_string(err)
      io.println("[E2E] ❌ Failed to send trigger: " <> err_str)
      io.println("[E2E] ❌ Error details: chat_id=" <> int.to_string(aimly_chat_id))
      TestResult(
        command: "lead_forward",
        expected: "response + forward",
        passed: False,
        response: "Failed to send trigger: " <> err_str,
        duration_ms: 0,
      )
    }
    Ok(_) -> {
      // Step 2: Wait for processing (15s for trigger + forward)
      io.println("[E2E] ⏳ Waiting 15s for agent to process and forward...")
      process.sleep(15_000)

      // Step 3: Check trigger group for agent response
      io.println("[E2E] 📥 Checking " <> chat_name <> " for response...")
      case client.get_history(bridge, aimly_chat_id, 5) {
        Error(err) -> {
          let err_str = telegram_error_to_string(err)
          io.println("[E2E] ❌ Failed to get trigger chat history: " <> err_str)
          TestResult(
            command: "lead_forward",
            expected: "response + forward",
            passed: False,
            response: "Failed to get trigger chat history: " <> err_str,
            duration_ms: elapsed_ms(start),
          )
        }
        Ok(aimly_msgs) -> {
          // IMPORTANT: Filter only NEW messages (msg_id > last_aimly_msg_id)
          // This prevents false positives from old agent responses
          let new_aimly_msgs = list.filter(aimly_msgs, fn(m) { m.id > last_aimly_msg_id })
          io.println("[E2E] 📊 Found " <> int.to_string(list.length(new_aimly_msgs)) <> " NEW messages in aimly chat")

          // Log new messages for debugging
          list.each(new_aimly_msgs, fn(m) {
            io.println("[E2E] 📨 NEW aimly msg_id=" <> int.to_string(m.id) <> " from=" <> m.from_name <> " text=" <> string.slice(m.text, 0, 40))
          })

          // Search only in NEW messages for agent response
          let #(response, resp_passed) = find_matching_response(new_aimly_msgs, expected_response)
          io.println("[E2E] 💬 Trigger response: " <> string.slice(response, 0, 60))
          io.println("[E2E] " <> case resp_passed { True -> "✅ Response PASS" False -> "❌ Response FAIL" })

          // If no new matching response, show diagnostic
          case resp_passed {
            False -> {
              io.println("[E2E] ⚠️ NO NEW agent response found! This indicates agent did not process the trigger.")
              io.println("[E2E] ⚠️ Possible causes:")
              io.println("[E2E] ⚠️   1. Trigger message already in seen_ids")
              io.println("[E2E] ⚠️   2. Agent not processing Aimly.io dev chat")
              io.println("[E2E] ⚠️   3. Trigger word not matching")
            }
            True -> Nil
          }

          // Step 4: Wait a bit more for forward to complete
          io.println("[E2E] ⏳ Waiting 5s for forward to complete...")
          process.sleep(5000)

          // Step 5: Check leads group for NEW forwarded dialog
          io.println("[E2E] 📥 Checking leads group for NEW forwarded dialog (limit=15)...")
          io.println("[E2E] 📊 Looking for messages with msg_id > " <> int.to_string(last_leads_msg_id))
          case client.get_history(bridge, leads_chat_id, 15) {
            Error(err) -> {
              let err_str = telegram_error_to_string(err)
              io.println("[E2E] ❌ Failed to get leads history: " <> err_str)
              TestResult(
                command: "lead_forward",
                expected: "response + forward",
                passed: False,
                response: "Aimly: " <> string.slice(response, 0, 80) <> " | Leads: error - " <> err_str,
                duration_ms: elapsed_ms(start),
              )
            }
            Ok(leads_msgs) -> {
              // IMPORTANT: Filter only NEW messages (msg_id > last_leads_msg_id)
              // This prevents false positives from old Lead Cards
              let new_msgs = list.filter(leads_msgs, fn(m) { m.id > last_leads_msg_id })
              io.println("[E2E] 📊 Found " <> int.to_string(list.length(new_msgs)) <> " NEW messages in leads group")

              // Log new messages for debugging
              list.each(new_msgs, fn(m) {
                io.println("[E2E] 📨 NEW msg_id=" <> int.to_string(m.id) <> " text=" <> string.slice(m.text, 0, 50))
              })

              // Search only in NEW messages
              let #(forward, fwd_passed) = find_any_matching_message(new_msgs, expected_forward)

              // If no new matching message found, show diagnostic info
              case fwd_passed {
                False -> {
                  io.println("[E2E] ⚠️ NO NEW Lead Card found! Expected pattern: " <> expected_forward)
                  io.println("[E2E] ⚠️ This indicates forward_dialog did not execute")
                }
                True -> {
                  io.println("[E2E] ✅ NEW Lead Card found!")
                }
              }

              let passed = resp_passed && fwd_passed

              TestResult(
                command: "lead_forward",
                expected: "response + forward",
                passed: passed,
                response: "Aimly: " <> string.slice(response, 0, 80) <> " | Leads: " <> string.slice(forward, 0, 80),
                duration_ms: elapsed_ms(start),
              )
            }
          }
        }
      }
    }
  }
}

/// Find a bot response that matches the expected pattern
fn find_matching_response(messages: List(TelegramMessage), expected_pattern: String) -> #(String, Bool) {
  // @vibee_agent user ID - Single Source of Truth
  let vibee_agent_id = config.get_env_int_or("VIBEE_AGENT_USER_ID", 0)

  // Debug log all messages
  list.each(messages, fn(m: TelegramMessage) {
    io.println("[E2E] msg_id=" <> int.to_string(m.id) <> " from_id=" <> int.to_string(m.from_id) <> " from=" <> m.from_name <> " text=" <> string.slice(m.text, 0, 40))
  })

  // Get all bot responses (non-command messages from @vibee_agent by user ID)
  let bot_responses = list.filter(messages, fn(m: TelegramMessage) {
    !string.starts_with(m.text, "/") && m.from_id == vibee_agent_id
  })

  io.println("[E2E] 📊 Found " <> int.to_string(list.length(bot_responses)) <> " bot responses in history")

  // Find the first response that matches the pattern
  let matching = list.find(bot_responses, fn(m: TelegramMessage) {
    pattern_matches(m.text, expected_pattern)
  })

  case matching {
    Ok(m) -> {
      io.println("[E2E] 🎯 Matching response (msg_id=" <> int.to_string(m.id) <> "): " <> string.slice(m.text, 0, 60))
      #(m.text, True)
    }
    Error(_) -> {
      // No matching response, return the latest bot response for debugging
      case list.first(bot_responses) {
        Ok(m) -> {
          io.println("[E2E] ⚠️ No matching response. Latest (msg_id=" <> int.to_string(m.id) <> "): " <> string.slice(m.text, 0, 60))
          #(m.text, False)
        }
        Error(_) -> {
          io.println("[E2E] ⚠️ No bot response found!")
          #("", False)
        }
      }
    }
  }
}

/// Find ANY message (from any sender) that matches the expected pattern
fn find_any_matching_message(messages: List(TelegramMessage), expected_pattern: String) -> #(String, Bool) {
  // Find the first message that matches the pattern (from ANY sender)
  let matching = list.find(messages, fn(m: TelegramMessage) {
    pattern_matches(m.text, expected_pattern)
  })

  case matching {
    Ok(m) -> #(m.text, True)
    Error(_) -> {
      // No matching message, return the latest for debugging
      case list.first(messages) {
        Ok(m) -> #(m.text, False)
        Error(_) -> #("", False)
      }
    }
  }
}

/// Check if response matches expected pattern (case-insensitive, OR patterns with |)
fn pattern_matches(text: String, pattern: String) -> Bool {
  let lower_text = string.lowercase(text)
  let patterns = string.split(pattern, "|")
  list.any(patterns, fn(p) {
    string.contains(lower_text, string.lowercase(p))
  })
}

/// Get bridge URL from config
fn get_bridge_url() -> String {
  case postgres.get_global_pool() {
    None -> config.get_env_or("VIBEE_BRIDGE_URL", "https://vibee-telegram-bridge.fly.dev")
    Some(pool) -> dynamic_config.get_bridge_url(pool)
  }
}


/// Encode results to JSON - public for MCP tool usage
pub fn encode_results(results: E2EResults) -> json.Json {
  json.object([
    #("status", json.string(case results.failed { 0 -> "passed" _ -> "failed" })),
    #("tester_session", json.string(results.tester_session)),
    #("tester_username", json.string(results.tester_username)),
    #("bot_username", json.string(results.bot_username)),
    #("bot_chat_id", json.int(results.bot_chat_id)),
    #("total", json.int(results.total)),
    #("passed", json.int(results.passed)),
    #("failed", json.int(results.failed)),
    #("tests", json.array(results.tests, encode_test_result)),
  ])
}

fn encode_test_result(t: TestResult) -> json.Json {
  json.object([
    #("command", json.string(t.command)),
    #("expected", json.string(t.expected)),
    #("passed", json.bool(t.passed)),
    #("response", json.string(t.response)),
    #("duration_ms", json.int(t.duration_ms)),
  ])
}

/// Poll render completion for pipeline test
fn poll_pipeline_render(remotion_url: String, job: vibee_pipeline.PipelineJob) -> Result(String, String) {
  case job.render_id {
    None -> Error("No render ID available")
    Some(render_id) -> {
      let max_attempts = 60  // 5 minutes with 5s intervals
      poll_render_status_loop(remotion_url, render_id, 0, max_attempts)
    }
  }
}

fn poll_render_status_loop(
  remotion_url: String,
  render_id: String,
  attempt: Int,
  max_attempts: Int,
) -> Result(String, String) {
  case attempt >= max_attempts {
    True -> Error("Render timeout after " <> int.to_string(max_attempts * 5) <> " seconds")
    False -> {
      // Wait 5 seconds between polls
      process.sleep(5000)

      // Check render status
      case check_render_status(remotion_url, render_id) {
        Ok(#("done", url)) -> Ok(url)
        Ok(#("error", err)) -> Error("Render error: " <> err)
        Ok(#("pending", _)) -> {
          io.println("[E2E-REELS-PIPELINE] Render in progress... attempt " <> int.to_string(attempt + 1))
          poll_render_status_loop(remotion_url, render_id, attempt + 1, max_attempts)
        }
        Ok(#(status, _)) -> {
          io.println("[E2E-REELS-PIPELINE] Render status: " <> status)
          poll_render_status_loop(remotion_url, render_id, attempt + 1, max_attempts)
        }
        Error(err) -> {
          io.println("[E2E-REELS-PIPELINE] Status check error: " <> err)
          poll_render_status_loop(remotion_url, render_id, attempt + 1, max_attempts)
        }
      }
    }
  }
}

fn check_render_status(remotion_url: String, render_id: String) -> Result(#(String, String), String) {
  // Use httpc to check render status
  let url = remotion_url <> "/renders/" <> render_id

  case make_get_request(url) {
    Ok(body) -> parse_render_status(body)
    Error(err) -> Error(err)
  }
}

fn make_get_request(url: String) -> Result(String, String) {
  // Parse URL to get host and path
  let parts = string.split(url, "://")
  case parts {
    [_, rest] -> {
      case string.split(rest, "/") {
        [host, ..path_parts] -> {
          let path = "/" <> string.join(path_parts, "/")

          let req = gleam_http_request.new()
            |> gleam_http_request.set_method(gleam_http.Get)
            |> gleam_http_request.set_scheme(gleam_http.Https)
            |> gleam_http_request.set_host(host)
            |> gleam_http_request.set_path(path)

          case gleam_httpc.send(req) {
            Ok(resp) -> Ok(resp.body)
            Error(e) -> Error("HTTP error: " <> string.inspect(e))
          }
        }
        _ -> Error("Invalid URL format")
      }
    }
    _ -> Error("Invalid URL scheme")
  }
}

fn parse_render_status(body: String) -> Result(#(String, String), String) {
  // Simple JSON parsing for status
  case string.contains(body, "\"done\":true") {
    True -> {
      // Extract outputUrl
      case extract_json_field(body, "outputUrl") {
        Some(url) -> Ok(#("done", url))
        None -> Ok(#("done", ""))
      }
    }
    False -> {
      case string.contains(body, "\"error\"") {
        True -> {
          case extract_json_field(body, "error") {
            Some(err) -> Ok(#("error", err))
            None -> Ok(#("error", "Unknown error"))
          }
        }
        False -> Ok(#("pending", ""))
      }
    }
  }
}

fn extract_json_field(json_str: String, field: String) -> Option(String) {
  // Simple extraction: find "field":"value"
  let pattern = "\"" <> field <> "\":\""
  case string.split(json_str, pattern) {
    [_, rest] -> {
      case string.split(rest, "\"") {
        [value, ..] -> Some(value)
        _ -> None
      }
    }
    _ -> None
  }
}

// FFI for timing
@external(erlang, "erlang", "monotonic_time")
fn erlang_monotonic_time() -> Int

fn elapsed_ms(start: Int) -> Int {
  let end = erlang_monotonic_time()
  // Convert from native time units to milliseconds
  { end - start } / 1_000_000
}

/// Convert TelegramError to string
fn telegram_error_to_string(err: TelegramError) -> String {
  case err {
    types.ConnectionError(msg) -> "ConnectionError: " <> msg
    types.AuthError(msg) -> "AuthError: " <> msg
    types.ApiError(code, msg) -> "ApiError(" <> int.to_string(code) <> "): " <> msg
    types.NetworkError(msg) -> "NetworkError: " <> msg
    types.InvalidSession -> "InvalidSession"
    types.NotAuthorized -> "NotAuthorized"
  }
}
