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

  // Define tests to run
  let tests = [
    e2e_test_runner.E2ETest("/help", "neurophoto|video|/menu|Команды"),
    e2e_test_runner.E2ETest("/pricing", "JUNIOR|MIDDLE|Тариф|$99"),
    // Test Bot API keyboard - /video should show provider selection buttons
    e2e_test_runner.E2ETest("/video", "Kling|Minimax|Выбери|провайдер"),
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
    bot_chat_id: 6579515876,
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
  let api_key = config.get_env_or("VIBEE_API_KEY", "vibee-secret-2024-prod")
  let tester_session = config.get_env_or("TELEGRAM_SESSION_ID_TESTER", "REDACTED_SESSION")
  let bot_chat_id = 6579515876

  let bridge = client.with_session_and_key(bridge_url, tester_session, api_key)

  // Send /neuro command
  vibe_logger.info(logger, "📤 Sending /neuro command to bot...")
  case client.send_message(bridge, bot_chat_id, "/neuro кот в космосе", None) {
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
          let vibee_agent_id = 6579515876
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

/// Video E2E tests handler - GET /api/e2e/video
/// Tests video commands to verify they respond correctly
/// NOTE: Full video generation with button clicks not supported for user-bots (MTProto)
///       because MessagesGetBotCallbackAnswer only works for actual Telegram bots.
/// Returns 202 Accepted with test_run_id for polling
pub fn video_handler() -> Response(ResponseData) {
  io.println("[E2E-VIDEO] 🎬 VIDEO E2E HANDLER STARTED")

  // Initialize ETS
  e2e_test_runner.init()

  // Test video commands - verify they show provider selection
  // NOTE: Cannot click inline buttons on user-bot messages (MTProto limitation)
  // The Digital Twin is a user-bot, not a Telegram Bot API bot
  let tests = [
    // Test /video shows provider selection
    e2e_test_runner.E2ETest("/video", "Kling|Veo3|provider|выбери|choose"),
    // Test /morph shows style selection
    e2e_test_runner.E2ETest("/morph", "стиль|style|morph|face-to-many"),
    // Test /animate shows animation options
    e2e_test_runner.E2ETest("/animate", "анимац|animate|image-to-video"),
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
  let api_key = config.get_env_or("VIBEE_API_KEY", "vibee-secret-2024-prod")

  vibe_logger.info(logger, "🤖 AI FUNCTION E2E TESTS STARTING")

  let tester_session = config.get_env_or("TELEGRAM_SESSION_ID_TESTER", "REDACTED_SESSION")
  let bot_username = "vibee_agent"
  let bot_chat_id = 6579515876

  let bridge = client.with_session_and_key(bridge_url, tester_session, api_key)

  case client.get_me(bridge) {
    Error(err) -> {
      let err_str = telegram_error_to_string(err)
      Error("Failed to verify tester session: " <> err_str)
    }
    Ok(me) -> {
      let tester_username = me.username |> option.unwrap("unknown")

      // AI test cases - longer timeout needed (60s for AI processing)
      // Pattern: command, expected pattern in response
      let ai_test_cases = [
        // /neuro - генерация изображений (FAL.ai)
        #("/neuro кот в космосе", "генерир|создаю|processing|fal.ai|изображение|🎨"),
        // /voice - голосовой клон (ElevenLabs)
        #("/voice Привет мир", "голос|voice|elevenlabs|audio|🎤|синтез"),
        // /video - видео генерация (Kling - может не работать без ключа)
        #("/video закат над океаном", "видео|video|kling|генерир|🎬|processing"),
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
  let api_key = config.get_env_or("VIBEE_API_KEY", "vibee-secret-2024-prod")

  vibe_logger.info(logger, "🌈 RAINBOW BRIDGE E2E TESTS STARTING")
  vibe_logger.info(logger
    |> vibe_logger.with_data("bridge_url", json.string(bridge_url))
    |> vibe_logger.with_data("api_key", json.string(string.slice(api_key, 0, 10) <> "...")),
    "Configuration loaded")

  // Test accounts loaded from environment (see .env.example)
  let tester_session = config.get_env_or("TELEGRAM_SESSION_ID_TESTER", "REDACTED_SESSION")
  let bot_username = "vibee_agent"
  let bot_chat_id = 6579515876  // @vibee_agent user ID

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

      // Define test cases with Russian patterns
      let test_cases = [
        #("/help", "neurophoto|video|/menu|Komandy"),
        #("/pricing", "JUNIOR|MIDDLE|Тариф|$99"),
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
              io.println("[E2E] ⚠️ No access to leads chat: " <> err_str)
              io.println("[E2E] ℹ️ SKIPPING - add @neuro_sage to leads group")
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

              let trigger_message = case list.first(config.custom_triggers) {
                Ok(t) -> t
                Error(_) -> "тест"
              }
              let expected_response = config.expected_response_pattern
              let expected_forward = config.expected_forward_pattern

              io.println("[E2E]    trigger: " <> trigger_message)

              run_lead_forward_test_impl(bridge, start, trigger_chat_id, leads_chat_id,
                config.chat_name, trigger_message, expected_response, expected_forward)
            }
          }
        }
      }
    }
  }
}

/// Normalize chat_id for API calls - handles -100 prefix
fn normalize_chat_id_for_api(chat_id: String) -> Int {
  // First try to parse as-is
  case int.parse(chat_id) {
    Ok(id) -> {
      // If it has -100 prefix (supergroup format), try without it
      case string.starts_with(chat_id, "-100") {
        True -> {
          // Try parsing without -100 prefix: "-1005082217642" -> "-5082217642"
          let without_prefix = "-" <> string.drop_start(chat_id, 4)
          case int.parse(without_prefix) {
            Ok(normalized) -> normalized
            Error(_) -> id  // fallback to original
          }
        }
        False -> id
      }
    }
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
          let #(response, resp_passed) = find_matching_response(aimly_msgs, expected_response)
          io.println("[E2E] 💬 Trigger response: " <> string.slice(response, 0, 60))
          io.println("[E2E] " <> case resp_passed { True -> "✅ Response PASS" False -> "❌ Response FAIL" })

          // Step 4: Check leads group for forwarded dialog
          io.println("[E2E] 📥 Checking leads group for forwarded dialog...")
          case client.get_history(bridge, leads_chat_id, 5) {
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
              let #(forward, fwd_passed) = find_matching_response(leads_msgs, expected_forward)
              io.println("[E2E] 📨 Leads forward: " <> string.slice(forward, 0, 60))
              io.println("[E2E] " <> case fwd_passed { True -> "✅ Forward PASS" False -> "❌ Forward FAIL" })

              let passed = resp_passed && fwd_passed
              io.println("[E2E] " <> case passed { True -> "✅ LEAD TEST PASS" False -> "❌ LEAD TEST FAIL" })

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
  let vibee_agent_id = 6579515876

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
