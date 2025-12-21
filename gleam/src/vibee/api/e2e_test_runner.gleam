// E2E Async Test Runner
// Runs tests asynchronously and stores results in ETS for polling

import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/integrations/telegram/client
import vibee/integrations/telegram/types.{type TelegramMessage, type TelegramError}
import vibee/mcp/config
import vibee/vibe_logger

/// Test status enum
pub type TestStatus {
  Running
  Completed
  Failed(error: String)
}

/// Single test definition (simple command -> response)
pub type E2ETest {
  E2ETest(
    command: String,
    expected_pattern: String,
  )
}

/// Multi-step test step types
pub type E2ETestStep {
  /// Send a command or text
  SendCommand(text: String)
  /// Click inline button by callback data
  ClickButton(callback_data: String, wait_ms: Int)
  /// Wait for response matching pattern
  WaitForResponse(pattern: String, timeout_ms: Int)
  /// Wait fixed duration
  Wait(ms: Int)
}

/// Multi-step test definition for complex flows (e.g., video generation)
pub type MultiStepTest {
  MultiStepTest(
    name: String,
    steps: List(E2ETestStep),
    final_pattern: String,
    timeout_ms: Int,
  )
}

/// Single test result
pub type TestResult {
  TestResult(
    command: String,
    expected: String,
    passed: Bool,
    response: String,
    duration_ms: Int,
  )
}

/// E2E test run state
pub type E2ETestRun {
  E2ETestRun(
    id: String,
    status: TestStatus,
    tests: List(TestResult),
    started_at: Int,
    completed_at: Option(Int),
    tester_session: String,
    bot_chat_id: Int,
  )
}

// FFI declarations
@external(erlang, "vibee_e2e_runner_ffi", "init")
fn ffi_init() -> Nil

@external(erlang, "vibee_e2e_runner_ffi", "save_test_run")
fn ffi_save_test_run(id: String, run: E2ETestRun) -> Nil

@external(erlang, "vibee_e2e_runner_ffi", "get_test_run")
fn ffi_get_test_run(id: String) -> Dynamic

@external(erlang, "vibee_e2e_runner_ffi", "generate_id")
fn ffi_generate_id() -> String

@external(erlang, "vibee_e2e_runner_ffi", "current_time_ms")
fn ffi_current_time_ms() -> Int

@external(erlang, "vibee_e2e_runner_ffi", "spawn_async")
fn ffi_spawn_async(f: fn() -> Nil) -> Nil

/// Initialize ETS table
pub fn init() -> Nil {
  ffi_init()
}

/// Generate unique test run ID
pub fn generate_id() -> String {
  ffi_generate_id()
}

/// Get current timestamp in milliseconds
pub fn current_time_ms() -> Int {
  ffi_current_time_ms()
}

/// Save test run to ETS
pub fn save(run: E2ETestRun) -> Nil {
  ffi_save_test_run(run.id, run)
}

/// Get test run from ETS
pub fn get_status(id: String) -> Option(E2ETestRun) {
  io.println("[E2E-STATUS] get_status called for " <> id)
  // FFI returns {some, TestRun} or none atom
  let result = decode_ffi_option(ffi_get_test_run(id))
  io.println("[E2E-STATUS] Result: " <> case result {
    Some(_) -> "Some(run)"
    None -> "None"
  })
  result
}

/// Decode Erlang option tuple to Gleam Option
fn decode_ffi_option(dyn: Dynamic) -> Option(E2ETestRun) {
  // The FFI returns {some, Value} or none
  // We use unsafe coercion since we know the structure
  case is_none_atom(dyn) {
    True -> None
    False -> {
      // It's {some, Value} tuple
      let run: E2ETestRun = extract_some_value(dyn)
      Some(run)
    }
  }
}

@external(erlang, "vibee_e2e_runner_ffi", "is_none")
fn is_none_atom(dyn: Dynamic) -> Bool

@external(erlang, "vibee_e2e_runner_ffi", "extract_some")
fn extract_some_value(dyn: Dynamic) -> E2ETestRun

/// Start async E2E test run
/// Returns test_run_id immediately, runs tests in background
pub fn start_async(tests: List(E2ETest)) -> String {
  io.println("[E2E-ASYNC] start_async called")
  let id = generate_id()
  io.println("[E2E-ASYNC] Generated ID: " <> id)
  let started_at = current_time_ms()

  let tester_session = config.get_env_or("TELEGRAM_SESSION_ID_TESTER", "REDACTED_SESSION")
  let bot_chat_id = 6579515876  // @vibee_agent

  // Create initial run state
  let run = E2ETestRun(
    id: id,
    status: Running,
    tests: [],
    started_at: started_at,
    completed_at: None,
    tester_session: tester_session,
    bot_chat_id: bot_chat_id,
  )

  // Save initial state
  io.println("[E2E-ASYNC] Saving initial state for " <> id)
  save(run)
  io.println("[E2E-ASYNC] Initial state saved")

  // Spawn background process to run tests
  let tests_copy = tests
  let id_copy = id
  let tester_copy = tester_session
  io.println("[E2E-ASYNC] Spawning background process")
  ffi_spawn_async(fn() {
    run_tests_background(id_copy, tests_copy, tester_copy, bot_chat_id)
  })
  io.println("[E2E-ASYNC] Background process spawned, returning ID")

  id
}

/// Run tests in background and update ETS
fn run_tests_background(
  run_id: String,
  tests: List(E2ETest),
  tester_session: String,
  bot_chat_id: Int,
) -> Nil {
  io.println("[E2E-BG] run_tests_background START for " <> run_id)

  let logger = vibe_logger.new("E2E-ASYNC")
    |> vibe_logger.with_data("run_id", json.string(run_id))

  vibe_logger.info(logger, "Starting async E2E tests")

  let bridge_url = config.get_env_or("VIBEE_BRIDGE_URL", "https://vibee-telegram-bridge.fly.dev")
  let api_key = config.get_env_or("VIBEE_API_KEY", "vibee-secret-2024-prod")
  let bridge = client.with_session_and_key(bridge_url, tester_session, api_key)

  // Run each test sequentially
  let results = list.map(tests, fn(t) {
    run_single_test(bridge, bot_chat_id, t)
  })

  // Calculate pass/fail
  let passed_count = list.count(results, fn(r) { r.passed })
  let failed_count = list.length(results) - passed_count

  // Update state with results
  let final_status = case failed_count {
    0 -> Completed
    _ -> Failed("" <> int.to_string(failed_count) <> " tests failed")
  }

  let updated_run = E2ETestRun(
    id: run_id,
    status: final_status,
    tests: results,
    started_at: case get_status(run_id) {
      Some(r) -> r.started_at
      None -> current_time_ms()
    },
    completed_at: Some(current_time_ms()),
    tester_session: tester_session,
    bot_chat_id: bot_chat_id,
  )

  io.println("[E2E-BG] Saving results to ETS for " <> run_id)
  save(updated_run)
  io.println("[E2E-BG] Results SAVED for " <> run_id)

  vibe_logger.info(logger
    |> vibe_logger.with_data("passed", json.int(passed_count))
    |> vibe_logger.with_data("failed", json.int(failed_count)),
    "Async E2E tests completed")

  io.println("[E2E-BG] run_tests_background END for " <> run_id)
  Nil
}

/// Run a single test
fn run_single_test(
  bridge: client.TelegramBridge,
  chat_id: Int,
  e2e_test: E2ETest,
) -> TestResult {
  let start = current_time_ms()
  let logger = vibe_logger.new("E2E-TEST")
    |> vibe_logger.with_data("command", json.string(e2e_test.command))

  vibe_logger.info(logger, "Sending command")

  // Send command
  case client.send_message(bridge, chat_id, e2e_test.command, None) {
    Error(err) -> {
      let err_str = telegram_error_to_string(err)
      vibe_logger.error(logger |> vibe_logger.with_data("error", json.string(err_str)), "Failed to send")
      TestResult(
        command: e2e_test.command,
        expected: e2e_test.expected_pattern,
        passed: False,
        response: "Failed to send: " <> err_str,
        duration_ms: current_time_ms() - start,
      )
    }
    Ok(_) -> {
      vibe_logger.info(logger, "Command sent, waiting 15s...")

      // Wait for bot response
      process.sleep(15000)

      // Get history
      case client.get_history(bridge, chat_id, 10) {
        Error(err) -> {
          let err_str = telegram_error_to_string(err)
          TestResult(
            command: e2e_test.command,
            expected: e2e_test.expected_pattern,
            passed: False,
            response: "Failed to get history: " <> err_str,
            duration_ms: current_time_ms() - start,
          )
        }
        Ok(messages) -> {
          let #(response_text, passed) = find_matching_response(messages, e2e_test.expected_pattern)

          vibe_logger.info(logger
            |> vibe_logger.with_data("passed", json.bool(passed))
            |> vibe_logger.with_data("response", json.string(string.slice(response_text, 0, 50))),
            case passed { True -> "PASS" False -> "FAIL" })

          TestResult(
            command: e2e_test.command,
            expected: e2e_test.expected_pattern,
            passed: passed,
            response: string.slice(response_text, 0, 200),
            duration_ms: current_time_ms() - start,
          )
        }
      }
    }
  }
}

/// Find matching bot response
fn find_matching_response(messages: List(TelegramMessage), expected_pattern: String) -> #(String, Bool) {
  let vibee_agent_id = 6579515876

  // Get bot responses (not commands)
  let bot_responses = list.filter(messages, fn(m) {
    !string.starts_with(m.text, "/") && m.from_id == vibee_agent_id
  })

  // Find matching response
  let matching = list.find(bot_responses, fn(m) {
    pattern_matches(m.text, expected_pattern)
  })

  case matching {
    Ok(m) -> #(m.text, True)
    Error(_) -> {
      case list.first(bot_responses) {
        Ok(m) -> #(m.text, False)
        Error(_) -> #("No bot response", False)
      }
    }
  }
}

/// Pattern matching with OR (|)
fn pattern_matches(text: String, pattern: String) -> Bool {
  let lower_text = string.lowercase(text)
  let patterns = string.split(pattern, "|")
  list.any(patterns, fn(p) {
    string.contains(lower_text, string.lowercase(p))
  })
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

/// Start async multi-step test run
/// Returns test_run_id immediately, runs tests in background
pub fn start_async_multi(tests: List(MultiStepTest)) -> String {
  io.println("[E2E-MULTI] start_async_multi called with " <> int.to_string(list.length(tests)) <> " tests")
  let id = generate_id()
  io.println("[E2E-MULTI] Generated ID: " <> id)
  let started_at = current_time_ms()

  let tester_session = config.get_env_or("TELEGRAM_SESSION_ID_TESTER", "REDACTED_SESSION")
  let bot_chat_id = 6579515876  // @vibee_agent

  // Create initial run state
  let run = E2ETestRun(
    id: id,
    status: Running,
    tests: [],
    started_at: started_at,
    completed_at: None,
    tester_session: tester_session,
    bot_chat_id: bot_chat_id,
  )

  // Save initial state
  io.println("[E2E-MULTI] Saving initial state")
  save(run)

  // Spawn background process
  let tests_copy = tests
  let id_copy = id
  let tester_copy = tester_session
  io.println("[E2E-MULTI] Spawning background process")
  ffi_spawn_async(fn() {
    run_multi_tests_background(id_copy, tests_copy, tester_copy, bot_chat_id)
  })
  io.println("[E2E-MULTI] Background process spawned")

  id
}

/// Run multi-step tests in background
fn run_multi_tests_background(
  run_id: String,
  tests: List(MultiStepTest),
  tester_session: String,
  bot_chat_id: Int,
) -> Nil {
  io.println("[E2E-MULTI-BG] Starting for " <> run_id)

  let bridge_url = config.get_env_or("VIBEE_BRIDGE_URL", "https://vibee-telegram-bridge.fly.dev")
  let api_key = config.get_env_or("VIBEE_API_KEY", "vibee-secret-2024-prod")
  let bridge = client.with_session_and_key(bridge_url, tester_session, api_key)

  // Run each multi-step test
  let results = list.map(tests, fn(t) {
    run_multi_step_test(bridge, bot_chat_id, t)
  })

  // Calculate pass/fail
  let passed_count = list.count(results, fn(r) { r.passed })
  let failed_count = list.length(results) - passed_count

  let final_status = case failed_count {
    0 -> Completed
    _ -> Failed(int.to_string(failed_count) <> " tests failed")
  }

  let updated_run = E2ETestRun(
    id: run_id,
    status: final_status,
    tests: results,
    started_at: case get_status(run_id) {
      Some(r) -> r.started_at
      None -> current_time_ms()
    },
    completed_at: Some(current_time_ms()),
    tester_session: tester_session,
    bot_chat_id: bot_chat_id,
  )

  io.println("[E2E-MULTI-BG] Saving results")
  save(updated_run)
  io.println("[E2E-MULTI-BG] Complete for " <> run_id)
  Nil
}

/// Execute a multi-step test
fn run_multi_step_test(
  bridge: client.TelegramBridge,
  chat_id: Int,
  multi_test: MultiStepTest,
) -> TestResult {
  let start = current_time_ms()
  io.println("")
  io.println("[E2E-MULTI] ═══════════════════════════════════════")
  io.println("[E2E-MULTI] 🎬 Test: " <> multi_test.name)
  io.println("[E2E-MULTI] Steps: " <> int.to_string(list.length(multi_test.steps)))

  // Track last message ID for button clicks
  let result = execute_steps(bridge, chat_id, multi_test.steps, 0, "")

  case result {
    Error(err) -> {
      io.println("[E2E-MULTI] ❌ FAILED: " <> err)
      TestResult(
        command: multi_test.name,
        expected: multi_test.final_pattern,
        passed: False,
        response: err,
        duration_ms: current_time_ms() - start,
      )
    }
    Ok(#(_last_msg_id, _last_response)) -> {
      // Wait for final response with long timeout
      io.println("[E2E-MULTI] ⏳ Waiting " <> int.to_string(multi_test.timeout_ms / 1000) <> "s for final response...")
      process.sleep(multi_test.timeout_ms)

      // Check for final pattern
      case client.get_history(bridge, chat_id, 20) {
        Error(err) -> {
          let err_str = telegram_error_to_string(err)
          TestResult(
            command: multi_test.name,
            expected: multi_test.final_pattern,
            passed: False,
            response: "Failed to get history: " <> err_str,
            duration_ms: current_time_ms() - start,
          )
        }
        Ok(messages) -> {
          let #(response_text, passed) = find_matching_response(messages, multi_test.final_pattern)
          io.println("[E2E-MULTI] 💬 Final: " <> string.slice(response_text, 0, 100))
          io.println("[E2E-MULTI] " <> case passed { True -> "✅ PASS" False -> "❌ FAIL" })

          TestResult(
            command: multi_test.name,
            expected: multi_test.final_pattern,
            passed: passed,
            response: string.slice(response_text, 0, 300),
            duration_ms: current_time_ms() - start,
          )
        }
      }
    }
  }
}

/// Execute test steps recursively
fn execute_steps(
  bridge: client.TelegramBridge,
  chat_id: Int,
  steps: List(E2ETestStep),
  last_msg_id: Int,
  last_response: String,
) -> Result(#(Int, String), String) {
  case steps {
    [] -> Ok(#(last_msg_id, last_response))
    [step, ..rest] -> {
      case execute_step(bridge, chat_id, step, last_msg_id) {
        Error(err) -> Error(err)
        Ok(#(new_msg_id, response)) -> {
          execute_steps(bridge, chat_id, rest, new_msg_id, response)
        }
      }
    }
  }
}

/// Execute a single step
fn execute_step(
  bridge: client.TelegramBridge,
  chat_id: Int,
  step: E2ETestStep,
  last_msg_id: Int,
) -> Result(#(Int, String), String) {
  case step {
    SendCommand(text) -> {
      io.println("[E2E-STEP] 📤 Send: " <> text)
      case client.send_message(bridge, chat_id, text, None) {
        Error(err) -> Error("Send failed: " <> telegram_error_to_string(err))
        Ok(result) -> {
          io.println("[E2E-STEP] ✅ Sent, msg_id=" <> int.to_string(result.message_id))
          // Brief wait for bot to receive
          process.sleep(2000)
          Ok(#(result.message_id, ""))
        }
      }
    }

    ClickButton(callback_data, wait_ms) -> {
      io.println("[E2E-STEP] 🖱️ Click button: " <> callback_data)
      // Find the message with buttons (should be the most recent bot message)
      case client.get_history(bridge, chat_id, 5) {
        Error(err) -> Error("Get history failed: " <> telegram_error_to_string(err))
        Ok(messages) -> {
          // Find the latest bot message (has buttons)
          let vibee_agent_id = 6579515876
          let bot_msgs = list.filter(messages, fn(m) { m.from_id == vibee_agent_id })
          case list.first(bot_msgs) {
            Error(_) -> Error("No bot message found for button click")
            Ok(bot_msg) -> {
              io.println("[E2E-STEP] Found bot message: id=" <> int.to_string(bot_msg.id))
              case client.click_button(bridge, chat_id, bot_msg.id, callback_data) {
                Error(err) -> Error("Click failed: " <> telegram_error_to_string(err))
                Ok(response) -> {
                  io.println("[E2E-STEP] ✅ Button clicked, response: " <> response)
                  process.sleep(wait_ms)
                  Ok(#(bot_msg.id, response))
                }
              }
            }
          }
        }
      }
    }

    WaitForResponse(pattern, timeout_ms) -> {
      io.println("[E2E-STEP] ⏳ Wait for pattern: " <> pattern <> " (max " <> int.to_string(timeout_ms / 1000) <> "s)")
      wait_for_pattern(bridge, chat_id, pattern, timeout_ms, last_msg_id)
    }

    Wait(ms) -> {
      io.println("[E2E-STEP] ⏳ Wait " <> int.to_string(ms) <> "ms")
      process.sleep(ms)
      Ok(#(last_msg_id, ""))
    }
  }
}

/// Wait for a response matching pattern with polling
fn wait_for_pattern(
  bridge: client.TelegramBridge,
  chat_id: Int,
  pattern: String,
  timeout_ms: Int,
  _last_msg_id: Int,
) -> Result(#(Int, String), String) {
  let start = current_time_ms()
  let poll_interval = 5000  // 5 seconds

  wait_for_pattern_loop(bridge, chat_id, pattern, timeout_ms, start, poll_interval)
}

fn wait_for_pattern_loop(
  bridge: client.TelegramBridge,
  chat_id: Int,
  pattern: String,
  timeout_ms: Int,
  start: Int,
  poll_interval: Int,
) -> Result(#(Int, String), String) {
  let elapsed = current_time_ms() - start
  case elapsed > timeout_ms {
    True -> Error("Timeout waiting for pattern: " <> pattern)
    False -> {
      case client.get_history(bridge, chat_id, 10) {
        Error(_) -> {
          process.sleep(poll_interval)
          wait_for_pattern_loop(bridge, chat_id, pattern, timeout_ms, start, poll_interval)
        }
        Ok(messages) -> {
          let #(response, found) = find_matching_response(messages, pattern)
          case found {
            True -> {
              io.println("[E2E-WAIT] ✅ Found pattern match: " <> string.slice(response, 0, 50))
              let msg_id = case list.first(messages) {
                Ok(m) -> m.id
                Error(_) -> 0
              }
              Ok(#(msg_id, response))
            }
            False -> {
              io.println("[E2E-WAIT] ... polling (" <> int.to_string(elapsed / 1000) <> "s)")
              process.sleep(poll_interval)
              wait_for_pattern_loop(bridge, chat_id, pattern, timeout_ms, start, poll_interval)
            }
          }
        }
      }
    }
  }
}

/// Encode test run to JSON
pub fn encode_run(run: E2ETestRun) -> json.Json {
  json.object([
    #("id", json.string(run.id)),
    #("status", json.string(case run.status {
      Running -> "running"
      Completed -> "completed"
      Failed(_) -> "failed"
    })),
    #("error", case run.status {
      Failed(err) -> json.string(err)
      _ -> json.null()
    }),
    #("tests", json.array(run.tests, encode_test_result)),
    #("total", json.int(list.length(run.tests))),
    #("passed", json.int(list.count(run.tests, fn(t) { t.passed }))),
    #("failed", json.int(list.count(run.tests, fn(t) { !t.passed }))),
    #("started_at", json.int(run.started_at)),
    #("completed_at", case run.completed_at {
      Some(t) -> json.int(t)
      None -> json.null()
    }),
    #("tester_session", json.string(run.tester_session)),
    #("bot_chat_id", json.int(run.bot_chat_id)),
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
