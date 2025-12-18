// =============================================================================
// Auto Tasks - Automatic task extraction after user authorization
// =============================================================================
//
// This module runs automatically after a user successfully authorizes via
// auth_verify_code. It:
// 1. Fetches ALL personal dialogs (with pagination)
// 2. Parses messages from each dialog
// 3. Extracts tasks using Gemini 3 Pro
// 4. Tracks token usage and costs for billing
//
// =============================================================================

import gleam/dynamic/decode
import gleam/erlang/process
import gleam/float
import gleam/io
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/otp/actor
import pog
import vibee/ai/task_extractor
import vibee/db/postgres
import vibee/mcp/config
import vibee/logging

// =============================================================================
// Types
// =============================================================================

pub type OnboardingConfig {
  OnboardingConfig(
    bridge_url: String,
    api_key: String,
    session_id: String,
    user_telegram_id: Int,
    markup_percent: Float,  // e.g., 30.0 for 30%
  )
}

pub type OnboardingStatus {
  OnboardingStatus(
    user_telegram_id: Int,
    status: String,
    dialogs_parsed: Int,
    tasks_extracted: Int,
    total_input_tokens: Int,
    total_output_tokens: Int,
    total_cost_usd: Float,
    error_message: Option(String),
  )
}

pub type Dialog {
  Dialog(
    id: Int,
    dialog_type: String,
    title: String,
    first_name: Option(String),
    last_name: Option(String),
    username: Option(String),
  )
}

// =============================================================================
// Gemini 3 Pro Pricing (via OpenRouter)
// =============================================================================

const input_price_per_million = 1.25   // $1.25 per 1M input tokens
const output_price_per_million = 10.0  // $10 per 1M output tokens

fn calculate_cost(input_tokens: Int, output_tokens: Int) -> Float {
  let input_cost = int.to_float(input_tokens) /. 1_000_000.0 *. input_price_per_million
  let output_cost = int.to_float(output_tokens) /. 1_000_000.0 *. output_price_per_million
  input_cost +. output_cost
}

fn apply_markup(cost: Float, markup_percent: Float) -> Float {
  cost *. { 1.0 +. markup_percent /. 100.0 }
}

// =============================================================================
// Main Entry Point
// =============================================================================

/// Start the onboarding process in a background process
pub fn start_onboarding_async(
  session_id: String,
  user_telegram_id: Int,
) -> Nil {
  let _ = process.spawn_unlinked(fn() {
    let _ = run_onboarding(session_id, user_telegram_id)
  })
  Nil
}

/// Run the full onboarding process (blocking)
pub fn run_onboarding(
  session_id: String,
  user_telegram_id: Int,
) -> Result(OnboardingStatus, String) {
  let bridge_url = config.get_env_or("VIBEE_BRIDGE_URL", "https://vibee-telegram-bridge.fly.dev")
  let api_key = config.get_env_or("VIBEE_API_KEY", "")
  let markup = 30.0  // 30% markup

  let cfg = OnboardingConfig(
    bridge_url: bridge_url,
    api_key: api_key,
    session_id: session_id,
    user_telegram_id: user_telegram_id,
    markup_percent: markup,
  )

  let _ = io.println_error("[ONBOARDING] run_onboarding called for user " <> int.to_string(user_telegram_id))
  logging.quick_info("Starting onboarding for user " <> int.to_string(user_telegram_id))

  // Initialize status in DB
  let _ = io.println_error("[ONBOARDING] Initializing status in DB...")
  case init_onboarding_status(cfg) {
    Ok(_) -> {
      let _ = io.println_error("[ONBOARDING] Status initialized OK")
      Nil
    }
    Error(e) -> {
      let _ = io.println_error("[ONBOARDING] Failed to init status: " <> e)
      logging.quick_error("Failed to init status: " <> e)
      Nil
    }
  }

  // Run the pipeline
  let _ = io.println_error("[ONBOARDING] Running pipeline...")
  case run_onboarding_pipeline(cfg) {
    Ok(status) -> {
      let _ = io.println_error("[ONBOARDING] Pipeline completed: " <> int.to_string(status.tasks_extracted) <> " tasks")
      logging.quick_info("Onboarding completed: " <> int.to_string(status.tasks_extracted) <> " tasks")
      Ok(status)
    }
    Error(e) -> {
      let _ = io.println_error("[ONBOARDING] Pipeline failed: " <> e)
      logging.quick_error("Onboarding failed: " <> e)
      let _ = update_status_error(cfg, e)
      Error(e)
    }
  }
}

// =============================================================================
// Onboarding Pipeline
// =============================================================================

fn run_onboarding_pipeline(cfg: OnboardingConfig) -> Result(OnboardingStatus, String) {
  // Step 1: Get ALL dialogs with pagination
  let _ = io.println_error("[ONBOARDING] Step 1: Fetching all dialogs...")
  use dialogs <- result.try(fetch_all_dialogs(cfg))
  let _ = io.println_error("[ONBOARDING] Fetched " <> int.to_string(list.length(dialogs)) <> " dialogs")
  logging.quick_info("Fetched " <> int.to_string(list.length(dialogs)) <> " dialogs")

  // Step 2: Filter personal dialogs only
  let personal_dialogs = list.filter(dialogs, fn(d) {
    d.dialog_type == "user"
  })
  let _ = io.println_error("[ONBOARDING] Found " <> int.to_string(list.length(personal_dialogs)) <> " personal dialogs")
  logging.quick_info("Found " <> int.to_string(list.length(personal_dialogs)) <> " personal dialogs")

  // Step 3: Get DB pool
  use pool <- result.try(get_db_pool())

  // Step 4: Process each dialog
  let initial_status = OnboardingStatus(
    user_telegram_id: cfg.user_telegram_id,
    status: "in_progress",
    dialogs_parsed: 0,
    tasks_extracted: 0,
    total_input_tokens: 0,
    total_output_tokens: 0,
    total_cost_usd: 0.0,
    error_message: None,
  )

  let final_status = list.fold(personal_dialogs, initial_status, fn(status, dialog) {
    case process_dialog(cfg, pool, dialog, status) {
      Ok(new_status) -> new_status
      Error(e) -> {
        logging.quick_error("Failed to process dialog " <> int.to_string(dialog.id) <> ": " <> e)
        status
      }
    }
  })

  // Step 5: Update final status
  let completed_status = OnboardingStatus(..final_status, status: "completed")
  let _ = update_status_completed(cfg, completed_status)

  postgres.disconnect(pool)
  Ok(completed_status)
}

// =============================================================================
// Fetch ALL Dialogs with Pagination
// =============================================================================

fn fetch_all_dialogs(cfg: OnboardingConfig) -> Result(List(Dialog), String) {
  fetch_dialogs_page(cfg, 0, [])
}

fn fetch_dialogs_page(
  cfg: OnboardingConfig,
  offset: Int,
  accumulated: List(Dialog),
) -> Result(List(Dialog), String) {
  let page_size = 500  // Fetch 500 at a time

  case fetch_dialogs_batch(cfg, page_size, offset) {
    Ok(dialogs) -> {
      let all = list.append(accumulated, dialogs)
      case list.length(dialogs) < page_size {
        True -> Ok(all)  // Last page
        False -> fetch_dialogs_page(cfg, offset + page_size, all)  // More pages
      }
    }
    Error(e) -> Error(e)
  }
}

fn fetch_dialogs_batch(
  cfg: OnboardingConfig,
  limit: Int,
  offset: Int,
) -> Result(List(Dialog), String) {
  let url = cfg.bridge_url <> "/api/v1/dialogs?limit=" <> int.to_string(limit) <> "&offset=" <> int.to_string(offset)
  let _ = io.println_error("[ONBOARDING] Fetching dialogs from: " <> url)
  let _ = io.println_error("[ONBOARDING] API Key: " <> string.slice(cfg.api_key, 0, 10) <> "...")
  let _ = io.println_error("[ONBOARDING] Session: " <> cfg.session_id)

  let req = request.new()
    |> request.set_method(http.Get)
    |> request.set_host(get_host(cfg.bridge_url))
    |> request.set_path("/api/v1/dialogs?limit=" <> int.to_string(limit) <> "&offset=" <> int.to_string(offset))
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("authorization", "Bearer " <> cfg.api_key)
    |> request.set_header("X-Session-ID", cfg.session_id)
    |> request.set_scheme(http.Https)

  let _ = io.println_error("[ONBOARDING] Sending HTTP request...")
  case httpc.send(req) {
    Ok(resp) -> {
      let _ = io.println_error("[ONBOARDING] Got response status: " <> int.to_string(resp.status))
      case resp.status {
        200 -> {
          let _ = io.println_error("[ONBOARDING] Parsing dialogs response...")
          parse_dialogs_response(resp.body)
        }
        status -> Error("Bridge returned status " <> int.to_string(status))
      }
    }
    Error(e) -> {
      let _ = io.println_error("[ONBOARDING] HTTP error: " <> string.inspect(e))
      Error("Failed to connect to bridge")
    }
  }
}

fn get_host(url: String) -> String {
  url
  |> string.replace("https://", "")
  |> string.replace("http://", "")
  |> string.split("/")
  |> list.first()
  |> result.unwrap("")
}

fn parse_dialogs_response(body: String) -> Result(List(Dialog), String) {
  let decoder = {
    use dialogs <- decode.field("dialogs", decode.list(dialog_decoder()))
    decode.success(dialogs)
  }

  case json.parse(body, decoder) {
    Ok(dialogs) -> Ok(dialogs)
    Error(_) -> Error("Failed to parse dialogs response")
  }
}

fn dialog_decoder() -> decode.Decoder(Dialog) {
  use id <- decode.field("id", decode.int)
  use dialog_type <- decode.field("type", decode.string)
  use title <- decode.optional_field("title", "", decode.string)
  use first_name <- decode.optional_field("first_name", None, decode.optional(decode.string))
  use last_name <- decode.optional_field("last_name", None, decode.optional(decode.string))
  use username <- decode.optional_field("username", None, decode.optional(decode.string))
  decode.success(Dialog(id, dialog_type, title, first_name, last_name, username))
}

// =============================================================================
// Process Single Dialog
// =============================================================================

fn process_dialog(
  cfg: OnboardingConfig,
  pool: pog.Connection,
  dialog: Dialog,
  status: OnboardingStatus,
) -> Result(OnboardingStatus, String) {
  let contact_name = case dialog.first_name, dialog.last_name {
    Some(first), Some(last) -> first <> " " <> last
    Some(first), None -> first
    None, Some(last) -> last
    None, None -> dialog.title
  }

  // Check if dialog was already processed (deduplication)
  case is_dialog_already_processed(pool, dialog.id) {
    True -> {
      logging.quick_info("Skipping already processed dialog: " <> contact_name <> " (id: " <> int.to_string(dialog.id) <> ")")
      Ok(status)  // Skip, return unchanged status
    }
    False -> process_new_dialog(cfg, pool, dialog, contact_name, status)
  }
}

/// Check if we already extracted tasks from this dialog
fn is_dialog_already_processed(pool: pog.Connection, dialog_id: Int) -> Bool {
  let sql = "SELECT COUNT(*) FROM extracted_task_sources WHERE dialog_id = $1"
  case pog.query(sql)
    |> pog.parameter(pog.int(dialog_id))
    |> pog.returning({
      use count <- decode.field(0, decode.int)
      decode.success(count)
    })
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [count, ..])) -> count > 0
    _ -> False
  }
}

/// Process a dialog that hasn't been processed yet
fn process_new_dialog(
  cfg: OnboardingConfig,
  pool: pog.Connection,
  dialog: Dialog,
  contact_name: String,
  status: OnboardingStatus,
) -> Result(OnboardingStatus, String) {
  logging.quick_info("Processing dialog: " <> contact_name <> " (id: " <> int.to_string(dialog.id) <> ")")

  // Parse chat messages (uses existing rag_tools parsing)
  case parse_dialog_messages(cfg, dialog.id) {
    Ok(_) -> {
      // Extract tasks
      let extractor_config = task_extractor.default_config()

      case task_extractor.extract_and_create_tasks(
        pool,
        dialog.id,
        cfg.user_telegram_id,
        500,  // Limit messages per dialog
        extractor_config,
      ) {
        Ok(result) -> {
          // Log AI usage
          let _ = log_ai_usage(
            pool,
            cfg.user_telegram_id,
            "task_extraction",
            result.input_tokens,
            result.output_tokens,
            cfg.markup_percent,
          )

          let cost = calculate_cost(result.input_tokens, result.output_tokens)

          Ok(OnboardingStatus(
            ..status,
            dialogs_parsed: status.dialogs_parsed + 1,
            tasks_extracted: status.tasks_extracted + result.tasks_created,
            total_input_tokens: status.total_input_tokens + result.input_tokens,
            total_output_tokens: status.total_output_tokens + result.output_tokens,
            total_cost_usd: status.total_cost_usd +. cost,
          ))
        }
        Error(e) -> Error("Task extraction failed: " <> string.inspect(e))
      }
    }
    Error(e) -> Error("Failed to parse messages: " <> e)
  }
}

fn parse_dialog_messages(cfg: OnboardingConfig, dialog_id: Int) -> Result(Int, String) {
  // Call telegram_parse_chat via bridge
  let url = cfg.bridge_url <> "/api/v1/messages/parse"

  let body = json.object([
    #("dialog_id", json.int(dialog_id)),
    #("limit", json.int(5000)),  // Get up to 5000 messages per dialog
  ])

  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_host(get_host(cfg.bridge_url))
    |> request.set_path("/api/v1/messages/parse")
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("authorization", "Bearer " <> cfg.api_key)
    |> request.set_header("X-Session-ID", cfg.session_id)
    |> request.set_scheme(http.Https)
    |> request.set_body(json.to_string(body))

  case httpc.send(req) {
    Ok(resp) -> {
      case resp.status {
        200 -> Ok(0)  // Success, count doesn't matter
        status -> Error("Parse returned status " <> int.to_string(status))
      }
    }
    Error(_) -> Error("Failed to connect to bridge")
  }
}

// =============================================================================
// Database Operations
// =============================================================================

fn get_db_pool() -> Result(pog.Connection, String) {
  let db_url = config.get_env_or("DATABASE_URL", "")
  case db_url {
    "" -> Error("DATABASE_URL not set")
    url -> {
      let pool_name: process.Name(pog.Message) = process.new_name(prefix: "onboarding_db")
      case pog.url_config(pool_name, url) {
        Ok(pool_config) -> {
          let cfg = pool_config
            |> pog.pool_size(3)
            |> pog.queue_target(50)
          case pog.start(cfg) {
            Ok(actor.Started(_, connection)) -> Ok(connection)
            Error(_) -> Error("Failed to start database pool")
          }
        }
        Error(_) -> Error("Invalid DATABASE_URL format")
      }
    }
  }
}

fn init_onboarding_status(cfg: OnboardingConfig) -> Result(Nil, String) {
  use pool <- result.try(get_db_pool())

  let sql = "
    INSERT INTO onboarding_status (user_telegram_id, session_id, status)
    VALUES ($1, $2, 'in_progress')
    ON CONFLICT (user_telegram_id) DO UPDATE SET
      session_id = $2,
      status = 'in_progress',
      started_at = NOW(),
      dialogs_parsed = 0,
      tasks_extracted = 0,
      total_input_tokens = 0,
      total_output_tokens = 0,
      total_cost_usd = 0,
      error_message = NULL
  "

  case pog.query(sql)
    |> pog.parameter(pog.int(cfg.user_telegram_id))
    |> pog.parameter(pog.text(cfg.session_id))
    |> pog.execute(pool)
  {
    Ok(_) -> {
      postgres.disconnect(pool)
      Ok(Nil)
    }
    Error(e) -> {
      let _ = io.println_error("[ONBOARDING] DB error: " <> string.inspect(e))
      postgres.disconnect(pool)
      Error("Failed to init onboarding status")
    }
  }
}

fn update_status_completed(cfg: OnboardingConfig, status: OnboardingStatus) -> Result(Nil, String) {
  use pool <- result.try(get_db_pool())

  let sql = "
    UPDATE onboarding_status SET
      status = 'completed',
      completed_at = NOW(),
      dialogs_parsed = $2,
      tasks_extracted = $3,
      total_input_tokens = $4,
      total_output_tokens = $5,
      total_cost_usd = $6
    WHERE user_telegram_id = $1
  "

  case pog.query(sql)
    |> pog.parameter(pog.int(cfg.user_telegram_id))
    |> pog.parameter(pog.int(status.dialogs_parsed))
    |> pog.parameter(pog.int(status.tasks_extracted))
    |> pog.parameter(pog.int(status.total_input_tokens))
    |> pog.parameter(pog.int(status.total_output_tokens))
    |> pog.parameter(pog.float(status.total_cost_usd))
    |> pog.execute(pool)
  {
    Ok(_) -> {
      postgres.disconnect(pool)
      Ok(Nil)
    }
    Error(_) -> {
      postgres.disconnect(pool)
      Error("Failed to update status")
    }
  }
}

fn update_status_error(cfg: OnboardingConfig, error: String) -> Result(Nil, String) {
  use pool <- result.try(get_db_pool())

  let sql = "
    UPDATE onboarding_status SET
      status = 'failed',
      error_message = $2
    WHERE user_telegram_id = $1
  "

  case pog.query(sql)
    |> pog.parameter(pog.int(cfg.user_telegram_id))
    |> pog.parameter(pog.text(error))
    |> pog.execute(pool)
  {
    Ok(_) -> {
      postgres.disconnect(pool)
      Ok(Nil)
    }
    Error(_) -> {
      postgres.disconnect(pool)
      Error("Failed to update error status")
    }
  }
}

fn log_ai_usage(
  pool: pog.Connection,
  user_telegram_id: Int,
  operation: String,
  input_tokens: Int,
  output_tokens: Int,
  markup_percent: Float,
) -> Result(Nil, String) {
  let cost = calculate_cost(input_tokens, output_tokens)
  let final_cost = apply_markup(cost, markup_percent)

  let sql = "
    INSERT INTO ai_usage_log
    (user_telegram_id, operation, model, input_tokens, output_tokens, cost_usd, markup_percent, final_cost_usd)
    VALUES ($1, $2, 'google/gemini-3-pro-preview', $3, $4, $5, $6, $7)
  "

  case pog.query(sql)
    |> pog.parameter(pog.int(user_telegram_id))
    |> pog.parameter(pog.text(operation))
    |> pog.parameter(pog.int(input_tokens))
    |> pog.parameter(pog.int(output_tokens))
    |> pog.parameter(pog.float(cost))
    |> pog.parameter(pog.float(markup_percent))
    |> pog.parameter(pog.float(final_cost))
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Failed to log AI usage")
  }
}
