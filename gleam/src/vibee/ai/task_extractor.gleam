// =============================================================================
// Task Extractor - AI-powered task extraction from Telegram messages
// =============================================================================
//
// Uses Gemini 3 Pro Preview via OpenRouter API to analyze conversations
// and extract tasks, promises, commitments, TODOs, and deadlines.
//
// =============================================================================
// Gemini 3 Pro - Model Capabilities
// =============================================================================
//
// Gemini 3 Pro is Google's flagship frontier model for high-precision
// multimodal information processing, combining high performance in text,
// images, video, audio, and code with a 1 million token context window.
//
// Key capabilities relevant to task extraction:
// - Long-context processing (1M tokens) - can analyze entire conversation history
// - Intent detection with minimal prompting - understands implicit tasks
// - Structured output generation - reliable JSON extraction
// - Multilingual support (RU/EN) - handles mixed language conversations
// - Reasoning tokens support - for complex multi-step task analysis
//
// Model benchmarks: Leading scores on LMArena, GPQA Diamond, MathArena Apex,
// MMMU-Pro and Video-MMMU.
//
// For reasoning token preservation in multi-turn tool calls, see:
// https://openrouter.ai/docs/use-cases/reasoning-tokens#preserving-reasoning-blocks
//
// Use cases for task extraction:
// - Promises: "I'll send...", "я отправлю...", "сделаю"
// - TODOs: "Need to...", "нужно", "не забыть"
// - Meetings: "Let's meet...", "давай созвонимся"
// - Deadlines: "by Friday", "до пятницы", "завтра"
//
// =============================================================================

import gleam/dynamic/decode
import gleam/float
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import pog
import vibee/db/postgres.{type DbPool, type TelegramMessage}
import vibee/db/tasks
import vibee/mcp/config
import vibee/logging

// =============================================================================
// Types
// =============================================================================

/// Extracted task from conversation
pub type ExtractedTask {
  ExtractedTask(
    title: String,
    description: String,
    responsibility: String,
    category: String,
    priority: Int,
    due_date: Option(String),
    source_message_id: Int,
    confidence: Float,
  )
}

/// Result of extraction process
pub type ExtractionResult {
  ExtractionResult(
    dialog_id: Int,
    contact_name: String,
    total_messages: Int,
    tasks_found: Int,
    tasks_created: Int,
    extracted_tasks: List(ExtractedTask),
    errors: List(String),
    // Token usage for billing
    input_tokens: Int,
    output_tokens: Int,
  )
}

/// Extractor configuration
pub type ExtractorConfig {
  ExtractorConfig(
    api_key: String,
    model: String,
    min_confidence: Float,
    auto_create: Bool,
  )
}

/// Extractor errors
pub type ExtractorError {
  ExtractorApiError(String)
  ExtractorDbError(String)
  ExtractorParseError(String)
  ExtractorNoMessages
}

// =============================================================================
// Configuration
// =============================================================================

/// Default configuration using OpenRouter
pub fn default_config() -> ExtractorConfig {
  ExtractorConfig(
    api_key: config.get_env("OPENROUTER_API_KEY"),
    model: "google/gemini-3-pro-preview",
    min_confidence: 0.7,
    auto_create: True,
  )
}

/// Check if configured
pub fn is_configured(cfg: ExtractorConfig) -> Bool {
  cfg.api_key != ""
}

// =============================================================================
// Main Extraction Functions
// =============================================================================

/// Extract tasks from a dialog and optionally create them
pub fn extract_and_create_tasks(
  pool: pog.Connection,
  dialog_id: Int,
  owner_id: Int,
  limit: Int,
  cfg: ExtractorConfig,
) -> Result(ExtractionResult, ExtractorError) {
  case is_configured(cfg) {
    False -> Error(ExtractorApiError("OPENROUTER_API_KEY not set"))
    True -> {
      // 1. Get dialog info
      let contact_name = get_dialog_name(pool, dialog_id)

      // 2. Get messages from dialog
      case get_messages_for_extraction(pool, dialog_id, limit) {
        Error(e) -> Error(ExtractorDbError(e))
        Ok([]) -> Error(ExtractorNoMessages)
        Ok(messages) -> {
          // 3. Call LLM to extract tasks (returns tasks + token usage)
          case call_gemini_extract(messages, contact_name, cfg) {
            Error(e) -> Error(e)
            Ok(#(extracted, usage)) -> {
              // 4. Filter by confidence
              let high_confidence = list.filter(extracted, fn(t) {
                t.confidence >=. cfg.min_confidence
              })

              // 5. Create tasks if auto_create is enabled
              let #(created_count, errors) = case cfg.auto_create {
                False -> #(0, [])
                True -> create_tasks_in_db(pool, high_confidence, owner_id, dialog_id)
              }

              Ok(ExtractionResult(
                dialog_id: dialog_id,
                contact_name: contact_name,
                total_messages: list.length(messages),
                tasks_found: list.length(extracted),
                tasks_created: created_count,
                extracted_tasks: extracted,
                errors: errors,
                input_tokens: usage.input_tokens,
                output_tokens: usage.output_tokens,
              ))
            }
          }
        }
      }
    }
  }
}

/// Extract tasks from messages without creating (dry run)
pub fn extract_tasks_only(
  messages: List(TelegramMessage),
  contact_name: String,
  cfg: ExtractorConfig,
) -> Result(List(ExtractedTask), ExtractorError) {
  case is_configured(cfg) {
    False -> Error(ExtractorApiError("OPENROUTER_API_KEY not set"))
    True -> {
      case call_gemini_extract(messages, contact_name, cfg) {
        Ok(#(tasks, _usage)) -> Ok(tasks)
        Error(e) -> Error(e)
      }
    }
  }
}

// =============================================================================
// Database Functions
// =============================================================================

/// Get dialog name from telegram_dialogs
fn get_dialog_name(pool: pog.Connection, dialog_id: Int) -> String {
  let sql = "SELECT COALESCE(title, first_name || ' ' || COALESCE(last_name, ''), username, 'Unknown')
             FROM telegram_dialogs WHERE id = $1"

  let name_decoder = decode.at([0], decode.string)

  case pog.query(sql)
    |> pog.parameter(pog.int(dialog_id))
    |> pog.returning(name_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [name])) -> name
    _ -> "Contact #" <> int.to_string(dialog_id)
  }
}

/// Get messages for extraction from telegram_messages
fn get_messages_for_extraction(
  pool: pog.Connection,
  dialog_id: Int,
  limit: Int,
) -> Result(List(TelegramMessage), String) {
  let sql = "
    SELECT id, message_id, dialog_id, sender_id, sender_name,
           content_type, text_content, media_id, reply_to_id,
           forward_from_id, forward_from_name, timestamp::text,
           CASE WHEN embedding IS NOT NULL THEN true ELSE false END as has_embedding
    FROM telegram_messages
    WHERE dialog_id = $1
      AND text_content IS NOT NULL
      AND text_content != ''
    ORDER BY timestamp DESC
    LIMIT $2
  "

  let message_decoder = {
    use id <- decode.field(0, decode.int)
    use message_id <- decode.field(1, decode.int)
    use dialog_id <- decode.field(2, decode.int)
    use sender_id <- decode.field(3, decode.optional(decode.int))
    use sender_name <- decode.field(4, decode.optional(decode.string))
    use content_type_str <- decode.field(5, decode.string)
    use text_content <- decode.field(6, decode.optional(decode.string))
    use media_id <- decode.field(7, decode.optional(decode.int))
    use reply_to_id <- decode.field(8, decode.optional(decode.int))
    use forward_from_id <- decode.field(9, decode.optional(decode.int))
    use forward_from_name <- decode.field(10, decode.optional(decode.string))
    use timestamp <- decode.field(11, decode.string)
    use has_embedding <- decode.field(12, decode.bool)

    let content_type = case content_type_str {
      "photo" -> postgres.ContentPhoto
      "voice" -> postgres.ContentVoice
      "video" -> postgres.ContentVideo
      "document" -> postgres.ContentDocument
      _ -> postgres.ContentText
    }

    decode.success(postgres.TelegramMessage(
      id: id,
      message_id: message_id,
      dialog_id: dialog_id,
      sender_id: sender_id,
      sender_name: sender_name,
      content_type: content_type,
      text_content: text_content,
      media_id: media_id,
      reply_to_id: reply_to_id,
      forward_from_id: forward_from_id,
      forward_from_name: forward_from_name,
      timestamp: timestamp,
      has_embedding: has_embedding,
    ))
  }

  case pog.query(sql)
    |> pog.parameter(pog.int(dialog_id))
    |> pog.parameter(pog.int(limit))
    |> pog.returning(message_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, messages)) -> Ok(messages)
    Error(e) -> Error("Database error: " <> string.inspect(e))
  }
}

/// Create tasks in database (with deduplication)
fn create_tasks_in_db(
  pool: pog.Connection,
  extracted_tasks: List(ExtractedTask),
  owner_id: Int,
  dialog_id: Int,
) -> #(Int, List(String)) {
  let results = list.map(extracted_tasks, fn(task) {
    // Check for duplicate task (same title + dialog)
    case is_duplicate_task(pool, task.title, dialog_id) {
      True -> {
        logging.quick_info("Skipping duplicate task: " <> task.title)
        Error("Duplicate task skipped: " <> task.title)
      }
      False -> {
        let new_task = tasks.NewTask(
          owner_telegram_id: owner_id,
          contact_dialog_id: Some(dialog_id),
          title: task.title,
          description: Some(task.description),
          category: tasks.string_to_category(task.category),
          priority: task.priority,
          responsibility: task.responsibility,
          due_date: task.due_date,
        )

        case tasks.create_task(pool, new_task) {
          Ok(task_id) -> {
            // Link task to source message
            let _ = link_task_to_source(pool, task_id, task.source_message_id, dialog_id, task.confidence)
            Ok(task_id)
          }
          Error(e) -> Error("Failed to create task: " <> string.inspect(e))
        }
      }
    }
  })

  let created = list.filter_map(results, fn(r) {
    case r {
      Ok(id) -> Ok(id)
      Error(_) -> Error(Nil)
    }
  })

  let errors = list.filter_map(results, fn(r) {
    case r {
      Ok(_) -> Error(Nil)
      Error(e) -> Ok(e)
    }
  })

  #(list.length(created), errors)
}

/// Check if a task with the same title already exists for this dialog
fn is_duplicate_task(pool: pog.Connection, title: String, dialog_id: Int) -> Bool {
  let sql = "SELECT COUNT(*) FROM user_tasks WHERE title = $1 AND contact_dialog_id = $2"
  case pog.query(sql)
    |> pog.parameter(pog.text(title))
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

/// Link task to source message
fn link_task_to_source(
  pool: pog.Connection,
  task_id: Int,
  message_id: Int,
  dialog_id: Int,
  confidence: Float,
) -> Result(Nil, String) {
  let sql = "
    INSERT INTO extracted_task_sources (task_id, message_id, dialog_id, extraction_confidence)
    VALUES ($1, $2, $3, $4)
  "

  case pog.query(sql)
    |> pog.parameter(pog.int(task_id))
    |> pog.parameter(pog.int(message_id))
    |> pog.parameter(pog.int(dialog_id))
    |> pog.parameter(pog.float(confidence))
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error("Failed to link: " <> string.inspect(e))
  }
}

// =============================================================================
// LLM Functions
// =============================================================================

/// Token usage from API response
pub type TokenUsage {
  TokenUsage(input_tokens: Int, output_tokens: Int)
}

/// Call OpenRouter to extract tasks from messages
/// Returns tasks AND token usage for billing
fn call_gemini_extract(
  messages: List(TelegramMessage),
  contact_name: String,
  cfg: ExtractorConfig,
) -> Result(#(List(ExtractedTask), TokenUsage), ExtractorError) {
  let conversation = format_conversation(messages)
  let prompt = build_extraction_prompt(conversation, contact_name)

  let url = "https://openrouter.ai/api/v1/chat/completions"

  let body = json.object([
    #("model", json.string(cfg.model)),
    #("messages", json.array([
      json.object([
        #("role", json.string("user")),
        #("content", json.string(prompt)),
      ]),
    ], fn(x) { x })),
    #("temperature", json.float(0.2)),
    #("max_tokens", json.int(4096)),
    #("response_format", json.object([
      #("type", json.string("json_object")),
    ])),
  ])

  case request.to(url) {
    Error(_) -> Error(ExtractorApiError("Invalid URL"))
    Ok(req) -> {
      let req = req
        |> request.set_method(http.Post)
        |> request.set_header("content-type", "application/json")
        |> request.set_header("authorization", "Bearer " <> cfg.api_key)
        |> request.set_header("http-referer", "https://vibee-mcp.fly.dev")
        |> request.set_header("x-title", "VIBEE Task Extractor")
        |> request.set_body(json.to_string(body))

      case httpc.send(req) {
        Error(e) -> Error(ExtractorApiError("HTTP error: " <> string.inspect(e)))
        Ok(resp) -> {
          case resp.status {
            200 -> parse_openrouter_response_with_usage(resp.body, messages)
            429 -> Error(ExtractorApiError("Rate limited"))
            status -> Error(ExtractorApiError("API error: " <> int.to_string(status) <> " - " <> resp.body))
          }
        }
      }
    }
  }
}

/// Format messages into conversation text
fn format_conversation(messages: List(TelegramMessage)) -> String {
  // Sort by timestamp ascending (oldest first)
  let sorted = list.sort(messages, fn(a, b) {
    string.compare(a.timestamp, b.timestamp)
  })

  list.map(sorted, fn(msg) {
    let sender = option.unwrap(msg.sender_name, "Unknown")
    let text = option.unwrap(msg.text_content, "")
    let time = string.slice(msg.timestamp, 0, 16)
    "[" <> time <> "] " <> sender <> ": " <> text
  })
  |> string.join("\n")
}

/// Build extraction prompt
fn build_extraction_prompt(conversation: String, contact_name: String) -> String {
  "You are a task extraction assistant. Analyze this conversation between the user and " <> contact_name <> ".

Extract any tasks, promises, or commitments mentioned. Look for:
- Explicit promises: \"I'll send...\", \"я отправлю...\", \"сделаю\", \"обещаю\"
- Action items: \"Need to...\", \"нужно\", \"не забыть\", \"TODO\"
- Meeting requests: \"Let's meet...\", \"давай созвонимся\", \"встретимся\"
- Deadlines: \"by Friday\", \"до пятницы\", \"завтра\", \"на следующей неделе\"

For each task, determine:
1. title: Brief task description (max 100 chars)
2. description: Full context from the conversation
3. responsibility: \"owner\" if the user should do it, \"contact\" if " <> contact_name <> " should do it, \"both\" if shared
4. category: \"promise\" | \"meeting\" | \"project\" | \"conversation\" | \"other\"
5. priority: 1 (high/urgent), 2 (medium/normal), 3 (low/someday)
6. due_date: ISO date (YYYY-MM-DD) if mentioned, null otherwise
7. source_message_id: 0 (will be filled later)
8. confidence: 0.0-1.0 how confident this is a real actionable task

Return a JSON object with format:
{
  \"tasks\": [
    {
      \"title\": \"...\",
      \"description\": \"...\",
      \"responsibility\": \"owner\",
      \"category\": \"promise\",
      \"priority\": 2,
      \"due_date\": null,
      \"source_message_id\": 0,
      \"confidence\": 0.85
    }
  ]
}

Only include tasks with confidence >= 0.5. Return {\"tasks\": []} if no tasks found.

CONVERSATION:
" <> conversation
}

/// Parse OpenRouter response with usage tracking
fn parse_openrouter_response_with_usage(
  response_body: String,
  messages: List(TelegramMessage),
) -> Result(#(List(ExtractedTask), TokenUsage), ExtractorError) {
  // Parse OpenRouter envelope with usage:
  // { "choices": [...], "usage": { "prompt_tokens": N, "completion_tokens": N } }
  let envelope_decoder = {
    use choices <- decode.field("choices", decode.list({
      use message <- decode.field("message", {
        use content <- decode.field("content", decode.string)
        decode.success(content)
      })
      decode.success(message)
    }))
    use input_tokens <- decode.optional_field("usage", 0, {
      use prompt_tokens <- decode.field("prompt_tokens", decode.int)
      decode.success(prompt_tokens)
    })
    use output_tokens <- decode.optional_field("usage", 0, {
      use completion_tokens <- decode.field("completion_tokens", decode.int)
      decode.success(completion_tokens)
    })
    decode.success(#(choices, TokenUsage(input_tokens, output_tokens)))
  }

  case json.parse(response_body, envelope_decoder) {
    Error(_) -> {
      logging.quick_warn("Failed to parse OpenRouter response: " <> response_body)
      Error(ExtractorParseError("Failed to parse OpenRouter response envelope"))
    }
    Ok(#([], usage)) -> Ok(#([], usage))
    Ok(#([content, ..], usage)) -> {
      // Now parse the JSON tasks from the content
      case parse_tasks_json(content, messages) {
        Ok(tasks) -> Ok(#(tasks, usage))
        Error(e) -> Error(e)
      }
    }
  }
}

/// Parse tasks JSON from Gemini text output
fn parse_tasks_json(
  text: String,
  messages: List(TelegramMessage),
) -> Result(List(ExtractedTask), ExtractorError) {
  let tasks_decoder = {
    use tasks <- decode.field("tasks", decode.list({
      use title <- decode.field("title", decode.string)
      use description <- decode.field("description", decode.string)
      use responsibility <- decode.field("responsibility", decode.string)
      use category <- decode.field("category", decode.string)
      use priority <- decode.field("priority", decode.int)
      use due_date <- decode.optional_field("due_date", None, decode.optional(decode.string))
      use confidence <- decode.field("confidence", decode.float)

      // Get the most recent message ID as source
      let source_id = case list.first(messages) {
        Ok(msg) -> msg.message_id
        Error(_) -> 0
      }

      decode.success(ExtractedTask(
        title: title,
        description: description,
        responsibility: responsibility,
        category: category,
        priority: priority,
        due_date: due_date,
        source_message_id: source_id,
        confidence: confidence,
      ))
    }))
    decode.success(tasks)
  }

  case json.parse(text, tasks_decoder) {
    Ok(tasks) -> Ok(tasks)
    Error(_) -> {
      logging.quick_warn("Failed to parse tasks JSON: " <> text)
      Error(ExtractorParseError("Failed to parse tasks JSON"))
    }
  }
}

// =============================================================================
// Utility Functions
// =============================================================================

/// Get all personal dialogs for scanning
pub fn get_personal_dialogs(pool: pog.Connection) -> Result(List(Int), String) {
  let sql = "SELECT id FROM telegram_dialogs WHERE type = 'user' ORDER BY id"

  let id_decoder = decode.at([0], decode.int)

  case pog.query(sql)
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, ids)) -> Ok(ids)
    Error(e) -> Error("Database error: " <> string.inspect(e))
  }
}

/// Scan all personal dialogs for tasks
pub fn scan_all_dialogs_for_tasks(
  pool: pog.Connection,
  owner_id: Int,
  limit_per_dialog: Int,
  cfg: ExtractorConfig,
) -> Result(List(ExtractionResult), ExtractorError) {
  case get_personal_dialogs(pool) {
    Error(e) -> Error(ExtractorDbError(e))
    Ok(dialog_ids) -> {
      let results = list.filter_map(dialog_ids, fn(dialog_id) {
        case extract_and_create_tasks(pool, dialog_id, owner_id, limit_per_dialog, cfg) {
          Ok(result) -> Ok(result)
          Error(_) -> Error(Nil)
        }
      })
      Ok(results)
    }
  }
}
