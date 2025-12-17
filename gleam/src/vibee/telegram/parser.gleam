// Telegram Dialog Parser
// Batch parsing of all dialogs with rate limiting and progress tracking

import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/db/postgres.{
  type DbPool, type ParseStatus, type TelegramDialog, type TelegramMessage,
  ContentPhoto, ContentText, ContentVoice, DialogBot, DialogChannel, DialogGroup,
  DialogUser, ParseCompleted, ParseFailed, ParseInProgress, ParsePending,
  TelegramDialog, TelegramMessage,
}
import vibee/mcp/config

// =============================================================================
// Types
// =============================================================================

/// Parser configuration
pub type ParserConfig {
  ParserConfig(
    batch_size: Int,
    delay_ms: Int,
    max_messages_per_dialog: Int,
    include_media: Bool,
  )
}

/// Parse result for a single dialog
pub type DialogParseResult {
  DialogParseResult(
    dialog_id: Int,
    messages_parsed: Int,
    media_found: Int,
    errors: List(String),
  )
}

/// Full parse result
pub type FullParseResult {
  FullParseResult(
    total_dialogs: Int,
    dialogs_parsed: Int,
    total_messages: Int,
    total_media: Int,
    errors: List(String),
    duration_seconds: Int,
  )
}

/// Parser error
pub type ParserError {
  ParserConnectionError(String)
  ParserApiError(String)
  ParserDbError(String)
  ParserRateLimited
  ParserCancelled
}

// =============================================================================
// Configuration
// =============================================================================

/// Default parser configuration
pub fn default_config() -> ParserConfig {
  ParserConfig(
    batch_size: 100,
    delay_ms: 250,
    max_messages_per_dialog: 0,
    // 0 = no limit
    include_media: True,
  )
}

/// Create config from JSON
pub fn config_from_json(json_str: String) -> ParserConfig {
  case json.parse(json_str, decode_config()) {
    Ok(cfg) -> cfg
    Error(_) -> default_config()
  }
}

/// Convert config to JSON
pub fn config_to_json(cfg: ParserConfig) -> String {
  json.object([
    #("batch_size", json.int(cfg.batch_size)),
    #("delay_ms", json.int(cfg.delay_ms)),
    #("max_messages_per_dialog", json.int(cfg.max_messages_per_dialog)),
    #("include_media", json.bool(cfg.include_media)),
  ])
  |> json.to_string
}

fn decode_config() -> Decoder(ParserConfig) {
  use batch_size <- decode.field("batch_size", decode.int)
  use delay_ms <- decode.field("delay_ms", decode.int)
  use max_messages_per_dialog <- decode.field(
    "max_messages_per_dialog",
    decode.int,
  )
  use include_media <- decode.field("include_media", decode.bool)
  decode.success(ParserConfig(
    batch_size: batch_size,
    delay_ms: delay_ms,
    max_messages_per_dialog: max_messages_per_dialog,
    include_media: include_media,
  ))
}

// =============================================================================
// Dialog Fetching (from Telegram Bridge)
// =============================================================================

/// Fetch all dialogs from Telegram
pub fn fetch_dialogs(
  session_id: String,
  limit: Int,
) -> Result(List(TelegramDialog), ParserError) {
  let cfg = config.get_config()
  let url =
    config.bridge_base_url(cfg)
    <> "/api/v1/dialogs?limit="
    <> int.to_string(limit)
    <> "&session="
    <> session_id

  let headers = [
    #("Authorization", "Bearer " <> config.api_key(cfg)),
    #("X-Session-ID", session_id),
  ]

  case http_get_with_headers(url, headers) {
    Ok(body) -> parse_dialogs_response(body)
    Error(e) -> Error(ParserApiError(e))
  }
}

/// Fetch message history for a dialog
pub fn fetch_messages(
  session_id: String,
  dialog_id: Int,
  limit: Int,
  offset_id: Option(Int),
) -> Result(List(RawMessage), ParserError) {
  let cfg = config.get_config()
  let base_url =
    config.bridge_base_url(cfg)
    <> "/api/v1/history/"
    <> int.to_string(dialog_id)
    <> "?limit="
    <> int.to_string(limit)
    <> "&session="
    <> session_id

  let url = case offset_id {
    Some(oid) -> base_url <> "&offset_id=" <> int.to_string(oid)
    None -> base_url
  }

  let headers = [
    #("Authorization", "Bearer " <> config.api_key(cfg)),
    #("X-Session-ID", session_id),
  ]

  case http_get_with_headers(url, headers) {
    Ok(body) -> parse_messages_response(body)
    Error(e) -> Error(ParserApiError(e))
  }
}

// =============================================================================
// Raw Message Type (from API)
// =============================================================================

/// Raw message from Telegram API
pub type RawMessage {
  RawMessage(
    id: Int,
    text: Option(String),
    from_id: Option(Int),
    from_name: Option(String),
    date: String,
    reply_to_id: Option(Int),
    forward_from_id: Option(Int),
    forward_from_name: Option(String),
    media_type: Option(String),
    views: Option(Int),
    forwards: Option(Int),
  )
}

/// Convert raw message to TelegramMessage for DB
pub fn raw_to_telegram_message(
  raw: RawMessage,
  dialog_id: Int,
) -> TelegramMessage {
  let content_type = case raw.media_type {
    Some("photo") -> ContentPhoto
    Some("voice") -> ContentVoice
    Some(_) -> ContentText
    None -> ContentText
  }

  TelegramMessage(
    id: 0,
    // Will be assigned by DB
    message_id: raw.id,
    dialog_id: dialog_id,
    sender_id: raw.from_id,
    sender_name: raw.from_name,
    content_type: content_type,
    text_content: raw.text,
    media_id: None,
    reply_to_id: raw.reply_to_id,
    forward_from_id: raw.forward_from_id,
    forward_from_name: raw.forward_from_name,
    timestamp: raw.date,
    has_embedding: False,
  )
}

// =============================================================================
// Parsing Logic
// =============================================================================

/// Parse all dialogs (main entry point)
pub fn parse_all_dialogs(
  pool: DbPool,
  session_id: String,
  cfg: ParserConfig,
  job_id: Int,
) -> Result(FullParseResult, ParserError) {
  // 1. Fetch all dialogs
  case fetch_dialogs(session_id, 500) {
    Error(e) -> Error(e)
    Ok(dialogs) -> {
      // 2. Save dialogs to DB
      let _ =
        dialogs
        |> list.map(fn(d) { postgres.upsert_dialog(pool, d) })

      let total = list.length(dialogs)

      // 3. Update job progress
      let _ = postgres.update_job_progress(pool, job_id, 0, total)

      // 4. Parse each dialog
      let results =
        dialogs
        |> list.index_map(fn(dialog, idx) {
          // Update progress
          let _ = postgres.update_job_progress(pool, job_id, idx + 1, total)

          // Parse this dialog
          let result = parse_single_dialog(pool, session_id, dialog.id, cfg)

          // Delay between dialogs
          sleep(cfg.delay_ms)

          result
        })

      // 5. Aggregate results
      let #(successes, errors) =
        results
        |> list.partition(fn(r) {
          case r {
            Ok(_) -> True
            Error(_) -> False
          }
        })

      let parsed_results =
        successes
        |> list.filter_map(fn(r) {
          case r {
            Ok(dr) -> Ok(dr)
            Error(_) -> Error(Nil)
          }
        })

      let error_msgs =
        errors
        |> list.filter_map(fn(r) {
          case r {
            Error(ParserApiError(e)) -> Ok(e)
            Error(ParserDbError(e)) -> Ok(e)
            _ -> Error(Nil)
          }
        })

      let total_messages =
        parsed_results
        |> list.map(fn(r: DialogParseResult) { r.messages_parsed })
        |> list.fold(0, fn(acc, n) { acc + n })

      let total_media =
        parsed_results
        |> list.map(fn(r: DialogParseResult) { r.media_found })
        |> list.fold(0, fn(acc, n) { acc + n })

      Ok(FullParseResult(
        total_dialogs: total,
        dialogs_parsed: list.length(successes),
        total_messages: total_messages,
        total_media: total_media,
        errors: error_msgs,
        duration_seconds: 0,
        // TODO: calculate
      ))
    }
  }
}

/// Parse a single dialog with all messages
pub fn parse_single_dialog(
  pool: DbPool,
  session_id: String,
  dialog_id: Int,
  cfg: ParserConfig,
) -> Result(DialogParseResult, ParserError) {
  // Mark dialog as in progress
  let _ = postgres.update_dialog_status(pool, dialog_id, ParseInProgress, 0)

  // Parse messages in batches
  parse_dialog_messages(pool, session_id, dialog_id, cfg, None, 0, 0, [])
}

/// Recursive batch parsing of dialog messages
fn parse_dialog_messages(
  pool: DbPool,
  session_id: String,
  dialog_id: Int,
  cfg: ParserConfig,
  offset_id: Option(Int),
  messages_count: Int,
  media_count: Int,
  errors: List(String),
) -> Result(DialogParseResult, ParserError) {
  // Check message limit
  case
    cfg.max_messages_per_dialog > 0
    && messages_count >= cfg.max_messages_per_dialog
  {
    True -> {
      // Reached limit, complete
      let _ =
        postgres.update_dialog_status(
          pool,
          dialog_id,
          ParseCompleted,
          messages_count,
        )
      Ok(DialogParseResult(
        dialog_id: dialog_id,
        messages_parsed: messages_count,
        media_found: media_count,
        errors: errors,
      ))
    }
    False -> {
      // Fetch next batch
      case fetch_messages(session_id, dialog_id, cfg.batch_size, offset_id) {
        Error(e) -> {
          let _ =
            postgres.update_dialog_status(
              pool,
              dialog_id,
              ParseFailed,
              messages_count,
            )
          Error(e)
        }
        Ok([]) -> {
          // No more messages
          let _ =
            postgres.update_dialog_status(
              pool,
              dialog_id,
              ParseCompleted,
              messages_count,
            )
          Ok(DialogParseResult(
            dialog_id: dialog_id,
            messages_parsed: messages_count,
            media_found: media_count,
            errors: errors,
          ))
        }
        Ok(raw_messages) -> {
          // Convert to DB format
          let messages =
            raw_messages
            |> list.map(fn(rm) { raw_to_telegram_message(rm, dialog_id) })

          // Insert batch to DB
          case postgres.insert_messages_batch(pool, messages) {
            Error(e) -> {
              let err_msg = case e {
                postgres.DbQueryError(msg) -> msg
                _ -> "Database error"
              }
              // Continue with error logged
              let new_errors = [err_msg, ..errors]
              let last_id =
                raw_messages
                |> list.last
                |> result.map(fn(m: RawMessage) { m.id })
                |> result.unwrap(0)

              // Delay between batches
              sleep(cfg.delay_ms)

              // Continue recursively
              parse_dialog_messages(
                pool,
                session_id,
                dialog_id,
                cfg,
                Some(last_id),
                messages_count + list.length(messages),
                media_count,
                new_errors,
              )
            }
            Ok(_) -> {
              // Count media
              let batch_media =
                raw_messages
                |> list.filter(fn(m) { option.is_some(m.media_type) })
                |> list.length

              // Get last message ID for pagination
              let last_id =
                raw_messages
                |> list.last
                |> result.map(fn(m: RawMessage) { m.id })
                |> result.unwrap(0)

              // Delay between batches
              sleep(cfg.delay_ms)

              // Continue recursively if we got a full batch
              case list.length(raw_messages) < cfg.batch_size {
                True -> {
                  // Last batch (incomplete)
                  let _ =
                    postgres.update_dialog_status(
                      pool,
                      dialog_id,
                      ParseCompleted,
                      messages_count + list.length(messages),
                    )
                  Ok(DialogParseResult(
                    dialog_id: dialog_id,
                    messages_parsed: messages_count + list.length(messages),
                    media_found: media_count + batch_media,
                    errors: errors,
                  ))
                }
                False -> {
                  // More messages to fetch
                  parse_dialog_messages(
                    pool,
                    session_id,
                    dialog_id,
                    cfg,
                    Some(last_id),
                    messages_count + list.length(messages),
                    media_count + batch_media,
                    errors,
                  )
                }
              }
            }
          }
        }
      }
    }
  }
}

// =============================================================================
// Response Parsing
// =============================================================================

fn parse_dialogs_response(
  body: String,
) -> Result(List(TelegramDialog), ParserError) {
  // Decoder for dialogs wrapped in an object
  let dialogs_wrapper_decoder = {
    use dialogs <- decode.field("dialogs", decode.list(decode_dialog_json()))
    decode.success(dialogs)
  }

  case json.parse(body, dialogs_wrapper_decoder) {
    Ok(dialogs) -> Ok(dialogs)
    Error(_) -> {
      // Try alternate format (direct list)
      case json.parse(body, decode.list(decode_dialog_json())) {
        Ok(dialogs) -> Ok(dialogs)
        Error(_) -> Error(ParserApiError("Failed to parse dialogs response"))
      }
    }
  }
}

fn decode_dialog_json() -> Decoder(TelegramDialog) {
  use id <- decode.field("id", decode.int)
  use dtype <- decode.optional_field("type", None, decode.optional(decode.string))
  use title <- decode.optional_field("title", None, decode.optional(decode.string))
  use username <- decode.optional_field("username", None, decode.optional(decode.string))
  use first_name <- decode.optional_field("first_name", None, decode.optional(decode.string))
  use last_name <- decode.optional_field("last_name", None, decode.optional(decode.string))
  use phone <- decode.optional_field("phone", None, decode.optional(decode.string))
  use participants <- decode.optional_field("participants_count", None, decode.optional(decode.int))
  use verified <- decode.optional_field("is_verified", None, decode.optional(decode.bool))
  use restricted <- decode.optional_field("is_restricted", None, decode.optional(decode.bool))
  use last_msg <- decode.optional_field("last_message_id", None, decode.optional(decode.int))
  use last_parsed <- decode.optional_field("last_parsed_at", None, decode.optional(decode.string))
  use total <- decode.optional_field("total_messages", None, decode.optional(decode.int))
  use parsed <- decode.optional_field("parsed_messages", None, decode.optional(decode.int))
  decode.success(TelegramDialog(
    id: id,
    dialog_type: parse_dialog_type(option.unwrap(dtype, "user")),
    title: title,
    username: username,
    first_name: first_name,
    last_name: last_name,
    phone: phone,
    participants_count: participants,
    is_verified: option.unwrap(verified, False),
    is_restricted: option.unwrap(restricted, False),
    last_message_id: last_msg,
    last_parsed_at: last_parsed,
    parse_status: ParsePending,
    total_messages: option.unwrap(total, 0),
    parsed_messages: option.unwrap(parsed, 0),
  ))
}

fn parse_dialog_type(s: String) -> postgres.DialogType {
  case s {
    "user" -> DialogUser
    "group" -> DialogGroup
    "supergroup" -> DialogGroup
    "channel" -> DialogChannel
    "bot" -> DialogBot
    _ -> DialogUser
  }
}

fn parse_messages_response(
  body: String,
) -> Result(List(RawMessage), ParserError) {
  // Decoder for messages wrapped in an object
  let messages_wrapper_decoder = {
    use messages <- decode.field("messages", decode.list(decode_raw_message()))
    decode.success(messages)
  }

  case json.parse(body, messages_wrapper_decoder) {
    Ok(messages) -> Ok(messages)
    Error(_) -> {
      // Try alternate format (direct list)
      case json.parse(body, decode.list(decode_raw_message())) {
        Ok(messages) -> Ok(messages)
        Error(_) -> Error(ParserApiError("Failed to parse messages response"))
      }
    }
  }
}

fn decode_raw_message() -> Decoder(RawMessage) {
  use id <- decode.field("id", decode.int)
  use text <- decode.field("text", decode.optional(decode.string))
  use from_id <- decode.field("from_id", decode.optional(decode.int))
  use from_name <- decode.field("from_name", decode.optional(decode.string))
  use date <- decode.field("date", decode.string)
  use reply_to <- decode.field("reply_to_id", decode.optional(decode.int))
  use forward_from_id <- decode.field(
    "forward_from_id",
    decode.optional(decode.int),
  )
  use forward_from_name <- decode.field(
    "forward_from_name",
    decode.optional(decode.string),
  )
  use media_type <- decode.field("media_type", decode.optional(decode.string))
  use views <- decode.field("views", decode.optional(decode.int))
  use forwards <- decode.field("forwards", decode.optional(decode.int))
  decode.success(RawMessage(
    id: id,
    text: text,
    from_id: from_id,
    from_name: from_name,
    date: date,
    reply_to_id: reply_to,
    forward_from_id: forward_from_id,
    forward_from_name: forward_from_name,
    media_type: media_type,
    views: views,
    forwards: forwards,
  ))
}

// =============================================================================
// FFI Functions
// =============================================================================

/// HTTP GET request (via Erlang httpc)
@external(erlang, "vibee_http_ffi", "get")
fn http_get(url: String) -> Result(String, String)

/// HTTP GET request with headers
@external(erlang, "vibee_http_ffi", "get_with_headers")
fn http_get_with_headers(
  url: String,
  headers: List(#(String, String)),
) -> Result(String, String)

/// Sleep for milliseconds
@external(erlang, "timer", "sleep")
fn sleep(ms: Int) -> Nil

// =============================================================================
// Result Serialization
// =============================================================================

/// Convert parse result to JSON
pub fn result_to_json(result: FullParseResult) -> String {
  json.object([
    #("total_dialogs", json.int(result.total_dialogs)),
    #("dialogs_parsed", json.int(result.dialogs_parsed)),
    #("total_messages", json.int(result.total_messages)),
    #("total_media", json.int(result.total_media)),
    #("errors", json.array(result.errors, json.string)),
    #("duration_seconds", json.int(result.duration_seconds)),
  ])
  |> json.to_string
}

/// Convert dialog parse result to JSON
pub fn dialog_result_to_json(result: DialogParseResult) -> String {
  json.object([
    #("dialog_id", json.int(result.dialog_id)),
    #("messages_parsed", json.int(result.messages_parsed)),
    #("media_found", json.int(result.media_found)),
    #("errors", json.array(result.errors, json.string)),
  ])
  |> json.to_string
}
