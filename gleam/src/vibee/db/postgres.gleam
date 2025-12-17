// PostgreSQL Database Module for Telegram RAG
// Connection pool and CRUD operations for telegram_dialogs, telegram_messages, telegram_media

import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/string
import pog
import shellout
import vibee/mcp/config

// =============================================================================
// Types
// =============================================================================

/// Database connection pool
pub type DbPool =
  pog.Connection

/// Dialog type enum
pub type DialogType {
  DialogUser
  DialogGroup
  DialogChannel
  DialogBot
}

/// Parse status enum
pub type ParseStatus {
  ParsePending
  ParseInProgress
  ParseCompleted
  ParseFailed
}

/// Media process status enum
pub type ProcessStatus {
  ProcessPending
  ProcessProcessing
  ProcessCompleted
  ProcessFailed
  ProcessSkipped
}

/// Content type enum
pub type ContentType {
  ContentText
  ContentPhoto
  ContentVoice
  ContentVideo
  ContentDocument
  ContentSticker
  ContentAnimation
  ContentVideoNote
  ContentAudio
  ContentContact
  ContentLocation
  ContentPoll
  ContentOther
}

/// Telegram Dialog record
pub type TelegramDialog {
  TelegramDialog(
    id: Int,
    dialog_type: DialogType,
    title: Option(String),
    username: Option(String),
    first_name: Option(String),
    last_name: Option(String),
    phone: Option(String),
    participants_count: Option(Int),
    is_verified: Bool,
    is_restricted: Bool,
    last_message_id: Option(Int),
    last_parsed_at: Option(String),
    parse_status: ParseStatus,
    total_messages: Int,
    parsed_messages: Int,
  )
}

/// Telegram Message record
pub type TelegramMessage {
  TelegramMessage(
    id: Int,
    message_id: Int,
    dialog_id: Int,
    sender_id: Option(Int),
    sender_name: Option(String),
    content_type: ContentType,
    text_content: Option(String),
    media_id: Option(Int),
    reply_to_id: Option(Int),
    forward_from_id: Option(Int),
    forward_from_name: Option(String),
    timestamp: String,
    has_embedding: Bool,
  )
}

/// Telegram Media record
pub type TelegramMedia {
  TelegramMedia(
    id: Int,
    message_id: Int,
    dialog_id: Int,
    media_type: String,
    file_id: Option(String),
    file_path: Option(String),
    file_size: Option(Int),
    mime_type: Option(String),
    duration_seconds: Option(Int),
    transcription: Option(String),
    image_description: Option(String),
    process_status: ProcessStatus,
  )
}

/// Parse Job record
pub type ParseJob {
  ParseJob(
    id: Int,
    job_type: String,
    dialog_id: Option(Int),
    status: String,
    progress_current: Int,
    progress_total: Int,
    started_at: Option(String),
    completed_at: Option(String),
    error_message: Option(String),
  )
}

/// Parse statistics
pub type ParseStats {
  ParseStats(
    total_dialogs: Int,
    parsed_dialogs: Int,
    total_messages: Int,
    messages_with_embedding: Int,
    total_media: Int,
    processed_media: Int,
    pending_jobs: Int,
    running_jobs: Int,
  )
}

/// Dialog metadata for context API
pub type DialogMetadata {
  DialogMetadata(
    dialog_id: Int,
    dialog_title: Option(String),
    dialog_type: String,
    total_messages: Int,
    messages_with_embedding: Int,
    first_message_date: Option(String),
    last_message_date: Option(String),
  )
}

/// Recent message for context (simplified)
pub type RecentMessage {
  RecentMessage(
    message_id: Int,
    sender_name: Option(String),
    text_content: String,
    timestamp: String,
    is_outgoing: Bool,
  )
}

/// Database error
pub type DbError {
  DbConnectionError(String)
  DbQueryError(String)
  DbNotFound
}

// =============================================================================
// Connection Pool
// =============================================================================

/// Create a new database connection pool
/// Uses SslVerified for Neon PostgreSQL (requires SNI)
pub fn connect(database_url: String) -> Result(DbPool, DbError) {
  let pool_name: process.Name(pog.Message) = process.new_name(prefix: "vibee_db_pool")
  case pog.url_config(pool_name, database_url) {
    Ok(config) -> {
      let pool_config =
        config
        |> pog.ssl(pog.SslVerified)  // Required for Neon PostgreSQL SNI
        |> pog.pool_size(10)
        |> pog.queue_target(15_000)
        |> pog.queue_interval(60_000)
        |> pog.idle_interval(600_000)

      case pog.start(pool_config) {
        Ok(actor.Started(_, connection)) -> Ok(connection)
        Error(_) -> Error(DbConnectionError("Failed to start database pool"))
      }
    }
    Error(_) -> Error(DbConnectionError("Invalid database URL"))
  }
}

/// Disconnect from database (no-op in pool-based architecture)
pub fn disconnect(_pool: DbPool) -> Nil {
  // In the new pog API, pools are managed through supervision
  // Connection cleanup happens automatically when the pool is stopped
  Nil
}

// =============================================================================
// Global Pool Management (via ETS)
// =============================================================================

/// Store pool in global ETS table
pub fn set_global_pool(pool: DbPool) -> Nil {
  set_global_pool_ffi(pool)
}

/// Get pool from global ETS table
pub fn get_global_pool() -> Option(DbPool) {
  get_global_pool_ffi()
}

@external(erlang, "vibee_db_pool_ffi", "set_global_pool")
fn set_global_pool_ffi(pool: DbPool) -> Nil

@external(erlang, "vibee_db_pool_ffi", "get_global_pool")
fn get_global_pool_ffi() -> Option(DbPool)

/// Helper to add multiple parameters to a query
fn add_parameters(query: pog.Query(t), params: List(pog.Value)) -> pog.Query(t) {
  list.fold(params, query, fn(q, p) { pog.parameter(q, p) })
}

// =============================================================================
// Dialog CRUD Operations
// =============================================================================

/// Insert or update a dialog
pub fn upsert_dialog(
  pool: DbPool,
  dialog: TelegramDialog,
) -> Result(Nil, DbError) {
  let sql =
    "INSERT INTO telegram_dialogs (id, type, title, username, first_name, last_name, phone, participants_count, is_verified, is_restricted, last_message_id, parse_status, total_messages, parsed_messages)
     VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)
     ON CONFLICT (id) DO UPDATE SET
       title = EXCLUDED.title,
       username = EXCLUDED.username,
       first_name = EXCLUDED.first_name,
       last_name = EXCLUDED.last_name,
       participants_count = EXCLUDED.participants_count,
       is_verified = EXCLUDED.is_verified,
       is_restricted = EXCLUDED.is_restricted,
       last_message_id = EXCLUDED.last_message_id,
       updated_at = NOW()"

  let params = [
    pog.int(dialog.id),
    pog.text(dialog_type_to_string(dialog.dialog_type)),
    pog.nullable(pog.text, dialog.title),
    pog.nullable(pog.text, dialog.username),
    pog.nullable(pog.text, dialog.first_name),
    pog.nullable(pog.text, dialog.last_name),
    pog.nullable(pog.text, dialog.phone),
    pog.nullable(pog.int, dialog.participants_count),
    pog.bool(dialog.is_verified),
    pog.bool(dialog.is_restricted),
    pog.nullable(pog.int, dialog.last_message_id),
    pog.text(parse_status_to_string(dialog.parse_status)),
    pog.int(dialog.total_messages),
    pog.int(dialog.parsed_messages),
  ]

  case pog.query(sql) |> add_parameters(params) |> pog.execute(pool) {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get dialog by ID
pub fn get_dialog(
  pool: DbPool,
  dialog_id: Int,
) -> Result(TelegramDialog, DbError) {
  let sql =
    "SELECT id, type, title, username, first_name, last_name, phone, participants_count,
            is_verified, is_restricted, last_message_id, last_parsed_at::text, parse_status,
            total_messages, parsed_messages
     FROM telegram_dialogs WHERE id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.int(dialog_id)])
    |> pog.returning(decode_dialog())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [dialog])) -> Ok(dialog)
    Ok(pog.Returned(_, [])) -> Error(DbNotFound)
    Ok(_) -> Error(DbNotFound)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// List all dialogs with optional filter
pub fn list_dialogs(
  pool: DbPool,
  status_filter: Option(ParseStatus),
  limit: Int,
  offset: Int,
) -> Result(List(TelegramDialog), DbError) {
  let base_sql =
    "SELECT id, type, title, username, first_name, last_name, phone, participants_count,
            is_verified, is_restricted, last_message_id, last_parsed_at::text, parse_status,
            total_messages, parsed_messages
     FROM telegram_dialogs"

  let #(sql, params) = case status_filter {
    Some(status) -> #(
      base_sql <> " WHERE parse_status = $1 ORDER BY id LIMIT $2 OFFSET $3",
      [
        pog.text(parse_status_to_string(status)),
        pog.int(limit),
        pog.int(offset),
      ],
    )
    None -> #(base_sql <> " ORDER BY id LIMIT $1 OFFSET $2", [
      pog.int(limit),
      pog.int(offset),
    ])
  }

  case
    pog.query(sql)
    |> add_parameters(params)
    |> pog.returning(decode_dialog())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, dialogs)) -> Ok(dialogs)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Update dialog parse status
pub fn update_dialog_status(
  pool: DbPool,
  dialog_id: Int,
  status: ParseStatus,
  parsed_messages: Int,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE telegram_dialogs
     SET parse_status = $2, parsed_messages = $3, last_parsed_at = NOW()
     WHERE id = $1"

  case
    pog.query(sql)
    |> add_parameters([
      pog.int(dialog_id),
      pog.text(parse_status_to_string(status)),
      pog.int(parsed_messages),
    ])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Message CRUD Operations
// =============================================================================

/// Insert a batch of messages (upsert)
pub fn insert_messages_batch(
  pool: DbPool,
  messages: List(TelegramMessage),
) -> Result(Int, DbError) {
  // Build batch insert with ON CONFLICT
  let values =
    messages
    |> list.index_map(fn(_msg, idx) {
      let base = idx * 11
      "($"
      <> int.to_string(base + 1)
      <> ", $"
      <> int.to_string(base + 2)
      <> ", $"
      <> int.to_string(base + 3)
      <> ", $"
      <> int.to_string(base + 4)
      <> ", $"
      <> int.to_string(base + 5)
      <> ", $"
      <> int.to_string(base + 6)
      <> ", $"
      <> int.to_string(base + 7)
      <> ", $"
      <> int.to_string(base + 8)
      <> ", $"
      <> int.to_string(base + 9)
      <> ", $"
      <> int.to_string(base + 10)
      <> ", $"
      <> int.to_string(base + 11)
      <> ")"
    })
    |> string.join(", ")

  let sql =
    "INSERT INTO telegram_messages (message_id, dialog_id, sender_id, sender_name, content_type, text_content, media_id, reply_to_id, forward_from_id, forward_from_name, timestamp)
     VALUES "
    <> values
    <> " ON CONFLICT (dialog_id, message_id) DO UPDATE SET
         text_content = EXCLUDED.text_content,
         sender_name = EXCLUDED.sender_name"

  let params =
    messages
    |> list.flat_map(fn(msg) {
      [
        pog.int(msg.message_id),
        pog.int(msg.dialog_id),
        pog.nullable(pog.int, msg.sender_id),
        pog.nullable(pog.text, msg.sender_name),
        pog.text(content_type_to_string(msg.content_type)),
        pog.nullable(pog.text, msg.text_content),
        pog.nullable(pog.int, msg.media_id),
        pog.nullable(pog.int, msg.reply_to_id),
        pog.nullable(pog.int, msg.forward_from_id),
        pog.nullable(pog.text, msg.forward_from_name),
        pog.text(msg.timestamp),
      ]
    })

  case pog.query(sql) |> add_parameters(params) |> pog.execute(pool) {
    Ok(pog.Returned(count, _)) -> Ok(count)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Сохранить одно сообщение в telegram_messages (для real-time polling)
/// Использует psql напрямую без пула соединений
/// Автоматически создает dialog если не существует (для RAG памяти)
pub fn insert_message_simple(
  dialog_id: Int,
  message_id: Int,
  sender_id: Int,
  sender_name: String,
  text_content: String,
) -> Result(Nil, String) {
  let db_url = config.get_env_or("DATABASE_URL", "")
  case db_url {
    "" -> {
      io.println("[DB] DATABASE_URL not set - skipping message save")
      Error("DATABASE_URL not set")
    }
    url -> {
      // Escape single quotes in text
      let escaped_text = string.replace(text_content, "'", "''")
      let escaped_name = string.replace(sender_name, "'", "''")

      // First ensure dialog exists (for foreign key constraint)
      // Schema: id BIGINT PRIMARY KEY, type VARCHAR(20), title VARCHAR(500)
      let dialog_sql = "INSERT INTO telegram_dialogs (id, type, title, created_at) VALUES ("
        <> int.to_string(dialog_id) <> ", 'user', '" <> escaped_name <> "', NOW()) "
        <> "ON CONFLICT (id) DO NOTHING"

      io.println("[DB] Creating dialog: " <> int.to_string(dialog_id))
      case shellout.command(run: "psql", with: [url, "-c", dialog_sql], in: ".", opt: []) {
        Ok(_) -> io.println("[DB] Dialog created or exists")
        Error(#(code, err)) -> io.println("[DB] Dialog insert error: code=" <> int.to_string(code) <> " err=" <> err)
      }

      // Then insert message
      let sql = "INSERT INTO telegram_messages (dialog_id, message_id, sender_id, sender_name, content_type, text_content, timestamp) VALUES ("
        <> int.to_string(dialog_id) <> ", "
        <> int.to_string(message_id) <> ", "
        <> int.to_string(sender_id) <> ", "
        <> "'" <> escaped_name <> "', "
        <> "'text', "
        <> "'" <> escaped_text <> "', "
        <> "NOW()) "
        <> "ON CONFLICT (dialog_id, message_id) DO UPDATE SET text_content = EXCLUDED.text_content"

      case shellout.command(run: "psql", with: [url, "-c", sql], in: ".", opt: []) {
        Ok(_) -> {
          io.println("[DB] Message saved: dialog=" <> int.to_string(dialog_id) <> " msg=" <> int.to_string(message_id))
          Ok(Nil)
        }
        Error(#(_, err)) -> {
          io.println("[DB] Insert error: " <> err)
          Error(err)
        }
      }
    }
  }
}

/// Get messages for a dialog with pagination
pub fn get_messages(
  pool: DbPool,
  dialog_id: Int,
  limit: Int,
  offset_message_id: Option(Int),
) -> Result(List(TelegramMessage), DbError) {
  let #(sql, params) = case offset_message_id {
    Some(offset_id) -> #(
      "SELECT id, message_id, dialog_id, sender_id, sender_name, content_type, text_content,
              media_id, reply_to_id, forward_from_id, forward_from_name, timestamp::text,
              (embedding IS NOT NULL) as has_embedding
       FROM telegram_messages
       WHERE dialog_id = $1 AND message_id < $2
       ORDER BY message_id DESC LIMIT $3",
      [pog.int(dialog_id), pog.int(offset_id), pog.int(limit)],
    )
    None -> #(
      "SELECT id, message_id, dialog_id, sender_id, sender_name, content_type, text_content,
              media_id, reply_to_id, forward_from_id, forward_from_name, timestamp::text,
              (embedding IS NOT NULL) as has_embedding
       FROM telegram_messages
       WHERE dialog_id = $1
       ORDER BY message_id DESC LIMIT $2",
      [pog.int(dialog_id), pog.int(limit)],
    )
  }

  case
    pog.query(sql)
    |> add_parameters(params)
    |> pog.returning(decode_message())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, messages)) -> Ok(messages)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get messages without embeddings (for batch embedding)
pub fn get_messages_without_embedding(
  pool: DbPool,
  limit: Int,
) -> Result(List(TelegramMessage), DbError) {
  let sql =
    "SELECT id, message_id, dialog_id, sender_id, sender_name, content_type, text_content,
            media_id, reply_to_id, forward_from_id, forward_from_name, timestamp::text, false
     FROM telegram_messages
     WHERE embedding IS NULL AND text_content IS NOT NULL AND LENGTH(text_content) > 10
     ORDER BY id LIMIT $1"

  case
    pog.query(sql)
    |> add_parameters([pog.int(limit)])
    |> pog.returning(decode_message())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, messages)) -> Ok(messages)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Update message embedding
pub fn update_message_embedding(
  pool: DbPool,
  message_id: Int,
  embedding: List(Float),
  model: String,
) -> Result(Nil, DbError) {
  // Convert embedding list to PostgreSQL vector format
  let embedding_str =
    "["
    <> {
      embedding
      |> list.map(float_to_string)
      |> string.join(",")
    }
    <> "]"

  let sql =
    "UPDATE telegram_messages
     SET embedding = $2::vector, embedding_model = $3, embedding_created_at = NOW()
     WHERE id = $1"

  case
    pog.query(sql)
    |> add_parameters([
      pog.int(message_id),
      pog.text(embedding_str),
      pog.text(model),
    ])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Media CRUD Operations
// =============================================================================

/// Insert media record
pub fn insert_media(pool: DbPool, media: TelegramMedia) -> Result(Int, DbError) {
  let sql =
    "INSERT INTO telegram_media (message_id, dialog_id, media_type, file_id, file_path, file_size, mime_type, duration_seconds, process_status)
     VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
     RETURNING id"

  let id_decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(id)
  }

  case
    pog.query(sql)
    |> add_parameters([
      pog.int(media.message_id),
      pog.int(media.dialog_id),
      pog.text(media.media_type),
      pog.nullable(pog.text, media.file_id),
      pog.nullable(pog.text, media.file_path),
      pog.nullable(pog.int, media.file_size),
      pog.nullable(pog.text, media.mime_type),
      pog.nullable(pog.int, media.duration_seconds),
      pog.text(process_status_to_string(media.process_status)),
    ])
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [id])) -> Ok(id)
    Ok(_) -> Error(DbQueryError("Failed to get inserted media id"))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get pending media for processing
pub fn get_pending_media(
  pool: DbPool,
  media_type: Option(String),
  limit: Int,
) -> Result(List(TelegramMedia), DbError) {
  let #(sql, params) = case media_type {
    Some(mt) -> #(
      "SELECT id, message_id, dialog_id, media_type, file_id, file_path, file_size,
              mime_type, duration_seconds, transcription, image_description, process_status
       FROM telegram_media
       WHERE process_status = 'pending' AND media_type = $1
       ORDER BY id LIMIT $2",
      [pog.text(mt), pog.int(limit)],
    )
    None -> #(
      "SELECT id, message_id, dialog_id, media_type, file_id, file_path, file_size,
              mime_type, duration_seconds, transcription, image_description, process_status
       FROM telegram_media
       WHERE process_status = 'pending'
       ORDER BY id LIMIT $1",
      [pog.int(limit)],
    )
  }

  case
    pog.query(sql)
    |> add_parameters(params)
    |> pog.returning(decode_media())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, media_list)) -> Ok(media_list)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Update media transcription (for voice messages)
pub fn update_media_transcription(
  pool: DbPool,
  media_id: Int,
  transcription: String,
  model: String,
  language: String,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE telegram_media
     SET transcription = $2, transcription_model = $3, transcription_language = $4,
         process_status = 'completed', transcription_created_at = NOW()
     WHERE id = $1"

  case
    pog.query(sql)
    |> add_parameters([
      pog.int(media_id),
      pog.text(transcription),
      pog.text(model),
      pog.text(language),
    ])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Update media image description (for photos)
pub fn update_media_vision(
  pool: DbPool,
  media_id: Int,
  description: String,
  ocr_text: Option(String),
  model: String,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE telegram_media
     SET image_description = $2, image_ocr_text = $3, vision_model = $4,
         process_status = 'completed', vision_created_at = NOW()
     WHERE id = $1"

  case
    pog.query(sql)
    |> add_parameters([
      pog.int(media_id),
      pog.text(description),
      pog.nullable(pog.text, ocr_text),
      pog.text(model),
    ])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Update media process status
pub fn update_media_status(
  pool: DbPool,
  media_id: Int,
  status: ProcessStatus,
  error: Option(String),
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE telegram_media SET process_status = $2, process_error = $3 WHERE id = $1"

  case
    pog.query(sql)
    |> add_parameters([
      pog.int(media_id),
      pog.text(process_status_to_string(status)),
      pog.nullable(pog.text, error),
    ])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Parse Job Operations
// =============================================================================

/// Create a new parse job (simplified - without config)
pub fn create_parse_job(
  pool: DbPool,
  job_type: String,
  dialog_id: Option(Int),
) -> Result(Int, DbError) {
  let sql =
    "INSERT INTO telegram_parse_jobs (job_type, dialog_id, status)
     VALUES ($1, $2, 'pending')
     RETURNING id"

  let id_decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(id)
  }

  case
    pog.query(sql)
    |> add_parameters([
      pog.text(job_type),
      pog.nullable(pog.int, dialog_id),
    ])
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [id])) -> Ok(id)
    Ok(_) -> Error(DbQueryError("Failed to create parse job"))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Create a new parse job with config
pub fn create_parse_job_with_config(
  pool: DbPool,
  job_type: String,
  dialog_id: Option(Int),
  config_json: String,
) -> Result(Int, DbError) {
  let sql =
    "INSERT INTO telegram_parse_jobs (job_type, dialog_id, status, config)
     VALUES ($1, $2, 'pending', $3::jsonb)
     RETURNING id"

  let id_decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(id)
  }

  case
    pog.query(sql)
    |> add_parameters([
      pog.text(job_type),
      pog.nullable(pog.int, dialog_id),
      pog.text(config_json),
    ])
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [id])) -> Ok(id)
    Ok(_) -> Error(DbQueryError("Failed to create parse job"))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Update parse job progress
pub fn update_job_progress(
  pool: DbPool,
  job_id: Int,
  current: Int,
  total: Int,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE telegram_parse_jobs
     SET progress_current = $2, progress_total = $3, status = 'running', started_at = COALESCE(started_at, NOW())
     WHERE id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.int(job_id), pog.int(current), pog.int(total)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Complete a parse job
pub fn complete_job(
  pool: DbPool,
  job_id: Int,
  result_json: String,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE telegram_parse_jobs
     SET status = 'completed', completed_at = NOW(), result = $2::jsonb
     WHERE id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.int(job_id), pog.text(result_json)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Fail a parse job
pub fn fail_job(
  pool: DbPool,
  job_id: Int,
  error: String,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE telegram_parse_jobs
     SET status = 'failed', completed_at = NOW(), error_message = $2
     WHERE id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.int(job_id), pog.text(error)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Alias for complete_job (used by router)
pub fn complete_parse_job(
  pool: DbPool,
  job_id: Int,
  result_json: String,
) -> Result(Nil, DbError) {
  complete_job(pool, job_id, result_json)
}

/// Alias for fail_job (used by router)
pub fn fail_parse_job(
  pool: DbPool,
  job_id: Int,
  error: String,
) -> Result(Nil, DbError) {
  fail_job(pool, job_id, error)
}

/// Get parse statistics (returns ParseStats directly, defaults on error)
pub fn get_parse_stats(pool: DbPool) -> ParseStats {
  // Use inline SQL query instead of stored function for portability
  let sql =
    "SELECT
       COALESCE((SELECT COUNT(*) FROM telegram_dialogs), 0) as total_dialogs,
       COALESCE((SELECT COUNT(*) FROM telegram_dialogs WHERE parse_status = 'completed'), 0) as parsed_dialogs,
       COALESCE((SELECT COUNT(*) FROM telegram_messages), 0) as total_messages,
       COALESCE((SELECT COUNT(*) FROM telegram_messages WHERE embedding IS NOT NULL), 0) as messages_with_embedding,
       COALESCE((SELECT COUNT(*) FROM telegram_media), 0) as total_media,
       COALESCE((SELECT COUNT(*) FROM telegram_media WHERE process_status = 'completed'), 0) as processed_media,
       COALESCE((SELECT COUNT(*) FROM telegram_parse_jobs WHERE status = 'pending'), 0) as pending_jobs,
       COALESCE((SELECT COUNT(*) FROM telegram_parse_jobs WHERE status = 'running'), 0) as running_jobs"

  case
    pog.query(sql)
    |> pog.returning(decode_parse_stats())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [stats])) -> stats
    _ -> ParseStats(0, 0, 0, 0, 0, 0, 0, 0)
  }
}

/// Get parse statistics as Result (for error handling)
pub fn get_parse_stats_result(pool: DbPool) -> Result(ParseStats, DbError) {
  let sql =
    "SELECT
       COALESCE((SELECT COUNT(*) FROM telegram_dialogs), 0) as total_dialogs,
       COALESCE((SELECT COUNT(*) FROM telegram_dialogs WHERE parse_status = 'completed'), 0) as parsed_dialogs,
       COALESCE((SELECT COUNT(*) FROM telegram_messages), 0) as total_messages,
       COALESCE((SELECT COUNT(*) FROM telegram_messages WHERE embedding IS NOT NULL), 0) as messages_with_embedding,
       COALESCE((SELECT COUNT(*) FROM telegram_media), 0) as total_media,
       COALESCE((SELECT COUNT(*) FROM telegram_media WHERE process_status = 'completed'), 0) as processed_media,
       COALESCE((SELECT COUNT(*) FROM telegram_parse_jobs WHERE status = 'pending'), 0) as pending_jobs,
       COALESCE((SELECT COUNT(*) FROM telegram_parse_jobs WHERE status = 'running'), 0) as running_jobs"

  case
    pog.query(sql)
    |> pog.returning(decode_parse_stats())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [stats])) -> Ok(stats)
    Ok(_) -> Ok(ParseStats(0, 0, 0, 0, 0, 0, 0, 0))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Search Operations
// =============================================================================

/// Hybrid search result
pub type SearchResult {
  SearchResult(
    message_id: Int,
    dialog_id: Int,
    text_content: String,
    timestamp: String,
    sender_name: Option(String),
    vector_rank: Int,
    keyword_rank: Int,
    rrf_score: Float,
  )
}

/// Perform hybrid search (vector + keyword with RRF)
pub fn hybrid_search(
  pool: DbPool,
  query_embedding: List(Float),
  query_text: String,
  limit: Int,
) -> Result(List(SearchResult), DbError) {
  let embedding_str =
    "["
    <> {
      query_embedding
      |> list.map(float_to_string)
      |> string.join(",")
    }
    <> "]"

  let sql = "SELECT * FROM hybrid_search($1::vector, $2, $3)"

  case
    pog.query(sql)
    |> add_parameters([
      pog.text(embedding_str),
      pog.text(query_text),
      pog.int(limit),
    ])
    |> pog.returning(decode_search_result())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, results)) -> Ok(results)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Vector-only semantic search
pub fn semantic_search(
  pool: DbPool,
  query_embedding: List(Float),
  limit: Int,
  threshold: Float,
) -> Result(List(SearchResult), DbError) {
  let embedding_str =
    "["
    <> {
      query_embedding
      |> list.map(float_to_string)
      |> string.join(",")
    }
    <> "]"

  let sql =
    "SELECT message_id, dialog_id, text_content, timestamp::text, sender_name,
            ROW_NUMBER() OVER () as vector_rank, 9999 as keyword_rank,
            1.0 - (embedding <=> $1::vector) as rrf_score
     FROM telegram_messages
     WHERE embedding IS NOT NULL AND 1.0 - (embedding <=> $1::vector) > $3
     ORDER BY embedding <=> $1::vector
     LIMIT $2"

  case
    pog.query(sql)
    |> add_parameters([
      pog.text(embedding_str),
      pog.int(limit),
      pog.float(threshold),
    ])
    |> pog.returning(decode_search_result())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, results)) -> Ok(results)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Full-text keyword search
pub fn keyword_search(
  pool: DbPool,
  query_text: String,
  limit: Int,
) -> Result(List(SearchResult), DbError) {
  let sql =
    "SELECT message_id, dialog_id, text_content, timestamp::text, sender_name,
            9999 as vector_rank, ROW_NUMBER() OVER () as keyword_rank,
            ts_rank(text_search, websearch_to_tsquery('russian', $1))::float as rrf_score
     FROM telegram_messages
     WHERE text_search @@ websearch_to_tsquery('russian', $1)
     ORDER BY ts_rank(text_search, websearch_to_tsquery('russian', $1)) DESC
     LIMIT $2"

  case
    pog.query(sql)
    |> add_parameters([pog.text(query_text), pog.int(limit)])
    |> pog.returning(decode_search_result())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, results)) -> Ok(results)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Conversation Context Operations (for AI Digital Clone)
// =============================================================================

/// Get recent messages for a dialog (for conversation context)
pub fn get_recent_messages(
  pool: DbPool,
  dialog_id: Int,
  limit: Int,
  owner_id: Int,
) -> Result(List(RecentMessage), DbError) {
  let sql =
    "SELECT message_id, sender_name, COALESCE(text_content, '') as text_content,
            timestamp::text, (sender_id = $3) as is_outgoing
     FROM telegram_messages
     WHERE dialog_id = $1 AND text_content IS NOT NULL AND LENGTH(text_content) > 0
     ORDER BY timestamp DESC
     LIMIT $2"

  case
    pog.query(sql)
    |> add_parameters([pog.int(dialog_id), pog.int(limit), pog.int(owner_id)])
    |> pog.returning(decode_recent_message())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, messages)) -> Ok(list.reverse(messages))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get dialog metadata for context
pub fn get_dialog_metadata(
  pool: DbPool,
  dialog_id: Int,
) -> Result(DialogMetadata, DbError) {
  let sql =
    "SELECT
       d.id as dialog_id,
       COALESCE(d.title, d.first_name || ' ' || COALESCE(d.last_name, '')) as dialog_title,
       d.type as dialog_type,
       COUNT(m.id) as total_messages,
       COUNT(m.embedding) as messages_with_embedding,
       MIN(m.timestamp)::text as first_message_date,
       MAX(m.timestamp)::text as last_message_date
     FROM telegram_dialogs d
     LEFT JOIN telegram_messages m ON d.id = m.dialog_id
     WHERE d.id = $1
     GROUP BY d.id, d.title, d.first_name, d.last_name, d.type"

  case
    pog.query(sql)
    |> add_parameters([pog.int(dialog_id)])
    |> pog.returning(decode_dialog_metadata())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [metadata])) -> Ok(metadata)
    Ok(pog.Returned(_, [])) -> Error(DbNotFound)
    Ok(_) -> Error(DbNotFound)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get semantic search results filtered by dialog
pub fn semantic_search_in_dialog(
  pool: DbPool,
  dialog_id: Int,
  query_embedding: List(Float),
  limit: Int,
) -> Result(List(SearchResult), DbError) {
  let embedding_str =
    "["
    <> {
      query_embedding
      |> list.map(float_to_string)
      |> string.join(",")
    }
    <> "]"

  let sql =
    "SELECT message_id, dialog_id, text_content, timestamp::text, sender_name,
            ROW_NUMBER() OVER () as vector_rank, 9999 as keyword_rank,
            1.0 - (embedding <=> $1::vector) as rrf_score
     FROM telegram_messages
     WHERE dialog_id = $2 AND embedding IS NOT NULL
     ORDER BY embedding <=> $1::vector
     LIMIT $3"

  case
    pog.query(sql)
    |> add_parameters([
      pog.text(embedding_str),
      pog.int(dialog_id),
      pog.int(limit),
    ])
    |> pog.returning(decode_search_result())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, results)) -> Ok(results)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

fn dialog_type_to_string(dt: DialogType) -> String {
  case dt {
    DialogUser -> "user"
    DialogGroup -> "group"
    DialogChannel -> "channel"
    DialogBot -> "bot"
  }
}

fn string_to_dialog_type(s: String) -> DialogType {
  case s {
    "user" -> DialogUser
    "group" -> DialogGroup
    "channel" -> DialogChannel
    "bot" -> DialogBot
    _ -> DialogUser
  }
}

fn parse_status_to_string(ps: ParseStatus) -> String {
  case ps {
    ParsePending -> "pending"
    ParseInProgress -> "in_progress"
    ParseCompleted -> "completed"
    ParseFailed -> "failed"
  }
}

fn string_to_parse_status(s: String) -> ParseStatus {
  case s {
    "pending" -> ParsePending
    "in_progress" -> ParseInProgress
    "completed" -> ParseCompleted
    "failed" -> ParseFailed
    _ -> ParsePending
  }
}

fn process_status_to_string(ps: ProcessStatus) -> String {
  case ps {
    ProcessPending -> "pending"
    ProcessProcessing -> "processing"
    ProcessCompleted -> "completed"
    ProcessFailed -> "failed"
    ProcessSkipped -> "skipped"
  }
}

fn string_to_process_status(s: String) -> ProcessStatus {
  case s {
    "pending" -> ProcessPending
    "processing" -> ProcessProcessing
    "completed" -> ProcessCompleted
    "failed" -> ProcessFailed
    "skipped" -> ProcessSkipped
    _ -> ProcessPending
  }
}

fn content_type_to_string(ct: ContentType) -> String {
  case ct {
    ContentText -> "text"
    ContentPhoto -> "photo"
    ContentVoice -> "voice"
    ContentVideo -> "video"
    ContentDocument -> "document"
    ContentSticker -> "sticker"
    ContentAnimation -> "animation"
    ContentVideoNote -> "video_note"
    ContentAudio -> "audio"
    ContentContact -> "contact"
    ContentLocation -> "location"
    ContentPoll -> "poll"
    ContentOther -> "other"
  }
}

fn string_to_content_type(s: String) -> ContentType {
  case s {
    "text" -> ContentText
    "photo" -> ContentPhoto
    "voice" -> ContentVoice
    "video" -> ContentVideo
    "document" -> ContentDocument
    "sticker" -> ContentSticker
    "animation" -> ContentAnimation
    "video_note" -> ContentVideoNote
    "audio" -> ContentAudio
    "contact" -> ContentContact
    "location" -> ContentLocation
    "poll" -> ContentPoll
    _ -> ContentOther
  }
}

fn pog_error_to_string(err: pog.QueryError) -> String {
  case err {
    pog.ConnectionUnavailable -> "Connection unavailable"
    pog.ConstraintViolated(msg, constraint, _) ->
      "Constraint violated: " <> msg <> " (" <> constraint <> ")"
    pog.PostgresqlError(code, name, msg) ->
      "PostgreSQL error " <> code <> " [" <> name <> "]: " <> msg
    pog.UnexpectedArgumentCount(expected, got) ->
      "Unexpected argument count: expected "
      <> int.to_string(expected)
      <> ", got "
      <> int.to_string(got)
    pog.UnexpectedArgumentType(expected, got) ->
      "Unexpected argument type: expected " <> expected <> ", got " <> got
    pog.UnexpectedResultType(_) -> "Unexpected result type"
    pog.QueryTimeout -> "Query timeout"
  }
}

@external(erlang, "erlang", "float_to_binary")
fn float_to_string(f: Float) -> String

// =============================================================================
// Dynamic Decoders using gleam/dynamic/decode
// =============================================================================

fn decode_dialog() -> Decoder(TelegramDialog) {
  use id <- decode.field(0, decode.int)
  use dtype <- decode.field(1, decode.string)
  use title <- decode.field(2, decode.optional(decode.string))
  use username <- decode.field(3, decode.optional(decode.string))
  use first_name <- decode.field(4, decode.optional(decode.string))
  use last_name <- decode.field(5, decode.optional(decode.string))
  use phone <- decode.field(6, decode.optional(decode.string))
  use participants <- decode.field(7, decode.optional(decode.int))
  use verified <- decode.field(8, decode.bool)
  use restricted <- decode.field(9, decode.bool)
  use last_msg <- decode.field(10, decode.optional(decode.int))
  use last_parsed <- decode.field(11, decode.optional(decode.string))
  use status <- decode.field(12, decode.string)
  use total <- decode.field(13, decode.int)
  use parsed <- decode.field(14, decode.int)
  decode.success(TelegramDialog(
    id: id,
    dialog_type: string_to_dialog_type(dtype),
    title: title,
    username: username,
    first_name: first_name,
    last_name: last_name,
    phone: phone,
    participants_count: participants,
    is_verified: verified,
    is_restricted: restricted,
    last_message_id: last_msg,
    last_parsed_at: last_parsed,
    parse_status: string_to_parse_status(status),
    total_messages: total,
    parsed_messages: parsed,
  ))
}

fn decode_message() -> Decoder(TelegramMessage) {
  use id <- decode.field(0, decode.int)
  use msg_id <- decode.field(1, decode.int)
  use dialog_id <- decode.field(2, decode.int)
  use sender_id <- decode.field(3, decode.optional(decode.int))
  use sender_name <- decode.field(4, decode.optional(decode.string))
  use content_type <- decode.field(5, decode.string)
  use text <- decode.field(6, decode.optional(decode.string))
  use media_id <- decode.field(7, decode.optional(decode.int))
  use reply_to <- decode.field(8, decode.optional(decode.int))
  use forward_from <- decode.field(9, decode.optional(decode.int))
  use forward_name <- decode.field(10, decode.optional(decode.string))
  use timestamp <- decode.field(11, decode.string)
  use has_embed <- decode.field(12, decode.bool)
  decode.success(TelegramMessage(
    id: id,
    message_id: msg_id,
    dialog_id: dialog_id,
    sender_id: sender_id,
    sender_name: sender_name,
    content_type: string_to_content_type(content_type),
    text_content: text,
    media_id: media_id,
    reply_to_id: reply_to,
    forward_from_id: forward_from,
    forward_from_name: forward_name,
    timestamp: timestamp,
    has_embedding: has_embed,
  ))
}

fn decode_media() -> Decoder(TelegramMedia) {
  use id <- decode.field(0, decode.int)
  use msg_id <- decode.field(1, decode.int)
  use dialog_id <- decode.field(2, decode.int)
  use media_type <- decode.field(3, decode.string)
  use file_id <- decode.field(4, decode.optional(decode.string))
  use file_path <- decode.field(5, decode.optional(decode.string))
  use file_size <- decode.field(6, decode.optional(decode.int))
  use mime <- decode.field(7, decode.optional(decode.string))
  use duration <- decode.field(8, decode.optional(decode.int))
  use transcription <- decode.field(9, decode.optional(decode.string))
  use description <- decode.field(10, decode.optional(decode.string))
  use status <- decode.field(11, decode.string)
  decode.success(TelegramMedia(
    id: id,
    message_id: msg_id,
    dialog_id: dialog_id,
    media_type: media_type,
    file_id: file_id,
    file_path: file_path,
    file_size: file_size,
    mime_type: mime,
    duration_seconds: duration,
    transcription: transcription,
    image_description: description,
    process_status: string_to_process_status(status),
  ))
}

fn decode_parse_stats() -> Decoder(ParseStats) {
  use total_dialogs <- decode.field(0, decode.int)
  use parsed_dialogs <- decode.field(1, decode.int)
  use total_messages <- decode.field(2, decode.int)
  use messages_with_embedding <- decode.field(3, decode.int)
  use total_media <- decode.field(4, decode.int)
  use processed_media <- decode.field(5, decode.int)
  use pending_jobs <- decode.field(6, decode.int)
  use running_jobs <- decode.field(7, decode.int)
  decode.success(ParseStats(
    total_dialogs: total_dialogs,
    parsed_dialogs: parsed_dialogs,
    total_messages: total_messages,
    messages_with_embedding: messages_with_embedding,
    total_media: total_media,
    processed_media: processed_media,
    pending_jobs: pending_jobs,
    running_jobs: running_jobs,
  ))
}

fn decode_search_result() -> Decoder(SearchResult) {
  use message_id <- decode.field(0, decode.int)
  use dialog_id <- decode.field(1, decode.int)
  use text_content <- decode.field(2, decode.string)
  use timestamp <- decode.field(3, decode.string)
  use sender_name <- decode.field(4, decode.optional(decode.string))
  use vector_rank <- decode.field(5, decode.int)
  use keyword_rank <- decode.field(6, decode.int)
  use rrf_score <- decode.field(7, decode.float)
  decode.success(SearchResult(
    message_id: message_id,
    dialog_id: dialog_id,
    text_content: text_content,
    timestamp: timestamp,
    sender_name: sender_name,
    vector_rank: vector_rank,
    keyword_rank: keyword_rank,
    rrf_score: rrf_score,
  ))
}

fn decode_recent_message() -> Decoder(RecentMessage) {
  use message_id <- decode.field(0, decode.int)
  use sender_name <- decode.field(1, decode.optional(decode.string))
  use text_content <- decode.field(2, decode.string)
  use timestamp <- decode.field(3, decode.string)
  use is_outgoing <- decode.field(4, decode.bool)
  decode.success(RecentMessage(
    message_id: message_id,
    sender_name: sender_name,
    text_content: text_content,
    timestamp: timestamp,
    is_outgoing: is_outgoing,
  ))
}

fn decode_dialog_metadata() -> Decoder(DialogMetadata) {
  use dialog_id <- decode.field(0, decode.int)
  use dialog_title <- decode.field(1, decode.optional(decode.string))
  use dialog_type <- decode.field(2, decode.string)
  use total_messages <- decode.field(3, decode.int)
  use messages_with_embedding <- decode.field(4, decode.int)
  use first_message_date <- decode.field(5, decode.optional(decode.string))
  use last_message_date <- decode.field(6, decode.optional(decode.string))
  decode.success(DialogMetadata(
    dialog_id: dialog_id,
    dialog_title: dialog_title,
    dialog_type: dialog_type,
    total_messages: total_messages,
    messages_with_embedding: messages_with_embedding,
    first_message_date: first_message_date,
    last_message_date: last_message_date,
  ))
}
