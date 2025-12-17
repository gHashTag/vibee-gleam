// AI Jobs Database Module
// CRUD operations for async AI service jobs (ElevenLabs, Hedra, BFL, Kling, etc.)
// Uses the same pog connection pool as postgres.gleam

import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import pog
import vibee/db/postgres.{type DbError, type DbPool, DbNotFound, DbQueryError}

// =============================================================================
// Types
// =============================================================================

/// AI Job status
pub type JobStatus {
  JobPending
  JobProcessing
  JobCompleted
  JobFailed
  JobCancelled
}

/// AI Job type - matches SQL CHECK constraint
pub type JobType {
  JobElevenLabsTTS
  JobElevenLabsVoiceClone
  JobHedraAvatar
  JobHeyGenVideo
  JobBflImage
  JobKlingVideo
  JobKlingImageToVideo
}

/// AI Job record
pub type AiJob {
  AiJob(
    id: String,
    job_type: JobType,
    status: JobStatus,
    user_id: Option(Int),
    input_data: String,
    output_data: Option(String),
    error_message: Option(String),
    external_id: Option(String),
    created_at: String,
    updated_at: String,
    completed_at: Option(String),
  )
}

/// Create job request
pub type CreateJobRequest {
  CreateJobRequest(
    id: String,
    job_type: JobType,
    user_id: Option(Int),
    input_data: String,
    external_id: Option(String),
  )
}

// =============================================================================
// CRUD Operations
// =============================================================================

/// Helper to add multiple parameters to a query
fn add_parameters(query: pog.Query(t), params: List(pog.Value)) -> pog.Query(t) {
  list.fold(params, query, fn(q, p) { pog.parameter(q, p) })
}

/// Create a new AI job
pub fn create_job(pool: DbPool, req: CreateJobRequest) -> Result(AiJob, DbError) {
  let sql =
    "INSERT INTO ai_jobs (id, job_type, status, user_id, input_data, external_job_id)
     VALUES ($1, $2, 'pending', $3, $4::jsonb, $5)
     RETURNING id, job_type, status, user_id, input_data::text, output_data::text,
               error_message, external_job_id, created_at::text, updated_at::text, completed_at::text"

  case
    pog.query(sql)
    |> add_parameters([
      pog.text(req.id),
      pog.text(job_type_to_string(req.job_type)),
      pog.nullable(pog.int, req.user_id),
      pog.text(req.input_data),
      pog.nullable(pog.text, req.external_id),
    ])
    |> pog.returning(decode_ai_job())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [job])) -> Ok(job)
    Ok(_) -> Error(DbQueryError("Failed to create job"))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get job by ID
pub fn get_job(pool: DbPool, job_id: String) -> Result(AiJob, DbError) {
  let sql =
    "SELECT id, job_type, status, user_id, input_data::text, output_data::text,
            error_message, external_job_id, created_at::text, updated_at::text, completed_at::text
     FROM ai_jobs WHERE id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.text(job_id)])
    |> pog.returning(decode_ai_job())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [job])) -> Ok(job)
    Ok(pog.Returned(_, [])) -> Error(DbNotFound)
    Ok(_) -> Error(DbNotFound)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get job by external ID (e.g., Replicate prediction ID, Hedra job ID)
pub fn get_job_by_external_id(
  pool: DbPool,
  external_id: String,
) -> Result(AiJob, DbError) {
  let sql =
    "SELECT id, job_type, status, user_id, input_data::text, output_data::text,
            error_message, external_job_id, created_at::text, updated_at::text, completed_at::text
     FROM ai_jobs WHERE external_job_id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.text(external_id)])
    |> pog.returning(decode_ai_job())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [job])) -> Ok(job)
    Ok(pog.Returned(_, [])) -> Error(DbNotFound)
    Ok(_) -> Error(DbNotFound)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// List jobs by status
pub fn list_jobs(
  pool: DbPool,
  status_filter: Option(JobStatus),
  type_filter: Option(JobType),
  limit: Int,
  offset: Int,
) -> Result(List(AiJob), DbError) {
  let base_sql =
    "SELECT id, job_type, status, user_id, input_data::text, output_data::text,
            error_message, external_job_id, created_at::text, updated_at::text, completed_at::text
     FROM ai_jobs"

  let #(sql, params) = case status_filter, type_filter {
    Some(status), Some(job_type) -> #(
      base_sql
        <> " WHERE status = $1 AND job_type = $2 ORDER BY created_at DESC LIMIT $3 OFFSET $4",
      [
        pog.text(job_status_to_string(status)),
        pog.text(job_type_to_string(job_type)),
        pog.int(limit),
        pog.int(offset),
      ],
    )
    Some(status), None -> #(
      base_sql
        <> " WHERE status = $1 ORDER BY created_at DESC LIMIT $2 OFFSET $3",
      [
        pog.text(job_status_to_string(status)),
        pog.int(limit),
        pog.int(offset),
      ],
    )
    None, Some(job_type) -> #(
      base_sql
        <> " WHERE job_type = $1 ORDER BY created_at DESC LIMIT $2 OFFSET $3",
      [
        pog.text(job_type_to_string(job_type)),
        pog.int(limit),
        pog.int(offset),
      ],
    )
    None, None -> #(base_sql <> " ORDER BY created_at DESC LIMIT $1 OFFSET $2", [
      pog.int(limit),
      pog.int(offset),
    ])
  }

  case
    pog.query(sql)
    |> add_parameters(params)
    |> pog.returning(decode_ai_job())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, jobs)) -> Ok(jobs)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Update job status to processing
pub fn start_job(
  pool: DbPool,
  job_id: String,
  external_id: Option(String),
) -> Result(Nil, DbError) {
  let sql = case external_id {
    Some(_) ->
      "UPDATE ai_jobs
       SET status = 'processing', external_job_id = $2, started_at = NOW(), updated_at = NOW()
       WHERE id = $1"
    None ->
      "UPDATE ai_jobs
       SET status = 'processing', started_at = NOW(), updated_at = NOW()
       WHERE id = $1"
  }

  let params = case external_id {
    Some(ext_id) -> [pog.text(job_id), pog.text(ext_id)]
    None -> [pog.text(job_id)]
  }

  case pog.query(sql) |> add_parameters(params) |> pog.execute(pool) {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Complete job with output
pub fn complete_job(
  pool: DbPool,
  job_id: String,
  output_data: String,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE ai_jobs
     SET status = 'completed', output_data = $2::jsonb, updated_at = NOW(), completed_at = NOW()
     WHERE id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.text(job_id), pog.text(output_data)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Fail job with error
pub fn fail_job(
  pool: DbPool,
  job_id: String,
  error_message: String,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE ai_jobs
     SET status = 'failed', error_message = $2, updated_at = NOW(), completed_at = NOW()
     WHERE id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.text(job_id), pog.text(error_message)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Complete job by external ID (for webhooks)
pub fn complete_job_by_external_id(
  pool: DbPool,
  external_id: String,
  output_data: String,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE ai_jobs
     SET status = 'completed', output_data = $2::jsonb, updated_at = NOW(), completed_at = NOW()
     WHERE external_job_id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.text(external_id), pog.text(output_data)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Fail job by external ID (for webhooks)
pub fn fail_job_by_external_id(
  pool: DbPool,
  external_id: String,
  error_message: String,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE ai_jobs
     SET status = 'failed', error_message = $2, updated_at = NOW(), completed_at = NOW()
     WHERE external_job_id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.text(external_id), pog.text(error_message)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Update job status to processing by external ID (for webhooks)
pub fn update_status_by_external_id(
  pool: DbPool,
  external_id: String,
  status: JobStatus,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE ai_jobs
     SET status = $2, updated_at = NOW()
     WHERE external_job_id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.text(external_id), pog.text(job_status_to_string(status))])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Cancel job
pub fn cancel_job(pool: DbPool, job_id: String) -> Result(Nil, DbError) {
  let sql =
    "UPDATE ai_jobs
     SET status = 'cancelled', updated_at = NOW(), completed_at = NOW()
     WHERE id = $1"

  case pog.query(sql) |> add_parameters([pog.text(job_id)]) |> pog.execute(pool) {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get pending jobs for processing (used by worker/scheduler)
pub fn get_pending_jobs(
  pool: DbPool,
  job_type: Option(JobType),
  limit: Int,
) -> Result(List(AiJob), DbError) {
  let #(sql, params) = case job_type {
    Some(jt) -> #(
      "SELECT id, job_type, status, user_id, input_data::text, output_data::text,
              error_message, external_job_id, created_at::text, updated_at::text, completed_at::text
       FROM ai_jobs
       WHERE status = 'pending' AND job_type = $1
       ORDER BY created_at ASC LIMIT $2",
      [pog.text(job_type_to_string(jt)), pog.int(limit)],
    )
    None -> #(
      "SELECT id, job_type, status, user_id, input_data::text, output_data::text,
              error_message, external_job_id, created_at::text, updated_at::text, completed_at::text
       FROM ai_jobs
       WHERE status = 'pending'
       ORDER BY created_at ASC LIMIT $1",
      [pog.int(limit)],
    )
  }

  case
    pog.query(sql)
    |> add_parameters(params)
    |> pog.returning(decode_ai_job())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, jobs)) -> Ok(jobs)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get job count by status
pub fn count_jobs_by_status(pool: DbPool) -> Result(List(#(String, Int)), DbError) {
  let sql =
    "SELECT status, COUNT(*) as count FROM ai_jobs GROUP BY status ORDER BY status"

  let decoder = {
    use status <- decode.field(0, decode.string)
    use count <- decode.field(1, decode.int)
    decode.success(#(status, count))
  }

  case pog.query(sql) |> pog.returning(decoder) |> pog.execute(pool) {
    Ok(pog.Returned(_, counts)) -> Ok(counts)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

fn job_type_to_string(jt: JobType) -> String {
  case jt {
    JobElevenLabsTTS -> "elevenlabs_tts"
    JobElevenLabsVoiceClone -> "elevenlabs_voice_clone"
    JobHedraAvatar -> "hedra_avatar"
    JobHeyGenVideo -> "heygen_video"
    JobBflImage -> "bfl_image"
    JobKlingVideo -> "kling_video"
    JobKlingImageToVideo -> "kling_image_to_video"
  }
}

fn string_to_job_type(s: String) -> JobType {
  case s {
    "elevenlabs_tts" -> JobElevenLabsTTS
    "elevenlabs_voice_clone" -> JobElevenLabsVoiceClone
    "hedra_avatar" -> JobHedraAvatar
    "heygen_video" -> JobHeyGenVideo
    "bfl_image" -> JobBflImage
    "kling_video" -> JobKlingVideo
    "kling_image_to_video" -> JobKlingImageToVideo
    _ -> JobElevenLabsTTS
  }
}

fn job_status_to_string(js: JobStatus) -> String {
  case js {
    JobPending -> "pending"
    JobProcessing -> "processing"
    JobCompleted -> "completed"
    JobFailed -> "failed"
    JobCancelled -> "cancelled"
  }
}

fn string_to_job_status(s: String) -> JobStatus {
  case s {
    "pending" -> JobPending
    "processing" -> JobProcessing
    "completed" -> JobCompleted
    "failed" -> JobFailed
    "cancelled" -> JobCancelled
    _ -> JobPending
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

// =============================================================================
// Decoder
// =============================================================================

fn decode_ai_job() -> Decoder(AiJob) {
  use id <- decode.field(0, decode.string)
  use job_type <- decode.field(1, decode.string)
  use status <- decode.field(2, decode.string)
  use user_id <- decode.field(3, decode.optional(decode.int))
  use input_data <- decode.field(4, decode.string)
  use output_data <- decode.field(5, decode.optional(decode.string))
  use error_message <- decode.field(6, decode.optional(decode.string))
  use external_id <- decode.field(7, decode.optional(decode.string))
  use created_at <- decode.field(8, decode.string)
  use updated_at <- decode.field(9, decode.string)
  use completed_at <- decode.field(10, decode.optional(decode.string))
  decode.success(AiJob(
    id: id,
    job_type: string_to_job_type(job_type),
    status: string_to_job_status(status),
    user_id: user_id,
    input_data: input_data,
    output_data: output_data,
    error_message: error_message,
    external_id: external_id,
    created_at: created_at,
    updated_at: updated_at,
    completed_at: completed_at,
  ))
}
