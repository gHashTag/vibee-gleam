// User Services Database Module for Digital Clone
// CRUD operations for user_services table

import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import pog

// =============================================================================
// Types
// =============================================================================

/// User Service record
pub type UserService {
  UserService(
    id: Int,
    telegram_id: Int,
    service_name: String,
    description: Option(String),
    price_range: Option(String),
    keywords: List(String),
    is_active: Bool,
    created_at: String,
  )
}

/// Service for creation (no id, timestamps)
pub type NewService {
  NewService(
    telegram_id: Int,
    service_name: String,
    description: Option(String),
    price_range: Option(String),
    keywords: List(String),
  )
}

/// Service match result (with similarity score)
pub type ServiceMatch {
  ServiceMatch(
    service: UserService,
    similarity: Float,
  )
}

/// Database error (re-export from postgres module)
pub type DbError {
  DbConnectionError(String)
  DbQueryError(String)
  DbNotFound
}

// =============================================================================
// Helper Functions
// =============================================================================

fn add_parameters(query: pog.Query(t), params: List(pog.Value)) -> pog.Query(t) {
  list.fold(params, query, fn(q, p) { pog.parameter(q, p) })
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

/// Convert keywords list to PostgreSQL array format
fn keywords_to_pg_array(keywords: List(String)) -> String {
  "{" <> string.join(keywords, ",") <> "}"
}

// =============================================================================
// CRUD Operations
// =============================================================================

/// Create a new service
pub fn create_service(
  pool: pog.Connection,
  service: NewService,
) -> Result(Int, DbError) {
  let sql =
    "INSERT INTO user_services (telegram_id, service_name, description, price_range, keywords, is_active)
     VALUES ($1, $2, $3, $4, $5, true)
     RETURNING id"

  let id_decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(id)
  }

  case
    pog.query(sql)
    |> add_parameters([
      pog.int(service.telegram_id),
      pog.text(service.service_name),
      pog.nullable(pog.text, service.description),
      pog.nullable(pog.text, service.price_range),
      pog.text(keywords_to_pg_array(service.keywords)),
    ])
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [id])) -> Ok(id)
    Ok(_) -> Error(DbQueryError("Failed to create service"))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get service by ID
pub fn get_service(
  pool: pog.Connection,
  service_id: Int,
) -> Result(UserService, DbError) {
  let sql =
    "SELECT id, telegram_id, service_name, description, price_range,
            keywords, is_active, created_at::text
     FROM user_services WHERE id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.int(service_id)])
    |> pog.returning(decode_service())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [service])) -> Ok(service)
    Ok(pog.Returned(_, [])) -> Error(DbNotFound)
    Ok(_) -> Error(DbNotFound)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// List services for a user
pub fn list_services(
  pool: pog.Connection,
  telegram_id: Int,
  active_only: Bool,
) -> Result(List(UserService), DbError) {
  let sql = case active_only {
    True ->
      "SELECT id, telegram_id, service_name, description, price_range,
              keywords, is_active, created_at::text
       FROM user_services
       WHERE telegram_id = $1 AND is_active = true
       ORDER BY created_at DESC"
    False ->
      "SELECT id, telegram_id, service_name, description, price_range,
              keywords, is_active, created_at::text
       FROM user_services
       WHERE telegram_id = $1
       ORDER BY created_at DESC"
  }

  case
    pog.query(sql)
    |> add_parameters([pog.int(telegram_id)])
    |> pog.returning(decode_service())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, services)) -> Ok(services)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Update service
pub fn update_service(
  pool: pog.Connection,
  service_id: Int,
  service_name: Option(String),
  description: Option(String),
  price_range: Option(String),
  keywords: Option(List(String)),
) -> Result(Nil, DbError) {
  // Build dynamic UPDATE query
  let updates = []
  let params = []
  let idx = 1

  let #(updates, params, idx) = case service_name {
    Some(name) -> #(
      ["service_name = $" <> int.to_string(idx), ..updates],
      [pog.text(name), ..params],
      idx + 1,
    )
    None -> #(updates, params, idx)
  }

  let #(updates, params, idx) = case description {
    Some(desc) -> #(
      ["description = $" <> int.to_string(idx), ..updates],
      [pog.text(desc), ..params],
      idx + 1,
    )
    None -> #(updates, params, idx)
  }

  let #(updates, params, idx) = case price_range {
    Some(price) -> #(
      ["price_range = $" <> int.to_string(idx), ..updates],
      [pog.text(price), ..params],
      idx + 1,
    )
    None -> #(updates, params, idx)
  }

  let #(updates, params, idx) = case keywords {
    Some(kw) -> #(
      ["keywords = $" <> int.to_string(idx), ..updates],
      [pog.text(keywords_to_pg_array(kw)), ..params],
      idx + 1,
    )
    None -> #(updates, params, idx)
  }

  case list.length(updates) {
    0 -> Ok(Nil)
    _ -> {
      let sql =
        "UPDATE user_services SET "
        <> string.join(list.reverse(updates), ", ")
        <> ", updated_at = NOW() WHERE id = $"
        <> int.to_string(idx)

      let all_params = list.reverse([pog.int(service_id), ..params])

      case pog.query(sql) |> add_parameters(all_params) |> pog.execute(pool) {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
      }
    }
  }
}

/// Delete service (soft delete - set is_active = false)
pub fn deactivate_service(
  pool: pog.Connection,
  service_id: Int,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE user_services SET is_active = false, updated_at = NOW() WHERE id = $1"

  case pog.query(sql) |> add_parameters([pog.int(service_id)]) |> pog.execute(pool) {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Permanently delete service
pub fn delete_service(
  pool: pog.Connection,
  service_id: Int,
) -> Result(Nil, DbError) {
  let sql = "DELETE FROM user_services WHERE id = $1"

  case pog.query(sql) |> add_parameters([pog.int(service_id)]) |> pog.execute(pool) {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Semantic Search for Services
// =============================================================================

/// Update service embedding
pub fn update_service_embedding(
  pool: pog.Connection,
  service_id: Int,
  embedding: List(Float),
) -> Result(Nil, DbError) {
  let embedding_str =
    "["
    <> {
      embedding
      |> list.map(float_to_string)
      |> string.join(",")
    }
    <> "]"

  let sql =
    "UPDATE user_services SET embedding = $2::vector, updated_at = NOW() WHERE id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.int(service_id), pog.text(embedding_str)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Match services by semantic similarity
pub fn match_services(
  pool: pog.Connection,
  telegram_id: Int,
  query_embedding: List(Float),
  limit: Int,
) -> Result(List(ServiceMatch), DbError) {
  let embedding_str =
    "["
    <> {
      query_embedding
      |> list.map(float_to_string)
      |> string.join(",")
    }
    <> "]"

  let sql =
    "SELECT id, telegram_id, service_name, description, price_range,
            keywords, is_active, created_at::text,
            1.0 - (embedding <=> $1::vector) as similarity
     FROM user_services
     WHERE telegram_id = $2 AND is_active = true AND embedding IS NOT NULL
     ORDER BY embedding <=> $1::vector
     LIMIT $3"

  case
    pog.query(sql)
    |> add_parameters([
      pog.text(embedding_str),
      pog.int(telegram_id),
      pog.int(limit),
    ])
    |> pog.returning(decode_service_match())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, matches)) -> Ok(matches)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Match services by keywords (fallback when no embedding)
pub fn match_services_by_keywords(
  pool: pog.Connection,
  telegram_id: Int,
  query_keywords: List(String),
  limit: Int,
) -> Result(List(UserService), DbError) {
  // Match any keyword
  let keywords_pattern =
    query_keywords
    |> list.map(string.lowercase)
    |> string.join("|")

  let sql =
    "SELECT id, telegram_id, service_name, description, price_range,
            keywords, is_active, created_at::text
     FROM user_services
     WHERE telegram_id = $1 AND is_active = true
       AND (
         LOWER(service_name) ~ $2
         OR LOWER(COALESCE(description, '')) ~ $2
         OR EXISTS (SELECT 1 FROM unnest(keywords) kw WHERE LOWER(kw) ~ $2)
       )
     LIMIT $3"

  case
    pog.query(sql)
    |> add_parameters([
      pog.int(telegram_id),
      pog.text(keywords_pattern),
      pog.int(limit),
    ])
    |> pog.returning(decode_service())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, services)) -> Ok(services)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Decoders
// =============================================================================

fn decode_service() -> Decoder(UserService) {
  use id <- decode.field(0, decode.int)
  use telegram_id <- decode.field(1, decode.int)
  use service_name <- decode.field(2, decode.string)
  use description <- decode.field(3, decode.optional(decode.string))
  use price_range <- decode.field(4, decode.optional(decode.string))
  use keywords_raw <- decode.field(5, decode.optional(decode.string))
  use is_active <- decode.field(6, decode.bool)
  use created_at <- decode.field(7, decode.string)

  // Parse PostgreSQL array format {item1,item2} to List
  let keywords = case keywords_raw {
    Some(raw) -> parse_pg_array(raw)
    None -> []
  }

  decode.success(UserService(
    id: id,
    telegram_id: telegram_id,
    service_name: service_name,
    description: description,
    price_range: price_range,
    keywords: keywords,
    is_active: is_active,
    created_at: created_at,
  ))
}

fn decode_service_match() -> Decoder(ServiceMatch) {
  use id <- decode.field(0, decode.int)
  use telegram_id <- decode.field(1, decode.int)
  use service_name <- decode.field(2, decode.string)
  use description <- decode.field(3, decode.optional(decode.string))
  use price_range <- decode.field(4, decode.optional(decode.string))
  use keywords_raw <- decode.field(5, decode.optional(decode.string))
  use is_active <- decode.field(6, decode.bool)
  use created_at <- decode.field(7, decode.string)
  use similarity <- decode.field(8, decode.float)

  let keywords = case keywords_raw {
    Some(raw) -> parse_pg_array(raw)
    None -> []
  }

  decode.success(ServiceMatch(
    service: UserService(
      id: id,
      telegram_id: telegram_id,
      service_name: service_name,
      description: description,
      price_range: price_range,
      keywords: keywords,
      is_active: is_active,
      created_at: created_at,
    ),
    similarity: similarity,
  ))
}

/// Parse PostgreSQL array format: {item1,item2,item3}
fn parse_pg_array(raw: String) -> List(String) {
  raw
  |> string.replace("{", "")
  |> string.replace("}", "")
  |> string.split(",")
  |> list.filter(fn(s) { string.length(s) > 0 })
}
