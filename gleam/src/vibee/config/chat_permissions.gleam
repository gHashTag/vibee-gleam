// Chat Permissions Module
// Централизованное управление правами чатов (whitelist подход)
// По умолчанию агент молчит, отвечает только в разрешённых чатах

import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import pog

// =============================================================================
// Types
// =============================================================================

/// Уровни доступа
pub type PermissionLevel {
  Owner       // Полный контроль + управление правами
  Admin       // Все действия + изменение настроек чата
  User        // Стандартное взаимодействие
  ObserveOnly // Только чтение, агент НЕ отвечает
  Blocked     // Полностью заблокирован
}

/// Тип чата
pub type ChatType {
  Private    // id > 0
  Group      // id < 0, без -100
  Supergroup // id < 0, с -100
  Channel
}

/// Права чата
pub type ChatPermission {
  ChatPermission(
    chat_id: Int,
    chat_type: ChatType,
    permission_level: PermissionLevel,
    chat_name: Option(String),
    username: Option(String),
    can_respond: Bool,
    can_initiate: Bool,
    use_triggers: Bool,
    trigger_config_id: Option(Int),
    forward_to_chat_id: Option(Int),
    granted_by: Option(Int),
    expires_at: Option(String),
    notes: Option(String),
  )
}

/// Результат проверки прав
pub type PermissionCheckResult {
  Allowed(permission: ChatPermission)
  Denied(reason: DenialReason)
  Unknown(chat_id: Int)
}

/// Причина отказа
pub type DenialReason {
  ChatBlocked
  ChatNotWhitelisted
  PermissionExpired
  ObserveOnlyMode
}

/// Pending review (неизвестный чат)
pub type PendingReview {
  PendingReview(
    chat_id: Int,
    chat_type: ChatType,
    first_contact_at: String,
    last_message_at: String,
    message_count: Int,
    sample_message: Option(String),
    from_user_id: Option(Int),
    from_username: Option(String),
    from_name: Option(String),
    reviewed: Bool,
  )
}

/// Ошибки
pub type PermissionError {
  PermissionConnectionError(String)
  PermissionQueryError(String)
  PermissionNotFound
  InvalidPermissionLevel(String)
}

// Cache TTL (короче чем global config для быстрой реакции)
const cache_ttl_seconds = 60

// =============================================================================
// ETS Cache FFI (использует тот же кеш что и dynamic_config)
// =============================================================================

@external(erlang, "vibee_dynamic_config_ffi", "cache_init")
fn cache_init() -> Nil

@external(erlang, "vibee_dynamic_config_ffi", "cache_get")
fn cache_get(key: String) -> Result(String, Nil)

@external(erlang, "vibee_dynamic_config_ffi", "cache_set")
fn cache_set(key: String, value: String, ttl: Int) -> Nil

@external(erlang, "vibee_dynamic_config_ffi", "cache_delete")
fn cache_delete(key: String) -> Nil

// =============================================================================
// Utility Functions
// =============================================================================

fn cache_key(chat_id: Int) -> String {
  "perm:" <> int.to_string(chat_id)
}

/// Определить тип чата по ID
pub fn detect_chat_type(chat_id: Int) -> ChatType {
  case chat_id > 0 {
    True -> Private
    False -> {
      // Supergroups начинаются с -100
      let id_str = int.to_string(int.absolute_value(chat_id))
      case string.starts_with(id_str, "100") {
        True -> Supergroup
        False -> Group
      }
    }
  }
}

/// Проверить: личный ли это чат
pub fn is_private_chat(chat_id: Int) -> Bool {
  chat_id > 0
}

/// Преобразовать строку в chat_id
pub fn parse_chat_id(chat_id_str: String) -> Int {
  case int.parse(chat_id_str) {
    Ok(id) -> id
    Error(_) -> 0
  }
}

fn permission_level_to_string(level: PermissionLevel) -> String {
  case level {
    Owner -> "owner"
    Admin -> "admin"
    User -> "user"
    ObserveOnly -> "observe_only"
    Blocked -> "blocked"
  }
}

fn string_to_permission_level(s: String) -> Result(PermissionLevel, Nil) {
  case string.lowercase(s) {
    "owner" -> Ok(Owner)
    "admin" -> Ok(Admin)
    "user" -> Ok(User)
    "observe_only" -> Ok(ObserveOnly)
    "blocked" -> Ok(Blocked)
    _ -> Error(Nil)
  }
}

fn chat_type_to_string(ct: ChatType) -> String {
  case ct {
    Private -> "private"
    Group -> "group"
    Supergroup -> "supergroup"
    Channel -> "channel"
  }
}

fn string_to_chat_type(s: String) -> ChatType {
  case string.lowercase(s) {
    "private" -> Private
    "group" -> Group
    "supergroup" -> Supergroup
    "channel" -> Channel
    _ -> Private
  }
}

fn pog_error_to_string(e: pog.QueryError) -> String {
  case e {
    pog.ConnectionUnavailable -> "Connection unavailable"
    pog.ConstraintViolated(msg, constraint, _) ->
      "Constraint violated: " <> constraint <> " - " <> msg
    pog.PostgresqlError(code, _, msg) -> "PostgreSQL error " <> code <> ": " <> msg
    pog.UnexpectedArgumentCount(_, _) -> "Unexpected argument count"
    pog.UnexpectedArgumentType(_, _) -> "Unexpected argument type"
    pog.UnexpectedResultType(_) -> "Unexpected result type"
    pog.QueryTimeout -> "Query timeout"
  }
}

// =============================================================================
// Main Permission Check (ГЛАВНАЯ ФУНКЦИЯ)
// =============================================================================

/// Проверить права на ответ в чате (с кешем)
pub fn can_respond(pool: pog.Connection, chat_id: Int) -> PermissionCheckResult {
  cache_init()

  // Проверяем кеш
  let key = cache_key(chat_id)
  case cache_get(key) {
    Ok(cached_json) -> {
      case decode_permission_json(cached_json) {
        Ok(perm) -> check_permission_result(perm)
        Error(_) -> fetch_and_check(pool, chat_id)
      }
    }
    Error(_) -> fetch_and_check(pool, chat_id)
  }
}

fn fetch_and_check(pool: pog.Connection, chat_id: Int) -> PermissionCheckResult {
  case get_permission(pool, chat_id) {
    Ok(perm) -> {
      // Кешируем
      cache_set(cache_key(chat_id), encode_permission(perm), cache_ttl_seconds)
      check_permission_result(perm)
    }
    Error(PermissionNotFound) -> Unknown(chat_id)
    Error(_) -> Unknown(chat_id)
  }
}

fn check_permission_result(perm: ChatPermission) -> PermissionCheckResult {
  case perm.permission_level {
    Blocked -> Denied(ChatBlocked)
    ObserveOnly -> Denied(ObserveOnlyMode)
    _ -> {
      case perm.can_respond {
        True -> Allowed(perm)
        False -> Denied(ChatNotWhitelisted)
      }
    }
  }
}

// =============================================================================
// CRUD Operations
// =============================================================================

/// Получить права чата из БД
pub fn get_permission(
  pool: pog.Connection,
  chat_id: Int,
) -> Result(ChatPermission, PermissionError) {
  let sql =
    "SELECT chat_id, chat_type::text, permission_level::text, chat_name, username,
            can_respond, can_initiate, use_triggers, trigger_config_id,
            forward_to_chat_id, granted_by, expires_at::text, notes
     FROM chat_permissions
     WHERE chat_id = $1"

  case
    pog.query(sql)
    |> pog.parameter(pog.int(chat_id))
    |> pog.returning(decode_permission())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [perm])) -> Ok(perm)
    Ok(pog.Returned(_, [])) -> Error(PermissionNotFound)
    Ok(_) -> Error(PermissionNotFound)
    Error(e) -> Error(PermissionQueryError(pog_error_to_string(e)))
  }
}

/// Установить права чата
pub fn set_permission(
  pool: pog.Connection,
  perm: ChatPermission,
) -> Result(Nil, PermissionError) {
  let sql =
    "INSERT INTO chat_permissions (
       chat_id, chat_type, permission_level, chat_name, username,
       can_respond, can_initiate, use_triggers, trigger_config_id,
       forward_to_chat_id, granted_by, notes
     ) VALUES ($1, $2::chat_type, $3::permission_level, $4, $5, $6, $7, $8, $9, $10, $11, $12)
     ON CONFLICT (chat_id) DO UPDATE SET
       chat_type = $2::chat_type,
       permission_level = $3::permission_level,
       chat_name = COALESCE($4, chat_permissions.chat_name),
       username = COALESCE($5, chat_permissions.username),
       can_respond = $6,
       can_initiate = $7,
       use_triggers = $8,
       trigger_config_id = $9,
       forward_to_chat_id = $10,
       granted_by = $11,
       notes = COALESCE($12, chat_permissions.notes),
       updated_at = NOW()"

  case
    pog.query(sql)
    |> pog.parameter(pog.int(perm.chat_id))
    |> pog.parameter(pog.text(chat_type_to_string(perm.chat_type)))
    |> pog.parameter(pog.text(permission_level_to_string(perm.permission_level)))
    |> pog.parameter(pog.nullable(pog.text, perm.chat_name))
    |> pog.parameter(pog.nullable(pog.text, perm.username))
    |> pog.parameter(pog.bool(perm.can_respond))
    |> pog.parameter(pog.bool(perm.can_initiate))
    |> pog.parameter(pog.bool(perm.use_triggers))
    |> pog.parameter(pog.nullable(pog.int, perm.trigger_config_id))
    |> pog.parameter(pog.nullable(pog.int, perm.forward_to_chat_id))
    |> pog.parameter(pog.nullable(pog.int, perm.granted_by))
    |> pog.parameter(pog.nullable(pog.text, perm.notes))
    |> pog.execute(pool)
  {
    Ok(_) -> {
      // Инвалидируем кеш
      invalidate_cache(perm.chat_id)
      Ok(Nil)
    }
    Error(e) -> Error(PermissionQueryError(pog_error_to_string(e)))
  }
}

/// Быстро выдать права
pub fn grant_permission(
  pool: pog.Connection,
  chat_id: Int,
  level: PermissionLevel,
  granted_by: Int,
  chat_name: Option(String),
) -> Result(Nil, PermissionError) {
  let can_respond = case level {
    Owner | Admin | User -> True
    ObserveOnly | Blocked -> False
  }

  let perm =
    ChatPermission(
      chat_id: chat_id,
      chat_type: detect_chat_type(chat_id),
      permission_level: level,
      chat_name: chat_name,
      username: None,
      can_respond: can_respond,
      can_initiate: level == Owner || level == Admin,
      use_triggers: level != Owner && level != Blocked,
      trigger_config_id: None,
      forward_to_chat_id: None,
      granted_by: Some(granted_by),
      expires_at: None,
      notes: None,
    )

  set_permission(pool, perm)
}

/// Забрать права (установить blocked)
pub fn revoke_permission(
  pool: pog.Connection,
  chat_id: Int,
  revoked_by: Int,
) -> Result(Nil, PermissionError) {
  let sql =
    "UPDATE chat_permissions
     SET permission_level = 'blocked'::permission_level,
         can_respond = false,
         can_initiate = false,
         granted_by = $2,
         notes = 'Revoked at ' || NOW()::text,
         updated_at = NOW()
     WHERE chat_id = $1"

  case
    pog.query(sql)
    |> pog.parameter(pog.int(chat_id))
    |> pog.parameter(pog.int(revoked_by))
    |> pog.execute(pool)
  {
    Ok(_) -> {
      invalidate_cache(chat_id)
      Ok(Nil)
    }
    Error(e) -> Error(PermissionQueryError(pog_error_to_string(e)))
  }
}

/// Список прав по уровню
pub fn list_by_level(
  pool: pog.Connection,
  level: PermissionLevel,
) -> Result(List(ChatPermission), PermissionError) {
  let sql =
    "SELECT chat_id, chat_type::text, permission_level::text, chat_name, username,
            can_respond, can_initiate, use_triggers, trigger_config_id,
            forward_to_chat_id, granted_by, expires_at::text, notes
     FROM chat_permissions
     WHERE permission_level = $1::permission_level
     ORDER BY chat_id"

  case
    pog.query(sql)
    |> pog.parameter(pog.text(permission_level_to_string(level)))
    |> pog.returning(decode_permission())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, perms)) -> Ok(perms)
    Error(e) -> Error(PermissionQueryError(pog_error_to_string(e)))
  }
}

/// Список всех активных (can_respond = true)
pub fn list_active(
  pool: pog.Connection,
) -> Result(List(ChatPermission), PermissionError) {
  let sql =
    "SELECT chat_id, chat_type::text, permission_level::text, chat_name, username,
            can_respond, can_initiate, use_triggers, trigger_config_id,
            forward_to_chat_id, granted_by, expires_at::text, notes
     FROM chat_permissions
     WHERE can_respond = true
     ORDER BY chat_id"

  case
    pog.query(sql)
    |> pog.returning(decode_permission())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, perms)) -> Ok(perms)
    Error(e) -> Error(PermissionQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Pending Reviews (неизвестные чаты)
// =============================================================================

/// Залогировать неизвестный чат для review
pub fn log_unknown_chat(
  pool: pog.Connection,
  chat_id: Int,
  from_user_id: Int,
  from_name: String,
  from_username: Option(String),
  sample_text: String,
) -> Nil {
  let chat_type = detect_chat_type(chat_id)

  let sql =
    "INSERT INTO pending_chat_reviews (
       chat_id, chat_type, from_user_id, from_name, from_username, sample_message
     ) VALUES ($1, $2::chat_type, $3, $4, $5, $6)
     ON CONFLICT (chat_id) DO UPDATE SET
       last_message_at = NOW(),
       message_count = pending_chat_reviews.message_count + 1,
       sample_message = $6"

  let _ =
    pog.query(sql)
    |> pog.parameter(pog.int(chat_id))
    |> pog.parameter(pog.text(chat_type_to_string(chat_type)))
    |> pog.parameter(pog.int(from_user_id))
    |> pog.parameter(pog.text(from_name))
    |> pog.parameter(pog.nullable(pog.text, from_username))
    |> pog.parameter(pog.text(string.slice(sample_text, 0, 500)))
    |> pog.execute(pool)

  Nil
}

/// Получить список pending reviews
pub fn get_pending_reviews(
  pool: pog.Connection,
  limit: Int,
) -> Result(List(PendingReview), PermissionError) {
  let sql =
    "SELECT chat_id, chat_type::text, first_contact_at::text, last_message_at::text,
            message_count, sample_message, from_user_id, from_username, from_name, reviewed
     FROM pending_chat_reviews
     WHERE reviewed = false
     ORDER BY message_count DESC, last_message_at DESC
     LIMIT $1"

  case
    pog.query(sql)
    |> pog.parameter(pog.int(limit))
    |> pog.returning(decode_pending_review())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, reviews)) -> Ok(reviews)
    Error(e) -> Error(PermissionQueryError(pog_error_to_string(e)))
  }
}

/// Пометить review как обработанный
pub fn mark_reviewed(
  pool: pog.Connection,
  chat_id: Int,
  reviewed_by: Int,
  action: String,
) -> Result(Nil, PermissionError) {
  let sql =
    "UPDATE pending_chat_reviews
     SET reviewed = true, reviewed_at = NOW(), reviewed_by = $2, review_action = $3
     WHERE chat_id = $1"

  case
    pog.query(sql)
    |> pog.parameter(pog.int(chat_id))
    |> pog.parameter(pog.int(reviewed_by))
    |> pog.parameter(pog.text(action))
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(PermissionQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Cache Management
// =============================================================================

/// Инвалидировать кеш для чата
pub fn invalidate_cache(chat_id: Int) -> Nil {
  cache_delete(cache_key(chat_id))
}

/// Очистить весь кеш прав (для миграции)
pub fn clear_all_cache() -> Nil {
  // Кеш общий с dynamic_config, нельзя очищать всё
  // Вместо этого очищаем по одному при изменении
  Nil
}

// =============================================================================
// Decoders
// =============================================================================

fn decode_permission() -> Decoder(ChatPermission) {
  use chat_id <- decode.field(0, decode.int)
  use chat_type_str <- decode.field(1, decode.string)
  use level_str <- decode.field(2, decode.string)
  use chat_name <- decode.field(3, decode.optional(decode.string))
  use username <- decode.field(4, decode.optional(decode.string))
  use can_respond <- decode.field(5, decode.bool)
  use can_initiate <- decode.field(6, decode.bool)
  use use_triggers <- decode.field(7, decode.bool)
  use trigger_config_id <- decode.field(8, decode.optional(decode.int))
  use forward_to_chat_id <- decode.field(9, decode.optional(decode.int))
  use granted_by <- decode.field(10, decode.optional(decode.int))
  use expires_at <- decode.field(11, decode.optional(decode.string))
  use notes <- decode.field(12, decode.optional(decode.string))

  let level = case string_to_permission_level(level_str) {
    Ok(l) -> l
    Error(_) -> Blocked
  }

  decode.success(ChatPermission(
    chat_id: chat_id,
    chat_type: string_to_chat_type(chat_type_str),
    permission_level: level,
    chat_name: chat_name,
    username: username,
    can_respond: can_respond,
    can_initiate: can_initiate,
    use_triggers: use_triggers,
    trigger_config_id: trigger_config_id,
    forward_to_chat_id: forward_to_chat_id,
    granted_by: granted_by,
    expires_at: expires_at,
    notes: notes,
  ))
}

fn decode_pending_review() -> Decoder(PendingReview) {
  use chat_id <- decode.field(0, decode.int)
  use chat_type_str <- decode.field(1, decode.string)
  use first_contact_at <- decode.field(2, decode.string)
  use last_message_at <- decode.field(3, decode.string)
  use message_count <- decode.field(4, decode.int)
  use sample_message <- decode.field(5, decode.optional(decode.string))
  use from_user_id <- decode.field(6, decode.optional(decode.int))
  use from_username <- decode.field(7, decode.optional(decode.string))
  use from_name <- decode.field(8, decode.optional(decode.string))
  use reviewed <- decode.field(9, decode.bool)

  decode.success(PendingReview(
    chat_id: chat_id,
    chat_type: string_to_chat_type(chat_type_str),
    first_contact_at: first_contact_at,
    last_message_at: last_message_at,
    message_count: message_count,
    sample_message: sample_message,
    from_user_id: from_user_id,
    from_username: from_username,
    from_name: from_name,
    reviewed: reviewed,
  ))
}

// =============================================================================
// JSON Encoding/Decoding (для кеша)
// =============================================================================

fn encode_permission(perm: ChatPermission) -> String {
  json.object([
    #("chat_id", json.int(perm.chat_id)),
    #("chat_type", json.string(chat_type_to_string(perm.chat_type))),
    #("permission_level", json.string(permission_level_to_string(perm.permission_level))),
    #("chat_name", case perm.chat_name {
      Some(n) -> json.string(n)
      None -> json.null()
    }),
    #("can_respond", json.bool(perm.can_respond)),
    #("can_initiate", json.bool(perm.can_initiate)),
    #("use_triggers", json.bool(perm.use_triggers)),
  ])
  |> json.to_string
}

fn decode_permission_json(json_str: String) -> Result(ChatPermission, Nil) {
  let decoder = {
    use chat_id <- decode.field(0, decode.int)
    use chat_type_str <- decode.field(1, decode.string)
    use level_str <- decode.field(2, decode.string)
    use chat_name <- decode.field(3, decode.optional(decode.string))
    use can_respond <- decode.field(4, decode.bool)
    use can_initiate <- decode.field(5, decode.bool)
    use use_triggers <- decode.field(6, decode.bool)

    let level = case string_to_permission_level(level_str) {
      Ok(l) -> l
      Error(_) -> Blocked
    }

    decode.success(ChatPermission(
      chat_id: chat_id,
      chat_type: string_to_chat_type(chat_type_str),
      permission_level: level,
      chat_name: chat_name,
      username: None,
      can_respond: can_respond,
      can_initiate: can_initiate,
      use_triggers: use_triggers,
      trigger_config_id: None,
      forward_to_chat_id: None,
      granted_by: None,
      expires_at: None,
      notes: None,
    ))
  }

  case json.parse(json_str, decoder) {
    Ok(perm) -> Ok(perm)
    Error(_) -> Error(Nil)
  }
}

// =============================================================================
// Public Helper for Permission Level Parsing
// =============================================================================

/// Преобразовать строку в уровень доступа (для MCP tools)
pub fn parse_permission_level(s: String) -> Result(PermissionLevel, Nil) {
  string_to_permission_level(s)
}

/// Получить строковое представление уровня
pub fn level_to_string(level: PermissionLevel) -> String {
  permission_level_to_string(level)
}
