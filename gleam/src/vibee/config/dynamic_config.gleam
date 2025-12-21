// Dynamic Configuration Module
// Централизованное управление конфигурацией с PostgreSQL + ETS кешем
// Заменяет захардкоженные значения в trigger_chats.gleam и других файлах

import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import pog

// =============================================================================
// Types
// =============================================================================

/// Глобальная конфигурация (key-value)
pub type GlobalConfig {
  GlobalConfig(
    key: String,
    value: String,
    value_type: String,
    description: Option(String),
  )
}

/// Конфигурация trigger чата (замена trigger_chats.gleam)
pub type TriggerChatConfig {
  TriggerChatConfig(
    chat_id: Int,
    chat_name: String,
    chat_type: String,
    is_active: Bool,
    can_write: Bool,
    response_probability: Float,
    custom_triggers: List(String),
    forward_chat_id: Option(Int),
    allow_images: Bool,
    response_template: Option(String),
  )
}

/// Ошибки конфигурации
pub type ConfigError {
  ConfigConnectionError(String)
  ConfigQueryError(String)
  ConfigNotFound
  ConfigInvalidType(String)
}

// Cache TTL в секундах
const cache_ttl_seconds = 300

// =============================================================================
// ETS Cache FFI
// =============================================================================

@external(erlang, "vibee_dynamic_config_ffi", "cache_init")
fn cache_init() -> Nil

@external(erlang, "vibee_dynamic_config_ffi", "cache_get")
fn cache_get(key: String) -> Result(String, Nil)

@external(erlang, "vibee_dynamic_config_ffi", "cache_set")
fn cache_set(key: String, value: String, ttl: Int) -> Nil

@external(erlang, "vibee_dynamic_config_ffi", "cache_delete")
fn cache_delete(key: String) -> Nil

@external(erlang, "vibee_dynamic_config_ffi", "cache_clear")
fn cache_clear() -> Nil

// =============================================================================
// Initialization
// =============================================================================

/// Инициализировать кеш
pub fn init() -> Nil {
  cache_init()
}

/// Очистить весь кеш (для hot-reload)
pub fn clear_cache() -> Nil {
  cache_clear()
}

// =============================================================================
// Global Config CRUD
// =============================================================================

/// Получить значение глобальной конфигурации
pub fn get_config(
  pool: pog.Connection,
  key: String,
) -> Result(GlobalConfig, ConfigError) {
  // Проверяем кеш
  let cache_key = "global:" <> key
  case cache_get(cache_key) {
    Ok(cached_json) -> {
      case decode_global_config_json(cached_json) {
        Ok(config) -> Ok(config)
        Error(_) -> fetch_global_config(pool, key)
      }
    }
    Error(_) -> fetch_global_config(pool, key)
  }
}

fn fetch_global_config(
  pool: pog.Connection,
  key: String,
) -> Result(GlobalConfig, ConfigError) {
  let sql =
    "SELECT key, value, value_type, description
     FROM global_config WHERE key = $1"

  case
    pog.query(sql)
    |> pog.parameter(pog.text(key))
    |> pog.returning(decode_global_config())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [config])) -> {
      // Кешируем
      let cache_key = "global:" <> key
      cache_set(cache_key, encode_global_config(config), cache_ttl_seconds)
      Ok(config)
    }
    Ok(pog.Returned(_, [])) -> Error(ConfigNotFound)
    Ok(_) -> Error(ConfigNotFound)
    Error(e) -> Error(ConfigQueryError(pog_error_to_string(e)))
  }
}

/// Установить значение глобальной конфигурации
pub fn set_config(
  pool: pog.Connection,
  key: String,
  value: String,
  value_type: String,
  description: Option(String),
) -> Result(Nil, ConfigError) {
  let sql =
    "INSERT INTO global_config (key, value, value_type, description)
     VALUES ($1, $2, $3, $4)
     ON CONFLICT (key) DO UPDATE SET
       value = $2, value_type = $3, description = COALESCE($4, global_config.description),
       updated_at = NOW()"

  case
    pog.query(sql)
    |> pog.parameter(pog.text(key))
    |> pog.parameter(pog.text(value))
    |> pog.parameter(pog.text(value_type))
    |> pog.parameter(pog.nullable(pog.text, description))
    |> pog.execute(pool)
  {
    Ok(_) -> {
      // Инвалидируем кеш
      cache_delete("global:" <> key)
      Ok(Nil)
    }
    Error(e) -> Error(ConfigQueryError(pog_error_to_string(e)))
  }
}

/// Получить значение как String
pub fn get_string(pool: pog.Connection, key: String) -> Result(String, ConfigError) {
  case get_config(pool, key) {
    Ok(config) -> Ok(config.value)
    Error(e) -> Error(e)
  }
}

/// Получить значение как Int
pub fn get_int(pool: pog.Connection, key: String) -> Result(Int, ConfigError) {
  case get_config(pool, key) {
    Ok(config) -> {
      case int.parse(config.value) {
        Ok(n) -> Ok(n)
        Error(_) -> Error(ConfigInvalidType("Expected int, got: " <> config.value))
      }
    }
    Error(e) -> Error(e)
  }
}

/// Получить значение как Bool
pub fn get_bool(pool: pog.Connection, key: String) -> Result(Bool, ConfigError) {
  case get_config(pool, key) {
    Ok(config) -> {
      case string.lowercase(config.value) {
        "true" | "1" | "yes" -> Ok(True)
        "false" | "0" | "no" -> Ok(False)
        _ -> Error(ConfigInvalidType("Expected bool, got: " <> config.value))
      }
    }
    Error(e) -> Error(e)
  }
}

/// Получить owner_id (часто используемое значение)
pub fn get_owner_id(pool: pog.Connection) -> Int {
  case get_int(pool, "owner_id") {
    Ok(id) -> id
    Error(_) -> 144_022_504  // Fallback to hardcoded value
  }
}

/// Получить bridge_url
pub fn get_bridge_url(pool: pog.Connection) -> String {
  case get_string(pool, "bridge_url") {
    Ok(url) -> url
    Error(_) -> "https://vibee-telegram-bridge.fly.dev"
  }
}

/// Получить embedding_url (Ollama или OpenAI compatible)
pub fn get_embedding_url(pool: pog.Connection) -> String {
  case get_string(pool, "embedding_url") {
    Ok(url) -> url
    Error(_) -> "https://api.openai.com/v1/embeddings"  // Production fallback
  }
}

/// Получить remotion_player_url
pub fn get_remotion_player_url(pool: pog.Connection) -> String {
  case get_string(pool, "remotion_player_url") {
    Ok(url) -> url
    Error(_) -> "https://vibee-remotion.fly.dev"  // Production fallback
  }
}

/// Получить target_chats (список chat_id для мониторинга)
pub fn get_target_chats(pool: pog.Connection) -> List(String) {
  // Проверяем кеш
  case cache_get("global:target_chats") {
    Ok(cached_json) -> parse_json_string_array(cached_json)
    Error(_) -> fetch_target_chats(pool)
  }
}

fn fetch_target_chats(pool: pog.Connection) -> List(String) {
  case get_string(pool, "target_chats") {
    Ok(json_str) -> {
      cache_set("global:target_chats", json_str, cache_ttl_seconds)
      parse_json_string_array(json_str)
    }
    Error(_) -> []  // Пустой fallback, хардкод в target_chats.gleam
  }
}

fn parse_json_string_array(json_str: String) -> List(String) {
  // Простой парсер JSON массива строк: ["a", "b", "c"]
  json_str
  |> string.replace("[", "")
  |> string.replace("]", "")
  |> string.replace("\"", "")
  |> string.split(",")
  |> list.map(string.trim)
  |> list.filter(fn(s) { s != "" })
}

/// Получить все глобальные конфигурации
pub fn list_configs(
  pool: pog.Connection,
) -> Result(List(GlobalConfig), ConfigError) {
  let sql =
    "SELECT key, value, value_type, description
     FROM global_config ORDER BY key"

  case
    pog.query(sql)
    |> pog.returning(decode_global_config())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, configs)) -> Ok(configs)
    Error(e) -> Error(ConfigQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Trigger Chat Config CRUD
// =============================================================================

/// Получить все trigger chat конфигурации
pub fn get_trigger_chats(
  pool: pog.Connection,
) -> Result(List(TriggerChatConfig), ConfigError) {
  // Проверяем кеш
  case cache_get("trigger_chats:all") {
    Ok(cached_json) -> {
      case decode_trigger_chats_json(cached_json) {
        Ok(configs) -> Ok(configs)
        Error(_) -> fetch_trigger_chats(pool)
      }
    }
    Error(_) -> fetch_trigger_chats(pool)
  }
}

fn fetch_trigger_chats(
  pool: pog.Connection,
) -> Result(List(TriggerChatConfig), ConfigError) {
  let sql =
    "SELECT chat_id, chat_name, chat_type, is_active, can_write,
            response_probability, custom_triggers, forward_chat_id,
            allow_images, response_template
     FROM trigger_chat_configs WHERE is_active = true
     ORDER BY chat_name"

  case
    pog.query(sql)
    |> pog.returning(decode_trigger_chat())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, configs)) -> {
      // Кешируем
      cache_set("trigger_chats:all", encode_trigger_chats(configs), cache_ttl_seconds)
      Ok(configs)
    }
    Error(e) -> Error(ConfigQueryError(pog_error_to_string(e)))
  }
}

/// Получить конфигурацию конкретного чата
pub fn get_trigger_chat(
  pool: pog.Connection,
  chat_id: Int,
) -> Result(TriggerChatConfig, ConfigError) {
  let cache_key = "trigger_chat:" <> int.to_string(chat_id)
  case cache_get(cache_key) {
    Ok(cached_json) -> {
      case decode_trigger_chat_json(cached_json) {
        Ok(config) -> Ok(config)
        Error(_) -> fetch_trigger_chat(pool, chat_id)
      }
    }
    Error(_) -> fetch_trigger_chat(pool, chat_id)
  }
}

fn fetch_trigger_chat(
  pool: pog.Connection,
  chat_id: Int,
) -> Result(TriggerChatConfig, ConfigError) {
  let sql =
    "SELECT chat_id, chat_name, chat_type, is_active, can_write,
            response_probability, custom_triggers, forward_chat_id,
            allow_images, response_template
     FROM trigger_chat_configs WHERE chat_id = $1"

  case
    pog.query(sql)
    |> pog.parameter(pog.int(chat_id))
    |> pog.returning(decode_trigger_chat())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [config])) -> {
      let cache_key = "trigger_chat:" <> int.to_string(chat_id)
      cache_set(cache_key, encode_trigger_chat(config), cache_ttl_seconds)
      Ok(config)
    }
    Ok(pog.Returned(_, [])) -> Error(ConfigNotFound)
    Ok(_) -> Error(ConfigNotFound)
    Error(e) -> Error(ConfigQueryError(pog_error_to_string(e)))
  }
}

/// Добавить или обновить trigger chat
pub fn upsert_trigger_chat(
  pool: pog.Connection,
  config: TriggerChatConfig,
) -> Result(Nil, ConfigError) {
  let triggers_json = json.array(config.custom_triggers, json.string)
    |> json.to_string

  let sql =
    "INSERT INTO trigger_chat_configs (
       chat_id, chat_name, chat_type, is_active, can_write,
       response_probability, custom_triggers, forward_chat_id,
       allow_images, response_template
     ) VALUES ($1, $2, $3, $4, $5, $6, $7::jsonb, $8, $9, $10)
     ON CONFLICT (chat_id) DO UPDATE SET
       chat_name = $2, chat_type = $3, is_active = $4, can_write = $5,
       response_probability = $6, custom_triggers = $7::jsonb, forward_chat_id = $8,
       allow_images = $9, response_template = $10, updated_at = NOW()"

  case
    pog.query(sql)
    |> pog.parameter(pog.int(config.chat_id))
    |> pog.parameter(pog.text(config.chat_name))
    |> pog.parameter(pog.text(config.chat_type))
    |> pog.parameter(pog.bool(config.is_active))
    |> pog.parameter(pog.bool(config.can_write))
    |> pog.parameter(pog.float(config.response_probability))
    |> pog.parameter(pog.text(triggers_json))
    |> pog.parameter(pog.nullable(pog.int, config.forward_chat_id))
    |> pog.parameter(pog.bool(config.allow_images))
    |> pog.parameter(pog.nullable(pog.text, config.response_template))
    |> pog.execute(pool)
  {
    Ok(_) -> {
      // Инвалидируем кеш
      invalidate_trigger_chat_cache(config.chat_id)
      Ok(Nil)
    }
    Error(e) -> Error(ConfigQueryError(pog_error_to_string(e)))
  }
}

/// Удалить trigger chat
pub fn delete_trigger_chat(
  pool: pog.Connection,
  chat_id: Int,
) -> Result(Nil, ConfigError) {
  let sql = "DELETE FROM trigger_chat_configs WHERE chat_id = $1"

  case
    pog.query(sql)
    |> pog.parameter(pog.int(chat_id))
    |> pog.execute(pool)
  {
    Ok(_) -> {
      invalidate_trigger_chat_cache(chat_id)
      Ok(Nil)
    }
    Error(e) -> Error(ConfigQueryError(pog_error_to_string(e)))
  }
}

/// Деактивировать trigger chat (soft delete)
pub fn deactivate_trigger_chat(
  pool: pog.Connection,
  chat_id: Int,
) -> Result(Nil, ConfigError) {
  let sql =
    "UPDATE trigger_chat_configs SET is_active = false, updated_at = NOW()
     WHERE chat_id = $1"

  case
    pog.query(sql)
    |> pog.parameter(pog.int(chat_id))
    |> pog.execute(pool)
  {
    Ok(_) -> {
      invalidate_trigger_chat_cache(chat_id)
      Ok(Nil)
    }
    Error(e) -> Error(ConfigQueryError(pog_error_to_string(e)))
  }
}

fn invalidate_trigger_chat_cache(chat_id: Int) -> Nil {
  cache_delete("trigger_chat:" <> int.to_string(chat_id))
  cache_delete("trigger_chats:all")
}

// =============================================================================
// Trigger Detection (замена trigger_chats.contains_trigger)
// =============================================================================

/// Проверяет, содержит ли текст триггерное слово
pub fn contains_trigger(text: String, triggers: List(String)) -> Bool {
  let lower_text = string.lowercase(text)
  list.any(triggers, fn(trigger) {
    let lower_trigger = string.lowercase(trigger)
    string.contains(lower_text, lower_trigger)
  })
}

/// Проверяет, нужно ли отвечать на сообщение с триггером
pub fn should_respond_to_trigger(
  pool: pog.Connection,
  chat_id: Int,
  message_text: String,
) -> Bool {
  case get_trigger_chat(pool, chat_id) {
    Ok(config) -> {
      config.is_active
      && config.can_write
      && contains_trigger(message_text, config.custom_triggers)
    }
    Error(_) -> False
  }
}

/// Получает ID чата для пересылки
pub fn get_forward_chat_id(
  pool: pog.Connection,
  chat_id: Int,
) -> Result(Int, Nil) {
  case get_trigger_chat(pool, chat_id) {
    Ok(config) -> {
      case config.forward_chat_id {
        Some(id) -> Ok(id)
        None -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}

// =============================================================================
// Helpers
// =============================================================================

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
// Decoders
// =============================================================================

fn decode_global_config() -> Decoder(GlobalConfig) {
  use key <- decode.field(0, decode.string)
  use value <- decode.field(1, decode.string)
  use value_type <- decode.field(2, decode.string)
  use description <- decode.field(3, decode.optional(decode.string))

  decode.success(GlobalConfig(
    key: key,
    value: value,
    value_type: value_type,
    description: description,
  ))
}

fn decode_trigger_chat() -> Decoder(TriggerChatConfig) {
  use chat_id <- decode.field(0, decode.int)
  use chat_name <- decode.field(1, decode.string)
  use chat_type <- decode.field(2, decode.string)
  use is_active <- decode.field(3, decode.bool)
  use can_write <- decode.field(4, decode.bool)
  use response_probability <- decode.field(5, decode.float)
  use triggers_json <- decode.field(6, decode.string)
  use forward_chat_id <- decode.field(7, decode.optional(decode.int))
  use allow_images <- decode.field(8, decode.bool)
  use response_template <- decode.field(9, decode.optional(decode.string))

  // Parse triggers from JSON string
  let triggers = parse_triggers_json(triggers_json)

  decode.success(TriggerChatConfig(
    chat_id: chat_id,
    chat_name: chat_name,
    chat_type: chat_type,
    is_active: is_active,
    can_write: can_write,
    response_probability: response_probability,
    custom_triggers: triggers,
    forward_chat_id: forward_chat_id,
    allow_images: allow_images,
    response_template: response_template,
  ))
}

// =============================================================================
// JSON Encoding/Decoding for Cache
// =============================================================================

fn encode_global_config(config: GlobalConfig) -> String {
  json.object([
    #("key", json.string(config.key)),
    #("value", json.string(config.value)),
    #("value_type", json.string(config.value_type)),
    #("description", case config.description {
      Some(d) -> json.string(d)
      None -> json.null()
    }),
  ])
  |> json.to_string
}

fn decode_global_config_json(json_str: String) -> Result(GlobalConfig, Nil) {
  // Simplified JSON parsing - in production use proper JSON decoder
  Error(Nil)
}

fn encode_trigger_chat(config: TriggerChatConfig) -> String {
  json.object([
    #("chat_id", json.int(config.chat_id)),
    #("chat_name", json.string(config.chat_name)),
    #("chat_type", json.string(config.chat_type)),
    #("is_active", json.bool(config.is_active)),
    #("can_write", json.bool(config.can_write)),
    #("response_probability", json.float(config.response_probability)),
    #("custom_triggers", json.array(config.custom_triggers, json.string)),
    #("forward_chat_id", case config.forward_chat_id {
      Some(id) -> json.int(id)
      None -> json.null()
    }),
    #("allow_images", json.bool(config.allow_images)),
    #("response_template", case config.response_template {
      Some(t) -> json.string(t)
      None -> json.null()
    }),
  ])
  |> json.to_string
}

fn encode_trigger_chats(configs: List(TriggerChatConfig)) -> String {
  json.array(configs, fn(c) {
    json.object([
      #("chat_id", json.int(c.chat_id)),
      #("chat_name", json.string(c.chat_name)),
      #("chat_type", json.string(c.chat_type)),
      #("is_active", json.bool(c.is_active)),
      #("can_write", json.bool(c.can_write)),
      #("response_probability", json.float(c.response_probability)),
      #("custom_triggers", json.array(c.custom_triggers, json.string)),
      #("forward_chat_id", case c.forward_chat_id {
        Some(id) -> json.int(id)
        None -> json.null()
      }),
      #("allow_images", json.bool(c.allow_images)),
      #("response_template", case c.response_template {
        Some(t) -> json.string(t)
        None -> json.null()
      }),
    ])
  })
  |> json.to_string
}

fn decode_trigger_chat_json(json_str: String) -> Result(TriggerChatConfig, Nil) {
  // Simplified - always refetch from DB
  Error(Nil)
}

fn decode_trigger_chats_json(json_str: String) -> Result(List(TriggerChatConfig), Nil) {
  // Simplified - always refetch from DB
  Error(Nil)
}

fn parse_triggers_json(json_str: String) -> List(String) {
  // Parse JSON array of strings
  // For simplicity, use string manipulation
  let cleaned = json_str
    |> string.replace("[", "")
    |> string.replace("]", "")
    |> string.replace("\"", "")

  case string.is_empty(cleaned) {
    True -> []
    False -> string.split(cleaned, ",")
      |> list.map(string.trim)
  }
}
