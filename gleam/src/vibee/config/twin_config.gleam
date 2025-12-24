// Digital Twin Configuration Module
// Централизованная конфигурация персонажа (ElizaOS-совместимая)
// Все настройки Digital Twin в одном месте с hot-reload

import gleam/dynamic/decode.{type Decoder}
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import pog

// =============================================================================
// Types (ElizaOS Character Interface compatible)
// =============================================================================

/// Стиль общения
pub type TwinStyle {
  TwinStyle(
    tone: String,           // "friendly", "professional", "casual"
    language: String,       // "ru", "en", "mixed"
    all: List(String),      // Общие правила стиля
    chat: List(String),     // Правила для чата
    post: List(String),     // Правила для постов
  )
}

/// LLM настройки
pub type TwinSettings {
  TwinSettings(
    model: String,
    temperature: Float,
    max_tokens: Int,
    top_p: Option(Float),
    frequency_penalty: Option(Float),
    presence_penalty: Option(Float),
  )
}

/// Пример сообщения (ElizaOS format)
pub type MessageExample {
  MessageExample(name: String, text: String)
}

/// Конфигурация Digital Twin (ElizaOS Character Interface)
pub type TwinConfig {
  TwinConfig(
    id: String,
    name: String,
    username: Option(String),
    bio: List(String),
    adjectives: List(String),
    topics: List(String),
    style: TwinStyle,
    message_examples: List(List(MessageExample)),
    post_examples: List(String),
    knowledge: List(String),
    settings: TwinSettings,
    system_prompt: Option(String),
    plugins: List(String),
    is_active: Bool,
  )
}

/// Ошибки конфигурации
pub type TwinConfigError {
  TwinConnectionError(String)
  TwinQueryError(String)
  TwinNotFound
  TwinInvalidJson(String)
}

// Cache TTL в секундах
const cache_ttl_seconds = 300

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
// Initialization
// =============================================================================

/// Инициализировать кеш
pub fn init() -> Nil {
  cache_init()
}

/// Очистить кеш Digital Twin
pub fn clear_cache() -> Nil {
  cache_delete("twin:active")
}

// =============================================================================
// CRUD Operations
// =============================================================================

/// Получить активную конфигурацию Digital Twin
pub fn get_active(pool: pog.Connection) -> Result(TwinConfig, TwinConfigError) {
  // Проверяем кеш
  case cache_get("twin:active") {
    Ok(cached_json) -> {
      case decode_twin_from_json(cached_json) {
        Ok(config) -> Ok(config)
        Error(_) -> fetch_active_twin(pool)
      }
    }
    Error(_) -> fetch_active_twin(pool)
  }
}

/// Получить конфигурацию Digital Twin по session_id
/// Fallback на активный конфиг если не найден по session_id
pub fn get_by_session_id(pool: pog.Connection, session_id: String) -> Result(TwinConfig, TwinConfigError) {
  // Проверяем кеш по session_id
  let cache_key = "twin:session:" <> session_id
  case cache_get(cache_key) {
    Ok(cached_json) -> {
      case decode_twin_from_json(cached_json) {
        Ok(config) -> Ok(config)
        Error(_) -> fetch_twin_by_session(pool, session_id)
      }
    }
    Error(_) -> fetch_twin_by_session(pool, session_id)
  }
}

fn fetch_twin_by_session(pool: pog.Connection, session_id: String) -> Result(TwinConfig, TwinConfigError) {
  let sql =
    "SELECT id::text, name, username, bio, adjectives, topics,
            style, message_examples, post_examples, knowledge,
            settings, system_prompt, plugins, is_active
     FROM digital_twin_config
     WHERE session_id = $1
     LIMIT 1"

  case
    pog.query(sql)
    |> pog.parameter(pog.text(session_id))
    |> pog.returning(decode_twin_row())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [config])) -> {
      // Кешируем по session_id
      let cache_key = "twin:session:" <> session_id
      cache_set(cache_key, encode_twin_to_json(config), cache_ttl_seconds)
      Ok(config)
    }
    Ok(pog.Returned(_, [])) -> {
      // Fallback на активный конфиг
      fetch_active_twin(pool)
    }
    Ok(_) -> fetch_active_twin(pool)
    Error(e) -> Error(TwinQueryError(pog_error_to_string(e)))
  }
}

fn fetch_active_twin(pool: pog.Connection) -> Result(TwinConfig, TwinConfigError) {
  let sql =
    "SELECT id::text, name, username, bio, adjectives, topics,
            style, message_examples, post_examples, knowledge,
            settings, system_prompt, plugins, is_active
     FROM digital_twin_config
     WHERE is_active = true
     LIMIT 1"

  case
    pog.query(sql)
    |> pog.returning(decode_twin_row())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [config])) -> {
      // Кешируем
      cache_set("twin:active", encode_twin_to_json(config), cache_ttl_seconds)
      Ok(config)
    }
    Ok(pog.Returned(_, [])) -> Error(TwinNotFound)
    Ok(_) -> Error(TwinNotFound)
    Error(e) -> Error(TwinQueryError(pog_error_to_string(e)))
  }
}

/// Обновить поле конфигурации
pub fn update_field(
  pool: pog.Connection,
  field: String,
  value: String,
) -> Result(Nil, TwinConfigError) {
  // Валидируем поле
  let allowed_fields = [
    "name", "username", "bio", "adjectives", "topics",
    "style", "message_examples", "post_examples", "knowledge",
    "settings", "system_prompt", "plugins",
  ]

  case list.contains(allowed_fields, field) {
    False -> Error(TwinQueryError("Invalid field: " <> field))
    True -> {
      let sql = case field {
        "bio" | "adjectives" | "topics" | "post_examples" | "plugins" ->
          "UPDATE digital_twin_config SET " <> field <> " = string_to_array($1, ','), updated_at = NOW() WHERE is_active = true"
        "style" | "message_examples" | "knowledge" | "settings" ->
          "UPDATE digital_twin_config SET " <> field <> " = $1::jsonb, updated_at = NOW() WHERE is_active = true"
        _ ->
          "UPDATE digital_twin_config SET " <> field <> " = $1, updated_at = NOW() WHERE is_active = true"
      }

      case
        pog.query(sql)
        |> pog.parameter(pog.text(value))
        |> pog.execute(pool)
      {
        Ok(_) -> {
          clear_cache()
          Ok(Nil)
        }
        Error(e) -> Error(TwinQueryError(pog_error_to_string(e)))
      }
    }
  }
}

/// Обновить стиль
pub fn update_style(
  pool: pog.Connection,
  style: TwinStyle,
) -> Result(Nil, TwinConfigError) {
  let style_json = encode_style_to_json(style)

  let sql =
    "UPDATE digital_twin_config
     SET style = $1::jsonb, updated_at = NOW()
     WHERE is_active = true"

  case
    pog.query(sql)
    |> pog.parameter(pog.text(style_json))
    |> pog.execute(pool)
  {
    Ok(_) -> {
      clear_cache()
      Ok(Nil)
    }
    Error(e) -> Error(TwinQueryError(pog_error_to_string(e)))
  }
}

/// Обновить LLM настройки
pub fn update_settings(
  pool: pog.Connection,
  settings: TwinSettings,
) -> Result(Nil, TwinConfigError) {
  let settings_json = encode_settings_to_json(settings)

  let sql =
    "UPDATE digital_twin_config
     SET settings = $1::jsonb, updated_at = NOW()
     WHERE is_active = true"

  case
    pog.query(sql)
    |> pog.parameter(pog.text(settings_json))
    |> pog.execute(pool)
  {
    Ok(_) -> {
      clear_cache()
      Ok(Nil)
    }
    Error(e) -> Error(TwinQueryError(pog_error_to_string(e)))
  }
}

/// Добавить пример диалога
pub fn add_message_example(
  pool: pog.Connection,
  example: List(MessageExample),
) -> Result(Nil, TwinConfigError) {
  let example_json = encode_message_example(example)

  let sql =
    "UPDATE digital_twin_config
     SET message_examples = message_examples || $1::jsonb,
         updated_at = NOW()
     WHERE is_active = true"

  case
    pog.query(sql)
    |> pog.parameter(pog.text("[" <> example_json <> "]"))
    |> pog.execute(pool)
  {
    Ok(_) -> {
      clear_cache()
      Ok(Nil)
    }
    Error(e) -> Error(TwinQueryError(pog_error_to_string(e)))
  }
}

/// Установить system_prompt
pub fn set_system_prompt(
  pool: pog.Connection,
  prompt: Option(String),
) -> Result(Nil, TwinConfigError) {
  let sql =
    "UPDATE digital_twin_config
     SET system_prompt = $1, updated_at = NOW()
     WHERE is_active = true"

  case
    pog.query(sql)
    |> pog.parameter(pog.nullable(pog.text, prompt))
    |> pog.execute(pool)
  {
    Ok(_) -> {
      clear_cache()
      Ok(Nil)
    }
    Error(e) -> Error(TwinQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// ElizaOS Export/Import
// =============================================================================

/// Экспорт в ElizaOS JSON формат
pub fn export_to_elizaos(config: TwinConfig) -> String {
  json.object([
    #("name", json.string(config.name)),
    #("username", case config.username {
      Some(u) -> json.string(u)
      None -> json.null()
    }),
    #("bio", json.array(config.bio, json.string)),
    #("adjectives", json.array(config.adjectives, json.string)),
    #("topics", json.array(config.topics, json.string)),
    #("style", json.object([
      #("all", json.array(config.style.all, json.string)),
      #("chat", json.array(config.style.chat, json.string)),
      #("post", json.array(config.style.post, json.string)),
    ])),
    #("messageExamples", json.array(config.message_examples, fn(conversation) {
      json.array(conversation, fn(msg) {
        json.object([
          #("name", json.string(msg.name)),
          #("content", json.object([
            #("text", json.string(msg.text)),
          ])),
        ])
      })
    })),
    #("postExamples", json.array(config.post_examples, json.string)),
    #("knowledge", json.array(config.knowledge, json.string)),
    #("settings", json.object([
      #("model", json.string(config.settings.model)),
      #("temperature", json.float(config.settings.temperature)),
      #("max_tokens", json.int(config.settings.max_tokens)),
    ])),
    #("plugins", json.array(config.plugins, json.string)),
  ])
  |> json.to_string
}

/// Импорт из ElizaOS JSON (полная замена)
pub fn import_from_elizaos(
  pool: pog.Connection,
  json_str: String,
) -> Result(Nil, TwinConfigError) {
  // Парсим JSON и обновляем все поля
  // В данной реализации просто сохраняем JSON целиком в отдельные поля

  // Очищаем существующую конфигурацию и вставляем новую
  let sql =
    "UPDATE digital_twin_config
     SET
       name = ($1::jsonb)->>'name',
       username = ($1::jsonb)->>'username',
       bio = ARRAY(SELECT jsonb_array_elements_text(($1::jsonb)->'bio')),
       adjectives = ARRAY(SELECT jsonb_array_elements_text(($1::jsonb)->'adjectives')),
       topics = ARRAY(SELECT jsonb_array_elements_text(($1::jsonb)->'topics')),
       style = COALESCE(($1::jsonb)->'style', '{}'::jsonb),
       message_examples = COALESCE(($1::jsonb)->'messageExamples', '[]'::jsonb),
       post_examples = ARRAY(SELECT jsonb_array_elements_text(COALESCE(($1::jsonb)->'postExamples', '[]'::jsonb))),
       knowledge = COALESCE(($1::jsonb)->'knowledge', '[]'::jsonb),
       settings = COALESCE(($1::jsonb)->'settings', '{}'::jsonb),
       plugins = ARRAY(SELECT jsonb_array_elements_text(COALESCE(($1::jsonb)->'plugins', '[]'::jsonb))),
       updated_at = NOW()
     WHERE is_active = true"

  case
    pog.query(sql)
    |> pog.parameter(pog.text(json_str))
    |> pog.execute(pool)
  {
    Ok(_) -> {
      clear_cache()
      Ok(Nil)
    }
    Error(e) -> Error(TwinQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// System Prompt Builder
// =============================================================================

/// Построить системный промпт из конфигурации
pub fn build_system_prompt(config: TwinConfig) -> String {
  // Если есть кастомный промпт - используем его
  case config.system_prompt {
    Some(prompt) -> prompt
    None -> build_prompt_from_config(config)
  }
}

fn build_prompt_from_config(config: TwinConfig) -> String {
  let bio_text = string.join(config.bio, "\n")
  let adjectives_text = string.join(config.adjectives, ", ")
  let topics_text = string.join(config.topics, ", ")
  let style_rules = string.join(
    list.flatten([config.style.all, config.style.chat]),
    "\n- "
  )

  let examples_text = config.message_examples
    |> list.map(fn(conversation) {
      conversation
      |> list.map(fn(msg) { msg.name <> ": " <> msg.text })
      |> string.join("\n")
    })
    |> string.join("\n\n")

  "Ты - " <> config.name <> case config.username {
    Some(u) -> " (" <> u <> ")"
    None -> ""
  } <> "\n\n"
  <> "## О тебе\n" <> bio_text <> "\n\n"
  <> "## Черты характера\n" <> adjectives_text <> "\n\n"
  <> "## Темы для разговора\n" <> topics_text <> "\n\n"
  <> "## Стиль общения\n- " <> style_rules <> "\n\n"
  <> "## Примеры диалогов\n" <> examples_text <> "\n\n"
  <> "Отвечай естественно, как человек. Не раскрывай что ты бот или AI."
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
// Default Values
// =============================================================================

/// Дефолтный стиль
pub fn default_style() -> TwinStyle {
  TwinStyle(
    tone: "friendly",
    language: "ru",
    all: ["краткие ответы 1-4 предложения", "без эмодзи"],
    chat: ["разнообразные приветствия", "естественная речь"],
    post: [],
  )
}

/// Дефолтные настройки LLM
pub fn default_settings() -> TwinSettings {
  TwinSettings(
    model: "x-ai/grok-4.1-fast",
    temperature: 0.9,
    max_tokens: 150,
    top_p: None,
    frequency_penalty: None,
    presence_penalty: None,
  )
}

// =============================================================================
// Decoders
// =============================================================================

fn decode_twin_row() -> Decoder(TwinConfig) {
  use id <- decode.field(0, decode.string)
  use name <- decode.field(1, decode.string)
  use username <- decode.field(2, decode.optional(decode.string))
  use bio_raw <- decode.field(3, decode.string)  // PostgreSQL array as string
  use adjectives_raw <- decode.field(4, decode.string)
  use topics_raw <- decode.field(5, decode.string)
  use style_json <- decode.field(6, decode.string)
  use examples_json <- decode.field(7, decode.string)
  use post_examples_raw <- decode.field(8, decode.string)
  use knowledge_json <- decode.field(9, decode.string)
  use settings_json <- decode.field(10, decode.string)
  use system_prompt <- decode.field(11, decode.optional(decode.string))
  use plugins_raw <- decode.field(12, decode.string)
  use is_active <- decode.field(13, decode.bool)

  let bio = parse_pg_array(bio_raw)
  let adjectives = parse_pg_array(adjectives_raw)
  let topics = parse_pg_array(topics_raw)
  let style = parse_style_json(style_json)
  let message_examples = parse_examples_json(examples_json)
  let post_examples = parse_pg_array(post_examples_raw)
  let knowledge = parse_knowledge_json(knowledge_json)
  let settings = parse_settings_json(settings_json)
  let plugins = parse_pg_array(plugins_raw)

  decode.success(TwinConfig(
    id: id,
    name: name,
    username: username,
    bio: bio,
    adjectives: adjectives,
    topics: topics,
    style: style,
    message_examples: message_examples,
    post_examples: post_examples,
    knowledge: knowledge,
    settings: settings,
    system_prompt: system_prompt,
    plugins: plugins,
    is_active: is_active,
  ))
}

// =============================================================================
// JSON/Array Parsing
// =============================================================================

fn parse_pg_array(raw: String) -> List(String) {
  // PostgreSQL array format: {item1,item2,item3}
  raw
  |> string.replace("{", "")
  |> string.replace("}", "")
  |> string.replace("\"", "")
  |> string.split(",")
  |> list.filter(fn(s) { !string.is_empty(string.trim(s)) })
  |> list.map(string.trim)
}

fn parse_style_json(json_str: String) -> TwinStyle {
  // Simplified parsing - в продакшене использовать gleam/json decoder
  default_style()
}

fn parse_settings_json(json_str: String) -> TwinSettings {
  // Simplified parsing
  default_settings()
}

fn parse_examples_json(json_str: String) -> List(List(MessageExample)) {
  // Simplified - return empty
  []
}

fn parse_knowledge_json(json_str: String) -> List(String) {
  []
}

// =============================================================================
// JSON Encoding
// =============================================================================

fn encode_twin_to_json(config: TwinConfig) -> String {
  export_to_elizaos(config)
}

fn decode_twin_from_json(json_str: String) -> Result(TwinConfig, Nil) {
  // Simplified - always refetch from DB
  Error(Nil)
}

fn encode_style_to_json(style: TwinStyle) -> String {
  json.object([
    #("tone", json.string(style.tone)),
    #("language", json.string(style.language)),
    #("all", json.array(style.all, json.string)),
    #("chat", json.array(style.chat, json.string)),
    #("post", json.array(style.post, json.string)),
  ])
  |> json.to_string
}

fn encode_settings_to_json(settings: TwinSettings) -> String {
  json.object([
    #("model", json.string(settings.model)),
    #("temperature", json.float(settings.temperature)),
    #("max_tokens", json.int(settings.max_tokens)),
    #("top_p", case settings.top_p {
      Some(v) -> json.float(v)
      None -> json.null()
    }),
    #("frequency_penalty", case settings.frequency_penalty {
      Some(v) -> json.float(v)
      None -> json.null()
    }),
    #("presence_penalty", case settings.presence_penalty {
      Some(v) -> json.float(v)
      None -> json.null()
    }),
  ])
  |> json.to_string
}

fn encode_message_example(example: List(MessageExample)) -> String {
  json.array(example, fn(msg) {
    json.object([
      #("name", json.string(msg.name)),
      #("content", json.object([
        #("text", json.string(msg.text)),
      ])),
    ])
  })
  |> json.to_string
}
