// Agent Persistence Layer
// Handles saving and loading agent state from PostgreSQL

import gleam/json
import gleam/result
import gleam/list
import gleam/int
import gleam/string
import gleam/dynamic
import gleam/option.{type Option, None, Some}
import gleam/io
import gleam/dynamic/decode.{type Decoder}
import pog
import vibee/types.{type AgentState, type Message, AgentState, Message, TextContent}
import vibee/db/postgres

/// Save agent state to database
pub fn save_state(state: AgentState) -> Result(Nil, String) {
  io.println("[PERSISTENCE] Saving state for agent: " <> state.id)
  
  // Encode state to JSON
  let config_json = encode_config(state)
  let history_json = encode_history(state.history)
  let state_json = encode_state_data(state)
  
  // Get database pool
  use pool <- result.try(
    postgres.get_global_pool()
    |> option.to_result("Database pool not initialized")
  )
  
  // Upsert agent state
  let sql = "
    INSERT INTO agent_states (agent_id, state, config, history, last_updated)
    VALUES ($1, $2::jsonb, $3::jsonb, $4::jsonb, NOW())
    ON CONFLICT (agent_id) 
    DO UPDATE SET
      state = EXCLUDED.state,
      config = EXCLUDED.config,
      history = EXCLUDED.history,
      last_updated = NOW()
  "
  
  case
    pog.query(sql)
    |> pog.parameter(pog.text(state.id))
    |> pog.parameter(pog.text(state_json))
    |> pog.parameter(pog.text(config_json))
    |> pog.parameter(pog.text(history_json))
    |> pog.execute(pool)
  {
    Ok(_) -> {
      io.println("[PERSISTENCE] ✅ State saved successfully")
      Ok(Nil)
    }
    Error(err) -> {
      let err_msg = "Failed to save state: " <> pog_error_to_string(err)
      io.println("[PERSISTENCE] ❌ " <> err_msg)
      Error(err_msg)
    }
  }
}

/// Load agent state from database
pub fn load_state(agent_id: String) -> Result(AgentState, String) {
  io.println("[PERSISTENCE] Loading state for agent: " <> agent_id)
  
  // Get database pool
  use pool <- result.try(
    postgres.get_global_pool()
    |> option.to_result("Database pool not initialized")
  )
  
  // Query agent state
  let sql = "
    SELECT state, config, history 
    FROM agent_states 
    WHERE agent_id = $1
  "
  
  case
    pog.query(sql)
    |> pog.parameter(pog.text(agent_id))
    |> pog.returning(decode_agent_state_row())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [])) -> {
      io.println("[PERSISTENCE] ℹ️  No saved state found")
      Error("No saved state found")
    }
    Ok(pog.Returned(_, [state, ..])) -> {
      io.println("[PERSISTENCE] ✅ State loaded successfully")
      Ok(state)
    }
    Ok(_) -> {
      io.println("[PERSISTENCE] ⚠️  Unexpected response")
      Error("Unexpected response")
    }
    Error(err) -> {
      let err_msg = "Failed to load state: " <> pog_error_to_string(err)
      io.println("[PERSISTENCE] ❌ " <> err_msg)
      Error(err_msg)
    }
  }
}

/// Delete agent state from database
pub fn delete_state(agent_id: String) -> Result(Nil, String) {
  io.println("[PERSISTENCE] Deleting state for agent: " <> agent_id)
  
  // Get database pool
  use pool <- result.try(
    postgres.get_global_pool()
    |> option.to_result("Database pool not initialized")
  )
  
  // Delete agent state
  let sql = "DELETE FROM agent_states WHERE agent_id = $1"
  
  case
    pog.query(sql)
    |> pog.parameter(pog.text(agent_id))
    |> pog.execute(pool)
  {
    Ok(_) -> {
      io.println("[PERSISTENCE] ✅ State deleted successfully")
      Ok(Nil)
    }
    Error(err) -> {
      let err_msg = "Failed to delete state: " <> pog_error_to_string(err)
      io.println("[PERSISTENCE] ❌ " <> err_msg)
      Error(err_msg)
    }
  }
}

/// Record agent metric
pub fn record_metric(
  agent_id: String,
  metric_type: String,
  value: Float,
  metadata: Option(String),
) -> Result(Nil, String) {
  io.println("[METRICS] " <> agent_id <> " - " <> metric_type <> ": " <> float_to_string(value))
  
  // Get database pool
  use pool <- result.try(
    postgres.get_global_pool()
    |> option.to_result("Database pool not initialized")
  )
  
  // Insert metric
  let sql = "
    INSERT INTO agent_metrics (agent_id, metric_type, value, metadata, recorded_at)
    VALUES ($1, $2, $3, $4::jsonb, NOW())
  "
  
  let metadata_json = case metadata {
    Some(m) -> m
    None -> "{}"
  }
  
  case
    pog.query(sql)
    |> pog.parameter(pog.text(agent_id))
    |> pog.parameter(pog.text(metric_type))
    |> pog.parameter(pog.float(value))
    |> pog.parameter(pog.text(metadata_json))
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(err) -> {
      let err_msg = "Failed to record metric: " <> pog_error_to_string(err)
      io.println("[METRICS] ❌ " <> err_msg)
      Error(err_msg)
    }
  }
}

fn float_to_string(f: Float) -> String {
  // Use external Erlang function for proper float formatting
  float_to_binary(f)
}

@external(erlang, "erlang", "float_to_binary")
fn float_to_binary(f: Float) -> String

/// Record agent error
pub fn record_error(
  agent_id: String,
  error_type: String,
  error_message: String,
  stack_trace: Option(String),
  context: Option(String),
) -> Result(Nil, String) {
  io.println("[ERROR] " <> agent_id <> " - " <> error_type <> ": " <> error_message)
  
  // Get database pool
  use pool <- result.try(
    postgres.get_global_pool()
    |> option.to_result("Database pool not initialized")
  )
  
  // Insert error
  let sql = "
    INSERT INTO agent_errors (agent_id, error_type, error_message, stack_trace, context, occurred_at)
    VALUES ($1, $2, $3, $4, $5::jsonb, NOW())
  "
  
  let stack_str = option.unwrap(stack_trace, "")
  let context_json = option.unwrap(context, "{}")
  
  case
    pog.query(sql)
    |> pog.parameter(pog.text(agent_id))
    |> pog.parameter(pog.text(error_type))
    |> pog.parameter(pog.text(error_message))
    |> pog.parameter(pog.text(stack_str))
    |> pog.parameter(pog.text(context_json))
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(err) -> {
      let err_msg = "Failed to record error: " <> pog_error_to_string(err)
      io.println("[ERROR] ❌ " <> err_msg)
      Error(err_msg)
    }
  }
}

// ============================================================================
// ENCODING FUNCTIONS
// ============================================================================

fn encode_config(state: AgentState) -> String {
  json.object([
    #("tone", json.string(tone_to_string(state.tone))),
    #("language", json.string(language_to_string(state.language))),
    #("history_limit", json.int(state.history_limit)),
    #("system_prompt", json.string(option.unwrap(state.system_prompt, ""))),
  ])
  |> json.to_string()
}

fn encode_history(history: List(Message)) -> String {
  json.array(history, fn(msg) {
    json.object([
      #("id", json.string(msg.id)),
      #("sender", json.string(msg.sender)),
      #("content", encode_content(msg.content)),
      #("timestamp", json.int(msg.timestamp)),
    ])
  })
  |> json.to_string()
}

fn encode_content(content: types.Content) -> json.Json {
  case content {
    TextContent(text) -> json.object([
      #("type", json.string("text")),
      #("text", json.string(text)),
    ])
    types.MediaContent(url, caption) -> json.object([
      #("type", json.string("media")),
      #("url", json.string(url)),
      #("caption", json.string(option.unwrap(caption, ""))),
    ])
  }
}

fn encode_state_data(state: AgentState) -> String {
  json.object([
    #("id", json.string(state.id)),
    #("name", json.string(state.name)),
  ])
  |> json.to_string()
}

fn tone_to_string(tone: types.Tone) -> String {
  case tone {
    types.Friendly -> "friendly"
    types.Professional -> "professional"
    types.Casual -> "casual"
    types.CustomTone(t) -> t
  }
}

fn language_to_string(language: types.Language) -> String {
  case language {
    types.En -> "en"
    types.Ru -> "ru"
    types.CustomLanguage(l) -> l
  }
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

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

// ============================================================================
// DECODING FUNCTIONS
// ============================================================================

fn decode_agent_state_row() -> Decoder(AgentState) {
  // Decoder that extracts three string columns (JSON data)
  use state_json <- decode.field(0, decode.string)
  use config_json <- decode.field(1, decode.string)
  use history_json <- decode.field(2, decode.string)
  
  // Parse JSON strings and decode to AgentState
  case parse_and_decode_state(state_json, config_json, history_json) {
    Ok(state) -> decode.success(state)
    Error(_) -> {
      // Fallback to placeholder if decoding fails
      io.println("[PERSISTENCE] ⚠️  Failed to decode state, using placeholder")
      decode.success(AgentState(
        id: "placeholder",
        name: "Placeholder Agent",
        tone: types.Friendly,
        language: types.En,
        history: [],
        history_limit: 10,
        system_prompt: None,
        messages_since_save: 0,
      ))
    }
  }
}

// Parse JSON strings and decode to AgentState
fn parse_and_decode_state(
  state_json: String,
  config_json: String,
  history_json: String,
) -> Result(AgentState, String) {
  // Parse state JSON
  use #(id, name) <- result.try(
    json.parse(state_json, decode_state_json())
    |> result.map_error(fn(_) { "Failed to parse state JSON" })
  )
  
  // Parse config JSON
  use #(tone, language, history_limit, system_prompt) <- result.try(
    json.parse(config_json, decode_config_json())
    |> result.map_error(fn(_) { "Failed to parse config JSON" })
  )
  
  // Parse history JSON
  use history <- result.try(
    json.parse(history_json, decode_history_json())
    |> result.map_error(fn(_) { "Failed to parse history JSON" })
  )
  
  // Combine into AgentState
  Ok(AgentState(
    id: id,
    name: name,
    tone: tone,
    language: language,
    history: history,
    history_limit: history_limit,
    system_prompt: system_prompt,
    messages_since_save: 0,
  ))
}

// Decode state JSON object
fn decode_state_json() -> decode.Decoder(#(String, String)) {
  use id <- decode.field("id", decode.string)
  use name <- decode.field("name", decode.string)
  decode.success(#(id, name))
}

// Decode config JSON object
fn decode_config_json() -> decode.Decoder(#(types.Tone, types.Language, Int, Option(String))) {
  use tone <- decode.field("tone", decode_tone_decoder())
  use language <- decode.field("language", decode_language_decoder())
  use limit <- decode.field("history_limit", decode.int)
  use prompt <- decode.field("system_prompt", decode.optional(decode.string))
  decode.success(#(tone, language, limit, prompt))
}

// Decode history JSON array
fn decode_history_json() -> decode.Decoder(List(Message)) {
  decode.list(decode_message_decoder())
}

// Decode a single message decoder
fn decode_message_decoder() -> decode.Decoder(Message) {
  use id <- decode.field("id", decode.string)
  use sender <- decode.field("sender", decode.string)
  use content <- decode.field("content", decode_content_decoder())
  use timestamp <- decode.field("timestamp", decode.int)
  decode.success(Message(id: id, sender: sender, content: content, timestamp: timestamp))
}

// Decode message content decoder
fn decode_content_decoder() -> decode.Decoder(types.Content) {
  use content_type <- decode.field("type", decode.string)
  
  case content_type {
    "text" -> {
      use text <- decode.field("text", decode.string)
      decode.success(TextContent(text))
    }
    "media" -> {
      use url <- decode.field("url", decode.string)
      use caption <- decode.field("caption", decode.optional(decode.string))
      decode.success(types.MediaContent(url, caption))
    }
    _ -> decode.failure(types.TextContent(""), "text or media")
  }
}

// Decode tone from string decoder
fn decode_tone_decoder() -> decode.Decoder(types.Tone) {
  use tone_str <- decode.then(decode.string)
  
  case tone_str {
    "friendly" -> decode.success(types.Friendly)
    "professional" -> decode.success(types.Professional)
    "casual" -> decode.success(types.Casual)
    custom -> decode.success(types.CustomTone(custom))
  }
}

// Decode language from string decoder
fn decode_language_decoder() -> decode.Decoder(types.Language) {
  use lang_str <- decode.then(decode.string)
  
  case lang_str {
    "en" -> decode.success(types.En)
    "ru" -> decode.success(types.Ru)
    custom -> decode.success(types.CustomLanguage(custom))
  }
}


