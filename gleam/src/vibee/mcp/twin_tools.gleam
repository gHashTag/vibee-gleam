// Twin Tools - MCP инструменты для управления Digital Twin (ElizaOS-совместимые)
// Централизованная конфигурация персонажа с hot-reload

import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/config/twin_config.{
  type TwinConfig, type TwinConfigError, type TwinSettings, type TwinStyle,
  MessageExample, TwinConfig, TwinNotFound, TwinSettings, TwinStyle,
}
import vibee/db/postgres
import vibee/mcp/config
import vibee/mcp/protocol
import vibee/mcp/types.{type Tool, type ToolResult, Tool}

// =============================================================================
// Tool Definitions
// =============================================================================

/// Tool: twin_config_get - получить конфигурацию Digital Twin
pub fn twin_config_get_tool() -> Tool {
  Tool(
    name: "twin_config_get",
    description: "Получить текущую конфигурацию Digital Twin (ElizaOS Character формат). Возвращает name, bio, style, settings и примеры диалогов.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
    ]),
  )
}

/// Tool: twin_config_set - обновить поле конфигурации
pub fn twin_config_set_tool() -> Tool {
  Tool(
    name: "twin_config_set",
    description: "Обновить поле конфигурации Digital Twin. Изменения применяются немедленно (hot-reload).",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "field",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array([
                "name", "username", "bio", "adjectives", "topics",
                "style", "message_examples", "post_examples", "knowledge",
                "settings", "system_prompt", "plugins",
              ], json.string)),
              #("description", json.string("Поле для обновления")),
            ]),
          ),
          #(
            "value",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Новое значение (строка, массив через запятую, или JSON)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["field", "value"], json.string)),
    ]),
  )
}

/// Tool: twin_style_update - обновить стиль общения
pub fn twin_style_update_tool() -> Tool {
  Tool(
    name: "twin_style_update",
    description: "Обновить стиль общения Digital Twin (tone, language, правила чата).",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "tone",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array(["friendly", "professional", "casual", "playful"], json.string)),
              #("description", json.string("Тон общения")),
            ]),
          ),
          #(
            "language",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array(["ru", "en", "mixed"], json.string)),
              #("description", json.string("Язык общения")),
            ]),
          ),
          #(
            "all_rules",
            json.object([
              #("type", json.string("array")),
              #("items", json.object([#("type", json.string("string"))])),
              #("description", json.string("Общие правила стиля (например: 'без эмодзи', 'краткие ответы')")),
            ]),
          ),
          #(
            "chat_rules",
            json.object([
              #("type", json.string("array")),
              #("items", json.object([#("type", json.string("string"))])),
              #("description", json.string("Правила для чата (например: 'разнообразные приветствия')")),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

/// Tool: twin_examples_add - добавить пример диалога
pub fn twin_examples_add_tool() -> Tool {
  Tool(
    name: "twin_examples_add",
    description: "Добавить пример диалога для обучения персонажа. Формат ElizaOS messageExamples.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "user_message",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Сообщение пользователя")),
            ]),
          ),
          #(
            "twin_response",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Ответ Digital Twin")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["user_message", "twin_response"], json.string)),
    ]),
  )
}

/// Tool: twin_export - экспорт в ElizaOS JSON
pub fn twin_export_tool() -> Tool {
  Tool(
    name: "twin_export",
    description: "Экспортировать конфигурацию Digital Twin в ElizaOS-совместимый JSON формат.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
    ]),
  )
}

/// Tool: twin_import - импорт из ElizaOS JSON
pub fn twin_import_tool() -> Tool {
  Tool(
    name: "twin_import",
    description: "Импортировать конфигурацию из ElizaOS Character JSON. Полностью заменяет текущую конфигурацию.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "character_json",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("ElizaOS Character JSON строка")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["character_json"], json.string)),
    ]),
  )
}

/// Tool: twin_prompt_get - получить сгенерированный system prompt
pub fn twin_prompt_get_tool() -> Tool {
  Tool(
    name: "twin_prompt_get",
    description: "Получить текущий системный промпт Digital Twin, сгенерированный из конфигурации.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
    ]),
  )
}

/// Tool: twin_cache_clear - очистить кеш
pub fn twin_cache_clear_tool() -> Tool {
  Tool(
    name: "twin_cache_clear",
    description: "Очистить ETS кеш Digital Twin для немедленной перезагрузки из PostgreSQL.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
    ]),
  )
}

// =============================================================================
// Helper: Get DB connection
// =============================================================================

fn with_db(handler: fn(postgres.DbPool) -> ToolResult) -> ToolResult {
  let db_url = config.get_env_or("DATABASE_URL", "")
  case db_url {
    "" -> protocol.error_result("DATABASE_URL not set")
    url -> {
      case postgres.connect(url) {
        Error(e) -> protocol.error_result("DB error: " <> db_error_to_string(e))
        Ok(pool) -> {
          let result = handler(pool)
          postgres.disconnect(pool)
          result
        }
      }
    }
  }
}

fn db_error_to_string(err: postgres.DbError) -> String {
  case err {
    postgres.DbConnectionError(msg) -> "Connection error: " <> msg
    postgres.DbQueryError(msg) -> "Query error: " <> msg
    postgres.DbNotFound -> "Not found"
  }
}

// =============================================================================
// Tool Handlers
// =============================================================================

/// Обработчик twin_config_get
pub fn handle_twin_config_get(_args: json.Json) -> ToolResult {
  with_db(fn(pool) {
    case twin_config.get_active(pool) {
      Ok(config) -> {
        let result = json.object([
          #("id", json.string(config.id)),
          #("name", json.string(config.name)),
          #("username", case config.username {
            Some(u) -> json.string(u)
            None -> json.null()
          }),
          #("bio", json.array(config.bio, json.string)),
          #("adjectives", json.array(config.adjectives, json.string)),
          #("topics", json.array(config.topics, json.string)),
          #("style", json.object([
            #("tone", json.string(config.style.tone)),
            #("language", json.string(config.style.language)),
            #("all", json.array(config.style.all, json.string)),
            #("chat", json.array(config.style.chat, json.string)),
            #("post", json.array(config.style.post, json.string)),
          ])),
          #("message_examples_count", json.int(list.length(config.message_examples))),
          #("settings", json.object([
            #("model", json.string(config.settings.model)),
            #("temperature", json.float(config.settings.temperature)),
            #("max_tokens", json.int(config.settings.max_tokens)),
          ])),
          #("plugins", json.array(config.plugins, json.string)),
          #("is_active", json.bool(config.is_active)),
        ])
        |> json.to_string
        protocol.text_result(result)
      }
      Error(TwinNotFound) ->
        protocol.error_result("Digital Twin config not found. Run migration 008 first.")
      Error(err) ->
        protocol.error_result("Error: " <> twin_error_to_string(err))
    }
  })
}

/// Обработчик twin_config_set
pub fn handle_twin_config_set(args: json.Json) -> ToolResult {
  let field = json_get_string(args, "field") |> result.unwrap("")
  let value = json_get_string(args, "value") |> result.unwrap("")

  case field, value {
    "", _ -> protocol.error_result("field is required")
    _, "" -> protocol.error_result("value is required")
    f, v -> {
      with_db(fn(pool) {
        case twin_config.update_field(pool, f, v) {
          Ok(Nil) -> {
            let result = json.object([
              #("success", json.bool(True)),
              #("message", json.string("Field updated: " <> f)),
              #("new_value", json.string(v)),
            ])
            |> json.to_string
            protocol.text_result(result)
          }
          Error(err) ->
            protocol.error_result("Error: " <> twin_error_to_string(err))
        }
      })
    }
  }
}

/// Обработчик twin_style_update
pub fn handle_twin_style_update(args: json.Json) -> ToolResult {
  let tone = json_get_string(args, "tone") |> result.unwrap("friendly")
  let language = json_get_string(args, "language") |> result.unwrap("ru")
  let all_rules = json_get_string_list(args, "all_rules") |> result.unwrap([])
  let chat_rules = json_get_string_list(args, "chat_rules") |> result.unwrap([])

  let style = TwinStyle(
    tone: tone,
    language: language,
    all: all_rules,
    chat: chat_rules,
    post: [],
  )

  with_db(fn(pool) {
    case twin_config.update_style(pool, style) {
      Ok(Nil) -> {
        let result = json.object([
          #("success", json.bool(True)),
          #("message", json.string("Style updated")),
          #("tone", json.string(tone)),
          #("language", json.string(language)),
          #("rules_count", json.int(list.length(all_rules) + list.length(chat_rules))),
        ])
        |> json.to_string
        protocol.text_result(result)
      }
      Error(err) ->
        protocol.error_result("Error: " <> twin_error_to_string(err))
    }
  })
}

/// Обработчик twin_examples_add
pub fn handle_twin_examples_add(args: json.Json) -> ToolResult {
  let user_message = json_get_string(args, "user_message") |> result.unwrap("")
  let twin_response = json_get_string(args, "twin_response") |> result.unwrap("")

  case user_message, twin_response {
    "", _ -> protocol.error_result("user_message is required")
    _, "" -> protocol.error_result("twin_response is required")
    um, tr -> {
      let example = [
        MessageExample(name: "User", text: um),
        MessageExample(name: "Twin", text: tr),
      ]

      with_db(fn(pool) {
        case twin_config.add_message_example(pool, example) {
          Ok(Nil) -> {
            let result = json.object([
              #("success", json.bool(True)),
              #("message", json.string("Example added")),
              #("user", json.string(um)),
              #("twin", json.string(tr)),
            ])
            |> json.to_string
            protocol.text_result(result)
          }
          Error(err) ->
            protocol.error_result("Error: " <> twin_error_to_string(err))
        }
      })
    }
  }
}

/// Обработчик twin_export
pub fn handle_twin_export(_args: json.Json) -> ToolResult {
  with_db(fn(pool) {
    case twin_config.get_active(pool) {
      Ok(config) -> {
        let elizaos_json = twin_config.export_to_elizaos(config)
        protocol.text_result(elizaos_json)
      }
      Error(TwinNotFound) ->
        protocol.error_result("Digital Twin config not found")
      Error(err) ->
        protocol.error_result("Error: " <> twin_error_to_string(err))
    }
  })
}

/// Обработчик twin_import
pub fn handle_twin_import(args: json.Json) -> ToolResult {
  let character_json = json_get_string(args, "character_json") |> result.unwrap("")

  case character_json {
    "" -> protocol.error_result("character_json is required")
    cj -> {
      with_db(fn(pool) {
        case twin_config.import_from_elizaos(pool, cj) {
          Ok(Nil) -> {
            let result = json.object([
              #("success", json.bool(True)),
              #("message", json.string("ElizaOS Character imported successfully")),
            ])
            |> json.to_string
            protocol.text_result(result)
          }
          Error(err) ->
            protocol.error_result("Import error: " <> twin_error_to_string(err))
        }
      })
    }
  }
}

/// Обработчик twin_prompt_get
pub fn handle_twin_prompt_get(_args: json.Json) -> ToolResult {
  with_db(fn(pool) {
    case twin_config.get_active(pool) {
      Ok(config) -> {
        let prompt = twin_config.build_system_prompt(config)
        let result = json.object([
          #("name", json.string(config.name)),
          #("system_prompt", json.string(prompt)),
          #("prompt_length", json.int(string.length(prompt))),
        ])
        |> json.to_string
        protocol.text_result(result)
      }
      Error(TwinNotFound) ->
        protocol.error_result("Digital Twin config not found")
      Error(err) ->
        protocol.error_result("Error: " <> twin_error_to_string(err))
    }
  })
}

/// Обработчик twin_cache_clear
pub fn handle_twin_cache_clear(_args: json.Json) -> ToolResult {
  twin_config.clear_cache()
  let result = json.object([
    #("success", json.bool(True)),
    #("message", json.string("Twin config cache cleared. Next request will reload from PostgreSQL.")),
  ])
  |> json.to_string
  protocol.text_result(result)
}

// =============================================================================
// Helpers
// =============================================================================

fn twin_error_to_string(err: TwinConfigError) -> String {
  case err {
    twin_config.TwinConnectionError(msg) -> "Connection error: " <> msg
    twin_config.TwinQueryError(msg) -> "Query error: " <> msg
    twin_config.TwinNotFound -> "Twin config not found"
    twin_config.TwinInvalidJson(msg) -> "Invalid JSON: " <> msg
  }
}

// JSON helpers
fn json_get_string(j: json.Json, key: String) -> Result(String, Nil) {
  let decoder = {
    use v <- decode.field(key, decode.string)
    decode.success(v)
  }
  let json_str = json.to_string(j)
  case json.parse(json_str, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

fn json_get_string_list(j: json.Json, key: String) -> Result(List(String), Nil) {
  let decoder = {
    use v <- decode.field(key, decode.list(decode.string))
    decode.success(v)
  }
  let json_str = json.to_string(j)
  case json.parse(json_str, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

// =============================================================================
// Tool Registry
// =============================================================================

/// Получить все twin tools
pub fn get_all_tools() -> List(Tool) {
  [
    twin_config_get_tool(),
    twin_config_set_tool(),
    twin_style_update_tool(),
    twin_examples_add_tool(),
    twin_export_tool(),
    twin_import_tool(),
    twin_prompt_get_tool(),
    twin_cache_clear_tool(),
  ]
}
