// Config Tools - MCP инструменты для управления динамической конфигурацией
// Hot-reload конфигурации без редеплоя

import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import vibee/config/dynamic_config.{
  type ConfigError, type GlobalConfig, type TriggerChatConfig,
  ConfigNotFound, GlobalConfig, TriggerChatConfig,
}
import vibee/db/postgres
import vibee/mcp/config
import vibee/mcp/protocol
import vibee/mcp/types.{type Tool, type ToolResult, Tool}

// =============================================================================
// Tool Definitions
// =============================================================================

/// Tool: config_get - получить значение глобальной конфигурации
pub fn config_get_tool() -> Tool {
  Tool(
    name: "config_get",
    description: "Получить значение глобальной конфигурации по ключу. Возвращает key, value, value_type и description.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "key",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Ключ конфигурации (например: owner_id, bridge_url, llm_model)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["key"], json.string)),
    ]),
  )
}

/// Tool: config_set - установить значение глобальной конфигурации
pub fn config_set_tool() -> Tool {
  Tool(
    name: "config_set",
    description: "Установить или обновить значение глобальной конфигурации. Изменения применяются немедленно (hot-reload).",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "key",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Ключ конфигурации")),
            ]),
          ),
          #(
            "value",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Значение (строка, число или JSON)")),
            ]),
          ),
          #(
            "value_type",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array(["string", "int", "float", "bool", "json"], json.string)),
              #("description", json.string("Тип значения (default: string)")),
              #("default", json.string("string")),
            ]),
          ),
          #(
            "description",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Описание параметра (опционально)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["key", "value"], json.string)),
    ]),
  )
}

/// Tool: config_list - список всех глобальных конфигураций
pub fn config_list_tool() -> Tool {
  Tool(
    name: "config_list",
    description: "Получить список всех глобальных конфигураций.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
    ]),
  )
}

/// Tool: trigger_chat_list - список всех trigger чатов
pub fn trigger_chat_list_tool() -> Tool {
  Tool(
    name: "trigger_chat_list",
    description: "Получить список всех trigger чатов с их настройками.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
    ]),
  )
}

/// Tool: trigger_chat_add - добавить или обновить trigger чат
pub fn trigger_chat_add_tool() -> Tool {
  Tool(
    name: "trigger_chat_add",
    description: "Добавить новый trigger чат или обновить существующий. Для supergroup чатов используйте отрицательный ID с префиксом -100.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "chat_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("ID чата (для supergroup: -100XXXXXXXXXX)")),
            ]),
          ),
          #(
            "chat_name",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Название чата для отображения")),
            ]),
          ),
          #(
            "triggers",
            json.object([
              #("type", json.string("array")),
              #("items", json.object([#("type", json.string("string"))])),
              #("description", json.string("Список триггерных слов")),
            ]),
          ),
          #(
            "forward_chat_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("ID чата для пересылки диалогов (lead группа)")),
            ]),
          ),
          #(
            "response_template",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Шаблон ответа на триггер")),
            ]),
          ),
          #(
            "use_preset",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Использовать preset триггеров (например: crypto_exchange)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["chat_id", "chat_name"], json.string)),
    ]),
  )
}

/// Tool: trigger_chat_remove - удалить trigger чат
pub fn trigger_chat_remove_tool() -> Tool {
  Tool(
    name: "trigger_chat_remove",
    description: "Удалить trigger чат из конфигурации (или деактивировать).",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "chat_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("ID чата для удаления")),
            ]),
          ),
          #(
            "hard_delete",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("Полное удаление (true) или деактивация (false, default)")),
              #("default", json.bool(False)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["chat_id"], json.string)),
    ]),
  )
}

/// Tool: config_cache_clear - очистить кеш конфигурации
pub fn config_cache_clear_tool() -> Tool {
  Tool(
    name: "config_cache_clear",
    description: "Очистить ETS кеш конфигурации для немедленной перезагрузки из PostgreSQL.",
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

/// Обработчик config_get
pub fn handle_config_get(args: json.Json) -> ToolResult {
  let key = json_get_string(args, "key") |> result.unwrap("")

  case key {
    "" -> protocol.error_result("key is required")
    k -> {
      with_db(fn(pool) {
        case dynamic_config.get_config(pool, k) {
          Ok(cfg) -> {
            let result = json.object([
              #("key", json.string(cfg.key)),
              #("value", json.string(cfg.value)),
              #("value_type", json.string(cfg.value_type)),
              #("description", case cfg.description {
                Some(d) -> json.string(d)
                None -> json.null()
              }),
            ])
            |> json.to_string
            protocol.text_result(result)
          }
          Error(ConfigNotFound) ->
            protocol.error_result("Config not found: " <> k)
          Error(err) ->
            protocol.error_result("Error: " <> config_error_to_string(err))
        }
      })
    }
  }
}

/// Обработчик config_set
pub fn handle_config_set(args: json.Json) -> ToolResult {
  let key = json_get_string(args, "key") |> result.unwrap("")
  let value = json_get_string(args, "value") |> result.unwrap("")
  let value_type = json_get_string(args, "value_type") |> result.unwrap("string")
  let description = json_get_string(args, "description") |> result.unwrap("")

  case key, value {
    "", _ -> protocol.error_result("key is required")
    _, "" -> protocol.error_result("value is required")
    k, v -> {
      let desc_opt = case description {
        "" -> None
        d -> Some(d)
      }

      with_db(fn(pool) {
        case dynamic_config.set_config(pool, k, v, value_type, desc_opt) {
          Ok(Nil) -> {
            let result = json.object([
              #("success", json.bool(True)),
              #("message", json.string("Config updated: " <> k <> " = " <> v)),
            ])
            |> json.to_string
            protocol.text_result(result)
          }
          Error(err) ->
            protocol.error_result("Error: " <> config_error_to_string(err))
        }
      })
    }
  }
}

/// Обработчик config_list
pub fn handle_config_list(_args: json.Json) -> ToolResult {
  with_db(fn(pool) {
    case dynamic_config.list_configs(pool) {
      Ok(configs) -> {
        let items = list.map(configs, fn(c: GlobalConfig) {
          json.object([
            #("key", json.string(c.key)),
            #("value", json.string(c.value)),
            #("type", json.string(c.value_type)),
            #("description", case c.description {
              Some(d) -> json.string(d)
              None -> json.null()
            }),
          ])
        })

        let result = json.object([
          #("count", json.int(list.length(configs))),
          #("configs", json.array(items, fn(x) { x })),
        ])
        |> json.to_string
        protocol.text_result(result)
      }
      Error(err) ->
        protocol.error_result("Error: " <> config_error_to_string(err))
    }
  })
}

/// Обработчик trigger_chat_list
pub fn handle_trigger_chat_list(_args: json.Json) -> ToolResult {
  with_db(fn(pool) {
    case dynamic_config.get_trigger_chats(pool) {
      Ok(chats) -> {
        let items = list.map(chats, fn(c: TriggerChatConfig) {
          json.object([
            #("chat_id", json.int(c.chat_id)),
            #("chat_name", json.string(c.chat_name)),
            #("chat_type", json.string(c.chat_type)),
            #("is_active", json.bool(c.is_active)),
            #("can_write", json.bool(c.can_write)),
            #("response_probability", json.float(c.response_probability)),
            #("triggers_count", json.int(list.length(c.custom_triggers))),
            #("forward_chat_id", case c.forward_chat_id {
              Some(id) -> json.int(id)
              None -> json.null()
            }),
            #("allow_images", json.bool(c.allow_images)),
          ])
        })

        let result = json.object([
          #("count", json.int(list.length(chats))),
          #("chats", json.array(items, fn(x) { x })),
        ])
        |> json.to_string
        protocol.text_result(result)
      }
      Error(err) ->
        protocol.error_result("Error: " <> config_error_to_string(err))
    }
  })
}

/// Обработчик trigger_chat_add
pub fn handle_trigger_chat_add(args: json.Json) -> ToolResult {
  let chat_id = json_get_int(args, "chat_id") |> result.unwrap(0)
  let chat_name = json_get_string(args, "chat_name") |> result.unwrap("")
  let triggers = json_get_string_list(args, "triggers") |> result.unwrap([])
  let forward_chat_id = json_get_int(args, "forward_chat_id") |> result.unwrap(0)
  let response_template = json_get_string(args, "response_template") |> result.unwrap("")
  let use_preset = json_get_string(args, "use_preset") |> result.unwrap("")

  case chat_id, chat_name {
    0, _ -> protocol.error_result("chat_id is required")
    _, "" -> protocol.error_result("chat_name is required")
    cid, cname -> {
      // Get triggers from preset if specified
      let final_triggers = case use_preset {
        "crypto_exchange" -> get_crypto_triggers()
        "" -> triggers
        _ -> triggers
      }

      let forward_opt = case forward_chat_id {
        0 -> None
        id -> Some(id)
      }

      let template_opt = case response_template {
        "" -> None
        t -> Some(t)
      }

      let cfg = TriggerChatConfig(
        chat_id: cid,
        chat_name: cname,
        chat_type: "supergroup",
        is_active: True,
        can_write: True,
        response_probability: 0.0,  // Sniper mode by default
        custom_triggers: final_triggers,
        forward_chat_id: forward_opt,
        allow_images: False,
        response_template: template_opt,
      )

      with_db(fn(pool) {
        case dynamic_config.upsert_trigger_chat(pool, cfg) {
          Ok(Nil) -> {
            let result = json.object([
              #("success", json.bool(True)),
              #("message", json.string("Trigger chat added: " <> cname)),
              #("chat_id", json.int(cid)),
              #("triggers_count", json.int(list.length(final_triggers))),
            ])
            |> json.to_string
            protocol.text_result(result)
          }
          Error(err) ->
            protocol.error_result("Error: " <> config_error_to_string(err))
        }
      })
    }
  }
}

/// Обработчик trigger_chat_remove
pub fn handle_trigger_chat_remove(args: json.Json) -> ToolResult {
  let chat_id = json_get_int(args, "chat_id") |> result.unwrap(0)
  let hard_delete = json_get_bool(args, "hard_delete") |> result.unwrap(False)

  case chat_id {
    0 -> protocol.error_result("chat_id is required")
    cid -> {
      with_db(fn(pool) {
        let op_result = case hard_delete {
          True -> dynamic_config.delete_trigger_chat(pool, cid)
          False -> dynamic_config.deactivate_trigger_chat(pool, cid)
        }

        case op_result {
          Ok(Nil) -> {
            let action = case hard_delete { True -> "deleted" False -> "deactivated" }
            let result = json.object([
              #("success", json.bool(True)),
              #("message", json.string("Trigger chat " <> action <> ": " <> int.to_string(cid))),
            ])
            |> json.to_string
            protocol.text_result(result)
          }
          Error(err) ->
            protocol.error_result("Error: " <> config_error_to_string(err))
        }
      })
    }
  }
}

/// Обработчик config_cache_clear
pub fn handle_config_cache_clear(_args: json.Json) -> ToolResult {
  dynamic_config.clear_cache()
  let result = json.object([
    #("success", json.bool(True)),
    #("message", json.string("Config cache cleared. Next requests will reload from PostgreSQL.")),
  ])
  |> json.to_string
  protocol.text_result(result)
}

// =============================================================================
// Helpers
// =============================================================================

fn config_error_to_string(err: ConfigError) -> String {
  case err {
    dynamic_config.ConfigConnectionError(msg) -> "Connection error: " <> msg
    dynamic_config.ConfigQueryError(msg) -> "Query error: " <> msg
    dynamic_config.ConfigNotFound -> "Config not found"
    dynamic_config.ConfigInvalidType(msg) -> "Invalid type: " <> msg
  }
}

/// Получить список крипто-триггеров (preset)
fn get_crypto_triggers() -> List(String) {
  [
    "куплю крипту", "купить крипту", "куплю крипты", "купить крипты",
    "где купить", "где куплю", "подскажите где купить", "как купить",
    "хочу купить", "хочу куплю", "я бы купил", "я бы крипты купил",
    "крипту купить", "крипты купить", "куплю биткоин", "купить биткоин",
    "обменять крипту", "обмен крипты", "обменять на", "обменник",
    "обмен", "п2п", "p2p", "обменять биткоин",
    "usdt", "баты", "купить usdt", "куплю usdt",
    "биткоин", "эфир", "токены", "монеты",
    "криптовалюту", "валюту", "биткоин на", "на биткоин",
    "крипта на", "на крипту", "крипты на", "на крипты",
    "где взять", "где достать", "пацаны где", "ребята где",
    "где можно купить", "где можно обменять",
  ]
}

// JSON helpers - using decoder pattern
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

fn json_get_int(j: json.Json, key: String) -> Result(Int, Nil) {
  let decoder = {
    use v <- decode.field(key, decode.int)
    decode.success(v)
  }
  let json_str = json.to_string(j)
  case json.parse(json_str, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

fn json_get_bool(j: json.Json, key: String) -> Result(Bool, Nil) {
  let decoder = {
    use v <- decode.field(key, decode.bool)
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

/// Получить все config tools
pub fn get_all_tools() -> List(Tool) {
  [
    config_get_tool(),
    config_set_tool(),
    config_list_tool(),
    trigger_chat_list_tool(),
    trigger_chat_add_tool(),
    trigger_chat_remove_tool(),
    config_cache_clear_tool(),
  ]
}
