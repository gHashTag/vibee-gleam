// Notification Tools - MCP инструменты для управления уведомлениями owner
// Настройки фильтров, mute, история уведомлений

import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import pog
import vibee/db/postgres
import vibee/mcp/protocol
import vibee/mcp/types.{type Tool, type ToolResult, Tool}

// =============================================================================
// Tool Definitions
// =============================================================================

/// Получить все notification tools
pub fn get_all_tools() -> List(Tool) {
  [
    owner_notifications_get_tool(),
    owner_notifications_set_tool(),
    owner_notifications_mute_tool(),
    owner_notifications_history_tool(),
  ]
}

/// Tool: owner_notifications_get - получить настройки уведомлений
pub fn owner_notifications_get_tool() -> Tool {
  Tool(
    name: "owner_notifications_get",
    description: "Получить текущие настройки уведомлений owner. Показывает какие типы событий включены, минимальный уровень важности, замьюченные чаты.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
    ]),
  )
}

/// Tool: owner_notifications_set - изменить настройки
pub fn owner_notifications_set_tool() -> Tool {
  Tool(
    name: "owner_notifications_set",
    description: "Изменить настройки уведомлений owner. Можно включить/выключить типы событий и установить минимальный уровень важности.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "enabled",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("Включить/выключить все уведомления")),
            ]),
          ),
          #(
            "notify_new_messages",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("Уведомлять о новых сообщениях")),
            ]),
          ),
          #(
            "notify_agent_replies",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("Уведомлять об ответах агента")),
            ]),
          ),
          #(
            "notify_triggers",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("Уведомлять о найденных триггерах")),
            ]),
          ),
          #(
            "notify_new_chats",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("Уведомлять о новых чатах")),
            ]),
          ),
          #(
            "notify_leads",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("Уведомлять о новых лидах")),
            ]),
          ),
          #(
            "notify_errors",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("Уведомлять об ошибках")),
            ]),
          ),
          #(
            "min_importance",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array(["low", "medium", "high", "critical"], json.string)),
              #("description", json.string("Минимальный уровень важности для уведомлений")),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

/// Tool: owner_notifications_mute - временно отключить уведомления
pub fn owner_notifications_mute_tool() -> Tool {
  Tool(
    name: "owner_notifications_mute",
    description: "Временно отключить уведомления для конкретного чата или всех уведомлений. duration: 1h, 4h, 24h, или пустая строка для unmute.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "chat_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("ID чата для mute (опционально, если не указан - mute всех)")),
            ]),
          ),
          #(
            "duration",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array(["1h", "4h", "24h", "unmute"], json.string)),
              #("description", json.string("Длительность mute или 'unmute' для снятия")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["duration"], json.string)),
    ]),
  )
}

/// Tool: owner_notifications_history - история уведомлений
pub fn owner_notifications_history_tool() -> Tool {
  Tool(
    name: "owner_notifications_history",
    description: "Получить историю отправленных уведомлений owner. Показывает последние N уведомлений с фильтром по типу события.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "limit",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Максимум записей (default: 20)")),
              #("default", json.int(20)),
            ]),
          ),
          #(
            "event_type",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array(["new_message", "agent_reply", "trigger_detected", "new_chat", "lead", "error", "system"], json.string)),
              #("description", json.string("Фильтр по типу события (опционально)")),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

// =============================================================================
// Tool Handlers
// =============================================================================

/// Handler: owner_notifications_get
pub fn handle_notifications_get(_args: json.Json) -> ToolResult {
  let owner_id = 144_022_504

  case postgres.get_global_pool() {
    None -> protocol.error_result("Database connection unavailable")
    Some(pool) -> {
      let sql = "SELECT enabled, notify_new_messages, notify_agent_replies, notify_triggers,
                        notify_new_chats, notify_leads, notify_errors, min_importance,
                        mute_until, muted_chats
                 FROM owner_notification_settings WHERE owner_id = $1"

      let decoder = {
        use enabled <- decode.field(0, decode.bool)
        use notify_new_messages <- decode.field(1, decode.bool)
        use notify_agent_replies <- decode.field(2, decode.bool)
        use notify_triggers <- decode.field(3, decode.bool)
        use notify_new_chats <- decode.field(4, decode.bool)
        use notify_leads <- decode.field(5, decode.bool)
        use notify_errors <- decode.field(6, decode.bool)
        use min_importance <- decode.field(7, decode.string)
        decode.success(#(
          enabled, notify_new_messages, notify_agent_replies, notify_triggers,
          notify_new_chats, notify_leads, notify_errors, min_importance,
        ))
      }

      case pog.query(sql)
        |> pog.parameter(pog.int(owner_id))
        |> pog.returning(decoder)
        |> pog.execute(pool)
      {
        Ok(pog.Returned(_, [settings])) -> {
          let #(enabled, nm, ar, tr, nc, leads, err, importance) = settings
          protocol.text_result(json.to_string(json.object([
            #("enabled", json.bool(enabled)),
            #("notify_new_messages", json.bool(nm)),
            #("notify_agent_replies", json.bool(ar)),
            #("notify_triggers", json.bool(tr)),
            #("notify_new_chats", json.bool(nc)),
            #("notify_leads", json.bool(leads)),
            #("notify_errors", json.bool(err)),
            #("min_importance", json.string(importance)),
          ])))
        }
        Ok(_) -> protocol.text_result(json.to_string(json.object([
          #("message", json.string("No settings found, using defaults")),
          #("enabled", json.bool(True)),
          #("min_importance", json.string("medium")),
        ])))
        Error(_) -> protocol.error_result("Database query failed")
      }
    }
  }
}

/// Handler: owner_notifications_set
pub fn handle_notifications_set(args: json.Json) -> ToolResult {
  let owner_id = 144_022_504

  case postgres.get_global_pool() {
    None -> protocol.error_result("Database connection unavailable")
    Some(pool) -> {
      // Собираем SET части для UPDATE
      let updates = []
        |> maybe_add_bool_update(args, "enabled", "enabled")
        |> maybe_add_bool_update(args, "notify_new_messages", "notify_new_messages")
        |> maybe_add_bool_update(args, "notify_agent_replies", "notify_agent_replies")
        |> maybe_add_bool_update(args, "notify_triggers", "notify_triggers")
        |> maybe_add_bool_update(args, "notify_new_chats", "notify_new_chats")
        |> maybe_add_bool_update(args, "notify_leads", "notify_leads")
        |> maybe_add_bool_update(args, "notify_errors", "notify_errors")
        |> maybe_add_string_update(args, "min_importance", "min_importance")

      case list.is_empty(updates) {
        True -> protocol.error_result("No settings to update")
        False -> {
          let set_clause = string.join(updates, ", ")
          let sql = "UPDATE owner_notification_settings SET " <> set_clause <> ", updated_at = NOW() WHERE owner_id = $1"

          case pog.query(sql)
            |> pog.parameter(pog.int(owner_id))
            |> pog.execute(pool)
          {
            Ok(_) -> protocol.text_result(json.to_string(json.object([
              #("success", json.bool(True)),
              #("message", json.string("Settings updated")),
              #("updated_fields", json.array(updates, json.string)),
            ])))
            Error(_) -> protocol.error_result("Failed to update settings")
          }
        }
      }
    }
  }
}

/// Handler: owner_notifications_mute
pub fn handle_notifications_mute(args: json.Json) -> ToolResult {
  let owner_id = 144_022_504
  let duration = json_get_string(args, "duration") |> result.unwrap("1h")
  let chat_id_opt = json_get_int(args, "chat_id")

  case postgres.get_global_pool() {
    None -> protocol.error_result("Database connection unavailable")
    Some(pool) -> {
      case duration {
        "unmute" -> {
          // Снять mute
          case chat_id_opt {
            Ok(chat_id) -> {
              let sql = "UPDATE owner_notification_settings SET muted_chats = array_remove(muted_chats, $1) WHERE owner_id = $2"
              let _ = pog.query(sql)
                |> pog.parameter(pog.int(chat_id))
                |> pog.parameter(pog.int(owner_id))
                |> pog.execute(pool)
              protocol.text_result(json.to_string(json.object([
                #("success", json.bool(True)),
                #("message", json.string("Chat unmuted")),
                #("chat_id", json.int(chat_id)),
              ])))
            }
            Error(_) -> {
              let sql = "UPDATE owner_notification_settings SET mute_until = NULL, muted_chats = '{}' WHERE owner_id = $1"
              let _ = pog.query(sql)
                |> pog.parameter(pog.int(owner_id))
                |> pog.execute(pool)
              protocol.text_result(json.to_string(json.object([
                #("success", json.bool(True)),
                #("message", json.string("All notifications unmuted")),
              ])))
            }
          }
        }
        _ -> {
          // Установить mute
          let hours = case duration {
            "1h" -> 1
            "4h" -> 4
            "24h" -> 24
            _ -> 1
          }

          case chat_id_opt {
            Ok(chat_id) -> {
              let sql = "UPDATE owner_notification_settings SET muted_chats = array_append(array_remove(muted_chats, $1), $1) WHERE owner_id = $2"
              let _ = pog.query(sql)
                |> pog.parameter(pog.int(chat_id))
                |> pog.parameter(pog.int(owner_id))
                |> pog.execute(pool)
              protocol.text_result(json.to_string(json.object([
                #("success", json.bool(True)),
                #("message", json.string("Chat muted for " <> duration)),
                #("chat_id", json.int(chat_id)),
              ])))
            }
            Error(_) -> {
              let sql = "UPDATE owner_notification_settings SET mute_until = NOW() + INTERVAL '" <> int.to_string(hours) <> " hours' WHERE owner_id = $1"
              let _ = pog.query(sql)
                |> pog.parameter(pog.int(owner_id))
                |> pog.execute(pool)
              protocol.text_result(json.to_string(json.object([
                #("success", json.bool(True)),
                #("message", json.string("All notifications muted for " <> duration)),
              ])))
            }
          }
        }
      }
    }
  }
}

/// Handler: owner_notifications_history
pub fn handle_notifications_history(args: json.Json) -> ToolResult {
  let owner_id = 144_022_504
  let limit = json_get_int(args, "limit") |> result.unwrap(20)
  let event_type_filter = json_get_string(args, "event_type") |> result.map(Some) |> result.unwrap(None)

  case postgres.get_global_pool() {
    None -> protocol.error_result("Database connection unavailable")
    Some(pool) -> {
      let base_sql = "SELECT id, event_type, source_chat_id, from_user_id, message_text, telegram_message_id, sent_at::text
                      FROM owner_notification_log WHERE owner_id = $1"
      let sql = case event_type_filter {
        Some(et) -> base_sql <> " AND event_type = '" <> et <> "'"
        None -> base_sql
      } <> " ORDER BY sent_at DESC LIMIT $2"

      let decoder = {
        use id <- decode.field(0, decode.int)
        use event_type <- decode.field(1, decode.string)
        use source_chat_id <- decode.field(2, decode.optional(decode.int))
        use from_user_id <- decode.field(3, decode.optional(decode.int))
        use message_text <- decode.field(4, decode.optional(decode.string))
        use telegram_msg_id <- decode.field(5, decode.optional(decode.int))
        use sent_at <- decode.field(6, decode.string)
        decode.success(#(id, event_type, source_chat_id, from_user_id, message_text, telegram_msg_id, sent_at))
      }

      case pog.query(sql)
        |> pog.parameter(pog.int(owner_id))
        |> pog.parameter(pog.int(limit))
        |> pog.returning(decoder)
        |> pog.execute(pool)
      {
        Ok(pog.Returned(_, rows)) -> {
          let items = list.map(rows, fn(row) {
            let #(id, et, chat_id, user_id, msg, tg_id, sent) = row
            json.object([
              #("id", json.int(id)),
              #("event_type", json.string(et)),
              #("source_chat_id", option_int_to_json(chat_id)),
              #("from_user_id", option_int_to_json(user_id)),
              #("message_preview", option_to_json(msg)),
              #("telegram_message_id", option_int_to_json(tg_id)),
              #("sent_at", json.string(sent)),
            ])
          })

          protocol.text_result(json.to_string(json.object([
            #("count", json.int(list.length(rows))),
            #("notifications", json.array(items, fn(x) { x })),
          ])))
        }
        Error(_) -> protocol.error_result("Database query failed")
      }
    }
  }
}

// =============================================================================
// Helpers
// =============================================================================

fn maybe_add_bool_update(updates: List(String), args: json.Json, key: String, column: String) -> List(String) {
  case json_get_bool(args, key) {
    Ok(value) -> [column <> " = " <> bool_to_sql(value), ..updates]
    Error(_) -> updates
  }
}

fn maybe_add_string_update(updates: List(String), args: json.Json, key: String, column: String) -> List(String) {
  case json_get_string(args, key) {
    Ok(value) -> [column <> " = '" <> value <> "'", ..updates]
    Error(_) -> updates
  }
}

fn bool_to_sql(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

fn option_to_json(opt: Option(String)) -> json.Json {
  case opt {
    Some(s) -> json.string(s)
    None -> json.null()
  }
}

fn option_int_to_json(opt: Option(Int)) -> json.Json {
  case opt {
    Some(i) -> json.int(i)
    None -> json.null()
  }
}

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
