// Permission Tools - MCP инструменты для управления правами чатов
// Whitelist подход: по умолчанию агент молчит, отвечает только в разрешённых чатах

import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import vibee/config/chat_permissions.{
  type ChatPermission, type PendingReview, type PermissionError, type PermissionLevel,
  Admin, Blocked, ChatPermission, ObserveOnly, Owner, PendingReview,
  PermissionNotFound, User,
}
import vibee/db/postgres
import vibee/mcp/protocol
import vibee/mcp/types.{type Tool, type ToolResult, Tool}

// =============================================================================
// Tool Definitions
// =============================================================================

/// Получить все permission tools
pub fn get_all_tools() -> List(Tool) {
  [
    chat_permission_get_tool(),
    chat_permission_grant_tool(),
    chat_permission_revoke_tool(),
    chat_permission_list_tool(),
    chat_permission_pending_tool(),
  ]
}

/// Tool: chat_permission_get - получить права чата
pub fn chat_permission_get_tool() -> Tool {
  Tool(
    name: "chat_permission_get",
    description: "Получить права чата по ID. Показывает уровень доступа, может ли агент отвечать и другие настройки.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "chat_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("ID чата (положительный для личных, отрицательный для групп)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["chat_id"], json.string)),
    ]),
  )
}

/// Tool: chat_permission_grant - выдать права на чат
pub fn chat_permission_grant_tool() -> Tool {
  Tool(
    name: "chat_permission_grant",
    description: "Выдать права на чат. Уровни: owner (полный контроль), admin (управление), user (стандартный), observe_only (только чтение), blocked (заблокирован). После выдачи агент сможет отвечать в этом чате.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "chat_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("ID чата")),
            ]),
          ),
          #(
            "level",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array(["owner", "admin", "user", "observe_only", "blocked"], json.string)),
              #("description", json.string("Уровень доступа. user - стандартный (агент отвечает), observe_only - только чтение")),
            ]),
          ),
          #(
            "chat_name",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Имя чата для удобства идентификации (опционально)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["chat_id", "level"], json.string)),
    ]),
  )
}

/// Tool: chat_permission_revoke - забрать права (заблокировать)
pub fn chat_permission_revoke_tool() -> Tool {
  Tool(
    name: "chat_permission_revoke",
    description: "Забрать права чата (установить blocked). Агент перестанет отвечать в этом чате.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "chat_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("ID чата для блокировки")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["chat_id"], json.string)),
    ]),
  )
}

/// Tool: chat_permission_list - список чатов с правами
pub fn chat_permission_list_tool() -> Tool {
  Tool(
    name: "chat_permission_list",
    description: "Получить список чатов с правами. Можно фильтровать по уровню или показать все активные (can_respond=true).",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "level",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array(["owner", "admin", "user", "observe_only", "blocked", "active"], json.string)),
              #("description", json.string("Фильтр по уровню. 'active' показывает все чаты где агент может отвечать.")),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

/// Tool: chat_permission_pending - чаты ожидающие review
pub fn chat_permission_pending_tool() -> Tool {
  Tool(
    name: "chat_permission_pending",
    description: "Показать новые чаты, которые писали агенту но не получили права. Помогает owner решить кому дать доступ. Показывает sample сообщения и количество попыток связаться.",
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
        ]),
      ),
    ]),
  )
}

// =============================================================================
// Tool Handlers (принимают json.Json как в config_tools)
// =============================================================================

/// Handler: chat_permission_get
pub fn handle_permission_get(args: json.Json) -> ToolResult {
  let chat_id = json_get_int(args, "chat_id") |> result.unwrap(0)

  case chat_id {
    0 -> protocol.error_result("chat_id is required (integer)")
    cid -> {
      case postgres.get_global_pool() {
        None -> protocol.error_result("Database connection unavailable")
        Some(pool) -> {
          case chat_permissions.get_permission(pool, cid) {
            Ok(perm) -> {
              protocol.text_result(json.to_string(json.object([
                #("chat_id", json.int(perm.chat_id)),
                #("chat_type", json.string(chat_type_string(perm.chat_type))),
                #("permission_level", json.string(chat_permissions.level_to_string(perm.permission_level))),
                #("chat_name", option_to_json(perm.chat_name)),
                #("can_respond", json.bool(perm.can_respond)),
                #("can_initiate", json.bool(perm.can_initiate)),
                #("use_triggers", json.bool(perm.use_triggers)),
                #("granted_by", option_int_to_json(perm.granted_by)),
                #("notes", option_to_json(perm.notes)),
              ])))
            }
            Error(PermissionNotFound) -> {
              protocol.text_result(json.to_string(json.object([
                #("chat_id", json.int(cid)),
                #("status", json.string("not_found")),
                #("message", json.string("Чат не найден в whitelist. Используйте chat_permission_grant для выдачи прав.")),
              ])))
            }
            Error(e) -> protocol.error_result("Database error: " <> permission_error_string(e))
          }
        }
      }
    }
  }
}

/// Handler: chat_permission_grant
pub fn handle_permission_grant(args: json.Json) -> ToolResult {
  let chat_id = json_get_int(args, "chat_id") |> result.unwrap(0)
  let level_str = json_get_string(args, "level") |> result.unwrap("")
  let chat_name = json_get_string(args, "chat_name") |> result.map(Some) |> result.unwrap(None)

  case chat_id, level_str {
    0, _ -> protocol.error_result("chat_id is required (integer)")
    _, "" -> protocol.error_result("level is required (owner, admin, user, observe_only, blocked)")
    cid, lvl -> {
      case chat_permissions.parse_permission_level(lvl) {
        Error(_) -> protocol.error_result("Invalid level. Use: owner, admin, user, observe_only, blocked")
        Ok(level) -> {
          case postgres.get_global_pool() {
            None -> protocol.error_result("Database connection unavailable")
            Some(pool) -> {
              let granted_by = 144_022_504  // Default owner

              case chat_permissions.grant_permission(pool, cid, level, granted_by, chat_name) {
                Ok(_) -> {
                  let can_respond = case level {
                    Owner | Admin | User -> True
                    ObserveOnly | Blocked -> False
                  }
                  protocol.text_result(json.to_string(json.object([
                    #("success", json.bool(True)),
                    #("chat_id", json.int(cid)),
                    #("level", json.string(lvl)),
                    #("can_respond", json.bool(can_respond)),
                    #("message", json.string(case can_respond {
                      True -> "Права выданы. Агент теперь будет отвечать в этом чате."
                      False -> "Права установлены. Агент НЕ будет отвечать в этом чате."
                    })),
                  ])))
                }
                Error(e) -> protocol.error_result("Failed to grant permission: " <> permission_error_string(e))
              }
            }
          }
        }
      }
    }
  }
}

/// Handler: chat_permission_revoke
pub fn handle_permission_revoke(args: json.Json) -> ToolResult {
  let chat_id = json_get_int(args, "chat_id") |> result.unwrap(0)

  case chat_id {
    0 -> protocol.error_result("chat_id is required (integer)")
    cid -> {
      case postgres.get_global_pool() {
        None -> protocol.error_result("Database connection unavailable")
        Some(pool) -> {
          let revoked_by = 144_022_504  // Default owner

          case chat_permissions.revoke_permission(pool, cid, revoked_by) {
            Ok(_) -> {
              protocol.text_result(json.to_string(json.object([
                #("success", json.bool(True)),
                #("chat_id", json.int(cid)),
                #("level", json.string("blocked")),
                #("message", json.string("Права отозваны. Агент больше не будет отвечать в этом чате.")),
              ])))
            }
            Error(e) -> protocol.error_result("Failed to revoke permission: " <> permission_error_string(e))
          }
        }
      }
    }
  }
}

/// Handler: chat_permission_list
pub fn handle_permission_list(args: json.Json) -> ToolResult {
  let level_filter = json_get_string(args, "level") |> result.unwrap("active")

  case postgres.get_global_pool() {
    None -> protocol.error_result("Database connection unavailable")
    Some(pool) -> {
      case level_filter {
        "active" -> {
          case chat_permissions.list_active(pool) {
            Ok(perms) -> format_permission_list(perms, "active")
            Error(e) -> protocol.error_result("Database error: " <> permission_error_string(e))
          }
        }
        level_str -> {
          case chat_permissions.parse_permission_level(level_str) {
            Ok(level) -> {
              case chat_permissions.list_by_level(pool, level) {
                Ok(perms) -> format_permission_list(perms, level_str)
                Error(e) -> protocol.error_result("Database error: " <> permission_error_string(e))
              }
            }
            Error(_) -> protocol.error_result("Invalid level. Use: owner, admin, user, observe_only, blocked, active")
          }
        }
      }
    }
  }
}

/// Handler: chat_permission_pending
pub fn handle_permission_pending(args: json.Json) -> ToolResult {
  let limit = json_get_int(args, "limit") |> result.unwrap(20)

  case postgres.get_global_pool() {
    None -> protocol.error_result("Database connection unavailable")
    Some(pool) -> {
      case chat_permissions.get_pending_reviews(pool, limit) {
        Ok(reviews) -> {
          let items = list.map(reviews, fn(r) {
            json.object([
              #("chat_id", json.int(r.chat_id)),
              #("chat_type", json.string(chat_type_string(r.chat_type))),
              #("message_count", json.int(r.message_count)),
              #("first_contact", json.string(r.first_contact_at)),
              #("last_message", json.string(r.last_message_at)),
              #("from_name", option_to_json(r.from_name)),
              #("from_username", option_to_json(r.from_username)),
              #("sample_message", option_to_json(r.sample_message)),
            ])
          })

          protocol.text_result(json.to_string(json.object([
            #("count", json.int(list.length(reviews))),
            #("pending_chats", json.array(items, fn(x) { x })),
            #("hint", json.string("Используйте chat_permission_grant(chat_id, level) для выдачи прав")),
          ])))
        }
        Error(e) -> protocol.error_result("Database error: " <> permission_error_string(e))
      }
    }
  }
}

// =============================================================================
// Helpers
// =============================================================================

fn format_permission_list(perms: List(ChatPermission), filter: String) -> ToolResult {
  let items = list.map(perms, fn(p) {
    json.object([
      #("chat_id", json.int(p.chat_id)),
      #("chat_type", json.string(chat_type_string(p.chat_type))),
      #("level", json.string(chat_permissions.level_to_string(p.permission_level))),
      #("chat_name", option_to_json(p.chat_name)),
      #("can_respond", json.bool(p.can_respond)),
      #("use_triggers", json.bool(p.use_triggers)),
    ])
  })

  protocol.text_result(json.to_string(json.object([
    #("filter", json.string(filter)),
    #("count", json.int(list.length(perms))),
    #("chats", json.array(items, fn(x) { x })),
  ])))
}

fn chat_type_string(ct: chat_permissions.ChatType) -> String {
  case ct {
    chat_permissions.Private -> "private"
    chat_permissions.Group -> "group"
    chat_permissions.Supergroup -> "supergroup"
    chat_permissions.Channel -> "channel"
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

fn permission_error_string(e: PermissionError) -> String {
  case e {
    chat_permissions.PermissionConnectionError(s) -> "Connection error: " <> s
    chat_permissions.PermissionQueryError(s) -> "Query error: " <> s
    chat_permissions.PermissionNotFound -> "Permission not found"
    chat_permissions.InvalidPermissionLevel(s) -> "Invalid level: " <> s
  }
}

// JSON helpers (same as config_tools)
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
