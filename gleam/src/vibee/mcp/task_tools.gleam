// TaskFlow MCP Tools - Task management with Telegram contacts
// Tools for creating, managing, and tracking tasks

import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/erlang/process
import gleam/otp/actor
import gleam/result
import gleam/string
import pog
import vibee/db/tasks.{
  type Task, type TaskCategory, type TaskComment, type TaskFilter,
  type TaskReminder, type TaskStats, type TaskStatus, CategoryConversation,
  CategoryMeeting, CategoryOther, CategoryProject, CategoryPromise,
  NewTask, Task, TaskComment, TaskFilter, TaskPending, TaskReminder,
  TaskStats,
}
import vibee/ai/task_extractor
import vibee/mcp/config
import vibee/mcp/protocol
import vibee/mcp/types.{type Tool, type ToolResult, Tool}

// =============================================================================
// Tool Definitions
// =============================================================================

/// Tool: task_create - Create a new task
pub fn task_create_tool() -> Tool {
  Tool(
    name: "task_create",
    description: "Создать задачу с привязкой к контакту из Telegram. Контакты берутся из telegram_dialogs.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "title",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Название задачи")),
            ]),
          ),
          #(
            "description",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Описание задачи (опционально)")),
            ]),
          ),
          #(
            "contact_id",
            json.object([
              #("type", json.string("integer")),
              #(
                "description",
                json.string("ID контакта из telegram_dialogs (опционально)"),
              ),
            ]),
          ),
          #(
            "category",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(
                  ["conversation", "meeting", "project", "promise", "other"],
                  json.string,
                ),
              ),
              #(
                "description",
                json.string(
                  "Категория: conversation=разговор, meeting=встреча, project=проект, promise=обещание",
                ),
              ),
            ]),
          ),
          #(
            "priority",
            json.object([
              #("type", json.string("integer")),
              #("enum", json.array([1, 2, 3], json.int)),
              #("description", json.string("Приоритет: 1=высокий, 2=средний, 3=низкий")),
            ]),
          ),
          #(
            "responsibility",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array(["owner", "contact", "both"], json.string)),
              #(
                "description",
                json.string("Ответственность: owner=я, contact=контакт, both=оба"),
              ),
            ]),
          ),
          #(
            "due_date",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("Дедлайн в формате ISO 8601 (например: 2024-12-31T18:00:00Z)"),
              ),
            ]),
          ),
          #(
            "owner_telegram_id",
            json.object([
              #("type", json.string("integer")),
              #(
                "description",
                json.string("Telegram ID владельца (опционально, берется из сессии)"),
              ),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["title"], json.string)),
    ]),
  )
}

/// Tool: task_list - List tasks with filters
pub fn task_list_tool() -> Tool {
  Tool(
    name: "task_list",
    description: "Получить список задач с фильтрами",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "status",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(
                  ["pending", "in_progress", "waiting", "completed", "cancelled"],
                  json.string,
                ),
              ),
              #("description", json.string("Фильтр по статусу")),
            ]),
          ),
          #(
            "contact_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Фильтр по контакту")),
            ]),
          ),
          #(
            "priority",
            json.object([
              #("type", json.string("integer")),
              #("enum", json.array([1, 2, 3], json.int)),
              #("description", json.string("Фильтр по приоритету")),
            ]),
          ),
          #(
            "category",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(
                  ["conversation", "meeting", "project", "promise", "other"],
                  json.string,
                ),
              ),
              #("description", json.string("Фильтр по категории")),
            ]),
          ),
          #(
            "overdue",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("Показать только просроченные")),
            ]),
          ),
          #(
            "include_archived",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("Включить архивные задачи")),
            ]),
          ),
          #(
            "owner_telegram_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Telegram ID владельца")),
            ]),
          ),
        ]),
      ),
      #("required", json.array([], json.string)),
    ]),
  )
}

/// Tool: task_get - Get task by ID
pub fn task_get_tool() -> Tool {
  Tool(
    name: "task_get",
    description: "Получить детали задачи по ID",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("ID задачи")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["task_id"], json.string)),
    ]),
  )
}

/// Tool: task_update - Update task fields
pub fn task_update_tool() -> Tool {
  Tool(
    name: "task_update",
    description: "Обновить поля задачи",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("ID задачи")),
            ]),
          ),
          #(
            "title",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Новое название")),
            ]),
          ),
          #(
            "description",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Новое описание")),
            ]),
          ),
          #(
            "category",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(
                  ["conversation", "meeting", "project", "promise", "other"],
                  json.string,
                ),
              ),
            ]),
          ),
          #(
            "priority",
            json.object([
              #("type", json.string("integer")),
              #("enum", json.array([1, 2, 3], json.int)),
            ]),
          ),
          #(
            "responsibility",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array(["owner", "contact", "both"], json.string)),
            ]),
          ),
          #(
            "due_date",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Новый дедлайн (ISO 8601)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["task_id"], json.string)),
    ]),
  )
}

/// Tool: task_status - Change task status
pub fn task_status_tool() -> Tool {
  Tool(
    name: "task_status",
    description: "Изменить статус задачи с логированием",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("ID задачи")),
            ]),
          ),
          #(
            "status",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(
                  ["pending", "in_progress", "waiting", "completed", "cancelled"],
                  json.string,
                ),
              ),
              #("description", json.string("Новый статус")),
            ]),
          ),
          #(
            "comment",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Комментарий к изменению статуса")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["task_id", "status"], json.string)),
    ]),
  )
}

/// Tool: task_comment - Add comment to task
pub fn task_comment_tool() -> Tool {
  Tool(
    name: "task_comment",
    description: "Добавить комментарий к задаче",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("ID задачи")),
            ]),
          ),
          #(
            "comment",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Текст комментария")),
            ]),
          ),
          #(
            "author_telegram_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Telegram ID автора")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["task_id", "comment"], json.string)),
    ]),
  )
}

/// Tool: task_archive - Archive task
pub fn task_archive_tool() -> Tool {
  Tool(
    name: "task_archive",
    description: "Архивировать задачу (мягкое удаление)",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("ID задачи")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["task_id"], json.string)),
    ]),
  )
}

/// Tool: task_remind - Set reminder for task
pub fn task_remind_tool() -> Tool {
  Tool(
    name: "task_remind",
    description: "Установить напоминание о задаче",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("ID задачи")),
            ]),
          ),
          #(
            "remind_at",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Время напоминания (ISO 8601)")),
            ]),
          ),
          #(
            "preset",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(["30min", "3hours", "1day", "3days"], json.string),
              ),
              #(
                "description",
                json.string("Пресет: за 30 минут/3 часа/1 день/3 дня до дедлайна"),
              ),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["task_id"], json.string)),
    ]),
  )
}

/// Tool: task_stats - Get task statistics
pub fn task_stats_tool() -> Tool {
  Tool(
    name: "task_stats",
    description: "Получить статистику по задачам",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "owner_telegram_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Telegram ID владельца")),
            ]),
          ),
        ]),
      ),
      #("required", json.array([], json.string)),
    ]),
  )
}

/// Tool: task_today - Get tasks for today
pub fn task_today_tool() -> Tool {
  Tool(
    name: "task_today",
    description: "Получить задачи на сегодня",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "owner_telegram_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Telegram ID владельца")),
            ]),
          ),
        ]),
      ),
      #("required", json.array([], json.string)),
    ]),
  )
}

/// Tool: task_overdue - Get overdue tasks
pub fn task_overdue_tool() -> Tool {
  Tool(
    name: "task_overdue",
    description: "Получить просроченные задачи",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "owner_telegram_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Telegram ID владельца")),
            ]),
          ),
        ]),
      ),
      #("required", json.array([], json.string)),
    ]),
  )
}

/// Tool: task_by_contact - Get tasks for specific contact
pub fn task_by_contact_tool() -> Tool {
  Tool(
    name: "task_by_contact",
    description: "Получить задачи с конкретным контактом",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "contact_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("ID контакта из telegram_dialogs")),
            ]),
          ),
          #(
            "owner_telegram_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Telegram ID владельца")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["contact_id"], json.string)),
    ]),
  )
}

/// Tool: task_history - Get task status history
pub fn task_history_tool() -> Tool {
  Tool(
    name: "task_history",
    description: "Получить историю изменений статуса задачи",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("ID задачи")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["task_id"], json.string)),
    ]),
  )
}

// =============================================================================
// All Tools List
// =============================================================================

/// Get all task tools
pub fn get_all_tools() -> List(Tool) {
  [
    task_create_tool(),
    task_list_tool(),
    task_get_tool(),
    task_update_tool(),
    task_status_tool(),
    task_comment_tool(),
    task_archive_tool(),
    task_remind_tool(),
    task_stats_tool(),
    task_today_tool(),
    task_overdue_tool(),
    task_by_contact_tool(),
    task_history_tool(),
    // AI Task Extraction
    telegram_extract_tasks_tool(),
    telegram_scan_all_for_tasks_tool(),
    // Onboarding & Assistant
    onboarding_status_tool(),
    task_assistant_tool(),
  ]
}

// =============================================================================
// JSON Helpers
// =============================================================================

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

fn json_get_optional_string(j: json.Json, key: String) -> Option(String) {
  case json_get_string(j, key) {
    Ok(v) -> Some(v)
    Error(_) -> None
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

fn json_get_optional_int(j: json.Json, key: String) -> Option(Int) {
  case json_get_int(j, key) {
    Ok(v) -> Some(v)
    Error(_) -> None
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

fn json_get_optional_bool(j: json.Json, key: String) -> Option(Bool) {
  case json_get_bool(j, key) {
    Ok(v) -> Some(v)
    Error(_) -> None
  }
}

// =============================================================================
// DB Pool Helper
// =============================================================================

// Global pool name for singleton connection
const pool_name_str = "vibee_task_db_pool"

/// Clear cached pool (used on connection errors)
@external(erlang, "vibee_db_pool_ffi", "clear_cache")
fn clear_pool_cache() -> Nil

/// Get or create the database connection pool (singleton with retry)
fn get_db_pool() -> Result(pog.Connection, String) {
  // Try to get existing pool first
  case get_cached_pool() {
    Some(conn) -> Ok(conn)
    None -> {
      // Clear any stale cache entry before creating new pool
      clear_pool_cache()
      create_and_cache_pool()
    }
  }
}

/// Check if pool is cached in ETS
@external(erlang, "vibee_db_pool_ffi", "get_cached")
fn get_cached_pool() -> Option(pog.Connection)

/// Cache pool in ETS
@external(erlang, "vibee_db_pool_ffi", "cache_pool")
fn cache_pool(conn: pog.Connection) -> Nil

/// Create new pool and cache it
fn create_and_cache_pool() -> Result(pog.Connection, String) {
  let db_url = config.get_env_or("DATABASE_URL", "")
  case db_url {
    "" -> Error("DATABASE_URL not set. Configure DATABASE_URL environment variable.")
    url -> {
      // Parse URL manually and create explicit config
      case parse_database_url(url) {
        Error(e) -> Error("Invalid DATABASE_URL: " <> e)
        Ok(#(host, port, user, password, database)) -> {
          let pool_name: process.Name(pog.Message) = process.new_name(prefix: pool_name_str)

          // Explicit config for Neon with SslVerified (includes SNI)
          let cfg = pog.Config(
            pool_name: pool_name,
            host: host,
            port: port,
            database: database,
            user: user,
            password: Some(password),
            ssl: pog.SslVerified,  // Required for SNI
            connection_parameters: [],
            pool_size: 2,
            queue_target: 15_000,  // 15 seconds
            queue_interval: 60_000,  // 60 seconds
            idle_interval: 30_000,
            trace: False,
            ip_version: pog.Ipv4,
            rows_as_map: False,
          )

          case pog.start(cfg) {
            Ok(actor.Started(_, connection)) -> {
              // Wait for connection to establish (Neon SSL + network)
              process.sleep(5000)
              // Cache for future requests
              cache_pool(connection)
              Ok(connection)
            }
            Error(actor.InitExited(reason)) ->
              Error("Failed to start database pool: " <> string.inspect(reason))
            Error(actor.InitTimeout) ->
              Error("Database connection timeout")
            Error(actor.InitFailed(reason)) ->
              Error("Database pool init failed: " <> reason)
          }
        }
      }
    }
  }
}

/// Parse PostgreSQL connection URL
/// Format: postgresql://user:password@host:port/database?sslmode=require
fn parse_database_url(url: String) -> Result(#(String, Int, String, String, String), String) {
  // Remove prefix
  let without_prefix = case string.split_once(url, "://") {
    Ok(#(_, rest)) -> rest
    Error(_) -> url
  }

  // Split at @ to get credentials and host
  case string.split_once(without_prefix, "@") {
    Error(_) -> Error("Missing @ in URL")
    Ok(#(creds, host_part)) -> {
      // Parse credentials (user:password)
      let #(user, password) = case string.split_once(creds, ":") {
        Ok(#(u, p)) -> #(u, p)
        Error(_) -> #(creds, "")
      }

      // Remove query params from host part
      let host_db = case string.split_once(host_part, "?") {
        Ok(#(h, _)) -> h
        Error(_) -> host_part
      }

      // Parse host:port/database
      case string.split_once(host_db, "/") {
        Error(_) -> Error("Missing database name in URL")
        Ok(#(host_port, database)) -> {
          // Parse host:port
          let #(host, port) = case string.split_once(host_port, ":") {
            Ok(#(h, p)) -> {
              let port_int = int.parse(p) |> result.unwrap(5432)
              #(h, port_int)
            }
            Error(_) -> #(host_port, 5432)
          }

          Ok(#(host, port, user, password, database))
        }
      }
    }
  }
}

// =============================================================================
// Status/Category Helpers
// =============================================================================

fn parse_status(s: String) -> TaskStatus {
  tasks.string_to_status(s)
}

fn parse_category(s: String) -> TaskCategory {
  tasks.string_to_category(s)
}

fn priority_emoji(p: Int) -> String {
  case p {
    1 -> "🔴"
    2 -> "🟡"
    3 -> "🟢"
    _ -> "⚪"
  }
}

fn status_emoji(s: TaskStatus) -> String {
  case s {
    TaskPending -> "📋"
    tasks.TaskInProgress -> "🔄"
    tasks.TaskWaiting -> "⏳"
    tasks.TaskCompleted -> "✅"
    tasks.TaskCancelled -> "❌"
  }
}

fn category_emoji(c: TaskCategory) -> String {
  case c {
    CategoryConversation -> "💬"
    CategoryMeeting -> "📞"
    CategoryProject -> "👥"
    CategoryPromise -> "✅"
    CategoryOther -> "📋"
  }
}

// =============================================================================
// Task JSON Formatters
// =============================================================================

fn task_to_json(task: Task) -> json.Json {
  json.object([
    #("id", json.int(task.id)),
    #("owner_telegram_id", json.int(task.owner_telegram_id)),
    #(
      "contact_dialog_id",
      case task.contact_dialog_id {
        Some(id) -> json.int(id)
        None -> json.null()
      },
    ),
    #(
      "contact_name",
      case task.contact_name {
        Some(name) -> json.string(name)
        None -> json.null()
      },
    ),
    #("title", json.string(task.title)),
    #(
      "description",
      case task.description {
        Some(d) -> json.string(d)
        None -> json.null()
      },
    ),
    #("category", json.string(tasks.category_to_string(task.category))),
    #("category_emoji", json.string(category_emoji(task.category))),
    #("status", json.string(tasks.status_to_string(task.status))),
    #("status_emoji", json.string(status_emoji(task.status))),
    #("priority", json.int(task.priority)),
    #("priority_emoji", json.string(priority_emoji(task.priority))),
    #("responsibility", json.string(task.responsibility)),
    #(
      "due_date",
      case task.due_date {
        Some(d) -> json.string(d)
        None -> json.null()
      },
    ),
    #("archived", json.bool(task.archived)),
    #("created_at", json.string(task.created_at)),
    #("updated_at", json.string(task.updated_at)),
  ])
}

fn comment_to_json(c: TaskComment) -> json.Json {
  json.object([
    #("id", json.int(c.id)),
    #("task_id", json.int(c.task_id)),
    #("author_telegram_id", json.int(c.author_telegram_id)),
    #("comment_text", json.string(c.comment_text)),
    #("created_at", json.string(c.created_at)),
  ])
}

fn reminder_to_json(r: TaskReminder) -> json.Json {
  json.object([
    #("id", json.int(r.id)),
    #("task_id", json.int(r.task_id)),
    #("remind_at", json.string(r.remind_at)),
    #("is_sent", json.bool(r.is_sent)),
    #(
      "sent_at",
      case r.sent_at {
        Some(s) -> json.string(s)
        None -> json.null()
      },
    ),
  ])
}

fn stats_to_json(s: TaskStats) -> json.Json {
  json.object([
    #("total", json.int(s.total)),
    #("pending", json.int(s.pending)),
    #("in_progress", json.int(s.in_progress)),
    #("waiting", json.int(s.waiting)),
    #("completed", json.int(s.completed)),
    #("cancelled", json.int(s.cancelled)),
    #("overdue", json.int(s.overdue)),
    #(
      "completion_rate",
      case s.total {
        0 -> json.float(0.0)
        _ -> json.float(int.to_float(s.completed) /. int.to_float(s.total) *. 100.0)
      },
    ),
  ])
}

// =============================================================================
// Tool Handlers
// =============================================================================

/// Handle task_create
pub fn handle_task_create(args: json.Json) -> ToolResult {
  case get_db_pool() {
    Error(e) -> protocol.error_result(e)
    Ok(pool) -> {
      case json_get_string(args, "title") {
        Error(_) -> protocol.error_result("title is required")
        Ok(title) -> {
          let owner_id = json_get_optional_int(args, "owner_telegram_id")
          case owner_id {
            None -> protocol.error_result(
              "owner_telegram_id is required. Use telegram_get_me to get your ID.",
            )
            Some(oid) -> {
              let new_task =
                NewTask(
                  owner_telegram_id: oid,
                  contact_dialog_id: json_get_optional_int(args, "contact_id"),
                  title: title,
                  description: json_get_optional_string(args, "description"),
                  category: json_get_optional_string(args, "category")
                    |> option.map(parse_category)
                    |> option.unwrap(CategoryOther),
                  priority: json_get_optional_int(args, "priority")
                    |> option.unwrap(2),
                  responsibility: json_get_optional_string(args, "responsibility")
                    |> option.unwrap("owner"),
                  due_date: json_get_optional_string(args, "due_date"),
                )

              case tasks.create_task(pool, new_task) {
                Ok(task_id) -> {
                  // Fetch created task to return full info
                  case tasks.get_task(pool, task_id) {
                    Ok(task) ->
                      protocol.text_result(
                        json.object([
                          #("success", json.bool(True)),
                          #("message", json.string("Задача создана")),
                          #("task", task_to_json(task)),
                        ])
                        |> json.to_string(),
                      )
                    Error(_) ->
                      protocol.text_result(
                        json.object([
                          #("success", json.bool(True)),
                          #("task_id", json.int(task_id)),
                          #("message", json.string("Задача создана")),
                        ])
                        |> json.to_string(),
                      )
                  }
                }
                Error(e) -> protocol.error_result(db_error_to_string(e))
              }
            }
          }
        }
      }
    }
  }
}

/// Handle task_list
pub fn handle_task_list(args: json.Json) -> ToolResult {
  case get_db_pool() {
    Error(e) -> protocol.error_result(e)
    Ok(pool) -> {
      case json_get_optional_int(args, "owner_telegram_id") {
        None -> protocol.error_result(
          "owner_telegram_id is required. Use telegram_get_me to get your ID.",
        )
        Some(owner_id) -> {
          let filter =
            TaskFilter(
              status: json_get_optional_string(args, "status")
                |> option.map(parse_status),
              contact_id: json_get_optional_int(args, "contact_id"),
              priority: json_get_optional_int(args, "priority"),
              category: json_get_optional_string(args, "category")
                |> option.map(parse_category),
              overdue: json_get_optional_bool(args, "overdue")
                |> option.unwrap(False),
              include_archived: json_get_optional_bool(args, "include_archived")
                |> option.unwrap(False),
            )

          case tasks.list_tasks(pool, owner_id, filter) {
            Ok(task_list) ->
              protocol.text_result(
                json.object([
                  #("success", json.bool(True)),
                  #("count", json.int(list.length(task_list))),
                  #("tasks", json.array(task_list, task_to_json)),
                ])
                |> json.to_string(),
              )
            Error(e) -> protocol.error_result(db_error_to_string(e))
          }
        }
      }
    }
  }
}

/// Handle task_get
pub fn handle_task_get(args: json.Json) -> ToolResult {
  case get_db_pool() {
    Error(e) -> protocol.error_result(e)
    Ok(pool) -> {
      case json_get_int(args, "task_id") {
        Error(_) -> protocol.error_result("task_id is required")
        Ok(task_id) -> {
          case tasks.get_task(pool, task_id) {
            Ok(task) -> {
              // Also get comments and reminders
              let comments =
                tasks.get_comments(pool, task_id)
                |> result.unwrap([])
              let reminders =
                tasks.get_user_task_reminders(pool, task_id)
                |> result.unwrap([])

              protocol.text_result(
                json.object([
                  #("success", json.bool(True)),
                  #("task", task_to_json(task)),
                  #("comments", json.array(comments, comment_to_json)),
                  #("reminders", json.array(reminders, reminder_to_json)),
                ])
                |> json.to_string(),
              )
            }
            Error(tasks.DbNotFound) ->
              protocol.error_result("Task not found: " <> int.to_string(task_id))
            Error(e) -> protocol.error_result(db_error_to_string(e))
          }
        }
      }
    }
  }
}

/// Handle task_update
pub fn handle_task_update(args: json.Json) -> ToolResult {
  case get_db_pool() {
    Error(e) -> protocol.error_result(e)
    Ok(pool) -> {
      case json_get_int(args, "task_id") {
        Error(_) -> protocol.error_result("task_id is required")
        Ok(task_id) -> {
          case
            tasks.update_task(
              pool,
              task_id,
              json_get_optional_string(args, "title"),
              json_get_optional_string(args, "description"),
              json_get_optional_string(args, "category")
                |> option.map(parse_category),
              json_get_optional_int(args, "priority"),
              json_get_optional_string(args, "responsibility"),
              json_get_optional_string(args, "due_date"),
            )
          {
            Ok(Nil) -> {
              case tasks.get_task(pool, task_id) {
                Ok(task) ->
                  protocol.text_result(
                    json.object([
                      #("success", json.bool(True)),
                      #("message", json.string("Задача обновлена")),
                      #("task", task_to_json(task)),
                    ])
                    |> json.to_string(),
                  )
                Error(_) ->
                  protocol.text_result(
                    json.object([
                      #("success", json.bool(True)),
                      #("message", json.string("Задача обновлена")),
                    ])
                    |> json.to_string(),
                  )
              }
            }
            Error(e) -> protocol.error_result(db_error_to_string(e))
          }
        }
      }
    }
  }
}

/// Handle task_status
pub fn handle_task_status(args: json.Json) -> ToolResult {
  case get_db_pool() {
    Error(e) -> protocol.error_result(e)
    Ok(pool) -> {
      case json_get_int(args, "task_id") {
        Error(_) -> protocol.error_result("task_id is required")
        Ok(task_id) -> {
          case json_get_string(args, "status") {
            Error(_) -> protocol.error_result("status is required")
            Ok(status_str) -> {
              let new_status = parse_status(status_str)
              let comment = json_get_optional_string(args, "comment")

              case
                tasks.update_task_status(
                  pool,
                  task_id,
                  new_status,
                  Some("owner"),
                  comment,
                )
              {
                Ok(Nil) -> {
                  case tasks.get_task(pool, task_id) {
                    Ok(task) ->
                      protocol.text_result(
                        json.object([
                          #("success", json.bool(True)),
                          #(
                            "message",
                            json.string(
                              "Статус изменен на " <> status_str,
                            ),
                          ),
                          #("task", task_to_json(task)),
                        ])
                        |> json.to_string(),
                      )
                    Error(_) ->
                      protocol.text_result(
                        json.object([
                          #("success", json.bool(True)),
                          #(
                            "message",
                            json.string(
                              "Статус изменен на " <> status_str,
                            ),
                          ),
                        ])
                        |> json.to_string(),
                      )
                  }
                }
                Error(e) -> protocol.error_result(db_error_to_string(e))
              }
            }
          }
        }
      }
    }
  }
}

/// Handle task_comment
pub fn handle_task_comment(args: json.Json) -> ToolResult {
  case get_db_pool() {
    Error(e) -> protocol.error_result(e)
    Ok(pool) -> {
      case json_get_int(args, "task_id") {
        Error(_) -> protocol.error_result("task_id is required")
        Ok(task_id) -> {
          case json_get_string(args, "comment") {
            Error(_) -> protocol.error_result("comment is required")
            Ok(comment_text) -> {
              // author_id from args or default
              let author_id =
                json_get_optional_int(args, "author_telegram_id")
                |> option.unwrap(0)

              case tasks.add_comment(pool, task_id, author_id, comment_text) {
                Ok(comment_id) ->
                  protocol.text_result(
                    json.object([
                      #("success", json.bool(True)),
                      #("comment_id", json.int(comment_id)),
                      #("message", json.string("Комментарий добавлен")),
                    ])
                    |> json.to_string(),
                  )
                Error(e) -> protocol.error_result(db_error_to_string(e))
              }
            }
          }
        }
      }
    }
  }
}

/// Handle task_archive
pub fn handle_task_archive(args: json.Json) -> ToolResult {
  case get_db_pool() {
    Error(e) -> protocol.error_result(e)
    Ok(pool) -> {
      case json_get_int(args, "task_id") {
        Error(_) -> protocol.error_result("task_id is required")
        Ok(task_id) -> {
          case tasks.archive_task(pool, task_id) {
            Ok(Nil) ->
              protocol.text_result(
                json.object([
                  #("success", json.bool(True)),
                  #("task_id", json.int(task_id)),
                  #("message", json.string("Задача архивирована")),
                ])
                |> json.to_string(),
              )
            Error(e) -> protocol.error_result(db_error_to_string(e))
          }
        }
      }
    }
  }
}

/// Handle task_remind
pub fn handle_task_remind(args: json.Json) -> ToolResult {
  case get_db_pool() {
    Error(e) -> protocol.error_result(e)
    Ok(pool) -> {
      case json_get_int(args, "task_id") {
        Error(_) -> protocol.error_result("task_id is required")
        Ok(task_id) -> {
          // Try explicit remind_at first, then preset
          let remind_at = case json_get_optional_string(args, "remind_at") {
            Some(t) -> Ok(t)
            None -> {
              case json_get_optional_string(args, "preset") {
                Some(_preset) -> {
                  // Calculate from task due_date
                  case tasks.get_task(pool, task_id) {
                    Ok(task) -> {
                      case task.due_date {
                        Some(_due) -> {
                          // TODO: Calculate reminder time based on preset
                          // For now, just use due_date
                          Ok(option.unwrap(task.due_date, ""))
                        }
                        None ->
                          Error(
                            "Task has no due_date, cannot use preset. Provide remind_at explicitly.",
                          )
                      }
                    }
                    Error(_) -> Error("Task not found")
                  }
                }
                None -> Error("Either remind_at or preset is required")
              }
            }
          }

          case remind_at {
            Error(e) -> protocol.error_result(e)
            Ok(time) -> {
              case tasks.add_reminder(pool, task_id, time) {
                Ok(reminder_id) ->
                  protocol.text_result(
                    json.object([
                      #("success", json.bool(True)),
                      #("reminder_id", json.int(reminder_id)),
                      #("remind_at", json.string(time)),
                      #("message", json.string("Напоминание установлено")),
                    ])
                    |> json.to_string(),
                  )
                Error(e) -> protocol.error_result(db_error_to_string(e))
              }
            }
          }
        }
      }
    }
  }
}

/// Handle task_stats
pub fn handle_task_stats(args: json.Json) -> ToolResult {
  case get_db_pool() {
    Error(e) -> protocol.error_result(e)
    Ok(pool) -> {
      case json_get_optional_int(args, "owner_telegram_id") {
        None -> protocol.error_result(
          "owner_telegram_id is required. Use telegram_get_me to get your ID.",
        )
        Some(owner_id) -> {
          case tasks.get_stats(pool, owner_id) {
            Ok(stats) ->
              protocol.text_result(
                json.object([
                  #("success", json.bool(True)),
                  #("stats", stats_to_json(stats)),
                ])
                |> json.to_string(),
              )
            Error(e) -> protocol.error_result(db_error_to_string(e))
          }
        }
      }
    }
  }
}

/// Handle task_today
pub fn handle_task_today(args: json.Json) -> ToolResult {
  case get_db_pool() {
    Error(e) -> protocol.error_result(e)
    Ok(pool) -> {
      case json_get_optional_int(args, "owner_telegram_id") {
        None -> protocol.error_result(
          "owner_telegram_id is required. Use telegram_get_me to get your ID.",
        )
        Some(owner_id) -> {
          case tasks.get_tasks_for_today(pool, owner_id) {
            Ok(task_list) ->
              protocol.text_result(
                json.object([
                  #("success", json.bool(True)),
                  #("date", json.string("today")),
                  #("count", json.int(list.length(task_list))),
                  #("tasks", json.array(task_list, task_to_json)),
                ])
                |> json.to_string(),
              )
            Error(e) -> protocol.error_result(db_error_to_string(e))
          }
        }
      }
    }
  }
}

/// Handle task_overdue
pub fn handle_task_overdue(args: json.Json) -> ToolResult {
  case get_db_pool() {
    Error(e) -> protocol.error_result(e)
    Ok(pool) -> {
      case json_get_optional_int(args, "owner_telegram_id") {
        None -> protocol.error_result(
          "owner_telegram_id is required. Use telegram_get_me to get your ID.",
        )
        Some(owner_id) -> {
          case tasks.get_overdue_tasks(pool, owner_id) {
            Ok(task_list) ->
              protocol.text_result(
                json.object([
                  #("success", json.bool(True)),
                  #("count", json.int(list.length(task_list))),
                  #("tasks", json.array(task_list, task_to_json)),
                ])
                |> json.to_string(),
              )
            Error(e) -> protocol.error_result(db_error_to_string(e))
          }
        }
      }
    }
  }
}

/// Handle task_by_contact
pub fn handle_task_by_contact(args: json.Json) -> ToolResult {
  case get_db_pool() {
    Error(e) -> protocol.error_result(e)
    Ok(pool) -> {
      case json_get_int(args, "contact_id") {
        Error(_) -> protocol.error_result("contact_id is required")
        Ok(contact_id) -> {
          case json_get_optional_int(args, "owner_telegram_id") {
            None -> protocol.error_result(
              "owner_telegram_id is required. Use telegram_get_me to get your ID.",
            )
            Some(owner_id) -> {
              case tasks.get_tasks_by_contact(pool, owner_id, contact_id) {
                Ok(task_list) ->
                  protocol.text_result(
                    json.object([
                      #("success", json.bool(True)),
                      #("contact_id", json.int(contact_id)),
                      #("count", json.int(list.length(task_list))),
                      #("tasks", json.array(task_list, task_to_json)),
                    ])
                    |> json.to_string(),
                  )
                Error(e) -> protocol.error_result(db_error_to_string(e))
              }
            }
          }
        }
      }
    }
  }
}

/// Handle task_history
pub fn handle_task_history(args: json.Json) -> ToolResult {
  case get_db_pool() {
    Error(e) -> protocol.error_result(e)
    Ok(pool) -> {
      case json_get_int(args, "task_id") {
        Error(_) -> protocol.error_result("task_id is required")
        Ok(task_id) -> {
          case tasks.get_status_history(pool, task_id) {
            Ok(history) -> {
              let history_json =
                list.map(history, fn(h) {
                  json.object([
                    #("id", json.int(h.id)),
                    #(
                      "old_status",
                      case h.old_status {
                        Some(s) -> json.string(s)
                        None -> json.null()
                      },
                    ),
                    #("new_status", json.string(h.new_status)),
                    #(
                      "changed_by",
                      case h.changed_by {
                        Some(c) -> json.string(c)
                        None -> json.null()
                      },
                    ),
                    #(
                      "comment",
                      case h.comment {
                        Some(c) -> json.string(c)
                        None -> json.null()
                      },
                    ),
                    #("changed_at", json.string(h.changed_at)),
                  ])
                })

              protocol.text_result(
                json.object([
                  #("success", json.bool(True)),
                  #("task_id", json.int(task_id)),
                  #("history", json.preprocessed_array(history_json)),
                ])
                |> json.to_string(),
              )
            }
            Error(e) -> protocol.error_result(db_error_to_string(e))
          }
        }
      }
    }
  }
}

// =============================================================================
// AI Task Extraction Tools
// =============================================================================

/// Tool: telegram_extract_tasks - Extract tasks from a dialog using AI
pub fn telegram_extract_tasks_tool() -> Tool {
  Tool(
    name: "telegram_extract_tasks",
    description: "Analyze a Telegram dialog and extract tasks, promises, and commitments using AI. Auto-creates tasks with confidence > 0.7.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("dialog_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Telegram dialog ID to analyze")),
        ])),
        #("limit", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Number of messages to analyze (default: 100)")),
        ])),
        #("auto_create", json.object([
          #("type", json.string("boolean")),
          #("description", json.string("Auto-create tasks with high confidence (default: true)")),
        ])),
        #("owner_telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Owner's Telegram ID for task assignment")),
        ])),
      ])),
      #("required", json.array([json.string("dialog_id"), json.string("owner_telegram_id")], fn(x) { x })),
    ]),
  )
}

/// Tool: telegram_scan_all_for_tasks - Scan all personal dialogs for tasks
pub fn telegram_scan_all_for_tasks_tool() -> Tool {
  Tool(
    name: "telegram_scan_all_for_tasks",
    description: "Scan all personal Telegram dialogs and extract tasks from each. Creates tasks automatically.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("limit_per_dialog", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Messages to analyze per dialog (default: 50)")),
        ])),
        #("owner_telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Owner's Telegram ID for task assignment")),
        ])),
      ])),
      #("required", json.array([json.string("owner_telegram_id")], fn(x) { x })),
    ]),
  )
}

/// Handler: telegram_extract_tasks
fn handle_telegram_extract_tasks(args: json.Json) -> ToolResult {
  let decoder = {
    use dialog_id <- decode.field("dialog_id", decode.int)
    use limit <- decode.optional_field("limit", 100, decode.int)
    use auto_create <- decode.optional_field("auto_create", True, decode.bool)
    use owner_id <- decode.field("owner_telegram_id", decode.int)
    decode.success(#(dialog_id, limit, auto_create, owner_id))
  }

  case json.parse(json.to_string(args), decoder) {
    Error(_) -> protocol.error_result("Invalid arguments. Required: dialog_id, owner_telegram_id")
    Ok(#(dialog_id, limit, auto_create, owner_id)) -> {
      case get_db_pool() {
        Error(e) -> protocol.error_result(e)
        Ok(pool) -> {
          let cfg = task_extractor.ExtractorConfig(
            api_key: config.get_env("OPENROUTER_API_KEY"),
            model: "google/gemini-3-pro-preview",
            min_confidence: 0.7,
            auto_create: auto_create,
          )

          case task_extractor.extract_and_create_tasks(pool, dialog_id, owner_id, limit, cfg) {
            Error(task_extractor.ExtractorApiError(e)) ->
              protocol.error_result("API error: " <> e)
            Error(task_extractor.ExtractorDbError(e)) ->
              protocol.error_result("Database error: " <> e)
            Error(task_extractor.ExtractorParseError(e)) ->
              protocol.error_result("Parse error: " <> e)
            Error(task_extractor.ExtractorNoMessages) ->
              protocol.error_result("No messages found in dialog")
            Ok(result) ->
              protocol.text_result(
                json.object([
                  #("success", json.bool(True)),
                  #("dialog_id", json.int(result.dialog_id)),
                  #("contact_name", json.string(result.contact_name)),
                  #("total_messages", json.int(result.total_messages)),
                  #("tasks_found", json.int(result.tasks_found)),
                  #("tasks_created", json.int(result.tasks_created)),
                  #("extracted_tasks", json.array(result.extracted_tasks, fn(t) {
                    json.object([
                      #("title", json.string(t.title)),
                      #("description", json.string(t.description)),
                      #("responsibility", json.string(t.responsibility)),
                      #("category", json.string(t.category)),
                      #("priority", json.int(t.priority)),
                      #("confidence", json.float(t.confidence)),
                    ])
                  })),
                  #("errors", json.array(result.errors, json.string)),
                ])
                |> json.to_string(),
              )
          }
        }
      }
    }
  }
}

/// Handler: telegram_scan_all_for_tasks
fn handle_telegram_scan_all_for_tasks(args: json.Json) -> ToolResult {
  let decoder = {
    use limit_per_dialog <- decode.optional_field("limit_per_dialog", 50, decode.int)
    use owner_id <- decode.field("owner_telegram_id", decode.int)
    decode.success(#(limit_per_dialog, owner_id))
  }

  case json.parse(json.to_string(args), decoder) {
    Error(_) -> protocol.error_result("Invalid arguments. Required: owner_telegram_id")
    Ok(#(limit_per_dialog, owner_id)) -> {
      case get_db_pool() {
        Error(e) -> protocol.error_result(e)
        Ok(pool) -> {
          let cfg = task_extractor.ExtractorConfig(
            api_key: config.get_env("OPENROUTER_API_KEY"),
            model: "google/gemini-3-pro-preview",
            min_confidence: 0.7,
            auto_create: True,
          )

          case task_extractor.scan_all_dialogs_for_tasks(pool, owner_id, limit_per_dialog, cfg) {
            Error(task_extractor.ExtractorApiError(e)) ->
              protocol.error_result("API error: " <> e)
            Error(task_extractor.ExtractorDbError(e)) ->
              protocol.error_result("Database error: " <> e)
            Error(_) ->
              protocol.error_result("Extraction failed")
            Ok(results) -> {
              let total_found = list.fold(results, 0, fn(acc, r) { acc + r.tasks_found })
              let total_created = list.fold(results, 0, fn(acc, r) { acc + r.tasks_created })

              protocol.text_result(
                json.object([
                  #("success", json.bool(True)),
                  #("dialogs_scanned", json.int(list.length(results))),
                  #("total_tasks_found", json.int(total_found)),
                  #("total_tasks_created", json.int(total_created)),
                  #("results", json.array(results, fn(r) {
                    json.object([
                      #("dialog_id", json.int(r.dialog_id)),
                      #("contact_name", json.string(r.contact_name)),
                      #("messages_analyzed", json.int(r.total_messages)),
                      #("tasks_found", json.int(r.tasks_found)),
                      #("tasks_created", json.int(r.tasks_created)),
                    ])
                  })),
                ])
                |> json.to_string(),
              )
            }
          }
        }
      }
    }
  }
}

// =============================================================================
// Onboarding Status Tool
// =============================================================================

/// Tool: onboarding_status - Check the status of automatic task extraction
pub fn onboarding_status_tool() -> Tool {
  Tool(
    name: "onboarding_status",
    description: "Check the status of automatic task extraction for a user. Shows progress, dialogs parsed, tasks extracted, and costs.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("user_telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Telegram user ID to check status for")),
        ])),
      ])),
      #("required", json.array([json.string("user_telegram_id")], fn(x) { x })),
    ]),
  )
}

/// Handler: onboarding_status
fn handle_onboarding_status(args: json.Json) -> ToolResult {
  let decoder = {
    use user_id <- decode.field("user_telegram_id", decode.int)
    decode.success(user_id)
  }

  case json.parse(json.to_string(args), decoder) {
    Error(_) -> protocol.error_result("Required: user_telegram_id")
    Ok(user_id) -> {
      case get_db_pool() {
        Error(e) -> protocol.error_result(e)
        Ok(pool) -> {
          let sql = "
            SELECT status, dialogs_parsed, tasks_extracted,
                   total_input_tokens, total_output_tokens, total_cost_usd,
                   started_at, completed_at, error_message
            FROM onboarding_status
            WHERE user_telegram_id = $1
          "
          case pog.query(sql)
            |> pog.parameter(pog.int(user_id))
            |> pog.returning({
              use status <- decode.field(0, decode.string)
              use dialogs <- decode.field(1, decode.int)
              use tasks_count <- decode.field(2, decode.int)
              use input_tokens <- decode.field(3, decode.int)
              use output_tokens <- decode.field(4, decode.int)
              use cost <- decode.field(5, decode.float)
              decode.success(#(status, dialogs, tasks_count, input_tokens, output_tokens, cost))
            })
            |> pog.execute(pool)
          {
            Ok(pog.Returned(_, [#(status, dialogs, tasks_count, input_tokens, output_tokens, cost), ..])) -> {
              let markup_cost = cost *. 1.3  // 30% markup
              protocol.text_result(
                json.object([
                  #("status", json.string(status)),
                  #("dialogs_parsed", json.int(dialogs)),
                  #("tasks_extracted", json.int(tasks_count)),
                  #("usage", json.object([
                    #("input_tokens", json.int(input_tokens)),
                    #("output_tokens", json.int(output_tokens)),
                    #("base_cost_usd", json.float(cost)),
                    #("final_cost_usd", json.float(markup_cost)),
                  ])),
                ])
                |> json.to_string(),
              )
            }
            _ -> protocol.text_result(
              json.object([
                #("status", json.string("not_started")),
                #("message", json.string("Onboarding not started for this user")),
              ])
              |> json.to_string(),
            )
          }
        }
      }
    }
  }
}

// =============================================================================
// Task Assistant Tool
// =============================================================================

/// Tool: task_assistant - AI assistant for task management
pub fn task_assistant_tool() -> Tool {
  Tool(
    name: "task_assistant",
    description: "AI-powered task assistant. Helps manage tasks, suggests priorities, identifies overdue items, and provides daily planning.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("action", json.object([
          #("type", json.string("string")),
          #("enum", json.array([
            json.string("daily_summary"),
            json.string("suggest_priorities"),
            json.string("find_overdue"),
            json.string("plan_today"),
          ], fn(x) { x })),
          #("description", json.string("Assistant action to perform")),
        ])),
        #("user_telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("User's Telegram ID")),
        ])),
      ])),
      #("required", json.array([json.string("action"), json.string("user_telegram_id")], fn(x) { x })),
    ]),
  )
}

/// Handler: task_assistant
fn handle_task_assistant(args: json.Json) -> ToolResult {
  let decoder = {
    use action <- decode.field("action", decode.string)
    use user_id <- decode.field("user_telegram_id", decode.int)
    decode.success(#(action, user_id))
  }

  case json.parse(json.to_string(args), decoder) {
    Error(_) -> protocol.error_result("Required: action, user_telegram_id")
    Ok(#(action, user_id)) -> {
      case get_db_pool() {
        Error(e) -> protocol.error_result(e)
        Ok(pool) -> {
          case action {
            "daily_summary" -> get_daily_summary(pool, user_id)
            "suggest_priorities" -> suggest_priorities(pool, user_id)
            "find_overdue" -> find_overdue_tasks(pool, user_id)
            "plan_today" -> plan_today(pool, user_id)
            _ -> protocol.error_result("Unknown action: " <> action)
          }
        }
      }
    }
  }
}

fn get_daily_summary(pool: pog.Connection, user_id: Int) -> ToolResult {
  let sql = "
    SELECT
      COUNT(*) FILTER (WHERE status = 'pending') as pending,
      COUNT(*) FILTER (WHERE status = 'in_progress') as in_progress,
      COUNT(*) FILTER (WHERE status = 'completed' AND updated_at > NOW() - INTERVAL '24 hours') as completed_today,
      COUNT(*) FILTER (WHERE due_date IS NOT NULL AND due_date < NOW()) as overdue
    FROM user_tasks
    WHERE owner_telegram_id = $1 AND archived = false
  "
  case pog.query(sql)
    |> pog.parameter(pog.int(user_id))
    |> pog.returning({
      use pending <- decode.field(0, decode.int)
      use in_progress <- decode.field(1, decode.int)
      use completed <- decode.field(2, decode.int)
      use overdue <- decode.field(3, decode.int)
      decode.success(#(pending, in_progress, completed, overdue))
    })
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [#(pending, in_progress, completed, overdue), ..])) ->
      protocol.text_result(
        json.object([
          #("summary", json.object([
            #("pending_tasks", json.int(pending)),
            #("in_progress", json.int(in_progress)),
            #("completed_today", json.int(completed)),
            #("overdue", json.int(overdue)),
          ])),
          #("recommendation", json.string(
            case overdue > 0 {
              True -> "You have " <> int.to_string(overdue) <> " overdue tasks. Focus on these first!"
              False -> case in_progress > 3 {
                True -> "You have many tasks in progress. Consider completing some before starting new ones."
                False -> "Good progress! Keep up the momentum."
              }
            }
          )),
        ])
        |> json.to_string(),
      )
    _ -> protocol.error_result("Failed to get summary")
  }
}

fn suggest_priorities(pool: pog.Connection, user_id: Int) -> ToolResult {
  let sql = "
    SELECT id, title, priority, due_date, category
    FROM user_tasks
    WHERE owner_telegram_id = $1 AND status IN ('pending', 'in_progress') AND archived = false
    ORDER BY
      CASE WHEN due_date IS NOT NULL AND due_date < NOW() THEN 0 ELSE 1 END,
      priority ASC,
      due_date ASC NULLS LAST
    LIMIT 5
  "
  case pog.query(sql)
    |> pog.parameter(pog.int(user_id))
    |> pog.returning({
      use id <- decode.field(0, decode.int)
      use title <- decode.field(1, decode.string)
      use priority <- decode.field(2, decode.int)
      decode.success(#(id, title, priority))
    })
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, tasks_list)) ->
      protocol.text_result(
        json.object([
          #("suggested_tasks", json.array(tasks_list, fn(t) {
            let #(id, title, priority) = t
            json.object([
              #("id", json.int(id)),
              #("title", json.string(title)),
              #("priority", json.int(priority)),
            ])
          })),
          #("tip", json.string("Focus on high-priority (1) and overdue tasks first")),
        ])
        |> json.to_string(),
      )
    _ -> protocol.error_result("Failed to suggest priorities")
  }
}

fn find_overdue_tasks(pool: pog.Connection, user_id: Int) -> ToolResult {
  let sql = "
    SELECT id, title, due_date, priority
    FROM user_tasks
    WHERE owner_telegram_id = $1 AND due_date < NOW() AND status != 'completed' AND archived = false
    ORDER BY due_date ASC
  "
  case pog.query(sql)
    |> pog.parameter(pog.int(user_id))
    |> pog.returning({
      use id <- decode.field(0, decode.int)
      use title <- decode.field(1, decode.string)
      decode.success(#(id, title))
    })
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, tasks_list)) ->
      protocol.text_result(
        json.object([
          #("overdue_count", json.int(list.length(tasks_list))),
          #("overdue_tasks", json.array(tasks_list, fn(t) {
            let #(id, title) = t
            json.object([
              #("id", json.int(id)),
              #("title", json.string(title)),
            ])
          })),
        ])
        |> json.to_string(),
      )
    _ -> protocol.error_result("Failed to find overdue tasks")
  }
}

fn plan_today(pool: pog.Connection, user_id: Int) -> ToolResult {
  let sql = "
    SELECT id, title, priority, category
    FROM user_tasks
    WHERE owner_telegram_id = $1
      AND status IN ('pending', 'in_progress')
      AND archived = false
      AND (due_date IS NULL OR due_date <= NOW() + INTERVAL '1 day')
    ORDER BY priority ASC, created_at ASC
    LIMIT 7
  "
  case pog.query(sql)
    |> pog.parameter(pog.int(user_id))
    |> pog.returning({
      use id <- decode.field(0, decode.int)
      use title <- decode.field(1, decode.string)
      use priority <- decode.field(2, decode.int)
      use category <- decode.field(3, decode.string)
      decode.success(#(id, title, priority, category))
    })
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, tasks_list)) ->
      protocol.text_result(
        json.object([
          #("today_plan", json.array(tasks_list, fn(t) {
            let #(id, title, priority, category) = t
            json.object([
              #("id", json.int(id)),
              #("title", json.string(title)),
              #("priority", json.int(priority)),
              #("category", json.string(category)),
            ])
          })),
          #("tasks_count", json.int(list.length(tasks_list))),
          #("tip", json.string("Start with priority 1 tasks. Take breaks between tasks.")),
        ])
        |> json.to_string(),
      )
    _ -> protocol.error_result("Failed to plan today")
  }
}

// =============================================================================
// Handler Registry
// =============================================================================

/// Get all task handlers as tuples
pub fn get_all_handlers() -> List(#(String, fn(json.Json) -> ToolResult)) {
  [
    #("task_create", handle_task_create),
    #("task_list", handle_task_list),
    #("task_get", handle_task_get),
    #("task_update", handle_task_update),
    #("task_status", handle_task_status),
    #("task_comment", handle_task_comment),
    #("task_archive", handle_task_archive),
    #("task_remind", handle_task_remind),
    #("task_stats", handle_task_stats),
    #("task_today", handle_task_today),
    #("task_overdue", handle_task_overdue),
    #("task_by_contact", handle_task_by_contact),
    #("task_history", handle_task_history),
    #("telegram_extract_tasks", handle_telegram_extract_tasks),
    #("telegram_scan_all_for_tasks", handle_telegram_scan_all_for_tasks),
    #("onboarding_status", handle_onboarding_status),
    #("task_assistant", handle_task_assistant),
  ]
}

// =============================================================================
// Error Helper
// =============================================================================

fn db_error_to_string(err: tasks.DbError) -> String {
  case err {
    tasks.DbConnectionError(s) -> "Database connection error: " <> s
    tasks.DbQueryError(s) -> "Database query error: " <> s
    tasks.DbNotFound -> "Not found"
  }
}
