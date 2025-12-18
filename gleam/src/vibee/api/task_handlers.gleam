// TaskFlow HTTP Handlers
// JSON API + HTML page handlers for TaskFlow UI

import gleam/bit_array
import gleam/bytes_tree
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import mist.{type Connection, type ResponseData}
import pog
import vibee/mcp/config
import vibee/db/tasks.{
  type Task, type TaskStats, type TaskFilter, type NewTask, type TaskComment,
  type StatusHistoryEntry, TaskPending, TaskInProgress, TaskWaiting,
  TaskCompleted, TaskCancelled,
}
import vibee/web/tasks_ui
import vibee/logging

// =============================================================================
// HTML PAGE HANDLERS
// =============================================================================

/// GET /tasks - Dashboard page
pub fn dashboard_page(
  _req: Request(Connection),
  pool: pog.Connection,
  owner_id: Int,
) -> Response(ResponseData) {
  // Get stats, overdue, and today's tasks
  let stats = case tasks.get_stats(pool, owner_id) {
    Ok(s) -> s
    Error(_) -> tasks.TaskStats(0, 0, 0, 0, 0, 0, 0)
  }

  let overdue = case tasks.get_overdue_tasks(pool, owner_id) {
    Ok(t) -> t
    Error(_) -> []
  }

  let today = case tasks.get_tasks_for_today(pool, owner_id) {
    Ok(t) -> t
    Error(_) -> []
  }

  let html = tasks_ui.render_dashboard(stats, overdue, today)
  html_response(html)
}

/// GET /tasks/list - Tasks list page
pub fn list_page(
  req: Request(Connection),
  pool: pog.Connection,
  owner_id: Int,
) -> Response(ResponseData) {
  // Parse query params
  let query = request.get_query(req) |> result.unwrap([])

  let filter_status = list.find(query, fn(p) { p.0 == "status" })
    |> result.map(fn(p) { p.1 })
    |> option.from_result

  let filter_priority = list.find(query, fn(p) { p.0 == "priority" })
    |> result.try(fn(p) { int.parse(p.1) })
    |> option.from_result

  let view_mode = list.find(query, fn(p) { p.0 == "view" })
    |> result.map(fn(p) { p.1 })
    |> result.unwrap("list")

  // Build filter
  let filter = tasks.TaskFilter(
    status: option.map(filter_status, tasks.string_to_status),
    contact_id: None,
    priority: filter_priority,
    category: None,
    overdue: False,
    include_archived: False,
  )

  let task_list = case tasks.list_tasks(pool, owner_id, filter) {
    Ok(t) -> t
    Error(_) -> []
  }

  let html = tasks_ui.render_tasks_list(task_list, filter_status, filter_priority, view_mode)
  html_response(html)
}

/// GET /tasks/new - Create task form
pub fn create_page(
  _req: Request(Connection),
  pool: pog.Connection,
) -> Response(ResponseData) {
  // Get contacts from telegram_dialogs for selector
  let contacts = get_contacts_for_selector(pool)
  let html = tasks_ui.render_task_create_form(contacts)
  html_response(html)
}

/// GET /tasks/:id - Task detail page
pub fn detail_page(
  _req: Request(Connection),
  pool: pog.Connection,
  task_id: Int,
) -> Response(ResponseData) {
  case tasks.get_task(pool, task_id) {
    Ok(task) -> {
      let comments = case tasks.get_comments(pool, task_id) {
        Ok(c) -> c
        Error(_) -> []
      }

      let history = case tasks.get_status_history(pool, task_id) {
        Ok(h) -> h
        Error(_) -> []
      }

      let html = tasks_ui.render_task_detail(task, comments, history)
      html_response(html)
    }
    Error(_) -> not_found_response()
  }
}

// =============================================================================
// JSON API HANDLERS
// =============================================================================

/// GET /api/v1/tasks - List tasks (JSON)
pub fn list_api(
  req: Request(Connection),
  pool: pog.Connection,
  owner_id: Int,
) -> Response(ResponseData) {
  let query = request.get_query(req) |> result.unwrap([])

  let filter_status = list.find(query, fn(p) { p.0 == "status" })
    |> result.map(fn(p) { p.1 })
    |> option.from_result

  let filter_priority = list.find(query, fn(p) { p.0 == "priority" })
    |> result.try(fn(p) { int.parse(p.1) })
    |> option.from_result

  let filter_contact = list.find(query, fn(p) { p.0 == "contact_id" })
    |> result.try(fn(p) { int.parse(p.1) })
    |> option.from_result

  let filter = tasks.TaskFilter(
    status: option.map(filter_status, tasks.string_to_status),
    contact_id: filter_contact,
    priority: filter_priority,
    category: None,
    overdue: False,
    include_archived: False,
  )

  case tasks.list_tasks(pool, owner_id, filter) {
    Ok(task_list) -> json_response(json.object([
      #("success", json.bool(True)),
      #("tasks", json.array(task_list, task_to_json)),
      #("count", json.int(list.length(task_list))),
    ]))
    Error(e) -> error_response(db_error_to_string(e))
  }
}

/// POST /api/v1/tasks - Create task
pub fn create_api(
  req: Request(Connection),
  pool: pog.Connection,
  owner_id: Int,
) -> Response(ResponseData) {
  // Parse request body
  case mist.read_body(req, 1_000_000) {
    Ok(req_with_body) -> {
      case json.parse(body_to_string(req_with_body.body), decode_new_task()) {
        Ok(new_task) -> {
          let task = tasks.NewTask(
            owner_telegram_id: owner_id,
            contact_dialog_id: case new_task.contact_id { 0 -> None _ -> Some(new_task.contact_id) },
            title: new_task.title,
            description: case new_task.description { "" -> None _ -> Some(new_task.description) },
            category: tasks.string_to_category(new_task.category),
            priority: new_task.priority,
            responsibility: new_task.responsibility,
            due_date: case new_task.due_date { "" -> None _ -> Some(new_task.due_date) },
          )

          case tasks.create_task(pool, task) {
            Ok(task_id) -> {
              logging.quick_info("Task created: " <> int.to_string(task_id))
              json_response(json.object([
                #("success", json.bool(True)),
                #("task_id", json.int(task_id)),
                #("message", json.string("Task created successfully")),
              ]))
            }
            Error(e) -> error_response(db_error_to_string(e))
          }
        }
        Error(_) -> error_response("Invalid request body")
      }
    }
    Error(_) -> error_response("Failed to read request body")
  }
}

/// GET /api/v1/tasks/stats - Task statistics
pub fn stats_api(
  _req: Request(Connection),
  pool: pog.Connection,
  owner_id: Int,
) -> Response(ResponseData) {
  case tasks.get_stats(pool, owner_id) {
    Ok(stats) -> json_response(json.object([
      #("success", json.bool(True)),
      #("stats", stats_to_json(stats)),
    ]))
    Error(e) -> error_response(db_error_to_string(e))
  }
}

/// GET /api/v1/tasks/today - Today's tasks
pub fn today_api(
  _req: Request(Connection),
  pool: pog.Connection,
  owner_id: Int,
) -> Response(ResponseData) {
  case tasks.get_tasks_for_today(pool, owner_id) {
    Ok(task_list) -> json_response(json.object([
      #("success", json.bool(True)),
      #("tasks", json.array(task_list, task_to_json)),
    ]))
    Error(e) -> error_response(db_error_to_string(e))
  }
}

/// GET /api/v1/tasks/overdue - Overdue tasks
pub fn overdue_api(
  _req: Request(Connection),
  pool: pog.Connection,
  owner_id: Int,
) -> Response(ResponseData) {
  case tasks.get_overdue_tasks(pool, owner_id) {
    Ok(task_list) -> json_response(json.object([
      #("success", json.bool(True)),
      #("tasks", json.array(task_list, task_to_json)),
    ]))
    Error(e) -> error_response(db_error_to_string(e))
  }
}

/// GET /api/v1/tasks/:id - Get task details
pub fn get_api(
  _req: Request(Connection),
  pool: pog.Connection,
  task_id: Int,
) -> Response(ResponseData) {
  case tasks.get_task(pool, task_id) {
    Ok(task) -> {
      let comments = tasks.get_comments(pool, task_id) |> result.unwrap([])
      let history = tasks.get_status_history(pool, task_id) |> result.unwrap([])

      json_response(json.object([
        #("success", json.bool(True)),
        #("task", task_to_json(task)),
        #("comments", json.array(comments, comment_to_json)),
        #("history", json.array(history, history_to_json)),
      ]))
    }
    Error(tasks.DbNotFound) -> not_found_json()
    Error(e) -> error_response(db_error_to_string(e))
  }
}

/// PATCH /api/v1/tasks/:id/status - Update task status
pub fn update_status_api(
  req: Request(Connection),
  pool: pog.Connection,
  task_id: Int,
) -> Response(ResponseData) {
  case mist.read_body(req, 100_000) {
    Ok(req_with_body) -> {
      case json.parse(body_to_string(req_with_body.body), decode_status_update()) {
        Ok(update) -> {
          let new_status = tasks.string_to_status(update.status)
          let comment_opt = case update.comment { "" -> None _ -> Some(update.comment) }
          case tasks.update_task_status(pool, task_id, new_status, Some("owner"), comment_opt) {
            Ok(_) -> json_response(json.object([
              #("success", json.bool(True)),
              #("message", json.string("Status updated")),
            ]))
            Error(e) -> error_response(db_error_to_string(e))
          }
        }
        Error(_) -> error_response("Invalid request body")
      }
    }
    Error(_) -> error_response("Failed to read request body")
  }
}

/// PATCH /api/v1/tasks/:id - Update task
pub fn update_api(
  req: Request(Connection),
  pool: pog.Connection,
  task_id: Int,
) -> Response(ResponseData) {
  case mist.read_body(req, 100_000) {
    Ok(req_with_body) -> {
      case json.parse(body_to_string(req_with_body.body), decode_task_update()) {
        Ok(update) -> {
          // Convert String/Int to Option types
          let title_opt = case update.title { "" -> None _ -> Some(update.title) }
          let desc_opt = case update.description { "" -> None _ -> Some(update.description) }
          let cat_opt = case update.category { "" -> None _ -> Some(tasks.string_to_category(update.category)) }
          let priority_opt = case update.priority { 0 -> None _ -> Some(update.priority) }
          let resp_opt = case update.responsibility { "" -> None _ -> Some(update.responsibility) }
          let due_opt = case update.due_date { "" -> None _ -> Some(update.due_date) }

          case tasks.update_task(
            pool,
            task_id,
            title_opt,
            desc_opt,
            cat_opt,
            priority_opt,
            resp_opt,
            due_opt,
          ) {
            Ok(_) -> json_response(json.object([
              #("success", json.bool(True)),
              #("message", json.string("Task updated")),
            ]))
            Error(e) -> error_response(db_error_to_string(e))
          }
        }
        Error(_) -> error_response("Invalid request body")
      }
    }
    Error(_) -> error_response("Failed to read request body")
  }
}

/// POST /api/v1/tasks/:id/comments - Add comment
pub fn add_comment_api(
  req: Request(Connection),
  pool: pog.Connection,
  task_id: Int,
  author_id: Int,
) -> Response(ResponseData) {
  case mist.read_body(req, 100_000) {
    Ok(req_with_body) -> {
      case json.parse(body_to_string(req_with_body.body), decode_comment()) {
        Ok(comment_text) -> {
          case tasks.add_comment(pool, task_id, author_id, comment_text) {
            Ok(comment_id) -> json_response(json.object([
              #("success", json.bool(True)),
              #("comment_id", json.int(comment_id)),
            ]))
            Error(e) -> error_response(db_error_to_string(e))
          }
        }
        Error(_) -> error_response("Invalid request body")
      }
    }
    Error(_) -> error_response("Failed to read request body")
  }
}

/// DELETE /api/v1/tasks/:id - Archive task
pub fn archive_api(
  _req: Request(Connection),
  pool: pog.Connection,
  task_id: Int,
) -> Response(ResponseData) {
  case tasks.archive_task(pool, task_id) {
    Ok(_) -> json_response(json.object([
      #("success", json.bool(True)),
      #("message", json.string("Task archived")),
    ]))
    Error(e) -> error_response(db_error_to_string(e))
  }
}

// =============================================================================
// HTMX PARTIAL HANDLERS
// =============================================================================

/// GET /api/v1/tasks/list-partial - HTML partial for task list
pub fn list_partial_api(
  req: Request(Connection),
  pool: pog.Connection,
  owner_id: Int,
) -> Response(ResponseData) {
  let query = request.get_query(req) |> result.unwrap([])

  let filter_status = list.find(query, fn(p) { p.0 == "status" })
    |> result.map(fn(p) { p.1 })
    |> option.from_result

  let filter = tasks.TaskFilter(
    status: option.map(filter_status, tasks.string_to_status),
    contact_id: None,
    priority: None,
    category: None,
    overdue: False,
    include_archived: False,
  )

  case tasks.list_tasks(pool, owner_id, filter) {
    Ok(task_list) -> {
      let html = "<div class=\"task-list\">" <>
        string.join(list.map(task_list, tasks_ui.render_task_card), "") <>
        "</div>"
      html_response(html)
    }
    Error(_) -> html_response("<div class=\"error\">Failed to load tasks</div>")
  }
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

fn html_response(html: String) -> Response(ResponseData) {
  response.new(200)
  |> response.set_header("content-type", "text/html; charset=utf-8")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(html)))
}

fn json_response(data: json.Json) -> Response(ResponseData) {
  response.new(200)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(json.to_string(data))))
}

fn error_response(message: String) -> Response(ResponseData) {
  response.new(400)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(
    json.to_string(json.object([
      #("success", json.bool(False)),
      #("error", json.string(message)),
    ]))
  )))
}

fn not_found_response() -> Response(ResponseData) {
  response.new(404)
  |> response.set_header("content-type", "text/html")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(
    "<h1>404 - Task Not Found</h1>"
  )))
}

fn not_found_json() -> Response(ResponseData) {
  response.new(404)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(
    json.to_string(json.object([
      #("success", json.bool(False)),
      #("error", json.string("Task not found")),
    ]))
  )))
}

fn body_to_string(body: BitArray) -> String {
  case bit_array.to_string(body) {
    Ok(s) -> s
    Error(_) -> ""
  }
}

fn db_error_to_string(err: tasks.DbError) -> String {
  case err {
    tasks.DbConnectionError(msg) -> "Connection error: " <> msg
    tasks.DbQueryError(msg) -> "Query error: " <> msg
    tasks.DbNotFound -> "Not found"
  }
}

// =============================================================================
// JSON SERIALIZATION
// =============================================================================

fn task_to_json(task: Task) -> json.Json {
  json.object([
    #("id", json.int(task.id)),
    #("owner_telegram_id", json.int(task.owner_telegram_id)),
    #("contact_dialog_id", json.nullable(task.contact_dialog_id, json.int)),
    #("contact_name", json.nullable(task.contact_name, json.string)),
    #("title", json.string(task.title)),
    #("description", json.nullable(task.description, json.string)),
    #("category", json.string(tasks.category_to_string(task.category))),
    #("status", json.string(tasks.status_to_string(task.status))),
    #("priority", json.int(task.priority)),
    #("responsibility", json.string(task.responsibility)),
    #("due_date", json.nullable(task.due_date, json.string)),
    #("archived", json.bool(task.archived)),
    #("created_at", json.string(task.created_at)),
    #("updated_at", json.string(task.updated_at)),
  ])
}

fn stats_to_json(stats: TaskStats) -> json.Json {
  json.object([
    #("total", json.int(stats.total)),
    #("pending", json.int(stats.pending)),
    #("in_progress", json.int(stats.in_progress)),
    #("waiting", json.int(stats.waiting)),
    #("completed", json.int(stats.completed)),
    #("cancelled", json.int(stats.cancelled)),
    #("overdue", json.int(stats.overdue)),
  ])
}

fn comment_to_json(comment: TaskComment) -> json.Json {
  json.object([
    #("id", json.int(comment.id)),
    #("task_id", json.int(comment.task_id)),
    #("author_telegram_id", json.int(comment.author_telegram_id)),
    #("comment_text", json.string(comment.comment_text)),
    #("created_at", json.string(comment.created_at)),
  ])
}

fn history_to_json(entry: StatusHistoryEntry) -> json.Json {
  json.object([
    #("id", json.int(entry.id)),
    #("task_id", json.int(entry.task_id)),
    #("old_status", json.nullable(entry.old_status, json.string)),
    #("new_status", json.string(entry.new_status)),
    #("changed_by", json.nullable(entry.changed_by, json.string)),
    #("comment", json.nullable(entry.comment, json.string)),
    #("changed_at", json.string(entry.changed_at)),
  ])
}

// =============================================================================
// JSON DECODERS
// =============================================================================

type NewTaskInput {
  NewTaskInput(
    title: String,
    description: String,
    contact_id: Int,
    category: String,
    priority: Int,
    responsibility: String,
    due_date: String,
  )
}

fn decode_new_task() -> decode.Decoder(NewTaskInput) {
  use title <- decode.field("title", decode.string)
  use description <- decode.optional_field("description", "", decode.string)
  use contact_id <- decode.optional_field("contact_id", 0, decode.int)
  use category <- decode.optional_field("category", "other", decode.string)
  use priority <- decode.optional_field("priority", 2, decode.int)
  use responsibility <- decode.optional_field("responsibility", "owner", decode.string)
  use due_date <- decode.optional_field("due_date", "", decode.string)

  decode.success(NewTaskInput(
    title: title,
    description: description,
    contact_id: contact_id,
    category: category,
    priority: priority,
    responsibility: responsibility,
    due_date: due_date,
  ))
}

type StatusUpdate {
  StatusUpdate(status: String, comment: String)
}

fn decode_status_update() -> decode.Decoder(StatusUpdate) {
  use status <- decode.field("status", decode.string)
  use comment <- decode.optional_field("comment", "", decode.string)
  decode.success(StatusUpdate(status: status, comment: comment))
}

type TaskUpdate {
  TaskUpdate(
    title: String,
    description: String,
    category: String,
    priority: Int,
    responsibility: String,
    due_date: String,
  )
}

fn decode_task_update() -> decode.Decoder(TaskUpdate) {
  use title <- decode.optional_field("title", "", decode.string)
  use description <- decode.optional_field("description", "", decode.string)
  use category <- decode.optional_field("category", "", decode.string)
  use priority <- decode.optional_field("priority", 0, decode.int)
  use responsibility <- decode.optional_field("responsibility", "", decode.string)
  use due_date <- decode.optional_field("due_date", "", decode.string)

  decode.success(TaskUpdate(
    title: title,
    description: description,
    category: category,
    priority: priority,
    responsibility: responsibility,
    due_date: due_date,
  ))
}

fn decode_comment() -> decode.Decoder(String) {
  use comment <- decode.field("comment", decode.string)
  decode.success(comment)
}

// =============================================================================
// CONTACTS HELPER
// =============================================================================

/// Get contacts from telegram_dialogs for selector dropdown
fn get_contacts_for_selector(pool: pog.Connection) -> List(#(Int, String)) {
  let sql = "SELECT id, name FROM telegram_dialogs WHERE name IS NOT NULL ORDER BY name LIMIT 100"

  let contact_decoder = {
    use id <- decode.field(0, decode.int)
    use name <- decode.field(1, decode.string)
    decode.success(#(id, name))
  }

  case pog.query(sql) |> pog.returning(contact_decoder) |> pog.execute(pool) {
    Ok(pog.Returned(_, contacts)) -> contacts
    Error(_) -> []
  }
}

// =============================================================================
// DATABASE POOL HELPER
// =============================================================================

/// Get database pool from DATABASE_URL
/// Creates a new connection - in production should use shared pool
pub fn get_pool() -> Result(pog.Connection, String) {
  let db_url = config.get_env_or("DATABASE_URL", "")
  case db_url {
    "" -> Error("DATABASE_URL not set")
    url -> {
      let pool_name: process.Name(pog.Message) = process.new_name(prefix: "taskflow_db")
      case pog.url_config(pool_name, url) {
        Ok(pool_config) -> {
          let config = pool_config
            |> pog.pool_size(5)
            |> pog.queue_target(50)
          case pog.start(config) {
            Ok(actor.Started(_, connection)) -> Ok(connection)
            Error(_) -> Error("Failed to start database pool")
          }
        }
        Error(_) -> Error("Invalid DATABASE_URL")
      }
    }
  }
}

/// Default owner ID for testing (get from session in production)
pub fn get_default_owner_id() -> Int {
  // In production, get from session_manager or request header
  // For now use a test value
  144022504  // Test Telegram ID
}

// =============================================================================
// UNIFIED HANDLERS (create pool internally)
// =============================================================================

/// GET /tasks - Dashboard page (creates pool internally)
pub fn dashboard_handler(req: Request(Connection)) -> Response(ResponseData) {
  case get_pool() {
    Ok(pool) -> {
      let owner_id = get_default_owner_id()
      dashboard_page(req, pool, owner_id)
    }
    Error(e) -> error_response(e)
  }
}

/// GET /tasks/list - Tasks list page (creates pool internally)
pub fn list_handler(req: Request(Connection)) -> Response(ResponseData) {
  case get_pool() {
    Ok(pool) -> {
      let owner_id = get_default_owner_id()
      list_page(req, pool, owner_id)
    }
    Error(e) -> error_response(e)
  }
}

/// GET /tasks/new - Create form page (creates pool internally)
pub fn create_form_handler(req: Request(Connection)) -> Response(ResponseData) {
  case get_pool() {
    Ok(pool) -> create_page(req, pool)
    Error(e) -> error_response(e)
  }
}

/// GET /tasks/:id - Detail page (creates pool internally)
pub fn detail_handler(req: Request(Connection), task_id: String) -> Response(ResponseData) {
  case int.parse(task_id), get_pool() {
    Ok(id), Ok(pool) -> detail_page(req, pool, id)
    _, Error(e) -> error_response(e)
    Error(_), _ -> not_found_response()
  }
}

/// GET /api/v1/tasks - List API (creates pool internally)
pub fn list_api_handler(req: Request(Connection)) -> Response(ResponseData) {
  case get_pool() {
    Ok(pool) -> {
      let owner_id = get_default_owner_id()
      list_api(req, pool, owner_id)
    }
    Error(e) -> error_response(e)
  }
}

/// POST /api/v1/tasks - Create API (creates pool internally)
pub fn create_api_handler(req: Request(Connection)) -> Response(ResponseData) {
  case get_pool() {
    Ok(pool) -> {
      let owner_id = get_default_owner_id()
      create_api(req, pool, owner_id)
    }
    Error(e) -> error_response(e)
  }
}

/// GET /api/v1/tasks/stats - Stats API (creates pool internally)
pub fn stats_api_handler(req: Request(Connection)) -> Response(ResponseData) {
  case get_pool() {
    Ok(pool) -> {
      let owner_id = get_default_owner_id()
      stats_api(req, pool, owner_id)
    }
    Error(e) -> error_response(e)
  }
}

/// GET /api/v1/tasks/today - Today API (creates pool internally)
pub fn today_api_handler(req: Request(Connection)) -> Response(ResponseData) {
  case get_pool() {
    Ok(pool) -> {
      let owner_id = get_default_owner_id()
      today_api(req, pool, owner_id)
    }
    Error(e) -> error_response(e)
  }
}

/// GET /api/v1/tasks/overdue - Overdue API (creates pool internally)
pub fn overdue_api_handler(req: Request(Connection)) -> Response(ResponseData) {
  case get_pool() {
    Ok(pool) -> {
      let owner_id = get_default_owner_id()
      overdue_api(req, pool, owner_id)
    }
    Error(e) -> error_response(e)
  }
}

/// GET /api/v1/tasks/:id - Get API (creates pool internally)
pub fn get_api_handler(req: Request(Connection), task_id: String) -> Response(ResponseData) {
  case int.parse(task_id), get_pool() {
    Ok(id), Ok(pool) -> get_api(req, pool, id)
    _, Error(e) -> error_response(e)
    Error(_), _ -> not_found_json()
  }
}

/// PATCH /api/v1/tasks/:id/status - Status API (creates pool internally)
pub fn update_status_api_handler(req: Request(Connection), task_id: String) -> Response(ResponseData) {
  case int.parse(task_id), get_pool() {
    Ok(id), Ok(pool) -> update_status_api(req, pool, id)
    _, Error(e) -> error_response(e)
    Error(_), _ -> not_found_json()
  }
}

/// PATCH /api/v1/tasks/:id - Update API (creates pool internally)
pub fn update_api_handler(req: Request(Connection), task_id: String) -> Response(ResponseData) {
  case int.parse(task_id), get_pool() {
    Ok(id), Ok(pool) -> update_api(req, pool, id)
    _, Error(e) -> error_response(e)
    Error(_), _ -> not_found_json()
  }
}

/// POST /api/v1/tasks/:id/comments - Comment API (creates pool internally)
pub fn add_comment_api_handler(req: Request(Connection), task_id: String) -> Response(ResponseData) {
  case int.parse(task_id), get_pool() {
    Ok(id), Ok(pool) -> {
      let author_id = get_default_owner_id()
      add_comment_api(req, pool, id, author_id)
    }
    _, Error(e) -> error_response(e)
    Error(_), _ -> not_found_json()
  }
}

/// DELETE /api/v1/tasks/:id - Archive API (creates pool internally)
pub fn archive_api_handler(req: Request(Connection), task_id: String) -> Response(ResponseData) {
  case int.parse(task_id), get_pool() {
    Ok(id), Ok(pool) -> archive_api(req, pool, id)
    _, Error(e) -> error_response(e)
    Error(_), _ -> not_found_json()
  }
}

/// GET /api/v1/tasks/list-partial - HTMX partial (creates pool internally)
pub fn list_partial_handler(req: Request(Connection)) -> Response(ResponseData) {
  case get_pool() {
    Ok(pool) -> {
      let owner_id = get_default_owner_id()
      list_partial_api(req, pool, owner_id)
    }
    Error(e) -> html_response("<div class=\"error\">" <> e <> "</div>")
  }
}
