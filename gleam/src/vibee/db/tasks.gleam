// TaskFlow: Collaborative Task Management with Telegram Contacts
// CRUD operations for tasks, comments, reminders, status history

import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import pog

// =============================================================================
// Types
// =============================================================================

/// Task status
pub type TaskStatus {
  TaskPending
  TaskInProgress
  TaskWaiting
  TaskCompleted
  TaskCancelled
}

/// Task category
pub type TaskCategory {
  CategoryConversation  // 💬 Discussion to have
  CategoryMeeting       // 📞 Meeting/call
  CategoryProject       // 👥 Joint project
  CategoryPromise       // ✅ Something promised
  CategoryOther         // 📋 Other
}

/// Task record
pub type Task {
  Task(
    id: Int,
    owner_telegram_id: Int,
    contact_dialog_id: Option(Int),
    contact_name: Option(String),  // Denormalized from telegram_dialogs
    title: String,
    description: Option(String),
    category: TaskCategory,
    status: TaskStatus,
    priority: Int,  // 1=high, 2=medium, 3=low
    responsibility: String,  // "owner", "contact", "both"
    due_date: Option(String),
    archived: Bool,
    created_at: String,
    updated_at: String,
  )
}

/// Task for creation
pub type NewTask {
  NewTask(
    owner_telegram_id: Int,
    contact_dialog_id: Option(Int),
    title: String,
    description: Option(String),
    category: TaskCategory,
    priority: Int,
    responsibility: String,
    due_date: Option(String),
  )
}

/// Task filter options
pub type TaskFilter {
  TaskFilter(
    status: Option(TaskStatus),
    contact_id: Option(Int),
    priority: Option(Int),
    category: Option(TaskCategory),
    overdue: Bool,
    include_archived: Bool,
  )
}

/// Task comment
pub type TaskComment {
  TaskComment(
    id: Int,
    task_id: Int,
    author_telegram_id: Int,
    comment_text: String,
    created_at: String,
  )
}

/// Task reminder
pub type TaskReminder {
  TaskReminder(
    id: Int,
    task_id: Int,
    remind_at: String,
    is_sent: Bool,
    sent_at: Option(String),
  )
}

/// Status history entry
pub type StatusHistoryEntry {
  StatusHistoryEntry(
    id: Int,
    task_id: Int,
    old_status: Option(String),
    new_status: String,
    changed_by: Option(String),
    comment: Option(String),
    changed_at: String,
  )
}

/// Task statistics
pub type TaskStats {
  TaskStats(
    total: Int,
    pending: Int,
    in_progress: Int,
    waiting: Int,
    completed: Int,
    cancelled: Int,
    overdue: Int,
  )
}

/// Database error
pub type DbError {
  DbConnectionError(String)
  DbQueryError(String)
  DbNotFound
}

// =============================================================================
// Helper Functions
// =============================================================================

fn add_parameters(query: pog.Query(t), params: List(pog.Value)) -> pog.Query(t) {
  list.fold(params, query, fn(q, p) { pog.parameter(q, p) })
}

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

/// Convert TaskStatus to string
pub fn status_to_string(status: TaskStatus) -> String {
  case status {
    TaskPending -> "pending"
    TaskInProgress -> "in_progress"
    TaskWaiting -> "waiting"
    TaskCompleted -> "completed"
    TaskCancelled -> "cancelled"
  }
}

/// Parse string to TaskStatus
pub fn string_to_status(s: String) -> TaskStatus {
  case s {
    "pending" -> TaskPending
    "in_progress" -> TaskInProgress
    "waiting" -> TaskWaiting
    "completed" -> TaskCompleted
    "cancelled" -> TaskCancelled
    _ -> TaskPending
  }
}

/// Convert TaskCategory to string
pub fn category_to_string(cat: TaskCategory) -> String {
  case cat {
    CategoryConversation -> "conversation"
    CategoryMeeting -> "meeting"
    CategoryProject -> "project"
    CategoryPromise -> "promise"
    CategoryOther -> "other"
  }
}

/// Parse string to TaskCategory
pub fn string_to_category(s: String) -> TaskCategory {
  case s {
    "conversation" -> CategoryConversation
    "meeting" -> CategoryMeeting
    "project" -> CategoryProject
    "promise" -> CategoryPromise
    _ -> CategoryOther
  }
}

/// Default empty filter
pub fn empty_filter() -> TaskFilter {
  TaskFilter(
    status: None,
    contact_id: None,
    priority: None,
    category: None,
    overdue: False,
    include_archived: False,
  )
}

// =============================================================================
// Task CRUD Operations
// =============================================================================

/// Create a new task
pub fn create_task(
  pool: pog.Connection,
  task: NewTask,
) -> Result(Int, DbError) {
  let sql =
    "INSERT INTO user_tasks (owner_telegram_id, contact_dialog_id, title, description,
     category, status, priority, responsibility, due_date)
     VALUES ($1, $2, $3, $4, $5, 'pending', $6, $7, $8::timestamptz)
     RETURNING id"

  let id_decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(id)
  }

  case
    pog.query(sql)
    |> add_parameters([
      pog.int(task.owner_telegram_id),
      pog.nullable(pog.int, task.contact_dialog_id),
      pog.text(task.title),
      pog.nullable(pog.text, task.description),
      pog.text(category_to_string(task.category)),
      pog.int(task.priority),
      pog.text(task.responsibility),
      pog.nullable(pog.text, task.due_date),
    ])
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [id])) -> {
      // Log initial status
      let _ = log_status_change(pool, id, None, TaskPending, Some("owner"), None)
      Ok(id)
    }
    Ok(_) -> Error(DbQueryError("Failed to create task"))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get task by ID with contact name
pub fn get_task(
  pool: pog.Connection,
  task_id: Int,
) -> Result(Task, DbError) {
  let sql =
    "SELECT t.id, t.owner_telegram_id, t.contact_dialog_id, d.title as contact_name,
            t.title, t.description, t.category, t.status, t.priority,
            t.responsibility, t.due_date::text, t.archived,
            t.created_at::text, t.updated_at::text
     FROM user_tasks t
     LEFT JOIN telegram_dialogs d ON t.contact_dialog_id = d.id
     WHERE t.id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.int(task_id)])
    |> pog.returning(decode_task())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [task])) -> Ok(task)
    Ok(pog.Returned(_, [])) -> Error(DbNotFound)
    Ok(_) -> Error(DbNotFound)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// List tasks for owner with filters
pub fn list_tasks(
  pool: pog.Connection,
  owner_id: Int,
  filter: TaskFilter,
) -> Result(List(Task), DbError) {
  // Build WHERE clauses dynamically
  let base_conditions = ["t.owner_telegram_id = $1"]
  let base_params = [pog.int(owner_id)]
  let idx = 2

  let #(conditions, params, idx) = case filter.include_archived {
    True -> #(base_conditions, base_params, idx)
    False -> #(["t.archived = false", ..base_conditions], base_params, idx)
  }

  let #(conditions, params, idx) = case filter.status {
    Some(status) -> #(
      ["t.status = $" <> int.to_string(idx), ..conditions],
      [pog.text(status_to_string(status)), ..params],
      idx + 1,
    )
    None -> #(conditions, params, idx)
  }

  let #(conditions, params, idx) = case filter.contact_id {
    Some(cid) -> #(
      ["t.contact_dialog_id = $" <> int.to_string(idx), ..conditions],
      [pog.int(cid), ..params],
      idx + 1,
    )
    None -> #(conditions, params, idx)
  }

  let #(conditions, params, idx) = case filter.priority {
    Some(p) -> #(
      ["t.priority = $" <> int.to_string(idx), ..conditions],
      [pog.int(p), ..params],
      idx + 1,
    )
    None -> #(conditions, params, idx)
  }

  let #(conditions, params, _idx) = case filter.category {
    Some(cat) -> #(
      ["t.category = $" <> int.to_string(idx), ..conditions],
      [pog.text(category_to_string(cat)), ..params],
      idx + 1,
    )
    None -> #(conditions, params, idx)
  }

  let conditions = case filter.overdue {
    True -> [
      "t.due_date < NOW() AND t.status NOT IN ('completed', 'cancelled')",
      ..conditions
    ]
    False -> conditions
  }

  let where_clause = string.join(list.reverse(conditions), " AND ")
  let all_params = list.reverse(params)

  let sql =
    "SELECT t.id, t.owner_telegram_id, t.contact_dialog_id, d.title as contact_name,
            t.title, t.description, t.category, t.status, t.priority,
            t.responsibility, t.due_date::text, t.archived,
            t.created_at::text, t.updated_at::text
     FROM user_tasks t
     LEFT JOIN telegram_dialogs d ON t.contact_dialog_id = d.id
     WHERE " <> where_clause <> "
     ORDER BY t.priority ASC, t.due_date ASC NULLS LAST, t.created_at DESC"

  case
    pog.query(sql)
    |> add_parameters(all_params)
    |> pog.returning(decode_task())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, tasks)) -> Ok(tasks)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Update task status with history logging
pub fn update_task_status(
  pool: pog.Connection,
  task_id: Int,
  new_status: TaskStatus,
  changed_by: Option(String),
  comment: Option(String),
) -> Result(Nil, DbError) {
  // Get current status first
  case get_task(pool, task_id) {
    Error(e) -> Error(e)
    Ok(task) -> {
      let old_status = task.status

      let completed_clause = case new_status {
        TaskCompleted -> ", completed_at = NOW()"
        _ -> ""
      }

      let sql =
        "UPDATE user_tasks SET status = $2, updated_at = NOW()" <> completed_clause <> "
         WHERE id = $1"

      case
        pog.query(sql)
        |> add_parameters([
          pog.int(task_id),
          pog.text(status_to_string(new_status)),
        ])
        |> pog.execute(pool)
      {
        Ok(_) -> {
          // Log status change
          let _ = log_status_change(pool, task_id, Some(old_status), new_status, changed_by, comment)
          Ok(Nil)
        }
        Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
      }
    }
  }
}

/// Update task fields
pub fn update_task(
  pool: pog.Connection,
  task_id: Int,
  title: Option(String),
  description: Option(String),
  category: Option(TaskCategory),
  priority: Option(Int),
  responsibility: Option(String),
  due_date: Option(String),
) -> Result(Nil, DbError) {
  let updates = []
  let params = []
  let idx = 1

  let #(updates, params, idx) = case title {
    Some(t) -> #(
      ["title = $" <> int.to_string(idx), ..updates],
      [pog.text(t), ..params],
      idx + 1,
    )
    None -> #(updates, params, idx)
  }

  let #(updates, params, idx) = case description {
    Some(d) -> #(
      ["description = $" <> int.to_string(idx), ..updates],
      [pog.text(d), ..params],
      idx + 1,
    )
    None -> #(updates, params, idx)
  }

  let #(updates, params, idx) = case category {
    Some(c) -> #(
      ["category = $" <> int.to_string(idx), ..updates],
      [pog.text(category_to_string(c)), ..params],
      idx + 1,
    )
    None -> #(updates, params, idx)
  }

  let #(updates, params, idx) = case priority {
    Some(p) -> #(
      ["priority = $" <> int.to_string(idx), ..updates],
      [pog.int(p), ..params],
      idx + 1,
    )
    None -> #(updates, params, idx)
  }

  let #(updates, params, idx) = case responsibility {
    Some(r) -> #(
      ["responsibility = $" <> int.to_string(idx), ..updates],
      [pog.text(r), ..params],
      idx + 1,
    )
    None -> #(updates, params, idx)
  }

  let #(updates, params, idx) = case due_date {
    Some(d) -> #(
      ["due_date = $" <> int.to_string(idx) <> "::timestamptz", ..updates],
      [pog.text(d), ..params],
      idx + 1,
    )
    None -> #(updates, params, idx)
  }

  case list.length(updates) {
    0 -> Ok(Nil)
    _ -> {
      let sql =
        "UPDATE user_tasks SET "
        <> string.join(list.reverse(updates), ", ")
        <> ", updated_at = NOW() WHERE id = $"
        <> int.to_string(idx)

      let all_params = list.reverse([pog.int(task_id), ..params])

      case pog.query(sql) |> add_parameters(all_params) |> pog.execute(pool) {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
      }
    }
  }
}

/// Archive task (soft delete)
pub fn archive_task(
  pool: pog.Connection,
  task_id: Int,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE user_tasks SET archived = true, updated_at = NOW() WHERE id = $1"

  case pog.query(sql) |> add_parameters([pog.int(task_id)]) |> pog.execute(pool) {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Permanently delete task
pub fn delete_task(
  pool: pog.Connection,
  task_id: Int,
) -> Result(Nil, DbError) {
  let sql = "DELETE FROM user_tasks WHERE id = $1"

  case pog.query(sql) |> add_parameters([pog.int(task_id)]) |> pog.execute(pool) {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Comments
// =============================================================================

/// Add comment to task
pub fn add_comment(
  pool: pog.Connection,
  task_id: Int,
  author_id: Int,
  text: String,
) -> Result(Int, DbError) {
  let sql =
    "INSERT INTO user_task_comments (task_id, author_telegram_id, comment_text)
     VALUES ($1, $2, $3)
     RETURNING id"

  let id_decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(id)
  }

  case
    pog.query(sql)
    |> add_parameters([pog.int(task_id), pog.int(author_id), pog.text(text)])
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [id])) -> Ok(id)
    Ok(_) -> Error(DbQueryError("Failed to add comment"))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get comments for task
pub fn get_comments(
  pool: pog.Connection,
  task_id: Int,
) -> Result(List(TaskComment), DbError) {
  let sql =
    "SELECT id, task_id, author_telegram_id, comment_text, created_at::text
     FROM user_task_comments
     WHERE task_id = $1
     ORDER BY created_at ASC"

  case
    pog.query(sql)
    |> add_parameters([pog.int(task_id)])
    |> pog.returning(decode_comment())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, comments)) -> Ok(comments)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Reminders
// =============================================================================

/// Add reminder for task
pub fn add_reminder(
  pool: pog.Connection,
  task_id: Int,
  remind_at: String,
) -> Result(Int, DbError) {
  let sql =
    "INSERT INTO user_task_reminders (task_id, remind_at)
     VALUES ($1, $2::timestamptz)
     RETURNING id"

  let id_decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(id)
  }

  case
    pog.query(sql)
    |> add_parameters([pog.int(task_id), pog.text(remind_at)])
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [id])) -> Ok(id)
    Ok(_) -> Error(DbQueryError("Failed to add reminder"))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get pending reminders (for reminder actor)
pub fn get_pending_reminders(
  pool: pog.Connection,
) -> Result(List(#(TaskReminder, Task)), DbError) {
  let sql =
    "SELECT r.id, r.task_id, r.remind_at::text, r.is_sent, r.sent_at::text,
            t.id, t.owner_telegram_id, t.contact_dialog_id, d.title,
            t.title, t.description, t.category, t.status, t.priority,
            t.responsibility, t.due_date::text, t.archived,
            t.created_at::text, t.updated_at::text
     FROM user_task_reminders r
     JOIN tasks t ON r.task_id = t.id
     LEFT JOIN telegram_dialogs d ON t.contact_dialog_id = d.id
     WHERE r.is_sent = false AND r.remind_at <= NOW()
     ORDER BY r.remind_at ASC"

  case
    pog.query(sql)
    |> pog.returning(decode_reminder_with_task())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, results)) -> Ok(results)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Mark reminder as sent
pub fn mark_reminder_sent(
  pool: pog.Connection,
  reminder_id: Int,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE user_task_reminders SET is_sent = true, sent_at = NOW()
     WHERE id = $1"

  case pog.query(sql) |> add_parameters([pog.int(reminder_id)]) |> pog.execute(pool) {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get reminders for task
pub fn get_user_task_reminders(
  pool: pog.Connection,
  task_id: Int,
) -> Result(List(TaskReminder), DbError) {
  let sql =
    "SELECT id, task_id, remind_at::text, is_sent, sent_at::text
     FROM user_task_reminders
     WHERE task_id = $1
     ORDER BY remind_at ASC"

  case
    pog.query(sql)
    |> add_parameters([pog.int(task_id)])
    |> pog.returning(decode_reminder())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, reminders)) -> Ok(reminders)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Status History
// =============================================================================

fn log_status_change(
  pool: pog.Connection,
  task_id: Int,
  old_status: Option(TaskStatus),
  new_status: TaskStatus,
  changed_by: Option(String),
  comment: Option(String),
) -> Result(Int, DbError) {
  let sql =
    "INSERT INTO user_task_status_history (task_id, old_status, new_status, changed_by, comment)
     VALUES ($1, $2, $3, $4, $5)
     RETURNING id"

  let id_decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(id)
  }

  case
    pog.query(sql)
    |> add_parameters([
      pog.int(task_id),
      pog.nullable(pog.text, option.map(old_status, status_to_string)),
      pog.text(status_to_string(new_status)),
      pog.nullable(pog.text, changed_by),
      pog.nullable(pog.text, comment),
    ])
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [id])) -> Ok(id)
    Ok(_) -> Error(DbQueryError("Failed to log status change"))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get status history for task
pub fn get_status_history(
  pool: pog.Connection,
  task_id: Int,
) -> Result(List(StatusHistoryEntry), DbError) {
  let sql =
    "SELECT id, task_id, old_status, new_status, changed_by, comment, changed_at::text
     FROM user_task_status_history
     WHERE task_id = $1
     ORDER BY changed_at ASC"

  case
    pog.query(sql)
    |> add_parameters([pog.int(task_id)])
    |> pog.returning(decode_status_history())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, entries)) -> Ok(entries)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Statistics
// =============================================================================

/// Get task statistics for owner
pub fn get_stats(
  pool: pog.Connection,
  owner_id: Int,
) -> Result(TaskStats, DbError) {
  let sql =
    "SELECT
       COUNT(*) as total,
       COUNT(*) FILTER (WHERE status = 'pending') as pending,
       COUNT(*) FILTER (WHERE status = 'in_progress') as in_progress,
       COUNT(*) FILTER (WHERE status = 'waiting') as waiting,
       COUNT(*) FILTER (WHERE status = 'completed') as completed,
       COUNT(*) FILTER (WHERE status = 'cancelled') as cancelled,
       COUNT(*) FILTER (WHERE due_date < NOW() AND status NOT IN ('completed', 'cancelled')) as overdue
     FROM user_tasks
     WHERE owner_telegram_id = $1 AND archived = false"

  case
    pog.query(sql)
    |> add_parameters([pog.int(owner_id)])
    |> pog.returning(decode_stats())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [stats])) -> Ok(stats)
    Ok(_) -> Ok(TaskStats(0, 0, 0, 0, 0, 0, 0))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get overdue tasks
pub fn get_overdue_tasks(
  pool: pog.Connection,
  owner_id: Int,
) -> Result(List(Task), DbError) {
  list_tasks(pool, owner_id, TaskFilter(..empty_filter(), overdue: True))
}

/// Get tasks for today (due today)
pub fn get_tasks_for_today(
  pool: pog.Connection,
  owner_id: Int,
) -> Result(List(Task), DbError) {
  let sql =
    "SELECT t.id, t.owner_telegram_id, t.contact_dialog_id, d.title as contact_name,
            t.title, t.description, t.category, t.status, t.priority,
            t.responsibility, t.due_date::text, t.archived,
            t.created_at::text, t.updated_at::text
     FROM user_tasks t
     LEFT JOIN telegram_dialogs d ON t.contact_dialog_id = d.id
     WHERE t.owner_telegram_id = $1
       AND t.archived = false
       AND t.status NOT IN ('completed', 'cancelled')
       AND DATE(t.due_date) = CURRENT_DATE
     ORDER BY t.priority ASC, t.due_date ASC"

  case
    pog.query(sql)
    |> add_parameters([pog.int(owner_id)])
    |> pog.returning(decode_task())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, tasks)) -> Ok(tasks)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get tasks by contact
pub fn get_tasks_by_contact(
  pool: pog.Connection,
  owner_id: Int,
  contact_id: Int,
) -> Result(List(Task), DbError) {
  list_tasks(pool, owner_id, TaskFilter(..empty_filter(), contact_id: Some(contact_id)))
}

// =============================================================================
// Decoders
// =============================================================================

fn decode_task() -> Decoder(Task) {
  use id <- decode.field(0, decode.int)
  use owner_telegram_id <- decode.field(1, decode.int)
  use contact_dialog_id <- decode.field(2, decode.optional(decode.int))
  use contact_name <- decode.field(3, decode.optional(decode.string))
  use title <- decode.field(4, decode.string)
  use description <- decode.field(5, decode.optional(decode.string))
  use category_str <- decode.field(6, decode.string)
  use status_str <- decode.field(7, decode.string)
  use priority <- decode.field(8, decode.int)
  use responsibility <- decode.field(9, decode.string)
  use due_date <- decode.field(10, decode.optional(decode.string))
  use archived <- decode.field(11, decode.bool)
  use created_at <- decode.field(12, decode.string)
  use updated_at <- decode.field(13, decode.string)

  decode.success(Task(
    id: id,
    owner_telegram_id: owner_telegram_id,
    contact_dialog_id: contact_dialog_id,
    contact_name: contact_name,
    title: title,
    description: description,
    category: string_to_category(category_str),
    status: string_to_status(status_str),
    priority: priority,
    responsibility: responsibility,
    due_date: due_date,
    archived: archived,
    created_at: created_at,
    updated_at: updated_at,
  ))
}

fn decode_comment() -> Decoder(TaskComment) {
  use id <- decode.field(0, decode.int)
  use task_id <- decode.field(1, decode.int)
  use author_telegram_id <- decode.field(2, decode.int)
  use comment_text <- decode.field(3, decode.string)
  use created_at <- decode.field(4, decode.string)

  decode.success(TaskComment(
    id: id,
    task_id: task_id,
    author_telegram_id: author_telegram_id,
    comment_text: comment_text,
    created_at: created_at,
  ))
}

fn decode_reminder() -> Decoder(TaskReminder) {
  use id <- decode.field(0, decode.int)
  use task_id <- decode.field(1, decode.int)
  use remind_at <- decode.field(2, decode.string)
  use is_sent <- decode.field(3, decode.bool)
  use sent_at <- decode.field(4, decode.optional(decode.string))

  decode.success(TaskReminder(
    id: id,
    task_id: task_id,
    remind_at: remind_at,
    is_sent: is_sent,
    sent_at: sent_at,
  ))
}

fn decode_reminder_with_task() -> Decoder(#(TaskReminder, Task)) {
  use r_id <- decode.field(0, decode.int)
  use r_task_id <- decode.field(1, decode.int)
  use r_remind_at <- decode.field(2, decode.string)
  use r_is_sent <- decode.field(3, decode.bool)
  use r_sent_at <- decode.field(4, decode.optional(decode.string))
  use t_id <- decode.field(5, decode.int)
  use t_owner <- decode.field(6, decode.int)
  use t_contact_id <- decode.field(7, decode.optional(decode.int))
  use t_contact_name <- decode.field(8, decode.optional(decode.string))
  use t_title <- decode.field(9, decode.string)
  use t_description <- decode.field(10, decode.optional(decode.string))
  use t_category <- decode.field(11, decode.string)
  use t_status <- decode.field(12, decode.string)
  use t_priority <- decode.field(13, decode.int)
  use t_responsibility <- decode.field(14, decode.string)
  use t_due_date <- decode.field(15, decode.optional(decode.string))
  use t_archived <- decode.field(16, decode.bool)
  use t_created_at <- decode.field(17, decode.string)
  use t_updated_at <- decode.field(18, decode.string)

  let reminder = TaskReminder(
    id: r_id,
    task_id: r_task_id,
    remind_at: r_remind_at,
    is_sent: r_is_sent,
    sent_at: r_sent_at,
  )

  let task = Task(
    id: t_id,
    owner_telegram_id: t_owner,
    contact_dialog_id: t_contact_id,
    contact_name: t_contact_name,
    title: t_title,
    description: t_description,
    category: string_to_category(t_category),
    status: string_to_status(t_status),
    priority: t_priority,
    responsibility: t_responsibility,
    due_date: t_due_date,
    archived: t_archived,
    created_at: t_created_at,
    updated_at: t_updated_at,
  )

  decode.success(#(reminder, task))
}

fn decode_status_history() -> Decoder(StatusHistoryEntry) {
  use id <- decode.field(0, decode.int)
  use task_id <- decode.field(1, decode.int)
  use old_status <- decode.field(2, decode.optional(decode.string))
  use new_status <- decode.field(3, decode.string)
  use changed_by <- decode.field(4, decode.optional(decode.string))
  use comment <- decode.field(5, decode.optional(decode.string))
  use changed_at <- decode.field(6, decode.string)

  decode.success(StatusHistoryEntry(
    id: id,
    task_id: task_id,
    old_status: old_status,
    new_status: new_status,
    changed_by: changed_by,
    comment: comment,
    changed_at: changed_at,
  ))
}

fn decode_stats() -> Decoder(TaskStats) {
  use total <- decode.field(0, decode.int)
  use pending <- decode.field(1, decode.int)
  use in_progress <- decode.field(2, decode.int)
  use waiting <- decode.field(3, decode.int)
  use completed <- decode.field(4, decode.int)
  use cancelled <- decode.field(5, decode.int)
  use overdue <- decode.field(6, decode.int)

  decode.success(TaskStats(
    total: total,
    pending: pending,
    in_progress: in_progress,
    waiting: waiting,
    completed: completed,
    cancelled: cancelled,
    overdue: overdue,
  ))
}
