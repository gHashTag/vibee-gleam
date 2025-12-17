// Task Store - ETS-backed task management for Rainbow Bridge
// Provides CRUD operations and subscriptions for autonomous tasks

import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import vibee/mcp/rainbow_types.{
  type AppliedFix, type Task, type TaskContext, type TaskEvent, type TaskMetrics,
  type TaskState, Completed, Failed, Healing, InProgress, Pending, Task,
  TaskCompleted, TaskContext, TaskFailed, TaskMetrics, TaskStarted,
  WaitingFeedback, empty_context, empty_metrics, encode_task, new_task,
  task_state_to_string,
}

/// Initialize the task store
pub fn init() -> Nil {
  init_ffi()
}

/// Create a new task
pub fn create(description: String, files: List(String)) -> Task {
  let id = generate_task_id()
  let now = get_timestamp()
  let task = new_task(id, description, files, now)
  store_task(task)
  task
}

/// Get task by ID
pub fn get(task_id: String) -> Option(Task) {
  get_task_ffi(task_id)
}

/// Update task state
pub fn update_state(task_id: String, new_state: TaskState) -> Result(Task, String) {
  case get(task_id) {
    Some(task) -> {
      let now = get_timestamp()
      let event = case new_state {
        InProgress -> TaskStarted(now)
        Completed -> TaskCompleted(now, True)
        Failed -> TaskFailed(now, "Task marked as failed")
        _ -> TaskStarted(now)
      }
      let updated = Task(
        ..task,
        state: new_state,
        history: list.append(task.history, [event]),
        updated_at: now,
      )
      store_task(updated)
      notify_subscribers(task_id, "state_changed", task_state_to_string(new_state))
      Ok(updated)
    }
    None -> Error("Task not found: " <> task_id)
  }
}

/// Update task context
pub fn update_context(task_id: String, context: TaskContext) -> Result(Task, String) {
  case get(task_id) {
    Some(task) -> {
      let now = get_timestamp()
      let updated = Task(
        ..task,
        context: context,
        updated_at: now,
      )
      store_task(updated)
      notify_subscribers(task_id, "context_updated", "")
      Ok(updated)
    }
    None -> Error("Task not found: " <> task_id)
  }
}

/// Update task metrics
pub fn update_metrics(task_id: String, metrics: TaskMetrics) -> Result(Task, String) {
  case get(task_id) {
    Some(task) -> {
      let now = get_timestamp()
      let updated = Task(
        ..task,
        metrics: metrics,
        updated_at: now,
      )
      store_task(updated)
      notify_subscribers(task_id, "metrics_updated", "")
      Ok(updated)
    }
    None -> Error("Task not found: " <> task_id)
  }
}

/// Add event to task history
pub fn add_event(task_id: String, event: TaskEvent) -> Result(Task, String) {
  case get(task_id) {
    Some(task) -> {
      let now = get_timestamp()
      let updated = Task(
        ..task,
        history: list.append(task.history, [event]),
        updated_at: now,
      )
      store_task(updated)
      Ok(updated)
    }
    None -> Error("Task not found: " <> task_id)
  }
}

/// Record a fix attempt
pub fn record_fix_attempt(task_id: String, fix: AppliedFix) -> Result(Task, String) {
  case get(task_id) {
    Some(task) -> {
      let now = get_timestamp()
      let new_context = TaskContext(
        ..task.context,
        fixes_applied: list.append(task.context.fixes_applied, [fix]),
      )
      let new_metrics = TaskMetrics(
        ..task.metrics,
        fixes_attempted: task.metrics.fixes_attempted + 1,
        fixes_successful: case fix.success {
          True -> task.metrics.fixes_successful + 1
          False -> task.metrics.fixes_successful
        },
      )
      let updated = Task(
        ..task,
        context: new_context,
        metrics: new_metrics,
        updated_at: now,
      )
      store_task(updated)
      notify_subscribers(task_id, "fix_applied", fix.fix.id)
      Ok(updated)
    }
    None -> Error("Task not found: " <> task_id)
  }
}

/// Increment iteration count
pub fn increment_iteration(task_id: String) -> Result(Task, String) {
  case get(task_id) {
    Some(task) -> {
      let now = get_timestamp()
      let new_metrics = TaskMetrics(
        ..task.metrics,
        iterations: task.metrics.iterations + 1,
      )
      let updated = Task(
        ..task,
        metrics: new_metrics,
        updated_at: now,
      )
      store_task(updated)
      Ok(updated)
    }
    None -> Error("Task not found: " <> task_id)
  }
}

/// Update error counts
pub fn update_error_counts(
  task_id: String,
  fixed: Int,
  remaining: Int,
) -> Result(Task, String) {
  case get(task_id) {
    Some(task) -> {
      let now = get_timestamp()
      let new_metrics = TaskMetrics(
        ..task.metrics,
        errors_fixed: task.metrics.errors_fixed + fixed,
        errors_remaining: remaining,
      )
      let updated = Task(
        ..task,
        metrics: new_metrics,
        updated_at: now,
      )
      store_task(updated)
      Ok(updated)
    }
    None -> Error("Task not found: " <> task_id)
  }
}

/// Record rollback
pub fn record_rollback(task_id: String) -> Result(Task, String) {
  case get(task_id) {
    Some(task) -> {
      let now = get_timestamp()
      let new_metrics = TaskMetrics(
        ..task.metrics,
        rollbacks: task.metrics.rollbacks + 1,
      )
      let updated = Task(
        ..task,
        metrics: new_metrics,
        updated_at: now,
      )
      store_task(updated)
      notify_subscribers(task_id, "rollback", "")
      Ok(updated)
    }
    None -> Error("Task not found: " <> task_id)
  }
}

/// Complete task
pub fn complete(task_id: String, success: Bool, duration_ms: Int) -> Result(Task, String) {
  case get(task_id) {
    Some(task) -> {
      let now = get_timestamp()
      let new_state = case success {
        True -> Completed
        False -> Failed
      }
      let new_metrics = TaskMetrics(
        ..task.metrics,
        total_duration_ms: duration_ms,
      )
      let event = TaskCompleted(now, success)
      let updated = Task(
        ..task,
        state: new_state,
        metrics: new_metrics,
        history: list.append(task.history, [event]),
        updated_at: now,
      )
      store_task(updated)
      notify_subscribers(task_id, "completed", case success {
        True -> "success"
        False -> "failed"
      })
      Ok(updated)
    }
    None -> Error("Task not found: " <> task_id)
  }
}

/// List all tasks
pub fn list_all() -> List(Task) {
  list_tasks_ffi(None)
}

/// List tasks by state
pub fn list_by_state(state: TaskState) -> List(Task) {
  list_tasks_ffi(Some(task_state_to_string(state)))
}

/// Delete task
pub fn delete(task_id: String) -> Bool {
  delete_task_ffi(task_id)
}

/// Subscribe to task updates
pub fn subscribe(task_id: String, subscriber_id: String) -> Nil {
  subscribe_ffi(task_id, subscriber_id)
}

/// Unsubscribe from task updates
pub fn unsubscribe(task_id: String, subscriber_id: String) -> Nil {
  unsubscribe_ffi(task_id, subscriber_id)
}

/// Get task stats
pub fn get_stats() -> json.Json {
  let tasks = list_all()
  let pending = list.filter(tasks, fn(t) { t.state == Pending })
  let in_progress = list.filter(tasks, fn(t) { t.state == InProgress })
  let completed = list.filter(tasks, fn(t) { t.state == Completed })
  let failed = list.filter(tasks, fn(t) { t.state == Failed })
  let healing = list.filter(tasks, fn(t) { t.state == Healing })

  json.object([
    #("total", json.int(list.length(tasks))),
    #("pending", json.int(list.length(pending))),
    #("in_progress", json.int(list.length(in_progress))),
    #("healing", json.int(list.length(healing))),
    #("completed", json.int(list.length(completed))),
    #("failed", json.int(list.length(failed))),
  ])
}

// === Internal Helpers ===

fn store_task(task: Task) -> Nil {
  store_task_ffi(task)
}

fn notify_subscribers(task_id: String, event_type: String, data: String) -> Nil {
  notify_subscribers_ffi(task_id, event_type, data)
}

// === FFI Functions ===

@external(erlang, "vibee_task_store_ffi", "init")
fn init_ffi() -> Nil

@external(erlang, "vibee_task_store_ffi", "generate_task_id")
fn generate_task_id() -> String

@external(erlang, "vibee_task_store_ffi", "get_timestamp")
fn get_timestamp() -> Int

@external(erlang, "vibee_task_store_ffi", "store_task")
fn store_task_ffi(task: Task) -> Nil

@external(erlang, "vibee_task_store_ffi", "get_task")
fn get_task_ffi(task_id: String) -> Option(Task)

@external(erlang, "vibee_task_store_ffi", "list_tasks")
fn list_tasks_ffi(state_filter: Option(String)) -> List(Task)

@external(erlang, "vibee_task_store_ffi", "delete_task")
fn delete_task_ffi(task_id: String) -> Bool

@external(erlang, "vibee_task_store_ffi", "subscribe")
fn subscribe_ffi(task_id: String, subscriber_id: String) -> Nil

@external(erlang, "vibee_task_store_ffi", "unsubscribe")
fn unsubscribe_ffi(task_id: String, subscriber_id: String) -> Nil

@external(erlang, "vibee_task_store_ffi", "notify_subscribers")
fn notify_subscribers_ffi(task_id: String, event_type: String, data: String) -> Nil
