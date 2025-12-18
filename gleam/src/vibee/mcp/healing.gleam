// Healing Actor - Self-Healing System for Rainbow Bridge
// Manages fix application, verification, and rollback

import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/logging
import vibee/mcp/rainbow_types.{
  type AppliedFix, type FileSnapshot, type SuggestedFix, type Task,
  type TaskContext, AppliedFix, BuildAttempted, FileSnapshot, FixApplied,
  FixVerified, Healing, Rollback, SuggestedFix, Task, TaskContext,
  encode_suggested_fix,
}
import vibee/mcp/task_store

/// Initialize healing system
pub fn init() -> Nil {
  init_ffi()
}

/// Start healing process for a task
pub fn start_healing(task_id: String) -> Result(Task, String) {
  case task_store.get(task_id) {
    None -> Error("Task not found: " <> task_id)
    Some(task) -> {
      logging.quick_info("[HEALING] Starting healing for task: " <> task_id)

      // Take snapshots of current files
      let snapshots = take_snapshots(task.context.files)
      let new_context = TaskContext(
        ..task.context,
        original_snapshots: snapshots,
        current_snapshots: snapshots,
      )

      let _ = task_store.update_context(task_id, new_context)
      let _ = task_store.update_state(task_id, Healing)

      task_store.get(task_id)
      |> option.to_result("Failed to get updated task")
    }
  }
}

/// Apply a fix to the codebase
pub fn apply_fix(task_id: String, fix: SuggestedFix) -> Result(AppliedFix, String) {
  case task_store.get(task_id) {
    None -> Error("Task not found: " <> task_id)
    Some(task) -> {
      logging.quick_info("[HEALING] Applying fix: " <> fix.id <> " to " <> fix.file_path)

      // Take snapshot before applying
      let snapshot = take_snapshot(fix.file_path)

      // Read current file
      case read_file_ffi(fix.file_path) {
        Error(e) -> Error("Failed to read file: " <> e)
        Ok(content) -> {
          // Apply the fix
          let new_content = apply_fix_to_content(content, fix)

          case new_content == content {
            True -> {
              // No change made - fix didn't apply
              let applied = AppliedFix(
                fix: fix,
                applied_at: get_timestamp_ffi(),
                verified: False,
                success: False,
              )
              Error("Fix did not produce any changes")
            }
            False -> {
              // Write the modified file
              case write_file_ffi(fix.file_path, new_content) {
                Error(e) -> Error("Failed to write file: " <> e)
                Ok(_) -> {
                  logging.quick_info("[HEALING] Fix applied successfully")

                  // Update snapshots
                  let new_snapshot = FileSnapshot(
                    path: fix.file_path,
                    content: new_content,
                    timestamp: get_timestamp_ffi(),
                  )
                  let updated_snapshots = update_snapshot_list(
                    task.context.current_snapshots,
                    new_snapshot,
                  )
                  let new_context = TaskContext(
                    ..task.context,
                    current_snapshots: updated_snapshots,
                  )
                  let _ = task_store.update_context(task_id, new_context)

                  let applied = AppliedFix(
                    fix: fix,
                    applied_at: get_timestamp_ffi(),
                    verified: False,
                    success: True,
                  )

                  // Add event to history
                  let _ = task_store.add_event(task_id, FixApplied(get_timestamp_ffi(), fix.id))
                  let _ = task_store.record_fix_attempt(task_id, applied)

                  Ok(applied)
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Verify if a fix was successful (build passes)
pub fn verify_fix(task_id: String, fix_id: String) -> Result(Bool, String) {
  case task_store.get(task_id) {
    None -> Error("Task not found: " <> task_id)
    Some(task) -> {
      logging.quick_info("[HEALING] Verifying fix: " <> fix_id)

      // Run build to check
      let build_result = run_build_ffi(get_project_path(task))

      let success = build_result.exit_code == 0

      // Add event
      let _ = task_store.add_event(task_id, FixVerified(get_timestamp_ffi(), fix_id, success))
      let _ = task_store.add_event(task_id, BuildAttempted(get_timestamp_ffi(), success, case success {
        True -> 0
        False -> count_errors(build_result.output)
      }))

      logging.quick_info("[HEALING] Verification result: " <> case success {
        True -> "SUCCESS"
        False -> "FAILED"
      })

      Ok(success)
    }
  }
}

/// Rollback to a previous snapshot
pub fn rollback(task_id: String, fix_id: String) -> Result(Nil, String) {
  case task_store.get(task_id) {
    None -> Error("Task not found: " <> task_id)
    Some(task) -> {
      logging.quick_info("[HEALING] Rolling back fix: " <> fix_id)

      // Find the fix that was applied
      let maybe_fix = list.find(task.context.fixes_applied, fn(f) {
        f.fix.id == fix_id
      })

      case maybe_fix {
        Error(_) -> Error("Fix not found in applied fixes: " <> fix_id)
        Ok(applied_fix) -> {
          // Find original snapshot for this file
          let file_path = applied_fix.fix.file_path
          let maybe_snapshot = list.find(task.context.original_snapshots, fn(s) {
            s.path == file_path
          })

          case maybe_snapshot {
            Error(_) -> Error("No original snapshot found for: " <> file_path)
            Ok(snapshot) -> {
              // Restore original content
              case write_file_ffi(file_path, snapshot.content) {
                Error(e) -> Error("Failed to restore file: " <> e)
                Ok(_) -> {
                  logging.quick_info("[HEALING] Rolled back to original: " <> file_path)

                  // Update task
                  let _ = task_store.add_event(task_id,
                    Rollback(get_timestamp_ffi(), fix_id, "Manual rollback"))
                  let _ = task_store.record_rollback(task_id)

                  Ok(Nil)
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Rollback all changes to original state
pub fn rollback_all(task_id: String) -> Result(Int, String) {
  case task_store.get(task_id) {
    None -> Error("Task not found: " <> task_id)
    Some(task) -> {
      logging.quick_info("[HEALING] Rolling back all changes for task: " <> task_id)

      let rolled_back = list.fold(task.context.original_snapshots, 0, fn(count, snapshot) {
        case write_file_ffi(snapshot.path, snapshot.content) {
          Ok(_) -> count + 1
          Error(_) -> count
        }
      })

      logging.quick_info("[HEALING] Rolled back " <> int.to_string(rolled_back) <> " files")
      Ok(rolled_back)
    }
  }
}

/// Get the diff between original and current for a file
pub fn get_diff(task_id: String, file_path: String) -> Result(String, String) {
  case task_store.get(task_id) {
    None -> Error("Task not found: " <> task_id)
    Some(task) -> {
      let orig = list.find(task.context.original_snapshots, fn(s) { s.path == file_path })
      let curr = list.find(task.context.current_snapshots, fn(s) { s.path == file_path })

      case orig, curr {
        Ok(o), Ok(c) -> {
          // Generate simple diff
          Ok(generate_diff(o.content, c.content))
        }
        _, _ -> Error("Snapshots not found for: " <> file_path)
      }
    }
  }
}

// === Internal Helpers ===

fn take_snapshots(files: List(String)) -> List(FileSnapshot) {
  list.filter_map(files, fn(path) {
    case take_snapshot(path) {
      s if s.content != "" -> Ok(s)
      _ -> Error(Nil)
    }
  })
}

fn take_snapshot(path: String) -> FileSnapshot {
  case read_file_ffi(path) {
    Ok(content) -> FileSnapshot(
      path: path,
      content: content,
      timestamp: get_timestamp_ffi(),
    )
    Error(_) -> FileSnapshot(
      path: path,
      content: "",
      timestamp: get_timestamp_ffi(),
    )
  }
}

fn update_snapshot_list(
  snapshots: List(FileSnapshot),
  new_snapshot: FileSnapshot,
) -> List(FileSnapshot) {
  let filtered = list.filter(snapshots, fn(s) { s.path != new_snapshot.path })
  [new_snapshot, ..filtered]
}

fn apply_fix_to_content(content: String, fix: SuggestedFix) -> String {
  case fix.old_code, fix.new_code {
    "", "" -> content  // No replacement specified
    "", new_code -> {
      // Insert at line
      insert_at_line(content, fix.line_number, new_code)
    }
    old_code, new_code -> {
      // Replace old with new
      string.replace(content, old_code, new_code)
    }
  }
}

fn insert_at_line(content: String, line_number: Int, insert: String) -> String {
  let lines = string.split(content, "\n")
  let indexed = list.index_map(lines, fn(line, idx) { #(idx + 1, line) })
  let new_lines = list.flat_map(indexed, fn(pair) {
    let #(idx, line) = pair
    case idx == line_number {
      True -> [line, insert]
      False -> [line]
    }
  })
  string.join(new_lines, "\n")
}

fn get_project_path(task: Task) -> String {
  case task.context.files {
    [first, ..] -> get_directory(first)
    [] -> "."
  }
}

fn get_directory(path: String) -> String {
  case string.split(path, "/") {
    [] -> "."
    parts -> {
      let without_file = list.take(parts, list.length(parts) - 1)
      case without_file {
        [] -> "."
        _ -> string.join(without_file, "/")
      }
    }
  }
}

fn count_errors(output: String) -> Int {
  let lines = string.split(output, "\n")
  list.count(lines, fn(line) { string.starts_with(line, "error:") })
}

fn generate_diff(original: String, current: String) -> String {
  case original == current {
    True -> "No changes"
    False -> {
      let orig_lines = string.split(original, "\n")
      let curr_lines = string.split(current, "\n")
      "--- original\n+++ current\n"
      <> "@@ Changes detected @@\n"
      <> "Original: " <> int.to_string(list.length(orig_lines)) <> " lines\n"
      <> "Current: " <> int.to_string(list.length(curr_lines)) <> " lines"
    }
  }
}

// === FFI ===

type BuildOutput {
  BuildOutput(exit_code: Int, output: String)
}

@external(erlang, "vibee_healing_ffi", "init")
fn init_ffi() -> Nil

@external(erlang, "vibee_healing_ffi", "read_file")
fn read_file_ffi(path: String) -> Result(String, String)

@external(erlang, "vibee_healing_ffi", "write_file")
fn write_file_ffi(path: String, content: String) -> Result(Nil, String)

@external(erlang, "vibee_healing_ffi", "run_build")
fn run_build_ffi(path: String) -> BuildOutput

@external(erlang, "vibee_healing_ffi", "get_timestamp")
fn get_timestamp_ffi() -> Int
