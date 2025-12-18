// Decision Engine - Rainbow Bridge Intelligence
// Determines the next action for autonomous agents

import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import vibee/logging
import vibee/mcp/rainbow_types.{
  type AppliedFix, type CompileError, type Decision, type DecisionAction,
  type SuggestedFix, type Task, type TaskMetrics, ApplyFix, AskHuman, Completed,
  Decision, Failed, Healing, InProgress, MarkComplete, MarkFailed, Pending,
  RetryBuild, RollbackFix, RunTests, WaitForFeedback, WaitingFeedback,
  encode_decision,
}
import vibee/mcp/task_store

/// Decide the next action for a task
pub fn decide_next_step(task_id: String) -> Result(Decision, String) {
  case task_store.get(task_id) {
    None -> Error("Task not found: " <> task_id)
    Some(task) -> {
      let decision = analyze_task_and_decide(task)
      logging.quick_info("[DECISION] Task " <> task_id <> " -> " <> action_to_string(decision.action))
      Ok(decision)
    }
  }
}

/// Main decision logic
fn analyze_task_and_decide(task: Task) -> Decision {
  let metrics = task.metrics
  let context = task.context
  let errors = context.errors
  let fixes = context.fixes_applied

  // Decision tree based on task state and metrics
  case task.state {
    Completed -> make_decision("already_complete", MarkComplete, 1.0,
      "Task is already completed")

    Failed -> make_decision("already_failed", MarkFailed("Task marked as failed"), 1.0,
      "Task has failed")

    Pending -> make_decision("start", RetryBuild, 0.95,
      "Start with a build to assess current state")

    WaitingFeedback -> make_decision("waiting", WaitForFeedback, 0.9,
      "Waiting for external feedback")

    InProgress | Healing -> decide_during_execution(task, metrics, errors, fixes)
  }
}

/// Decision logic during active execution
fn decide_during_execution(
  task: Task,
  metrics: TaskMetrics,
  errors: List(CompileError),
  fixes: List(AppliedFix),
) -> Decision {
  let error_count = list.length(errors)
  let fixes_count = list.length(fixes)

  // Check for success
  case error_count {
    0 -> {
      // No errors - run tests or complete
      case metrics.iterations > 0 {
        True -> make_decision("success", RunTests, 0.95,
          "No compile errors - run tests to verify")
        False -> make_decision("initial_success", MarkComplete, 0.9,
          "Build successful with no changes needed")
      }
    }
    _ -> {
      // Still have errors
      decide_with_errors(task, metrics, errors, fixes)
    }
  }
}

/// Decision when there are still errors
fn decide_with_errors(
  task: Task,
  metrics: TaskMetrics,
  errors: List(CompileError),
  fixes: List(AppliedFix),
) -> Decision {
  let error_count = list.length(errors)
  let fixes_count = list.length(fixes)
  let failed_fixes = list.filter(fixes, fn(f) { !f.success })
  let failed_count = list.length(failed_fixes)

  // Check iteration limit
  case metrics.iterations >= 10 {
    True -> make_decision("max_iterations", AskHuman(
      "Reached maximum iterations (" <> int.to_string(metrics.iterations)
      <> ") with " <> int.to_string(error_count) <> " errors remaining. "
      <> "Please review and provide guidance."
    ), 0.95, "Max iterations reached, need human input")

    False -> {
      // Check if we're making progress
      case check_progress(metrics, error_count) {
        NoProgress -> {
          case failed_count > 2 {
            True -> make_decision("stuck", AskHuman(
              "Multiple fix attempts failed (" <> int.to_string(failed_count)
              <> "). Errors remaining: " <> int.to_string(error_count)
              <> ". Need guidance on approach."
            ), 0.85, "Multiple failures, need human guidance")

            False -> {
              // Try rollback if last fix failed
              case list.last(fixes) {
                Ok(last_fix) -> {
                  case last_fix.success {
                    False -> make_decision("rollback_failed", RollbackFix(last_fix.fix.id), 0.8,
                      "Last fix failed, rolling back")
                    True -> make_decision("retry_analyze", RetryBuild, 0.75,
                      "Retrying build to get fresh errors")
                  }
                }
                Error(_) ->
                  make_decision("retry_analyze", RetryBuild, 0.75,
                    "Retrying build to get fresh errors")
              }
            }
          }
        }

        SlowProgress -> {
          // Making some progress, continue
          make_decision("continue", RetryBuild, 0.7,
            "Making slow progress, continuing cycle")
        }

        GoodProgress -> {
          // Good progress, continue confidently
          make_decision("continue_good", RetryBuild, 0.85,
            "Good progress, continuing cycle")
        }
      }
    }
  }
}

/// Check if we're making progress
type ProgressStatus {
  NoProgress
  SlowProgress
  GoodProgress
}

fn check_progress(metrics: TaskMetrics, current_errors: Int) -> ProgressStatus {
  // Compare with initial state
  case metrics.iterations {
    0 | 1 -> GoodProgress  // Just started
    _ -> {
      let fix_rate = case metrics.fixes_attempted {
        0 -> 0.0
        n -> int.to_float(metrics.fixes_successful) /. int.to_float(n)
      }

      case fix_rate >=. 0.5 {
        True -> GoodProgress
        False -> case fix_rate >=. 0.25 {
          True -> SlowProgress
          False -> NoProgress
        }
      }
    }
  }
}

/// Create a decision
fn make_decision(
  id: String,
  action: DecisionAction,
  confidence: Float,
  reasoning: String,
) -> Decision {
  Decision(
    id: generate_decision_id(id),
    action: action,
    confidence: confidence,
    reasoning: reasoning,
  )
}

/// Apply a decision
pub fn apply_decision(task_id: String, decision: Decision) -> Result(String, String) {
  case task_store.get(task_id) {
    None -> Error("Task not found: " <> task_id)
    Some(task) -> {
      logging.quick_info("[DECISION] Applying: " <> decision.id)

      case decision.action {
        MarkComplete -> {
          let _ = task_store.update_state(task_id, Completed)
          Ok("Task marked as completed")
        }

        MarkFailed(reason) -> {
          let _ = task_store.update_state(task_id, Failed)
          Ok("Task marked as failed: " <> reason)
        }

        RunTests -> {
          // Trigger test run
          Ok("Tests triggered")
        }

        RetryBuild -> {
          // Will trigger build in autonomous cycle
          Ok("Build retry scheduled")
        }

        RollbackFix(fix_id) -> {
          let _ = task_store.record_rollback(task_id)
          Ok("Rollback initiated for fix: " <> fix_id)
        }

        ApplyFix(fix) -> {
          Ok("Fix application initiated: " <> fix.id)
        }

        AskHuman(question) -> {
          let _ = task_store.update_state(task_id, WaitingFeedback)
          Ok("Awaiting human input: " <> question)
        }

        WaitForFeedback -> {
          Ok("Waiting for feedback")
        }
      }
    }
  }
}

/// Suggest fixes based on task context
pub fn suggest_fixes(task_id: String) -> Result(List(SuggestedFix), String) {
  case task_store.get(task_id) {
    None -> Error("Task not found: " <> task_id)
    Some(task) -> {
      // Generate fix suggestions based on errors
      let fixes = list.flat_map(task.context.errors, suggest_fix_for_error)
      Ok(fixes)
    }
  }
}

fn suggest_fix_for_error(error: CompileError) -> List(SuggestedFix) {
  // Use the autonomous module's error analysis
  // This is a simplified version
  [rainbow_types.SuggestedFix(
    id: generate_decision_id("fix"),
    file_path: error.file,
    line_number: error.line,
    fix_type: rainbow_types.UnknownFix,
    old_code: "",
    new_code: "",
    confidence: 0.5,
    explanation: "Suggested fix for: " <> error.message,
  )]
}

/// Get confidence threshold for automatic action
pub fn get_confidence_threshold() -> Float {
  0.7
}

/// Check if decision should be auto-applied
pub fn should_auto_apply(decision: Decision) -> Bool {
  case decision.action {
    AskHuman(_) -> False
    WaitForFeedback -> False
    _ -> decision.confidence >=. get_confidence_threshold()
  }
}

// === Helpers ===

fn action_to_string(action: DecisionAction) -> String {
  case action {
    ApplyFix(fix) -> "apply_fix:" <> fix.id
    RunTests -> "run_tests"
    RollbackFix(fix_id) -> "rollback:" <> fix_id
    AskHuman(_) -> "ask_human"
    MarkComplete -> "mark_complete"
    MarkFailed(_) -> "mark_failed"
    RetryBuild -> "retry_build"
    WaitForFeedback -> "wait_feedback"
  }
}

fn generate_decision_id(prefix: String) -> String {
  let ts = get_timestamp_ffi()
  prefix <> "_" <> int.to_string(ts)
}

// === FFI ===

@external(erlang, "vibee_task_store_ffi", "get_timestamp")
fn get_timestamp_ffi() -> Int
