// Rainbow Bridge Types
// Types for autonomous agent self-healing and task management
// Part of P6: Rainbow Bridge implementation

import gleam/json
import gleam/option.{type Option}

/// Task state in the lifecycle
pub type TaskState {
  Pending
  InProgress
  WaitingFeedback
  Healing
  Completed
  Failed
}

/// A task with full context for autonomous processing
pub type Task {
  Task(
    id: String,
    description: String,
    state: TaskState,
    context: TaskContext,
    metrics: TaskMetrics,
    history: List(TaskEvent),
    created_at: Int,
    updated_at: Int,
  )
}

/// Task context with file snapshots and error tracking
pub type TaskContext {
  TaskContext(
    files: List(String),
    original_snapshots: List(FileSnapshot),
    current_snapshots: List(FileSnapshot),
    errors: List(CompileError),
    fixes_applied: List(AppliedFix),
  )
}

/// Snapshot of file content for rollback support
pub type FileSnapshot {
  FileSnapshot(
    path: String,
    content: String,
    timestamp: Int,
  )
}

/// Task metrics for monitoring
pub type TaskMetrics {
  TaskMetrics(
    iterations: Int,
    errors_fixed: Int,
    errors_remaining: Int,
    total_duration_ms: Int,
    fixes_attempted: Int,
    fixes_successful: Int,
    rollbacks: Int,
  )
}

/// Task event for history tracking
pub type TaskEvent {
  TaskCreated(timestamp: Int)
  TaskStarted(timestamp: Int)
  BuildAttempted(timestamp: Int, success: Bool, error_count: Int)
  FixSuggested(timestamp: Int, fix_id: String, confidence: Float)
  FixApplied(timestamp: Int, fix_id: String)
  FixVerified(timestamp: Int, fix_id: String, success: Bool)
  Rollback(timestamp: Int, fix_id: String, reason: String)
  TaskCompleted(timestamp: Int, success: Bool)
  TaskFailed(timestamp: Int, reason: String)
}

/// Compile error from build
pub type CompileError {
  CompileError(
    file: String,
    line: Int,
    column: Int,
    error_type: String,
    message: String,
    code: Option(String),
  )
}

/// Suggested fix from analysis
pub type SuggestedFix {
  SuggestedFix(
    id: String,
    file_path: String,
    line_number: Int,
    fix_type: FixType,
    old_code: String,
    new_code: String,
    confidence: Float,
    explanation: String,
  )
}

/// Type of fix
pub type FixType {
  SyntaxFix
  TypeFix
  ImportFix
  LogicFix
  RefactorFix
  UnknownFix
}

/// Applied fix with result
pub type AppliedFix {
  AppliedFix(
    fix: SuggestedFix,
    applied_at: Int,
    verified: Bool,
    success: Bool,
  )
}

/// Result of a debug iteration
pub type DebugIteration {
  DebugIteration(
    iteration: Int,
    build_result: BuildResult,
    fixes_suggested: List(SuggestedFix),
    fixes_applied: List(SuggestedFix),
    remaining_errors: Int,
  )
}

/// Build result
pub type BuildResult {
  BuildSuccess(output: String)
  BuildFailure(errors: List(CompileError), raw_output: String)
}

/// Decision about next action
pub type Decision {
  Decision(
    id: String,
    action: DecisionAction,
    confidence: Float,
    reasoning: String,
  )
}

/// Possible actions for decision engine
pub type DecisionAction {
  ApplyFix(SuggestedFix)
  RunTests
  RollbackFix(fix_id: String)
  AskHuman(question: String)
  MarkComplete
  MarkFailed(reason: String)
  RetryBuild
  WaitForFeedback
}

/// Report from autonomous debug cycle
pub type DebugCycleReport {
  DebugCycleReport(
    task_id: String,
    success: Bool,
    iterations: Int,
    errors_fixed: Int,
    errors_remaining: Int,
    fixes_applied: List(AppliedFix),
    final_build_output: String,
    duration_ms: Int,
    history: List(DebugIteration),
  )
}

/// Async operation status
pub type AsyncOperationStatus {
  AsyncPending
  AsyncRunning(progress: Int, message: String)
  AsyncCompleted(result: String)
  AsyncFailed(error: String)
  AsyncCancelled
}

/// Async operation
pub type AsyncOperation {
  AsyncOperation(
    id: String,
    operation_type: String,
    status: AsyncOperationStatus,
    created_at: Int,
    updated_at: Int,
    result: Option(String),
  )
}

/// Bot test plan
pub type BotTestPlan {
  BotTestPlan(
    id: String,
    bot_username: String,
    interactions: List(TestInteraction),
    timeout_ms: Int,
    created_at: Int,
  )
}

/// Test interaction step
pub type TestInteraction {
  TestInteraction(
    step: Int,
    action: TestAction,
    expected_pattern: Option(String),
    timeout_ms: Int,
  )
}

/// Test action types
pub type TestAction {
  SendCommand(command: String)
  SendText(text: String)
  ClickButton(button_text: String)
  WaitForMessage
  WaitForButtons
}

/// Bot test result
pub type BotTestResult {
  BotTestResult(
    plan_id: String,
    success: Bool,
    steps_passed: Int,
    steps_failed: Int,
    results: List(StepResult),
    duration_ms: Int,
  )
}

/// Result of a single test step
pub type StepResult {
  StepResult(
    step: Int,
    passed: Bool,
    expected: Option(String),
    actual: Option(String),
    error: Option(String),
    duration_ms: Int,
  )
}

// === JSON Encoding ===

/// Encode task state to string
pub fn task_state_to_string(state: TaskState) -> String {
  case state {
    Pending -> "pending"
    InProgress -> "in_progress"
    WaitingFeedback -> "waiting_feedback"
    Healing -> "healing"
    Completed -> "completed"
    Failed -> "failed"
  }
}

/// Parse task state from string
pub fn parse_task_state(s: String) -> TaskState {
  case s {
    "pending" -> Pending
    "in_progress" -> InProgress
    "waiting_feedback" -> WaitingFeedback
    "healing" -> Healing
    "completed" -> Completed
    "failed" -> Failed
    _ -> Pending
  }
}

/// Encode fix type to string
pub fn fix_type_to_string(ft: FixType) -> String {
  case ft {
    SyntaxFix -> "syntax"
    TypeFix -> "type"
    ImportFix -> "import"
    LogicFix -> "logic"
    RefactorFix -> "refactor"
    UnknownFix -> "unknown"
  }
}

/// Parse fix type from string
pub fn parse_fix_type(s: String) -> FixType {
  case s {
    "syntax" -> SyntaxFix
    "type" -> TypeFix
    "import" -> ImportFix
    "logic" -> LogicFix
    "refactor" -> RefactorFix
    _ -> UnknownFix
  }
}

/// Encode debug cycle report to JSON
pub fn encode_debug_report(report: DebugCycleReport) -> json.Json {
  json.object([
    #("task_id", json.string(report.task_id)),
    #("success", json.bool(report.success)),
    #("iterations", json.int(report.iterations)),
    #("errors_fixed", json.int(report.errors_fixed)),
    #("errors_remaining", json.int(report.errors_remaining)),
    #("fixes_applied", json.array(report.fixes_applied, encode_applied_fix)),
    #("final_build_output", json.string(report.final_build_output)),
    #("duration_ms", json.int(report.duration_ms)),
  ])
}

/// Encode applied fix to JSON
pub fn encode_applied_fix(fix: AppliedFix) -> json.Json {
  json.object([
    #("fix_id", json.string(fix.fix.id)),
    #("file_path", json.string(fix.fix.file_path)),
    #("line_number", json.int(fix.fix.line_number)),
    #("fix_type", json.string(fix_type_to_string(fix.fix.fix_type))),
    #("confidence", json.float(fix.fix.confidence)),
    #("explanation", json.string(fix.fix.explanation)),
    #("applied_at", json.int(fix.applied_at)),
    #("verified", json.bool(fix.verified)),
    #("success", json.bool(fix.success)),
  ])
}

/// Encode suggested fix to JSON
pub fn encode_suggested_fix(fix: SuggestedFix) -> json.Json {
  json.object([
    #("id", json.string(fix.id)),
    #("file_path", json.string(fix.file_path)),
    #("line_number", json.int(fix.line_number)),
    #("fix_type", json.string(fix_type_to_string(fix.fix_type))),
    #("old_code", json.string(fix.old_code)),
    #("new_code", json.string(fix.new_code)),
    #("confidence", json.float(fix.confidence)),
    #("explanation", json.string(fix.explanation)),
  ])
}

/// Encode task to JSON
pub fn encode_task(task: Task) -> json.Json {
  json.object([
    #("id", json.string(task.id)),
    #("description", json.string(task.description)),
    #("state", json.string(task_state_to_string(task.state))),
    #("metrics", encode_task_metrics(task.metrics)),
    #("created_at", json.int(task.created_at)),
    #("updated_at", json.int(task.updated_at)),
  ])
}

/// Encode task metrics to JSON
pub fn encode_task_metrics(m: TaskMetrics) -> json.Json {
  json.object([
    #("iterations", json.int(m.iterations)),
    #("errors_fixed", json.int(m.errors_fixed)),
    #("errors_remaining", json.int(m.errors_remaining)),
    #("total_duration_ms", json.int(m.total_duration_ms)),
    #("fixes_attempted", json.int(m.fixes_attempted)),
    #("fixes_successful", json.int(m.fixes_successful)),
    #("rollbacks", json.int(m.rollbacks)),
  ])
}

/// Encode compile error to JSON
pub fn encode_compile_error(e: CompileError) -> json.Json {
  json.object([
    #("file", json.string(e.file)),
    #("line", json.int(e.line)),
    #("column", json.int(e.column)),
    #("error_type", json.string(e.error_type)),
    #("message", json.string(e.message)),
  ])
}

/// Encode decision to JSON
pub fn encode_decision(d: Decision) -> json.Json {
  json.object([
    #("id", json.string(d.id)),
    #("action", encode_decision_action(d.action)),
    #("confidence", json.float(d.confidence)),
    #("reasoning", json.string(d.reasoning)),
  ])
}

/// Encode decision action to JSON
pub fn encode_decision_action(action: DecisionAction) -> json.Json {
  case action {
    ApplyFix(fix) -> json.object([
      #("type", json.string("apply_fix")),
      #("fix", encode_suggested_fix(fix)),
    ])
    RunTests -> json.object([#("type", json.string("run_tests"))])
    RollbackFix(fix_id) -> json.object([
      #("type", json.string("rollback")),
      #("fix_id", json.string(fix_id)),
    ])
    AskHuman(question) -> json.object([
      #("type", json.string("ask_human")),
      #("question", json.string(question)),
    ])
    MarkComplete -> json.object([#("type", json.string("mark_complete"))])
    MarkFailed(reason) -> json.object([
      #("type", json.string("mark_failed")),
      #("reason", json.string(reason)),
    ])
    RetryBuild -> json.object([#("type", json.string("retry_build"))])
    WaitForFeedback -> json.object([#("type", json.string("wait_for_feedback"))])
  }
}

/// Encode async operation status to JSON
pub fn encode_async_status(status: AsyncOperationStatus) -> json.Json {
  case status {
    AsyncPending -> json.object([#("status", json.string("pending"))])
    AsyncRunning(progress, message) -> json.object([
      #("status", json.string("running")),
      #("progress", json.int(progress)),
      #("message", json.string(message)),
    ])
    AsyncCompleted(result) -> json.object([
      #("status", json.string("completed")),
      #("result", json.string(result)),
    ])
    AsyncFailed(error) -> json.object([
      #("status", json.string("failed")),
      #("error", json.string(error)),
    ])
    AsyncCancelled -> json.object([#("status", json.string("cancelled"))])
  }
}

/// Encode bot test result to JSON
pub fn encode_bot_test_result(result: BotTestResult) -> json.Json {
  json.object([
    #("plan_id", json.string(result.plan_id)),
    #("success", json.bool(result.success)),
    #("steps_passed", json.int(result.steps_passed)),
    #("steps_failed", json.int(result.steps_failed)),
    #("duration_ms", json.int(result.duration_ms)),
    #("results", json.array(result.results, encode_step_result)),
  ])
}

/// Encode step result to JSON
pub fn encode_step_result(r: StepResult) -> json.Json {
  json.object([
    #("step", json.int(r.step)),
    #("passed", json.bool(r.passed)),
    #("duration_ms", json.int(r.duration_ms)),
  ])
}

// === Helper Functions ===

/// Create empty task context
pub fn empty_context() -> TaskContext {
  TaskContext(
    files: [],
    original_snapshots: [],
    current_snapshots: [],
    errors: [],
    fixes_applied: [],
  )
}

/// Create empty task metrics
pub fn empty_metrics() -> TaskMetrics {
  TaskMetrics(
    iterations: 0,
    errors_fixed: 0,
    errors_remaining: 0,
    total_duration_ms: 0,
    fixes_attempted: 0,
    fixes_successful: 0,
    rollbacks: 0,
  )
}

/// Create new task
pub fn new_task(id: String, description: String, files: List(String), now: Int) -> Task {
  Task(
    id: id,
    description: description,
    state: Pending,
    context: TaskContext(..empty_context(), files: files),
    metrics: empty_metrics(),
    history: [TaskCreated(now)],
    created_at: now,
    updated_at: now,
  )
}

/// Check if build was successful
pub fn is_build_success(result: BuildResult) -> Bool {
  case result {
    BuildSuccess(_) -> True
    BuildFailure(_, _) -> False
  }
}

/// Get errors from build result
pub fn get_build_errors(result: BuildResult) -> List(CompileError) {
  case result {
    BuildSuccess(_) -> []
    BuildFailure(errors, _) -> errors
  }
}
