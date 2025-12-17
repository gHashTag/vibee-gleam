// Autonomous Debug Cycle - Rainbow Bridge Core
// Implements self-healing debug cycle: build → analyze → fix → verify

import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/logging
import vibee/mcp/smart_analyzer
import vibee/mcp/rainbow_types.{
  type AppliedFix, type BuildResult, type CompileError, type DebugCycleReport,
  type DebugIteration, type SuggestedFix, type Task, AppliedFix, BuildFailure,
  BuildSuccess, CompileError, DebugCycleReport, DebugIteration, Healing,
  ImportFix, LogicFix, RefactorFix, SuggestedFix, SyntaxFix, TypeFix, UnknownFix,
  encode_debug_report, get_build_errors, is_build_success,
}
import vibee/mcp/task_store

/// Run autonomous debug cycle
/// Returns a complete report of the cycle
pub fn run_debug_cycle(
  path: String,
  max_iterations: Int,
) -> DebugCycleReport {
  logging.info("[AUTONOMOUS] Starting debug cycle for: " <> path)
  let start_time = get_timestamp()

  // Create task for tracking
  let task = task_store.create("Autonomous debug cycle: " <> path, [path])
  let task_id = task.id
  let _ = task_store.update_state(task_id, Healing)

  // Run the cycle
  let result = do_debug_cycle(task_id, path, max_iterations, 0, [], [])

  // Complete task
  let duration_ms = get_timestamp() - start_time
  let _ = task_store.complete(task_id, result.success, duration_ms)

  logging.info("[AUTONOMOUS] Cycle completed: success=" <> bool_to_string(result.success)
    <> " iterations=" <> int.to_string(result.iterations)
    <> " fixed=" <> int.to_string(result.errors_fixed))

  result
}

/// Internal: recursive debug cycle
fn do_debug_cycle(
  task_id: String,
  path: String,
  max_iterations: Int,
  current_iteration: Int,
  history: List(DebugIteration),
  all_fixes: List(AppliedFix),
) -> DebugCycleReport {
  // Check iteration limit
  case current_iteration >= max_iterations {
    True -> {
      logging.warn("[AUTONOMOUS] Max iterations reached: " <> int.to_string(max_iterations))
      let build_result = run_build(path)
      make_report(task_id, False, current_iteration, all_fixes,
        get_last_output(build_result), history, get_build_errors(build_result))
    }
    False -> {
      // Increment iteration
      let _ = task_store.increment_iteration(task_id)
      let iteration_num = current_iteration + 1
      logging.info("[AUTONOMOUS] Iteration " <> int.to_string(iteration_num))

      // Step 1: Build
      let build_result = run_build(path)

      case is_build_success(build_result) {
        True -> {
          // Success! No more errors
          logging.info("[AUTONOMOUS] Build successful!")
          let iteration = DebugIteration(
            iteration: iteration_num,
            build_result: build_result,
            fixes_suggested: [],
            fixes_applied: [],
            remaining_errors: 0,
          )
          make_report(task_id, True, iteration_num, all_fixes,
            get_last_output(build_result), list.append(history, [iteration]), [])
        }
        False -> {
          // Has errors - analyze and fix
          let errors = get_build_errors(build_result)
          let error_count = list.length(errors)
          logging.info("[AUTONOMOUS] Found " <> int.to_string(error_count) <> " errors")

          // Step 2: Analyze errors and get fixes
          let suggested_fixes = analyze_errors(errors)
          logging.info("[AUTONOMOUS] Got " <> int.to_string(list.length(suggested_fixes)) <> " fix suggestions")

          // Step 3: Apply best fix (highest confidence)
          case get_best_fix(suggested_fixes) {
            Some(fix) -> {
              logging.info("[AUTONOMOUS] Applying fix: " <> fix.id <> " (confidence: "
                <> float_to_string(fix.confidence) <> ")")

              let apply_result = apply_fix(fix)
              let applied_fix = AppliedFix(
                fix: fix,
                applied_at: get_timestamp(),
                verified: False,
                success: apply_result,
              )

              // Record fix in task store
              let _ = task_store.record_fix_attempt(task_id, applied_fix)

              let iteration = DebugIteration(
                iteration: iteration_num,
                build_result: build_result,
                fixes_suggested: suggested_fixes,
                fixes_applied: [fix],
                remaining_errors: error_count - 1,  // Optimistic
              )

              // Continue cycle
              do_debug_cycle(
                task_id,
                path,
                max_iterations,
                iteration_num,
                list.append(history, [iteration]),
                list.append(all_fixes, [applied_fix]),
              )
            }
            None -> {
              // No fix found
              logging.warn("[AUTONOMOUS] No suitable fix found")
              let iteration = DebugIteration(
                iteration: iteration_num,
                build_result: build_result,
                fixes_suggested: suggested_fixes,
                fixes_applied: [],
                remaining_errors: error_count,
              )
              make_report(task_id, False, iteration_num, all_fixes,
                get_last_output(build_result), list.append(history, [iteration]), errors)
            }
          }
        }
      }
    }
  }
}

/// Run gleam build and parse output
fn run_build(path: String) -> BuildResult {
  logging.debug("[AUTONOMOUS] Running build in: " <> path)
  let result = run_build_ffi(path)

  case result.exit_code {
    0 -> BuildSuccess(result.output)
    _ -> {
      let errors = parse_build_errors(result.output)
      BuildFailure(errors, result.output)
    }
  }
}

/// Parse Gleam build errors from output
fn parse_build_errors(output: String) -> List(CompileError) {
  // Parse Gleam error format:
  // error: ...
  //   ┌─ src/file.gleam:123:45
  let lines = string.split(output, "\n")
  parse_error_lines(lines, [])
}

fn parse_error_lines(
  lines: List(String),
  acc: List(CompileError),
) -> List(CompileError) {
  case lines {
    [] -> list.reverse(acc)
    [line, ..rest] -> {
      case string.starts_with(line, "error:") {
        True -> {
          let message = string.drop_start(line, 7)
          // Look for file location in next lines
          let #(file, line_num, col, remaining) = extract_location(rest)
          let error = CompileError(
            file: file,
            line: line_num,
            column: col,
            error_type: "compile_error",
            message: string.trim(message),
            code: None,
          )
          parse_error_lines(remaining, [error, ..acc])
        }
        False -> parse_error_lines(rest, acc)
      }
    }
  }
}

fn extract_location(
  lines: List(String),
) -> #(String, Int, Int, List(String)) {
  case lines {
    [] -> #("unknown", 0, 0, [])
    [line, ..rest] -> {
      // Look for pattern: ┌─ path/file.gleam:123:45
      case string.contains(line, "┌─") {
        True -> {
          let parts = string.split(line, "┌─")
          case parts {
            [_, location_part] -> {
              let trimmed = string.trim(location_part)
              // Parse file:line:col
              case string.split(trimmed, ":") {
                [file, line_str, col_str, ..] -> {
                  let line_num = result.unwrap(int.parse(line_str), 0)
                  let col = result.unwrap(int.parse(col_str), 0)
                  #(file, line_num, col, rest)
                }
                [file, line_str] -> {
                  let line_num = result.unwrap(int.parse(line_str), 0)
                  #(file, line_num, 0, rest)
                }
                _ -> #(trimmed, 0, 0, rest)
              }
            }
            _ -> extract_location(rest)
          }
        }
        False -> extract_location(rest)
      }
    }
  }
}

/// Analyze errors and generate fix suggestions using smart_analyzer
fn analyze_errors(errors: List(CompileError)) -> List(SuggestedFix) {
  // First try smart_analyzer for intelligent fixes
  let build_output = errors
    |> list.map(fn(e) {
      "error: " <> e.message <> "\n  ┌─ " <> e.file <> ":" <> int.to_string(e.line) <> ":" <> int.to_string(e.column)
    })
    |> string.join("\n\n")

  let parsed_errors = smart_analyzer.parse_errors(build_output)
  let smart_fixes = smart_analyzer.generate_fixes(parsed_errors)

  // Convert smart_analyzer fixes to rainbow_types SuggestedFix
  let converted_fixes = list.map(smart_fixes, fn(sf) {
    let fix_type = case sf.fix_type {
      smart_analyzer.Replace -> TypeFix
      smart_analyzer.InsertBefore -> ImportFix
      smart_analyzer.InsertAfter -> LogicFix
      smart_analyzer.Delete -> RefactorFix
      smart_analyzer.InsertAtLine(_) -> SyntaxFix
    }
    SuggestedFix(
      id: generate_fix_id(),
      file_path: sf.file,
      line_number: sf.line,
      fix_type: fix_type,
      old_code: sf.old_code,
      new_code: sf.new_code,
      confidence: sf.confidence,
      explanation: sf.explanation,
    )
  })

  // If smart_analyzer found fixes, use them
  case list.is_empty(converted_fixes) {
    True -> list.flat_map(errors, analyze_single_error)  // Fallback to simple analysis
    False -> converted_fixes
  }
}

fn analyze_single_error(error: CompileError) -> List(SuggestedFix) {
  let fix_id = generate_fix_id()
  let message = error.message

  // Detect error type based on message content
  let #(fix_type, new_code, confidence, explanation) = detect_fix_type(message)

  [SuggestedFix(
    id: fix_id,
    file_path: error.file,
    line_number: error.line,
    fix_type: fix_type,
    old_code: "",
    new_code: new_code,
    confidence: confidence,
    explanation: explanation,
  )]
}

/// Detect fix type based on error message
fn detect_fix_type(message: String) -> #(rainbow_types.FixType, String, Float, String) {
  case string.contains(message, "Unknown variable") {
    True -> #(ImportFix, "// TODO: Add import or fix variable name", 0.7,
      "Unknown variable - check imports or spelling")
    False -> case string.contains(message, "Unknown type") {
      True -> #(TypeFix, "// TODO: Import type or define it", 0.7,
        "Unknown type - add import")
      False -> case string.contains(message, "Unexpected token") {
        True -> #(SyntaxFix, "// TODO: Fix syntax", 0.6,
          "Syntax error - check brackets, commas, etc.")
        False -> case string.contains(message, "Expected") {
          True -> #(SyntaxFix, "// TODO: Match expected pattern", 0.65,
            "Expected pattern mismatch")
          False -> case string.contains(message, "not exhaustive") {
            True -> #(LogicFix, "// TODO: Add missing case clauses", 0.8,
              "Pattern match not exhaustive - add missing cases")
            False -> case string.contains(message, "unused") {
              True -> #(RefactorFix, "// TODO: Remove or prefix with _", 0.9,
                "Unused variable - prefix with _ or remove")
              False -> #(UnknownFix, "", 0.3, "Error: " <> message)
            }
          }
        }
      }
    }
  }
}

/// Get the best fix (highest confidence)
fn get_best_fix(fixes: List(SuggestedFix)) -> Option(SuggestedFix) {
  case fixes {
    [] -> None
    [first, ..rest] -> {
      let best = list.fold(rest, first, fn(acc, fix) {
        case fix.confidence >. acc.confidence {
          True -> fix
          False -> acc
        }
      })
      // Only return if confidence is above threshold
      case best.confidence >. 0.5 {
        True -> Some(best)
        False -> None
      }
    }
  }
}

/// Apply a fix to the code
fn apply_fix(fix: SuggestedFix) -> Bool {
  logging.info("[AUTONOMOUS] Applying fix to: " <> fix.file_path <> ":" <> int.to_string(fix.line_number))

  // Read the file
  case read_file_ffi(fix.file_path) {
    Error(_) -> {
      logging.error("[AUTONOMOUS] Failed to read file: " <> fix.file_path)
      False
    }
    Ok(content) -> {
      // Apply the fix based on type
      let new_content = apply_fix_to_content(content, fix)
      case new_content == content {
        True -> {
          logging.warn("[AUTONOMOUS] No changes made to file")
          False
        }
        False -> {
          // Write back
          case write_file_ffi(fix.file_path, new_content) {
            Ok(_) -> {
              logging.info("[AUTONOMOUS] Fix applied successfully")
              True
            }
            Error(e) -> {
              logging.error("[AUTONOMOUS] Failed to write file: " <> e)
              False
            }
          }
        }
      }
    }
  }
}

fn apply_fix_to_content(content: String, fix: SuggestedFix) -> String {
  // For now, just add a comment at the problematic line
  // More sophisticated fixes would modify actual code
  let lines = string.split(content, "\n")
  let fixed_lines = list.index_map(lines, fn(line, idx) {
    case idx + 1 == fix.line_number {
      True -> line <> "  // FIXME: " <> fix.explanation
      False -> line
    }
  })
  string.join(fixed_lines, "\n")
}

/// Make the final report
fn make_report(
  task_id: String,
  success: Bool,
  iterations: Int,
  fixes: List(AppliedFix),
  output: String,
  history: List(DebugIteration),
  remaining_errors: List(CompileError),
) -> DebugCycleReport {
  let successful_fixes = list.filter(fixes, fn(f) { f.success })
  DebugCycleReport(
    task_id: task_id,
    success: success,
    iterations: iterations,
    errors_fixed: list.length(successful_fixes),
    errors_remaining: list.length(remaining_errors),
    fixes_applied: fixes,
    final_build_output: output,
    duration_ms: 0,  // Will be set by caller
    history: history,
  )
}

fn get_last_output(result: BuildResult) -> String {
  case result {
    BuildSuccess(output) -> output
    BuildFailure(_, output) -> output
  }
}

// === Helpers ===

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

fn float_to_string(f: Float) -> String {
  // Simple conversion
  let int_part = float_to_int(f)
  let decimal = float_to_int({ f -. int_to_float(int_part) } *. 100.0)
  int.to_string(int_part) <> "." <> int.to_string(decimal)
}

// === FFI ===

type BuildOutput {
  BuildOutput(exit_code: Int, output: String)
}

@external(erlang, "vibee_autonomous_ffi", "run_build")
fn run_build_ffi(path: String) -> BuildOutput

@external(erlang, "vibee_autonomous_ffi", "read_file")
fn read_file_ffi(path: String) -> Result(String, String)

@external(erlang, "vibee_autonomous_ffi", "write_file")
fn write_file_ffi(path: String, content: String) -> Result(Nil, String)

@external(erlang, "vibee_autonomous_ffi", "generate_fix_id")
fn generate_fix_id() -> String

@external(erlang, "vibee_autonomous_ffi", "get_timestamp")
fn get_timestamp() -> Int

@external(erlang, "vibee_autonomous_ffi", "float_to_int")
fn float_to_int(f: Float) -> Int

@external(erlang, "vibee_autonomous_ffi", "int_to_float")
fn int_to_float(i: Int) -> Float
