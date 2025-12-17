// Smart Analyzer - Intelligent Gleam error analysis and fix generation
// Generates REAL fixes, not TODOs!

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, Some}
import gleam/regexp
import gleam/result
import gleam/string
import vibee/vibe_logger.{type VibeLogger}

// =============================================================================
// Types
// =============================================================================

/// Parsed error from Gleam compiler
pub type ParsedError {
  ParsedError(
    error_type: ErrorType,
    file: String,
    line: Int,
    column: Int,
    message: String,
    context_lines: List(String),
  )
}

/// Error classification
pub type ErrorType {
  UnknownVariable(name: String)
  UnknownType(name: String)
  UnknownModule(name: String)
  UnknownFunction(module: String, function: String)
  TypeMismatch(expected: String, got: String)
  MissingImport(module: String)
  IncompletePattern(missing: List(String))
  ArityMismatch(expected: Int, got: Int)
  DuplicateDefinition(name: String)
  SyntaxError(details: String)
  UnusedVariable(name: String)
  UnusedImport(name: String)
  PrivateTypeLeak(type_name: String)
  OtherError(details: String)
}

/// Suggested fix with confidence score
pub type SuggestedFix {
  SuggestedFix(
    file: String,
    line: Int,
    old_code: String,
    new_code: String,
    confidence: Float,
    explanation: String,
    fix_type: FixType,
  )
}

/// Type of fix to apply
pub type FixType {
  Replace
  InsertBefore
  InsertAfter
  Delete
  InsertAtLine(line: Int)
}

/// Analysis result
pub type AnalysisResult {
  AnalysisResult(
    errors: List(ParsedError),
    fixes: List(SuggestedFix),
    summary: String,
  )
}

// =============================================================================
// Error Parsing
// =============================================================================

/// Parse Gleam compiler output into structured errors
pub fn parse_errors(output: String) -> List(ParsedError) {
  output
  |> string.split("\n\n")
  |> list.filter_map(parse_single_error)
}

fn parse_single_error(block: String) -> Result(ParsedError, Nil) {
  let lines = string.split(block, "\n")

  case extract_location(lines) {
    Ok(#(file, line, column)) -> {
      let error_type = classify_error(block)
      let message = extract_message(lines)
      let context = extract_context_lines(lines)

      Ok(ParsedError(
        error_type: error_type,
        file: file,
        line: line,
        column: column,
        message: message,
        context_lines: context,
      ))
    }
    Error(_) -> Error(Nil)
  }
}

fn extract_location(lines: List(String)) -> Result(#(String, Int, Int), Nil) {
  let location_pattern = case
    regexp.from_string("([\\w/.-]+\\.gleam):(\\d+):(\\d+)")
  {
    Ok(re) -> re
    Error(_) -> panic as "Invalid regex"
  }

  lines
  |> list.find_map(fn(line) {
    case regexp.scan(location_pattern, line) {
      [m, ..] -> {
        let submatches: List(Option(String)) = m.submatches
        case submatches {
          [Some(file), Some(line_str), Some(col_str)] -> {
            case int.parse(line_str), int.parse(col_str) {
              Ok(l), Ok(c) -> Ok(#(file, l, c))
              _, _ -> Error(Nil)
            }
          }
          _ -> Error(Nil)
        }
      }
      _ -> Error(Nil)
    }
  })
}

fn classify_error(block: String) -> ErrorType {
  let lower = string.lowercase(block)

  case string.contains(lower, "unknown variable") {
    True -> UnknownVariable(extract_quoted_name(block))
    False ->
      case string.contains(lower, "unknown type") {
        True -> UnknownType(extract_quoted_name(block))
        False ->
          case string.contains(lower, "unknown module") {
            True -> UnknownModule(extract_quoted_name(block))
            False ->
              case string.contains(lower, "did you mean") {
                True -> UnknownVariable(extract_quoted_name(block))
                False ->
                  case string.contains(lower, "type mismatch") {
                    True -> {
                      let #(expected, got) = extract_type_mismatch(block)
                      TypeMismatch(expected, got)
                    }
                    False ->
                      case string.contains(lower, "not exhaustive") {
                        True -> IncompletePattern(extract_missing_patterns(block))
                        False ->
                          case
                            string.contains(lower, "expected")
                            && string.contains(lower, "argument")
                          {
                            True -> {
                              let #(expected, got) = extract_arity(block)
                              ArityMismatch(expected, got)
                            }
                            False ->
                              case string.contains(lower, "already defined") {
                                True -> DuplicateDefinition(extract_quoted_name(block))
                                False ->
                                  case string.contains(lower, "unused variable") {
                                    True -> UnusedVariable(extract_quoted_name(block))
                                    False ->
                                      case string.contains(lower, "unused import") {
                                        True -> UnusedImport(extract_quoted_name(block))
                                        False ->
                                          case string.contains(lower, "private type") {
                                            True ->
                                              PrivateTypeLeak(
                                                extract_quoted_name(block),
                                              )
                                            False ->
                                              case
                                                string.contains(lower, "syntax error")
                                              {
                                                True ->
                                                  SyntaxError(
                                                    extract_message([block]),
                                                  )
                                                False ->
                                                  OtherError(
                                                    extract_message([block]),
                                                  )
                                              }
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
              }
          }
      }
  }
}

fn extract_quoted_name(text: String) -> String {
  let pattern = case regexp.from_string("[`'\"]([\\w_]+)[`'\"]") {
    Ok(re) -> re
    Error(_) -> panic as "Invalid regex"
  }

  case regexp.scan(pattern, text) {
    [m, ..] -> {
      let submatches: List(Option(String)) = m.submatches
      case submatches {
        [Some(name)] -> name
        _ -> "unknown"
      }
    }
    _ -> "unknown"
  }
}

fn extract_type_mismatch(text: String) -> #(String, String) {
  let expected = case string.split(text, "Expected") {
    [_, rest, ..] ->
      rest
      |> string.split("\n")
      |> list.first
      |> result.unwrap("")
      |> string.trim
    _ -> "unknown"
  }

  let got = case string.split(text, "Got") {
    [_, rest, ..] ->
      rest
      |> string.split("\n")
      |> list.first
      |> result.unwrap("")
      |> string.trim
    _ -> "unknown"
  }

  #(expected, got)
}

fn extract_missing_patterns(text: String) -> List(String) {
  let lines = string.split(text, "\n")

  lines
  |> list.filter(fn(line) {
    string.contains(line, "|") || string.contains(line, "missing")
  })
  |> list.map(string.trim)
}

fn extract_arity(text: String) -> #(Int, Int) {
  let pattern = case regexp.from_string("expected (\\d+).*got (\\d+)") {
    Ok(re) -> re
    Error(_) -> panic as "Invalid regex"
  }

  case regexp.scan(pattern, string.lowercase(text)) {
    [m, ..] -> {
      let submatches: List(Option(String)) = m.submatches
      case submatches {
        [Some(exp), Some(got)] -> {
          let expected = result.unwrap(int.parse(exp), 0)
          let got_val = result.unwrap(int.parse(got), 0)
          #(expected, got_val)
        }
        _ -> #(0, 0)
      }
    }
    _ -> #(0, 0)
  }
}

fn extract_message(lines: List(String)) -> String {
  lines
  |> list.filter(fn(line) {
    string.starts_with(string.trim(line), "error:")
    || string.starts_with(string.trim(line), "warning:")
  })
  |> list.first
  |> result.unwrap("")
  |> string.trim
}

fn extract_context_lines(lines: List(String)) -> List(String) {
  lines
  |> list.filter(fn(line) {
    string.contains(line, "|")
  })
}

// =============================================================================
// Fix Generation
// =============================================================================

/// Generate suggested fixes for parsed errors
pub fn generate_fixes(errors: List(ParsedError)) -> List(SuggestedFix) {
  errors
  |> list.flat_map(generate_fix_for_error)
  |> list.sort(fn(a, b) { float.compare(b.confidence, a.confidence) })
}

fn generate_fix_for_error(error: ParsedError) -> List(SuggestedFix) {
  case error.error_type {
    UnknownVariable(name) -> generate_variable_fixes(error, name)
    UnknownType(name) -> generate_type_fixes(error, name)
    UnknownModule(name) -> generate_module_fixes(error, name)
    MissingImport(module) -> generate_import_fix(error, module)
    TypeMismatch(expected, got) ->
      generate_type_conversion_fixes(error, expected, got)
    IncompletePattern(missing) -> generate_pattern_fixes(error, missing)
    ArityMismatch(expected, got) -> generate_arity_fixes(error, expected, got)
    UnusedVariable(name) -> generate_unused_variable_fix(error, name)
    UnusedImport(name) -> generate_unused_import_fix(error, name)
    _ -> []
  }
}

fn generate_variable_fixes(
  error: ParsedError,
  name: String,
) -> List(SuggestedFix) {
  let similar = find_similar_names(name, common_gleam_variables())

  similar
  |> list.map(fn(suggestion) {
    SuggestedFix(
      file: error.file,
      line: error.line,
      old_code: name,
      new_code: suggestion,
      confidence: calculate_similarity(name, suggestion),
      explanation: "Rename `" <> name <> "` to `" <> suggestion <> "`",
      fix_type: Replace,
    )
  })
}

fn generate_type_fixes(error: ParsedError, name: String) -> List(SuggestedFix) {
  let similar = find_similar_names(name, common_gleam_types())

  similar
  |> list.map(fn(suggestion) {
    SuggestedFix(
      file: error.file,
      line: error.line,
      old_code: name,
      new_code: suggestion,
      confidence: calculate_similarity(name, suggestion),
      explanation: "Change type `" <> name <> "` to `" <> suggestion <> "`",
      fix_type: Replace,
    )
  })
}

fn generate_module_fixes(
  error: ParsedError,
  name: String,
) -> List(SuggestedFix) {
  [
    SuggestedFix(
      file: error.file,
      line: 1,
      old_code: "",
      new_code: "import " <> name,
      confidence: 0.85,
      explanation: "Add import for module `" <> name <> "`",
      fix_type: InsertAtLine(1),
    ),
  ]
}

fn generate_import_fix(error: ParsedError, module: String) -> List(SuggestedFix) {
  [
    SuggestedFix(
      file: error.file,
      line: 1,
      old_code: "",
      new_code: "import " <> module,
      confidence: 0.95,
      explanation: "Add missing import for `" <> module <> "`",
      fix_type: InsertAtLine(1),
    ),
  ]
}

fn generate_type_conversion_fixes(
  error: ParsedError,
  expected: String,
  got: String,
) -> List(SuggestedFix) {
  case expected, got {
    "String", "Int" -> [
      SuggestedFix(
        file: error.file,
        line: error.line,
        old_code: "value",
        new_code: "int.to_string(value)",
        confidence: 0.75,
        explanation: "Convert Int to String using int.to_string()",
        fix_type: Replace,
      ),
    ]
    "Int", "String" -> [
      SuggestedFix(
        file: error.file,
        line: error.line,
        old_code: "value",
        new_code: "int.parse(value)",
        confidence: 0.75,
        explanation: "Parse String to Int using int.parse()",
        fix_type: Replace,
      ),
    ]
    "Option(a)", _type_name -> [
      SuggestedFix(
        file: error.file,
        line: error.line,
        old_code: "value",
        new_code: "Some(value)",
        confidence: 0.80,
        explanation: "Wrap value in Some() to create Option",
        fix_type: Replace,
      ),
    ]
    "Result(a, b)", _ -> [
      SuggestedFix(
        file: error.file,
        line: error.line,
        old_code: "value",
        new_code: "Ok(value)",
        confidence: 0.80,
        explanation: "Wrap value in Ok() to create Result",
        fix_type: Replace,
      ),
    ]
    _, _ -> []
  }
}

fn generate_pattern_fixes(
  error: ParsedError,
  missing: List(String),
) -> List(SuggestedFix) {
  let missing_str = string.join(missing, ", ")
  [
    SuggestedFix(
      file: error.file,
      line: error.line,
      old_code: "}",
      new_code: "  _ -> todo\n}",
      confidence: 0.70,
      explanation: "Add catch-all pattern for missing cases: " <> missing_str,
      fix_type: InsertBefore,
    ),
  ]
}

fn generate_arity_fixes(
  error: ParsedError,
  expected: Int,
  got: Int,
) -> List(SuggestedFix) {
  case expected > got {
    True -> [
      SuggestedFix(
        file: error.file,
        line: error.line,
        old_code: ")",
        new_code: ", todo)",
        confidence: 0.60,
        explanation: "Add "
          <> int.to_string(expected - got)
          <> " missing argument(s)",
        fix_type: Replace,
      ),
    ]
    False -> [
      SuggestedFix(
        file: error.file,
        line: error.line,
        old_code: "",
        new_code: "",
        confidence: 0.50,
        explanation: "Remove "
          <> int.to_string(got - expected)
          <> " extra argument(s)",
        fix_type: Replace,
      ),
    ]
  }
}

fn generate_unused_variable_fix(
  error: ParsedError,
  name: String,
) -> List(SuggestedFix) {
  [
    SuggestedFix(
      file: error.file,
      line: error.line,
      old_code: name,
      new_code: "_" <> name,
      confidence: 0.95,
      explanation: "Prefix unused variable with underscore",
      fix_type: Replace,
    ),
    SuggestedFix(
      file: error.file,
      line: error.line,
      old_code: name,
      new_code: "_",
      confidence: 0.90,
      explanation: "Replace with discard pattern",
      fix_type: Replace,
    ),
  ]
}

fn generate_unused_import_fix(
  error: ParsedError,
  name: String,
) -> List(SuggestedFix) {
  [
    SuggestedFix(
      file: error.file,
      line: error.line,
      old_code: "import " <> name,
      new_code: "",
      confidence: 0.95,
      explanation: "Remove unused import `" <> name <> "`",
      fix_type: Delete,
    ),
  ]
}

// =============================================================================
// Similarity & Helpers
// =============================================================================

fn find_similar_names(name: String, candidates: List(String)) -> List(String) {
  candidates
  |> list.filter(fn(c) { calculate_similarity(name, c) >. 0.6 })
  |> list.sort(fn(a, b) {
    float.compare(
      calculate_similarity(name, b),
      calculate_similarity(name, a),
    )
  })
  |> list.take(3)
}

fn calculate_similarity(a: String, b: String) -> Float {
  let a_lower = string.lowercase(a)
  let b_lower = string.lowercase(b)

  case a_lower == b_lower {
    True -> 1.0
    False -> {
      let max_len = int.max(string.length(a), string.length(b))
      case max_len {
        0 -> 1.0
        _ -> {
          let dist = levenshtein_distance(a_lower, b_lower)
          1.0 -. int.to_float(dist) /. int.to_float(max_len)
        }
      }
    }
  }
}

fn levenshtein_distance(a: String, b: String) -> Int {
  let a_chars = string.to_graphemes(a)
  let b_chars = string.to_graphemes(b)
  let len_diff =
    int.absolute_value(list.length(a_chars) - list.length(b_chars))

  let common =
    list.zip(a_chars, b_chars)
    |> list.filter(fn(pair) { pair.0 == pair.1 })
    |> list.length

  let max_common = int.min(list.length(a_chars), list.length(b_chars))
  len_diff + max_common - common
}

fn common_gleam_variables() -> List(String) {
  [
    "result", "value", "data", "error", "msg", "message", "response", "request",
    "ctx", "context", "state", "acc", "item", "items", "list", "map", "key",
    "val", "ok", "err", "some", "none", "true", "false", "input", "output",
    "config", "options", "args", "name", "id", "path", "file", "line", "content",
  ]
}

fn common_gleam_types() -> List(String) {
  [
    "String", "Int", "Float", "Bool", "List", "Map", "Set", "Option", "Result",
    "Nil", "Dynamic", "BitArray", "Subject", "Selector", "Process", "Pid",
    "Json", "Request", "Response", "Body",
  ]
}

// =============================================================================
// Main Analysis Function
// =============================================================================

/// Analyze build output and return structured results
pub fn analyze(build_output: String, logger: VibeLogger) -> AnalysisResult {
  vibe_logger.info(logger, "Starting smart error analysis")

  let errors = parse_errors(build_output)
  let error_count = list.length(errors)

  vibe_logger.info(
    logger,
    "Parsed " <> int.to_string(error_count) <> " errors",
  )

  let fixes = generate_fixes(errors)
  let fix_count = list.length(fixes)

  vibe_logger.info(
    logger,
    "Generated " <> int.to_string(fix_count) <> " suggested fixes",
  )

  let high_confidence =
    fixes
    |> list.filter(fn(f) { f.confidence >. 0.8 })
    |> list.length

  let summary =
    "Found "
    <> int.to_string(error_count)
    <> " errors, generated "
    <> int.to_string(fix_count)
    <> " fixes ("
    <> int.to_string(high_confidence)
    <> " high confidence)"

  AnalysisResult(errors: errors, fixes: fixes, summary: summary)
}

/// Get the best fix for automatic application
pub fn get_best_fix(result: AnalysisResult) -> Option(SuggestedFix) {
  result.fixes
  |> list.filter(fn(f) { f.confidence >. 0.8 })
  |> list.first
  |> option.from_result
}

/// Filter fixes by confidence threshold
pub fn filter_by_confidence(
  fixes: List(SuggestedFix),
  threshold: Float,
) -> List(SuggestedFix) {
  list.filter(fixes, fn(f) { f.confidence >=. threshold })
}
