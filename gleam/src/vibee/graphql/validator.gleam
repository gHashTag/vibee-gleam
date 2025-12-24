// GraphQL Query Validator
// Validates query complexity to prevent resource exhaustion attacks

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/graphql/types.{
  type Document, type Field, type Operation, type Selection, FieldSelection,
}

/// Query complexity limits
pub type QueryLimits {
  QueryLimits(
    max_depth: Int,      // Maximum nesting depth (default: 10)
    max_fields: Int,     // Maximum total fields (default: 100)
    max_aliases: Int,    // Maximum field aliases (default: 10)
  )
}

/// Validation result
pub type ValidationResult {
  Valid
  DepthExceeded(depth: Int, max: Int)
  FieldsExceeded(count: Int, max: Int)
  AliasesExceeded(count: Int, max: Int)
}

/// Default limits
pub fn default_limits() -> QueryLimits {
  QueryLimits(
    max_depth: 10,
    max_fields: 100,
    max_aliases: 10,
  )
}

/// Validate a parsed GraphQL document
pub fn validate(document: Document, limits: QueryLimits) -> ValidationResult {
  // Validate each operation
  let results = list.map(document.operations, fn(op) {
    validate_operation(op, limits)
  })

  // Return first error or Valid
  case list.find(results, fn(r) {
    case r {
      Valid -> False
      _ -> True
    }
  }) {
    Ok(error) -> error
    Error(_) -> Valid
  }
}

/// Validate a single operation
fn validate_operation(operation: Operation, limits: QueryLimits) -> ValidationResult {
  // Check depth
  let depth = calculate_depth(operation.selections, 0)
  case depth > limits.max_depth {
    True -> DepthExceeded(depth: depth, max: limits.max_depth)
    False -> {
      // Check total fields
      let field_count = count_fields(operation.selections)
      case field_count > limits.max_fields {
        True -> FieldsExceeded(count: field_count, max: limits.max_fields)
        False -> {
          // Check aliases
          let alias_count = count_aliases(operation.selections)
          case alias_count > limits.max_aliases {
            True -> AliasesExceeded(count: alias_count, max: limits.max_aliases)
            False -> Valid
          }
        }
      }
    }
  }
}

/// Calculate maximum depth of selections
fn calculate_depth(selections: List(Selection), current_depth: Int) -> Int {
  case selections {
    [] -> current_depth
    _ -> {
      let depths = list.map(selections, fn(sel) {
        case sel {
          FieldSelection(field) -> {
            calculate_depth(field.selections, current_depth + 1)
          }
          _ -> current_depth
        }
      })
      list.fold(depths, current_depth, fn(acc, d) {
        int.max(acc, d)
      })
    }
  }
}

/// Count total number of fields (including nested)
fn count_fields(selections: List(Selection)) -> Int {
  list.fold(selections, 0, fn(acc, sel) {
    case sel {
      FieldSelection(field) -> {
        acc + 1 + count_fields(field.selections)
      }
      _ -> acc
    }
  })
}

/// Count number of field aliases
fn count_aliases(selections: List(Selection)) -> Int {
  list.fold(selections, 0, fn(acc, sel) {
    case sel {
      FieldSelection(field) -> {
        let has_alias = case field.alias {
          Some(_) -> 1
          None -> 0
        }
        acc + has_alias + count_aliases(field.selections)
      }
      _ -> acc
    }
  })
}

/// Convert validation result to error message
pub fn result_to_error(result: ValidationResult) -> Option(String) {
  case result {
    Valid -> None
    DepthExceeded(depth, max) ->
      Some("Query depth " <> int.to_string(depth) <> " exceeds maximum " <> int.to_string(max))
    FieldsExceeded(count, max) ->
      Some("Query field count " <> int.to_string(count) <> " exceeds maximum " <> int.to_string(max))
    AliasesExceeded(count, max) ->
      Some("Query alias count " <> int.to_string(count) <> " exceeds maximum " <> int.to_string(max))
  }
}
