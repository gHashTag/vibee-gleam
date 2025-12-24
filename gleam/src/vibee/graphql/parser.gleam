// GraphQL Parser - Simple implementation for common queries
// Supports: queries, mutations, field arguments, selection sets

import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/graphql/types.{
  type Argument, type Document, type Field, type OperationType, type Selection,
  type Value, Argument, Document, Field, FieldSelection, IntValue, Mutation,
  Operation, Query, StringValue, Subscription,
}

pub type ParseError {
  UnexpectedToken(expected: String, got: String)
  UnexpectedEnd
  InvalidNumber(String)
  InvalidString(String)
  InvalidCharacter(String)
}

pub fn error_to_string(err: ParseError) -> String {
  case err {
    UnexpectedToken(expected, got) -> "Expected " <> expected <> ", got " <> got
    UnexpectedEnd -> "Unexpected end of input"
    InvalidNumber(s) -> "Invalid number: " <> s
    InvalidString(s) -> "Invalid string: " <> s
    InvalidCharacter(c) -> "Invalid character: " <> c
  }
}

/// Parse GraphQL document
pub fn parse(input: String) -> Result(Document, ParseError) {
  let trimmed = string.trim(input)

  case trimmed {
    "" -> Error(UnexpectedEnd)
    _ -> {
      // Determine operation type and parse
      let #(op_type, rest) = detect_operation_type(trimmed)

      case parse_selection_set(rest) {
        Ok(selections) -> {
          let operation = Operation(
            operation_type: op_type,
            name: None,
            variables: [],
            directives: [],
            selections: selections,
          )
          Ok(Document(operations: [operation], fragments: dict.new()))
        }
        Error(e) -> Error(e)
      }
    }
  }
}

/// Detect operation type (query/mutation/subscription) or default to Query
fn detect_operation_type(input: String) -> #(OperationType, String) {
  let trimmed = string.trim(input)

  case string.starts_with(trimmed, "query") {
    True -> {
      let rest = string.drop_start(trimmed, 5)
      let rest = skip_operation_name(rest)
      #(Query, rest)
    }
    False -> {
      case string.starts_with(trimmed, "mutation") {
        True -> {
          let rest = string.drop_start(trimmed, 8)
          let rest = skip_operation_name(rest)
          #(Mutation, rest)
        }
        False -> {
          case string.starts_with(trimmed, "subscription") {
            True -> {
              let rest = string.drop_start(trimmed, 12)
              let rest = skip_operation_name(rest)
              #(Subscription, rest)
            }
            False -> {
              // Shorthand query syntax: { ... }
              #(Query, trimmed)
            }
          }
        }
      }
    }
  }
}

/// Skip optional operation name until we hit {
fn skip_operation_name(input: String) -> String {
  let trimmed = string.trim(input)
  case string.first(trimmed) {
    Ok("{") -> trimmed
    Ok(_) -> {
      // Skip until { or (
      case string.split_once(trimmed, "{") {
        Ok(#(_, rest)) -> "{" <> rest
        Error(_) -> trimmed
      }
    }
    Error(_) -> trimmed
  }
}

/// Parse selection set { field1 field2 ... }
fn parse_selection_set(input: String) -> Result(List(Selection), ParseError) {
  let trimmed = string.trim(input)

  // Remove leading {
  case string.first(trimmed) {
    Ok("{") -> {
      let content = string.drop_start(trimmed, 1)
      // Find matching }
      case find_matching_brace(content, 0, "") {
        Some(inner) -> parse_fields(string.trim(inner))
        None -> Error(UnexpectedToken("}", "end of input"))
      }
    }
    Ok(c) -> Error(UnexpectedToken("{", c))
    Error(_) -> Error(UnexpectedEnd)
  }
}

/// Find content until matching closing brace
fn find_matching_brace(input: String, depth: Int, acc: String) -> Option(String) {
  case string.first(input) {
    Error(_) -> None
    Ok("{") -> {
      let rest = string.drop_start(input, 1)
      find_matching_brace(rest, depth + 1, acc <> "{")
    }
    Ok("}") -> {
      case depth {
        0 -> Some(acc)
        _ -> {
          let rest = string.drop_start(input, 1)
          find_matching_brace(rest, depth - 1, acc <> "}")
        }
      }
    }
    Ok(c) -> {
      let rest = string.drop_start(input, 1)
      find_matching_brace(rest, depth, acc <> c)
    }
  }
}

/// Parse fields from content between braces
fn parse_fields(input: String) -> Result(List(Selection), ParseError) {
  let trimmed = string.trim(input)
  case trimmed {
    "" -> Ok([])
    _ -> {
      let tokens = tokenize_fields(trimmed)
      let fields = list.filter_map(tokens, fn(token) {
        case parse_field(token) {
          Ok(field) -> Ok(FieldSelection(field))
          Error(_) -> Error(Nil)
        }
      })
      Ok(fields)
    }
  }
}

/// Tokenize input into field tokens
fn tokenize_fields(input: String) -> List(String) {
  // Split by whitespace and newlines, keeping balanced braces together
  let chars = string.to_graphemes(input)
  tokenize_chars(chars, "", [], 0)
}

fn tokenize_chars(
  chars: List(String),
  current: String,
  tokens: List(String),
  depth: Int,
) -> List(String) {
  case chars {
    [] -> {
      case string.trim(current) {
        "" -> list.reverse(tokens)
        c -> list.reverse([c, ..tokens])
      }
    }
    // Track both braces and parentheses for grouping
    ["{", ..rest] -> {
      tokenize_chars(rest, current <> "{", tokens, depth + 1)
    }
    ["(", ..rest] -> {
      tokenize_chars(rest, current <> "(", tokens, depth + 1)
    }
    ["}", ..rest] -> {
      case depth {
        0 -> tokenize_chars(rest, current <> "}", tokens, 0)
        _ -> tokenize_chars(rest, current <> "}", tokens, depth - 1)
      }
    }
    [")", ..rest] -> {
      case depth {
        0 -> tokenize_chars(rest, current <> ")", tokens, 0)
        _ -> tokenize_chars(rest, current <> ")", tokens, depth - 1)
      }
    }
    [c, ..rest] -> {
      case depth > 0 {
        True -> tokenize_chars(rest, current <> c, tokens, depth)
        False -> {
          case is_whitespace(c) {
            True -> {
              case string.trim(current) {
                "" -> tokenize_chars(rest, "", tokens, depth)
                cur -> tokenize_chars(rest, "", [cur, ..tokens], depth)
              }
            }
            False -> tokenize_chars(rest, current <> c, tokens, depth)
          }
        }
      }
    }
  }
}

fn is_whitespace(c: String) -> Bool {
  case c {
    " " | "\n" | "\r" | "\t" -> True
    _ -> False
  }
}

/// Parse a single field token
fn parse_field(token: String) -> Result(Field, ParseError) {
  let trimmed = string.trim(token)

  // Check for arguments (...)
  case string.split_once(trimmed, "(") {
    Ok(#(name, rest)) -> {
      // Parse arguments
      case string.split_once(rest, ")") {
        Ok(#(args_str, after_args)) -> {
          let arguments = parse_arguments(args_str)
          let selections = case string.contains(after_args, "{") {
            True -> {
              case parse_selection_set(string.trim(after_args)) {
                Ok(sels) -> sels
                Error(_) -> []
              }
            }
            False -> []
          }
          Ok(Field(
            alias: None,
            name: string.trim(name),
            arguments: arguments,
            directives: [],
            selections: selections,
          ))
        }
        Error(_) -> Error(UnexpectedToken(")", "end"))
      }
    }
    Error(_) -> {
      // Check for sub-selections
      case string.split_once(trimmed, "{") {
        Ok(#(name, rest)) -> {
          let selections = case parse_selection_set("{" <> rest) {
            Ok(sels) -> sels
            Error(_) -> []
          }
          Ok(Field(
            alias: None,
            name: string.trim(name),
            arguments: [],
            directives: [],
            selections: selections,
          ))
        }
        Error(_) -> {
          // Simple field name
          Ok(Field(
            alias: None,
            name: trimmed,
            arguments: [],
            directives: [],
            selections: [],
          ))
        }
      }
    }
  }
}

/// Parse arguments from string like "id: 1, status: NEW"
fn parse_arguments(input: String) -> List(Argument) {
  let parts = string.split(input, ",")
  list.filter_map(parts, fn(part) {
    case string.split_once(string.trim(part), ":") {
      Ok(#(name, value)) -> {
        let arg = Argument(
          name: string.trim(name),
          value: parse_value(string.trim(value)),
        )
        Ok(arg)
      }
      Error(_) -> Error(Nil)
    }
  })
}

/// Parse a GraphQL value
fn parse_value(input: String) -> Value {
  let trimmed = string.trim(input)

  // Check for string (quoted)
  case string.starts_with(trimmed, "\"") {
    True -> {
      let inner = string.slice(trimmed, 1, string.length(trimmed) - 2)
      StringValue(inner)
    }
    False -> {
      // Try to parse as int
      case int.parse(trimmed) {
        Ok(n) -> IntValue(n)
        Error(_) -> {
          // Treat as enum or string
          StringValue(trimmed)
        }
      }
    }
  }
}
