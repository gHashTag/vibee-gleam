// MCP Protocol - JSON-RPC message handling

import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/mcp/types.{
  type ContentBlock, type JsonRpcError, type JsonRpcRequest, type JsonRpcResponse,
  type ServerInfo, type Tool, type ToolResult,
  ImageContent, JsonRpcError, JsonRpcRequest, JsonRpcResponse, ResourceContent,
  TextContent,
}

/// Parse JSON-RPC request from string
pub fn parse_request(input: String) -> Result(JsonRpcRequest, String) {
  let jsonrpc = extract_string_field(input, "jsonrpc")
  let method = extract_string_field(input, "method")
  let id = extract_int_field(input, "id")

  case jsonrpc, method {
    "2.0", m if m != "" -> Ok(JsonRpcRequest(
      jsonrpc: jsonrpc,
      id: id,
      method: method,
      params: Some(json.string(input)),  // Pass raw input as params
    ))
    _, _ -> Error("Invalid JSON-RPC request")
  }
}

/// Extract string field from JSON string
fn extract_string_field(j: String, field: String) -> String {
  let pattern = "\"" <> field <> "\":\""
  case string.split(j, pattern) {
    [_, rest, ..] -> {
      case string.split(rest, "\"") {
        [value, ..] -> value
        _ -> ""
      }
    }
    _ -> ""
  }
}

/// Extract int field from JSON string
fn extract_int_field(j: String, field: String) -> Option(Int) {
  let pattern = "\"" <> field <> "\":"
  case string.split_once(j, pattern) {
    Ok(#(_, rest)) -> {
      // Trim leading whitespace and extract digits
      let trimmed = string.trim_start(rest)
      let num_str = extract_leading_number(trimmed)
      case int.parse(num_str) {
        Ok(n) -> Some(n)
        Error(_) -> None
      }
    }
    Error(_) -> None
  }
}

/// Extract leading number from string (handles "1,", "1}", "1 ", etc.)
fn extract_leading_number(s: String) -> String {
  s
  |> string.to_graphemes()
  |> do_extract_digits("")
}

fn do_extract_digits(chars: List(String), acc: String) -> String {
  case chars {
    [] -> acc
    [c, ..rest] -> {
      case c {
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
          do_extract_digits(rest, acc <> c)
        _ -> acc
      }
    }
  }
}

/// Extract tool name from params - handles both escaped and non-escaped JSON
pub fn extract_tool_name(params: json.Json) -> String {
  let s = json.to_string(params)

  // Try escaped version first (when params is stringified JSON)
  let escaped_pattern = "\\\"name\\\":\\\""
  case string.contains(s, escaped_pattern) {
    True -> {
      case string.split_once(s, escaped_pattern) {
        Ok(#(_, rest)) -> {
          case string.split_once(rest, "\\\"") {
            Ok(#(name, _)) -> name
            Error(_) -> ""
          }
        }
        Error(_) -> ""
      }
    }
    False -> {
      // Try non-escaped version
      case string.contains(s, "\"name\":\"") {
        True -> {
          case string.split_once(s, "\"name\":\"") {
            Ok(#(_, rest)) -> {
              case string.split_once(rest, "\"") {
                Ok(#(name, _)) -> name
                Error(_) -> ""
              }
            }
            Error(_) -> ""
          }
        }
        False -> ""
      }
    }
  }
}

/// Extract tool arguments from params
/// The params is json.string(raw_input) where raw_input is the full JSON-RPC request
/// We need to extract just the arguments object from the full request
pub fn extract_tool_args(params: json.Json) -> json.Json {
  // json.to_string returns the JSON representation, which for a string is quoted
  // So if params = json.string("..."), json.to_string(params) = "\"...\""
  // We need the inner string value
  let s = json.to_string(params)

  // Remove outer quotes if present (json.string produces "\"content\"")
  let unquoted = case string.starts_with(s, "\"") && string.ends_with(s, "\"") {
    True -> s
      |> string.drop_start(1)
      |> string.drop_end(1)
      // Unescape inner quotes
      |> string.replace("\\\"", "\"")
      |> string.replace("\\\\", "\\")
    False -> s
  }

  // Now find "arguments": in the full request and extract the object
  case string.split_once(unquoted, "\"arguments\":") {
    Ok(#(_, rest)) -> {
      let trimmed = string.trim_start(rest)
      case extract_json_object(trimmed) {
        Ok(args_str) -> {
          // Return as json.Json by parsing the args string
          parse_args_to_json(args_str)
        }
        Error(_) -> json.object([])
      }
    }
    Error(_) -> {
      // Try also the params field directly in case args is nested differently
      case string.split_once(unquoted, "\"params\":") {
        Ok(#(_, rest)) -> {
          let trimmed = string.trim_start(rest)
          case extract_json_object(trimmed) {
            Ok(params_str) -> {
              // Now extract arguments from params object
              case string.split_once(params_str, "\"arguments\":") {
                Ok(#(_, args_rest)) -> {
                  let args_trimmed = string.trim_start(args_rest)
                  case extract_json_object(args_trimmed) {
                    Ok(args_str) -> parse_args_to_json(args_str)
                    Error(_) -> json.object([])
                  }
                }
                Error(_) -> json.object([])
              }
            }
            Error(_) -> json.object([])
          }
        }
        Error(_) -> json.object([])
      }
    }
  }
}

/// Extract JSON object string from start of string, handling nested braces
fn extract_json_object(s: String) -> Result(String, Nil) {
  case string.first(s) {
    Ok("{") -> {
      let chars = string.to_graphemes(s)
      extract_balanced_braces(chars, 0, "")
    }
    Ok("n") -> {
      // null
      case string.starts_with(s, "null") {
        True -> Ok("null")
        False -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

fn extract_balanced_braces(chars: List(String), depth: Int, acc: String) -> Result(String, Nil) {
  case chars {
    [] -> Error(Nil)
    [c, ..rest] -> {
      let new_acc = acc <> c
      let new_depth = case c {
        "{" -> depth + 1
        "}" -> depth - 1
        _ -> depth
      }
      case new_depth {
        0 -> Ok(new_acc)
        _ -> extract_balanced_braces(rest, new_depth, new_acc)
      }
    }
  }
}

/// Parse arguments string to json.Json by using individual field decoders
fn parse_args_to_json(s: String) -> json.Json {
  // Build json.Json by manually constructing the object
  // Extract known fields using string parsing
  let fields = extract_json_fields(s)
  json.object(fields)
}

/// Extract fields from JSON object string
fn extract_json_fields(s: String) -> List(#(String, json.Json)) {
  // Remove outer braces
  let inner = s
    |> string.drop_start(1)
    |> string.drop_end(1)
    |> string.trim()

  // Split by commas (simple, won't handle nested correctly but good for flat args)
  case inner {
    "" -> []
    _ -> {
      // Parse key-value pairs
      parse_json_pairs(inner, [])
    }
  }
}

fn parse_json_pairs(s: String, acc: List(#(String, json.Json))) -> List(#(String, json.Json)) {
  case string.trim(s) {
    "" -> list.reverse(acc)
    trimmed -> {
      // Find first "key":
      case string.split_once(trimmed, "\":") {
        Ok(#(key_part, value_part)) -> {
          // Extract key (remove leading quote)
          let key = key_part
            |> string.trim()
            |> string.drop_start(1)  // remove leading "

          // Extract value and remaining
          let #(value, remaining) = extract_json_value(string.trim(value_part))

          let json_value = case value {
            "null" -> json.null()
            "true" -> json.bool(True)
            "false" -> json.bool(False)
            v -> {
              case string.first(v) {
                Ok("\"") -> {
                  // String value - remove quotes
                  let unquoted = v
                    |> string.drop_start(1)
                    |> string.drop_end(1)
                  json.string(unquoted)
                }
                Ok(d) -> {
                  // Check if digit and parse as number
                  case is_digit(d) {
                    True -> {
                      case int.parse(v) {
                        Ok(n) -> json.int(n)
                        Error(_) -> {
                          // Try float parsing
                          case float.parse(v) {
                            Ok(f) -> json.float(f)
                            Error(_) -> json.string(v)
                          }
                        }
                      }
                    }
                    False -> json.string(v)
                  }
                }
                _ -> json.string(v)
              }
            }
          }

          parse_json_pairs(remaining, [#(key, json_value), ..acc])
        }
        Error(_) -> list.reverse(acc)
      }
    }
  }
}

fn is_digit(s: String) -> Bool {
  case s {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "-" -> True
    _ -> False
  }
}

fn extract_json_value(s: String) -> #(String, String) {
  case string.first(s) {
    Ok("\"") -> {
      // String value - find closing quote (not escaped)
      extract_string_value(string.drop_start(s, 1), "\"")
    }
    Ok("{") -> {
      // Object - find matching brace
      case extract_balanced_braces(string.to_graphemes(s), 0, "") {
        Ok(obj) -> {
          let rest = string.drop_start(s, string.length(obj))
          let remaining = case string.first(rest) {
            Ok(",") -> string.drop_start(rest, 1)
            _ -> rest
          }
          #(obj, remaining)
        }
        Error(_) -> #("null", "")
      }
    }
    Ok("[") -> {
      // Array - find matching bracket
      case extract_balanced_brackets(string.to_graphemes(s), 0, "") {
        Ok(arr) -> {
          let rest = string.drop_start(s, string.length(arr))
          let remaining = case string.first(rest) {
            Ok(",") -> string.drop_start(rest, 1)
            _ -> rest
          }
          #(arr, remaining)
        }
        Error(_) -> #("null", "")
      }
    }
    _ -> {
      // Primitive value - find next comma or end
      case string.split_once(s, ",") {
        Ok(#(value, rest)) -> #(string.trim(value), rest)
        Error(_) -> #(string.trim(s), "")
      }
    }
  }
}

fn extract_string_value(s: String, acc: String) -> #(String, String) {
  case string.first(s) {
    Ok("\"") -> {
      let rest = string.drop_start(s, 1)
      let remaining = case string.first(rest) {
        Ok(",") -> string.drop_start(rest, 1)
        _ -> rest
      }
      #(acc <> "\"", remaining)
    }
    Ok("\\") -> {
      // Escaped character
      case string.slice(s, 0, 2) {
        chars -> extract_string_value(string.drop_start(s, 2), acc <> chars)
      }
    }
    Ok(c) -> extract_string_value(string.drop_start(s, 1), acc <> c)
    Error(_) -> #(acc <> "\"", "")
  }
}

fn extract_balanced_brackets(chars: List(String), depth: Int, acc: String) -> Result(String, Nil) {
  case chars {
    [] -> Error(Nil)
    [c, ..rest] -> {
      let new_acc = acc <> c
      let new_depth = case c {
        "[" -> depth + 1
        "]" -> depth - 1
        _ -> depth
      }
      case new_depth {
        0 -> Ok(new_acc)
        _ -> extract_balanced_brackets(rest, new_depth, new_acc)
      }
    }
  }
}

/// Encode JSON-RPC response to string
pub fn encode_response(response: JsonRpcResponse) -> String {
  let base = [
    #("jsonrpc", json.string(response.jsonrpc)),
  ]

  let with_id = case response.id {
    Some(id) -> list.append(base, [#("id", json.int(id))])
    None -> list.append(base, [#("id", json.null())])
  }

  let with_result = case response.result {
    Some(r) -> list.append(with_id, [#("result", r)])
    None -> with_id
  }

  let with_error = case response.error {
    Some(err) -> list.append(with_result, [
      #("error", json.object([
        #("code", json.int(err.code)),
        #("message", json.string(err.message)),
      ])),
    ])
    None -> with_result
  }

  json.object(with_error)
  |> json.to_string()
}

/// Create success response
pub fn success_response(id: Option(Int), result: json.Json) -> JsonRpcResponse {
  JsonRpcResponse(
    jsonrpc: "2.0",
    id: id,
    result: Some(result),
    error: None,
  )
}

/// Create error response
pub fn error_response(id: Option(Int), code: Int, message: String) -> JsonRpcResponse {
  JsonRpcResponse(
    jsonrpc: "2.0",
    id: id,
    result: None,
    error: Some(JsonRpcError(code: code, message: message, data: None)),
  )
}

/// Encode server info for initialize response
pub fn encode_server_info(info: ServerInfo) -> json.Json {
  json.object([
    #("protocolVersion", json.string("2024-11-05")),
    #("serverInfo", json.object([
      #("name", json.string(info.name)),
      #("version", json.string(info.version)),
    ])),
    #("capabilities", json.object([
      #("tools", json.object([])),
      #("resources", json.object([])),
      #("prompts", json.object([])),
      #("logging", json.object([])),
    ])),
  ])
}

/// Encode tool list
pub fn encode_tools(tools: List(Tool)) -> json.Json {
  json.object([
    #("tools", json.array(tools, fn(tool) {
      json.object([
        #("name", json.string(tool.name)),
        #("description", json.string(tool.description)),
        #("inputSchema", tool.input_schema),
      ])
    })),
  ])
}

/// Encode tool list with MCP 2024-11-05 annotations (readOnlyHint etc)
pub fn encode_tools_with_annotations(tools: List(#(Tool, ToolAnnotations))) -> json.Json {
  json.object([
    #("tools", json.array(tools, fn(item) {
      let #(tool, annotations) = item
      let base = [
        #("name", json.string(tool.name)),
        #("description", json.string(tool.description)),
        #("inputSchema", tool.input_schema),
      ]
      let with_annotations = case annotations.read_only_hint {
        Some(True) -> list.append(base, [
          #("annotations", json.object([
            #("readOnlyHint", json.bool(True)),
          ])),
        ])
        Some(False) -> list.append(base, [
          #("annotations", json.object([
            #("readOnlyHint", json.bool(False)),
            #("destructiveHint", json.bool(annotations.destructive_hint)),
          ])),
        ])
        None -> base
      }
      json.object(with_annotations)
    })),
  ])
}

/// Tool annotations per MCP spec
pub type ToolAnnotations {
  ToolAnnotations(
    read_only_hint: Option(Bool),
    destructive_hint: Bool,
    idempotent_hint: Bool,
    open_world_hint: Bool,
  )
}

/// Create read-only tool annotations
pub fn read_only_annotations() -> ToolAnnotations {
  ToolAnnotations(
    read_only_hint: Some(True),
    destructive_hint: False,
    idempotent_hint: True,
    open_world_hint: False,
  )
}

/// Create write/destructive tool annotations
pub fn write_annotations(destructive: Bool) -> ToolAnnotations {
  ToolAnnotations(
    read_only_hint: Some(False),
    destructive_hint: destructive,
    idempotent_hint: False,
    open_world_hint: True,
  )
}

/// Encode tool result
pub fn encode_tool_result(result: ToolResult) -> json.Json {
  json.object([
    #("content", json.array(result.content, encode_content_block)),
    #("isError", json.bool(result.is_error)),
  ])
}

fn encode_content_block(block: ContentBlock) -> json.Json {
  case block {
    TextContent(text) -> json.object([
      #("type", json.string("text")),
      #("text", json.string(text)),
    ])
    ImageContent(data, mime) -> json.object([
      #("type", json.string("image")),
      #("data", json.string(data)),
      #("mimeType", json.string(mime)),
    ])
    ResourceContent(uri, mime, text) -> {
      let base = [
        #("type", json.string("resource")),
        #("resource", json.object([
          #("uri", json.string(uri)),
          #("mimeType", json.string(mime)),
        ])),
      ]
      case text {
        Some(t) -> json.object(list.append(base, [#("text", json.string(t))]))
        None -> json.object(base)
      }
    }
  }
}

/// Create text tool result
pub fn text_result(text: String) -> ToolResult {
  types.ToolResult(
    content: [TextContent(text)],
    is_error: False,
  )
}

/// Create error tool result
pub fn error_result(message: String) -> ToolResult {
  types.ToolResult(
    content: [TextContent(message)],
    is_error: True,
  )
}
