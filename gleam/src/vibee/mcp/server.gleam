// MCP Server - stdio-based JSON-RPC server with full MCP 2024-11-05 compliance

import gleam/io
import gleam/json
import gleam/option.{None, Some}
import gleam/string
import vibee/logging
import vibee/mcp/config
import vibee/mcp/protocol
import vibee/mcp/tools.{type ToolRegistry}
import vibee/mcp/types.{
  type JsonRpcRequest, ServerCapabilities, ServerInfo,
}
import vibee/mcp/audit
import vibee/mcp/cache
import vibee/mcp/events
import vibee/mcp/telemetry
import vibee/mcp/validation

/// Current log level for logging/setLevel
type LogLevel {
  LogDebug
  LogInfo
  LogWarning
  LogErr
}

/// Start MCP server on stdio
pub fn start(registry: ToolRegistry) -> Result(Nil, String) {
  logging.quick_info("[MCP] Server starting...")

  // Load configuration
  let cfg = config.get_config()
  logging.quick_info("[MCP] Bridge URL: " <> config.bridge_host(cfg) <> ":" <> int_to_string(config.bridge_port(cfg)))

  // Initialize cache
  cache.init()
  logging.quick_info("[MCP] Cache initialized")

  // Initialize Event Bus
  events.init()
  logging.quick_info("[MCP] Event Bus initialized")

  // Initialize Telemetry
  telemetry.init()
  logging.quick_info("[MCP] Telemetry initialized")

  // Run the main loop
  main_loop(registry, LogInfo)

  Ok(Nil)
}

/// Main server loop - reads stdin, processes requests, writes stdout
fn main_loop(registry: ToolRegistry, log_level: LogLevel) -> Nil {
  case read_line() {
    Ok(line) -> {
      case string.trim(line) {
        "" -> main_loop(registry, log_level)
        input -> {
          let #(response, new_log_level) = handle_request(registry, input, log_level)
          write_line(response)
          main_loop(registry, new_log_level)
        }
      }
    }
    Error(_) -> {
      logging.quick_info("[MCP] EOF received, shutting down")
      Nil
    }
  }
}

/// Handle a single JSON-RPC request
fn handle_request(registry: ToolRegistry, input: String, log_level: LogLevel) -> #(String, LogLevel) {
  logging.quick_info("[MCP] Request: " <> string.slice(input, 0, 100) <> "...")

  case protocol.parse_request(input) {
    Ok(req) -> {
      let #(response, new_level) = dispatch_method(registry, req, log_level)
      let encoded = protocol.encode_response(response)
      logging.quick_info("[MCP] Response: " <> string.slice(encoded, 0, 100) <> "...")
      #(encoded, new_level)
    }
    Error(err) -> {
      logging.quick_error("[MCP] Parse error: " <> err)
      #(protocol.encode_response(
        protocol.error_response(None, types.parse_error, err)
      ), log_level)
    }
  }
}

/// Dispatch request to appropriate handler
fn dispatch_method(registry: ToolRegistry, req: JsonRpcRequest, log_level: LogLevel) -> #(types.JsonRpcResponse, LogLevel) {
  logging.quick_info("[MCP] Method: " <> req.method)

  case req.method {
    // Core protocol
    "initialize" -> #(handle_initialize(req), log_level)
    "initialized" -> #(handle_initialized(req), log_level)
    "ping" -> #(handle_ping(req), log_level)

    // Tools
    "tools/list" -> #(handle_tools_list(registry, req), log_level)
    "tools/call" -> #(handle_tools_call(registry, req), log_level)

    // Resources
    "resources/list" -> #(handle_resources_list(req), log_level)
    "resources/read" -> #(handle_resources_read(req), log_level)

    // Prompts
    "prompts/list" -> #(handle_prompts_list(req), log_level)
    "prompts/get" -> #(handle_prompts_get(req), log_level)

    // Logging
    "logging/setLevel" -> handle_logging_set_level(req, log_level)

    // Unknown
    _ -> {
      logging.quick_warn("[MCP] Unknown method: " <> req.method)
      #(protocol.error_response(req.id, types.method_not_found, "Method not found: " <> req.method), log_level)
    }
  }
}

/// Handle initialize request
fn handle_initialize(req: JsonRpcRequest) -> types.JsonRpcResponse {
  logging.quick_info("[MCP] Initializing...")

  let info = ServerInfo(
    name: "vibee-mcp",
    version: "1.1.0",  // Updated version
    capabilities: ServerCapabilities(
      tools: True,
      resources: True,
      prompts: True,
      logging: True,
    ),
  )

  protocol.success_response(req.id, protocol.encode_server_info(info))
}

/// Handle initialized notification
fn handle_initialized(req: JsonRpcRequest) -> types.JsonRpcResponse {
  logging.quick_info("[MCP] Client initialized")
  protocol.success_response(req.id, json.object([]))
}

/// Handle tools/list request with optional category filtering
fn handle_tools_list(registry: ToolRegistry, req: JsonRpcRequest) -> types.JsonRpcResponse {
  logging.quick_info("[MCP] Listing tools...")

  // Check for category filter in params
  let tools_with_annotations = case req.params {
    Some(params) -> {
      let category_str = extract_category_filter(params)
      case category_str {
        "" -> tools.get_all_tools_with_annotations(registry)
        cat_name -> {
          case tools.parse_category(cat_name) {
            Ok(category) -> {
              logging.quick_info("[MCP] Filtering by category: " <> cat_name)
              tools.get_tools_by_category_with_annotations(registry, category)
            }
            Error(_) -> {
              logging.quick_warn("[MCP] Unknown category: " <> cat_name <> ", returning all tools")
              tools.get_all_tools_with_annotations(registry)
            }
          }
        }
      }
    }
    None -> tools.get_all_tools_with_annotations(registry)
  }

  protocol.success_response(req.id, protocol.encode_tools_with_annotations(tools_with_annotations))
}

/// Extract category filter from params
fn extract_category_filter(params: json.Json) -> String {
  let s = json.to_string(params)
  // Try both escaped and unescaped JSON patterns
  case string.split_once(s, "\\\"category\\\":\\\"") {
    Ok(#(_, rest)) -> {
      case string.split_once(rest, "\\\"") {
        Ok(#(value, _)) -> value
        Error(_) -> ""
      }
    }
    Error(_) -> {
      // Try unescaped pattern for some JSON encoders
      case string.split_once(s, "\"category\":\"") {
        Ok(#(_, rest)) -> {
          case string.split_once(rest, "\"") {
            Ok(#(value, _)) -> value
            Error(_) -> ""
          }
        }
        Error(_) -> ""
      }
    }
  }
}

/// Handle tools/call request with caching, rate limiting and audit logging
fn handle_tools_call(registry: ToolRegistry, req: JsonRpcRequest) -> types.JsonRpcResponse {
  case req.params {
    Some(params) -> {
      let name = protocol.extract_tool_name(params)
      let args = protocol.extract_tool_args(params)

      logging.quick_info("[MCP] Calling tool: " <> name)

      // Start timing for audit
      let start_time = audit.start_timer()

      // Check cache for cacheable tools
      case cache.is_cacheable(name) {
        True -> {
          let cache_key = cache.make_cache_key(name, args)
          case cache.get(cache_key) {
            cache.Hit(cached_value) -> {
              logging.quick_info("[MCP] Cache hit for: " <> name)
              // Record cache hit telemetry
              telemetry.metric_cache_access(True)
              // Parse cached JSON and return
              protocol.success_response(req.id, json.string(cached_value))
            }
            _ -> {
              // Cache miss or expired, execute tool with rate limiting
              // Record cache miss telemetry
              telemetry.metric_cache_access(False)
              execute_tool_with_limits(registry, req, name, args, start_time, True)
            }
          }
        }
        False -> {
          // Non-cacheable tool, execute with rate limiting
          execute_tool_with_limits(registry, req, name, args, start_time, False)
        }
      }
    }
    None -> {
      protocol.error_response(req.id, types.invalid_params, "Missing params for tools/call")
    }
  }
}

/// Execute tool with rate limiting, circuit breaker, and optional caching
fn execute_tool_with_limits(
  registry: ToolRegistry,
  req: JsonRpcRequest,
  name: String,
  args: json.Json,
  start_time: Int,
  should_cache: Bool,
) -> types.JsonRpcResponse {
  // Get config for rate limit
  let base_limit = config.get_config().rate_limit_per_minute
  let tool_limit = validation.get_tool_rate_limit(name, base_limit)

  // Check rate limit
  case validation.check_rate_limit(name, tool_limit) {
    validation.Allowed -> {
      // Check circuit breaker for external tools
      let service = get_service_name(name)
      case validation.check_circuit(service) {
        Ok(Nil) -> {
          let result = tools.execute_tool(registry, name, args)
          let duration = audit.get_duration(start_time)

          // Record success/failure for circuit breaker and telemetry
          case result.is_error {
            True -> {
              validation.record_failure(service)
              audit.log_failure(name, args, duration, "Tool returned error")
              // Record telemetry for failed tool call
              telemetry.metric_tool_call(name, False)
              telemetry.metric_tool_duration(name, duration)
            }
            False -> {
              validation.record_success(service)
              audit.log_success(name, args, duration)
              // Record telemetry for successful tool call
              telemetry.metric_tool_call(name, True)
              telemetry.metric_tool_duration(name, duration)

              // Cache successful result for cacheable tools
              case should_cache {
                True -> {
                  let cache_key = cache.make_cache_key(name, args)
                  let ttl = cache.get_tool_ttl(name)
                  let result_json = protocol.encode_tool_result(result)
                  cache.set(cache_key, json.to_string(result_json), ttl)
                }
                False -> Nil
              }
            }
          }

          protocol.success_response(req.id, protocol.encode_tool_result(result))
        }
        Error(err) -> {
          let msg = validation.rate_limit_error_to_string(err)
          logging.quick_warn("[MCP] Circuit open: " <> msg)
          audit.log_circuit_open(name, args, service)
          // Record telemetry for circuit breaker
          telemetry.metric_circuit_breaker(service, "open")
          protocol.error_response(req.id, types.internal_error, msg)
        }
      }
    }
    validation.RateLimited(retry_after) -> {
      let err = validation.TooManyRequests(name, tool_limit, 60)
      let msg = validation.rate_limit_error_to_string(err) <> ". Retry after " <> int_to_string(retry_after) <> "s"
      logging.quick_warn("[MCP] Rate limited: " <> msg)
      audit.log_rate_limited(name, args)
      // Record telemetry for rate limit hit
      telemetry.metric_rate_limit_hit(name)
      protocol.error_response(req.id, types.internal_error, msg)
    }
  }
}

/// Get service name for circuit breaker based on tool name
fn get_service_name(tool_name: String) -> String {
  case string.starts_with(tool_name, "telegram_") {
    True -> "telegram_bridge"
    False -> case string.starts_with(tool_name, "knowledge_") {
      True -> "ollama"
      False -> case tool_name {
        "system_exec" -> "shell"
        "voice_transcribe" -> "whisper"
        _ -> "internal"
      }
    }
  }
}

/// Handle resources/list request
fn handle_resources_list(req: JsonRpcRequest) -> types.JsonRpcResponse {
  logging.quick_info("[MCP] Listing resources...")

  let cfg = config.get_config()
  let data_dir = config.data_dir(cfg)

  // Return available resources
  let resources = json.object([
    #("resources", json.array([
      json.object([
        #("uri", json.string("file://" <> data_dir <> "/dialogs")),
        #("name", json.string("Telegram Dialogs")),
        #("description", json.string("Exported Telegram dialog files")),
        #("mimeType", json.string("application/json")),
      ]),
      json.object([
        #("uri", json.string("file://" <> data_dir <> "/embeddings.json")),
        #("name", json.string("Knowledge Embeddings")),
        #("description", json.string("Vector embeddings for semantic search")),
        #("mimeType", json.string("application/json")),
      ]),
      json.object([
        #("uri", json.string("vibee://config")),
        #("name", json.string("VIBEE Configuration")),
        #("description", json.string("Current MCP server configuration")),
        #("mimeType", json.string("application/json")),
      ]),
    ], fn(x) { x })),
  ])

  protocol.success_response(req.id, resources)
}

/// Handle resources/read request (NEW - MCP spec compliance)
fn handle_resources_read(req: JsonRpcRequest) -> types.JsonRpcResponse {
  logging.quick_info("[MCP] Reading resource...")

  case req.params {
    Some(params) -> {
      let uri = extract_uri(params)

      case uri {
        "vibee://config" -> {
          // Return current config as resource
          let cfg = config.get_config()
          let content = json.object([
            #("bridge_url", json.string(config.bridge_host(cfg))),
            #("bridge_port", json.int(config.bridge_port(cfg))),
            #("data_dir", json.string(config.data_dir(cfg))),
          ])
          protocol.success_response(req.id, json.object([
            #("contents", json.array([
              json.object([
                #("uri", json.string(uri)),
                #("mimeType", json.string("application/json")),
                #("text", json.string(json.to_string(content))),
              ]),
            ], fn(x) { x })),
          ]))
        }
        _ -> {
          // Try to read file resource
          case string.starts_with(uri, "file://") {
            True -> {
              let path = string.drop_start(uri, 7)
              case read_file(path) {
                Ok(content) -> {
                  protocol.success_response(req.id, json.object([
                    #("contents", json.array([
                      json.object([
                        #("uri", json.string(uri)),
                        #("mimeType", json.string(detect_mime_type(path))),
                        #("text", json.string(content)),
                      ]),
                    ], fn(x) { x })),
                  ]))
                }
                Error(_) -> {
                  protocol.error_response(req.id, types.invalid_params, "Resource not found: " <> uri)
                }
              }
            }
            False -> {
              protocol.error_response(req.id, types.invalid_params, "Unknown resource URI scheme: " <> uri)
            }
          }
        }
      }
    }
    None -> {
      protocol.error_response(req.id, types.invalid_params, "Missing params for resources/read")
    }
  }
}

/// Handle prompts/list request
fn handle_prompts_list(req: JsonRpcRequest) -> types.JsonRpcResponse {
  logging.quick_info("[MCP] Listing prompts...")

  let prompts = json.object([
    #("prompts", json.array([
      json.object([
        #("name", json.string("telegram_context")),
        #("description", json.string("Get context from Telegram chat history")),
        #("arguments", json.array([
          json.object([
            #("name", json.string("chat_id")),
            #("description", json.string("Chat ID to get context from")),
            #("required", json.bool(True)),
          ]),
        ], fn(x) { x })),
      ]),
      json.object([
        #("name", json.string("search_knowledge")),
        #("description", json.string("Search through all exported dialogs")),
        #("arguments", json.array([
          json.object([
            #("name", json.string("query")),
            #("description", json.string("Search query")),
            #("required", json.bool(True)),
          ]),
        ], fn(x) { x })),
      ]),
      json.object([
        #("name", json.string("debug_code")),
        #("description", json.string("Debug and fix code issues")),
        #("arguments", json.array([
          json.object([
            #("name", json.string("file_path")),
            #("description", json.string("Path to file with issues")),
            #("required", json.bool(True)),
          ]),
          json.object([
            #("name", json.string("error_message")),
            #("description", json.string("Error message to analyze")),
            #("required", json.bool(False)),
          ]),
        ], fn(x) { x })),
      ]),
    ], fn(x) { x })),
  ])

  protocol.success_response(req.id, prompts)
}

/// Handle prompts/get request (NEW - MCP spec compliance)
fn handle_prompts_get(req: JsonRpcRequest) -> types.JsonRpcResponse {
  logging.quick_info("[MCP] Getting prompt...")

  case req.params {
    Some(params) -> {
      let name = extract_prompt_name(params)
      let args = extract_prompt_arguments(params)

      case name {
        "telegram_context" -> {
          let chat_id = get_arg_string(args, "chat_id", "")
          let messages = [
            json.object([
              #("role", json.string("user")),
              #("content", json.object([
                #("type", json.string("text")),
                #("text", json.string("Get context from Telegram chat " <> chat_id <> ". Use telegram_get_history tool to fetch messages and summarize the key points.")),
              ])),
            ]),
          ]
          protocol.success_response(req.id, json.object([
            #("description", json.string("Context retrieval for chat " <> chat_id)),
            #("messages", json.array(messages, fn(x) { x })),
          ]))
        }
        "search_knowledge" -> {
          let query = get_arg_string(args, "query", "")
          let messages = [
            json.object([
              #("role", json.string("user")),
              #("content", json.object([
                #("type", json.string("text")),
                #("text", json.string("Search knowledge base for: " <> query <> ". Use knowledge_search tool with this query and analyze the results.")),
              ])),
            ]),
          ]
          protocol.success_response(req.id, json.object([
            #("description", json.string("Knowledge search for: " <> query)),
            #("messages", json.array(messages, fn(x) { x })),
          ]))
        }
        "debug_code" -> {
          let file_path = get_arg_string(args, "file_path", "")
          let error_msg = get_arg_string(args, "error_message", "")
          let prompt_text = case error_msg {
            "" -> "Debug the code in " <> file_path <> ". Use debug_build and debug_analyze tools to find and fix issues."
            _ -> "Debug error in " <> file_path <> ": " <> error_msg <> ". Use debug_analyze to understand the issue and suggest fixes."
          }
          let messages = [
            json.object([
              #("role", json.string("user")),
              #("content", json.object([
                #("type", json.string("text")),
                #("text", json.string(prompt_text)),
              ])),
            ]),
          ]
          protocol.success_response(req.id, json.object([
            #("description", json.string("Debug assistance for " <> file_path)),
            #("messages", json.array(messages, fn(x) { x })),
          ]))
        }
        _ -> {
          protocol.error_response(req.id, types.invalid_params, "Unknown prompt: " <> name)
        }
      }
    }
    None -> {
      protocol.error_response(req.id, types.invalid_params, "Missing params for prompts/get")
    }
  }
}

/// Handle logging/setLevel request (NEW - MCP spec compliance)
fn handle_logging_set_level(req: JsonRpcRequest, current_level: LogLevel) -> #(types.JsonRpcResponse, LogLevel) {
  logging.quick_info("[MCP] Setting log level...")

  case req.params {
    Some(params) -> {
      let level_str = extract_log_level(params)
      let new_level = case level_str {
        "debug" -> LogDebug
        "info" -> LogInfo
        "warning" -> LogWarning
        "error" -> LogErr
        _ -> current_level
      }

      logging.quick_info("[MCP] Log level set to: " <> level_str)

      #(protocol.success_response(req.id, json.object([])), new_level)
    }
    None -> {
      #(protocol.error_response(req.id, types.invalid_params, "Missing params for logging/setLevel"), current_level)
    }
  }
}

/// Handle ping request
fn handle_ping(req: JsonRpcRequest) -> types.JsonRpcResponse {
  protocol.success_response(req.id, json.object([]))
}

// ============================================================
// Helper Functions
// ============================================================

fn extract_uri(params: json.Json) -> String {
  let s = json.to_string(params)
  case string.contains(s, "\"uri\":\"") {
    True -> {
      case string.split_once(s, "\"uri\":\"") {
        Ok(#(_, rest)) -> {
          case string.split_once(rest, "\"") {
            Ok(#(uri, _)) -> uri
            Error(_) -> ""
          }
        }
        Error(_) -> ""
      }
    }
    False -> ""
  }
}

fn extract_prompt_name(params: json.Json) -> String {
  let s = json.to_string(params)
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

fn extract_prompt_arguments(params: json.Json) -> json.Json {
  // Extract arguments object from params
  let s = json.to_string(params)
  case string.contains(s, "\"arguments\":") {
    True -> {
      // Return the params itself since get_arg_string will parse from it
      params
    }
    False -> json.object([])
  }
}

fn extract_log_level(params: json.Json) -> String {
  let s = json.to_string(params)
  case string.contains(s, "\"level\":\"") {
    True -> {
      case string.split_once(s, "\"level\":\"") {
        Ok(#(_, rest)) -> {
          case string.split_once(rest, "\"") {
            Ok(#(level, _)) -> level
            Error(_) -> "info"
          }
        }
        Error(_) -> "info"
      }
    }
    False -> "info"
  }
}

fn get_arg_string(args: json.Json, key: String, default: String) -> String {
  let s = json.to_string(args)
  // Try to find the key in arguments object or directly in params
  // Pattern: "arguments":{"chat_id":"123"} or just {"chat_id":"123"}
  let pattern = "\"" <> key <> "\":\""
  case string.contains(s, pattern) {
    True -> {
      case string.split_once(s, pattern) {
        Ok(#(_, rest)) -> {
          case string.split_once(rest, "\"") {
            Ok(#(value, _)) -> value
            Error(_) -> default
          }
        }
        Error(_) -> default
      }
    }
    False -> {
      // Also check for numeric values: "chat_id":123
      let num_pattern = "\"" <> key <> "\":"
      case string.contains(s, num_pattern) {
        True -> {
          case string.split_once(s, num_pattern) {
            Ok(#(_, rest)) -> {
              // Extract until comma, brace or end
              extract_number_value(rest)
            }
            Error(_) -> default
          }
        }
        False -> default
      }
    }
  }
}

fn extract_number_value(s: String) -> String {
  // Extract numeric value until delimiter
  do_extract_number(s, "")
}

fn do_extract_number(s: String, acc: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(c, rest)) -> {
      case c {
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "-" -> {
          do_extract_number(rest, acc <> c)
        }
        _ -> acc
      }
    }
    Error(_) -> acc
  }
}

fn detect_mime_type(path: String) -> String {
  case string.ends_with(path, ".json") {
    True -> "application/json"
    False -> case string.ends_with(path, ".txt") {
      True -> "text/plain"
      False -> case string.ends_with(path, ".md") {
        True -> "text/markdown"
        False -> case string.ends_with(path, ".gleam") {
          True -> "text/x-gleam"
          False -> case string.ends_with(path, ".erl") {
            True -> "text/x-erlang"
            False -> "application/octet-stream"
          }
        }
      }
    }
  }
}

// FFI for stdio and file operations
@external(erlang, "vibee_io_ffi", "get_line")
fn io_get_line() -> Result(String, Nil)

@external(erlang, "vibee_io_ffi", "read_file")
fn read_file(path: String) -> Result(String, Nil)

fn read_line() -> Result(String, Nil) {
  io_get_line()
}

fn write_line(s: String) -> Nil {
  io.println(s)
}

@external(erlang, "erlang", "integer_to_list")
fn int_to_charlist(n: Int) -> List(Int)

fn int_to_string(n: Int) -> String {
  charlist_to_string(int_to_charlist(n))
}

@external(erlang, "erlang", "list_to_binary")
fn charlist_to_string(chars: List(Int)) -> String
