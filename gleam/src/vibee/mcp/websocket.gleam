// MCP WebSocket Transport
// Implements MCP over WebSocket for bidirectional streaming
// MCP spec: https://modelcontextprotocol.io/specification

import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/option.{None, Some}
import gleam/string
import mist.{type Connection, type ResponseData}
import vibee/logging
import vibee/mcp/config
import vibee/mcp/protocol
import vibee/mcp/tools.{type ToolRegistry}
import vibee/mcp/types
import vibee/mcp/audit
import vibee/mcp/cache
import vibee/mcp/telemetry
import vibee/mcp/validation

/// WebSocket state for MCP connection
pub type McpWsState {
  McpWsState(
    registry: ToolRegistry,
    session_id: String,
    initialized: Bool,
  )
}

/// WebSocket message from client
pub type WsIncoming {
  TextMessage(String)
  BinaryMessage(BitArray)
  Closed
}

/// Start MCP WebSocket handler
pub fn handler(
  req: Request(Connection),
  registry: ToolRegistry,
) -> Response(ResponseData) {
  let session_id = generate_session_id()
  logging.info("[MCP-WS] New connection: " <> session_id)

  let selector = process.new_selector()
  let initial_state = McpWsState(
    registry: registry,
    session_id: session_id,
    initialized: False,
  )

  mist.websocket(
    request: req,
    on_init: fn(_conn) {
      logging.info("[MCP-WS] WebSocket initialized for session: " <> session_id)
      #(initial_state, Some(selector))
    },
    on_close: fn(state) {
      logging.info("[MCP-WS] Connection closed: " <> state.session_id)
      Nil
    },
    handler: handle_ws_message,
  )
}

/// Handle incoming WebSocket message
fn handle_ws_message(
  state: McpWsState,
  message: mist.WebsocketMessage(McpWsState),
  conn: mist.WebsocketConnection,
) {
  case message {
    mist.Text(text) -> {
      logging.debug("[MCP-WS] Received: " <> string.slice(text, 0, 100))

      // Parse and handle JSON-RPC request
      let #(response, new_state) = handle_jsonrpc_request(state, text)

      // Send response back through WebSocket
      let _ = mist.send_text_frame(conn, response)

      mist.continue(new_state)
    }

    mist.Binary(_data) -> {
      // Binary not supported for JSON-RPC
      logging.warn("[MCP-WS] Received binary data, ignoring")
      mist.continue(state)
    }

    mist.Custom(_) -> {
      mist.continue(state)
    }

    mist.Closed | mist.Shutdown -> {
      logging.info("[MCP-WS] Connection closed")
      mist.stop()
    }
  }
}

/// Handle JSON-RPC request and return response
fn handle_jsonrpc_request(
  state: McpWsState,
  input: String,
) -> #(String, McpWsState) {
  case protocol.parse_request(input) {
    Ok(req) -> {
      let #(response, new_state) = dispatch_method(state, req)
      let encoded = protocol.encode_response(response)
      #(encoded, new_state)
    }
    Error(err) -> {
      logging.error("[MCP-WS] Parse error: " <> err)
      let response = protocol.encode_response(
        protocol.error_response(None, types.parse_error, err)
      )
      #(response, state)
    }
  }
}

/// Dispatch request to appropriate handler
fn dispatch_method(
  state: McpWsState,
  req: types.JsonRpcRequest,
) -> #(types.JsonRpcResponse, McpWsState) {
  logging.info("[MCP-WS] Method: " <> req.method)

  case req.method {
    // Core protocol
    "initialize" -> {
      let response = handle_initialize(req)
      #(response, McpWsState(..state, initialized: True))
    }

    "initialized" -> {
      #(protocol.success_response(req.id, json.object([])), state)
    }

    "ping" -> {
      #(protocol.success_response(req.id, json.object([])), state)
    }

    // Tools
    "tools/list" -> {
      let response = handle_tools_list(state.registry, req)
      #(response, state)
    }

    "tools/call" -> {
      let response = handle_tools_call(state.registry, req)
      #(response, state)
    }

    // Resources
    "resources/list" -> {
      let response = handle_resources_list(req)
      #(response, state)
    }

    "resources/read" -> {
      let response = handle_resources_read(req)
      #(response, state)
    }

    // Prompts
    "prompts/list" -> {
      let response = handle_prompts_list(req)
      #(response, state)
    }

    "prompts/get" -> {
      let response = handle_prompts_get(req)
      #(response, state)
    }

    // Unknown
    _ -> {
      logging.warn("[MCP-WS] Unknown method: " <> req.method)
      #(
        protocol.error_response(req.id, types.method_not_found, "Method not found: " <> req.method),
        state,
      )
    }
  }
}

/// Handle initialize request
fn handle_initialize(req: types.JsonRpcRequest) -> types.JsonRpcResponse {
  logging.info("[MCP-WS] Initializing...")

  let info = types.ServerInfo(
    name: "vibee-mcp-ws",
    version: "1.1.0",
    capabilities: types.ServerCapabilities(
      tools: True,
      resources: True,
      prompts: True,
      logging: True,
    ),
  )

  protocol.success_response(req.id, protocol.encode_server_info(info))
}

/// Handle tools/list
fn handle_tools_list(
  registry: ToolRegistry,
  req: types.JsonRpcRequest,
) -> types.JsonRpcResponse {
  logging.info("[MCP-WS] Listing tools...")

  let tools_with_annotations = case req.params {
    Some(params) -> {
      let category_str = extract_category_filter(params)
      case category_str {
        "" -> tools.get_all_tools_with_annotations(registry)
        cat_name -> {
          case tools.parse_category(cat_name) {
            Ok(category) -> {
              logging.info("[MCP-WS] Filtering by category: " <> cat_name)
              tools.get_tools_by_category_with_annotations(registry, category)
            }
            Error(_) -> {
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

/// Handle tools/call with caching, rate limiting and telemetry
fn handle_tools_call(
  registry: ToolRegistry,
  req: types.JsonRpcRequest,
) -> types.JsonRpcResponse {
  case req.params {
    Some(params) -> {
      let name = protocol.extract_tool_name(params)
      let args = protocol.extract_tool_args(params)

      logging.info("[MCP-WS] Calling tool: " <> name)

      let start_time = audit.start_timer()

      // Check cache for cacheable tools
      case cache.is_cacheable(name) {
        True -> {
          let cache_key = cache.make_cache_key(name, args)
          case cache.get(cache_key) {
            cache.Hit(cached_value) -> {
              logging.info("[MCP-WS] Cache hit for: " <> name)
              telemetry.metric_cache_access(True)
              protocol.success_response(req.id, json.string(cached_value))
            }
            _ -> {
              telemetry.metric_cache_access(False)
              execute_tool(registry, req, name, args, start_time, True)
            }
          }
        }
        False -> {
          execute_tool(registry, req, name, args, start_time, False)
        }
      }
    }
    None -> {
      protocol.error_response(req.id, types.invalid_params, "Missing params for tools/call")
    }
  }
}

/// Execute tool with rate limiting and telemetry
fn execute_tool(
  registry: ToolRegistry,
  req: types.JsonRpcRequest,
  name: String,
  args: json.Json,
  start_time: Int,
  should_cache: Bool,
) -> types.JsonRpcResponse {
  let cfg = config.get_config()
  let tool_limit = validation.get_tool_rate_limit(name, cfg.rate_limit_per_minute)

  case validation.check_rate_limit(name, tool_limit) {
    validation.Allowed -> {
      let service = get_service_name(name)
      case validation.check_circuit(service) {
        Ok(Nil) -> {
          let result = tools.execute_tool(registry, name, args)
          let duration = audit.get_duration(start_time)

          case result.is_error {
            True -> {
              validation.record_failure(service)
              audit.log_failure(name, args, duration, "Tool returned error")
              telemetry.metric_tool_call(name, False)
              telemetry.metric_tool_duration(name, duration)
            }
            False -> {
              validation.record_success(service)
              audit.log_success(name, args, duration)
              telemetry.metric_tool_call(name, True)
              telemetry.metric_tool_duration(name, duration)

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
          logging.warn("[MCP-WS] Circuit open: " <> msg)
          telemetry.metric_circuit_breaker(service, "open")
          protocol.error_response(req.id, types.internal_error, msg)
        }
      }
    }
    validation.RateLimited(retry_after) -> {
      let err = validation.TooManyRequests(name, tool_limit, 60)
      let msg = validation.rate_limit_error_to_string(err)
      logging.warn("[MCP-WS] Rate limited: " <> msg)
      telemetry.metric_rate_limit_hit(name)
      protocol.error_response(req.id, types.internal_error, msg)
    }
  }
}

/// Get service name for circuit breaker
fn get_service_name(tool_name: String) -> String {
  case string.starts_with(tool_name, "telegram_") {
    True -> "telegram_bridge"
    False -> case string.starts_with(tool_name, "knowledge_") {
      True -> "ollama"
      False -> case tool_name {
        "voice_transcribe" -> "whisper"
        _ -> "local"
      }
    }
  }
}

/// Handle resources/list
fn handle_resources_list(req: types.JsonRpcRequest) -> types.JsonRpcResponse {
  protocol.success_response(req.id, json.object([
    #("resources", json.array([], fn(x) { x })),
  ]))
}

/// Handle resources/read
fn handle_resources_read(req: types.JsonRpcRequest) -> types.JsonRpcResponse {
  protocol.error_response(req.id, types.invalid_params, "Resource not found")
}

/// Handle prompts/list
fn handle_prompts_list(req: types.JsonRpcRequest) -> types.JsonRpcResponse {
  protocol.success_response(req.id, json.object([
    #("prompts", json.array([], fn(x) { x })),
  ]))
}

/// Handle prompts/get
fn handle_prompts_get(req: types.JsonRpcRequest) -> types.JsonRpcResponse {
  protocol.error_response(req.id, types.invalid_params, "Prompt not found")
}

/// Extract category filter from params
fn extract_category_filter(params: json.Json) -> String {
  let s = json.to_string(params)
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

/// Generate unique session ID
fn generate_session_id() -> String {
  generate_session_id_ffi()
}

@external(erlang, "vibee_mcp_ws_ffi", "generate_session_id")
fn generate_session_id_ffi() -> String
