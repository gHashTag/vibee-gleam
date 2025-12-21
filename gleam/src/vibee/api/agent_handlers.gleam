// Agent Control API Handlers
// HTTP endpoints for agent configuration and control
// Integrates with dynamic_config (ETS + PostgreSQL) and agent_registry

import gleam/bytes_tree
import gleam/bit_array
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import mist.{type Connection, type ResponseData}
import vibee/config/dynamic_config
import vibee/db/postgres
import vibee/agent/agent_registry
import vibee/agent/polling_actor
import vibee/mcp/config
import vibee/vibe_logger

// =============================================================================
// Types
// =============================================================================

/// Agent configuration from UI
pub type AgentConfig {
  AgentConfig(
    cooldown_ms: Int,
    confidence: Float,
    strategy: String,
    auto_reply: Bool,
    digital_twin: Bool,
    sniper_mode: Bool,
    forward_leads: Bool,
  )
}

// Default values
const default_cooldown_ms = 5000
const default_confidence = 0.7
const default_strategy = "selective"

// =============================================================================
// Handlers
// =============================================================================

/// GET /api/agent/config - Get current agent configuration
pub fn config_get_handler() -> Response(ResponseData) {
  // Try to get from database
  let config = get_agent_config_from_db()

  let body = json.object([
    #("cooldown_ms", json.int(config.cooldown_ms)),
    #("confidence", json.float(config.confidence)),
    #("strategy", json.string(config.strategy)),
    #("auto_reply", json.bool(config.auto_reply)),
    #("digital_twin", json.bool(config.digital_twin)),
    #("sniper_mode", json.bool(config.sniper_mode)),
    #("forward_leads", json.bool(config.forward_leads)),
  ])

  json_response(200, body)
}

/// POST /api/agent/config - Update agent configuration
pub fn config_update_handler(req: Request(Connection)) -> Response(ResponseData) {
  case mist.read_body(req, 1024 * 10) {
    Error(_) -> {
      json_response(400, json.object([#("error", json.string("Failed to read body"))]))
    }
    Ok(req_with_body) -> {
      case bit_array.to_string(req_with_body.body) {
        Error(_) -> json_response(400, json.object([#("error", json.string("Invalid UTF-8"))]))
        Ok(body_str) -> {
          case parse_config_update(body_str) {
            Error(err) -> json_response(400, json.object([#("error", json.string(err))]))
            Ok(updates) -> {
              // Apply updates to database
              let result = apply_config_updates(updates)

              case result {
                Ok(_) -> {
                  // Get updated config
                  let config = get_agent_config_from_db()

                  let body = json.object([
                    #("success", json.bool(True)),
                    #("config", json.object([
                      #("cooldown_ms", json.int(config.cooldown_ms)),
                      #("confidence", json.float(config.confidence)),
                      #("strategy", json.string(config.strategy)),
                      #("auto_reply", json.bool(config.auto_reply)),
                      #("digital_twin", json.bool(config.digital_twin)),
                      #("sniper_mode", json.bool(config.sniper_mode)),
                      #("forward_leads", json.bool(config.forward_leads)),
                    ])),
                  ])
                  json_response(200, body)
                }
                Error(err) -> {
                  json_response(500, json.object([#("error", json.string(err))]))
                }
              }
            }
          }
        }
      }
    }
  }
}

/// POST /api/agent/start - Start the agent
pub fn start_handler(_req: Request(Connection)) -> Response(ResponseData) {
  vibe_logger.new("agent")
    |> vibe_logger.info("Starting agent via API")

  // Get agent from registry
  let agents = agent_registry.list_all()

  case list.first(agents) {
    Ok(agent) -> {
      // Update status to running
      agent_registry.update_status(agent.id, agent_registry.Running)

      let body = json.object([
        #("success", json.bool(True)),
        #("message", json.string("Agent started")),
        #("agent_id", json.string(agent.id)),
      ])
      json_response(200, body)
    }
    Error(_) -> {
      // No agents registered yet - start polling actor
      case start_polling_agent() {
        Ok(agent_id) -> {
          let body = json.object([
            #("success", json.bool(True)),
            #("message", json.string("Polling agent started")),
            #("agent_id", json.string(agent_id)),
          ])
          json_response(200, body)
        }
        Error(err) -> {
          let body = json.object([
            #("success", json.bool(False)),
            #("error", json.string(err)),
          ])
          json_response(500, body)
        }
      }
    }
  }
}

/// POST /api/agent/stop - Stop the agent
pub fn stop_handler(_req: Request(Connection)) -> Response(ResponseData) {
  vibe_logger.new("agent")
    |> vibe_logger.info("Stopping agent via API")

  let agents = agent_registry.list_all()

  case list.first(agents) {
    Ok(agent) -> {
      agent_registry.update_status(agent.id, agent_registry.Stopped)

      let body = json.object([
        #("success", json.bool(True)),
        #("message", json.string("Agent stopped")),
        #("agent_id", json.string(agent.id)),
      ])
      json_response(200, body)
    }
    Error(_) -> {
      let body = json.object([
        #("success", json.bool(True)),
        #("message", json.string("No active agents")),
      ])
      json_response(200, body)
    }
  }
}

/// POST /api/agent/pause - Pause the agent
pub fn pause_handler(_req: Request(Connection)) -> Response(ResponseData) {
  vibe_logger.new("agent")
    |> vibe_logger.info("Pausing agent via API")

  let agents = agent_registry.list_all()

  case list.first(agents) {
    Ok(agent) -> {
      agent_registry.update_status(agent.id, agent_registry.Paused)

      let body = json.object([
        #("success", json.bool(True)),
        #("message", json.string("Agent paused")),
        #("agent_id", json.string(agent.id)),
      ])
      json_response(200, body)
    }
    Error(_) -> {
      let body = json.object([
        #("success", json.bool(True)),
        #("message", json.string("No active agents")),
      ])
      json_response(200, body)
    }
  }
}

/// POST /api/agent/reset - Reset agent state
pub fn reset_handler(_req: Request(Connection)) -> Response(ResponseData) {
  vibe_logger.new("agent")
    |> vibe_logger.info("Resetting agent via API")

  // Reset to default configuration
  let _ = apply_config_updates([
    #("cooldown_ms", json.int(default_cooldown_ms)),
    #("confidence", json.float(default_confidence)),
    #("strategy", json.string(default_strategy)),
    #("auto_reply", json.bool(True)),
    #("digital_twin", json.bool(True)),
    #("sniper_mode", json.bool(False)),
    #("forward_leads", json.bool(False)),
  ])

  // Reset agent message counts
  let agents = agent_registry.list_all()
  list.each(agents, fn(agent) {
    agent_registry.reset_counts(agent.id)
  })

  let body = json.object([
    #("success", json.bool(True)),
    #("message", json.string("Agent reset to defaults")),
  ])
  json_response(200, body)
}

/// GET /api/agent/status - Enhanced status with config
pub fn status_handler() -> Response(ResponseData) {
  let agents = agent_registry.list_all()
  let config = get_agent_config_from_db()

  // Calculate totals
  let total_messages = list.fold(agents, 0, fn(acc, a) { acc + a.message_count })
  let total_errors = list.fold(agents, 0, fn(acc, a) { acc + a.error_count })
  let running_count = list.count(agents, fn(a) { a.status == agent_registry.Running })

  let agents_json = json.array(agents, fn(a) {
    json.object([
      #("id", json.string(a.id)),
      #("type", json.string(agent_type_to_string(a.agent_type))),
      #("status", json.string(agent_status_to_string(a.status))),
      #("started_at", json.string(a.started_at)),
      #("last_activity", json.string(a.last_activity)),
      #("message_count", json.int(a.message_count)),
      #("error_count", json.int(a.error_count)),
    ])
  })

  let body = json.object([
    #("status", json.string(case running_count > 0 {
      True -> "running"
      False -> "stopped"
    })),
    #("agents", agents_json),
    #("agents_count", json.int(list.length(agents))),
    #("running_count", json.int(running_count)),
    #("total_messages", json.int(total_messages)),
    #("total_errors", json.int(total_errors)),
    #("config", json.object([
      #("cooldown_ms", json.int(config.cooldown_ms)),
      #("confidence", json.float(config.confidence)),
      #("strategy", json.string(config.strategy)),
      #("auto_reply", json.bool(config.auto_reply)),
      #("digital_twin", json.bool(config.digital_twin)),
      #("sniper_mode", json.bool(config.sniper_mode)),
      #("forward_leads", json.bool(config.forward_leads)),
    ])),
  ])

  json_response(200, body)
}

// =============================================================================
// Helpers
// =============================================================================

fn get_agent_config_from_db() -> AgentConfig {
  case postgres.get_global_pool() {
    None -> default_config()
    Some(pool) -> {
      // Get from global_config table
      let cooldown = case dynamic_config.get_int(pool, "agent_cooldown_ms") {
        Ok(v) -> v
        Error(_) -> default_cooldown_ms
      }

      let confidence = case dynamic_config.get_string(pool, "agent_confidence") {
        Ok(v) -> case float.parse(v) {
          Ok(f) -> f
          Error(_) -> default_confidence
        }
        Error(_) -> default_confidence
      }

      let strategy = case dynamic_config.get_string(pool, "agent_strategy") {
        Ok(v) -> v
        Error(_) -> default_strategy
      }

      let auto_reply = case dynamic_config.get_bool(pool, "agent_auto_reply") {
        Ok(v) -> v
        Error(_) -> True
      }

      let digital_twin = case dynamic_config.get_bool(pool, "agent_digital_twin") {
        Ok(v) -> v
        Error(_) -> True
      }

      let sniper_mode = case dynamic_config.get_bool(pool, "agent_sniper_mode") {
        Ok(v) -> v
        Error(_) -> False
      }

      let forward_leads = case dynamic_config.get_bool(pool, "agent_forward_leads") {
        Ok(v) -> v
        Error(_) -> False
      }

      AgentConfig(
        cooldown_ms: cooldown,
        confidence: confidence,
        strategy: strategy,
        auto_reply: auto_reply,
        digital_twin: digital_twin,
        sniper_mode: sniper_mode,
        forward_leads: forward_leads,
      )
    }
  }
}

fn default_config() -> AgentConfig {
  AgentConfig(
    cooldown_ms: default_cooldown_ms,
    confidence: default_confidence,
    strategy: default_strategy,
    auto_reply: True,
    digital_twin: True,
    sniper_mode: False,
    forward_leads: False,
  )
}

fn parse_config_update(body: String) -> Result(List(#(String, json.Json)), String) {
  // Simple JSON parsing for config updates
  // Expected format: {"cooldown_ms": 5000, "confidence": 0.7, ...}
  let updates = []

  // Parse cooldown_ms
  let updates = case extract_int_field(body, "cooldown_ms") {
    Some(v) -> [#("cooldown_ms", json.int(v)), ..updates]
    None -> updates
  }

  // Parse confidence
  let updates = case extract_float_field(body, "confidence") {
    Some(v) -> [#("confidence", json.float(v)), ..updates]
    None -> updates
  }

  // Parse strategy
  let updates = case extract_string_field(body, "strategy") {
    Some(v) -> [#("strategy", json.string(v)), ..updates]
    None -> updates
  }

  // Parse booleans
  let updates = case extract_bool_field(body, "auto_reply") {
    Some(v) -> [#("auto_reply", json.bool(v)), ..updates]
    None -> updates
  }

  let updates = case extract_bool_field(body, "digital_twin") {
    Some(v) -> [#("digital_twin", json.bool(v)), ..updates]
    None -> updates
  }

  let updates = case extract_bool_field(body, "sniper_mode") {
    Some(v) -> [#("sniper_mode", json.bool(v)), ..updates]
    None -> updates
  }

  let updates = case extract_bool_field(body, "forward_leads") {
    Some(v) -> [#("forward_leads", json.bool(v)), ..updates]
    None -> updates
  }

  Ok(updates)
}

fn apply_config_updates(updates: List(#(String, json.Json))) -> Result(Nil, String) {
  case postgres.get_global_pool() {
    None -> Error("Database not available")
    Some(pool) -> {
      list.each(updates, fn(update) {
        let #(key, value) = update
        let db_key = "agent_" <> key
        let value_str = json.to_string(value)
        let value_type = case key {
          "cooldown_ms" -> "int"
          "confidence" -> "float"
          "strategy" -> "string"
          _ -> "bool"
        }
        let _ = dynamic_config.set_config(pool, db_key, value_str, value_type, None)
        Nil
      })
      Ok(Nil)
    }
  }
}

fn start_polling_agent() -> Result(String, String) {
  // This would start the polling actor
  // For now return a placeholder
  Error("Use gleam run to start agent")
}

fn agent_type_to_string(t: agent_registry.AgentType) -> String {
  case t {
    agent_registry.PollingAgent -> "polling"
    agent_registry.WebSocketAgent -> "websocket"
    agent_registry.GenericAgent -> "generic"
    agent_registry.SuperAgent -> "super"
    agent_registry.EventBusAgent -> "event_bus"
  }
}

fn agent_status_to_string(s: agent_registry.AgentStatus) -> String {
  case s {
    agent_registry.Starting -> "starting"
    agent_registry.Running -> "running"
    agent_registry.Paused -> "paused"
    agent_registry.Stopping -> "stopping"
    agent_registry.Stopped -> "stopped"
    agent_registry.Failed(_) -> "failed"
  }
}

// Simple field extractors
fn extract_int_field(json_str: String, key: String) -> Option(Int) {
  let pattern = "\"" <> key <> "\":"
  case string.split(json_str, pattern) {
    [_, rest, ..] -> {
      let trimmed = string.trim_start(rest)
      let digits = take_digits(trimmed)
      case int.parse(digits) {
        Ok(v) -> Some(v)
        Error(_) -> None
      }
    }
    _ -> None
  }
}

fn extract_float_field(json_str: String, key: String) -> Option(Float) {
  let pattern = "\"" <> key <> "\":"
  case string.split(json_str, pattern) {
    [_, rest, ..] -> {
      let trimmed = string.trim_start(rest)
      let num = take_number(trimmed)
      case float.parse(num) {
        Ok(v) -> Some(v)
        Error(_) -> None
      }
    }
    _ -> None
  }
}

fn extract_string_field(json_str: String, key: String) -> Option(String) {
  let pattern = "\"" <> key <> "\":\""
  case string.split(json_str, pattern) {
    [_, rest, ..] -> {
      case string.split(rest, "\"") {
        [value, ..] -> Some(value)
        _ -> None
      }
    }
    _ -> None
  }
}

fn extract_bool_field(json_str: String, key: String) -> Option(Bool) {
  let pattern = "\"" <> key <> "\":"
  case string.split(json_str, pattern) {
    [_, rest, ..] -> {
      let trimmed = string.trim_start(rest)
      case string.starts_with(trimmed, "true") {
        True -> Some(True)
        False -> case string.starts_with(trimmed, "false") {
          True -> Some(False)
          False -> None
        }
      }
    }
    _ -> None
  }
}

fn take_digits(s: String) -> String {
  let chars = string.to_graphemes(s)
  let digits = list.take_while(chars, fn(c) {
    c == "0" || c == "1" || c == "2" || c == "3" || c == "4" ||
    c == "5" || c == "6" || c == "7" || c == "8" || c == "9"
  })
  string.concat(digits)
}

fn take_number(s: String) -> String {
  let chars = string.to_graphemes(s)
  let digits = list.take_while(chars, fn(c) {
    c == "0" || c == "1" || c == "2" || c == "3" || c == "4" ||
    c == "5" || c == "6" || c == "7" || c == "8" || c == "9" ||
    c == "." || c == "-"
  })
  string.concat(digits)
}

// Response helper
fn json_response(status: Int, body: json.Json) -> Response(ResponseData) {
  let body_string = json.to_string(body)
  let body_bytes = bytes_tree.from_string(body_string)

  response.new(status)
  |> response.set_header("content-type", "application/json")
  |> response.set_header("access-control-allow-origin", "*")
  |> response.set_body(mist.Bytes(body_bytes))
}
