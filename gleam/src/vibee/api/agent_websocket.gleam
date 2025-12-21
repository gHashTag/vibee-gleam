// Agent WebSocket - Real-time agent status updates via WebSocket
// Endpoint: /ws/agents

import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import mist.{type Connection, type ResponseData}
import vibee/agent/agent_registry
import vibee/vibe_logger

// =============================================================================
// Types
// =============================================================================

/// WebSocket state for agent updates
pub type AgentWsState {
  AgentWsState(
    client_id: String,
    subscribed: Bool,
    last_update: Int,
  )
}

/// WebSocket message types
pub type AgentWsMessage {
  AgentsUpdate(String)
  Tick
}

// =============================================================================
// WebSocket Handler
// =============================================================================

/// Main WebSocket handler for /ws/agents
pub fn handler(req: Request(Connection)) -> Response(ResponseData) {
  let logger = vibe_logger.new("agent_ws")
  vibe_logger.info(logger, "New agent WebSocket connection")

  // Create a ticker subject for periodic updates
  let ticker = process.new_subject()

  // Start ticker process
  start_ticker(ticker)

  // Create selector for ticker messages
  let selector =
    process.new_selector()
    |> process.select_map(for: ticker, mapping: fn(_) { Tick })

  let client_id = generate_client_id()
  let state = AgentWsState(client_id: client_id, subscribed: True, last_update: 0)

  mist.websocket(
    request: req,
    on_init: fn(conn) {
      // Send initial agent list
      let agents_json = build_agents_update()
      let _ = mist.send_text_frame(conn, agents_json)

      vibe_logger.info(
        logger |> vibe_logger.with_data("client_id", json.string(client_id)),
        "Agent WebSocket initialized",
      )

      #(state, Some(selector))
    },
    on_close: fn(state) {
      vibe_logger.info(
        logger |> vibe_logger.with_data("client_id", json.string(state.client_id)),
        "Agent WebSocket closed",
      )
    },
    handler: handle_message,
  )
}

/// Handle incoming WebSocket messages
fn handle_message(
  state: AgentWsState,
  message: mist.WebsocketMessage(AgentWsMessage),
  conn: mist.WebsocketConnection,
) {
  case message {
    // Ping/pong for keepalive
    mist.Text("ping") -> {
      let _ = mist.send_text_frame(conn, "pong")
      mist.continue(state)
    }

    // Request immediate update
    mist.Text("refresh") -> {
      let agents_json = build_agents_update()
      let _ = mist.send_text_frame(conn, agents_json)
      mist.continue(state)
    }

    // Get specific agent
    mist.Text(text) -> {
      let _ = case parse_get_agent_request(text) {
        Some(agent_id) -> {
          let agent_json = build_agent_detail(agent_id)
          mist.send_text_frame(conn, agent_json)
        }
        None -> Ok(Nil)
      }
      mist.continue(state)
    }

    mist.Binary(_) -> {
      mist.continue(state)
    }

    // Periodic tick - send agent updates
    mist.Custom(Tick) -> {
      case state.subscribed {
        True -> {
          let agents_json = build_agents_update()
          let _ = mist.send_text_frame(conn, agents_json)
          let new_state = AgentWsState(..state, last_update: get_timestamp())
          mist.continue(new_state)
        }
        False -> mist.continue(state)
      }
    }

    mist.Custom(AgentsUpdate(json_str)) -> {
      let _ = mist.send_text_frame(conn, json_str)
      mist.continue(state)
    }

    mist.Closed | mist.Shutdown -> {
      mist.stop()
    }
  }
}

// =============================================================================
// Helpers
// =============================================================================

/// Build JSON update with all agents
fn build_agents_update() -> String {
  agent_registry.init()
  let agents = agent_registry.list_all()
  let total = list.length(agents)
  let running = agent_registry.count_running()

  let agents_json =
    json.array(agents, fn(agent) { agent_registry.to_json(agent) })

  json.object([
    #("type", json.string("agents_update")),
    #("timestamp", json.string(get_iso_timestamp())),
    #("agents", agents_json),
    #("summary", json.object([
      #("total", json.int(total)),
      #("running", json.int(running)),
      #("stopped", json.int(total - running)),
    ])),
  ])
  |> json.to_string
}

/// Build JSON for single agent detail
fn build_agent_detail(agent_id: String) -> String {
  agent_registry.init()
  case agent_registry.get(agent_id) {
    Some(agent) -> {
      json.object([
        #("type", json.string("agent_detail")),
        #("agent", agent_registry.to_json(agent)),
      ])
      |> json.to_string
    }
    None -> {
      json.object([
        #("type", json.string("error")),
        #("error", json.string("Agent not found")),
        #("agent_id", json.string(agent_id)),
      ])
      |> json.to_string
    }
  }
}

/// Parse "get:agent_id" request
fn parse_get_agent_request(text: String) -> Option(String) {
  case text {
    "get:" <> agent_id -> Some(agent_id)
    _ -> None
  }
}

/// Start a ticker process that sends updates every 5 seconds
fn start_ticker(subject: process.Subject(Nil)) -> Nil {
  ffi_spawn_ticker(subject)
}

@external(erlang, "vibee_agent_ws_ffi", "spawn_ticker")
fn ffi_spawn_ticker(subject: process.Subject(Nil)) -> Nil

/// Generate a unique client ID
fn generate_client_id() -> String {
  "ws_" <> int.to_string(get_timestamp()) <> "_" <> random_suffix()
}

/// Get current Unix timestamp
@external(erlang, "vibee_agent_registry_ffi", "get_unix_timestamp")
fn get_timestamp() -> Int

/// Get ISO timestamp
@external(erlang, "vibee_agent_registry_ffi", "get_iso_timestamp_string")
fn get_iso_timestamp() -> String

/// Generate random suffix
fn random_suffix() -> String {
  int.to_string(ffi_random())
}

@external(erlang, "rand", "uniform")
fn ffi_random() -> Int
