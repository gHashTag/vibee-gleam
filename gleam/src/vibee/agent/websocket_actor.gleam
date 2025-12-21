// VIBEE WebSocket Actor
// OTP Actor for real-time Telegram updates via Go Bridge WebSocket
// Replaces HTTP polling with push-based event streaming

import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/set.{type Set}
import gleam/string
import vibee/agent/agent_registry
import vibee/agent/ws_events.{type TelegramEvent}
import vibee/db/postgres
import vibee/events/event_bus
import vibee/mcp/config
import vibee/telegram/telegram_agent
import vibee/vibe_logger

/// WebSocket connection handle (opaque from FFI)
pub type WsConnection

/// WebSocket Actor State
pub type WebSocketState {
  WebSocketState(
    config: telegram_agent.TelegramAgentConfig,
    agent_state: telegram_agent.AgentState,
    connection: Option(WsConnection),
    event_bus: Option(Subject(event_bus.PubSubMessage)),
    seen_ids: Set(String),
    agent_id: String,
    logger: vibe_logger.VibeLogger,
    reconnect_attempts: Int,
    message_count: Int,
    error_count: Int,
    is_connected: Bool,
  )
}

/// Messages for WebSocket Actor
pub type WebSocketMessage {
  Connect
  Reconnect
  Poll
  Disconnect
  Ping
  Stop
}

// ============================================================
// FFI Bindings
// ============================================================

@external(erlang, "vibee_ws_client_ffi", "connect")
fn ws_connect_ffi(
  host: String,
  port: Int,
  path: String,
) -> Result(WsConnection, Dynamic)

@external(erlang, "vibee_ws_client_ffi", "connect_tls")
fn ws_connect_tls_ffi(
  host: String,
  port: Int,
  path: String,
  headers: List(#(String, String)),
) -> Result(WsConnection, Dynamic)

@external(erlang, "vibee_ws_client_ffi", "receive_message")
fn ws_receive_ffi(conn: WsConnection, timeout: Int) -> Result(String, Dynamic)

@external(erlang, "vibee_ws_client_ffi", "close")
fn ws_close_ffi(conn: WsConnection) -> Dynamic

@external(erlang, "vibee_ws_client_ffi", "send_ping")
fn ws_send_ping_ffi(conn: WsConnection) -> Dynamic

@external(erlang, "vibee_vibe_logger_ffi", "get_iso_timestamp")
fn get_iso_timestamp() -> String

@external(erlang, "vibee_ffi", "get_unix_timestamp")
fn get_timestamp() -> Int

@external(erlang, "vibee_polling_ffi", "get_api_key")
fn get_api_key() -> String

// ============================================================
// Public API
// ============================================================

/// Start WebSocket actor
pub fn start(
  config: telegram_agent.TelegramAgentConfig,
) -> Result(Subject(WebSocketMessage), actor.StartError) {
  let initial_state = init_state(config)

  let spec =
    actor.new(initial_state)
    |> actor.on_message(handle_message)

  case actor.start(spec) {
    Ok(started) -> {
      agent_registry.update_status(initial_state.agent_id, agent_registry.Running)
      vibe_logger.info(initial_state.logger, "WebSocket actor started")
      Ok(started.data)
    }
    Error(err) -> {
      agent_registry.update_status(
        initial_state.agent_id,
        agent_registry.Failed("Failed to start"),
      )
      Error(err)
    }
  }
}

/// Start WebSocket actor with shared event bus
pub fn start_with_events(
  config: telegram_agent.TelegramAgentConfig,
  bus: Subject(event_bus.PubSubMessage),
) -> Result(Subject(WebSocketMessage), actor.StartError) {
  let initial_state = init_state_with_events(config, bus)

  let spec =
    actor.new(initial_state)
    |> actor.on_message(handle_message)

  case actor.start(spec) {
    Ok(started) -> {
      agent_registry.update_status(initial_state.agent_id, agent_registry.Running)
      vibe_logger.info(initial_state.logger, "WebSocket actor started with event bus")
      Ok(started.data)
    }
    Error(err) -> {
      agent_registry.update_status(
        initial_state.agent_id,
        agent_registry.Failed("Failed to start"),
      )
      Error(err)
    }
  }
}

/// Initiate WebSocket connection
pub fn connect(subject: Subject(WebSocketMessage)) {
  process.send(subject, Connect)
}

/// Stop the actor
pub fn stop(subject: Subject(WebSocketMessage)) {
  process.send(subject, Stop)
}

// ============================================================
// Internal Functions
// ============================================================

fn init_state(config: telegram_agent.TelegramAgentConfig) -> WebSocketState {
  let agent_id = "websocket_" <> config.session_id
  let logger =
    vibe_logger.new("websocket")
    |> vibe_logger.with_session_id(config.session_id)
    |> vibe_logger.with_data("agent_id", json.string(agent_id))

  // Initialize agent registry
  agent_registry.init()
  let timestamp = get_iso_timestamp()
  agent_registry.register(agent_registry.AgentInfo(
    id: agent_id,
    agent_type: agent_registry.WebSocketAgent,
    status: agent_registry.Starting,
    started_at: timestamp,
    last_activity: timestamp,
    message_count: 0,
    error_count: 0,
    session_id: Some(config.session_id),
    extra: [],
  ))

  vibe_logger.info(logger, "WebSocket actor initializing")

  WebSocketState(
    config: config,
    agent_state: telegram_agent.init(config),
    connection: None,
    event_bus: None,
    seen_ids: set.new(),
    agent_id: agent_id,
    logger: logger,
    reconnect_attempts: 0,
    message_count: 0,
    error_count: 0,
    is_connected: False,
  )
}

fn init_state_with_events(
  config: telegram_agent.TelegramAgentConfig,
  bus: Subject(event_bus.PubSubMessage),
) -> WebSocketState {
  let state = init_state(config)
  WebSocketState(..state, event_bus: Some(bus))
}

/// Main message handler
fn handle_message(
  state: WebSocketState,
  msg: WebSocketMessage,
) -> actor.Next(WebSocketState, WebSocketMessage) {
  case msg {
    Connect -> handle_connect(state)
    Reconnect -> handle_reconnect(state)
    Poll -> handle_poll(state)
    Ping -> handle_ping(state)
    Disconnect -> handle_disconnect(state)
    Stop -> handle_stop(state)
  }
}

/// Handle connection request
fn handle_connect(state: WebSocketState) -> actor.Next(WebSocketState, WebSocketMessage) {
  let log = state.logger

  // Parse bridge URL to get host/port/path
  let #(use_tls, host, port) = parse_bridge_url(state.config.bridge_url)
  let path = "/api/v1/updates?session_id=" <> state.config.session_id
  let api_key = get_api_key()

  vibe_logger.info(
    log
      |> vibe_logger.with_data("host", json.string(host))
      |> vibe_logger.with_data("port", json.int(port))
      |> vibe_logger.with_data("tls", json.bool(use_tls)),
    "Connecting to WebSocket",
  )

  // Connect based on TLS requirement
  let connect_result = case use_tls {
    True -> {
      let headers = [
        #("Authorization", "Bearer " <> api_key),
        #("X-Session-ID", state.config.session_id),
      ]
      ws_connect_tls_ffi(host, port, path, headers)
    }
    False -> ws_connect_ffi(host, port, path)
  }

  case connect_result {
    Ok(conn) -> {
      vibe_logger.info(log, "WebSocket connected successfully")

      // Publish connected event
      publish_event(
        state.event_bus,
        event_bus.system_event("websocket", "connected", get_timestamp()),
      )

      // Schedule first poll
      let self = process.new_subject()
      process.send(self, Poll)

      // Schedule keepalive ping
      schedule_ping()

      actor.continue(WebSocketState(
        ..state,
        connection: Some(conn),
        is_connected: True,
        reconnect_attempts: 0,
      ))
    }
    Error(err) -> {
      vibe_logger.error(
        log |> vibe_logger.with_data("error", json.string(format_error(err))),
        "WebSocket connection failed",
      )

      // Schedule reconnect with exponential backoff
      let delay = calculate_backoff(state.reconnect_attempts)
      let self = process.new_subject()
      process.send_after(self, delay, Reconnect)

      actor.continue(WebSocketState(
        ..state,
        reconnect_attempts: state.reconnect_attempts + 1,
        error_count: state.error_count + 1,
      ))
    }
  }
}

/// Handle reconnection (after disconnect or error)
fn handle_reconnect(state: WebSocketState) -> actor.Next(WebSocketState, WebSocketMessage) {
  vibe_logger.info(
    state.logger
      |> vibe_logger.with_data("attempt", json.int(state.reconnect_attempts + 1)),
    "Attempting WebSocket reconnection",
  )

  // Close any existing connection
  case state.connection {
    Some(conn) -> {
      let _ = ws_close_ffi(conn)
      Nil
    }
    None -> Nil
  }

  handle_connect(WebSocketState(..state, connection: None, is_connected: False))
}

/// Handle poll (receive messages from WebSocket)
fn handle_poll(state: WebSocketState) -> actor.Next(WebSocketState, WebSocketMessage) {
  case state.connection, state.is_connected {
    Some(conn), True -> {
      // Try to receive message with short timeout (non-blocking)
      case ws_receive_ffi(conn, 100) {
        Ok(data) -> {
          // Parse and process the event
          let event = ws_events.parse(data)
          let new_state = process_event(state, event)

          // Continue polling
          let self = process.new_subject()
          process.send(self, Poll)

          actor.continue(new_state)
        }
        Error(err) -> {
          // Check if it's just a timeout (normal) or real error
          case is_timeout_error(err) {
            True -> {
              // Normal timeout, continue polling
              let self = process.new_subject()
              process.send(self, Poll)
              actor.continue(state)
            }
            False -> {
              // Real error - connection lost
              vibe_logger.warn(
                state.logger
                  |> vibe_logger.with_data("error", json.string(format_error(err))),
                "WebSocket error, will reconnect",
              )

              // Schedule reconnect
              let delay = calculate_backoff(state.reconnect_attempts)
              let self = process.new_subject()
              process.send_after(self, delay, Reconnect)

              actor.continue(WebSocketState(
                ..state,
                connection: None,
                is_connected: False,
                error_count: state.error_count + 1,
              ))
            }
          }
        }
      }
    }
    _, _ -> {
      // Not connected, try to connect
      handle_connect(state)
    }
  }
}

/// Handle ping (keepalive)
fn handle_ping(state: WebSocketState) -> actor.Next(WebSocketState, WebSocketMessage) {
  case state.connection, state.is_connected {
    Some(conn), True -> {
      let _ = ws_send_ping_ffi(conn)
      schedule_ping()
      actor.continue(state)
    }
    _, _ -> {
      // Not connected, skip ping
      actor.continue(state)
    }
  }
}

/// Handle disconnect request
fn handle_disconnect(state: WebSocketState) -> actor.Next(WebSocketState, WebSocketMessage) {
  case state.connection {
    Some(conn) -> {
      let _ = ws_close_ffi(conn)
      vibe_logger.info(state.logger, "WebSocket disconnected")
      Nil
    }
    None -> Nil
  }

  actor.continue(WebSocketState(..state, connection: None, is_connected: False))
}

/// Handle stop request
fn handle_stop(state: WebSocketState) -> actor.Next(WebSocketState, WebSocketMessage) {
  case state.connection {
    Some(conn) -> {
      let _ = ws_close_ffi(conn)
      Nil
    }
    None -> Nil
  }

  agent_registry.update_status(state.agent_id, agent_registry.Stopped)
  agent_registry.unregister(state.agent_id)
  vibe_logger.info(state.logger, "WebSocket actor stopped")

  actor.stop()
}

/// Process a parsed Telegram event
fn process_event(state: WebSocketState, event: TelegramEvent) -> WebSocketState {
  let log = state.logger

  case event {
    ws_events.NewMessage(chat_id, msg_id, from_id, from_name, text, timestamp, is_outgoing) -> {
      let unique_id = int.to_string(chat_id) <> ":" <> int.to_string(msg_id)

      // Skip outgoing messages
      case is_outgoing {
        True -> state
        False -> {
          // Check if already seen
          case set.contains(state.seen_ids, unique_id) {
            True -> state
            False -> {
              // New incoming message
              vibe_logger.info(
                log
                  |> vibe_logger.with_data("chat_id", json.int(chat_id))
                  |> vibe_logger.with_data("msg_id", json.int(msg_id))
                  |> vibe_logger.with_data("from_name", json.string(from_name)),
                "New incoming message",
              )
              vibe_logger.debug(
                log
                  |> vibe_logger.with_data(
                    "text_preview",
                    json.string(string.slice(text, 0, 100)),
                  ),
                "Message content",
              )

              // Update registry
              agent_registry.update_activity(state.agent_id)
              agent_registry.increment_messages(state.agent_id)

              // Publish event
              publish_event(
                state.event_bus,
                event_bus.telegram_message(
                  int.to_string(chat_id),
                  msg_id,
                  from_name,
                  text,
                  get_timestamp(),
                ),
              )

              // Process through telegram_agent
              // Note: reply_to_id = 0 for WebSocket events (not yet supported)
              let new_agent_state =
                telegram_agent.handle_incoming_message(
                  state.agent_state,
                  int.to_string(chat_id),
                  from_id,
                  from_name,
                  text,
                  msg_id,
                  0,  // reply_to_id not supported in WS yet
                )

              // Save to database for RAG
              case postgres.insert_message_simple(chat_id, msg_id, from_id, from_name, text) {
                Ok(_) -> vibe_logger.trace(log, "Message saved to DB")
                Error(e) -> {
                  vibe_logger.error(
                    log |> vibe_logger.with_data("error", json.string(e)),
                    "Failed to save message to DB",
                  )
                  agent_registry.increment_errors(state.agent_id)
                }
              }

              WebSocketState(
                ..state,
                agent_state: new_agent_state,
                seen_ids: set.insert(state.seen_ids, unique_id),
                message_count: state.message_count + 1,
              )
            }
          }
        }
      }
    }

    ws_events.EditMessage(chat_id, msg_id, text, _) -> {
      vibe_logger.debug(
        log
          |> vibe_logger.with_data("chat_id", json.int(chat_id))
          |> vibe_logger.with_data("msg_id", json.int(msg_id)),
        "Message edited",
      )
      state
    }

    ws_events.DeleteMessage(chat_id, msg_id, _) -> {
      vibe_logger.debug(
        log
          |> vibe_logger.with_data("chat_id", json.int(chat_id))
          |> vibe_logger.with_data("msg_id", json.int(msg_id)),
        "Message deleted",
      )
      state
    }

    ws_events.Ping -> state
    ws_events.Pong -> state
    ws_events.Connected -> {
      vibe_logger.info(log, "Bridge confirmed connection")
      state
    }
    ws_events.Disconnected(reason) -> {
      vibe_logger.warn(
        log |> vibe_logger.with_data("reason", json.string(reason)),
        "Bridge disconnected",
      )
      state
    }
    ws_events.Unknown(raw) -> {
      vibe_logger.trace(
        log |> vibe_logger.with_data("raw", json.string(string.slice(raw, 0, 200))),
        "Unknown event",
      )
      state
    }
  }
}

/// Parse bridge URL to components
fn parse_bridge_url(url: String) -> #(Bool, String, Int) {
  case string.starts_with(url, "https://") {
    True -> {
      let host =
        string.drop_start(url, 8)
        |> string.split("/")
        |> list.first
        |> result.unwrap("localhost")
      #(True, host, 443)
    }
    False -> {
      case string.starts_with(url, "http://") {
        True -> {
          let rest =
            string.drop_start(url, 7)
            |> string.split("/")
            |> list.first
            |> result.unwrap("localhost:8081")
          case string.split(rest, ":") {
            [h, p] -> {
              let port = int.parse(p) |> result.unwrap(80)
              #(False, h, port)
            }
            _ -> #(False, rest, 80)
          }
        }
        False -> #(False, "localhost", 8081)
      }
    }
  }
}

/// Calculate exponential backoff delay
fn calculate_backoff(attempts: Int) -> Int {
  // Base: 1 second, max: 30 seconds
  let base = 1000
  let max = 30_000
  let delay = base * power_of_2(attempts)
  case delay > max {
    True -> max
    False -> delay
  }
}

fn power_of_2(n: Int) -> Int {
  case n <= 0 {
    True -> 1
    False -> 2 * power_of_2(n - 1)
  }
}

/// Check if error is just a timeout
fn is_timeout_error(err: Dynamic) -> Bool {
  let err_str = format_error(err)
  string.contains(err_str, "timeout")
}

/// Format error for logging
fn format_error(_err: Dynamic) -> String {
  // Dynamic errors are opaque - just return a generic message
  "connection error"
}

/// Schedule ping for keepalive (every 30 seconds)
fn schedule_ping() {
  let self = process.new_subject()
  process.send_after(self, 30_000, Ping)
}

/// Publish event to bus if available
fn publish_event(
  bus: Option(Subject(event_bus.PubSubMessage)),
  event: event_bus.Event,
) {
  case bus {
    Some(b) -> event_bus.publish(b, event)
    None -> Nil
  }
}
