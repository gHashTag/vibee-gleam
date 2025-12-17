// WebSocket Client for Telegram Bridge
// Connects to Go bridge via WebSocket and receives updates

import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import vibee/logging

// ============================================================
// Types
// ============================================================

/// WebSocket connection handle (opaque)
pub type WsConnection

/// WebSocket client configuration
pub type WsConfig {
  WsConfig(
    host: String,
    port: Int,
    path: String,
    session_id: String,
    reconnect_delay_ms: Int,
  )
}

/// Messages received from WebSocket
pub type WsMessage {
  WsTextMessage(String)
  WsConnected
  WsDisconnected(reason: String)
  WsError(String)
}

/// Internal actor messages
pub type WsActorMessage {
  // Control
  Connect
  Disconnect
  // Internal
  Poll
  // Query
  GetStatus(reply_to: Subject(WsStatus))
}

/// WebSocket client status
pub type WsStatus {
  WsStatus(connected: Bool, messages_received: Int, errors: Int)
}

/// Actor state
pub type WsState {
  WsState(
    config: WsConfig,
    connection: Option(WsConnection),
    handler: fn(WsMessage) -> Nil,
    messages_received: Int,
    errors: Int,
    is_running: Bool,
  )
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

@external(erlang, "vibee_ws_client_ffi", "send")
fn ws_send_ffi(conn: WsConnection, text: String) -> Dynamic

@external(erlang, "vibee_ws_client_ffi", "close")
fn ws_close_ffi(conn: WsConnection) -> Dynamic

@external(erlang, "vibee_ws_client_ffi", "receive_message")
fn ws_receive_ffi(conn: WsConnection, timeout: Int) -> Result(String, Dynamic)

// ============================================================
// Public API
// ============================================================

/// Create a new WebSocket client configuration
pub fn new_config(
  host: String,
  port: Int,
  session_id: String,
) -> WsConfig {
  WsConfig(
    host: host,
    port: port,
    path: "/api/v1/updates?session_id=" <> session_id,
    session_id: session_id,
    reconnect_delay_ms: 5000,
  )
}

/// Start WebSocket client actor
pub fn start(
  config: WsConfig,
  handler: fn(WsMessage) -> Nil,
) -> Result(Subject(WsActorMessage), actor.StartError) {
  let initial_state =
    WsState(
      config: config,
      connection: None,
      handler: handler,
      messages_received: 0,
      errors: 0,
      is_running: False,
    )

  let spec =
    actor.new(initial_state)
    |> actor.on_message(handle_message)

  case actor.start(spec) {
    Ok(started) -> Ok(started.data)
    Error(err) -> Error(err)
  }
}

/// Connect to WebSocket server
pub fn connect(client: Subject(WsActorMessage)) -> Nil {
  process.send(client, Connect)
}

/// Disconnect from WebSocket server
pub fn disconnect(client: Subject(WsActorMessage)) -> Nil {
  process.send(client, Disconnect)
}

/// Get client status
pub fn get_status(client: Subject(WsActorMessage)) -> WsStatus {
  let reply_subject = process.new_subject()
  process.send(client, GetStatus(reply_subject))
  case process.receive(reply_subject, 5000) {
    Ok(status) -> status
    Error(_) -> WsStatus(connected: False, messages_received: 0, errors: 0)
  }
}

/// Send a message through WebSocket
pub fn send(conn: WsConnection, text: String) -> Nil {
  let _ = ws_send_ffi(conn, text)
  Nil
}

// ============================================================
// Actor Implementation
// ============================================================

fn handle_message(
  state: WsState,
  message: WsActorMessage,
) -> actor.Next(WsState, WsActorMessage) {
  case message {
    Connect -> {
      logging.info("[WS] Connecting to " <> state.config.host <> ":" <> int.to_string(state.config.port))

      case ws_connect_ffi(state.config.host, state.config.port, state.config.path) {
        Ok(conn) -> {
          logging.info("[WS] Connected successfully")
          state.handler(WsConnected)

          // Start polling for messages
          let self = process.new_subject()
          process.send(self, Poll)

          actor.continue(WsState(
            ..state,
            connection: Some(conn),
            is_running: True,
          ))
        }
        Error(_) -> {
          logging.error("[WS] Connection failed, retrying...")
          state.handler(WsError("Connection failed"))

          // Retry after delay
          let self = process.new_subject()
          process.send_after(self, state.config.reconnect_delay_ms, Connect)

          actor.continue(WsState(..state, errors: state.errors + 1))
        }
      }
    }

    Disconnect -> {
      case state.connection {
        Some(conn) -> {
          let _ = ws_close_ffi(conn)
          Nil
        }
        None -> Nil
      }
      state.handler(WsDisconnected("Manual disconnect"))
      actor.continue(WsState(
        ..state,
        connection: None,
        is_running: False,
      ))
    }

    Poll -> {
      case state.is_running, state.connection {
        True, Some(conn) -> {
          // Try to receive message with 100ms timeout (non-blocking)
          case ws_receive_ffi(conn, 100) {
            Ok(data) -> {
              // Got a message, handle it
              logging.debug("[WS] Received: " <> string.slice(data, 0, 100))
              state.handler(WsTextMessage(data))

              // Continue polling
              let self = process.new_subject()
              process.send(self, Poll)

              actor.continue(WsState(
                ..state,
                messages_received: state.messages_received + 1,
              ))
            }
            Error(_) -> {
              // Timeout or error, continue polling
              let self = process.new_subject()
              process.send(self, Poll)
              actor.continue(state)
            }
          }
        }
        _, _ -> {
          // Not running or no connection, stop polling
          actor.continue(state)
        }
      }
    }

    GetStatus(reply_to) -> {
      let status = WsStatus(
        connected: option.is_some(state.connection) && state.is_running,
        messages_received: state.messages_received,
        errors: state.errors,
      )
      process.send(reply_to, status)
      actor.continue(state)
    }
  }
}
