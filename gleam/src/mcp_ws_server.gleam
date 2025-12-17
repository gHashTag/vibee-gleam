// MCP WebSocket Server Entry Point
// Run with: gleam run -m mcp_ws_server
// Or: erl -pa build/erlang/vibee/_gleam_artefacts/*.beam -noshell -eval 'mcp_ws_server:main(), timer:sleep(infinity)'

import gleam/io
import gleam/erlang/process
import gleam/int
import vibee/api/router
import vibee/events/event_bus
import vibee/mcp/tools.{init_registry}
import vibee/mcp/events
import vibee/mcp/cache
import vibee/mcp/telemetry
import vibee/mcp/config

pub fn main() {
  io.println("VIBEE MCP WebSocket Server")
  io.println("==========================")

  // Initialize modules
  io.println("[INIT] Initializing modules...")
  events.init()
  cache.init()
  telemetry.init()
  io.println("[INIT] ✓ Modules initialized")

  // Create tool registry
  io.println("[MCP] Creating tool registry...")
  let registry = init_registry()
  io.println("[MCP] ✓ Tool registry created")

  // Start event bus
  io.println("[EVENTS] Starting event bus...")
  case event_bus.start() {
    Ok(bus) -> {
      io.println("[EVENTS] ✓ Event bus started")

      // Start HTTP server with MCP WebSocket
      // Read port from ENV (for Fly.io) or use default
      let port = case config.get_env("PORT") {
        "" -> 8082
        port_str -> case int.parse(port_str) {
          Ok(p) -> p
          Error(_) -> 8082
        }
      }
      io.println("[HTTP] Starting server on port " <> int_to_string(port) <> "...")

      case router.start_with_mcp(port, bus, registry) {
        Ok(_) -> {
          io.println("")
          io.println("✅ MCP WebSocket Server Ready!")
          io.println("")
          io.println("  WebSocket: ws://localhost:" <> int_to_string(port) <> "/ws/mcp")
          io.println("  Health:    http://localhost:" <> int_to_string(port) <> "/health")
          io.println("  Dashboard: http://localhost:" <> int_to_string(port))
          io.println("")
          io.println("Test with:")
          io.println("  echo '{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}' | websocat ws://localhost:" <> int_to_string(port) <> "/ws/mcp")
          io.println("")

          // Keep alive
          process.sleep_forever()
        }
        Error(msg) -> {
          io.println("[ERROR] Failed to start server: " <> msg)
          Nil
        }
      }
    }
    Error(_) -> {
      io.println("[ERROR] Failed to start event bus")
      Nil
    }
  }
}

fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ -> do_int_to_string(n, "")
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        9 -> "9"
        _ -> "?"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}
