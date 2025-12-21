// VIBEE - Agent Framework on Gleam/BEAM
// Entry point for the application
// Checks VIBEE_MODE env var: "mcp" for MCP WebSocket Server, otherwise Telegram agent
// Build version: 2025-12-13-v16 (Payment integration: Robokassa, TON, Telegram Stars)

import gleam/io
import gleam/erlang/process
import gleam/option.{None, Some}
import gleam/string
import vibee/api/router
import vibee/agent/polling_actor
import vibee/agent/websocket_actor
import vibee/config/telegram_config
import vibee/events/event_bus
import vibee/health
import vibee/shutdown
import vibee/telegram/telegram_agent.{TelegramAgentConfig}
import vibee/mcp/config
import vibee/mcp/tools.{init_registry}
import vibee/mcp/events
import vibee/mcp/cache
import vibee/mcp/telemetry
import vibee/mcp/session_manager
import vibee/onboarding/state as onboarding_state
import vibee/log_aggregator
import vibee/db/postgres
import vibee/api/e2e_test_runner
import gleam/int
import gleam/list

// Payment store initialization
@external(erlang, "vibee_payment_ffi", "init")
fn init_payment_store() -> Nil

// P2P Escrow store initialization
@external(erlang, "vibee_p2p_ffi", "init")
fn init_p2p_store() -> Nil

// Earning FFI initialization
@external(erlang, "vibee_earning_ffi", "init")
fn init_earning_store() -> Nil

// Earning worker startup
@external(erlang, "vibee_earning_worker", "start_link")
fn start_earning_worker() -> Result(process.Pid, String)

pub fn main() {
  run(Nil)
}

// Entry function for erlang-shipment entrypoint.sh
pub fn run(_arg) {
  // Check if we should run MCP server
  let mode = config.get_env("VIBEE_MODE")
  case mode {
    "mcp" -> run_mcp_server()
    _ -> run_telegram_agent()
  }
}

fn run_telegram_agent() {
  io.println("VIBEE Agent Framework v0.1.0")
  io.println("============================")

  // Start the global event bus first
  io.println("[EVENTS] Starting global event bus...")
  case event_bus.start() {
    Ok(event_bus_subject) -> {
      io.println("[EVENTS] ✓ Event bus started")

      // Конфигурация агента с OpenRouter API (используем централизованный конфиг)
      // Initialize session manager ETS table
      session_manager.init()
      io.println("[SESSION] Session manager initialized")

      // Register sessions from ENV
      // SECURITY: Phone numbers and usernames come from ENV, not hardcoded
      let session_1 = config.get_env("TELEGRAM_SESSION_ID")
      let session_1_phone = config.get_env("TELEGRAM_SESSION_1_PHONE")
      let session_1_username = config.get_env("TELEGRAM_SESSION_1_USERNAME")

      let session_2 = config.get_env("TELEGRAM_SESSION_ID_2")
      let session_2_phone = config.get_env("TELEGRAM_SESSION_2_PHONE")
      let session_2_username = config.get_env("TELEGRAM_SESSION_2_USERNAME")

      // Register session 1
      case session_1 {
        "" -> Nil
        sid -> {
          session_manager.upsert(session_manager.SessionInfo(
            session_id: sid,
            phone: case session_1_phone { "" -> None _ -> Some(session_1_phone) },
            username: case session_1_username { "" -> None _ -> Some(session_1_username) },
            authorized: True,
            created_at: 0,
          ))
          io.println("[SESSION] Registered session 1: " <> mask_sensitive(sid))
        }
      }

      // Register session 2
      case session_2 {
        "" -> Nil
        sid -> {
          session_manager.upsert(session_manager.SessionInfo(
            session_id: sid,
            phone: case session_2_phone { "" -> None _ -> Some(session_2_phone) },
            username: case session_2_username { "" -> None _ -> Some(session_2_username) },
            authorized: True,
            created_at: 0,
          ))
          io.println("[SESSION] Registered session 2: " <> mask_sensitive(sid))
        }
      }

      // Set active session (prefer session_1)
      let session_id = case session_1 {
        "" -> case session_2 {
          "" -> case session_manager.get_active() {
            Some(sid) -> sid
            None -> ""
          }
          sid -> { session_manager.set_active(sid) sid }
        }
        sid -> { session_manager.set_active(sid) sid }
      }
      io.println("[CONFIG] Active Session ID: " <> session_id)
      io.println("[CONFIG] Bridge URL: " <> telegram_config.bridge_url())

      // Get LLM API key from env (optional - fallback response if not set)
      let llm_api_key = case config.get_env("OPENROUTER_API_KEY") {
        "" -> {
          io.println("[CONFIG] No OPENROUTER_API_KEY - using fallback replies")
          None
        }
        key -> {
          io.println("[CONFIG] OPENROUTER_API_KEY set")
          Some(key)
        }
      }

      // Start agents for configured sessions only (session_1 and session_2 from ENV)
      // Check for WebSocket mode feature flag
      let use_websocket = config.get_env("VIBEE_USE_WEBSOCKET") == "true"
      io.println("[AGENT] Starting VIBEE Telegram Agents...")
      io.println("[AGENT] Mode: " <> case use_websocket { True -> "WebSocket (real-time)" False -> "HTTP Polling" })

      // Build list of sessions from ENV
      let sessions_to_poll = case session_1 {
        "" -> case session_2 {
          "" -> []
          _ -> [session_2]
        }
        _ -> case session_2 {
          "" -> [session_1]
          _ -> [session_1, session_2]
        }
      }
      io.println("[AGENT] Will connect " <> int_to_string(list.length(sessions_to_poll)) <> " configured sessions")

      // Track first polling agent for shutdown handler and count successful agents
      let #(first_agent_subject, agents_started) = list.fold(sessions_to_poll, #(None, 0), fn(acc, sid) {
        let #(first_subject, count) = acc
        let agent_config = TelegramAgentConfig(
          bridge_url: telegram_config.bridge_url(),
          session_id: sid,
          llm_api_key: llm_api_key,
          llm_model: "x-ai/grok-4.1-fast",
          auto_reply_enabled: True,
          cooldown_ms: 30_000,
          // Digital Twin enabled only for primary session
          digital_twin_enabled: sid == session_id,
          owner_id: 144_022_504,
        )

        case use_websocket {
          True -> {
            // WebSocket mode - real-time updates
            case websocket_actor.start_with_events(agent_config, event_bus_subject) {
              Ok(ws_subject) -> {
                websocket_actor.connect(ws_subject)
                io.println("[AGENT] ✓ WebSocket started for session: " <> mask_sensitive(sid))
                #(first_subject, count + 1)
              }
              Error(_) -> {
                io.println("[AGENT] ! Failed to start WebSocket for session: " <> mask_sensitive(sid))
                acc
              }
            }
          }
          False -> {
            // Polling mode - HTTP polling
            case polling_actor.start_with_events(agent_config, event_bus_subject) {
              Ok(agent_subject) -> {
                polling_actor.start_polling(agent_subject)
                io.println("[AGENT] ✓ Polling started for session: " <> mask_sensitive(sid))
                case first_subject {
                  None -> #(Some(agent_subject), count + 1)
                  _ -> #(first_subject, count + 1)
                }
              }
              Error(_) -> {
                io.println("[AGENT] ! Failed to start polling for session: " <> mask_sensitive(sid))
                acc
              }
            }
          }
        }
      })

      // Check if any agents were started
      case agents_started > 0 {
        True -> {
          io.println("[AGENT] ✓ Started " <> int_to_string(agents_started) <> " agents")

          // Setup graceful shutdown for first polling agent (if any)
          case first_agent_subject {
            Some(agent_subject) -> {
              shutdown.setup_handler(agent_subject)
              io.println("[SHUTDOWN] ✓ Graceful shutdown handler registered")
            }
            None -> io.println("[SHUTDOWN] WebSocket mode - no shutdown handler needed")
          }

          // Initialize stores
          init_payment_store()
          init_p2p_store()
          init_earning_store()
          io.println("[STORES] ✓ Payment, P2P, Earning stores initialized")

          // Initialize database pool (for RAG parsing)
          case config.get_env("DATABASE_URL") {
            "" -> io.println("[DB] DATABASE_URL not set - RAG parsing disabled")
            db_url -> {
              case postgres.connect(db_url) {
                Ok(pool) -> {
                  postgres.set_global_pool(pool)
                  io.println("[DB] ✓ PostgreSQL pool initialized and cached")
                }
                Error(_) -> io.println("[DB] ! Failed to connect to PostgreSQL")
              }
            }
          }

          // Start global log aggregator (for WebSocket log streaming)
          case log_aggregator.start_global() {
            Ok(_) -> io.println("[LOGS] ✓ Log aggregator started")
            Error(_) -> io.println("[LOGS] ! Log aggregator failed to start")
          }

          // Start Earning Worker (background arbitrage scanner)
          case start_earning_worker() {
            Ok(_) -> io.println("[EARNING] ✓ Earning worker started (scans every 2 min)")
            Error(_) -> io.println("[EARNING] ! Worker start failed, continuing without it")
          }

          // Initialize E2E ETS table (owned by main process for persistence)
          e2e_test_runner.init()
          io.println("[E2E] ✓ ETS table initialized (vibee_e2e_runs)")

          // Start HTTP API server with Web UI and shared event bus
          let port = 8080
          case router.start_with_events(port, event_bus_subject) {
            Ok(_) -> {
              io.println("[OK] HTTP API started on port " <> int_to_string(port))
              io.println("")
              io.println("VIBEE is ready!")
              io.println("  Dashboard: http://localhost:" <> int_to_string(port))
              io.println("  Events:    http://localhost:" <> int_to_string(port) <> "/events")
              io.println("  API:       http://localhost:" <> int_to_string(port) <> "/api/v1")
              io.println("  Health:    http://localhost:" <> int_to_string(port) <> "/health")
              io.println("")
              io.println("Target chats: VIBEE AGENT (-1002737186844) + личные чаты")
              io.println("Triggers: vibee, vibe, @vibee, бот, агент")
              io.println("")

              // Keep the main process alive
              process.sleep_forever()
            }
            Error(msg) -> {
              io.println("[ERROR] Failed to start HTTP API: " <> msg)
              Nil
            }
          }
        }
        False -> {
          io.println("[ERROR] No agents started - check session configuration")
          Nil
        }
      }
    }
    Error(_) -> {
      io.println("[ERROR] Failed to start Event Bus")
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

/// SECURITY: Mask sensitive data for logging
/// Shows first 8 chars + "****" to allow debugging without exposing full secrets
fn mask_sensitive(value: String) -> String {
  case string.length(value) > 8 {
    True -> string.slice(value, 0, 8) <> "****"
    False -> "****"
  }
}

// MCP WebSocket Server mode
fn run_mcp_server() {
  io.println("VIBEE MCP WebSocket Server")
  io.println("==========================")

  // Initialize modules
  io.println("[INIT] Initializing modules...")
  events.init()
  cache.init()
  telemetry.init()
  onboarding_state.init_store()
  init_payment_store()
  init_p2p_store()
  init_earning_store()

  // Initialize database pool (for RAG parsing)
  case config.get_env("DATABASE_URL") {
    "" -> io.println("[DB] DATABASE_URL not set - RAG parsing disabled")
    db_url -> {
      case postgres.connect(db_url) {
        Ok(pool) -> {
          postgres.set_global_pool(pool)
          io.println("[DB] ✓ PostgreSQL pool initialized and cached")
        }
        Error(_) -> io.println("[DB] ! Failed to connect to PostgreSQL")
      }
    }
  }

  // Start global log aggregator (for WebSocket log streaming)
  case log_aggregator.start_global() {
    Ok(_) -> io.println("[LOGS] ✓ Log aggregator started")
    Error(_) -> io.println("[LOGS] ! Log aggregator failed to start")
  }

  // Start Earning Worker (background arbitrage scanner + alerts)
  case start_earning_worker() {
    Ok(_) -> io.println("[EARNING] ✓ Earning worker started (scans every 2 min, alerts to Telegram)")
    Error(_) -> io.println("[EARNING] ! Worker start failed")
  }

  // Initialize session manager for multi-account support
  session_manager.init()

  // Register sessions from ENV
  // SECURITY: Phone numbers and usernames come from ENV, not hardcoded
  let session_1 = config.get_env("TELEGRAM_SESSION_ID")
  let session_1_phone = config.get_env("TELEGRAM_SESSION_1_PHONE")
  let session_1_username = config.get_env("TELEGRAM_SESSION_1_USERNAME")

  let session_2 = config.get_env("TELEGRAM_SESSION_ID_2")
  let session_2_phone = config.get_env("TELEGRAM_SESSION_2_PHONE")
  let session_2_username = config.get_env("TELEGRAM_SESSION_2_USERNAME")

  case session_1 {
    "" -> Nil
    sid -> {
      session_manager.upsert(session_manager.SessionInfo(
        session_id: sid,
        phone: case session_1_phone { "" -> None _ -> Some(session_1_phone) },
        username: case session_1_username { "" -> None _ -> Some(session_1_username) },
        authorized: True,
        created_at: 0,
      ))
      io.println("[SESSION] Registered session 1: " <> mask_sensitive(sid))
    }
  }

  case session_2 {
    "" -> Nil
    sid -> {
      session_manager.upsert(session_manager.SessionInfo(
        session_id: sid,
        phone: case session_2_phone { "" -> None _ -> Some(session_2_phone) },
        username: case session_2_username { "" -> None _ -> Some(session_2_username) },
        authorized: True,
        created_at: 0,
      ))
      io.println("[SESSION] Registered session 2: " <> mask_sensitive(sid))
    }
  }

  io.println("[INIT] ✓ Modules initialized (including onboarding, payments, P2P, sessions)")

  // Create tool registry
  io.println("[MCP] Creating tool registry...")
  let registry = init_registry()
  io.println("[MCP] ✓ Tool registry created")

  // Start event bus
  io.println("[EVENTS] Starting event bus...")
  case event_bus.start() {
    Ok(bus) -> {
      io.println("[EVENTS] ✓ Event bus started")

      // === Start agents for configured sessions only ===
      let use_websocket = config.get_env("VIBEE_USE_WEBSOCKET") == "true"
      io.println("[AGENT] Mode: " <> case use_websocket { True -> "WebSocket (real-time)" False -> "HTTP Polling" })

      let session_1 = config.get_env("TELEGRAM_SESSION_ID")
      let session_2 = config.get_env("TELEGRAM_SESSION_ID_2")
      let primary_session_id = case session_1 {
        "" -> session_2
        sid -> sid
      }

      let llm_api_key = case config.get_env("OPENROUTER_API_KEY") {
        "" -> None
        key -> Some(key)
      }

      // Build list of sessions from ENV only
      let sessions_to_poll = case session_1 {
        "" -> case session_2 {
          "" -> []
          _ -> [session_2]
        }
        _ -> case session_2 {
          "" -> [session_1]
          _ -> [session_1, session_2]
        }
      }
      io.println("[AGENT] Will connect " <> int.to_string(list.length(sessions_to_poll)) <> " configured sessions")

      list.each(sessions_to_poll, fn(sid) {
        let agent_config = TelegramAgentConfig(
          bridge_url: telegram_config.bridge_url(),
          session_id: sid,
          llm_api_key: llm_api_key,
          llm_model: "x-ai/grok-4.1-fast",
          auto_reply_enabled: True,
          cooldown_ms: 30_000,
          // Digital Twin enabled only for primary session
          digital_twin_enabled: sid == primary_session_id,
          owner_id: 144_022_504,
        )

        case use_websocket {
          True -> {
            // WebSocket mode - real-time updates
            case websocket_actor.start_with_events(agent_config, bus) {
              Ok(ws_subject) -> {
                websocket_actor.connect(ws_subject)
                io.println("[AGENT] ✓ WebSocket started for: " <> mask_sensitive(sid))
              }
              Error(_) -> io.println("[AGENT] ! WebSocket failed for: " <> mask_sensitive(sid))
            }
          }
          False -> {
            // Polling mode - HTTP polling
            case polling_actor.start_with_events(agent_config, bus) {
              Ok(agent_subject) -> {
                polling_actor.start_polling(agent_subject)
                io.println("[AGENT] ✓ Polling started for: " <> mask_sensitive(sid))
              }
              Error(_) -> io.println("[AGENT] ! Polling failed for: " <> mask_sensitive(sid))
            }
          }
        }
      })
      // === End multi-session agents ===

      // Read port from ENV (for Fly.io) or use default
      let port = case config.get_env("PORT") {
        "" -> 8080
        port_str -> case int.parse(port_str) {
          Ok(p) -> p
          Error(_) -> 8080
        }
      }
      // Initialize E2E ETS table (owned by main process for persistence)
      e2e_test_runner.init()
      io.println("[E2E] ✓ ETS table initialized (vibee_e2e_runs)")

      io.println("[HTTP] Starting server on port " <> int.to_string(port) <> "...")

      case router.start_with_mcp(port, bus, registry) {
        Ok(_) -> {
          io.println("")
          io.println("✅ MCP WebSocket Server Ready!")
          io.println("")
          io.println("  WebSocket: ws://0.0.0.0:" <> int.to_string(port) <> "/ws/mcp")
          io.println("  Health:    http://0.0.0.0:" <> int.to_string(port) <> "/health")
          io.println("  Dashboard: http://0.0.0.0:" <> int.to_string(port))
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
