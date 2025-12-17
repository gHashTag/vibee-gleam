// HTTP API Router for VIBEE
// Built on Mist HTTP server

import gleam/bytes_tree
import gleam/erlang/process
import gleam/http
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri
import mist.{type Connection, type ResponseData}
import simplifile
import vibee/config/telegram_config
import vibee/events/event_bus
import vibee/mcp/config
import vibee/integrations/telegram/client as tg_client
import vibee/integrations/telegram/types as tg_types
import vibee/logging
import vibee/web/html
import vibee/web/p2p_panel
import vibee/web/factory_panel
import vibee/mcp/websocket as mcp_ws
import vibee/mcp/tools.{type ToolRegistry}
import vibee/mcp/session_manager
import vibee/api/p2p_ws
import vibee/api/p2p_handlers
import vibee/api/invoice_handlers
import vibee/api/task_handlers
import vibee/api/webhook_handlers
import vibee/auth/web_auth
import vibee/log_aggregator
import vibee/telegram/parser
import vibee/db/postgres

/// WebSocket message types
pub type WsMessage {
  Broadcast(String)
}

/// API context passed to handlers
pub type Context {
  Context(
    // Add database connection, config etc here
  )
}

/// Telegram bridge configuration (теперь в telegram_config)
// Используем централизованный конфиг: telegram_config.bridge_url, telegram_config.session_id

/// Global shared event bus reference (set by start_with_events)
/// This is a module-level mutable reference pattern using process dictionary
/// In production, use an ETS table or registry

/// Start the HTTP server
pub fn start(port: Int) -> Result(Nil, String) {
  logging.info("🚀 Starting VIBEE HTTP server on port " <> int.to_string(port))

  let handler = fn(req: Request(Connection)) -> Response(ResponseData) {
    handle_request(req, None, None)
  }

  case mist.new(handler)
    |> mist.port(port)
    |> mist.start()
  {
    Ok(_) -> {
      logging.info("✅ HTTP server started successfully")
      Ok(Nil)
    }
    Error(_) -> {
      logging.error("❌ Failed to start HTTP server")
      Error("Failed to start server")
    }
  }
}

/// Start the HTTP server with shared event bus
pub fn start_with_events(
  port: Int,
  bus: process.Subject(event_bus.PubSubMessage),
) -> Result(Nil, String) {
  logging.info("🚀 Starting VIBEE HTTP server on port " <> int.to_string(port) <> " with event bus")

  let handler = fn(req: Request(Connection)) -> Response(ResponseData) {
    handle_request(req, Some(bus), None)
  }

  case mist.new(handler)
    |> mist.port(port)
    |> mist.bind("0.0.0.0")
    |> mist.start()
  {
    Ok(_) -> {
      logging.info("✅ HTTP server started successfully with event bus")
      Ok(Nil)
    }
    Error(_) -> {
      logging.error("❌ Failed to start HTTP server")
      Error("Failed to start server")
    }
  }
}

/// Start the HTTP server with MCP WebSocket support
pub fn start_with_mcp(
  port: Int,
  bus: process.Subject(event_bus.PubSubMessage),
  registry: ToolRegistry,
) -> Result(Nil, String) {
  logging.info("🚀 Starting VIBEE HTTP server on port " <> int.to_string(port) <> " with MCP WebSocket")

  let handler = fn(req: Request(Connection)) -> Response(ResponseData) {
    handle_request(req, Some(bus), Some(registry))
  }

  case mist.new(handler)
    |> mist.port(port)
    |> mist.bind("0.0.0.0")
    |> mist.start()
  {
    Ok(_) -> {
      logging.info("✅ HTTP server started with MCP WebSocket at /ws/mcp")
      Ok(Nil)
    }
    Error(_) -> {
      logging.error("❌ Failed to start HTTP server")
      Error("Failed to start server")
    }
  }
}

/// Main request handler
fn handle_request(
  req: Request(Connection),
  bus: option.Option(process.Subject(event_bus.PubSubMessage)),
  mcp_registry: option.Option(ToolRegistry),
) -> Response(ResponseData) {
  let path = request.path_segments(req)

  case req.method, path {
    // MCP WebSocket endpoint
    http.Get, ["ws", "mcp"] -> {
      case mcp_registry {
        Some(registry) -> mcp_ws.handler(req, registry)
        None -> {
          response.new(503)
          |> response.set_body(mist.Bytes(bytes_tree.from_string("MCP not configured")))
        }
      }
    }

    // Web UI - root path serves HTML dashboard
    http.Get, [] -> dashboard_handler()

    // Lustre app - full Telegram message viewer
    http.Get, ["app"] -> lustre_app_handler()

    // Health check
    http.Get, ["health"] -> health_handler()

    // Ready check
    http.Get, ["ready"] -> ready_handler()

    // Agent endpoints
    http.Get, ["api", "v1", "agents"] -> list_agents_handler()
    http.Post, ["api", "v1", "agents"] -> create_agent_handler(req)
    http.Get, ["api", "v1", "agents", id] -> get_agent_handler(id)
    http.Delete, ["api", "v1", "agents", id] -> delete_agent_handler(id)

    // Message endpoint
    http.Post, ["api", "v1", "agents", id, "message"] -> send_message_handler(req, id)

    // Messages API
    http.Get, ["api", "v1", "messages"] -> list_messages_handler()

    // Telegram API endpoints
    http.Get, ["api", "v1", "telegram", "dialogs"] -> telegram_dialogs_handler()
    http.Get, ["api", "v1", "telegram", "history", chat_id] -> telegram_history_handler(chat_id)
    http.Get, ["api", "v1", "telegram", "all-messages"] -> telegram_all_messages_handler()

    // Logs page - real-time log viewer
    http.Get, ["logs"] -> logs_page_handler()
    http.Get, ["api", "v1", "logs", "tail"] -> logs_tail_handler()

    // Events page - real-time event stream (PubSub pattern)
    http.Get, ["events"] -> events_page_handler()

    // WebSocket endpoint for real-time updates
    http.Get, ["ws"] -> websocket_handler(req)
    http.Get, ["ws", "logs"] -> logs_websocket_handler(req)
    http.Get, ["ws", "events"] -> events_websocket_handler_with_bus(req, bus)

    // Payment webhooks
    http.Post, ["api", "robokassa-result"] -> robokassa_webhook_handler(req)
    http.Post, ["api", "payment-success"] -> robokassa_webhook_handler(req)

    // AI Service Webhooks (Replicate, Hedra, HeyGen, Kling, KIE.ai, BFL, ElevenLabs)
    http.Post, ["api", "webhooks", service] -> webhook_handlers.handle_webhook(req, service)

    // P2P Control Panel UI (Earning Dashboard)
    http.Get, ["p2p"] -> p2p_panel_handler()
    http.Get, ["earning"] -> p2p_panel_handler()  // Alias route

    // ==========================================================================
    // Template Factory UI - Vibe Reels Variants Gallery
    // ==========================================================================
    http.Get, ["factory"] -> factory_panel_handler()
    http.Get, ["api", "v1", "factory", "variants"] -> factory_variants_handler(req)

    // P2P WebSocket for real-time updates
    http.Get, ["ws", "p2p"] -> p2p_ws.handler(req, bus)

    // P2P API endpoints
    http.Get, ["api", "v1", "p2p", "status"] -> p2p_handlers.status_handler()
    http.Get, ["api", "v1", "p2p", "stats"] -> p2p_handlers.stats_handler()
    http.Post, ["api", "v1", "p2p", "start"] -> p2p_handlers.start_handler(req)
    http.Post, ["api", "v1", "p2p", "stop"] -> p2p_handlers.stop_handler()
    http.Post, ["api", "v1", "p2p", "config"] -> p2p_handlers.config_handler(req)
    http.Get, ["api", "v1", "p2p", "orders"] -> p2p_handlers.orders_list_handler()
    http.Post, ["api", "v1", "p2p", "orders", order_id, "cancel"] -> p2p_handlers.order_cancel_handler(order_id)
    http.Post, ["api", "v1", "p2p", "order", "accept"] -> p2p_handlers.order_accept_handler(req)
    http.Get, ["api", "v1", "p2p", "arbitrage"] -> {
      let query = request.get_query(req) |> result.unwrap([])
      let crypto = list.find(query, fn(pair) { pair.0 == "crypto" })
        |> result.map(fn(pair) { Some(pair.1) })
        |> result.unwrap(None)
      let fiat = list.find(query, fn(pair) { pair.0 == "fiat" })
        |> result.map(fn(pair) { Some(pair.1) })
        |> result.unwrap(None)
      p2p_handlers.arbitrage_scan_handler(crypto, fiat)
    }
    http.Post, ["api", "v1", "p2p", "arbitrage", "execute"] -> p2p_handlers.arbitrage_execute_handler(req)
    http.Get, ["api", "v1", "p2p", "market-maker", "rates"] -> p2p_handlers.market_maker_rates_handler()

    // Maker Bot API
    http.Get, ["api", "v1", "p2p", "maker", "prices"] -> {
      let query = request.get_query(req) |> result.unwrap([])
      let crypto = list.find(query, fn(pair) { pair.0 == "crypto" })
        |> result.map(fn(pair) { Some(pair.1) })
        |> result.unwrap(None)
      let fiat = list.find(query, fn(pair) { pair.0 == "fiat" })
        |> result.map(fn(pair) { Some(pair.1) })
        |> result.unwrap(None)
      p2p_handlers.maker_prices_handler(crypto, fiat)
    }
    http.Post, ["api", "v1", "p2p", "maker", "toggle"] -> {
      let query = request.get_query(req) |> result.unwrap([])
      let telegram_id = list.find(query, fn(pair) { pair.0 == "telegram_id" })
        |> result.map(fn(pair) { Some(pair.1) })
        |> result.unwrap(None)
      let enabled = list.find(query, fn(pair) { pair.0 == "enabled" })
        |> result.map(fn(pair) { Some(pair.1) })
        |> result.unwrap(None)
      p2p_handlers.maker_toggle_handler(telegram_id, enabled)
    }

    // Alerts API
    http.Get, ["api", "v1", "p2p", "alerts", "scan"] -> {
      let query = request.get_query(req) |> result.unwrap([])
      let crypto = list.find(query, fn(pair) { pair.0 == "crypto" })
        |> result.map(fn(pair) { Some(pair.1) })
        |> result.unwrap(None)
      let fiat = list.find(query, fn(pair) { pair.0 == "fiat" })
        |> result.map(fn(pair) { Some(pair.1) })
        |> result.unwrap(None)
      let min_spread = list.find(query, fn(pair) { pair.0 == "min_spread" })
        |> result.map(fn(pair) { Some(pair.1) })
        |> result.unwrap(None)
      p2p_handlers.alerts_scan_handler(crypto, fiat, min_spread)
    }
    http.Get, ["api", "v1", "p2p", "alerts", "monitor"] -> p2p_handlers.alerts_monitor_handler()

    // Executor API
    http.Get, ["api", "v1", "p2p", "executor", "status"] -> p2p_handlers.executor_status_handler()
    http.Post, ["api", "v1", "p2p", "executor", "simulate"] -> {
      let query = request.get_query(req) |> result.unwrap([])
      let crypto = list.find(query, fn(pair) { pair.0 == "crypto" })
        |> result.map(fn(pair) { Some(pair.1) })
        |> result.unwrap(None)
      let fiat = list.find(query, fn(pair) { pair.0 == "fiat" })
        |> result.map(fn(pair) { Some(pair.1) })
        |> result.unwrap(None)
      p2p_handlers.executor_simulate_handler(crypto, fiat)
    }

    // Activity Log API
    http.Get, ["api", "v1", "p2p", "activity"] -> {
      let query = request.get_query(req) |> result.unwrap([])
      let limit = list.find(query, fn(pair) { pair.0 == "limit" })
        |> result.map(fn(pair) { Some(pair.1) })
        |> result.unwrap(None)
      p2p_handlers.activity_log_handler(limit)
    }

    // ==========================================================================
    // Web Authentication (Phone + OTP for /earning page)
    // ==========================================================================
    http.Post, ["api", "v1", "auth", "web", "send-code"] -> web_auth.send_code_handler(req)
    http.Post, ["api", "v1", "auth", "web", "verify-code"] -> web_auth.verify_code_handler(req)
    http.Get, ["api", "v1", "auth", "web", "me"] -> web_auth.me_handler(req)
    http.Post, ["api", "v1", "auth", "web", "logout"] -> web_auth.logout_handler(req)
    http.Post, ["api", "v1", "auth", "web", "wallet"] -> web_auth.save_wallet_handler(req)
    http.Delete, ["api", "v1", "auth", "web", "wallet"] -> web_auth.delete_wallet_handler(req)

    // TON Connect manifest
    http.Get, ["tonconnect-manifest.json"] -> web_auth.tonconnect_manifest_handler(req)

    // Invoice API endpoints (xRocket, CryptoBot - send invoices to users)
    http.Post, ["api", "v1", "invoice", "create"] -> invoice_handlers.create_handler(req)
    http.Get, ["api", "v1", "invoice", "create"] -> {
      let amount = get_query_param(req, "amount")
      let currency = get_query_param(req, "currency")
      let description = get_query_param(req, "description")
      let provider = get_query_param(req, "provider")
      invoice_handlers.create_get_handler(amount, currency, description, provider)
    }
    http.Get, ["api", "v1", "invoice", "cheque"] -> {
      let amount = get_query_param(req, "amount")
      let currency = get_query_param(req, "currency")
      let users = get_query_param(req, "users")
      let description = get_query_param(req, "description")
      invoice_handlers.cheque_handler(amount, currency, users, description)
    }
    http.Get, ["api", "v1", "invoice", "send"] -> {
      let telegram_id = get_query_param(req, "telegram_id")
      let amount = get_query_param(req, "amount")
      let currency = get_query_param(req, "currency")
      let comment = get_query_param(req, "comment")
      let provider = get_query_param(req, "provider")
      invoice_handlers.send_handler(telegram_id, amount, currency, comment, provider)
    }

    // ==========================================================================
    // TaskFlow UI - Task Management System
    // ==========================================================================

    http.Get, ["tasks"] -> task_handlers.dashboard_handler(req)
    http.Get, ["tasks", "list"] -> task_handlers.list_handler(req)
    http.Get, ["tasks", "new"] -> task_handlers.create_form_handler(req)
    http.Get, ["tasks", id] -> task_handlers.detail_handler(req, id)
    http.Get, ["api", "v1", "tasks"] -> task_handlers.list_api_handler(req)
    http.Post, ["api", "v1", "tasks"] -> task_handlers.create_api_handler(req)
    http.Get, ["api", "v1", "tasks", "stats"] -> task_handlers.stats_api_handler(req)
    http.Get, ["api", "v1", "tasks", "today"] -> task_handlers.today_api_handler(req)
    http.Get, ["api", "v1", "tasks", "overdue"] -> task_handlers.overdue_api_handler(req)
    http.Get, ["api", "v1", "tasks", "list-partial"] -> task_handlers.list_partial_handler(req)
    http.Get, ["api", "v1", "tasks", id] -> task_handlers.get_api_handler(req, id)
    http.Patch, ["api", "v1", "tasks", id] -> task_handlers.update_api_handler(req, id)
    http.Patch, ["api", "v1", "tasks", id, "status"] -> task_handlers.update_status_api_handler(req, id)
    http.Post, ["api", "v1", "tasks", id, "comments"] -> task_handlers.add_comment_api_handler(req, id)
    http.Delete, ["api", "v1", "tasks", id] -> task_handlers.archive_api_handler(req, id)

    // ==========================================================================
    // RAG Parser API - Parse all Telegram dialogs for Digital Twin memory
    // ==========================================================================
    http.Post, ["api", "parse", "all"] -> handle_parse_all(req)
    http.Post, ["api", "parse", chat_id] -> handle_parse_chat(req, chat_id)
    http.Get, ["api", "parse", "status"] -> handle_parse_status(req)

    // 404 for unknown routes
    _, _ -> not_found_handler()
  }
}

// Handlers

fn dashboard_handler() -> Response(ResponseData) {
  let body = html.index_page()
  html_response(200, body)
}

fn lustre_app_handler() -> Response(ResponseData) {
  let body = html.lustre_app_page()
  html_response(200, body)
}

fn p2p_panel_handler() -> Response(ResponseData) {
  let body = p2p_panel.render()
  html_response(200, body)
}

fn factory_panel_handler() -> Response(ResponseData) {
  let body = factory_panel.render()
  html_response(200, body)
}

/// Factory variants API - returns generated variants with stats
fn factory_variants_handler(req: Request(Connection)) -> Response(ResponseData) {
  // Parse limit from query params
  let limit = case get_query_param(req, "limit") {
    Some(l) -> case int.parse(l) {
      Ok(n) -> n
      Error(_) -> 100
    }
    None -> 100
  }

  // Generate demo variants (in production, load from DB or generate via Remotion)
  let variants = generate_demo_variants(limit)
  let total = list.length(variants)
  let avg_priority = case total {
    0 -> 0
    _ -> {
      let sum = list.fold(variants, 0, fn(acc, v) {
        let #(_, p, _) = v
        acc + p
      })
      sum / total
    }
  }

  let variants_json = json.array(variants, fn(v) {
    let #(id, priority, axes) = v
    let #(avatar, hook, caption, broll) = axes
    json.object([
      #("id", json.string(id)),
      #("priority", json.int(priority)),
      #("axes", json.object([
        #("avatarPosition", json.string(avatar)),
        #("hookStyle", json.string(hook)),
        #("captionStyle", json.string(caption)),
        #("bRollPattern", json.string(broll)),
      ])),
    ])
  })

  let body = json.object([
    #("variants", variants_json),
    #("total", json.int(total)),
    #("stats", json.object([
      #("avgPriority", json.int(avg_priority)),
      #("possibleCombinations", json.int(1568)),
      #("axes", json.object([
        #("avatarPosition", json.int(8)),
        #("hookStyle", json.int(7)),
        #("captionStyle", json.int(7)),
        #("bRollPattern", json.int(4)),
      ])),
    ])),
  ])

  json_response(200, body)
}

/// Generate demo variants for testing
fn generate_demo_variants(limit: Int) -> List(#(String, Int, #(String, String, String, String))) {
  let avatar_positions = [
    #("circle-bottom-left", "cbl", 30),
    #("circle-bottom-right", "cbr", 25),
    #("fullscreen", "fs", 20),
    #("side-left", "sl", 10),
    #("side-right", "sr", 10),
    #("floating-center", "fc", 5),
  ]

  let hook_styles = [
    #("zoom-impact", "zi", 35),
    #("slide-reveal", "sr", 25),
    #("typewriter", "tw", 15),
    #("pulse", "pu", 10),
    #("question", "qu", 8),
    #("glitch", "gl", 5),
    #("none", "no", 2),
  ]

  let caption_styles = [
    #("karaoke", "k", 30),
    #("bounce", "b", 25),
    #("modern", "m", 20),
    #("typewriter", "tw", 10),
    #("word-highlight", "wh", 8),
    #("classic", "c", 5),
    #("none", "no", 2),
  ]

  let broll_patterns = [
    #("hook-content-cta", "hcc", 40),
    #("even-distribution", "ed", 30),
    #("progressive", "pr", 20),
    #("random-weighted", "rw", 10),
  ]

  // Generate cartesian product with priority weighting
  let all_variants = list.flat_map(avatar_positions, fn(ap) {
    let #(avatar_name, avatar_abbr, avatar_weight) = ap
    list.flat_map(hook_styles, fn(hs) {
      let #(hook_name, hook_abbr, hook_weight) = hs
      list.flat_map(caption_styles, fn(cs) {
        let #(caption_name, caption_abbr, caption_weight) = cs
        list.map(broll_patterns, fn(bp) {
          let #(broll_name, broll_abbr, broll_weight) = bp
          let id = "TH_" <> avatar_abbr <> "_" <> hook_abbr <> "_" <> caption_abbr <> "_" <> broll_abbr
          let priority = { avatar_weight + hook_weight + caption_weight + broll_weight } / 4
          #(id, priority, #(avatar_name, hook_name, caption_name, broll_name))
        })
      })
    })
  })

  // Sort by priority descending and take limit
  let sorted = list.sort(all_variants, fn(a, b) {
    let #(_, p1, _) = a
    let #(_, p2, _) = b
    int.compare(p2, p1)
  })

  list.take(sorted, limit)
}

fn health_handler() -> Response(ResponseData) {
  let body = json.object([
    #("status", json.string("ok")),
    #("service", json.string("vibee")),
    #("version", json.string("0.1.0")),
  ])

  json_response(200, body)
}

fn ready_handler() -> Response(ResponseData) {
  // TODO: Check database connection
  let body = json.object([
    #("status", json.string("ready")),
    #("agents", json.int(0)),
  ])

  json_response(200, body)
}

fn list_agents_handler() -> Response(ResponseData) {
  // TODO: Load from database
  let body = json.object([
    #("agents", json.array([], json.object)),
  ])

  json_response(200, body)
}

fn create_agent_handler(_req: Request(Connection)) -> Response(ResponseData) {
  // TODO: Parse body and create agent
  let body = json.object([
    #("id", json.string("agent-1")),
    #("status", json.string("created")),
    #("message", json.string("Agent created successfully")),
  ])

  json_response(201, body)
}

fn get_agent_handler(id: String) -> Response(ResponseData) {
  // TODO: Load from database
  let body = json.object([
    #("id", json.string(id)),
    #("name", json.string("Test Agent")),
    #("status", json.string("running")),
    #("tone", json.string("friendly")),
    #("language", json.string("en")),
  ])

  json_response(200, body)
}

fn delete_agent_handler(id: String) -> Response(ResponseData) {
  // TODO: Delete from database
  let body = json.object([
    #("id", json.string(id)),
    #("status", json.string("deleted")),
  ])

  json_response(200, body)
}

fn send_message_handler(_req: Request(Connection), id: String) -> Response(ResponseData) {
  // TODO: Send message to agent actor
  let body = json.object([
    #("agent_id", json.string(id)),
    #("status", json.string("processing")),
    #("message", json.string("Message sent to agent")),
  ])

  json_response(202, body)
}

fn list_messages_handler() -> Response(ResponseData) {
  // Sample messages for demo
  let messages = json.array([
    json.object([
      #("id", json.string("msg-1")),
      #("group_id", json.string("group-1")),
      #("group_name", json.string("Development Team")),
      #("sender_name", json.string("Alice")),
      #("text", json.string("Just deployed the new VIBEE update!")),
      #("timestamp", json.int(1733650000)),
    ]),
    json.object([
      #("id", json.string("msg-2")),
      #("group_id", json.string("group-2")),
      #("group_name", json.string("AI Research")),
      #("sender_name", json.string("Bob")),
      #("text", json.string("The new Claude model is impressive")),
      #("timestamp", json.int(1733649500)),
    ]),
    json.object([
      #("id", json.string("msg-3")),
      #("group_id", json.string("group-1")),
      #("group_name", json.string("Development Team")),
      #("sender_name", json.string("Charlie")),
      #("text", json.string("Great work on the Gleam integration")),
      #("timestamp", json.int(1733649000)),
    ]),
  ], fn(x) { x })

  let body = json.object([
    #("messages", messages),
    #("total", json.int(3)),
  ])

  json_response(200, body)
}

fn not_found_handler() -> Response(ResponseData) {
  let body = json.object([
    #("error", json.string("Not found")),
  ])

  json_response(404, body)
}

// Telegram handlers

fn get_telegram_bridge() -> tg_client.TelegramBridge {
  // Get active session from session manager, fall back to empty string if none
  let session_id = case session_manager.get_active() {
    Some(sid) -> sid
    None -> ""
  }
  tg_client.with_session(telegram_config.bridge_url(), session_id)
}

fn telegram_dialogs_handler() -> Response(ResponseData) {
  // For now return hardcoded real data from Telegram
  // TODO: Fix HTTP client integration
  let dialogs_json = json.array([
    json.object([
      #("id", json.int(-1_002_737_186_844)),
      #("title", json.string("Agent Vibe")),
      #("type", json.string("supergroup")),
      #("unread_count", json.int(0)),
    ]),
    json.object([
      #("id", json.int(-1_002_643_951_085)),
      #("title", json.string("AiStars ОФИС")),
      #("type", json.string("supergroup")),
      #("unread_count", json.int(2)),
    ]),
    json.object([
      #("id", json.int(-1_001_978_334_539)),
      #("title", json.string("НейроКодер - Вайб-кодинг")),
      #("type", json.string("supergroup")),
      #("unread_count", json.int(0)),
    ]),
    json.object([
      #("id", json.int(-1_001_165_767_969)),
      #("title", json.string("Биржа IT I Удаленка/Офис")),
      #("type", json.string("supergroup")),
      #("unread_count", json.int(2478)),
    ]),
    json.object([
      #("id", json.int(-1_001_728_766_323)),
      #("title", json.string("Паттайя - Объявления/Барахолка")),
      #("type", json.string("supergroup")),
      #("unread_count", json.int(854)),
    ]),
  ], fn(x) { x })

  json_response(200, json.object([
    #("dialogs", dialogs_json),
    #("total", json.int(5)),
    #("source", json.string("telegram")),
  ]))
}

fn telegram_history_handler(chat_id_str: String) -> Response(ResponseData) {
  // Return real messages from НейроКодер - Вайб-кодинг group
  // TODO: Fix HTTP client integration
  let _chat_id = chat_id_str

  let messages_json = json.array([
    json.object([
      #("id", json.int(5583)),
      #("text", json.string("ElevenLabs API временно недоступен (Cloudflare защита)")),
      #("from_id", json.int(7_655_182_164)),
      #("from_name", json.string("NeuroBlogger")),
      #("date", json.string("2025-09-18T21:23:50+07:00")),
    ]),
    json.object([
      #("id", json.int(5579)),
      #("text", json.string("🎨 НОВАЯ ГЕНЕРАЦИЯ - @keity8 сгенерировал изображение")),
      #("from_id", json.int(7_655_182_164)),
      #("from_name", json.string("NeuroBlogger")),
      #("date", json.string("2025-09-02T15:12:37+07:00")),
    ]),
  ], fn(x) { x })

  json_response(200, json.object([
    #("messages", messages_json),
    #("chat_id", json.string(chat_id_str)),
    #("total", json.int(2)),
    #("source", json.string("telegram")),
  ]))
}

fn telegram_all_messages_handler() -> Response(ResponseData) {
  // Return real messages from multiple Telegram groups
  // TODO: Fix HTTP client integration for live data

  let messages_json = json.array([
    json.object([
      #("id", json.int(5583)),
      #("group_name", json.string("НейроКодер - Вайб-кодинг")),
      #("sender_name", json.string("NeuroBlogger")),
      #("text", json.string("ElevenLabs API временно недоступен (Cloudflare защита)")),
      #("date", json.string("2025-09-18T21:23:50+07:00")),
    ]),
    json.object([
      #("id", json.int(5579)),
      #("group_name", json.string("НейроКодер - Вайб-кодинг")),
      #("sender_name", json.string("NeuroBlogger")),
      #("text", json.string("🎨 НОВАЯ ГЕНЕРАЦИЯ - @keity8 сгенерировал изображение")),
      #("date", json.string("2025-09-02T15:12:37+07:00")),
    ]),
    json.object([
      #("id", json.int(1001)),
      #("group_name", json.string("Agent Vibe")),
      #("sender_name", json.string("Dmitrii")),
      #("text", json.string("VIBEE Agent Framework запущен на Gleam/BEAM!")),
      #("date", json.string("2025-12-08T16:00:00+07:00")),
    ]),
    json.object([
      #("id", json.int(1002)),
      #("group_name", json.string("AiStars ОФИС")),
      #("sender_name", json.string("Team Lead")),
      #("text", json.string("Отличная работа с интеграцией MTProto через Go bridge")),
      #("date", json.string("2025-12-08T15:30:00+07:00")),
    ]),
  ], fn(x) { x })

  json_response(200, json.object([
    #("messages", messages_json),
    #("total", json.int(4)),
    #("source", json.string("telegram")),
  ]))
}

fn dialog_type_to_string(dt: tg_types.DialogType) -> String {
  case dt {
    tg_types.UserDialog -> "user"
    tg_types.GroupDialog -> "group"
    tg_types.SupergroupDialog -> "supergroup"
    tg_types.ChannelDialog -> "channel"
  }
}

fn telegram_error_to_string(err: tg_types.TelegramError) -> String {
  case err {
    tg_types.ConnectionError(msg) -> "Connection error: " <> msg
    tg_types.AuthError(msg) -> "Auth error: " <> msg
    tg_types.ApiError(code, msg) -> "API error " <> int.to_string(code) <> ": " <> msg
    tg_types.NetworkError(msg) -> "Network error: " <> msg
    tg_types.InvalidSession -> "Invalid session"
    tg_types.NotAuthorized -> "Not authorized"
  }
}

// Events page handler - real-time event stream
fn events_page_handler() -> Response(ResponseData) {
  let body = html.events_page()
  html_response(200, body)
}

/// WebSocket state for events
pub type EventWsState {
  EventWsState(
    event_bus: process.Subject(event_bus.PubSubMessage),
    client_subject: process.Subject(String),
  )
}

// Events WebSocket handler - simplified version (legacy - creates new bus)
fn events_websocket_handler(req: Request(Connection)) -> Response(ResponseData) {
  events_websocket_handler_with_bus(req, None)
}

// Events WebSocket handler with shared event bus
fn events_websocket_handler_with_bus(
  req: Request(Connection),
  shared_bus: option.Option(process.Subject(event_bus.PubSubMessage)),
) -> Response(ResponseData) {
  // Use shared bus if provided, otherwise create a new one
  let bus_result = case shared_bus {
    Some(bus) -> Ok(bus)
    None -> event_bus.start()
  }

  case bus_result {
    Ok(bus) -> {
      io.println("[WS:Events] Starting WebSocket handler with shared bus")

      mist.websocket(
        request: req,
        on_init: fn(_conn) {
          // IMPORTANT: Create client_subject HERE in WebSocket process!
          // Subject is bound to the process where it's created
          let client_subject = process.new_subject()
          event_bus.subscribe(bus, client_subject)

          io.println("[WS:Events] Client subscribed to event bus (in WS process)")

          // Create selector to receive events from PubSub and convert to WsMessage
          let selector = process.new_selector()
            |> process.select_map(for: client_subject, mapping: fn(json_str: String) {
              Broadcast(json_str)
            })

          let state = EventWsState(event_bus: bus, client_subject: client_subject)

          // Send welcome event
          let ts = get_unix_timestamp()
          event_bus.publish(bus, event_bus.system_event("connected", "Client connected to event stream", ts))

          #(state, Some(selector))
        },
        on_close: fn(st) {
          io.println("[WS:Events] Client disconnected")
          event_bus.unsubscribe(st.event_bus, st.client_subject)
        },
        handler: handle_event_ws_message,
      )
    }
    Error(_) -> {
      json_response(500, json.object([
        #("error", json.string("Failed to start event bus")),
      ]))
    }
  }
}

fn handle_event_ws_message(
  state: EventWsState,
  message: mist.WebsocketMessage(WsMessage),
  conn: mist.WebsocketConnection,
) {
  io.println("[WS:Handler] Received message type")
  case message {
    mist.Text("ping") -> {
      let assert Ok(_) = mist.send_text_frame(conn, "pong")
      mist.continue(state)
    }
    mist.Text("get_count") -> {
      // Return subscriber count
      let count = event_bus.get_subscriber_count(state.event_bus)
      let response = json.object([
        #("type", json.string("subscriber_count")),
        #("count", json.int(count)),
      ])
      let assert Ok(_) = mist.send_text_frame(conn, json.to_string(response))
      mist.continue(state)
    }
    mist.Text("test_event") -> {
      // Send a test event for demo purposes
      let ts = get_unix_timestamp()
      event_bus.publish(state.event_bus, event_bus.telegram_message(
        "12345",
        0,  // test msg_id
        "TestUser",
        "This is a test message from VIBEE!",
        ts,
      ))
      mist.continue(state)
    }
    mist.Text(_) | mist.Binary(_) -> {
      mist.continue(state)
    }
    mist.Custom(Broadcast(text)) -> {
      // Event received from PubSub - forward to WebSocket client
      io.println("[WS:Handler] Custom Broadcast received! Sending to client...")
      let assert Ok(_) = mist.send_text_frame(conn, text)
      mist.continue(state)
    }
    mist.Closed | mist.Shutdown -> {
      event_bus.unsubscribe(state.event_bus, state.client_subject)
      mist.stop()
    }
  }
}

// Helper to get current Unix timestamp
fn get_unix_timestamp() -> Int {
  // Simple timestamp - seconds since epoch (approximate)
  // In production, use erlang:system_time/1
  1733680000  // Base timestamp, will be replaced with real time
}

// WebSocket handler for real-time updates
fn websocket_handler(req: Request(Connection)) -> Response(ResponseData) {
  let selector = process.new_selector()
  let state = Nil

  mist.websocket(
    request: req,
    on_init: fn(_conn) { #(state, Some(selector)) },
    on_close: fn(_state) { io.println("[WS] Client disconnected") },
    handler: handle_ws_message,
  )
}

fn handle_ws_message(
  state: Nil,
  message: mist.WebsocketMessage(WsMessage),
  conn: mist.WebsocketConnection,
) {
  case message {
    mist.Text("ping") -> {
      let assert Ok(_) = mist.send_text_frame(conn, "pong")
      mist.continue(state)
    }
    mist.Text("get_messages") -> {
      // Send current messages as JSON
      let messages_json = get_messages_json()
      let assert Ok(_) = mist.send_text_frame(conn, messages_json)
      mist.continue(state)
    }
    mist.Text(_) | mist.Binary(_) -> {
      mist.continue(state)
    }
    mist.Custom(Broadcast(text)) -> {
      let assert Ok(_) = mist.send_text_frame(conn, text)
      mist.continue(state)
    }
    mist.Closed | mist.Shutdown -> mist.stop()
  }
}

fn get_messages_json() -> String {
  let messages = json.array([
    json.object([
      #("id", json.string("msg-1")),
      #("group_name", json.string("Development Team")),
      #("sender_name", json.string("Alice")),
      #("text", json.string("Just deployed the new VIBEE update!")),
      #("timestamp", json.int(1733650000)),
    ]),
    json.object([
      #("id", json.string("msg-2")),
      #("group_name", json.string("AI Research")),
      #("sender_name", json.string("Bob")),
      #("text", json.string("The new Claude model is impressive")),
      #("timestamp", json.int(1733649500)),
    ]),
    json.object([
      #("id", json.string("msg-3")),
      #("group_name", json.string("Development Team")),
      #("sender_name", json.string("Charlie")),
      #("text", json.string("Great work on the Gleam integration")),
      #("timestamp", json.int(1733649000)),
    ]),
  ], fn(x) { x })

  json.object([
    #("type", json.string("messages")),
    #("data", messages),
  ])
  |> json.to_string()
}

// Response helpers

/// Get allowed CORS origin from ENV
/// Default includes localhost for development and Fly.io domains for production
fn get_cors_origin(request_origin: String) -> String {
  let allowed_origins = config.get_env_or("CORS_ALLOWED_ORIGINS", "http://localhost:8080,http://localhost:3000,https://vibee-mcp.fly.dev,https://vibee-eliza-999-prod-v2.fly.dev,https://vibee-telegram-bridge.fly.dev")
  case allowed_origins == "*" {
    True -> "*"
    False -> {
      let origins = string.split(allowed_origins, ",")
      let trimmed = list.map(origins, string.trim)
      case list.contains(trimmed, request_origin) {
        True -> request_origin
        False -> ""  // Origin not allowed
      }
    }
  }
}

/// Add security headers to response (OWASP recommendations)
fn add_security_headers(resp: Response(ResponseData)) -> Response(ResponseData) {
  resp
  |> response.set_header("x-frame-options", "DENY")
  |> response.set_header("x-content-type-options", "nosniff")
  |> response.set_header("x-xss-protection", "1; mode=block")
  |> response.set_header("referrer-policy", "strict-origin-when-cross-origin")
  |> response.set_header("permissions-policy", "geolocation=(), microphone=(), camera=()")
}

/// Extract query parameter from request
fn get_query_param(req: Request(Connection), name: String) -> Option(String) {
  case request.get_query(req) {
    Ok(params) -> {
      case list.find(params, fn(p) { p.0 == name }) {
        Ok(found) -> Some(found.1)
        Error(_) -> None
      }
    }
    Error(_) -> None
  }
}

fn json_response(status: Int, body: json.Json) -> Response(ResponseData) {
  json_response_with_cors(status, body, "")
}

fn json_response_with_cors(status: Int, body: json.Json, request_origin: String) -> Response(ResponseData) {
  let body_string = json.to_string(body)
  let body_bytes = bytes_tree.from_string(body_string)
  let cors_origin = get_cors_origin(request_origin)

  let base_resp = response.new(status)
    |> response.set_header("content-type", "application/json")
    |> response.set_body(mist.Bytes(body_bytes))
    |> add_security_headers

  case cors_origin {
    "" -> base_resp
    origin -> base_resp
      |> response.set_header("access-control-allow-origin", origin)
      |> response.set_header("access-control-allow-credentials", "true")
  }
}

fn html_response(status: Int, body: String) -> Response(ResponseData) {
  let body_bytes = bytes_tree.from_string(body)

  response.new(status)
  |> response.set_header("content-type", "text/html; charset=utf-8")
  |> response.set_body(mist.Bytes(body_bytes))
}

// Log file path - VIBEE Gleam project logs
const log_file_path = "/Users/playra/vibee-eliza-999/vibee/gleam/logs/vibee.log"

// Logs page handler - real-time log viewer
fn logs_page_handler() -> Response(ResponseData) {
  let body = html.logs_page()
  html_response(200, body)
}

// Logs tail handler - returns last N lines
fn logs_tail_handler() -> Response(ResponseData) {
  // Read last 100 lines from log file
  case simplifile.read(log_file_path) {
    Ok(content) -> {
      let lines = string.split(content, "\n")
      let total = list.length(lines)
      let last_lines = list.drop(lines, int.max(0, total - 100))
      let body = json.object([
        #("lines", json.array(last_lines, json.string)),
        #("total", json.int(total)),
        #("file", json.string(log_file_path)),
      ])
      json_response(200, body)
    }
    Error(_) -> {
      let body = json.object([
        #("error", json.string("Cannot read log file")),
        #("file", json.string(log_file_path)),
      ])
      json_response(500, body)
    }
  }
}

// WebSocket handler for log streaming (now uses log_aggregator)
fn logs_websocket_handler(req: Request(Connection)) -> Response(ResponseData) {
  // Create a subject to receive log broadcasts
  let log_subject = process.new_subject()

  // Subscribe to log_aggregator if available
  case log_aggregator.get_global() {
    Some(aggregator) -> {
      log_aggregator.subscribe(aggregator, log_subject)
    }
    None -> Nil
  }

  // Create selector to receive log messages
  let selector = process.new_selector()
    |> process.select_map(for: log_subject, mapping: fn(log_json: String) {
      LogMessage(log_json)
    })

  let state = LogWsState(log_subject: log_subject)

  mist.websocket(
    request: req,
    on_init: fn(_conn) { #(state, Some(selector)) },
    on_close: fn(state) {
      // Unsubscribe on close
      case log_aggregator.get_global() {
        Some(aggregator) -> log_aggregator.unsubscribe(aggregator, state.log_subject)
        None -> Nil
      }
      io.println("[WS:Logs] Client disconnected")
    },
    handler: handle_log_ws_message,
  )
}

pub type LogWsState {
  LogWsState(log_subject: process.Subject(String))
}

pub type LogWsEvent {
  LogMessage(String)
}

fn handle_log_ws_message(
  state: LogWsState,
  message: mist.WebsocketMessage(LogWsEvent),
  conn: mist.WebsocketConnection,
) {
  case message {
    mist.Text("history") -> {
      // Send recent logs from aggregator
      case log_aggregator.get_global() {
        Some(aggregator) -> {
          let recent_logs = log_aggregator.get_recent(aggregator, 50)
          list.each(recent_logs, fn(entry) {
            let json_str = log_entry_to_json(entry)
            let _ = mist.send_text_frame(conn, json_str)
            Nil
          })
        }
        None -> {
          let _ = mist.send_text_frame(conn, "{\"error\": \"Log aggregator not initialized\"}")
          Nil
        }
      }
      mist.continue(state)
    }
    mist.Text("ping") -> {
      let _ = mist.send_text_frame(conn, "pong")
      mist.continue(state)
    }
    mist.Text(_) | mist.Binary(_) -> {
      mist.continue(state)
    }
    mist.Custom(LogMessage(log_json)) -> {
      // Forward log message to WebSocket client
      let _ = mist.send_text_frame(conn, log_json)
      mist.continue(state)
    }
    mist.Closed | mist.Shutdown -> mist.stop()
  }
}

// Helper to convert LogEntry to JSON for history
fn log_entry_to_json(entry: log_aggregator.LogEntry) -> String {
  let base = [
    #("timestamp", json.string(entry.timestamp)),
    #("level", json.string(entry.level)),
    #("logger", json.string(entry.logger)),
    #("message", json.string(entry.message)),
  ]

  let with_trace = case entry.trace_id {
    Some(id) -> [#("trace_id", json.string(id)), ..base]
    None -> base
  }

  let with_request = case entry.request_id {
    Some(id) -> [#("request_id", json.string(id)), ..with_trace]
    None -> with_trace
  }

  let with_session = case entry.session_id {
    Some(id) -> [#("session_id", json.string(id)), ..with_request]
    None -> with_request
  }

  let with_span = case entry.span_id {
    Some(id) -> [#("span_id", json.string(id)), ..with_session]
    None -> with_session
  }

  let with_tool = case entry.tool {
    Some(t) -> [#("tool", json.string(t)), ..with_span]
    None -> with_span
  }

  let all_fields = list.flatten([with_tool, entry.extra])

  json.object(all_fields)
  |> json.to_string
}

// =============================================================================
// PAYMENT WEBHOOK HANDLERS
// =============================================================================

/// Robokassa webhook handler (POST /api/robokassa-result or /api/payment-success)
fn robokassa_webhook_handler(_req: Request(Connection)) -> Response(ResponseData) {
  logging.info("[PAYMENT] Robokassa webhook received")

  // Parse form body (OutSum, InvId, SignatureValue)
  // For now, return OK response expected by Robokassa
  // In production: validate signature, update payment status, update balance

  // Robokassa expects "OK<inv_id>" response on success
  response.new(200)
  |> response.set_body(mist.Bytes(bytes_tree.from_string("OK")))
  |> response.set_header("content-type", "text/plain")
}

// =============================================================================
// RAG PARSER HANDLERS - Parse Telegram dialogs for Digital Twin memory
// =============================================================================

/// Parse all dialogs - POST /api/parse/all
fn handle_parse_all(_req: Request(Connection)) -> Response(ResponseData) {
  logging.info("[PARSER] Starting full parse of all dialogs")

  // Get active session
  let session_id = case session_manager.get_active() {
    Some(sid) -> sid
    None -> ""
  }

  case session_id {
    "" -> {
      let body = json.object([
        #("error", json.string("No active Telegram session")),
        #("hint", json.string("First authenticate via /api/v1/telegram/auth")),
      ])
      json_response(400, body)
    }
    sid -> {
      // Get DB pool
      case postgres.get_global_pool() {
        None -> {
          let body = json.object([
            #("error", json.string("Database not initialized")),
          ])
          json_response(500, body)
        }
        Some(pool) -> {
          // Create parse job
          case postgres.create_parse_job(pool, "full_sync", None) {
            Error(_) -> {
              let body = json.object([
                #("error", json.string("Failed to create parse job")),
              ])
              json_response(500, body)
            }
            Ok(job_id) -> {
              // Spawn async parse process
              let _ = spawn_full_parse(pool, sid, job_id)

              let body = json.object([
                #("success", json.bool(True)),
                #("job_id", json.int(job_id)),
                #("status", json.string("started")),
                #("message", json.string("Full parse started in background")),
              ])
              json_response(202, body)
            }
          }
        }
      }
    }
  }
}

/// Parse single chat - POST /api/parse/{chat_id}
fn handle_parse_chat(_req: Request(Connection), chat_id_str: String) -> Response(ResponseData) {
  logging.info("[PARSER] Starting parse for chat: " <> chat_id_str)

  case int.parse(chat_id_str) {
    Error(_) -> {
      let body = json.object([
        #("error", json.string("Invalid chat_id")),
      ])
      json_response(400, body)
    }
    Ok(chat_id) -> {
      // Get active session
      let session_id = case session_manager.get_active() {
        Some(sid) -> sid
        None -> ""
      }

      case session_id {
        "" -> {
          let body = json.object([
            #("error", json.string("No active Telegram session")),
          ])
          json_response(400, body)
        }
        sid -> {
          case postgres.get_global_pool() {
            None -> {
              let body = json.object([
                #("error", json.string("Database not initialized")),
              ])
              json_response(500, body)
            }
            Some(pool) -> {
              // Create parse job for single dialog
              case postgres.create_parse_job(pool, "dialog_sync", Some(chat_id)) {
                Error(_) -> {
                  let body = json.object([
                    #("error", json.string("Failed to create parse job")),
                  ])
                  json_response(500, body)
                }
                Ok(job_id) -> {
                  // Spawn async parse for single dialog
                  let _ = spawn_dialog_parse(pool, sid, chat_id, job_id)

                  let body = json.object([
                    #("success", json.bool(True)),
                    #("job_id", json.int(job_id)),
                    #("chat_id", json.int(chat_id)),
                    #("status", json.string("started")),
                  ])
                  json_response(202, body)
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Get parse status - GET /api/parse/status
fn handle_parse_status(_req: Request(Connection)) -> Response(ResponseData) {
  case postgres.get_global_pool() {
    None -> {
      let body = json.object([
        #("error", json.string("Database not initialized")),
      ])
      json_response(500, body)
    }
    Some(pool) -> {
      // Get stats from DB
      let stats = postgres.get_parse_stats(pool)

      let body = json.object([
        #("total_dialogs", json.int(stats.total_dialogs)),
        #("parsed_dialogs", json.int(stats.parsed_dialogs)),
        #("total_messages", json.int(stats.total_messages)),
        #("messages_with_embedding", json.int(stats.messages_with_embedding)),
        #("pending_jobs", json.int(stats.pending_jobs)),
        #("running_jobs", json.int(stats.running_jobs)),
      ])
      json_response(200, body)
    }
  }
}

/// Spawn full parse process (runs in background via Erlang spawn)
fn spawn_full_parse(pool: postgres.DbPool, session_id: String, job_id: Int) -> Nil {
  do_spawn_full_parse(pool, session_id, job_id)
}

@external(erlang, "vibee_parser_ffi", "spawn_full_parse")
fn do_spawn_full_parse(pool: postgres.DbPool, session_id: String, job_id: Int) -> Nil

/// Spawn single dialog parse (runs in background via Erlang spawn)
fn spawn_dialog_parse(pool: postgres.DbPool, session_id: String, dialog_id: Int, job_id: Int) -> Nil {
  do_spawn_dialog_parse(pool, session_id, dialog_id, job_id)
}

@external(erlang, "vibee_parser_ffi", "spawn_dialog_parse")
fn do_spawn_dialog_parse(pool: postgres.DbPool, session_id: String, dialog_id: Int, job_id: Int) -> Nil
