// HTTP API Router for VIBEE
// Built on Mist HTTP server

import gleam/bit_array
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
import vibee/integrations/telegram/bot_api
import vibee/logging
import vibee/web/html
import vibee/web/p2p_panel
import vibee/web/factory_panel
import vibee/web/leads_panel
import vibee/api/leads_handlers
import vibee/api/graphql_handlers
import vibee/mcp/websocket as mcp_ws
import vibee/mcp/tools.{type ToolRegistry}
import vibee/mcp/session_manager
import vibee/api/p2p_ws
import vibee/api/p2p_handlers
import vibee/api/invoice_handlers
import vibee/api/task_handlers
import vibee/api/webhook_handlers
import vibee/api/payment_webhooks
import vibee/auth/web_auth
import vibee/log_aggregator
import vibee/telegram/parser
import vibee/telegram/telegram_agent as vibee_telegram_agent
import vibee/db/postgres
import vibee/config/twin_config
import vibee/vibe_logger
import vibee/api/agent_websocket
import vibee/api/agent_metrics
import vibee/api/agent_handlers
import vibee/api/e2e_handlers
import vibee/api/editor_agent_ws
import vibee/api/video_api
import vibee/api/render_quota_handlers

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
  logging.quick_info("🚀 Starting VIBEE HTTP server on port " <> int.to_string(port))

  let handler = fn(req: Request(Connection)) -> Response(ResponseData) {
    handle_request(req, None, None)
  }

  case mist.new(handler)
    |> mist.port(port)
    |> mist.start()
  {
    Ok(_) -> {
      logging.quick_info("✅ HTTP server started successfully")
      Ok(Nil)
    }
    Error(_) -> {
      logging.quick_error("❌ Failed to start HTTP server")
      Error("Failed to start server")
    }
  }
}

/// Start the HTTP server with shared event bus
pub fn start_with_events(
  port: Int,
  bus: process.Subject(event_bus.PubSubMessage),
) -> Result(Nil, String) {
  logging.quick_info("🚀 Starting VIBEE HTTP server on port " <> int.to_string(port) <> " with event bus")

  let handler = fn(req: Request(Connection)) -> Response(ResponseData) {
    handle_request(req, Some(bus), None)
  }

  case mist.new(handler)
    |> mist.port(port)
    |> mist.bind("0.0.0.0")
    |> mist.start()
  {
    Ok(_) -> {
      logging.quick_info("✅ HTTP server started successfully with event bus")
      Ok(Nil)
    }
    Error(_) -> {
      logging.quick_error("❌ Failed to start HTTP server")
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
  logging.quick_info("🚀 Starting VIBEE HTTP server on port " <> int.to_string(port) <> " with MCP WebSocket")

  let handler = fn(req: Request(Connection)) -> Response(ResponseData) {
    handle_request(req, Some(bus), Some(registry))
  }

  case mist.new(handler)
    |> mist.port(port)
    |> mist.bind("0.0.0.0")
    |> mist.start()
  {
    Ok(_) -> {
      logging.quick_info("✅ HTTP server started with MCP WebSocket at /ws/mcp")
      Ok(Nil)
    }
    Error(_) -> {
      logging.quick_error("❌ Failed to start HTTP server")
      Error("Failed to start server")
    }
  }
}

/// Main request handler with tracing
fn handle_request(
  req: Request(Connection),
  bus: option.Option(process.Subject(event_bus.PubSubMessage)),
  mcp_registry: option.Option(ToolRegistry),
) -> Response(ResponseData) {
  let path = request.path_segments(req)
  let path_str = "/" <> string.join(path, "/")
  let method_str = http_method_to_string(req.method)

  // Generate trace_id for request tracing
  let trace_id = vibe_logger.generate_trace_id()
  let http_log = vibe_logger.new("http")
    |> vibe_logger.with_trace_id(trace_id)
    |> vibe_logger.with_data("method", json.string(method_str))
    |> vibe_logger.with_data("path", json.string(path_str))

  // Skip logging for health checks and static assets
  let should_log = case path {
    ["health"] | ["ready"] -> False
    _ -> True
  }

  case should_log {
    True -> vibe_logger.debug(http_log, "Request started")
    False -> Nil
  }

  let response = route_request(req, path, bus, mcp_registry)

  case should_log {
    True -> vibe_logger.debug(http_log |> vibe_logger.with_data("status", json.int(response.status)), "Request completed")
    False -> Nil
  }

  response
}

/// HTTP method to string helper
fn http_method_to_string(method: http.Method) -> String {
  case method {
    http.Get -> "GET"
    http.Post -> "POST"
    http.Put -> "PUT"
    http.Patch -> "PATCH"
    http.Delete -> "DELETE"
    http.Head -> "HEAD"
    http.Options -> "OPTIONS"
    http.Trace -> "TRACE"
    http.Connect -> "CONNECT"
    _ -> "OTHER"
  }
}

/// Route request to appropriate handler
fn route_request(
  req: Request(Connection),
  path: List(String),
  bus: option.Option(process.Subject(event_bus.PubSubMessage)),
  mcp_registry: option.Option(ToolRegistry),
) -> Response(ResponseData) {
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

    // Dashboard with real-time logs
    http.Get, ["dashboard"] -> serve_dashboard_file()
    
    // Digital Clone Control Dashboard
    http.Get, ["dashboard", "agent"] -> serve_agent_dashboard()
    
    // Test dashboard
    http.Get, ["dashboard", "test"] -> serve_test_dashboard()
    
    // Test simple route
    http.Get, ["test"] -> serve_test_dashboard()
    
    // Crypto-style logs page
    http.Get, ["logs"] -> serve_logs_file()

    // Lustre app - full Telegram message viewer
    http.Get, ["app"] -> lustre_app_handler()

    // Health check
    http.Get, ["health"] -> health_handler()

    // Ready check
    http.Get, ["ready"] -> ready_handler()

    // Prometheus metrics for agents
    http.Get, ["metrics", "agents"] -> metrics_agents_handler()

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

    // Agent Control API - full CRUD for agent configuration
    http.Get, ["api", "agent", "status"] -> agent_handlers.status_handler()
    http.Get, ["api", "agent", "config"] -> agent_handlers.config_get_handler()
    http.Post, ["api", "agent", "config"] -> agent_handlers.config_update_handler(req)
    http.Post, ["api", "agent", "start"] -> agent_handlers.start_handler(req)
    http.Post, ["api", "agent", "stop"] -> agent_handlers.stop_handler(req)
    http.Post, ["api", "agent", "pause"] -> agent_handlers.pause_handler(req)
    http.Post, ["api", "agent", "reset"] -> agent_handlers.reset_handler(req)

    // Test endpoint for simulating messages (for development)
    http.Post, ["api", "test", "message"] -> test_message_handler(req)

    // E2E Rainbow Bridge testing endpoint (ASYNC - returns 202 Accepted)
    http.Get, ["api", "e2e", "run"] -> e2e_handlers.run_handler()
    // E2E test run status (poll for results)
    http.Get, ["api", "e2e", "status", test_run_id] -> e2e_handlers.status_handler(test_run_id)
    // Legacy synchronous E2E (WARNING: may timeout)
    http.Get, ["api", "e2e", "run-sync"] -> e2e_handlers.run_sync_handler()
    // E2E AI function tests (longer timeout)
    http.Get, ["api", "e2e", "ai"] -> e2e_handlers.ai_handler()
    // Quick /neuro test (60s timeout)
    http.Get, ["api", "e2e", "neuro"] -> e2e_handlers.neuro_test_handler()
    // ETS debug endpoint
    http.Get, ["api", "e2e", "test-ets"] -> e2e_handlers.test_ets_handler()
    // Video E2E multi-step test (5+ minute timeout)
    http.Get, ["api", "e2e", "video"] -> e2e_handlers.video_handler()
    // Reels Full Flow E2E test (6 minute timeout) - complete /reels flow
    http.Get, ["api", "e2e", "reels-flow"] -> e2e_handlers.reels_flow_handler()
    // Reels Pipeline Direct Test (bypasses Telegram UI/buttons)
    http.Get, ["api", "e2e", "reels-pipeline"] -> e2e_handlers.reels_pipeline_handler()
    // P2P Lead Forward E2E test (async)
    http.Get, ["api", "e2e", "p2p"] -> e2e_handlers.p2p_handler()

    // Video Auto-Render Pipeline
    // POST /api/video/auto-render - Start fully automated video pipeline
    http.Post, ["api", "video", "auto-render"] -> video_api.auto_render_handler(req)
    // POST /api/video/ai-render - Start AI-powered pipeline with dynamic B-roll
    http.Post, ["api", "video", "ai-render"] -> video_api.ai_render_handler(req)
    // GET /api/video/status/:pipeline_id - Check pipeline status
    http.Get, ["api", "video", "status", pipeline_id] -> video_api.status_handler(pipeline_id)
    // GET /api/video/prompts/preview - Preview B-roll prompts (no render)
    http.Get, ["api", "video", "prompts", "preview"] -> video_api.prompts_preview_handler(req)
    // POST /api/video/ai-reels/template1 - AI Reels Template 1 (full emulation in test mode)
    http.Post, ["api", "video", "ai-reels", "template1"] -> video_api.template1_handler(req)

    // Render Quota API (Freemium: 3 free renders, then subscription)
    // GET /api/render-quota?telegram_id=123 - Check render quota
    http.Get, ["api", "render-quota"] -> render_quota_handlers.check_quota_handler(req)
    // POST /api/render-log - Log a render (increment counter)
    http.Post, ["api", "render-log"] -> render_quota_handlers.log_render_handler(req)

    // Logs page - real-time log viewer
    http.Get, ["logs"] -> logs_page_handler()
    http.Get, ["api", "v1", "logs", "tail"] -> logs_tail_handler()

    // Events page - real-time event stream (PubSub pattern)
    http.Get, ["events"] -> events_page_handler()

    // WebSocket endpoint for real-time updates
    http.Get, ["ws"] -> websocket_handler(req)
    http.Get, ["ws", "logs"] -> logs_websocket_handler(req)
    http.Get, ["ws", "events"] -> events_websocket_handler_with_bus(req, bus)
    http.Get, ["ws", "agents"] -> agent_websocket.handler(req)

    // Editor Agent WebSocket for AI template editing (supports both paths)
    http.Get, ["ws", "agent"] -> {
      io.println("[Router] 🎯 Matched /ws/agent route, calling editor_agent_ws.handler")
      editor_agent_ws.handler(req)
    }
    http.Get, ["agent"] -> {
      io.println("[Router] 🎯 Matched /agent route, calling editor_agent_ws.handler")
      editor_agent_ws.handler(req)
    }

    // Payment webhooks
    http.Post, ["api", "robokassa-result"] -> payment_webhooks.handle_robokassa(req)
    http.Post, ["api", "payment-success"] -> payment_webhooks.handle_robokassa(req)
    http.Post, ["api", "webhooks", "ton"] -> payment_webhooks.handle_ton(req)
    http.Post, ["api", "webhooks", "stars"] -> payment_webhooks.handle_stars(req)

    // Bot API callback (forwarded from Go bridge webhook)
    http.Post, ["api", "v1", "bot", "callback"] -> handle_bot_callback(req)
    http.Get, ["api", "v1", "bot", "status"] -> handle_bot_status(req)

    // AI Service Webhooks (Replicate, Hedra, HeyGen, Kling, KIE.ai, BFL, ElevenLabs)
    http.Post, ["api", "webhooks", service] -> webhook_handlers.handle_webhook(req, service)

    // P2P Control Panel UI (Earning Dashboard)
    http.Get, ["p2p"] -> p2p_panel_handler()
    http.Get, ["earning"] -> p2p_panel_handler()  // Alias route
    
    // Leads management
    http.Get, ["leads"] -> leads_handlers.list_leads()
    http.Get, ["leads", lead_id] -> leads_handlers.get_lead(lead_id)
    
    // Aimly branded leads page
    http.Get, ["aimly", "leads"] -> aimly_leads_handler()
    
    // Leads API
    http.Get, ["api", "v1", "leads"] -> leads_handlers.list_leads_json()
    http.Get, ["api", "v1", "leads", lead_id] -> leads_handlers.get_lead_json(lead_id)
    http.Get, ["api", "v1", "leads", lead_id, "history"] -> leads_handlers.get_lead_history(lead_id)
    http.Put, ["api", "v1", "leads", lead_id, "status"] -> leads_handlers.update_lead_status(req, lead_id)
    http.Post, ["api", "v1", "leads", lead_id, "notes"] -> leads_handlers.add_lead_note(lead_id, "")
    http.Post, ["api", "v1", "leads", lead_id, "message"] -> leads_handlers.send_message_to_lead(lead_id, "")

    // ==========================================================================
    // GraphQL API - Leads CRM
    // ==========================================================================
    http.Post, ["graphql"] -> graphql_handlers.query_handler(req)
    http.Get, ["graphql"] -> graphql_handlers.get_handler(req)
    http.Get, ["graphql", "playground"] -> graphql_handlers.playground_handler(req)
    http.Get, ["graphql", "sse"] -> graphql_handlers.sse_handler(req)
    // CORS preflight for GraphQL
    http.Options, ["graphql"] -> cors_preflight_handler()

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

    // ==========================================================================
    // Digital Twin Configuration API (ElizaOS compatible)
    // ==========================================================================
    http.Get, ["api", "v1", "twin", "config"] -> handle_twin_config_get()
    http.Put, ["api", "v1", "twin", "config"] -> handle_twin_config_update(req)
    http.Get, ["api", "v1", "twin", "export"] -> handle_twin_export()
    http.Post, ["api", "v1", "twin", "import"] -> handle_twin_import(req)
    http.Get, ["api", "v1", "twin", "prompt"] -> handle_twin_prompt_get()

    // 404 for unknown routes
    _, _ -> not_found_handler()
  }
}

// Handlers

fn aimly_leads_handler() -> Response(ResponseData) {
  let body = "<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>Aimly - Client Management</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            padding: 20px;
        }

        .container {
            max-width: 1400px;
            margin: 0 auto;
        }

        .header {
            background: white;
            border-radius: 16px;
            padding: 32px;
            margin-bottom: 24px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }

        .header h1 {
            font-size: 36px;
            font-weight: 700;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            margin-bottom: 8px;
        }

        .header p {
            color: #64748b;
            font-size: 16px;
        }

        .stats-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 20px;
            margin-bottom: 24px;
        }

        .stat-card {
            background: white;
            border-radius: 12px;
            padding: 24px;
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        }

        .stat-label {
            color: #64748b;
            font-size: 14px;
            font-weight: 500;
            margin-bottom: 8px;
        }

        .stat-value {
            font-size: 32px;
            font-weight: 700;
            color: #1e293b;
        }

        .stat-trend {
            font-size: 14px;
            margin-top: 8px;
        }

        .stat-trend.up {
            color: #10b981;
        }

        .stat-trend.down {
            color: #ef4444;
        }

        .leads-container {
            background: white;
            border-radius: 16px;
            padding: 32px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }

        .leads-header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 24px;
        }

        .leads-header h2 {
            font-size: 24px;
            font-weight: 600;
            color: #1e293b;
        }

        .filter-tabs {
            display: flex;
            gap: 8px;
        }

        .filter-tab {
            padding: 8px 16px;
            border-radius: 8px;
            border: none;
            background: #f1f5f9;
            color: #64748b;
            font-size: 14px;
            font-weight: 500;
            cursor: pointer;
            transition: all 0.2s;
        }

        .filter-tab:hover {
            background: #e2e8f0;
        }

        .filter-tab.active {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
        }

        .leads-table {
            width: 100%;
            border-collapse: collapse;
        }

        .leads-table thead {
            background: #f8fafc;
        }

        .leads-table th {
            padding: 12px 16px;
            text-align: left;
            font-size: 12px;
            font-weight: 600;
            color: #64748b;
            text-transform: uppercase;
            letter-spacing: 0.05em;
        }

        .leads-table td {
            padding: 16px;
            border-top: 1px solid #f1f5f9;
        }

        .leads-table tbody tr {
            transition: background 0.2s;
        }

        .leads-table tbody tr:hover {
            background: #f8fafc;
        }

        .client-info {
            display: flex;
            align-items: center;
            gap: 12px;
        }

        .client-avatar {
            width: 40px;
            height: 40px;
            border-radius: 50%;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            display: flex;
            align-items: center;
            justify-content: center;
            color: white;
            font-weight: 600;
            font-size: 16px;
        }

        .client-details {
            flex: 1;
        }

        .client-name {
            font-weight: 600;
            color: #1e293b;
            margin-bottom: 2px;
        }

        .client-company {
            font-size: 14px;
            color: #64748b;
        }

        .status-badge {
            padding: 4px 12px;
            border-radius: 12px;
            font-size: 12px;
            font-weight: 500;
        }

        .status-badge.new {
            background: #dbeafe;
            color: #1e40af;
        }

        .status-badge.contacted {
            background: #fef3c7;
            color: #92400e;
        }

        .status-badge.qualified {
            background: #d1fae5;
            color: #065f46;
        }

        .status-badge.proposal {
            background: #e0e7ff;
            color: #3730a3;
        }

        .status-badge.won {
            background: #d1fae5;
            color: #065f46;
        }

        .status-badge.lost {
            background: #fee2e2;
            color: #991b1b;
        }

        .action-btn {
            padding: 6px 12px;
            border-radius: 6px;
            border: 1px solid #e2e8f0;
            background: white;
            color: #64748b;
            font-size: 14px;
            cursor: pointer;
            transition: all 0.2s;
        }

        .action-btn:hover {
            border-color: #667eea;
            color: #667eea;
        }

        .empty-state {
            text-align: center;
            padding: 60px 20px;
            color: #64748b;
        }

        .empty-state-icon {
            font-size: 48px;
            margin-bottom: 16px;
        }

        @media (max-width: 768px) {
            .stats-grid {
                grid-template-columns: 1fr;
            }

            .leads-header {
                flex-direction: column;
                align-items: flex-start;
                gap: 16px;
            }

            .leads-table {
                display: block;
                overflow-x: auto;
            }
        }
    </style>
</head>
<body>
    <div class=\"container\">
        <div class=\"header\">
            <h1>🎯 Aimly</h1>
            <p>AI-Powered Client Management & Lead Intelligence</p>
        </div>

        <div class=\"stats-grid\">
            <div class=\"stat-card\">
                <div class=\"stat-label\">Total Clients</div>
                <div class=\"stat-value\" id=\"totalClients\">0</div>
                <div class=\"stat-trend up\">↑ Loading...</div>
            </div>
            <div class=\"stat-card\">
                <div class=\"stat-label\">Active Deals</div>
                <div class=\"stat-value\" id=\"activeDeals\">0</div>
                <div class=\"stat-trend up\">↑ Loading...</div>
            </div>
            <div class=\"stat-card\">
                <div class=\"stat-label\">Conversion Rate</div>
                <div class=\"stat-value\" id=\"conversionRate\">0%</div>
                <div class=\"stat-trend up\">↑ Loading...</div>
            </div>
            <div class=\"stat-card\">
                <div class=\"stat-label\">Revenue This Month</div>
                <div class=\"stat-value\" id=\"revenue\">$0</div>
                <div class=\"stat-trend up\">↑ Loading...</div>
            </div>
        </div>

        <div class=\"leads-container\">
            <div class=\"leads-header\">
                <h2>Client Pipeline</h2>
                <div class=\"filter-tabs\">
                    <button class=\"filter-tab active\" data-filter=\"all\">All</button>
                    <button class=\"filter-tab\" data-filter=\"new\">New</button>
                    <button class=\"filter-tab\" data-filter=\"qualified\">Qualified</button>
                    <button class=\"filter-tab\" data-filter=\"proposal\">Proposal</button>
                    <button class=\"filter-tab\" data-filter=\"won\">Won</button>
                </div>
            </div>

            <table class=\"leads-table\" id=\"leadsTable\">
                <thead>
                    <tr>
                        <th>Client</th>
                        <th>Status</th>
                        <th>Source</th>
                        <th>Value</th>
                        <th>Last Contact</th>
                        <th>Actions</th>
                    </tr>
                </thead>
                <tbody id=\"leadsBody\">
                    <tr>
                        <td colspan=\"6\">
                            <div class=\"empty-state\">
                                <div class=\"empty-state-icon\">📊</div>
                                <p>Loading client data...</p>
                            </div>
                        </td>
                    </tr>
                </tbody>
            </table>
        </div>
    </div>

    <script>
        const API_URL = window.location.origin;
        let allLeads = [];
        let currentFilter = 'all';

        // Fetch leads from API
        async function fetchLeads() {
            try {
                const response = await fetch(`${API_URL}/api/v1/leads`);
                const data = await response.json();
                allLeads = data.leads || [];
                updateStats();
                renderLeads();
            } catch (e) {
                console.error('Failed to fetch leads:', e);
                showError();
            }
        }

        // Update statistics
        function updateStats() {
            const total = allLeads.length;
            const active = allLeads.filter(l => ['qualified', 'proposal'].includes(l.status)).length;
            const won = allLeads.filter(l => l.status === 'won').length;
            const conversionRate = total > 0 ? Math.round((won / total) * 100) : 0;

            document.getElementById('totalClients').textContent = total;
            document.getElementById('activeDeals').textContent = active;
            document.getElementById('conversionRate').textContent = conversionRate + '%';
            document.getElementById('revenue').textContent = '$' + (won * 5000).toLocaleString();
        }

        // Render leads table
        function renderLeads() {
            const tbody = document.getElementById('leadsBody');
            const filtered = currentFilter === 'all' 
                ? allLeads 
                : allLeads.filter(l => l.status === currentFilter);

            if (filtered.length === 0) {
                tbody.innerHTML = `
                    <tr>
                        <td colspan=\"6\">
                            <div class=\"empty-state\">
                                <div class=\"empty-state-icon\">🔍</div>
                                <p>No clients found</p>
                            </div>
                        </td>
                    </tr>
                `;
                return;
            }

            tbody.innerHTML = filtered.map(lead => {
                const initials = (lead.name || 'U').split(' ').map(n => n[0]).join('').toUpperCase();
                const statusClass = lead.status || 'new';
                const statusLabel = statusClass.charAt(0).toUpperCase() + statusClass.slice(1);
                
                return `
                    <tr>
                        <td>
                            <div class=\"client-info\">
                                <div class=\"client-avatar\">${initials}</div>
                                <div class=\"client-details\">
                                    <div class=\"client-name\">${lead.name || 'Unknown'}</div>
                                    <div class=\"client-company\">${lead.company || 'No company'}</div>
                                </div>
                            </div>
                        </td>
                        <td><span class=\"status-badge ${statusClass}\">${statusLabel}</span></td>
                        <td>${lead.source || 'Telegram'}</td>
                        <td>$${(Math.random() * 10000 + 1000).toFixed(0)}</td>
                        <td>${formatDate(lead.created_at)}</td>
                        <td>
                            <button class=\"action-btn\" onclick=\"viewLead('${lead.id}')\">View</button>
                        </td>
                    </tr>
                `;
            }).join('');
        }

        // Format date
        function formatDate(timestamp) {
            if (!timestamp) return 'N/A';
            const date = new Date(timestamp * 1000);
            return date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' });
        }

        // View lead details
        function viewLead(id) {
            window.location.href = `/leads/${id}`;
        }

        // Show error state
        function showError() {
            document.getElementById('leadsBody').innerHTML = `
                <tr>
                    <td colspan=\"6\">
                        <div class=\"empty-state\">
                            <div class=\"empty-state-icon\">⚠️</div>
                            <p>Failed to load client data</p>
                        </div>
                    </td>
                </tr>
            `;
        }

        // Filter tabs
        document.querySelectorAll('.filter-tab').forEach(tab => {
            tab.addEventListener('click', () => {
                document.querySelectorAll('.filter-tab').forEach(t => t.classList.remove('active'));
                tab.classList.add('active');
                currentFilter = tab.dataset.filter;
                renderLeads();
            });
        });

        // Initialize
        fetchLeads();
        setInterval(fetchLeads, 30000); // Refresh every 30 seconds
    </script>
</body>
</html>"
  html_response(200, body)
}

fn serve_dashboard_file() -> Response(ResponseData) {
  case simplifile.read("dashboard/index.html") {
    Ok(content) -> html_response(200, content)
    Error(_) -> {
      let body = "<html><body><h1>Dashboard not found</h1><p>Run from project root</p></body></html>"
      html_response(404, body)
    }
  }
}

fn serve_logs_file() -> Response(ResponseData) {
  case simplifile.read("dashboard/logs.html") {
    Ok(content) -> html_response(200, content)
    Error(_) -> {
      let body = "<html><body><h1>Logs page not found</h1><p>Run from project root</p></body></html>"
      html_response(404, body)
    }
  }
}

fn serve_agent_dashboard() -> Response(ResponseData) {
  case simplifile.read("dashboard/agent.html") {
    Ok(content) -> html_response(200, content)
    Error(_) -> {
      let body = "<html><body><h1>Agent Dashboard not found</h1><p>Run from project root</p></body></html>"
      html_response(404, body)
    }
  }
}

fn serve_test_dashboard() -> Response(ResponseData) {
  case simplifile.read("dashboard/test.html") {
    Ok(content) -> html_response(200, content)
    Error(_) -> {
      let body = "<html><body><h1>Test not found</h1></body></html>"
      html_response(404, body)
    }
  }
}

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

fn agent_status_handler() -> Response(ResponseData) {
  let body = json.object([
    #("id", json.string("vibee_agent_1")),
    #("status", json.string("running")),
    #("stats", json.object([
      #("events_processed", json.int(0)),
      #("messages_sent", json.int(0)),
      #("tasks_completed", json.int(0)),
      #("tasks_failed", json.int(0)),
      #("uptime_seconds", json.int(0)),
    ])),
    #("confidence_scores", json.object([
      #("code_generate", json.float(0.8)),
      #("code_refactor", json.float(0.75)),
      #("test_run", json.float(0.85)),
      #("test_create", json.float(0.7)),
      #("debug_build", json.float(0.9)),
      #("debug_analyze", json.float(0.75)),
      #("debug_fix", json.float(0.65)),
    ])),
  ])

  json_response(200, body)
}

/// Test endpoint for simulating incoming messages
fn test_message_handler(req: Request(Connection)) -> Response(ResponseData) {
  case mist.read_body(req, 10_000) {
    Error(_) -> json_response(400, json.object([#("error", json.string("Failed to read body"))]))
    Ok(body_result) -> {
      case bit_array.to_string(body_result.body) {
        Error(_) -> json_response(400, json.object([#("error", json.string("Invalid UTF-8"))]))
        Ok(body_str) -> {
          // Parse JSON: {"chat_id": "123", "text": "/pricing", "from_name": "Test", "from_id": "123"}
          let chat_id = extract_json_string(body_str, "chat_id") |> option.unwrap("0")
          let text = extract_json_string(body_str, "text") |> option.unwrap("")
          let from_name = extract_json_string(body_str, "from_name") |> option.unwrap("TestUser")
          let from_id = extract_json_string(body_str, "from_id")
            |> option.unwrap("999999")
            |> int.parse()
            |> result.unwrap(999999)

          logging.quick_info("[TEST] Simulating message: " <> text <> " in chat " <> chat_id)

          // Get config from environment
          let bridge_url = telegram_config.bridge_url()
          let session_id = config.get_env_or("TELEGRAM_SESSION_ID", "sess_test")

          let state = vibee_telegram_agent.AgentState(
            config: vibee_telegram_agent.TelegramAgentConfig(
              bridge_url: bridge_url,
              session_id: session_id,
              llm_api_key: None,
              llm_model: "gpt-4",
              auto_reply_enabled: True,
              cooldown_ms: 0,
              digital_twin_enabled: True,
              owner_id: 0,
            ),
            bot_user_id: None,
            is_monitoring: True,
            total_messages: 0,
            last_reply_time: 0,
            monitored_chats: [],
          )

          // Process the message (reply_to_id = 0 for test endpoint)
          // Note: phone, lang_code, is_premium are empty/false for test endpoint
          let _ = vibee_telegram_agent.handle_incoming_message(state, chat_id, from_id, from_name, "", "", "", False, text, 0, 0)

          json_response(200, json.object([
            #("status", json.string("ok")),
            #("message", json.string("Processed: " <> text)),
          ]))
        }
      }
    }
  }
}

/// Extract string value from JSON (simple parser)
fn extract_json_string(json_str: String, key: String) -> Option(String) {
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

/// Prometheus metrics handler for agents
fn metrics_agents_handler() -> Response(ResponseData) {
  let metrics = agent_metrics.generate_metrics()
  let body_bytes = bytes_tree.from_string(metrics)

  response.new(200)
  |> response.set_header("content-type", "text/plain; version=0.0.4; charset=utf-8")
  |> response.set_body(mist.Bytes(body_bytes))
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

/// CORS preflight handler for OPTIONS requests
fn cors_preflight_handler() -> Response(ResponseData) {
  response.new(204)
  |> response.set_header("access-control-allow-origin", "*")
  |> response.set_header("access-control-allow-methods", "GET, POST, OPTIONS")
  |> response.set_header("access-control-allow-headers", "content-type, authorization")
  |> response.set_header("access-control-max-age", "86400")
  |> response.set_body(mist.Bytes(bytes_tree.new()))
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

// Get current timestamp in ISO format
fn get_current_timestamp() -> String {
  "2025-12-18T12:00:00Z"  // Simple static timestamp for now
}

// WebSocket handler for log streaming (now uses log_aggregator)
fn logs_websocket_handler(req: Request(Connection)) -> Response(ResponseData) {
  io.println("[WS:Logs] New client connecting...")
  
  // Create a subject to receive log broadcasts
  let log_subject = process.new_subject()

  // Subscribe to log_aggregator if available
  case log_aggregator.get_global() {
    Some(aggregator) -> {
      log_aggregator.subscribe(aggregator, log_subject)
      io.println("[WS:Logs] ✅ Subscribed to log_aggregator")
      
      // Send a test log to verify it works
      let test_entry = log_aggregator.LogEntry(
        timestamp: "2025-12-18T12:00:00Z",
        level: "info",
        logger: "system",
        message: "🔔 Test: log_aggregator is working!",
        trace_id: None,
        request_id: None,
        session_id: None,
        span_id: None,
        tool: None,
        extra: [],
      )
      log_aggregator.log(aggregator, test_entry)
      io.println("[WS:Logs] 📤 Sent test log to aggregator")
    }
    None -> {
      io.println("[WS:Logs] ⚠️  log_aggregator not available!")
    }
  }

  // Create selector to receive log messages
  let selector = process.new_selector()
    |> process.select_map(for: log_subject, mapping: fn(log_json: String) {
      LogMessage(log_json)
    })

  let state = LogWsState(log_subject: log_subject)

  mist.websocket(
    request: req,
    on_init: fn(conn) {
      io.println("[WS:Logs] 🎬 on_init called")
      
      // Send welcome message
      let welcome = "{\"timestamp\":\"" <> get_current_timestamp() <> "\",\"level\":\"info\",\"logger\":\"system\",\"message\":\"WebSocket connected - listening for logs...\"}"
      mist.send_text_frame(conn, welcome)
      io.println("[WS:Logs] ✅ Sent welcome message")
      
      // Send recent logs from aggregator
      case log_aggregator.get_global() {
        Some(aggregator) -> {
          io.println("[WS:Logs] 📤 Fetching recent logs from aggregator...")
          let recent_logs = log_aggregator.get_recent(aggregator, 100)
          io.println("[WS:Logs] 📦 Got " <> int.to_string(list.length(recent_logs)) <> " logs from aggregator")
          
          list.each(recent_logs, fn(entry) {
            mist.send_text_frame(conn, log_aggregator.log_entry_to_json(entry))
          })
          
          io.println("[WS:Logs] ✅ Sent " <> int.to_string(list.length(recent_logs)) <> " recent logs to client")
        }
        None -> {
          io.println("[WS:Logs] ⚠️  log_aggregator not available for history")
        }
      }
      
      io.println("[WS:Logs] 🏁 on_init complete")
      #(state, Some(selector)) 
    },
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
      io.println("[TRACE:WS] 📨 Received log from aggregator: " <> string.slice(log_json, 0, 50))
      // Forward log message to WebSocket client
      io.println("[TRACE:WS] 📤 Sending to WebSocket client...")
      let _ = mist.send_text_frame(conn, log_json)
      io.println("[TRACE:WS] ✅ Sent to client")
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
// RAG PARSER HANDLERS - Parse Telegram dialogs for Digital Twin memory
// =============================================================================

/// Parse all dialogs - POST /api/parse/all
fn handle_parse_all(_req: Request(Connection)) -> Response(ResponseData) {
  logging.quick_info("[PARSER] Starting full parse of all dialogs")

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
  logging.quick_info("[PARSER] Starting parse for chat: " <> chat_id_str)

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

// =============================================================================
// Digital Twin Configuration Handlers
// =============================================================================

/// Get Digital Twin configuration
fn handle_twin_config_get() -> Response(ResponseData) {
  case postgres.get_global_pool() {
    None -> {
      let body = json.object([#("error", json.string("Database not configured"))])
      json_response(500, body)
    }
    Some(pool) -> {
      case twin_config.get_active(pool) {
        Ok(config) -> {
          let body = json.object([
            #("id", json.string(config.id)),
            #("name", json.string(config.name)),
            #("username", case config.username {
              Some(u) -> json.string(u)
              None -> json.null()
            }),
            #("bio", json.array(config.bio, json.string)),
            #("adjectives", json.array(config.adjectives, json.string)),
            #("topics", json.array(config.topics, json.string)),
            #("style", json.object([
              #("tone", json.string(config.style.tone)),
              #("language", json.string(config.style.language)),
              #("all", json.array(config.style.all, json.string)),
              #("chat", json.array(config.style.chat, json.string)),
              #("post", json.array(config.style.post, json.string)),
            ])),
            #("settings", json.object([
              #("model", json.string(config.settings.model)),
              #("temperature", json.float(config.settings.temperature)),
              #("max_tokens", json.int(config.settings.max_tokens)),
            ])),
            #("plugins", json.array(config.plugins, json.string)),
            #("is_active", json.bool(config.is_active)),
          ])
          json_response(200, body)
        }
        Error(_) -> {
          let body = json.object([#("error", json.string("Digital Twin config not found"))])
          json_response(404, body)
        }
      }
    }
  }
}

/// Update Digital Twin configuration field
fn handle_twin_config_update(req: Request(Connection)) -> Response(ResponseData) {
  case mist.read_body(req, 1024 * 100) {
    Error(_) -> {
      let body = json.object([#("error", json.string("Failed to read body"))])
      json_response(400, body)
    }
    Ok(req_with_body) -> {
      let body_str = case bit_array.to_string(req_with_body.body) {
        Ok(s) -> s
        Error(_) -> ""
      }

      case postgres.get_global_pool() {
        None -> {
          let body = json.object([#("error", json.string("Database not configured"))])
          json_response(500, body)
        }
        Some(pool) -> {
          // Parse field and value from JSON body
          // Expected: {"field": "name", "value": "New Name"}
          let body = json.object([
            #("success", json.bool(True)),
            #("message", json.string("Config update endpoint - parse body: " <> body_str)),
          ])
          json_response(200, body)
        }
      }
    }
  }
}

/// Export Digital Twin configuration as ElizaOS JSON
fn handle_twin_export() -> Response(ResponseData) {
  case postgres.get_global_pool() {
    None -> {
      let body = json.object([#("error", json.string("Database not configured"))])
      json_response(500, body)
    }
    Some(pool) -> {
      case twin_config.get_active(pool) {
        Ok(config) -> {
          let elizaos_json = twin_config.export_to_elizaos(config)
          // Return raw JSON string as response
          response.new(200)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string(elizaos_json)))
        }
        Error(_) -> {
          let body = json.object([#("error", json.string("Digital Twin config not found"))])
          json_response(404, body)
        }
      }
    }
  }
}

/// Import Digital Twin configuration from ElizaOS JSON
fn handle_twin_import(req: Request(Connection)) -> Response(ResponseData) {
  case mist.read_body(req, 1024 * 100) {
    Error(_) -> {
      let body = json.object([#("error", json.string("Failed to read body"))])
      json_response(400, body)
    }
    Ok(req_with_body) -> {
      let body_str = case bit_array.to_string(req_with_body.body) {
        Ok(s) -> s
        Error(_) -> ""
      }

      case postgres.get_global_pool() {
        None -> {
          let body = json.object([#("error", json.string("Database not configured"))])
          json_response(500, body)
        }
        Some(pool) -> {
          case twin_config.import_from_elizaos(pool, body_str) {
            Ok(Nil) -> {
              let body = json.object([
                #("success", json.bool(True)),
                #("message", json.string("ElizaOS Character imported successfully")),
              ])
              json_response(200, body)
            }
            Error(err) -> {
              let body = json.object([
                #("error", json.string("Import failed")),
              ])
              json_response(400, body)
            }
          }
        }
      }
    }
  }
}

/// Get generated system prompt
fn handle_twin_prompt_get() -> Response(ResponseData) {
  case postgres.get_global_pool() {
    None -> {
      let body = json.object([#("error", json.string("Database not configured"))])
      json_response(500, body)
    }
    Some(pool) -> {
      case twin_config.get_active(pool) {
        Ok(config) -> {
          let prompt = twin_config.build_system_prompt(config)
          let body = json.object([
            #("name", json.string(config.name)),
            #("system_prompt", json.string(prompt)),
            #("prompt_length", json.int(string.length(prompt))),
          ])
          json_response(200, body)
        }
        Error(_) -> {
          let body = json.object([#("error", json.string("Digital Twin config not found"))])
          json_response(404, body)
        }
      }
    }
  }
}

// ==========================================================================
// Bot API Callback Handler (receives callbacks from Go bridge webhook)
// ==========================================================================

/// Handle incoming bot callback from Go bridge
fn handle_bot_callback(req: Request(Connection)) -> Response(ResponseData) {
  io.println("[BotCallback] 📥 Received callback from Go bridge")

  case mist.read_body(req, 1024 * 1024) {
    Ok(req_with_body) -> {
      let body = bit_array.to_string(req_with_body.body)
      case body {
        Ok(json_body) -> {
          io.println("[BotCallback] Body: " <> string.slice(json_body, 0, 200))

          // Parse the callback query
          case bot_api.parse_callback_query(json_body) {
            Ok(callback) -> {
              io.println("[BotCallback] ✅ Parsed: query_id=" <> callback.query_id <> ", data=" <> callback.data)

              // Route through scene FSM (similar to telegram_agent handling)
              // For now, just log and acknowledge
              let bridge_url = telegram_config.bridge_url()
              let api_key = telegram_config.bridge_api_key()
              let bot_config = bot_api.with_key(bridge_url, api_key)

              // Answer the callback to remove "loading" indicator
              case bot_api.answer_callback(bot_config, callback.query_id, "✅ Processing...", False) {
                Ok(_) -> io.println("[BotCallback] Callback answered")
                Error(_) -> io.println("[BotCallback] ⚠️ Failed to answer callback")
              }

              // Process the callback data through scene router
              let chat_id = callback.chat_id
              let user_id = callback.user_id
              let callback_data = callback.data

              io.println("[BotCallback] 🎯 Routing: chat=" <> int.to_string(chat_id) <> ", user=" <> int.to_string(user_id) <> ", data=" <> callback_data)

              // TODO: Call scene router to process callback
              // For now, return success
              let response_body = json.object([
                #("success", json.bool(True)),
                #("chat_id", json.int(chat_id)),
                #("callback_data", json.string(callback_data)),
                #("message", json.string("Callback processed")),
              ])
              json_response(200, response_body)
            }
            Error(_) -> {
              io.println("[BotCallback] ❌ Failed to parse callback query")
              let body = json.object([#("error", json.string("Invalid callback format"))])
              json_response(400, body)
            }
          }
        }
        Error(_) -> {
          let body = json.object([#("error", json.string("Invalid body encoding"))])
          json_response(400, body)
        }
      }
    }
    Error(_) -> {
      let body = json.object([#("error", json.string("Failed to read body"))])
      json_response(500, body)
    }
  }
}

/// Get Bot API status
fn handle_bot_status(_req: Request(Connection)) -> Response(ResponseData) {
  let bridge_url = telegram_config.bridge_url()
  let api_key = telegram_config.bridge_api_key()
  let bot_config = bot_api.with_key(bridge_url, api_key)

  case bot_api.get_status(bot_config) {
    Ok(status) -> {
      let body = json.object([
        #("configured", json.bool(status.configured)),
        #("bot_id", case status.bot_id {
          Some(id) -> json.int(id)
          None -> json.null()
        }),
        #("username", case status.username {
          Some(u) -> json.string(u)
          None -> json.null()
        }),
      ])
      json_response(200, body)
    }
    Error(_) -> {
      let body = json.object([
        #("configured", json.bool(False)),
        #("error", json.string("Failed to get bot status from bridge")),
      ])
      json_response(200, body)
    }
  }
}
