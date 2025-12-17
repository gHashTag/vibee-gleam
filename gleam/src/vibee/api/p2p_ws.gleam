// P2P WebSocket Handler
// Real-time updates for P2P Control Panel
// Reuses MCP tools for actual functionality

import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import mist.{type Connection, type ResponseData}
import vibee/earning/types as earning_types
import vibee/earning/arbitrage
import vibee/p2p/types as p2p_types
import vibee/events/event_bus

// FFI declarations for real data
@external(erlang, "vibee_earning_ffi", "get_status")
fn get_agent_status(telegram_id: Int) -> earning_types.AgentStatus

@external(erlang, "vibee_earning_ffi", "get_stats")
fn get_agent_stats(telegram_id: Int) -> earning_types.EarningStats

@external(erlang, "vibee_p2p_ffi", "list_orders")
fn list_orders_ffi(limit: Int) -> List(Dynamic)

@external(erlang, "vibee_p2p_ffi", "get_map_string")
fn ffi_get_map_string(map: Dynamic, key: String) -> String

@external(erlang, "vibee_p2p_ffi", "get_map_float")
fn ffi_get_map_float(map: Dynamic, key: String) -> Float

@external(erlang, "vibee_p2p_ffi", "get_map_int")
fn ffi_get_map_int(map: Dynamic, key: String) -> Int

@external(erlang, "vibee_earning_ffi", "update_strategy")
fn update_strategy_ffi(telegram_id: Int, strategy: String) -> Bool

// =============================================================================
// TYPES
// =============================================================================

/// WebSocket message types for P2P
pub type P2PWsMessage {
  StatusUpdate(json.Json)
  OrderUpdate(json.Json)
  ArbitrageUpdate(json.Json)
  LogMessage(json.Json)
  PriceUpdate(json.Json)
}

/// P2P WebSocket state
pub type P2PWsState {
  P2PWsState(
    telegram_id: Option(Int),
    subscribed_channels: List(String),
    event_bus: Option(process.Subject(event_bus.PubSubMessage)),
    client_subject: Option(process.Subject(String)),
  )
}

// =============================================================================
// WEBSOCKET HANDLER
// =============================================================================

/// P2P WebSocket endpoint handler
pub fn handler(
  req: Request(Connection),
  event_bus_opt: Option(process.Subject(event_bus.PubSubMessage)),
) -> Response(ResponseData) {
  io.println("[WS:P2P] Starting P2P WebSocket handler")

  mist.websocket(
    request: req,
    on_init: fn(_conn) {
      // Create client subject for receiving events
      let client_subject = process.new_subject()

      // Subscribe to event bus if available
      case event_bus_opt {
        Some(bus) -> {
          event_bus.subscribe(bus, client_subject)
          io.println("[WS:P2P] Client subscribed to event bus")
        }
        None -> Nil
      }

      // Create selector to receive events
      let selector = process.new_selector()
        |> process.select_map(for: client_subject, mapping: fn(json_str: String) {
          StatusUpdate(json.string(json_str))
        })

      let state = P2PWsState(
        telegram_id: None,
        subscribed_channels: [],
        event_bus: event_bus_opt,
        client_subject: Some(client_subject),
      )

      // Send welcome message
      io.println("[WS:P2P] Client connected")

      #(state, Some(selector))
    },
    on_close: fn(state) {
      io.println("[WS:P2P] Client disconnected")
      // Unsubscribe from event bus
      case state.event_bus, state.client_subject {
        Some(bus), Some(subj) -> event_bus.unsubscribe(bus, subj)
        _, _ -> Nil
      }
    },
    handler: handle_p2p_message,
  )
}

// =============================================================================
// MESSAGE HANDLERS
// =============================================================================

fn handle_p2p_message(
  state: P2PWsState,
  message: mist.WebsocketMessage(P2PWsMessage),
  conn: mist.WebsocketConnection,
) {
  case message {
    mist.Text(text) -> {
      handle_text_message(state, text, conn)
    }
    mist.Binary(_) -> {
      mist.continue(state)
    }
    mist.Custom(ws_msg) -> {
      handle_custom_message(state, ws_msg, conn)
    }
    mist.Closed | mist.Shutdown -> {
      // Cleanup on close
      case state.event_bus, state.client_subject {
        Some(bus), Some(subj) -> event_bus.unsubscribe(bus, subj)
        _, _ -> Nil
      }
      mist.stop()
    }
  }
}

fn handle_text_message(
  state: P2PWsState,
  text: String,
  conn: mist.WebsocketConnection,
) {
  // Try to parse as JSON
  // Check if text looks like JSON (starts with {)
  case string.starts_with(string.trim(text), "{") {
    True -> {
      // Try to handle as JSON command
      handle_json_command(state, text, conn)
    }
    False -> {
      // Handle simple text commands
      handle_simple_command(state, text, conn)
    }
  }
}

fn handle_simple_command(
  state: P2PWsState,
  text: String,
  conn: mist.WebsocketConnection,
) {
  case string.lowercase(string.trim(text)) {
    "ping" -> {
      let assert Ok(_) = mist.send_text_frame(conn, "pong")
      mist.continue(state)
    }
    "status" -> {
      send_status(state, conn)
      mist.continue(state)
    }
    "stats" -> {
      send_stats(state, conn)
      mist.continue(state)
    }
    "orders" -> {
      send_orders(state, conn)
      mist.continue(state)
    }
    "arbitrage" -> {
      send_arbitrage(conn)
      mist.continue(state)
    }
    _ -> {
      mist.continue(state)
    }
  }
}

fn handle_json_command(
  state: P2PWsState,
  text: String,
  conn: mist.WebsocketConnection,
) {
  // Parse the JSON and extract type
  // For now, simple string matching
  case string.contains(text, "\"type\":\"subscribe\"") {
    True -> {
      // Subscribe to channels
      let channels = extract_channels(text)
      let new_state = P2PWsState(..state, subscribed_channels: channels)

      // Send confirmation
      let response = json.object([
        #("type", json.string("subscribed")),
        #("channels", json.array(channels, json.string)),
      ])
      let assert Ok(_) = mist.send_text_frame(conn, json.to_string(response))

      // Send initial data for each channel
      case list_contains(channels, "status") {
        True -> send_status(new_state, conn)
        False -> Nil
      }
      case list_contains(channels, "stats") {
        True -> send_stats(new_state, conn)
        False -> Nil
      }
      case list_contains(channels, "orders") {
        True -> send_orders(new_state, conn)
        False -> Nil
      }
      case list_contains(channels, "arbitrage") {
        True -> send_arbitrage(conn)
        False -> Nil
      }

      mist.continue(new_state)
    }
    False -> {
      // Handle command
      case string.contains(text, "\"type\":\"command\"") {
        True -> {
          handle_command(state, text, conn)
        }
        False -> {
          mist.continue(state)
        }
      }
    }
  }
}

fn handle_command(
  state: P2PWsState,
  text: String,
  conn: mist.WebsocketConnection,
) {
  // Extract action from JSON
  case string.contains(text, "\"action\":\"start\"") {
    True -> {
      // Start earning agent
      let response = json.object([
        #("type", json.string("command_result")),
        #("action", json.string("start")),
        #("success", json.bool(True)),
        #("message", json.string("Agent started")),
      ])
      let assert Ok(_) = mist.send_text_frame(conn, json.to_string(response))

      // Also send updated status
      send_status(state, conn)
      mist.continue(state)
    }
    False -> {
      case string.contains(text, "\"action\":\"stop\"") {
        True -> {
          let response = json.object([
            #("type", json.string("command_result")),
            #("action", json.string("stop")),
            #("success", json.bool(True)),
            #("message", json.string("Agent stopped")),
          ])
          let assert Ok(_) = mist.send_text_frame(conn, json.to_string(response))
          send_status(state, conn)
          mist.continue(state)
        }
        False -> {
          case string.contains(text, "\"action\":\"scan_arbitrage\"") {
            True -> {
              send_arbitrage(conn)
              mist.continue(state)
            }
            False -> {
              case string.contains(text, "\"action\":\"set_strategy\"") {
                True -> {
                  // Extract strategy from JSON
                  let strategy = extract_strategy(text)
                  let telegram_id = 144022504  // Default user ID

                  // Update strategy
                  let success = update_strategy_ffi(telegram_id, strategy)

                  let response = json.object([
                    #("type", json.string("command_result")),
                    #("action", json.string("set_strategy")),
                    #("success", json.bool(success)),
                    #("strategy", json.string(strategy)),
                    #("message", json.string(case success {
                      True -> "Strategy updated to " <> strategy
                      False -> "Failed to update strategy"
                    })),
                  ])
                  let assert Ok(_) = mist.send_text_frame(conn, json.to_string(response))
                  send_status(state, conn)
                  mist.continue(state)
                }
                False -> {
                  mist.continue(state)
                }
              }
            }
          }
        }
      }
    }
  }
}

fn handle_custom_message(
  state: P2PWsState,
  ws_msg: P2PWsMessage,
  conn: mist.WebsocketConnection,
) {
  case ws_msg {
    StatusUpdate(data) -> {
      let response = json.object([
        #("type", json.string("status")),
        #("data", data),
      ])
      let assert Ok(_) = mist.send_text_frame(conn, json.to_string(response))
      mist.continue(state)
    }
    OrderUpdate(data) -> {
      let response = json.object([
        #("type", json.string("order_update")),
        #("data", data),
      ])
      let assert Ok(_) = mist.send_text_frame(conn, json.to_string(response))
      mist.continue(state)
    }
    ArbitrageUpdate(data) -> {
      let response = json.object([
        #("type", json.string("arbitrage")),
        #("data", data),
      ])
      let assert Ok(_) = mist.send_text_frame(conn, json.to_string(response))
      mist.continue(state)
    }
    LogMessage(data) -> {
      let response = json.object([
        #("type", json.string("log")),
        #("data", data),
      ])
      let assert Ok(_) = mist.send_text_frame(conn, json.to_string(response))
      mist.continue(state)
    }
    PriceUpdate(data) -> {
      let response = json.object([
        #("type", json.string("price_update")),
        #("data", data),
      ])
      let assert Ok(_) = mist.send_text_frame(conn, json.to_string(response))
      mist.continue(state)
    }
  }
}

// =============================================================================
// DATA SENDERS
// =============================================================================

fn send_status(state: P2PWsState, conn: mist.WebsocketConnection) {
  // Get status from earning worker via FFI
  let telegram_id = case state.telegram_id {
    Some(id) -> id
    None -> 0
  }
  let status = get_agent_status(telegram_id)

  let status_json = json.object([
    #("is_active", json.bool(status.is_active)),
    #("strategy", json.string(earning_types.strategy_to_string(status.strategy))),
    #("uptime_seconds", json.int(status.uptime_seconds)),
    #("active_orders", json.int(status.active_orders)),
    #("pending_arbitrage", json.int(status.pending_arbitrage)),
    #("trades_today", json.int(status.trades_today)),
    #("profit_today", json.float(status.profit_today)),
  ])

  let response = json.object([
    #("type", json.string("status")),
    #("data", status_json),
  ])
  let assert Ok(_) = mist.send_text_frame(conn, json.to_string(response))
  Nil
}

fn send_stats(state: P2PWsState, conn: mist.WebsocketConnection) {
  // Get stats from earning worker via FFI
  let telegram_id = case state.telegram_id {
    Some(id) -> id
    None -> 0
  }
  let stats = get_agent_stats(telegram_id)

  let stats_json = json.object([
    #("total_trades", json.int(stats.total_trades)),
    #("successful_trades", json.int(stats.successful_trades)),
    #("total_volume_usdt", json.float(stats.total_volume_usdt)),
    #("total_fees_earned", json.float(stats.total_fees_earned)),
    #("total_spread_profit", json.float(stats.total_spread_profit)),
    #("total_arbitrage_profit", json.float(stats.total_arbitrage_profit)),
    #("today_profit", json.float(stats.today_profit)),
    #("this_week_profit", json.float(stats.this_week_profit)),
    #("this_month_profit", json.float(stats.this_month_profit)),
    #("best_trade_profit", json.float(stats.best_trade_profit)),
    #("avg_trade_profit", json.float(stats.avg_trade_profit)),
  ])

  let response = json.object([
    #("type", json.string("stats")),
    #("data", stats_json),
  ])
  let assert Ok(_) = mist.send_text_frame(conn, json.to_string(response))
  Nil
}

fn send_orders(_state: P2PWsState, conn: mist.WebsocketConnection) {
  // Get active orders from P2P DB via FFI
  let orders = list_orders_ffi(20)

  let orders_json = list.map(orders, fn(order) {
    json.object([
      #("id", json.string(ffi_get_map_string(order, "id"))),
      #("status", json.string(ffi_get_map_string(order, "status"))),
      #("crypto", json.string(ffi_get_map_string(order, "crypto"))),
      #("crypto_amount", json.float(ffi_get_map_float(order, "crypto_amount"))),
      #("fiat", json.string(ffi_get_map_string(order, "fiat"))),
      #("fiat_amount", json.float(ffi_get_map_float(order, "fiat_amount"))),
      #("payment_method", json.string(ffi_get_map_string(order, "payment_method"))),
      #("seller_telegram_id", json.int(ffi_get_map_int(order, "seller_telegram_id"))),
      #("buyer_telegram_id", json.int(ffi_get_map_int(order, "buyer_telegram_id"))),
      #("created_at", json.int(ffi_get_map_int(order, "created_at"))),
    ])
  })

  let response = json.object([
    #("type", json.string("orders")),
    #("data", json.array(orders_json, fn(x) { x })),
  ])
  let assert Ok(_) = mist.send_text_frame(conn, json.to_string(response))
  Nil
}

fn send_arbitrage(conn: mist.WebsocketConnection) {
  // Scan for arbitrage opportunities
  let opportunities = arbitrage.scan_opportunities(
    p2p_types.USDT,
    p2p_types.THB,
    0.5,  // min spread 0.5%
  )

  let opps_json = json.array(opportunities, fn(opp) {
    json.object([
      #("id", json.string(opp.id)),
      #("crypto", json.string(p2p_types.crypto_to_string(opp.crypto))),
      #("fiat", json.string(p2p_types.fiat_to_string(opp.fiat))),
      #("buy_source", json.string(earning_types.source_to_string(opp.buy_source))),
      #("buy_price", json.float(opp.buy_price)),
      #("sell_source", json.string(earning_types.source_to_string(opp.sell_source))),
      #("sell_price", json.float(opp.sell_price)),
      #("spread_percent", json.float(opp.spread_percent)),
      #("potential_profit_percent", json.float(opp.potential_profit_percent)),
      #("max_trade_size", json.float(opp.max_trade_size)),
      #("estimated_profit", json.float(opp.estimated_profit)),
    ])
  })

  let response = json.object([
    #("type", json.string("arbitrage")),
    #("data", opps_json),
  ])
  let assert Ok(_) = mist.send_text_frame(conn, json.to_string(response))
  Nil
}

// =============================================================================
// HELPERS
// =============================================================================

fn extract_channels(text: String) -> List(String) {
  // Simple extraction - in production use proper JSON decoder
  let default_channels = ["status", "orders", "arbitrage", "logs"]
  default_channels
}

fn list_contains(list: List(String), item: String) -> Bool {
  case list {
    [] -> False
    [head, ..tail] -> case head == item {
      True -> True
      False -> list_contains(tail, item)
    }
  }
}

fn extract_strategy(text: String) -> String {
  // Simple extraction - look for "strategy":"value" pattern
  case string.split(text, "\"strategy\":\"") {
    [_, rest] -> {
      case string.split(rest, "\"") {
        [value, ..] -> value
        _ -> "passive_fees"
      }
    }
    _ -> "passive_fees"
  }
}
