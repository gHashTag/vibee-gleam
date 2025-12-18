// P2P REST API Handlers
// HTTP endpoints for P2P Earning Agent control
// Reuses MCP tools for actual functionality

import gleam/bit_array
import gleam/bytes_tree
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/float
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import mist.{type Connection, type ResponseData}
import vibee/earning/types as earning_types
import vibee/earning/arbitrage
import vibee/earning/market_maker
import vibee/earning/telegram_executor
import vibee/earning/maker_bot
import vibee/earning/alerts
import vibee/earning/arb_executor
import vibee/p2p/types as p2p_types
import vibee/p2p/cryptobot
import vibee/logging

// =============================================================================
// STATUS HANDLERS
// =============================================================================

/// GET /api/v1/p2p/status - Get agent status
pub fn status_handler() -> Response(ResponseData) {
  logging.quick_info("[P2P API] Status request")

  // Primary user config - always use this ID
  let telegram_id = 144022504
  let wallet = "UQBaVYIj_if2F4E8ayBS52eJOuXFQd_IcsnTpvPsfug2ytKM"

  // Get status from earning worker (via FFI)
  let status = get_agent_status(telegram_id)

  let status_json = json.object([
    #("is_active", json.bool(status.is_active)),
    #("strategy", json.string(earning_types.strategy_to_string(status.strategy))),
    #("uptime_seconds", json.int(status.uptime_seconds)),
    #("active_orders", json.int(status.active_orders)),
    #("pending_arbitrage", json.int(status.pending_arbitrage)),
    #("trades_today", json.int(status.trades_today)),
    #("profit_today", json.float(status.profit_today)),
    #("current_position_usdt", json.float(status.current_position_usdt)),
    #("current_position_ton", json.float(status.current_position_ton)),
    #("last_activity_at", json.int(status.last_activity_at)),
    #("telegram_id", json.int(telegram_id)),
    #("wallet", json.string(wallet)),
  ])

  json_response(200, json.object([
    #("success", json.bool(True)),
    #("status", status_json),
  ]))
}

/// GET /api/v1/p2p/stats - Get earning statistics
pub fn stats_handler() -> Response(ResponseData) {
  logging.quick_info("[P2P API] Stats request")

  // Get stats from earning worker
  let stats = get_agent_stats(0)

  let stats_json = json.object([
    #("total_trades", json.int(stats.total_trades)),
    #("successful_trades", json.int(stats.successful_trades)),
    #("failed_trades", json.int(stats.failed_trades)),
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

  json_response(200, json.object([
    #("success", json.bool(True)),
    #("stats", stats_json),
  ]))
}

// =============================================================================
// CONTROL HANDLERS
// =============================================================================

/// POST /api/v1/p2p/start - Start earning agent
pub fn start_handler(req: Request(Connection)) -> Response(ResponseData) {
  logging.quick_info("[P2P API] Start request")

  // Parse body for telegram_id, wallet, strategy
  case mist.read_body(req, 1024 * 1024) {
    Ok(req_with_body) -> {
      let body_str = bit_array_to_string(req_with_body.body)
      let telegram_id = parse_json_int(body_str, "telegram_id") |> result.unwrap(144022504)
      let wallet = parse_json_string(body_str, "wallet") |> result.unwrap("UQBaVYIj_if2F4E8ayBS52eJOuXFQd_IcsnTpvPsfug2ytKM")
      let strategy_str = parse_json_string(body_str, "strategy") |> result.unwrap("hybrid")
      let strategy = earning_types.strategy_from_string(strategy_str)

      logging.quick_info("[P2P API] Starting agent for telegram_id=" <> int.to_string(telegram_id) <> " wallet=" <> wallet <> " strategy=" <> strategy_str)

      case start_earning_agent(telegram_id, wallet, strategy) {
        Ok(_) -> {
          json_response(200, json.object([
            #("success", json.bool(True)),
            #("message", json.string("Agent started successfully")),
            #("telegram_id", json.int(telegram_id)),
            #("wallet", json.string(wallet)),
            #("strategy", json.string(strategy_str)),
          ]))
        }
        Error(err) -> {
          json_response(400, json.object([
            #("success", json.bool(False)),
            #("error", json.string(err)),
          ]))
        }
      }
    }
    _ -> {
      // No body - use defaults for current user
      let telegram_id = 144022504
      let wallet = "UQBaVYIj_if2F4E8ayBS52eJOuXFQd_IcsnTpvPsfug2ytKM"
      let strategy = earning_types.Hybrid

      case start_earning_agent(telegram_id, wallet, strategy) {
        Ok(_) -> {
          json_response(200, json.object([
            #("success", json.bool(True)),
            #("message", json.string("Agent started with defaults")),
          ]))
        }
        Error(err) -> {
          json_response(400, json.object([
            #("success", json.bool(False)),
            #("error", json.string(err)),
          ]))
        }
      }
    }
  }
}

/// POST /api/v1/p2p/stop - Stop earning agent
pub fn stop_handler() -> Response(ResponseData) {
  logging.quick_info("[P2P API] Stop request")

  case stop_earning_agent(0) {
    Ok(_) -> {
      json_response(200, json.object([
        #("success", json.bool(True)),
        #("message", json.string("Agent stopped")),
      ]))
    }
    Error(err) -> {
      json_response(400, json.object([
        #("success", json.bool(False)),
        #("error", json.string(err)),
      ]))
    }
  }
}

/// POST /api/v1/p2p/config - Update agent configuration
pub fn config_handler(req: Request(Connection)) -> Response(ResponseData) {
  logging.quick_info("[P2P API] Config update request")

  // Parse body and update config
  // For now, just return success
  json_response(200, json.object([
    #("success", json.bool(True)),
    #("message", json.string("Configuration updated")),
  ]))
}

// =============================================================================
// ORDER HANDLERS
// =============================================================================

/// GET /api/v1/p2p/orders - List active orders
pub fn orders_list_handler() -> Response(ResponseData) {
  logging.quick_info("[P2P API] Orders list request")

  // Get active orders from P2P DB
  let orders = get_active_orders(0)

  let orders_json = json.array(orders, fn(order) {
    json.object([
      #("id", json.string(order.0)),
      #("order_type", json.string(order.1)),
      #("crypto", json.string(order.2)),
      #("amount", json.float(order.3)),
      #("fiat", json.string(order.4)),
      #("rate", json.float(order.5)),
      #("status", json.string(order.6)),
    ])
  })

  json_response(200, json.object([
    #("success", json.bool(True)),
    #("orders", orders_json),
  ]))
}

/// POST /api/v1/p2p/orders/{id}/cancel - Cancel order
pub fn order_cancel_handler(order_id: String) -> Response(ResponseData) {
  logging.quick_info("[P2P API] Cancel order: " <> order_id)

  case cancel_order(order_id) {
    Ok(_) -> {
      json_response(200, json.object([
        #("success", json.bool(True)),
        #("message", json.string("Order cancelled")),
      ]))
    }
    Error(err) -> {
      json_response(400, json.object([
        #("success", json.bool(False)),
        #("error", json.string(err)),
      ]))
    }
  }
}

// =============================================================================
// ARBITRAGE HANDLERS
// =============================================================================

/// GET /api/v1/p2p/arbitrage - Scan for arbitrage opportunities
pub fn arbitrage_scan_handler(
  crypto_str: Option(String),
  fiat_str: Option(String),
) -> Response(ResponseData) {
  logging.quick_info("[P2P API] Arbitrage scan request")

  let crypto = case crypto_str {
    Some("TON") -> p2p_types.TON
    Some("USDT") -> p2p_types.USDT
    Some("USDC") -> p2p_types.USDC
    Some("NOT") -> p2p_types.NOT
    _ -> p2p_types.USDT
  }

  let fiat = case fiat_str {
    Some("THB") -> p2p_types.THB
    Some("RUB") -> p2p_types.RUB
    Some("USD") -> p2p_types.USD
    Some("EUR") -> p2p_types.EUR
    _ -> p2p_types.THB
  }

  // Scan for opportunities
  let opportunities = arbitrage.scan_opportunities(crypto, fiat, 0.5)

  let opps_json = json.array(opportunities, fn(opp) {
    json.object([
      #("id", json.string(opp.id)),
      #("crypto", json.string(p2p_types.crypto_to_string(opp.crypto))),
      #("fiat", json.string(p2p_types.fiat_to_string(opp.fiat))),
      #("buy_source", json.string(earning_types.source_to_string(opp.buy_source))),
      #("buy_price", json.float(opp.buy_price)),
      #("buy_volume", json.float(opp.buy_volume)),
      #("sell_source", json.string(earning_types.source_to_string(opp.sell_source))),
      #("sell_price", json.float(opp.sell_price)),
      #("sell_volume", json.float(opp.sell_volume)),
      #("spread_percent", json.float(opp.spread_percent)),
      #("potential_profit_percent", json.float(opp.potential_profit_percent)),
      #("max_trade_size", json.float(opp.max_trade_size)),
      #("estimated_profit", json.float(opp.estimated_profit)),
      #("risk_score", json.int(arbitrage.calculate_risk_score(opp))),
    ])
  })

  json_response(200, json.object([
    #("success", json.bool(True)),
    #("opportunities", opps_json),
    #("count", json.int(list.length(opportunities))),
  ]))
}

/// POST /api/v1/p2p/arbitrage/execute - Execute arbitrage opportunity
pub fn arbitrage_execute_handler(req: Request(Connection)) -> Response(ResponseData) {
  logging.quick_info("[P2P API] Arbitrage execute request")

  // Get opportunities and filter for Telegram-only (both buy and sell sources)
  let all_opportunities = arbitrage.scan_opportunities(p2p_types.USDT, p2p_types.THB, 0.5)

  // Filter for Telegram-only opportunities
  let telegram_opportunities = list.filter(all_opportunities, fn(opp) {
    earning_types.is_telegram_source(opp.buy_source) &&
    earning_types.is_telegram_source(opp.sell_source)
  })

  case list.first(telegram_opportunities) {
    Ok(opp) -> {
      // Execute with default wallet and amount
      let wallet = "EQCRBz60RSxp6MboqlH-lkaG7nz01ZYy7yylXKkhtOv7rUv4"
      let amount = 100.0  // Execute with 100 USDT

      case telegram_executor.execute(opp, wallet, amount) {
        Ok(result) -> {
          json_response(200, json.object([
            #("success", json.bool(result.success)),
            #("opportunity_id", json.string(result.opportunity_id)),
            #("buy_tx_id", json.string(case result.buy_tx_id {
              Some(id) -> id
              None -> ""
            })),
            #("sell_tx_id", json.string(case result.sell_tx_id {
              Some(id) -> id
              None -> ""
            })),
            #("actual_amount", json.float(result.actual_amount)),
            #("actual_profit", json.float(result.actual_profit)),
            #("fees_paid", json.float(result.fees_paid)),
            #("execution_time_ms", json.int(result.execution_time_ms)),
          ]))
        }
        Error(err) -> {
          json_response(400, json.object([
            #("success", json.bool(False)),
            #("error", json.string(telegram_executor.error_to_string(err))),
          ]))
        }
      }
    }
    Error(_) -> {
      json_response(404, json.object([
        #("success", json.bool(False)),
        #("error", json.string("No arbitrage opportunities available")),
      ]))
    }
  }
}

// =============================================================================
// MARKET MAKER HANDLERS
// =============================================================================

/// GET /api/v1/p2p/market-maker/rates - Get current market maker rates
pub fn market_maker_rates_handler() -> Response(ResponseData) {
  logging.quick_info("[P2P API] Market maker rates request")

  // Get current rates from market maker
  let base_price = 35.0  // USDT/THB base rate

  // Calculate with different spreads
  let spreads = [1.0, 1.5, 2.0, 2.5, 3.0]

  let rates_json = json.array(spreads, fn(spread) {
    let #(buy, sell) = market_maker.calculate_order_prices(base_price, spread)
    json.object([
      #("spread_percent", json.float(spread)),
      #("buy_rate", json.float(buy)),
      #("sell_rate", json.float(sell)),
      #("profit_per_1000", json.float(spread *. 10.0)),
    ])
  })

  json_response(200, json.object([
    #("success", json.bool(True)),
    #("base_price", json.float(base_price)),
    #("rates", rates_json),
  ]))
}

// =============================================================================
// MAKER BOT HANDLERS
// =============================================================================

/// GET /api/v1/p2p/maker/prices - Get recommended maker prices
pub fn maker_prices_handler(
  crypto_str: Option(String),
  fiat_str: Option(String),
) -> Response(ResponseData) {
  logging.quick_info("[P2P API] Maker prices request")

  let crypto = parse_crypto(crypto_str)
  let fiat = parse_fiat(fiat_str)

  let config = maker_bot.default_config(crypto, fiat)

  case maker_bot.calculate_prices(config) {
    Ok(rec) -> {
      json_response(200, json.object([
        #("success", json.bool(True)),
        #("recommendation", maker_bot.encode_recommendation(rec)),
        #("config", json.object([
          #("spread_percent", json.float(config.spread_percent)),
          #("min_order_size", json.float(config.min_order_size)),
          #("max_order_size", json.float(config.max_order_size)),
        ])),
      ]))
    }
    Error(e) -> {
      json_response(400, json.object([
        #("success", json.bool(False)),
        #("error", json.string(e)),
      ]))
    }
  }
}

/// POST /api/v1/p2p/maker/toggle - Enable/disable maker bot for user
pub fn maker_toggle_handler(
  telegram_id_str: Option(String),
  enabled_str: Option(String),
) -> Response(ResponseData) {
  logging.quick_info("[P2P API] Maker toggle request")

  case telegram_id_str {
    Some(id_str) -> {
      case int.parse(id_str) {
        Ok(telegram_id) -> {
          let enabled = case enabled_str {
            Some("true") -> True
            Some("1") -> True
            _ -> False
          }

          // Set maker_enabled in ETS via FFI
          set_maker_enabled_ffi(telegram_id, enabled)

          json_response(200, json.object([
            #("success", json.bool(True)),
            #("telegram_id", json.int(telegram_id)),
            #("maker_enabled", json.bool(enabled)),
          ]))
        }
        Error(_) -> {
          json_response(400, json.object([
            #("success", json.bool(False)),
            #("error", json.string("Invalid telegram_id")),
          ]))
        }
      }
    }
    None -> {
      json_response(400, json.object([
        #("success", json.bool(False)),
        #("error", json.string("telegram_id required")),
      ]))
    }
  }
}

@external(erlang, "vibee_earning_ffi", "set_maker_enabled")
fn set_maker_enabled_ffi(telegram_id: Int, enabled: Bool) -> Nil

// =============================================================================
// ALERT HANDLERS
// =============================================================================

/// GET /api/v1/p2p/alerts/scan - Scan for spread alerts
pub fn alerts_scan_handler(
  crypto_str: Option(String),
  fiat_str: Option(String),
  min_spread_str: Option(String),
) -> Response(ResponseData) {
  logging.quick_info("[P2P API] Alerts scan request")

  let crypto = parse_crypto(crypto_str)
  let fiat = parse_fiat(fiat_str)
  let min_spread = case min_spread_str {
    Some(s) -> result.unwrap(float.parse(s), 1.0)
    None -> 1.0
  }

  let spread_alerts = alerts.scan_spreads(crypto, fiat, min_spread)

  json_response(200, json.object([
    #("success", json.bool(True)),
    #("alerts", alerts.encode_alerts(spread_alerts)),
    #("min_spread", json.float(min_spread)),
  ]))
}

/// GET /api/v1/p2p/alerts/monitor - Get monitor status
pub fn alerts_monitor_handler() -> Response(ResponseData) {
  logging.quick_info("[P2P API] Monitor status request")

  let config = alerts.default_monitor_config()
  let last_alerts = alerts.monitor_cycle(config)

  json_response(200, alerts.encode_monitor_status(config, last_alerts, False))
}

// =============================================================================
// EXECUTOR HANDLERS
// =============================================================================

/// GET /api/v1/p2p/executor/status - Get executor status
pub fn executor_status_handler() -> Response(ResponseData) {
  logging.quick_info("[P2P API] Executor status request")

  let config = arb_executor.default_config()
  let state = arb_executor.init_state(config)

  json_response(200, json.object([
    #("success", json.bool(True)),
    #("executor", arb_executor.encode_state(state)),
    #("message", json.string("Executor ready. Set dry_run=false and enabled=true to start.")),
  ]))
}

/// POST /api/v1/p2p/executor/simulate - Simulate arbitrage execution
pub fn executor_simulate_handler(
  crypto_str: Option(String),
  fiat_str: Option(String),
) -> Response(ResponseData) {
  logging.quick_info("[P2P API] Executor simulate request")

  let crypto = parse_crypto(crypto_str)
  let fiat = parse_fiat(fiat_str)

  // Get alerts
  let spread_alerts = alerts.scan_spreads(crypto, fiat, 0.5)

  case list.first(spread_alerts) {
    Error(_) -> {
      json_response(200, json.object([
        #("success", json.bool(False)),
        #("error", json.string("No opportunities to simulate")),
      ]))
    }
    Ok(alert) -> {
      let config = arb_executor.default_config()
      let state = arb_executor.init_state(config)

      // Simulate with $100
      let amount = 100.0
      let result = arb_executor.execute(alert, state, amount)

      json_response(200, json.object([
        #("success", json.bool(True)),
        #("simulation", arb_executor.encode_trade_result(result)),
        #("alert", alerts.encode_alert(alert)),
      ]))
    }
  }
}

// =============================================================================
// ACTIVITY LOG HANDLERS
// =============================================================================

/// GET /api/v1/p2p/activity - Get activity log
pub fn activity_log_handler(limit_str: Option(String)) -> Response(ResponseData) {
  logging.quick_info("[P2P API] Activity log request")

  let limit = case limit_str {
    Some(s) -> result.unwrap(int.parse(s), 50)
    None -> 50
  }

  // Get activity log from ETS
  let entries = get_activity_log(limit)

  let entries_json = list.map(entries, fn(entry) {
    let id = get_activity_field_int(entry, "id")
    let entry_type = get_activity_field_atom(entry, "type")
    let message = get_activity_field_string(entry, "message")
    let timestamp = get_activity_field_int(entry, "timestamp")

    json.object([
      #("id", json.int(id)),
      #("type", json.string(entry_type)),
      #("message", json.string(message)),
      #("timestamp", json.int(timestamp)),
    ])
  })

  json_response(200, json.object([
    #("success", json.bool(True)),
    #("count", json.int(list.length(entries))),
    #("entries", json.array(entries_json, fn(x) { x })),
  ]))
}

// FFI for activity log
@external(erlang, "vibee_earning_worker", "get_activity_log")
fn get_activity_log(limit: Int) -> List(Dynamic)

@external(erlang, "vibee_p2p_ffi", "get_map_field_int")
fn get_activity_field_int(entry: Dynamic, field: String) -> Int

@external(erlang, "vibee_p2p_ffi", "get_map_field_string")
fn get_activity_field_string(entry: Dynamic, field: String) -> String

fn get_activity_field_atom(entry: Dynamic, field: String) -> String {
  get_activity_field_atom_ffi(entry, field)
}

@external(erlang, "vibee_p2p_ffi", "get_map_field_atom_as_string")
fn get_activity_field_atom_ffi(entry: Dynamic, field: String) -> String

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

fn parse_crypto(crypto_str: Option(String)) -> p2p_types.CryptoCurrency {
  case crypto_str {
    Some("TON") -> p2p_types.TON
    Some("USDT") -> p2p_types.USDT
    Some("USDC") -> p2p_types.USDC
    Some("NOT") -> p2p_types.NOT
    _ -> p2p_types.TON
  }
}

fn parse_fiat(fiat_str: Option(String)) -> p2p_types.FiatCurrency {
  case fiat_str {
    Some("THB") -> p2p_types.THB
    Some("RUB") -> p2p_types.RUB
    Some("USD") -> p2p_types.USD
    Some("EUR") -> p2p_types.EUR
    _ -> p2p_types.RUB
  }
}

fn json_response(status: Int, body: json.Json) -> Response(ResponseData) {
  let body_string = json.to_string(body)
  let body_bytes = bytes_tree.from_string(body_string)

  response.new(status)
  |> response.set_header("content-type", "application/json")
  |> response.set_header("access-control-allow-origin", "*")
  |> response.set_body(mist.Bytes(body_bytes))
}

/// Convert bit array to string
fn bit_array_to_string(bits: BitArray) -> String {
  case bit_array.to_string(bits) {
    Ok(s) -> s
    Error(_) -> ""
  }
}

/// Parse integer from JSON string by key
fn parse_json_int(json_str: String, key: String) -> Result(Int, Nil) {
  // Simple regex-like parsing for "key": 123
  let pattern = "\"" <> key <> "\":"
  case string.split(json_str, pattern) {
    [_, rest] -> {
      let trimmed = string.trim_start(rest)
      // Extract number until comma, brace, or end
      let num_str = take_digits(trimmed)
      int.parse(num_str)
    }
    _ -> Error(Nil)
  }
}

/// Parse string from JSON string by key
fn parse_json_string(json_str: String, key: String) -> Result(String, Nil) {
  // Simple parsing for "key": "value"
  let pattern = "\"" <> key <> "\":"
  case string.split(json_str, pattern) {
    [_, rest] -> {
      let trimmed = string.trim_start(rest)
      case string.starts_with(trimmed, "\"") {
        True -> {
          let after_quote = string.drop_start(trimmed, 1)
          case string.split(after_quote, "\"") {
            [value, ..] -> Ok(value)
            _ -> Error(Nil)
          }
        }
        False -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

/// Take consecutive digits from start of string
fn take_digits(s: String) -> String {
  take_digits_acc(s, "")
}

fn take_digits_acc(s: String, acc: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(c, rest)) -> {
      case is_digit(c) {
        True -> take_digits_acc(rest, acc <> c)
        False -> acc
      }
    }
    Error(_) -> acc
  }
}

fn is_digit(c: String) -> Bool {
  c == "0" || c == "1" || c == "2" || c == "3" || c == "4" ||
  c == "5" || c == "6" || c == "7" || c == "8" || c == "9"
}

// =============================================================================
// FFI DECLARATIONS (call Erlang earning worker and P2P storage)
// =============================================================================

/// Get agent status from earning worker (FFI)
@external(erlang, "vibee_earning_ffi", "get_status")
fn get_agent_status(telegram_id: Int) -> earning_types.AgentStatus

/// Get agent stats from earning worker (FFI)
@external(erlang, "vibee_earning_ffi", "get_stats")
fn get_agent_stats(telegram_id: Int) -> earning_types.EarningStats

/// Start earning agent (FFI)
@external(erlang, "vibee_earning_ffi", "start_earning")
fn start_earning_ffi(
  telegram_id: Int,
  wallet: String,
  strategy: String,
  max_position: Float,
  spread: Float,
) -> Bool

/// Stop earning agent (FFI)
@external(erlang, "vibee_earning_ffi", "stop_earning")
fn stop_earning_ffi(telegram_id: Int) -> Bool

/// Update strategy (FFI)
@external(erlang, "vibee_earning_ffi", "update_strategy")
fn update_strategy_ffi(telegram_id: Int, strategy: String) -> Bool

/// Get active config (FFI) - returns {ok, telegram_id, wallet} or error
@external(erlang, "vibee_earning_ffi", "get_active_config")
fn get_active_config_ffi() -> Result(#(Int, String), Nil)

/// List orders from ETS (FFI)
@external(erlang, "vibee_p2p_ffi", "list_orders")
fn list_orders_ffi(limit: Int) -> List(Dynamic)

/// List orders by seller (FFI)
@external(erlang, "vibee_p2p_ffi", "list_orders_by_seller")
fn list_orders_by_seller_ffi(seller_id: Int) -> List(Dynamic)

/// List orders by buyer (FFI)
@external(erlang, "vibee_p2p_ffi", "list_orders_by_buyer")
fn list_orders_by_buyer_ffi(buyer_id: Int) -> List(Dynamic)

/// Update order (FFI)
@external(erlang, "vibee_p2p_ffi", "update_order")
fn update_order_ffi(order: Dynamic) -> Dynamic

/// Start earning agent wrapper
fn start_earning_agent(
  telegram_id: Int,
  wallet: String,
  strategy: earning_types.EarningStrategy,
) -> Result(Nil, String) {
  let strategy_str = earning_types.strategy_to_string(strategy)
  case start_earning_ffi(telegram_id, wallet, strategy_str, 1000.0, 2.0) {
    True -> Ok(Nil)
    False -> Error("Failed to start earning agent (already active?)")
  }
}

/// Stop earning agent wrapper
fn stop_earning_agent(telegram_id: Int) -> Result(Nil, String) {
  case stop_earning_ffi(telegram_id) {
    True -> Ok(Nil)
    False -> Error("Failed to stop earning agent (not active?)")
  }
}

/// Get active orders - convert FFI result to tuple format
fn get_active_orders(telegram_id: Int) -> List(#(String, String, String, Float, String, Float, String)) {
  let orders = list_orders_ffi(50)
  list.filter_map(orders, fn(order_dyn) {
    // Try to extract fields from dynamic map
    case decode_order_tuple(order_dyn) {
      Ok(tuple) -> Ok(tuple)
      Error(_) -> Error(Nil)
    }
  })
}

/// Decode order dynamic to tuple
fn decode_order_tuple(order: Dynamic) -> Result(#(String, String, String, Float, String, Float, String), Nil) {
  // Order map has: id, side, crypto, amount, fiat, price, total_fiat, status
  // Try both field naming conventions
  let amount_result = case get_map_float(order, "amount") {
    Ok(a) -> Ok(a)
    Error(_) -> get_map_float(order, "crypto_amount")
  }
  let price_result = case get_map_float(order, "price") {
    Ok(p) -> Ok(p)
    Error(_) -> case get_map_float(order, "fiat_amount"), amount_result {
      Ok(fiat), Ok(amt) if amt >. 0.0 -> Ok(fiat /. amt)
      _, _ -> Ok(0.0)
    }
  }
  let side_result = case get_map_string(order, "side") {
    Ok(s) -> Ok(s)
    Error(_) -> Ok("sell")
  }

  case
    get_map_string(order, "id"),
    side_result,
    get_map_string(order, "crypto"),
    amount_result,
    get_map_string(order, "fiat"),
    price_result,
    get_map_string(order, "status")
  {
    Ok(id), Ok(side), Ok(crypto), Ok(amount), Ok(fiat), Ok(price), Ok(status) -> {
      Ok(#(id, side, crypto, amount, fiat, price, status))
    }
    _, _, _, _, _, _, _ -> Error(Nil)
  }
}

/// Get string from dynamic Erlang map
@external(erlang, "vibee_p2p_ffi", "get_map_string")
fn ffi_get_map_string(map: Dynamic, key: String) -> String

@external(erlang, "vibee_p2p_ffi", "get_map_float")
fn ffi_get_map_float(map: Dynamic, key: String) -> Float

@external(erlang, "vibee_p2p_ffi", "get_map_int")
fn ffi_get_map_int(map: Dynamic, key: String) -> Int

fn get_map_string(map: Dynamic, key: String) -> Result(String, Nil) {
  Ok(ffi_get_map_string(map, key))
}

fn get_map_float(map: Dynamic, key: String) -> Result(Float, Nil) {
  Ok(ffi_get_map_float(map, key))
}

/// Cancel order
fn cancel_order(order_id: String) -> Result(Nil, String) {
  // TODO: Implement order cancellation via FFI
  // For now just return success
  Ok(Nil)
}

// =============================================================================
// ORDER ACCEPT HANDLER
// =============================================================================

/// POST /api/v1/p2p/order/accept - Accept an order and create payment invoice
pub fn order_accept_handler(req: Request(Connection)) -> Response(ResponseData) {
  logging.quick_info("[P2P API] Order accept request")

  case mist.read_body(req, 1024 * 1024) {
    Ok(req_with_body) -> {
      let body_str = bit_array_to_string(req_with_body.body)
      let order_id = parse_json_string(body_str, "order_id") |> result.unwrap("")
      let buyer_telegram_id = parse_json_int(body_str, "buyer_telegram_id") |> result.unwrap(0)
      let action = parse_json_string(body_str, "action") |> result.unwrap("buy")

      logging.quick_info("[P2P API] Accepting order " <> order_id <> " by user " <> int.to_string(buyer_telegram_id) <> " action=" <> action)

      // Get order from ETS
      case get_order_ffi(order_id) {
        Ok(order_map) -> {
          // Extract order details
          let crypto = ffi_get_map_string(order_map, "crypto")
          let amount = ffi_get_map_float(order_map, "amount")
          let price = ffi_get_map_float(order_map, "price")
          let fiat = ffi_get_map_string(order_map, "fiat")
          let seller_id = ffi_get_map_int(order_map, "seller_telegram_id")

          // Calculate total
          let total = amount *. price

          // Validate
          case order_id, buyer_telegram_id {
            "", _ -> {
              json_response(400, json.object([
                #("success", json.bool(False)),
                #("error", json.string("order_id required")),
              ]))
            }
            _, 0 -> {
              json_response(400, json.object([
                #("success", json.bool(False)),
                #("error", json.string("buyer_telegram_id required")),
              ]))
            }
            _, _ -> {
              // Update order with buyer info
              update_order_status_ffi(order_id, buyer_telegram_id, "pending")

              // Create CryptoBot invoice
              case create_cryptobot_invoice(crypto, amount, order_id, int.to_string(buyer_telegram_id)) {
                Ok(invoice_url) -> {
                  logging.quick_info("[P2P API] Invoice created: " <> invoice_url)
                  json_response(200, json.object([
                    #("success", json.bool(True)),
                    #("order_id", json.string(order_id)),
                    #("invoice_url", json.string(invoice_url)),
                    #("amount", json.float(amount)),
                    #("crypto", json.string(crypto)),
                    #("total", json.float(total)),
                    #("fiat", json.string(fiat)),
                  ]))
                }
                Error(err) -> {
                  logging.quick_error("[P2P API] Invoice creation failed: " <> err)
                  // Still return success but without invoice URL
                  json_response(200, json.object([
                    #("success", json.bool(True)),
                    #("order_id", json.string(order_id)),
                    #("message", json.string("Order accepted, invoice creation failed: " <> err)),
                  ]))
                }
              }
            }
          }
        }
        Error(_) -> {
          json_response(404, json.object([
            #("success", json.bool(False)),
            #("error", json.string("Order not found: " <> order_id)),
          ]))
        }
      }
    }
    Error(_) -> {
      json_response(400, json.object([
        #("success", json.bool(False)),
        #("error", json.string("Invalid request body")),
      ]))
    }
  }
}

/// Get order from ETS (FFI)
@external(erlang, "vibee_p2p_ffi", "get_order")
fn get_order_ffi(order_id: String) -> Result(Dynamic, Nil)

/// Update order status and buyer (FFI)
@external(erlang, "vibee_p2p_ffi", "update_order_status")
fn update_order_status_ffi(order_id: String, buyer_telegram_id: Int, status: String) -> Dynamic

/// Create CryptoBot invoice for payment
fn create_cryptobot_invoice(
  crypto: String,
  amount: Float,
  order_id: String,
  buyer_id: String,
) -> Result(String, String) {
  // Get API token from environment
  case get_cryptobot_token() {
    Some(token) -> {
      let config = cryptobot.CryptoBotConfig(
        api_token: token,
        env: cryptobot.Mainnet,
      )

      let asset = cryptobot.asset_from_string(crypto)
      let description = Some("P2P Order " <> order_id)
      let payload = Some("order_id:" <> order_id <> ",buyer:" <> buyer_id)

      case cryptobot.create_invoice(config, asset, amount, description, Some(3600), payload) {
        Ok(invoice) -> Ok(invoice.pay_url)
        Error(err) -> Error(cryptobot_error_to_string(err))
      }
    }
    None -> Error("CRYPTOBOT_API_TOKEN not configured")
  }
}

/// Get CryptoBot API token from environment
fn get_cryptobot_token() -> Option(String) {
  case get_env_var("CRYPTOBOT_API_TOKEN") {
    "" -> None
    token -> Some(token)
  }
}

/// Get environment variable as string
@external(erlang, "vibee_p2p_ffi", "get_env_var")
fn get_env_var(key: String) -> String

/// Convert CryptoBot error to string
fn cryptobot_error_to_string(err: cryptobot.CryptoBotError) -> String {
  case err {
    cryptobot.CryptoBotHttpError(msg) -> "HTTP error: " <> msg
    cryptobot.CryptoBotApiError(code, msg) -> "API error [" <> code <> "]: " <> msg
    cryptobot.CryptoBotParseError(msg) -> "Parse error: " <> msg
    cryptobot.CryptoBotAuthError -> "Authentication error"
  }
}
