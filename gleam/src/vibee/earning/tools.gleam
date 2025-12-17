// VIBEE P2P Earning MCP Tools
// MCP tools for automated P2P trading and earning

import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/int
import gleam/float
import gleam/string
import gleam/list
import gleam/dynamic.{type Dynamic}
import vibee/mcp/protocol
import vibee/mcp/types.{type Tool, type ToolResult, Tool}
import vibee/earning/types as earning_types
import vibee/earning/arbitrage
import vibee/p2p/types as p2p_types

// =============================================================================
// TOOL DEFINITIONS
// =============================================================================

/// Get all earning tools
pub fn get_tools() -> List(Tool) {
  [
    earning_start_tool(),
    earning_stop_tool(),
    earning_status_tool(),
    earning_config_tool(),
    earning_history_tool(),
    arbitrage_scan_tool(),
    credentials_set_tool(),
    arbitrage_execute_tool(),
  ]
}

/// Start automated earning tool
pub fn earning_start_tool() -> Tool {
  Tool(
    name: "earning_start",
    description: "Start automated P2P trading with specified strategy. Strategies: passive_fees (0.5% platform fee), market_making (buy/sell spread), arbitrage (cross-platform), hybrid (all combined)",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Your Telegram ID")),
        ])),
        #("wallet", json.object([
          #("type", json.string("string")),
          #("description", json.string("Your TON wallet address")),
        ])),
        #("strategy", json.object([
          #("type", json.string("string")),
          #("enum", json.array([
            json.string("passive_fees"),
            json.string("market_making"),
            json.string("arbitrage"),
            json.string("hybrid"),
          ], fn(x) { x })),
          #("description", json.string("Trading strategy to use")),
        ])),
        #("max_position_usdt", json.object([
          #("type", json.string("number")),
          #("description", json.string("Maximum position size in USDT (default: 1000)")),
        ])),
        #("spread_percent", json.object([
          #("type", json.string("number")),
          #("description", json.string("Spread percentage for market making (default: 2%)")),
        ])),
      ])),
      #("required", json.array([
        json.string("telegram_id"),
        json.string("wallet"),
        json.string("strategy"),
      ], fn(x) { x })),
    ]),
  )
}

/// Stop automated earning tool
pub fn earning_stop_tool() -> Tool {
  Tool(
    name: "earning_stop",
    description: "Stop automated P2P trading and close all positions",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Your Telegram ID")),
        ])),
      ])),
      #("required", json.array([json.string("telegram_id")], fn(x) { x })),
    ]),
  )
}

/// Get earning status tool
pub fn earning_status_tool() -> Tool {
  Tool(
    name: "earning_status",
    description: "Get current status and statistics of automated trading",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Your Telegram ID")),
        ])),
      ])),
      #("required", json.array([json.string("telegram_id")], fn(x) { x })),
    ]),
  )
}

/// Configure earning settings tool
pub fn earning_config_tool() -> Tool {
  Tool(
    name: "earning_config",
    description: "Configure automated trading settings: position limits, spreads, risk management",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Your Telegram ID")),
        ])),
        #("strategy", json.object([
          #("type", json.string("string")),
          #("enum", json.array([
            json.string("passive_fees"),
            json.string("market_making"),
            json.string("arbitrage"),
            json.string("hybrid"),
          ], fn(x) { x })),
        ])),
        #("max_position_usdt", json.object([
          #("type", json.string("number")),
          #("description", json.string("Maximum position size in USDT")),
        ])),
        #("spread_percent", json.object([
          #("type", json.string("number")),
          #("description", json.string("Spread percentage for market making")),
        ])),
        #("daily_loss_limit", json.object([
          #("type", json.string("number")),
          #("description", json.string("Maximum daily loss in USDT")),
        ])),
        #("stop_loss_percent", json.object([
          #("type", json.string("number")),
          #("description", json.string("Stop loss percentage")),
        ])),
        #("enabled_crypto", json.object([
          #("type", json.string("array")),
          #("items", json.object([#("type", json.string("string"))])),
          #("description", json.string("List of crypto to trade: TON, USDT")),
        ])),
        #("enabled_fiat", json.object([
          #("type", json.string("array")),
          #("items", json.object([#("type", json.string("string"))])),
          #("description", json.string("List of fiat currencies: THB, RUB, USD, EUR")),
        ])),
      ])),
      #("required", json.array([json.string("telegram_id")], fn(x) { x })),
    ]),
  )
}

/// Get earning history tool
pub fn earning_history_tool() -> Tool {
  Tool(
    name: "earning_history",
    description: "Get trading history and profit/loss breakdown",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Your Telegram ID")),
        ])),
        #("period", json.object([
          #("type", json.string("string")),
          #("enum", json.array([
            json.string("today"),
            json.string("week"),
            json.string("month"),
            json.string("all"),
          ], fn(x) { x })),
          #("description", json.string("Time period (default: week)")),
        ])),
        #("limit", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Max number of trades to return (default: 50)")),
        ])),
      ])),
      #("required", json.array([json.string("telegram_id")], fn(x) { x })),
    ]),
  )
}

/// Scan for arbitrage opportunities tool
pub fn arbitrage_scan_tool() -> Tool {
  Tool(
    name: "arbitrage_scan",
    description: "Scan for cross-platform arbitrage opportunities. Shows price differences between exchanges.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("crypto", json.object([
          #("type", json.string("string")),
          #("enum", json.array([json.string("TON"), json.string("USDT")], fn(x) { x })),
          #("description", json.string("Cryptocurrency to scan (default: USDT)")),
        ])),
        #("fiat", json.object([
          #("type", json.string("string")),
          #("enum", json.array([
            json.string("THB"),
            json.string("RUB"),
            json.string("USD"),
            json.string("EUR"),
          ], fn(x) { x })),
          #("description", json.string("Fiat currency (default: THB)")),
        ])),
        #("min_spread", json.object([
          #("type", json.string("number")),
          #("description", json.string("Minimum spread percentage to show (default: 1.0)")),
        ])),
      ])),
      #("required", json.array([], fn(x) { x })),
    ]),
  )
}

/// Set exchange API credentials tool
pub fn credentials_set_tool() -> Tool {
  Tool(
    name: "credentials_set",
    description: "Set API credentials for exchange trading. Required for real arbitrage execution. Supports: binance, okx",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("exchange", json.object([
          #("type", json.string("string")),
          #("enum", json.array([
            json.string("binance"),
            json.string("okx"),
          ], fn(x) { x })),
          #("description", json.string("Exchange to configure")),
        ])),
        #("api_key", json.object([
          #("type", json.string("string")),
          #("description", json.string("API key from exchange")),
        ])),
        #("api_secret", json.object([
          #("type", json.string("string")),
          #("description", json.string("API secret from exchange")),
        ])),
        #("passphrase", json.object([
          #("type", json.string("string")),
          #("description", json.string("Passphrase (required for OKX)")),
        ])),
        #("enabled", json.object([
          #("type", json.string("boolean")),
          #("description", json.string("Enable/disable this exchange (default: true)")),
        ])),
      ])),
      #("required", json.array([
        json.string("exchange"),
        json.string("api_key"),
        json.string("api_secret"),
      ], fn(x) { x })),
    ]),
  )
}

/// Execute arbitrage manually tool
pub fn arbitrage_execute_tool() -> Tool {
  Tool(
    name: "arbitrage_execute",
    description: "Execute arbitrage trade manually. Buy on one exchange, sell on another. Requires configured credentials.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("opportunity_id", json.object([
          #("type", json.string("string")),
          #("description", json.string("Opportunity ID from arbitrage_scan result")),
        ])),
        #("amount", json.object([
          #("type", json.string("number")),
          #("description", json.string("Amount to trade in crypto units")),
        ])),
        #("dry_run", json.object([
          #("type", json.string("boolean")),
          #("description", json.string("If true, simulate without real execution (default: true)")),
        ])),
      ])),
      #("required", json.array([
        json.string("opportunity_id"),
        json.string("amount"),
      ], fn(x) { x })),
    ]),
  )
}

// =============================================================================
// TOOL HANDLERS
// =============================================================================

/// Handle earning tool calls
pub fn handle_earning_tool(name: String, args: Dynamic) -> ToolResult {
  case name {
    "earning_start" -> handle_earning_start(args)
    "earning_stop" -> handle_earning_stop(args)
    "earning_status" -> handle_earning_status(args)
    "earning_config" -> handle_earning_config(args)
    "earning_history" -> handle_earning_history(args)
    "arbitrage_scan" -> handle_arbitrage_scan(args)
    "credentials_set" -> handle_credentials_set(args)
    "arbitrage_execute" -> handle_arbitrage_execute(args)
    _ -> protocol.error_result("Unknown earning tool: " <> name)
  }
}

// Public wrappers for MCP registry
pub fn handle_earning_start_wrapper(args: json.Json) -> ToolResult {
  handle_earning_start(json_to_dynamic(args))
}

pub fn handle_earning_stop_wrapper(args: json.Json) -> ToolResult {
  handle_earning_stop(json_to_dynamic(args))
}

pub fn handle_earning_status_wrapper(args: json.Json) -> ToolResult {
  handle_earning_status(json_to_dynamic(args))
}

pub fn handle_earning_config_wrapper(args: json.Json) -> ToolResult {
  handle_earning_config(json_to_dynamic(args))
}

pub fn handle_earning_history_wrapper(args: json.Json) -> ToolResult {
  handle_earning_history(json_to_dynamic(args))
}

pub fn handle_arbitrage_scan_wrapper(args: json.Json) -> ToolResult {
  handle_arbitrage_scan(json_to_dynamic(args))
}

pub fn handle_credentials_set_wrapper(args: json.Json) -> ToolResult {
  handle_credentials_set(json_to_dynamic(args))
}

pub fn handle_arbitrage_execute_wrapper(args: json.Json) -> ToolResult {
  handle_arbitrage_execute(json_to_dynamic(args))
}

@external(erlang, "vibee_p2p_ffi", "to_dynamic")
fn json_to_dynamic(j: json.Json) -> Dynamic

/// Handle start earning
fn handle_earning_start(args: Dynamic) -> ToolResult {
  let telegram_id = get_int_field(args, "telegram_id") |> result.unwrap(0)
  let wallet = get_string_field(args, "wallet") |> result.unwrap("")
  let strategy_str = get_string_field(args, "strategy") |> result.unwrap("passive_fees")
  let max_position = get_float_field(args, "max_position_usdt") |> result.unwrap(1000.0)
  let spread = get_float_field(args, "spread_percent") |> result.unwrap(2.0)

  case telegram_id > 0 && wallet != "" {
    False -> protocol.error_result("Missing required fields: telegram_id, wallet")
    True -> {
      let strategy = earning_types.strategy_from_string(strategy_str)

      // Call Erlang worker to start earning
      case start_earning_ffi(telegram_id, wallet, strategy_str, max_position, spread) {
        True -> {
          protocol.text_result(json.to_string(json.object([
            #("success", json.bool(True)),
            #("status", json.string("active")),
            #("strategy", json.string(earning_types.strategy_to_string(strategy))),
            #("max_position_usdt", json.float(max_position)),
            #("spread_percent", json.float(spread)),
            #("message", json.string("Automated earning started! Strategy: " <> strategy_str)),
            #("tips", json.array([
              json.string("Use earning_status to check your progress"),
              json.string("Use earning_stop to pause trading"),
              json.string("Use arbitrage_scan to see current opportunities"),
            ], fn(x) { x })),
          ])))
        }
        False -> protocol.error_result("Failed to start earning agent. Already active?")
      }
    }
  }
}

/// Handle stop earning
fn handle_earning_stop(args: Dynamic) -> ToolResult {
  let telegram_id = get_int_field(args, "telegram_id") |> result.unwrap(0)

  case telegram_id > 0 {
    False -> protocol.error_result("Missing required field: telegram_id")
    True -> {
      case stop_earning_ffi(telegram_id) {
        True -> {
          protocol.text_result(json.to_string(json.object([
            #("success", json.bool(True)),
            #("status", json.string("stopped")),
            #("message", json.string("Automated earning stopped. All positions closed.")),
          ])))
        }
        False -> protocol.error_result("Earning not active for this user")
      }
    }
  }
}

/// Handle get status
fn handle_earning_status(args: Dynamic) -> ToolResult {
  let telegram_id = get_int_field(args, "telegram_id") |> result.unwrap(0)

  case telegram_id > 0 {
    False -> protocol.error_result("Missing required field: telegram_id")
    True -> {
      // Get status from worker
      let status = get_status_ffi(telegram_id)

      protocol.text_result(json.to_string(json.object([
        #("success", json.bool(True)),
        #("is_active", json.bool(status.is_active)),
        #("strategy", json.string(earning_types.strategy_to_string(status.strategy))),
        #("uptime_seconds", json.int(status.uptime_seconds)),
        #("active_orders", json.int(status.active_orders)),
        #("trades_today", json.int(status.trades_today)),
        #("profit_today", json.float(status.profit_today)),
        #("current_position_usdt", json.float(status.current_position_usdt)),
      ])))
    }
  }
}

/// Handle config update
fn handle_earning_config(args: Dynamic) -> ToolResult {
  let telegram_id = get_int_field(args, "telegram_id") |> result.unwrap(0)
  let strategy_str = get_string_field(args, "strategy") |> result.unwrap("")
  let max_position = get_float_field(args, "max_position_usdt") |> result.unwrap(0.0)
  let spread = get_float_field(args, "spread_percent") |> result.unwrap(0.0)
  let daily_loss = get_float_field(args, "daily_loss_limit") |> result.unwrap(0.0)
  let stop_loss = get_float_field(args, "stop_loss_percent") |> result.unwrap(0.0)

  case telegram_id > 0 {
    False -> protocol.error_result("Missing required field: telegram_id")
    True -> {
      // Update strategy in FFI if provided
      case strategy_str {
        "" -> Nil
        s -> {
          let _ = update_strategy_ffi(telegram_id, s)
          Nil
        }
      }

      // Build config update
      let config = earning_types.default_config(telegram_id, "")

      let updated_config = earning_types.EarningConfig(
        ..config,
        strategy: case strategy_str {
          "" -> config.strategy
          s -> earning_types.strategy_from_string(s)
        },
        max_position_usdt: case max_position >. 0.0 {
          True -> max_position
          False -> config.max_position_usdt
        },
        spread_percent: case spread >. 0.0 {
          True -> spread
          False -> config.spread_percent
        },
        daily_loss_limit: case daily_loss >. 0.0 {
          True -> daily_loss
          False -> config.daily_loss_limit
        },
        stop_loss_percent: case stop_loss >. 0.0 {
          True -> stop_loss
          False -> config.stop_loss_percent
        },
      )

      protocol.text_result(json.to_string(json.object([
        #("success", json.bool(True)),
        #("config", json.object([
          #("strategy", json.string(earning_types.strategy_to_string(updated_config.strategy))),
          #("max_position_usdt", json.float(updated_config.max_position_usdt)),
          #("spread_percent", json.float(updated_config.spread_percent)),
          #("daily_loss_limit", json.float(updated_config.daily_loss_limit)),
          #("stop_loss_percent", json.float(updated_config.stop_loss_percent)),
        ])),
        #("message", json.string("Configuration updated")),
      ])))
    }
  }
}

/// Handle earning history
fn handle_earning_history(args: Dynamic) -> ToolResult {
  let telegram_id = get_int_field(args, "telegram_id") |> result.unwrap(0)
  let period = get_string_field(args, "period") |> result.unwrap("week")
  let limit = get_int_field(args, "limit") |> result.unwrap(50)

  case telegram_id > 0 {
    False -> protocol.error_result("Missing required field: telegram_id")
    True -> {
      // Get stats from worker
      let stats = get_stats_ffi(telegram_id)

      protocol.text_result(json.to_string(json.object([
        #("success", json.bool(True)),
        #("period", json.string(period)),
        #("summary", json.object([
          #("total_trades", json.int(stats.total_trades)),
          #("successful_trades", json.int(stats.successful_trades)),
          #("total_volume_usdt", json.float(stats.total_volume_usdt)),
          #("total_profit", json.float(
            stats.total_fees_earned +. stats.total_spread_profit +. stats.total_arbitrage_profit
          )),
          #("fees_earned", json.float(stats.total_fees_earned)),
          #("spread_profit", json.float(stats.total_spread_profit)),
          #("arbitrage_profit", json.float(stats.total_arbitrage_profit)),
        ])),
        #("breakdown", json.object([
          #("today", json.float(stats.today_profit)),
          #("this_week", json.float(stats.this_week_profit)),
          #("this_month", json.float(stats.this_month_profit)),
        ])),
        #("best_trade", json.float(stats.best_trade_profit)),
        #("avg_trade", json.float(stats.avg_trade_profit)),
      ])))
    }
  }
}

/// Handle arbitrage scan
fn handle_arbitrage_scan(args: Dynamic) -> ToolResult {
  let crypto_str = get_string_field(args, "crypto") |> result.unwrap("USDT")
  let fiat_str = get_string_field(args, "fiat") |> result.unwrap("THB")
  let min_spread = get_float_field(args, "min_spread") |> result.unwrap(1.0)

  let crypto = p2p_types.crypto_from_string(crypto_str)
  let fiat = p2p_types.fiat_from_string(fiat_str)

  // Scan for opportunities
  let opportunities = arbitrage.scan_opportunities(crypto, fiat, min_spread)

  let opp_json = list.map(opportunities, fn(opp) {
    json.object([
      #("id", json.string(opp.id)),
      #("buy_from", json.string(earning_types.source_to_string(opp.buy_source))),
      #("buy_price", json.float(opp.buy_price)),
      #("sell_to", json.string(earning_types.source_to_string(opp.sell_source))),
      #("sell_price", json.float(opp.sell_price)),
      #("spread_percent", json.float(opp.spread_percent)),
      #("potential_profit_percent", json.float(opp.potential_profit_percent)),
      #("max_trade_size", json.float(opp.max_trade_size)),
      #("estimated_profit", json.float(opp.estimated_profit)),
      #("risk_score", json.int(arbitrage.calculate_risk_score(opp))),
    ])
  })

  let message = case list.length(opportunities) {
    0 -> "No arbitrage opportunities found with spread > " <> float.to_string(min_spread) <> "%"
    n -> "Found " <> int.to_string(n) <> " arbitrage opportunities"
  }

  protocol.text_result(json.to_string(json.object([
    #("success", json.bool(True)),
    #("crypto", json.string(p2p_types.crypto_to_string(crypto))),
    #("fiat", json.string(p2p_types.fiat_to_string(fiat))),
    #("min_spread", json.float(min_spread)),
    #("opportunities", json.array(opp_json, fn(x) { x })),
    #("count", json.int(list.length(opportunities))),
    #("message", json.string(message)),
  ])))
}

/// Handle credentials set
fn handle_credentials_set(args: Dynamic) -> ToolResult {
  let exchange = get_string_field(args, "exchange") |> result.unwrap("")
  let api_key = get_string_field(args, "api_key") |> result.unwrap("")
  let api_secret = get_string_field(args, "api_secret") |> result.unwrap("")
  let passphrase = get_string_field(args, "passphrase") |> result.unwrap("")
  let enabled = get_bool_field(args, "enabled") |> result.unwrap(True)

  case exchange != "" && api_key != "" && api_secret != "" {
    False -> protocol.error_result("Missing required fields: exchange, api_key, api_secret")
    True -> {
      // Store credentials via FFI
      case store_credentials_ffi(exchange, api_key, api_secret, passphrase, enabled) {
        True -> {
          protocol.text_result(json.to_string(json.object([
            #("success", json.bool(True)),
            #("exchange", json.string(exchange)),
            #("enabled", json.bool(enabled)),
            #("message", json.string("Credentials stored for " <> exchange)),
            #("warning", json.string("API credentials are stored in memory. They will be lost on restart.")),
          ])))
        }
        False -> protocol.error_result("Failed to store credentials")
      }
    }
  }
}

/// Handle arbitrage execute
fn handle_arbitrage_execute(args: Dynamic) -> ToolResult {
  let opportunity_id = get_string_field(args, "opportunity_id") |> result.unwrap("")
  let amount = get_float_field(args, "amount") |> result.unwrap(0.0)
  let dry_run = get_bool_field(args, "dry_run") |> result.unwrap(True)

  case opportunity_id != "" && amount >. 0.0 {
    False -> protocol.error_result("Missing required fields: opportunity_id, amount (> 0)")
    True -> {
      // Execute arbitrage via FFI
      case execute_arbitrage_ffi(opportunity_id, amount, dry_run) {
        Ok(result_json) -> {
          protocol.text_result(result_json)
        }
        Error(err) -> protocol.error_result("Execution failed: " <> err)
      }
    }
  }
}

fn get_bool_field(data: Dynamic, key: String) -> Result(Bool, Nil) {
  get_field_ffi(data, key)
  |> result.then(fn(v) {
    get_bool_value_ffi(v)
  })
}

@external(erlang, "vibee_payment_tools_ffi", "get_bool_value")
fn get_bool_value_ffi(value: Dynamic) -> Result(Bool, Nil)

@external(erlang, "vibee_credentials_ffi", "store_credentials")
fn store_credentials_ffi(
  exchange: String,
  api_key: String,
  api_secret: String,
  passphrase: String,
  enabled: Bool,
) -> Bool

@external(erlang, "vibee_arb_executor_ffi", "execute_arbitrage")
fn execute_arbitrage_ffi(
  opportunity_id: String,
  amount: Float,
  dry_run: Bool,
) -> Result(String, String)

// =============================================================================
// FFI FUNCTIONS
// =============================================================================

@external(erlang, "vibee_earning_ffi", "start_earning")
fn start_earning_ffi(
  telegram_id: Int,
  wallet: String,
  strategy: String,
  max_position: Float,
  spread: Float,
) -> Bool

@external(erlang, "vibee_earning_ffi", "stop_earning")
fn stop_earning_ffi(telegram_id: Int) -> Bool

@external(erlang, "vibee_earning_ffi", "get_status")
fn get_status_ffi(telegram_id: Int) -> earning_types.AgentStatus

@external(erlang, "vibee_earning_ffi", "get_stats")
fn get_stats_ffi(telegram_id: Int) -> earning_types.EarningStats

@external(erlang, "vibee_earning_ffi", "update_strategy")
fn update_strategy_ffi(telegram_id: Int, strategy: String) -> Bool

// =============================================================================
// HELPERS
// =============================================================================

fn get_string_field(data: Dynamic, key: String) -> Result(String, Nil) {
  get_field_ffi(data, key)
  |> result.then(fn(v) {
    get_string_value_ffi(v)
  })
}

@external(erlang, "vibee_payment_tools_ffi", "get_string_value")
fn get_string_value_ffi(value: Dynamic) -> Result(String, Nil)

fn get_int_field(data: Dynamic, key: String) -> Result(Int, Nil) {
  get_field_ffi(data, key)
  |> result.then(fn(v) {
    get_int_value_ffi(v)
  })
}

@external(erlang, "vibee_payment_tools_ffi", "get_int_value")
fn get_int_value_ffi(value: Dynamic) -> Result(Int, Nil)

fn get_float_field(data: Dynamic, key: String) -> Result(Float, Nil) {
  get_field_ffi(data, key)
  |> result.then(fn(v) {
    get_float_value_ffi(v)
  })
}

@external(erlang, "vibee_payment_tools_ffi", "get_float_value")
fn get_float_value_ffi(value: Dynamic) -> Result(Float, Nil)

@external(erlang, "vibee_payment_tools_ffi", "get_map_field")
fn get_field_ffi(map: Dynamic, key: String) -> Result(Dynamic, Nil)
