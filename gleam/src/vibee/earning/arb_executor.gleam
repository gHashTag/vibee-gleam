// VIBEE Cross-Exchange Arbitrage Executor
// Automatic execution of arbitrage opportunities
//
// Flow:
// 1. Detect opportunity (spread > threshold)
// 2. Check balances on both exchanges
// 3. Execute buy order on cheaper exchange
// 4. Execute sell order on expensive exchange
// 5. Track profit and fees

import gleam/float
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import vibee/p2p/types.{type CryptoCurrency, type FiatCurrency, TON, USDT, RUB, THB, USD}
import vibee/earning/types as earning_types
import vibee/earning/alerts.{type SpreadAlert}

// =============================================================================
// TYPES
// =============================================================================

/// Exchange API credentials
pub type ExchangeCredentials {
  ExchangeCredentials(
    exchange: earning_types.PriceSource,
    api_key: String,
    api_secret: String,
    enabled: Bool,
  )
}

/// Balance on an exchange
pub type ExchangeBalance {
  ExchangeBalance(
    exchange: earning_types.PriceSource,
    crypto_balance: Float,
    fiat_balance: Float,
    crypto_available: Float,  // Not in orders
    fiat_available: Float,
  )
}

/// Execution configuration
pub type ExecutorConfig {
  ExecutorConfig(
    min_spread_percent: Float,  // Minimum spread to execute (e.g., 1.5)
    min_profit_usd: Float,      // Minimum profit per trade
    max_trade_size_usd: Float,  // Maximum trade size
    fee_percent: Float,         // Estimated fee per side
    slippage_percent: Float,    // Expected slippage
    dry_run: Bool,              // If true, don't execute real trades
    enabled: Bool,
  )
}

/// Trade execution result
pub type TradeResult {
  TradeResult(
    success: Bool,
    opportunity_id: String,
    buy_order_id: Option(String),
    sell_order_id: Option(String),
    actual_buy_price: Float,
    actual_sell_price: Float,
    amount_traded: Float,
    gross_profit: Float,
    fees_paid: Float,
    net_profit: Float,
    executed_at: Int,
    error: Option(String),
  )
}

/// Executor state
pub type ExecutorState {
  ExecutorState(
    config: ExecutorConfig,
    credentials: List(ExchangeCredentials),
    balances: List(ExchangeBalance),
    pending_trades: List(String),
    completed_trades: Int,
    total_profit: Float,
    total_fees: Float,
    last_execution: Int,
  )
}

// =============================================================================
// CONFIGURATION
// =============================================================================

/// Default executor config (conservative)
pub fn default_config() -> ExecutorConfig {
  ExecutorConfig(
    min_spread_percent: 1.5,    // Need 1.5% spread
    min_profit_usd: 5.0,        // Minimum $5 profit
    max_trade_size_usd: 500.0,  // Max $500 per trade
    fee_percent: 0.5,           // 0.5% fee estimate
    slippage_percent: 0.2,      // 0.2% slippage estimate
    dry_run: True,              // Start in dry run mode
    enabled: False,
  )
}

/// Initialize executor state
pub fn init_state(config: ExecutorConfig) -> ExecutorState {
  ExecutorState(
    config: config,
    credentials: [],
    balances: [],
    pending_trades: [],
    completed_trades: 0,
    total_profit: 0.0,
    total_fees: 0.0,
    last_execution: 0,
  )
}

// =============================================================================
// EXECUTION LOGIC
// =============================================================================

/// Check if opportunity is executable
pub fn can_execute(
  alert: SpreadAlert,
  state: ExecutorState,
) -> Result(Float, String) {
  // Check if enabled
  case state.config.enabled {
    False -> Error("Executor disabled")
    True -> {
      // Check minimum spread after fees
      let net_spread = alert.spread_percent
        -. state.config.fee_percent *. 2.0  // Fees on both sides
        -. state.config.slippage_percent *. 2.0

      case net_spread >. state.config.min_spread_percent -. 1.0 {
        False -> Error("Net spread too low: " <> float.to_string(net_spread) <> "%")
        True -> {
          // Check balances
          case check_balances(alert, state) {
            Error(e) -> Error(e)
            Ok(max_amount) -> {
              // Check minimum profit
              let estimated_profit = max_amount *. alert.spread_percent /. 100.0
              case estimated_profit >. state.config.min_profit_usd {
                False -> Error("Profit too low: $" <> float.to_string(estimated_profit))
                True -> Ok(max_amount)
              }
            }
          }
        }
      }
    }
  }
}

/// Check if we have sufficient balances
fn check_balances(
  alert: SpreadAlert,
  state: ExecutorState,
) -> Result(Float, String) {
  // Find balance for buy exchange (need fiat)
  let buy_balance = list.find(state.balances, fn(b) {
    b.exchange == alert.buy_source
  })

  // Find balance for sell exchange (need crypto)
  let sell_balance = list.find(state.balances, fn(b) {
    b.exchange == alert.sell_source
  })

  case buy_balance, sell_balance {
    Error(_), _ -> Error("No balance on " <> earning_types.source_to_string(alert.buy_source))
    _, Error(_) -> Error("No balance on " <> earning_types.source_to_string(alert.sell_source))
    Ok(buy_bal), Ok(sell_bal) -> {
      // Calculate max trade size
      let max_from_fiat = buy_bal.fiat_available /. alert.buy_price
      let max_from_crypto = sell_bal.crypto_available
      let max_from_config = state.config.max_trade_size_usd /. alert.buy_price

      let max_amount = float.min(max_from_fiat, float.min(max_from_crypto, max_from_config))

      case max_amount >. 0.0 {
        False -> Error("Insufficient balance")
        True -> Ok(max_amount)
      }
    }
  }
}

/// Execute arbitrage (dry run or real)
pub fn execute(
  alert: SpreadAlert,
  state: ExecutorState,
  amount: Float,
) -> TradeResult {
  let timestamp = current_timestamp()

  case state.config.dry_run {
    True -> {
      // Simulate execution
      io.println("[EXECUTOR] DRY RUN: Would execute " <> float.to_string(amount) <> " " <> crypto_to_string(alert.crypto))
      io.println("  Buy @ " <> earning_types.source_to_string(alert.buy_source) <> ": " <> float.to_string(alert.buy_price))
      io.println("  Sell @ " <> earning_types.source_to_string(alert.sell_source) <> ": " <> float.to_string(alert.sell_price))

      let gross_profit = amount *. { alert.sell_price -. alert.buy_price }
      let fees = amount *. alert.buy_price *. state.config.fee_percent /. 100.0 *. 2.0
      let net_profit = gross_profit -. fees

      TradeResult(
        success: True,
        opportunity_id: alert.id,
        buy_order_id: Some("DRY-BUY-" <> int.to_string(timestamp)),
        sell_order_id: Some("DRY-SELL-" <> int.to_string(timestamp)),
        actual_buy_price: alert.buy_price,
        actual_sell_price: alert.sell_price,
        amount_traded: amount,
        gross_profit: gross_profit,
        fees_paid: fees,
        net_profit: net_profit,
        executed_at: timestamp,
        error: None,
      )
    }
    False -> {
      // Real execution
      io.println("[EXECUTOR] LIVE: Executing arbitrage...")
      execute_real_trade(alert, state, amount, timestamp)
    }
  }
}

/// Execute real trade using exchange APIs
fn execute_real_trade(
  alert: SpreadAlert,
  state: ExecutorState,
  amount: Float,
  timestamp: Int,
) -> TradeResult {
  io.println("[EXECUTOR] Starting REAL trade execution...")
  io.println("  Opportunity: " <> alert.id)
  io.println("  Amount: " <> float.to_string(amount) <> " " <> crypto_to_string(alert.crypto))
  io.println("  Buy @ " <> earning_types.source_to_string(alert.buy_source) <> ": " <> float.to_string(alert.buy_price))
  io.println("  Sell @ " <> earning_types.source_to_string(alert.sell_source) <> ": " <> float.to_string(alert.sell_price))

  // Step 1: Place buy order on cheaper exchange
  let buy_result = place_exchange_order(
    alert.buy_source,
    "BUY",
    alert.crypto,
    alert.fiat,
    amount,
    alert.buy_price,
    state,
  )

  case buy_result {
    Error(buy_error) -> {
      io.println("[EXECUTOR] ✗ Buy order failed: " <> buy_error)
      TradeResult(
        success: False,
        opportunity_id: alert.id,
        buy_order_id: None,
        sell_order_id: None,
        actual_buy_price: 0.0,
        actual_sell_price: 0.0,
        amount_traded: 0.0,
        gross_profit: 0.0,
        fees_paid: 0.0,
        net_profit: 0.0,
        executed_at: timestamp,
        error: Some("Buy failed: " <> buy_error),
      )
    }
    Ok(buy_order_id) -> {
      io.println("[EXECUTOR] ✓ Buy order placed: " <> buy_order_id)

      // Step 2: Place sell order on expensive exchange
      let sell_result = place_exchange_order(
        alert.sell_source,
        "SELL",
        alert.crypto,
        alert.fiat,
        amount,
        alert.sell_price,
        state,
      )

      case sell_result {
        Error(sell_error) -> {
          io.println("[EXECUTOR] ✗ Sell order failed: " <> sell_error)
          // TODO: Implement rollback - cancel buy order
          TradeResult(
            success: False,
            opportunity_id: alert.id,
            buy_order_id: Some(buy_order_id),
            sell_order_id: None,
            actual_buy_price: alert.buy_price,
            actual_sell_price: 0.0,
            amount_traded: amount,
            gross_profit: 0.0,
            fees_paid: 0.0,
            net_profit: 0.0,
            executed_at: timestamp,
            error: Some("Sell failed (buy succeeded): " <> sell_error),
          )
        }
        Ok(sell_order_id) -> {
          io.println("[EXECUTOR] ✓ Sell order placed: " <> sell_order_id)

          // Calculate profit
          let gross_profit = amount *. { alert.sell_price -. alert.buy_price }
          let fees = amount *. alert.buy_price *. state.config.fee_percent /. 100.0 *. 2.0
          let net_profit = gross_profit -. fees

          io.println("[EXECUTOR] ✓ Trade complete!")
          io.println("  Gross profit: " <> float.to_string(gross_profit))
          io.println("  Fees: " <> float.to_string(fees))
          io.println("  Net profit: " <> float.to_string(net_profit))

          TradeResult(
            success: True,
            opportunity_id: alert.id,
            buy_order_id: Some(buy_order_id),
            sell_order_id: Some(sell_order_id),
            actual_buy_price: alert.buy_price,
            actual_sell_price: alert.sell_price,
            amount_traded: amount,
            gross_profit: gross_profit,
            fees_paid: fees,
            net_profit: net_profit,
            executed_at: timestamp,
            error: None,
          )
        }
      }
    }
  }
}

/// Place order on exchange using FFI
fn place_exchange_order(
  source: earning_types.PriceSource,
  side: String,
  crypto: CryptoCurrency,
  fiat: FiatCurrency,
  amount: Float,
  price: Float,
  _state: ExecutorState,
) -> Result(String, String) {
  let exchange = earning_types.source_to_string(source)
  let crypto_str = crypto_to_string(crypto)
  let fiat_str = fiat_to_string(fiat)

  io.println("[EXECUTOR] Placing " <> side <> " order on " <> exchange)
  io.println("  Pair: " <> crypto_str <> "/" <> fiat_str)
  io.println("  Amount: " <> float.to_string(amount))
  io.println("  Price: " <> float.to_string(price))

  // Call FFI to execute order
  ffi_place_order(exchange, side, crypto_str, fiat_str, amount, price)
}

fn fiat_to_string(f: FiatCurrency) -> String {
  case f {
    RUB -> "RUB"
    THB -> "THB"
    USD -> "USD"
    _ -> "USD"
  }
}

// FFI for exchange order execution
@external(erlang, "vibee_arb_ffi", "place_order")
fn ffi_place_order(
  exchange: String,
  side: String,
  crypto: String,
  fiat: String,
  amount: Float,
  price: Float,
) -> Result(String, String)

// =============================================================================
// SERIALIZATION
// =============================================================================

/// Encode trade result to JSON
pub fn encode_trade_result(result: TradeResult) -> json.Json {
  json.object([
    #("success", json.bool(result.success)),
    #("opportunity_id", json.string(result.opportunity_id)),
    #("buy_order_id", encode_option_string(result.buy_order_id)),
    #("sell_order_id", encode_option_string(result.sell_order_id)),
    #("actual_buy_price", json.float(result.actual_buy_price)),
    #("actual_sell_price", json.float(result.actual_sell_price)),
    #("amount_traded", json.float(result.amount_traded)),
    #("gross_profit", json.float(result.gross_profit)),
    #("fees_paid", json.float(result.fees_paid)),
    #("net_profit", json.float(result.net_profit)),
    #("executed_at", json.int(result.executed_at)),
    #("error", encode_option_string(result.error)),
  ])
}

fn encode_option_string(opt: Option(String)) -> json.Json {
  case opt {
    None -> json.null()
    Some(s) -> json.string(s)
  }
}

/// Encode executor state
pub fn encode_state(state: ExecutorState) -> json.Json {
  json.object([
    #("enabled", json.bool(state.config.enabled)),
    #("dry_run", json.bool(state.config.dry_run)),
    #("min_spread_percent", json.float(state.config.min_spread_percent)),
    #("min_profit_usd", json.float(state.config.min_profit_usd)),
    #("max_trade_size_usd", json.float(state.config.max_trade_size_usd)),
    #("exchanges_connected", json.int(list.length(state.credentials))),
    #("balances_count", json.int(list.length(state.balances))),
    #("completed_trades", json.int(state.completed_trades)),
    #("total_profit", json.float(state.total_profit)),
    #("total_fees", json.float(state.total_fees)),
    #("pending_trades", json.int(list.length(state.pending_trades))),
  ])
}

/// Encode balance
pub fn encode_balance(balance: ExchangeBalance) -> json.Json {
  json.object([
    #("exchange", json.string(earning_types.source_to_string(balance.exchange))),
    #("crypto_balance", json.float(balance.crypto_balance)),
    #("fiat_balance", json.float(balance.fiat_balance)),
    #("crypto_available", json.float(balance.crypto_available)),
    #("fiat_available", json.float(balance.fiat_available)),
  ])
}

// =============================================================================
// HELPERS
// =============================================================================

fn crypto_to_string(c: CryptoCurrency) -> String {
  case c {
    TON -> "TON"
    USDT -> "USDT"
    _ -> "CRYPTO"
  }
}

@external(erlang, "vibee_p2p_ffi", "current_timestamp")
fn current_timestamp() -> Int
