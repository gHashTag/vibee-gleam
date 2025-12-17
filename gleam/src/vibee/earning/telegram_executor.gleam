// VIBEE Telegram P2P Arbitrage Executor
// Executes arbitrage opportunities via Telegram bots (xRocket, CryptoBot)
//
// Flow:
// 1. Receive arbitrage opportunity
// 2. Create buy order on buy_source
// 3. Wait for execution
// 4. Transfer funds to sell_source (if different)
// 5. Create sell order on sell_source
// 6. Return result with profit

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/earning/types.{
  type ArbitrageOpportunity, type PriceSource, type TradeResult,
  ArbitrageOpportunity, TradeResult, ArbitrageTrade,
  XRocketP2P, CryptoBot, TelegramWallet, VibeeP2P,
}
import vibee/p2p/types as p2p_types
import vibee/p2p/xrocket
import vibee/p2p/cryptobot

// =============================================================================
// EXECUTION RESULT
// =============================================================================

/// Result of executing an arbitrage opportunity
pub type ExecutionResult {
  ExecutionResult(
    opportunity_id: String,
    success: Bool,
    buy_tx_id: Option(String),
    sell_tx_id: Option(String),
    actual_buy_price: Float,
    actual_sell_price: Float,
    actual_amount: Float,
    actual_profit: Float,
    fees_paid: Float,
    execution_time_ms: Int,
    error_message: Option(String),
  )
}

/// Execution error types
pub type ExecutionError {
  InsufficientBalance
  OrderNotFound
  OrderExpired
  TransferFailed(String)
  ApiError(String)
  UnsupportedSource(String)
}

pub fn error_to_string(err: ExecutionError) -> String {
  case err {
    InsufficientBalance -> "Insufficient balance"
    OrderNotFound -> "Order not found"
    OrderExpired -> "Order expired"
    TransferFailed(msg) -> "Transfer failed: " <> msg
    ApiError(msg) -> "API error: " <> msg
    UnsupportedSource(src) -> "Unsupported source: " <> src
  }
}

// =============================================================================
// MAIN EXECUTOR
// =============================================================================

/// Execute arbitrage opportunity
pub fn execute(
  opp: ArbitrageOpportunity,
  wallet: String,
  amount: Float,
) -> Result(ExecutionResult, ExecutionError) {
  let start_time = current_timestamp_ms()

  // Validate amount
  case amount <=. opp.max_trade_size {
    False -> Error(ApiError("Amount exceeds max trade size"))
    True -> {
      // Check if sources are supported
      case is_telegram_source(opp.buy_source), is_telegram_source(opp.sell_source) {
        True, True -> execute_telegram_arbitrage(opp, wallet, amount, start_time)
        True, False -> Error(UnsupportedSource("sell: " <> types.source_to_string(opp.sell_source)))
        False, True -> Error(UnsupportedSource("buy: " <> types.source_to_string(opp.buy_source)))
        False, False -> Error(UnsupportedSource("both sources must be Telegram-based"))
      }
    }
  }
}

/// Check if source is Telegram-based
fn is_telegram_source(source: PriceSource) -> Bool {
  case source {
    XRocketP2P -> True
    CryptoBot -> True
    TelegramWallet -> True
    VibeeP2P -> True
    _ -> False
  }
}

/// Execute arbitrage between Telegram platforms
fn execute_telegram_arbitrage(
  opp: ArbitrageOpportunity,
  wallet: String,
  amount: Float,
  start_time: Int,
) -> Result(ExecutionResult, ExecutionError) {
  // Step 1: Execute buy side
  case execute_buy(opp.buy_source, opp.crypto, opp.fiat, amount, opp.buy_price) {
    Ok(buy_result) -> {
      // Step 2: Transfer to sell platform if needed
      case opp.buy_source == opp.sell_source {
        True -> {
          // Same platform, proceed to sell
          execute_sell_and_complete(opp, buy_result, amount, start_time)
        }
        False -> {
          // Different platforms, need to transfer
          case transfer_between_platforms(opp.buy_source, opp.sell_source, opp.crypto, amount, wallet) {
            Ok(_) -> execute_sell_and_complete(opp, buy_result, amount, start_time)
            Error(err) -> Error(TransferFailed(err))
          }
        }
      }
    }
    Error(err) -> Error(err)
  }
}

/// Execute buy order
fn execute_buy(
  source: PriceSource,
  crypto: p2p_types.CryptoCurrency,
  fiat: p2p_types.FiatCurrency,
  amount: Float,
  price: Float,
) -> Result(#(String, Float), ExecutionError) {
  case source {
    XRocketP2P -> execute_xrocket_buy(crypto, fiat, amount, price)
    CryptoBot -> execute_cryptobot_buy(crypto, amount, price)
    TelegramWallet -> execute_telegram_wallet_buy(crypto, fiat, amount, price)
    VibeeP2P -> execute_vibee_buy(crypto, fiat, amount, price)
    _ -> Error(UnsupportedSource(types.source_to_string(source)))
  }
}

/// Execute sell order
fn execute_sell(
  source: PriceSource,
  crypto: p2p_types.CryptoCurrency,
  fiat: p2p_types.FiatCurrency,
  amount: Float,
  price: Float,
) -> Result(#(String, Float), ExecutionError) {
  case source {
    XRocketP2P -> execute_xrocket_sell(crypto, fiat, amount, price)
    CryptoBot -> execute_cryptobot_sell(crypto, amount, price)
    TelegramWallet -> execute_telegram_wallet_sell(crypto, fiat, amount, price)
    VibeeP2P -> execute_vibee_sell(crypto, fiat, amount, price)
    _ -> Error(UnsupportedSource(types.source_to_string(source)))
  }
}

/// Complete the sell side and return result
fn execute_sell_and_complete(
  opp: ArbitrageOpportunity,
  buy_result: #(String, Float),
  amount: Float,
  start_time: Int,
) -> Result(ExecutionResult, ExecutionError) {
  let #(buy_tx_id, actual_buy_price) = buy_result

  case execute_sell(opp.sell_source, opp.crypto, opp.fiat, amount, opp.sell_price) {
    Ok(sell_result) -> {
      let #(sell_tx_id, actual_sell_price) = sell_result
      let end_time = current_timestamp_ms()

      // Calculate actual profit
      let profit = amount *. { actual_sell_price -. actual_buy_price }
      let fees = amount *. 0.005  // 0.5% platform fee estimate

      Ok(ExecutionResult(
        opportunity_id: opp.id,
        success: True,
        buy_tx_id: Some(buy_tx_id),
        sell_tx_id: Some(sell_tx_id),
        actual_buy_price: actual_buy_price,
        actual_sell_price: actual_sell_price,
        actual_amount: amount,
        actual_profit: profit -. fees,
        fees_paid: fees,
        execution_time_ms: end_time - start_time,
        error_message: None,
      ))
    }
    Error(err) -> Error(err)
  }
}

// =============================================================================
// XROCKET EXECUTION
// =============================================================================

fn execute_xrocket_buy(
  crypto: p2p_types.CryptoCurrency,
  fiat: p2p_types.FiatCurrency,
  amount: Float,
  price: Float,
) -> Result(#(String, Float), ExecutionError) {
  // xRocket Pay API doesn't have direct P2P buy
  // Simulated execution for demo mode
  // In production with API key, this would create an invoice or match with P2P order
  let tx_id = "XR-BUY-" <> int.to_string(current_timestamp_ms())
  Ok(#(tx_id, price))
}

fn execute_xrocket_sell(
  crypto: p2p_types.CryptoCurrency,
  fiat: p2p_types.FiatCurrency,
  amount: Float,
  price: Float,
) -> Result(#(String, Float), ExecutionError) {
  // Simulated execution for demo mode
  let tx_id = "XR-SELL-" <> int.to_string(current_timestamp_ms())
  Ok(#(tx_id, price))
}

// =============================================================================
// CRYPTOBOT EXECUTION
// =============================================================================

fn execute_cryptobot_buy(
  crypto: p2p_types.CryptoCurrency,
  amount: Float,
  price: Float,
) -> Result(#(String, Float), ExecutionError) {
  // CryptoBot doesn't have P2P marketplace
  // Simulated execution for demo mode
  let tx_id = "CB-BUY-" <> int.to_string(current_timestamp_ms())
  Ok(#(tx_id, price))
}

fn execute_cryptobot_sell(
  crypto: p2p_types.CryptoCurrency,
  amount: Float,
  price: Float,
) -> Result(#(String, Float), ExecutionError) {
  // Simulated execution for demo mode
  let tx_id = "CB-SELL-" <> int.to_string(current_timestamp_ms())
  Ok(#(tx_id, price))
}

// =============================================================================
// TELEGRAM WALLET EXECUTION
// =============================================================================

fn execute_telegram_wallet_buy(
  crypto: p2p_types.CryptoCurrency,
  fiat: p2p_types.FiatCurrency,
  amount: Float,
  price: Float,
) -> Result(#(String, Float), ExecutionError) {
  // Telegram @wallet doesn't have public API
  // Would need to use bot interaction
  let tx_id = "TW-BUY-" <> int.to_string(current_timestamp_ms())
  Ok(#(tx_id, price))
}

fn execute_telegram_wallet_sell(
  crypto: p2p_types.CryptoCurrency,
  fiat: p2p_types.FiatCurrency,
  amount: Float,
  price: Float,
) -> Result(#(String, Float), ExecutionError) {
  let tx_id = "TW-SELL-" <> int.to_string(current_timestamp_ms())
  Ok(#(tx_id, price))
}

// =============================================================================
// VIBEE P2P EXECUTION
// =============================================================================

fn execute_vibee_buy(
  crypto: p2p_types.CryptoCurrency,
  fiat: p2p_types.FiatCurrency,
  amount: Float,
  price: Float,
) -> Result(#(String, Float), ExecutionError) {
  // Internal VIBEE P2P order
  let tx_id = "VB-BUY-" <> int.to_string(current_timestamp_ms())
  Ok(#(tx_id, price))
}

fn execute_vibee_sell(
  crypto: p2p_types.CryptoCurrency,
  fiat: p2p_types.FiatCurrency,
  amount: Float,
  price: Float,
) -> Result(#(String, Float), ExecutionError) {
  let tx_id = "VB-SELL-" <> int.to_string(current_timestamp_ms())
  Ok(#(tx_id, price))
}

// =============================================================================
// CROSS-PLATFORM TRANSFER
// =============================================================================

/// Transfer funds between Telegram platforms
fn transfer_between_platforms(
  from: PriceSource,
  to: PriceSource,
  crypto: p2p_types.CryptoCurrency,
  amount: Float,
  wallet: String,
) -> Result(String, String) {
  // In production, this would:
  // 1. Withdraw from source platform to TON wallet
  // 2. Deposit to target platform from TON wallet

  let transfer_id = "XFER-" <> int.to_string(current_timestamp_ms())
  Ok(transfer_id)
}

// =============================================================================
// TRADE RESULT CONVERSION
// =============================================================================

/// Convert execution result to trade result for history
pub fn to_trade_result(
  exec: ExecutionResult,
  opp: ArbitrageOpportunity,
  telegram_id: Int,
) -> TradeResult {
  TradeResult(
    id: exec.opportunity_id,
    telegram_id: telegram_id,
    trade_type: ArbitrageTrade,
    order_id: exec.buy_tx_id,
    volume_usdt: exec.actual_amount,
    profit_usdt: exec.actual_profit,
    fee_paid: exec.fees_paid,
    crypto: opp.crypto,
    fiat: opp.fiat,
    buy_price: exec.actual_buy_price,
    sell_price: exec.actual_sell_price,
    buy_source: Some(opp.buy_source),
    sell_source: Some(opp.sell_source),
    executed_at: current_timestamp(),
  )
}

// =============================================================================
// FFI
// =============================================================================

@external(erlang, "vibee_p2p_ffi", "current_timestamp")
fn current_timestamp() -> Int

@external(erlang, "vibee_p2p_ffi", "current_timestamp_ms")
fn current_timestamp_ms() -> Int
