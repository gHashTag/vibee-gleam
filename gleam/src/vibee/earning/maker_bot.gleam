// VIBEE P2P Maker Bot
// Automated market making - place buy/sell orders with spread
//
// Strategy:
// 1. Monitor market prices from multiple sources
// 2. Place buy orders slightly below market
// 3. Place sell orders slightly above market
// 4. Earn the spread on each completed trade

import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import vibee/p2p/types.{type CryptoCurrency, type FiatCurrency, TON, USDT, RUB, THB, USD, EUR}
import vibee/earning/arbitrage

// =============================================================================
// TYPES
// =============================================================================

/// Maker bot configuration
pub type MakerConfig {
  MakerConfig(
    crypto: CryptoCurrency,
    fiat: FiatCurrency,
    spread_percent: Float,      // Target spread (e.g., 1.5 = 1.5%)
    min_order_size: Float,      // Minimum order size in crypto
    max_order_size: Float,      // Maximum order size in crypto
    total_capital: Float,       // Total capital in fiat
    rebalance_threshold: Float, // Rebalance when price moves this much (%)
    enabled: Bool,
  )
}

/// Active order placed by maker
pub type MakerOrder {
  MakerOrder(
    id: String,
    side: OrderSide,
    crypto: CryptoCurrency,
    fiat: FiatCurrency,
    price: Float,
    amount: Float,
    filled: Float,
    status: OrderStatus,
    created_at: Int,
  )
}

pub type OrderSide {
  Buy
  Sell
}

pub type OrderStatus {
  Active
  PartiallyFilled
  Filled
  Cancelled
}

/// Maker bot state
pub type MakerState {
  MakerState(
    config: MakerConfig,
    buy_order: Option(MakerOrder),
    sell_order: Option(MakerOrder),
    last_market_price: Float,
    total_profit: Float,
    trades_count: Int,
    last_update: Int,
  )
}

/// Price recommendation for maker
pub type PriceRecommendation {
  PriceRecommendation(
    buy_price: Float,   // Price to place buy order
    sell_price: Float,  // Price to place sell order
    market_price: Float,
    spread_percent: Float,
    confidence: Float,  // 0-1, based on data quality
  )
}

// =============================================================================
// MAKER BOT LOGIC
// =============================================================================

/// Create default maker config
pub fn default_config(crypto: CryptoCurrency, fiat: FiatCurrency) -> MakerConfig {
  MakerConfig(
    crypto: crypto,
    fiat: fiat,
    spread_percent: 1.5,        // 1.5% spread
    min_order_size: 10.0,       // Min 10 units
    max_order_size: 1000.0,     // Max 1000 units
    total_capital: 100_000.0,   // 100K fiat
    rebalance_threshold: 0.5,   // Rebalance if price moves 0.5%
    enabled: True,
  )
}

/// Initialize maker state
pub fn init_state(config: MakerConfig) -> MakerState {
  MakerState(
    config: config,
    buy_order: None,
    sell_order: None,
    last_market_price: 0.0,
    total_profit: 0.0,
    trades_count: 0,
    last_update: 0,
  )
}

/// Calculate recommended prices based on market data
pub fn calculate_prices(
  config: MakerConfig,
) -> Result(PriceRecommendation, String) {
  // Fetch current market prices
  let prices = arbitrage.fetch_all_prices(config.crypto, config.fiat)

  case list.length(prices) {
    0 -> Error("No market data available")
    _ -> {
      // Calculate average market price
      let total = list.fold(prices, 0.0, fn(acc, p) {
        acc +. { p.buy_price +. p.sell_price } /. 2.0
      })
      let market_price = total /. int.to_float(list.length(prices))

      // Calculate our buy/sell prices with spread
      let half_spread = config.spread_percent /. 200.0
      let buy_price = market_price *. { 1.0 -. half_spread }
      let sell_price = market_price *. { 1.0 +. half_spread }

      // Confidence based on number of sources
      let confidence = case list.length(prices) {
        n if n >= 3 -> 0.9
        2 -> 0.7
        1 -> 0.5
        _ -> 0.3
      }

      Ok(PriceRecommendation(
        buy_price: buy_price,
        sell_price: sell_price,
        market_price: market_price,
        spread_percent: config.spread_percent,
        confidence: confidence,
      ))
    }
  }
}

/// Check if orders need rebalancing
pub fn needs_rebalance(state: MakerState, current_price: Float) -> Bool {
  case state.last_market_price {
    0.0 -> True  // No previous price, need initial orders
    prev -> {
      let change_percent = float.absolute_value(
        { current_price -. prev } /. prev *. 100.0
      )
      change_percent >. state.config.rebalance_threshold
    }
  }
}

/// Calculate optimal order size based on capital and risk
pub fn calculate_order_size(
  config: MakerConfig,
  price: Float,
) -> Float {
  // Use 20% of capital per side (conservative)
  let capital_per_side = config.total_capital *. 0.2
  let size_from_capital = capital_per_side /. price

  // Clamp to min/max
  float.min(config.max_order_size, float.max(config.min_order_size, size_from_capital))
}

/// Calculate profit from a completed trade pair
pub fn calculate_trade_profit(
  buy_price: Float,
  sell_price: Float,
  amount: Float,
) -> Float {
  { sell_price -. buy_price } *. amount
}

/// Estimate daily profit based on current spread and expected volume
pub fn estimate_daily_profit(
  config: MakerConfig,
  market_price: Float,
  daily_volume: Float,  // Expected volume in crypto units
) -> Float {
  // Assume we capture 10% of market volume
  let our_volume = daily_volume *. 0.1

  // Profit per unit = spread * price
  let profit_per_unit = market_price *. config.spread_percent /. 100.0

  our_volume *. profit_per_unit
}

// =============================================================================
// ORDER GENERATION
// =============================================================================

/// Generate new orders based on recommendation
pub fn generate_orders(
  config: MakerConfig,
  rec: PriceRecommendation,
  timestamp: Int,
) -> #(MakerOrder, MakerOrder) {
  let buy_size = calculate_order_size(config, rec.buy_price)
  let sell_size = calculate_order_size(config, rec.sell_price)

  let buy_order = MakerOrder(
    id: "BUY-" <> int.to_string(timestamp),
    side: Buy,
    crypto: config.crypto,
    fiat: config.fiat,
    price: rec.buy_price,
    amount: buy_size,
    filled: 0.0,
    status: Active,
    created_at: timestamp,
  )

  let sell_order = MakerOrder(
    id: "SELL-" <> int.to_string(timestamp),
    side: Sell,
    crypto: config.crypto,
    fiat: config.fiat,
    price: rec.sell_price,
    amount: sell_size,
    filled: 0.0,
    status: Active,
    created_at: timestamp,
  )

  #(buy_order, sell_order)
}

/// Log maker activity
pub fn log_activity(state: MakerState, rec: PriceRecommendation) -> Nil {
  io.println("[MAKER] " <> crypto_to_string(state.config.crypto) <> "/" <> fiat_to_string(state.config.fiat))
  io.println("  Market: " <> float.to_string(rec.market_price))
  io.println("  Buy at: " <> float.to_string(rec.buy_price))
  io.println("  Sell at: " <> float.to_string(rec.sell_price))
  io.println("  Spread: " <> float.to_string(rec.spread_percent) <> "%")
  io.println("  Confidence: " <> float.to_string(rec.confidence *. 100.0) <> "%")
  io.println("  Total profit: " <> float.to_string(state.total_profit))
  io.println("  Trades: " <> int.to_string(state.trades_count))
  Nil
}

fn crypto_to_string(c: CryptoCurrency) -> String {
  case c {
    TON -> "TON"
    USDT -> "USDT"
    _ -> "CRYPTO"
  }
}

fn fiat_to_string(f: FiatCurrency) -> String {
  case f {
    types.RUB -> "RUB"
    types.THB -> "THB"
    types.USD -> "USD"
    types.EUR -> "EUR"
  }
}

// =============================================================================
// SERIALIZATION
// =============================================================================

import gleam/json

/// Encode maker state to JSON
pub fn encode_state(state: MakerState) -> json.Json {
  json.object([
    #("crypto", json.string(crypto_to_string(state.config.crypto))),
    #("fiat", json.string(fiat_to_string(state.config.fiat))),
    #("spread_percent", json.float(state.config.spread_percent)),
    #("last_market_price", json.float(state.last_market_price)),
    #("total_profit", json.float(state.total_profit)),
    #("trades_count", json.int(state.trades_count)),
    #("enabled", json.bool(state.config.enabled)),
    #("buy_order", encode_optional_order(state.buy_order)),
    #("sell_order", encode_optional_order(state.sell_order)),
  ])
}

fn encode_optional_order(order: Option(MakerOrder)) -> json.Json {
  case order {
    None -> json.null()
    Some(o) -> encode_order(o)
  }
}

fn encode_order(order: MakerOrder) -> json.Json {
  json.object([
    #("id", json.string(order.id)),
    #("side", json.string(case order.side { Buy -> "buy" Sell -> "sell" })),
    #("price", json.float(order.price)),
    #("amount", json.float(order.amount)),
    #("filled", json.float(order.filled)),
    #("status", json.string(case order.status {
      Active -> "active"
      PartiallyFilled -> "partial"
      Filled -> "filled"
      Cancelled -> "cancelled"
    })),
  ])
}

/// Encode price recommendation
pub fn encode_recommendation(rec: PriceRecommendation) -> json.Json {
  json.object([
    #("buy_price", json.float(rec.buy_price)),
    #("sell_price", json.float(rec.sell_price)),
    #("market_price", json.float(rec.market_price)),
    #("spread_percent", json.float(rec.spread_percent)),
    #("confidence", json.float(rec.confidence)),
    #("estimated_profit_per_1000", json.float(
      rec.market_price *. rec.spread_percent /. 100.0 *. 1000.0 /. rec.market_price
    )),
  ])
}

// =============================================================================
// P2P ORDER PLACEMENT
// =============================================================================

/// Place buy and sell orders on VibeeP2P
pub fn place_maker_orders(
  config: MakerConfig,
  telegram_id: Int,
) -> Result(#(String, String), String) {
  // Get price recommendations
  case calculate_prices(config) {
    Error(e) -> {
      io.println("[MAKER] Failed to get prices: " <> e)
      Error(e)
    }
    Ok(rec) -> {
      io.println("[MAKER] Market price: " <> float.to_string(rec.market_price))
      io.println("[MAKER] Placing buy @ " <> float.to_string(rec.buy_price)
        <> ", sell @ " <> float.to_string(rec.sell_price))

      let timestamp = current_timestamp()
      let amount = calculate_order_size(config, rec.market_price)

      // Create sell order (we sell crypto for fiat)
      let sell_order_id = generate_order_id()
      let sell_order = create_p2p_order_map(
        sell_order_id,
        telegram_id,
        "sell",
        config.crypto,
        config.fiat,
        amount,
        rec.sell_price,
        timestamp,
      )

      case ffi_create_order(sell_order) {
        Ok(_) -> {
          io.println("[MAKER] ✓ Sell order created: " <> sell_order_id)

          // Create buy order (we buy crypto for fiat)
          let buy_order_id = generate_order_id()
          let buy_order = create_p2p_order_map(
            buy_order_id,
            telegram_id,
            "buy",
            config.crypto,
            config.fiat,
            amount,
            rec.buy_price,
            timestamp,
          )

          case ffi_create_order(buy_order) {
            Ok(_) -> {
              io.println("[MAKER] ✓ Buy order created: " <> buy_order_id)
              Ok(#(buy_order_id, sell_order_id))
            }
            Error(e) -> {
              io.println("[MAKER] Failed to create buy order: " <> e)
              Error("Failed to create buy order")
            }
          }
        }
        Error(e) -> {
          io.println("[MAKER] Failed to create sell order: " <> e)
          Error("Failed to create sell order")
        }
      }
    }
  }
}

/// Cancel old maker orders for a user
pub fn cancel_old_orders(telegram_id: Int) -> Int {
  let orders = ffi_list_orders_by_seller(telegram_id)
  let cancelled = list.filter_map(orders, fn(order_map) {
    let order_id = ffi_get_map_string(order_map, "id")
    let status = ffi_get_map_string(order_map, "status")
    // Only cancel open orders
    case status {
      "open" -> {
        let _ = ffi_cancel_order(order_id)
        io.println("[MAKER] Cancelled order: " <> order_id)
        Ok(order_id)
      }
      _ -> Error(Nil)
    }
  })
  list.length(cancelled)
}

/// Create a P2P order map for the FFI
fn create_p2p_order_map(
  id: String,
  seller_id: Int,
  side: String,
  crypto: CryptoCurrency,
  fiat: FiatCurrency,
  amount: Float,
  price: Float,
  timestamp: Int,
) -> json.Json {
  json.object([
    #("id", json.string(id)),
    #("seller_telegram_id", json.int(seller_id)),
    #("buyer_telegram_id", json.null()),
    #("side", json.string(side)),
    #("crypto", json.string(crypto_to_string(crypto))),
    #("fiat", json.string(fiat_to_string(fiat))),
    #("amount", json.float(amount)),
    #("price", json.float(price)),
    #("total_fiat", json.float(amount *. price)),
    #("status", json.string("open")),
    #("payment_method", json.string("any")),
    #("min_amount", json.float(10.0)),
    #("max_amount", json.float(amount)),
    #("escrow_address", json.string("")),
    #("escrow_amount", json.float(0.0)),
    #("created_at", json.int(timestamp)),
    #("updated_at", json.int(timestamp)),
    #("expires_at", json.int(timestamp + 86400)),  // 24 hours
    #("is_maker_bot", json.bool(True)),
  ])
}

// FFI declarations for P2P operations
@external(erlang, "vibee_p2p_ffi", "generate_order_id")
fn generate_order_id() -> String

@external(erlang, "vibee_p2p_ffi", "current_timestamp")
fn current_timestamp() -> Int

@external(erlang, "vibee_maker_ffi", "create_order")
fn ffi_create_order(order: json.Json) -> Result(String, String)

@external(erlang, "vibee_maker_ffi", "list_orders_by_seller")
fn ffi_list_orders_by_seller(seller_id: Int) -> List(Dynamic)

@external(erlang, "vibee_maker_ffi", "cancel_order")
fn ffi_cancel_order(order_id: String) -> Result(Nil, String)

@external(erlang, "vibee_maker_ffi", "get_map_string")
fn ffi_get_map_string(map: Dynamic, key: String) -> String

import gleam/dynamic.{type Dynamic}
