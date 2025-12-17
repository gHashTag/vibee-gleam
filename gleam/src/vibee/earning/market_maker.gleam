// VIBEE P2P Market Maker
// Logic for automated market making with buy/sell spread

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/earning/types.{type EarningConfig, type EarningStrategy, MarketMaking, Hybrid}
import vibee/p2p/types as p2p_types
import vibee/p2p/escrow_client

// =============================================================================
// PRICE CALCULATIONS
// =============================================================================

/// Calculate buy and sell prices with spread
/// Returns (buy_price, sell_price) where:
/// - buy_price is below market (we buy cheaper)
/// - sell_price is above market (we sell more expensive)
pub fn calculate_order_prices(
  market_rate: Float,
  spread_percent: Float,
) -> #(Float, Float) {
  let half_spread = spread_percent /. 2.0
  let buy_price = market_rate *. { 1.0 -. half_spread /. 100.0 }
  let sell_price = market_rate *. { 1.0 +. half_spread /. 100.0 }
  #(buy_price, sell_price)
}

/// Calculate potential profit from completed buy+sell cycle
pub fn calculate_cycle_profit(
  amount: Float,
  buy_price: Float,
  sell_price: Float,
  fee_percent: Float,
) -> Float {
  let gross_profit = amount *. { sell_price -. buy_price }
  let fee = amount *. sell_price *. fee_percent /. 100.0
  gross_profit -. fee
}

/// Check if spread is profitable after fees
pub fn is_spread_profitable(
  spread_percent: Float,
  fee_percent: Float,
  min_profit_percent: Float,
) -> Bool {
  let net_profit = spread_percent -. fee_percent *. 2.0
  net_profit >. min_profit_percent
}

// =============================================================================
// ORDER SIZE OPTIMIZATION
// =============================================================================

/// Calculate optimal order sizes based on config and market conditions
pub fn calculate_order_sizes(
  config: EarningConfig,
  available_balance: Float,
  market_depth: Float,
) -> List(Float) {
  // Use geometric series of order sizes
  let max_size = float.min(config.max_order_size, available_balance /. 3.0)
  let min_size = config.min_order_size

  case max_size <. min_size {
    True -> []
    False -> {
      // Create 3 orders of different sizes
      let sizes = [
        min_size,
        { max_size +. min_size } /. 2.0,
        max_size,
      ]
      list.filter(sizes, fn(s) { s >=. min_size && s <=. max_size })
    }
  }
}

/// Distribute total amount into multiple orders
pub fn split_into_orders(
  total_amount: Float,
  min_size: Float,
  max_size: Float,
  num_orders: Int,
) -> List(Float) {
  case num_orders <= 0 {
    True -> []
    False -> {
      let per_order = total_amount /. int.to_float(num_orders)
      case per_order <. min_size {
        True -> [total_amount]
        False -> {
          let capped = float.min(per_order, max_size)
          list.repeat(capped, num_orders)
        }
      }
    }
  }
}

// =============================================================================
// MARKET MAKING ORDERS
// =============================================================================

/// Order to create for market making
pub type MMOrder {
  MMOrder(
    side: OrderSide,
    crypto: p2p_types.CryptoCurrency,
    crypto_amount: Float,
    fiat: p2p_types.FiatCurrency,
    fiat_amount: Float,
    rate: Float,
    payment_method: p2p_types.PaymentMethod,
  )
}

pub type OrderSide {
  Buy
  Sell
}

/// Generate market making orders for a currency pair
pub fn generate_mm_orders(
  config: EarningConfig,
  crypto: p2p_types.CryptoCurrency,
  fiat: p2p_types.FiatCurrency,
  market_rate: Float,
  payment_method: p2p_types.PaymentMethod,
) -> List(MMOrder) {
  case config.strategy {
    MarketMaking | Hybrid -> {
      let #(buy_rate, sell_rate) = calculate_order_prices(market_rate, config.spread_percent)
      let sizes = calculate_order_sizes(config, config.max_position_usdt, 10000.0)

      // Create both buy and sell orders
      let buy_orders = list.map(sizes, fn(size) {
        MMOrder(
          side: Buy,
          crypto: crypto,
          crypto_amount: size /. buy_rate,
          fiat: fiat,
          fiat_amount: size,
          rate: buy_rate,
          payment_method: payment_method,
        )
      })

      let sell_orders = list.map(sizes, fn(size) {
        MMOrder(
          side: Sell,
          crypto: crypto,
          crypto_amount: size /. sell_rate,
          fiat: fiat,
          fiat_amount: size,
          rate: sell_rate,
          payment_method: payment_method,
        )
      })

      list.append(buy_orders, sell_orders)
    }
    _ -> []
  }
}

// =============================================================================
// PRICE MONITORING
// =============================================================================

/// Price deviation check result
pub type PriceCheck {
  PriceWithinRange
  PriceDeviatedUp(Float)
  PriceDeviatedDown(Float)
}

/// Check if market price has deviated from order price
pub fn check_price_deviation(
  order_rate: Float,
  current_market_rate: Float,
  max_deviation_percent: Float,
) -> PriceCheck {
  let deviation = { current_market_rate -. order_rate } /. order_rate *. 100.0

  case deviation {
    d if d >. max_deviation_percent -> PriceDeviatedUp(d)
    d if d <. 0.0 -. max_deviation_percent -> PriceDeviatedDown(float.absolute_value(d))
    _ -> PriceWithinRange
  }
}

/// Determine if order should be repriced
pub fn should_reprice_order(
  order_rate: Float,
  current_market_rate: Float,
  side: OrderSide,
  max_deviation_percent: Float,
) -> Bool {
  case check_price_deviation(order_rate, current_market_rate, max_deviation_percent) {
    PriceWithinRange -> False
    PriceDeviatedUp(_) -> {
      // Market moved up
      case side {
        Buy -> True   // Increase buy price to stay competitive
        Sell -> False // Sell price is now more attractive
      }
    }
    PriceDeviatedDown(_) -> {
      // Market moved down
      case side {
        Buy -> False  // Buy price is now more attractive
        Sell -> True  // Decrease sell price to stay competitive
      }
    }
  }
}

// =============================================================================
// INVENTORY MANAGEMENT
// =============================================================================

/// Current inventory position
pub type Inventory {
  Inventory(
    usdt_balance: Float,
    ton_balance: Float,
    pending_buy_usdt: Float,
    pending_sell_usdt: Float,
  )
}

/// Calculate net position
pub fn net_position(inv: Inventory) -> Float {
  inv.usdt_balance +. inv.pending_buy_usdt -. inv.pending_sell_usdt
}

/// Check if we can open new position
pub fn can_open_position(
  inv: Inventory,
  amount: Float,
  side: OrderSide,
  config: EarningConfig,
) -> Bool {
  case side {
    Buy -> {
      let total_exposure = inv.pending_buy_usdt +. amount
      total_exposure <=. config.max_position_usdt
    }
    Sell -> {
      let total_exposure = inv.pending_sell_usdt +. amount
      total_exposure <=. config.max_position_usdt
    }
  }
}

/// Calculate how much more we can buy/sell
pub fn available_capacity(
  inv: Inventory,
  side: OrderSide,
  config: EarningConfig,
) -> Float {
  case side {
    Buy -> float.max(0.0, config.max_position_usdt -. inv.pending_buy_usdt)
    Sell -> float.max(0.0, config.max_position_usdt -. inv.pending_sell_usdt)
  }
}

// =============================================================================
// RISK MANAGEMENT
// =============================================================================

/// Risk check result
pub type RiskCheckResult {
  RiskOk
  DailyLossLimitReached
  StopLossTriggered
  MaxPositionReached
  InsufficientBalance
}

/// Check all risk limits
pub fn check_risk_limits(
  config: EarningConfig,
  daily_pnl: Float,
  unrealized_pnl: Float,
  current_position: Float,
) -> RiskCheckResult {
  // Check daily loss limit
  case daily_pnl <. 0.0 -. config.daily_loss_limit {
    True -> DailyLossLimitReached
    False -> {
      // Check stop loss on unrealized
      let stop_loss_amount = config.max_position_usdt *. config.stop_loss_percent /. 100.0
      case unrealized_pnl <. 0.0 -. stop_loss_amount {
        True -> StopLossTriggered
        False -> {
          // Check max position
          case current_position >. config.max_position_usdt {
            True -> MaxPositionReached
            False -> RiskOk
          }
        }
      }
    }
  }
}

/// Should we pause trading based on risk
pub fn should_pause_trading(risk_result: RiskCheckResult) -> Bool {
  case risk_result {
    RiskOk -> False
    _ -> True
  }
}
