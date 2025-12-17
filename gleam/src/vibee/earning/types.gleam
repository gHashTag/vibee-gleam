// VIBEE P2P Earning Agent Types
// Types for automated P2P trading and earning strategies

import gleam/option.{type Option}
import vibee/p2p/types.{type CryptoCurrency, type FiatCurrency, type PaymentMethod}

// =============================================================================
// EARNING STRATEGIES
// =============================================================================

/// Available earning strategies
pub type EarningStrategy {
  /// Passive platform fees only (0.5% per trade)
  PassiveFees
  /// Active market making with buy/sell spread
  MarketMaking
  /// Cross-platform arbitrage hunting
  Arbitrage
  /// Combination of all strategies
  Hybrid
}

/// Convert strategy to string
pub fn strategy_to_string(strategy: EarningStrategy) -> String {
  case strategy {
    PassiveFees -> "passive_fees"
    MarketMaking -> "market_making"
    Arbitrage -> "arbitrage"
    Hybrid -> "hybrid"
  }
}

/// Parse strategy from string
pub fn strategy_from_string(s: String) -> EarningStrategy {
  case s {
    "passive_fees" -> PassiveFees
    "market_making" -> MarketMaking
    "arbitrage" -> Arbitrage
    "hybrid" -> Hybrid
    _ -> PassiveFees
  }
}

// =============================================================================
// EARNING CONFIGURATION
// =============================================================================

/// User's earning agent configuration
pub type EarningConfig {
  EarningConfig(
    telegram_id: Int,
    wallet: String,
    strategy: EarningStrategy,
    // Position limits
    max_position_usdt: Float,
    max_position_ton: Float,
    // Market making params
    spread_percent: Float,
    min_order_size: Float,
    max_order_size: Float,
    // Risk management
    daily_loss_limit: Float,
    stop_loss_percent: Float,
    // Enabled currencies
    enabled_crypto: List(CryptoCurrency),
    enabled_fiat: List(FiatCurrency),
    enabled_methods: List(PaymentMethod),
    // Status
    is_active: Bool,
    created_at: Int,
    updated_at: Int,
  )
}

/// Create default earning config
pub fn default_config(telegram_id: Int, wallet: String) -> EarningConfig {
  EarningConfig(
    telegram_id: telegram_id,
    wallet: wallet,
    strategy: PassiveFees,
    max_position_usdt: 1000.0,
    max_position_ton: 200.0,
    spread_percent: 2.0,
    min_order_size: 10.0,
    max_order_size: 500.0,
    daily_loss_limit: 50.0,
    stop_loss_percent: 5.0,
    enabled_crypto: [types.USDT, types.TON],
    enabled_fiat: [types.THB, types.RUB],
    enabled_methods: [types.PromptPay, types.Sberbank],
    is_active: False,
    created_at: 0,
    updated_at: 0,
  )
}

// =============================================================================
// EARNING STATISTICS
// =============================================================================

/// User's earning statistics
pub type EarningStats {
  EarningStats(
    telegram_id: Int,
    // Trade counts
    total_trades: Int,
    successful_trades: Int,
    failed_trades: Int,
    // Volume
    total_volume_usdt: Float,
    // Profit breakdown
    total_fees_earned: Float,
    total_spread_profit: Float,
    total_arbitrage_profit: Float,
    // Time-based profits
    today_profit: Float,
    this_week_profit: Float,
    this_month_profit: Float,
    // Best/avg
    best_trade_profit: Float,
    avg_trade_profit: Float,
    // Last update
    updated_at: Int,
  )
}

/// Empty stats for new user
pub fn empty_stats(telegram_id: Int) -> EarningStats {
  EarningStats(
    telegram_id: telegram_id,
    total_trades: 0,
    successful_trades: 0,
    failed_trades: 0,
    total_volume_usdt: 0.0,
    total_fees_earned: 0.0,
    total_spread_profit: 0.0,
    total_arbitrage_profit: 0.0,
    today_profit: 0.0,
    this_week_profit: 0.0,
    this_month_profit: 0.0,
    best_trade_profit: 0.0,
    avg_trade_profit: 0.0,
    updated_at: 0,
  )
}

// =============================================================================
// ARBITRAGE TYPES
// =============================================================================

/// External price source for arbitrage
pub type PriceSource {
  CoinGecko
  BinanceP2P
  BybitP2P
  OkxP2P
  GarantexP2P
  LocalBitcoins
  Paxful
  VibeeP2P
  /// Telegram @tonRocketBot P2P
  XRocketP2P
  /// Telegram @CryptoBot
  CryptoBot
  /// Telegram @wallet
  TelegramWallet
  /// TonAPI - real TON market rates
  TonAPI
  /// Cross-pair calculation via Garantex (TON/USDT × USDT/RUB)
  GarantexCross
}

/// Convert source to string
pub fn source_to_string(source: PriceSource) -> String {
  case source {
    CoinGecko -> "coingecko"
    BinanceP2P -> "binance_p2p"
    BybitP2P -> "bybit_p2p"
    OkxP2P -> "okx_p2p"
    GarantexP2P -> "garantex_p2p"
    LocalBitcoins -> "localbitcoins"
    Paxful -> "paxful"
    VibeeP2P -> "vibee_p2p"
    XRocketP2P -> "xrocket_p2p"
    CryptoBot -> "cryptobot"
    TelegramWallet -> "telegram_wallet"
    TonAPI -> "tonapi"
    GarantexCross -> "garantex_cross"
  }
}

/// Parse source from string
pub fn source_from_string(s: String) -> PriceSource {
  case s {
    "coingecko" -> CoinGecko
    "binance_p2p" -> BinanceP2P
    "bybit_p2p" -> BybitP2P
    "okx_p2p" -> OkxP2P
    "garantex_p2p" -> GarantexP2P
    "localbitcoins" -> LocalBitcoins
    "paxful" -> Paxful
    "vibee_p2p" -> VibeeP2P
    "xrocket_p2p" -> XRocketP2P
    "cryptobot" -> CryptoBot
    "telegram_wallet" -> TelegramWallet
    "tonapi" -> TonAPI
    "garantex_cross" -> GarantexCross
    _ -> CoinGecko
  }
}

/// Check if source is a Telegram-based platform
pub fn is_telegram_source(source: PriceSource) -> Bool {
  case source {
    XRocketP2P -> True
    CryptoBot -> True
    TelegramWallet -> True
    VibeeP2P -> True
    _ -> False
  }
}

/// Price data from external source
pub type ExternalPrice {
  ExternalPrice(
    source: PriceSource,
    crypto: CryptoCurrency,
    fiat: FiatCurrency,
    buy_price: Float,
    sell_price: Float,
    volume_24h: Float,
    updated_at: Int,
  )
}

/// Detected arbitrage opportunity
pub type ArbitrageOpportunity {
  ArbitrageOpportunity(
    id: String,
    crypto: CryptoCurrency,
    fiat: FiatCurrency,
    // Where to buy
    buy_source: PriceSource,
    buy_price: Float,
    buy_volume: Float,
    // Where to sell
    sell_source: PriceSource,
    sell_price: Float,
    sell_volume: Float,
    // Calculated values
    spread_percent: Float,
    potential_profit_percent: Float,
    max_trade_size: Float,
    estimated_profit: Float,
    // Timing
    detected_at: Int,
    expires_at: Int,
  )
}

/// Calculate spread percentage between two prices
pub fn calculate_spread(buy_price: Float, sell_price: Float) -> Float {
  case buy_price >. 0.0 {
    True -> { sell_price -. buy_price } /. buy_price *. 100.0
    False -> 0.0
  }
}

// =============================================================================
// TRADE HISTORY
// =============================================================================

/// Type of earning trade
pub type TradeType {
  FeeTrade
  SpreadTrade
  ArbitrageTrade
}

/// Convert trade type to string
pub fn trade_type_to_string(t: TradeType) -> String {
  case t {
    FeeTrade -> "fee"
    SpreadTrade -> "spread"
    ArbitrageTrade -> "arbitrage"
  }
}

/// Trade result for history
pub type TradeResult {
  TradeResult(
    id: String,
    telegram_id: Int,
    trade_type: TradeType,
    order_id: Option(String),
    // Amounts
    volume_usdt: Float,
    profit_usdt: Float,
    fee_paid: Float,
    // Details
    crypto: CryptoCurrency,
    fiat: FiatCurrency,
    buy_price: Float,
    sell_price: Float,
    // Sources (for arbitrage)
    buy_source: Option(PriceSource),
    sell_source: Option(PriceSource),
    // Timing
    executed_at: Int,
  )
}

// =============================================================================
// AGENT STATUS
// =============================================================================

/// Current status of earning agent
pub type AgentStatus {
  AgentStatus(
    telegram_id: Int,
    is_active: Bool,
    strategy: EarningStrategy,
    // Current state
    uptime_seconds: Int,
    active_orders: Int,
    pending_arbitrage: Int,
    // Position info
    current_position_usdt: Float,
    current_position_ton: Float,
    // Today's performance
    trades_today: Int,
    profit_today: Float,
    // Errors
    last_error: Option(String),
    error_count: Int,
    // Timestamps
    started_at: Int,
    last_activity_at: Int,
  )
}

/// Create inactive agent status
pub fn inactive_status(telegram_id: Int) -> AgentStatus {
  AgentStatus(
    telegram_id: telegram_id,
    is_active: False,
    strategy: PassiveFees,
    uptime_seconds: 0,
    active_orders: 0,
    pending_arbitrage: 0,
    current_position_usdt: 0.0,
    current_position_ton: 0.0,
    trades_today: 0,
    profit_today: 0.0,
    last_error: option.None,
    error_count: 0,
    started_at: 0,
    last_activity_at: 0,
  )
}
