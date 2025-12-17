// VIBEE P2P Escrow Types
// Types for peer-to-peer trading with fiat currencies

import gleam/option.{type Option, None, Some}

// =============================================================================
// ORDER STATUS
// =============================================================================

/// P2P Order status
pub type OrderStatus {
  /// Order is open and waiting for a buyer
  Open
  /// Buyer has taken the order, waiting for fiat payment
  Locked
  /// Buyer claims they paid, waiting for confirmation
  FiatSent
  /// Payment confirmed, crypto released
  Completed
  /// Dispute opened
  Disputed
  /// Order cancelled or expired
  Cancelled
  /// Order expired (buyer didn't pay in time)
  Expired
}

/// Convert status to string
pub fn status_to_string(status: OrderStatus) -> String {
  case status {
    Open -> "open"
    Locked -> "locked"
    FiatSent -> "fiat_sent"
    Completed -> "completed"
    Disputed -> "disputed"
    Cancelled -> "cancelled"
    Expired -> "expired"
  }
}

/// Parse status from string
pub fn status_from_string(s: String) -> OrderStatus {
  case s {
    "open" -> Open
    "locked" -> Locked
    "fiat_sent" -> FiatSent
    "completed" -> Completed
    "disputed" -> Disputed
    "cancelled" -> Cancelled
    "expired" -> Expired
    _ -> Open
  }
}

// =============================================================================
// CRYPTO CURRENCY
// =============================================================================

/// Supported cryptocurrencies for P2P
pub type CryptoCurrency {
  TON
  USDT
  USDC
  NOT
}

pub fn crypto_to_string(crypto: CryptoCurrency) -> String {
  case crypto {
    TON -> "TON"
    USDT -> "USDT"
    USDC -> "USDC"
    NOT -> "NOT"
  }
}

pub fn crypto_from_string(s: String) -> CryptoCurrency {
  case s {
    "TON" -> TON
    "USDT" -> USDT
    "USDC" -> USDC
    "NOT" -> NOT
    _ -> USDT
  }
}

// =============================================================================
// FIAT CURRENCY
// =============================================================================

/// Supported fiat currencies
pub type FiatCurrency {
  THB  // Thai Baht
  RUB  // Russian Ruble
  USD  // US Dollar
  EUR  // Euro
}

pub fn fiat_to_string(fiat: FiatCurrency) -> String {
  case fiat {
    THB -> "THB"
    RUB -> "RUB"
    USD -> "USD"
    EUR -> "EUR"
  }
}

pub fn fiat_from_string(s: String) -> FiatCurrency {
  case s {
    "THB" -> THB
    "RUB" -> RUB
    "USD" -> USD
    "EUR" -> EUR
    _ -> THB
  }
}

/// Get fiat currency symbol
pub fn fiat_symbol(fiat: FiatCurrency) -> String {
  case fiat {
    THB -> "฿"
    RUB -> "₽"
    USD -> "$"
    EUR -> "€"
  }
}

// =============================================================================
// PAYMENT METHOD
// =============================================================================

/// Supported payment methods
pub type PaymentMethod {
  BangkokBank
  PromptPay
  Sberbank
  Tinkoff
  Wise
  BankTransfer
}

pub fn payment_method_to_string(method: PaymentMethod) -> String {
  case method {
    BangkokBank -> "bangkok_bank"
    PromptPay -> "promptpay"
    Sberbank -> "sberbank"
    Tinkoff -> "tinkoff"
    Wise -> "wise"
    BankTransfer -> "bank_transfer"
  }
}

pub fn payment_method_from_string(s: String) -> PaymentMethod {
  case s {
    "bangkok_bank" -> BangkokBank
    "promptpay" -> PromptPay
    "sberbank" -> Sberbank
    "tinkoff" -> Tinkoff
    "wise" -> Wise
    "bank_transfer" -> BankTransfer
    _ -> BankTransfer
  }
}

/// Get payment method display name
pub fn payment_method_display(method: PaymentMethod) -> String {
  case method {
    BangkokBank -> "Bangkok Bank"
    PromptPay -> "PromptPay"
    Sberbank -> "Сбербанк"
    Tinkoff -> "Тинькофф"
    Wise -> "Wise"
    BankTransfer -> "Bank Transfer"
  }
}

// =============================================================================
// P2P ORDER
// =============================================================================

/// P2P Order structure (supports both ETS in-memory and PostgreSQL)
pub type P2POrder {
  P2POrder(
    /// Unique order ID (e.g., "P2P-123456789-1702500000")
    id: String,
    /// Seller's Telegram ID
    seller_telegram_id: Int,
    /// Seller's TON wallet address
    seller_wallet: String,
    /// Buyer's Telegram ID (None if order is open)
    buyer_telegram_id: Option(Int),
    /// Buyer's TON wallet address
    buyer_wallet: Option(String),
    /// Cryptocurrency being sold
    crypto: CryptoCurrency,
    /// Amount of crypto
    crypto_amount: Float,
    /// Fiat currency
    fiat: FiatCurrency,
    /// Fiat amount
    fiat_amount: Float,
    /// Payment method
    payment_method: PaymentMethod,
    /// Payment details (bank account, etc.)
    payment_details: String,
    /// Order status
    status: OrderStatus,
    /// Escrow transaction hash (when crypto is deposited)
    escrow_tx_hash: Option(String),
    /// Release transaction hash (when crypto is released)
    release_tx_hash: Option(String),
    /// Escrow smart contract address (for PostgreSQL)
    escrow_address: Option(String),
    /// Created timestamp (Unix seconds for ETS, ISO string converted)
    created_at: Int,
    /// Locked timestamp (when buyer takes order)
    locked_at: Option(Int),
    /// Completed timestamp
    completed_at: Option(Int),
    /// Expiration time in minutes
    expires_in_minutes: Int,
    /// Seller's rating (0-5) - cached from trader profile
    seller_rating: Float,
    /// Seller's completed trades count - cached from trader profile
    seller_trades: Int,
  )
}

// =============================================================================
// TRADER PROFILE
// =============================================================================

/// Trader profile with reputation
pub type TraderProfile {
  TraderProfile(
    telegram_id: Int,
    username: Option(String),
    wallet_address: String,
    /// Number of completed trades
    completed_trades: Int,
    /// Total volume traded (in USD equivalent)
    total_volume_usd: Float,
    /// Average rating (0-5)
    rating: Float,
    /// Number of ratings received
    rating_count: Int,
    /// Number of disputes (as seller)
    disputes_as_seller: Int,
    /// Number of disputes (as buyer)
    disputes_as_buyer: Int,
    /// Account created timestamp (Unix)
    created_at: Int,
    /// Last active timestamp (Unix)
    last_active: Int,
    /// Is verified trader
    is_verified: Bool,
  )
}

// =============================================================================
// DISPUTE
// =============================================================================

/// Dispute reason
pub type DisputeReason {
  PaymentNotReceived
  WrongAmount
  PaymentDelayed
  ScamSuspicion
  Other
}

pub fn dispute_reason_to_string(reason: DisputeReason) -> String {
  case reason {
    PaymentNotReceived -> "payment_not_received"
    WrongAmount -> "wrong_amount"
    PaymentDelayed -> "payment_delayed"
    ScamSuspicion -> "scam_suspicion"
    Other -> "other"
  }
}

/// Dispute structure
pub type Dispute {
  Dispute(
    id: String,
    order_id: String,
    /// Who opened the dispute
    opened_by: Int,
    reason: DisputeReason,
    description: String,
    /// Evidence (screenshot URLs, etc.)
    evidence: List(String),
    /// Resolution: "seller_wins", "buyer_wins", "split", "pending"
    resolution: Option(String),
    /// Arbitrator's Telegram ID
    arbitrator: Option(Int),
    created_at: Int,
    resolved_at: Option(Int),
  )
}

// =============================================================================
// ORACLE NOTIFICATION
// =============================================================================

/// Bank notification from Oracle
pub type BankNotification {
  BankNotification(
    /// Source: "sms", "email", "api"
    source: String,
    /// Bank name
    bank: String,
    /// Amount received
    amount: Float,
    /// Currency
    currency: FiatCurrency,
    /// Sender name/reference
    sender_ref: String,
    /// Transaction reference/comment
    tx_reference: Option(String),
    /// Raw message text
    raw_message: String,
    /// Timestamp when received
    received_at: Int,
  )
}

// =============================================================================
// PRICING
// =============================================================================

/// Market rate for crypto/fiat pair
pub type MarketRate {
  MarketRate(
    crypto: CryptoCurrency,
    fiat: FiatCurrency,
    /// Buy rate (how much fiat for 1 crypto)
    buy_rate: Float,
    /// Sell rate
    sell_rate: Float,
    /// Last updated timestamp
    updated_at: Int,
  )
}

/// Default rates (approximate, should be fetched from API)
pub fn default_rates() -> List(MarketRate) {
  let now = current_timestamp()
  [
    // USDT rates
    MarketRate(USDT, THB, 34.50, 35.00, now),
    MarketRate(USDT, RUB, 92.0, 94.0, now),
    MarketRate(USDT, USD, 1.0, 1.0, now),
    // TON rates (approximate)
    MarketRate(TON, THB, 200.0, 210.0, now),
    MarketRate(TON, RUB, 550.0, 570.0, now),
    MarketRate(TON, USD, 6.0, 6.2, now),
  ]
}

// =============================================================================
// PLATFORM SETTINGS
// =============================================================================

/// P2P Platform configuration
pub type P2PConfig {
  P2PConfig(
    /// Platform fee percentage (e.g., 0.5 = 0.5%)
    fee_percent: Float,
    /// Minimum order amount in USD equivalent
    min_order_usd: Float,
    /// Maximum order amount in USD equivalent
    max_order_usd: Float,
    /// Default order expiration in minutes
    default_expiry_minutes: Int,
    /// Payment timeout in minutes (after buyer takes order)
    payment_timeout_minutes: Int,
    /// Auto-confirm threshold (orders below this are auto-confirmed by oracle)
    auto_confirm_threshold_usd: Float,
    /// Escrow contract address
    escrow_contract_address: String,
    /// Oracle wallet address
    oracle_address: String,
    /// Platform wallet address (for fees)
    platform_wallet: String,
  )
}

/// Default platform configuration
pub fn default_config() -> P2PConfig {
  P2PConfig(
    fee_percent: 0.5,
    min_order_usd: 10.0,
    max_order_usd: 10_000.0,
    default_expiry_minutes: 1440,  // 24 hours
    payment_timeout_minutes: 30,
    auto_confirm_threshold_usd: 500.0,
    escrow_contract_address: "",  // Set from env
    oracle_address: "",  // Set from env
    platform_wallet: "",  // Set from env
  )
}

// =============================================================================
// HELPERS
// =============================================================================

/// Calculate exchange rate from order
pub fn calculate_rate(order: P2POrder) -> Float {
  order.fiat_amount /. order.crypto_amount
}

/// Calculate platform fee
pub fn calculate_fee(crypto_amount: Float, fee_percent: Float) -> Float {
  crypto_amount *. fee_percent /. 100.0
}

/// Check if order is expired
pub fn is_order_expired(order: P2POrder, current_time: Int) -> Bool {
  case order.status, order.locked_at {
    Locked, Some(locked_time) -> {
      let expiry = locked_time + order.expires_in_minutes * 60
      current_time > expiry
    }
    Open, _ -> {
      let expiry = order.created_at + order.expires_in_minutes * 60
      current_time > expiry
    }
    _, _ -> False
  }
}

/// Generate order ID
pub fn generate_order_id() -> String {
  generate_order_id_ffi()
}

// =============================================================================
// FFI
// =============================================================================

@external(erlang, "vibee_p2p_ffi", "generate_order_id")
fn generate_order_id_ffi() -> String

@external(erlang, "vibee_p2p_ffi", "current_timestamp")
fn current_timestamp() -> Int
