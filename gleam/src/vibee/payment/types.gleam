// Payment Types for VIBEE MCP
// Supports: Telegram Stars, Robokassa, TON USDT, TON Native

import gleam/option.{type Option}
import gleam/json

// =============================================================================
// ENUMS
// =============================================================================

/// Payment methods supported by VIBEE
pub type PaymentMethod {
  Telegram
  Robokassa
  TonUsdt
  TonNative
}

/// Payment status
pub type PaymentStatus {
  Pending
  Completed
  Failed
  Cancelled
}

/// Supported currencies
pub type Currency {
  XTR      // Telegram Stars
  RUB      // Russian Ruble (Robokassa)
  USDT     // USDT on TON blockchain
  TON      // Native TON coin
}

// =============================================================================
// TYPES
// =============================================================================

/// Payment record
pub type Payment {
  Payment(
    id: String,
    telegram_id: Int,
    amount: Float,
    stars: Int,
    currency: Currency,
    status: PaymentStatus,
    method: PaymentMethod,
    inv_id: String,
    invoice_url: Option(String),
    description: Option(String),
    created_at: Int,
    completed_at: Option(Int),
  )
}

/// Top-up option for display
pub type TopUpOption {
  TopUpOption(
    amount: Float,
    stars: Int,
    label: String,
  )
}

/// TON transaction from blockchain
pub type TonTransaction {
  TonTransaction(
    hash: String,
    lt: String,
    timestamp: Int,
    from_address: String,
    to_address: String,
    amount: Int,
    comment: String,
    is_incoming: Bool,
  )
}

// =============================================================================
// CONVERSIONS
// =============================================================================

/// Convert PaymentMethod to string
pub fn method_to_string(method: PaymentMethod) -> String {
  case method {
    Telegram -> "telegram"
    Robokassa -> "robokassa"
    TonUsdt -> "ton_usdt"
    TonNative -> "ton_native"
  }
}

/// Parse PaymentMethod from string
pub fn method_from_string(s: String) -> PaymentMethod {
  case s {
    "telegram" -> Telegram
    "robokassa" -> Robokassa
    "ton_usdt" -> TonUsdt
    "ton_native" -> TonNative
    _ -> Telegram
  }
}

/// Convert PaymentStatus to string
pub fn status_to_string(status: PaymentStatus) -> String {
  case status {
    Pending -> "PENDING"
    Completed -> "COMPLETED"
    Failed -> "FAILED"
    Cancelled -> "CANCELLED"
  }
}

/// Parse PaymentStatus from string
pub fn status_from_string(s: String) -> PaymentStatus {
  case s {
    "PENDING" -> Pending
    "COMPLETED" -> Completed
    "FAILED" -> Failed
    "CANCELLED" -> Cancelled
    _ -> Pending
  }
}

/// Convert Currency to string
pub fn currency_to_string(currency: Currency) -> String {
  case currency {
    XTR -> "XTR"
    RUB -> "RUB"
    USDT -> "USDT"
    TON -> "TON"
  }
}

/// Parse Currency from string
pub fn currency_from_string(s: String) -> Currency {
  case s {
    "XTR" -> XTR
    "RUB" -> RUB
    "USDT" -> USDT
    "USDT_TON" -> USDT
    "TON" -> TON
    _ -> XTR
  }
}

// =============================================================================
// JSON SERIALIZATION
// =============================================================================

/// Convert Payment to JSON
pub fn payment_to_json(p: Payment) -> json.Json {
  json.object([
    #("id", json.string(p.id)),
    #("telegram_id", json.int(p.telegram_id)),
    #("amount", json.float(p.amount)),
    #("stars", json.int(p.stars)),
    #("currency", json.string(currency_to_string(p.currency))),
    #("status", json.string(status_to_string(p.status))),
    #("method", json.string(method_to_string(p.method))),
    #("inv_id", json.string(p.inv_id)),
    #("invoice_url", case p.invoice_url {
      option.Some(url) -> json.string(url)
      option.None -> json.null()
    }),
    #("description", case p.description {
      option.Some(desc) -> json.string(desc)
      option.None -> json.null()
    }),
    #("created_at", json.int(p.created_at)),
    #("completed_at", case p.completed_at {
      option.Some(t) -> json.int(t)
      option.None -> json.null()
    }),
  ])
}

/// Convert TopUpOption to JSON
pub fn option_to_json(opt: TopUpOption) -> json.Json {
  json.object([
    #("amount", json.float(opt.amount)),
    #("stars", json.int(opt.stars)),
    #("label", json.string(opt.label)),
  ])
}

/// Convert TonTransaction to JSON
pub fn transaction_to_json(tx: TonTransaction) -> json.Json {
  json.object([
    #("hash", json.string(tx.hash)),
    #("lt", json.string(tx.lt)),
    #("timestamp", json.int(tx.timestamp)),
    #("from", json.string(tx.from_address)),
    #("to", json.string(tx.to_address)),
    #("amount", json.int(tx.amount)),
    #("comment", json.string(tx.comment)),
    #("is_incoming", json.bool(tx.is_incoming)),
  ])
}

// =============================================================================
// PRICING OPTIONS (from 999-multibots-telegraf)
// =============================================================================

/// Robokassa (RUB) top-up options
pub fn rub_options() -> List(TopUpOption) {
  [
    TopUpOption(amount: 100.0, stars: 43, label: "100₽"),
    TopUpOption(amount: 500.0, stars: 217, label: "500₽"),
    TopUpOption(amount: 1000.0, stars: 434, label: "1000₽"),
    TopUpOption(amount: 2000.0, stars: 869, label: "2000₽"),
    TopUpOption(amount: 5000.0, stars: 2173, label: "5000₽"),
    TopUpOption(amount: 10000.0, stars: 4347, label: "10000₽"),
  ]
}

/// TON USDT top-up options
pub fn usdt_options() -> List(TopUpOption) {
  [
    TopUpOption(amount: 5.0, stars: 217, label: "$5"),
    TopUpOption(amount: 10.0, stars: 434, label: "$10"),
    TopUpOption(amount: 25.0, stars: 1085, label: "$25"),
    TopUpOption(amount: 50.0, stars: 2170, label: "$50"),
    TopUpOption(amount: 100.0, stars: 4340, label: "$100"),
    TopUpOption(amount: 500.0, stars: 21700, label: "$500"),
  ]
}

/// TON Native top-up options (at ~$6/TON rate)
pub fn ton_options() -> List(TopUpOption) {
  [
    TopUpOption(amount: 1.0, stars: 26, label: "1 TON"),
    TopUpOption(amount: 5.0, stars: 130, label: "5 TON"),
    TopUpOption(amount: 10.0, stars: 260, label: "10 TON"),
    TopUpOption(amount: 50.0, stars: 1300, label: "50 TON"),
    TopUpOption(amount: 100.0, stars: 2600, label: "100 TON"),
  ]
}

/// Telegram Stars options
pub fn stars_options() -> List(TopUpOption) {
  [
    TopUpOption(amount: 50.0, stars: 50, label: "50⭐"),
    TopUpOption(amount: 100.0, stars: 100, label: "100⭐"),
    TopUpOption(amount: 250.0, stars: 250, label: "250⭐"),
    TopUpOption(amount: 500.0, stars: 500, label: "500⭐"),
    TopUpOption(amount: 1000.0, stars: 1000, label: "1000⭐"),
    TopUpOption(amount: 2500.0, stars: 2500, label: "2500⭐"),
  ]
}

/// Get top-up options for a payment method
pub fn get_options_for_method(method: PaymentMethod) -> List(TopUpOption) {
  case method {
    Telegram -> stars_options()
    Robokassa -> rub_options()
    TonUsdt -> usdt_options()
    TonNative -> ton_options()
  }
}

// =============================================================================
// CONSTANTS
// =============================================================================

/// USDT Master contract on TON mainnet
pub const usdt_master_address = "EQCxE6mUtQJKFnGfaROTKOt1lZbDiiX1kCixRv7Nw2Id_sDs"

/// USDT decimals (6 for TON USDT)
pub const usdt_decimals = 6

/// TON decimals (9 for native TON)
pub const ton_decimals = 9

/// Convert USDT to nano (6 decimals)
pub fn usdt_to_nano(amount: Float) -> Int {
  float_to_int(amount *. 1000000.0)
}

/// Convert nano to USDT
pub fn nano_to_usdt(nano: Int) -> Float {
  int_to_float(nano) /. 1000000.0
}

/// Convert TON to nano (9 decimals)
pub fn ton_to_nano(amount: Float) -> Int {
  float_to_int(amount *. 1000000000.0)
}

/// Convert nano to TON
pub fn nano_to_ton(nano: Int) -> Float {
  int_to_float(nano) /. 1000000000.0
}

// Helper functions
fn float_to_int(f: Float) -> Int {
  float_round(f)
}

fn int_to_float(i: Int) -> Float {
  gleam_stdlib_int_to_float(i)
}

@external(erlang, "erlang", "round")
fn float_round(f: Float) -> Int

@external(erlang, "erlang", "float")
fn gleam_stdlib_int_to_float(i: Int) -> Float
