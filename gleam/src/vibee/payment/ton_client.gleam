// TON Blockchain Payment Client for VIBEE MCP
// Supports USDT Jetton and Native TON payments

import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/list
import gleam/int
import gleam/float
import gleam/string
import gleam/json
import gleam/http/request
import vibee/logging
import vibee/payment/types.{
  type TonTransaction, TonTransaction,
  usdt_to_nano, nano_to_usdt, ton_to_nano, nano_to_ton,
  usdt_master_address,
}

// =============================================================================
// CONFIGURATION
// =============================================================================

/// TON configuration from environment
pub type TonConfig {
  TonConfig(
    wallet_address: String,
    api_endpoint: String,
    api_key: String,
    usdt_master_address: String,
    network: String,
  )
}

/// Get TON config from environment
pub fn get_config() -> TonConfig {
  let wallet = get_env("TON_WALLET_ADDRESS", "")
  let api_key = get_env("TON_API_KEY", "")
  let network = get_env("TON_NETWORK", "mainnet")

  let endpoint = case network {
    "testnet" -> "https://testnet.toncenter.com/api/v2"
    _ -> "https://toncenter.com/api/v2"
  }

  TonConfig(
    wallet_address: wallet,
    api_endpoint: endpoint,
    api_key: api_key,
    usdt_master_address: usdt_master_address,
    network: network,
  )
}

// =============================================================================
// TONKEEPER DEEP LINKS
// =============================================================================

/// Generate Tonkeeper deep link for USDT payment
pub fn generate_tonkeeper_usdt_link(
  recipient: String,
  amount_usdt: Float,
  comment: String,
) -> String {
  // USDT has 6 decimals
  let nano_amount = usdt_to_nano(amount_usdt)

  // Tonkeeper link format for jetton transfer
  // tonkeeper://transfer/<recipient>?jetton=<usdt_master>&amount=<nano>&text=<comment>
  let base = "tonkeeper://transfer/" <> recipient
  let params = "?jetton=" <> usdt_master_address
    <> "&amount=" <> int.to_string(nano_amount)
    <> "&text=" <> url_encode(comment)

  base <> params
}

/// Generate Tonkeeper deep link for native TON payment
pub fn generate_tonkeeper_ton_link(
  recipient: String,
  amount_ton: Float,
  comment: String,
) -> String {
  // TON has 9 decimals
  let nano_amount = ton_to_nano(amount_ton)

  // Tonkeeper link format for TON transfer
  // tonkeeper://transfer/<recipient>?amount=<nano>&text=<comment>
  let base = "tonkeeper://transfer/" <> recipient
  let params = "?amount=" <> int.to_string(nano_amount)
    <> "&text=" <> url_encode(comment)

  base <> params
}

/// Generate web link for Tonkeeper (for browsers without app)
pub fn generate_tonkeeper_web_link(
  recipient: String,
  amount: Float,
  comment: String,
  is_usdt: Bool,
) -> String {
  case is_usdt {
    True -> {
      let nano_amount = usdt_to_nano(amount)
      "https://app.tonkeeper.com/transfer/" <> recipient
        <> "?jetton=" <> usdt_master_address
        <> "&amount=" <> int.to_string(nano_amount)
        <> "&text=" <> url_encode(comment)
    }
    False -> {
      let nano_amount = ton_to_nano(amount)
      "https://app.tonkeeper.com/transfer/" <> recipient
        <> "?amount=" <> int.to_string(nano_amount)
        <> "&text=" <> url_encode(comment)
    }
  }
}

// =============================================================================
// TRANSACTION VERIFICATION (via TON Center API)
// =============================================================================

/// Find payment by comment (invoice ID) in recent transactions
/// This polls the TON Center API to find a matching transaction
pub fn find_payment_by_comment(
  wallet_address: String,
  expected_comment: String,
  expected_amount: Float,
  is_usdt: Bool,
) -> Result(Option(TonTransaction), String) {
  let config = get_config()

  case config.api_key {
    "" -> Error("TON_API_KEY not configured")
    _ -> {
      logging.quick_info("[TON] Searching for payment with comment: " <> expected_comment)

      // In production, this would make HTTP request to TON Center API
      // For now, return mock not found (actual implementation needs hackney/httpc)
      // The actual verification should be done via webhook or periodic polling

      Ok(None)
    }
  }
}

/// Check if a specific transaction hash exists and is valid
pub fn verify_transaction(
  tx_hash: String,
  expected_amount: Float,
  expected_comment: String,
  is_usdt: Bool,
) -> Result(Bool, String) {
  let config = get_config()

  case config.api_key {
    "" -> Error("TON_API_KEY not configured")
    _ -> {
      logging.quick_info("[TON] Verifying transaction: " <> tx_hash)

      // In production, verify via TON Center API
      // GET /getTransactions with address and check tx hash

      Ok(False)
    }
  }
}

// =============================================================================
// PAYMENT CREATION HELPERS
// =============================================================================

/// Payment link result
pub type PaymentLinkResult {
  PaymentLinkResult(
    deep_link: String,
    web_link: String,
    wallet_address: String,
    amount: Float,
    comment: String,
    currency: String,
  )
}

/// Create USDT payment links
pub fn create_usdt_payment(
  amount_usdt: Float,
  inv_id: String,
) -> Result(PaymentLinkResult, String) {
  let config = get_config()

  case config.wallet_address {
    "" -> Error("TON_WALLET_ADDRESS not configured")
    wallet -> {
      let deep_link = generate_tonkeeper_usdt_link(wallet, amount_usdt, inv_id)
      let web_link = generate_tonkeeper_web_link(wallet, amount_usdt, inv_id, True)

      logging.quick_info("[TON] Created USDT payment link for inv_id: " <> inv_id <> ", amount: $" <> float.to_string(amount_usdt))

      Ok(PaymentLinkResult(
        deep_link: deep_link,
        web_link: web_link,
        wallet_address: wallet,
        amount: amount_usdt,
        comment: inv_id,
        currency: "USDT",
      ))
    }
  }
}

/// Create native TON payment links
pub fn create_ton_payment(
  amount_ton: Float,
  inv_id: String,
) -> Result(PaymentLinkResult, String) {
  let config = get_config()

  case config.wallet_address {
    "" -> Error("TON_WALLET_ADDRESS not configured")
    wallet -> {
      let deep_link = generate_tonkeeper_ton_link(wallet, amount_ton, inv_id)
      let web_link = generate_tonkeeper_web_link(wallet, amount_ton, inv_id, False)

      logging.quick_info("[TON] Created TON payment link for inv_id: " <> inv_id <> ", amount: " <> float.to_string(amount_ton) <> " TON")

      Ok(PaymentLinkResult(
        deep_link: deep_link,
        web_link: web_link,
        wallet_address: wallet,
        amount: amount_ton,
        comment: inv_id,
        currency: "TON",
      ))
    }
  }
}

// =============================================================================
// JSON SERIALIZATION
// =============================================================================

/// Convert PaymentLinkResult to JSON
pub fn payment_link_to_json(result: PaymentLinkResult) -> json.Json {
  json.object([
    #("deep_link", json.string(result.deep_link)),
    #("web_link", json.string(result.web_link)),
    #("wallet_address", json.string(result.wallet_address)),
    #("amount", json.float(result.amount)),
    #("comment", json.string(result.comment)),
    #("currency", json.string(result.currency)),
  ])
}

// =============================================================================
// HELPERS
// =============================================================================

/// Simple URL encoding for comments
fn url_encode(s: String) -> String {
  s
  |> string.replace(" ", "%20")
  |> string.replace("&", "%26")
  |> string.replace("=", "%3D")
  |> string.replace("?", "%3F")
  |> string.replace("#", "%23")
}

/// Get environment variable
@external(erlang, "vibee_ton_ffi", "get_env")
fn get_env(key: String, default: String) -> String

// =============================================================================
// TON PRICE (approximate, for display)
// =============================================================================

/// Current approximate TON price in USD (should be fetched from API in production)
pub const ton_price_usd = 6.0

/// Convert TON amount to approximate USD value
pub fn ton_to_usd(ton_amount: Float) -> Float {
  ton_amount *. ton_price_usd
}

/// Convert USD value to approximate TON amount
pub fn usd_to_ton(usd_amount: Float) -> Float {
  usd_amount /. ton_price_usd
}
