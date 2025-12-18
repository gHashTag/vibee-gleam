// Robokassa Payment Integration for VIBEE MCP
// Russian payment gateway with MD5 signature validation

import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/float
import gleam/int
import gleam/string
import vibee/logging

// =============================================================================
// CONFIGURATION
// =============================================================================

/// Robokassa configuration from environment
pub type RobokassaConfig {
  RobokassaConfig(
    merchant_login: String,
    password1: String,
    password2: String,
    test_mode: Bool,
  )
}

/// Get Robokassa config from environment
pub fn get_config() -> RobokassaConfig {
  let config_map = get_config_ffi()

  let merchant_login = get_map_string(config_map, "merchant_login") |> result.unwrap("")
  let password1 = get_map_string(config_map, "password1") |> result.unwrap("")
  let password2 = get_map_string(config_map, "password2") |> result.unwrap("")
  let test_mode_str = get_map_string(config_map, "test_mode") |> result.unwrap("true")

  RobokassaConfig(
    merchant_login: merchant_login,
    password1: password1,
    password2: password2,
    test_mode: test_mode_str == "true" || test_mode_str == "1",
  )
}

// =============================================================================
// SIGNATURE VALIDATION
// =============================================================================

/// Validate Robokassa webhook signature
/// Formula: MD5(OutSum:InvId:Password2)
pub fn validate_signature(
  out_sum: String,
  inv_id: String,
  signature: String,
) -> Bool {
  let config = get_config()

  case config.password2 {
    "" -> {
      logging.quick_warn("[ROBOKASSA] Password2 not configured")
      False
    }
    password2 -> {
      let is_valid = validate_signature_ffi(out_sum, inv_id, password2, signature)

      case is_valid {
        True -> logging.quick_info("[ROBOKASSA] Signature valid for inv_id: " <> inv_id)
        False -> logging.quick_warn("[ROBOKASSA] Invalid signature for inv_id: " <> inv_id)
      }

      is_valid
    }
  }
}

/// Calculate MD5 hash (for testing)
pub fn md5_hash(data: String) -> String {
  md5_hash_ffi(data)
}

// =============================================================================
// PAYMENT URL GENERATION
// =============================================================================

/// Generate Robokassa payment URL
pub fn generate_payment_url(
  amount: Float,
  inv_id: String,
  description: String,
) -> Result(String, String) {
  let config = get_config()

  case config.merchant_login, config.password1 {
    "", _ -> Error("ROBOKASSA_MERCHANT_LOGIN not configured")
    _, "" -> Error("ROBOKASSA_PASSWORD1 not configured")
    merchant_login, password1 -> {
      // Format amount to 2 decimal places
      let amount_str = format_amount(amount)

      let url = generate_payment_url_ffi(
        merchant_login,
        amount_str,
        inv_id,
        description,
        password1,
      )

      logging.quick_info("[ROBOKASSA] Generated payment URL for inv_id: " <> inv_id <> ", amount: " <> amount_str)

      Ok(url)
    }
  }
}

/// Generate payment URL with custom config (for testing)
pub fn generate_payment_url_with_config(
  config: RobokassaConfig,
  amount: Float,
  inv_id: String,
  description: String,
) -> String {
  let amount_str = format_amount(amount)

  generate_payment_url_ffi(
    config.merchant_login,
    amount_str,
    inv_id,
    description,
    config.password1,
  )
}

// =============================================================================
// HELPERS
// =============================================================================

/// Format amount to string with 2 decimal places (e.g., "100.00")
fn format_amount(amount: Float) -> String {
  // Round to 2 decimal places
  let cents = float.round(amount *. 100.0)
  let rubles = cents / 100
  let kopecks = cents % 100

  int.to_string(rubles) <> "." <> pad_left(int.to_string(kopecks), 2, "0")
}

/// Pad string on the left with character
fn pad_left(s: String, len: Int, char: String) -> String {
  let current_len = string.length(s)
  case current_len < len {
    True -> pad_left(char <> s, len, char)
    False -> s
  }
}

/// Parse Robokassa webhook parameters
pub type RobokassaWebhookParams {
  RobokassaWebhookParams(
    out_sum: String,
    inv_id: String,
    signature_value: String,
  )
}

/// Validate webhook and extract parameters
pub fn parse_webhook(
  out_sum: Option(String),
  inv_id: Option(String),
  signature_value: Option(String),
) -> Result(RobokassaWebhookParams, String) {
  case out_sum, inv_id, signature_value {
    Some(os), Some(id), Some(sig) -> {
      Ok(RobokassaWebhookParams(
        out_sum: os,
        inv_id: id,
        signature_value: sig,
      ))
    }
    _, _, _ -> Error("Missing required webhook parameters: OutSum, InvId, SignatureValue")
  }
}

// =============================================================================
// FFI BINDINGS
// =============================================================================

@external(erlang, "vibee_robokassa_ffi", "md5_hash")
fn md5_hash_ffi(data: String) -> String

@external(erlang, "vibee_robokassa_ffi", "validate_signature")
fn validate_signature_ffi(
  out_sum: String,
  inv_id: String,
  password2: String,
  signature: String,
) -> Bool

@external(erlang, "vibee_robokassa_ffi", "generate_payment_url")
fn generate_payment_url_ffi(
  merchant_login: String,
  out_sum: String,
  inv_id: String,
  description: String,
  password1: String,
) -> String

@external(erlang, "vibee_robokassa_ffi", "get_config")
fn get_config_ffi() -> Dynamic

// Helper to get string from dynamic map (using FFI)
fn get_map_string(map: Dynamic, key: String) -> Result(String, Nil) {
  get_map_string_ffi(map, key)
}

@external(erlang, "vibee_payment_tools_ffi", "get_map_field")
fn get_map_string_ffi(map: Dynamic, key: String) -> Result(String, Nil)
