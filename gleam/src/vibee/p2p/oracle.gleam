// VIBEE P2P Oracle
// Oracle for automatic fiat payment verification

import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/int
import gleam/float
import gleam/string
import gleam/list
import gleam/json
import gleam/dynamic.{type Dynamic}
import vibee/logging
import vibee/p2p/types.{
  type BankNotification, type FiatCurrency, type P2POrder,
  BankNotification, THB, RUB, USD, EUR,
  fiat_from_string, fiat_to_string,
}
import vibee/p2p/escrow_client

// =============================================================================
// ORACLE CONFIGURATION
// =============================================================================

/// Oracle configuration
pub type OracleConfig {
  OracleConfig(
    /// Oracle wallet address (for signing)
    wallet_address: String,
    /// Private key for signing (hex)
    private_key: String,
    /// Auto-confirm threshold in USD
    auto_confirm_threshold_usd: Float,
    /// Supported banks
    supported_banks: List(String),
    /// Tolerance for amount matching (e.g., 0.01 = 1%)
    amount_tolerance: Float,
  )
}

/// Default oracle config
pub fn default_oracle_config() -> OracleConfig {
  OracleConfig(
    wallet_address: get_env("P2P_ORACLE_ADDRESS", ""),
    private_key: get_env("P2P_ORACLE_PRIVATE_KEY", ""),
    auto_confirm_threshold_usd: 500.0,
    supported_banks: ["bangkok_bank", "kasikorn", "scb", "krungthai", "sberbank", "tinkoff"],
    amount_tolerance: 0.01,
  )
}

// =============================================================================
// BANK NOTIFICATION PROCESSING
// =============================================================================

/// Process incoming bank notification (from SMS/email/webhook)
pub fn process_notification(notification: BankNotification) -> Result(String, String) {
  logging.info("[ORACLE] Processing notification from " <> notification.bank <> ": " <> float.to_string(notification.amount) <> " " <> fiat_to_string(notification.currency))

  // Find matching order
  case find_matching_order(notification) {
    Ok(order) -> {
      logging.info("[ORACLE] Found matching order: " <> order.id)

      // Verify amount matches (with tolerance)
      case verify_amount(notification.amount, order.fiat_amount) {
        True -> {
          // Auto-confirm the order
          case auto_confirm_order(order.id) {
            Ok(msg) -> Ok(msg)
            Error(e) -> Error("Failed to auto-confirm: " <> e)
          }
        }
        False -> Error("Amount mismatch: expected " <> float.to_string(order.fiat_amount) <> ", got " <> float.to_string(notification.amount))
      }
    }
    Error(e) -> Error("No matching order found: " <> e)
  }
}

/// Find order matching the notification
fn find_matching_order(notification: BankNotification) -> Result(P2POrder, String) {
  // Strategy 1: Match by tx_reference (order ID in comment)
  case notification.tx_reference {
    Some(ref) -> {
      // Try to find order by ID from ETS
      case get_order_by_id_ffi(ref) {
        Ok(order_dyn) -> decode_p2p_order(order_dyn)
        Error(_) -> {
          // Fallback to amount-based matching
          find_order_by_amount(notification)
        }
      }
    }
    None -> {
      // Strategy 2: Match by amount + currency + time window
      find_order_by_amount(notification)
    }
  }
}

/// Find locked order by amount and currency
fn find_order_by_amount(notification: BankNotification) -> Result(P2POrder, String) {
  // Get all locked orders (status = "locked")
  let orders = list_orders_by_status_ffi("locked")

  // Find orders matching fiat amount and currency within tolerance window
  let config = default_oracle_config()
  let tolerance = notification.amount *. config.amount_tolerance
  let currency_str = fiat_to_string(notification.currency)

  // Filter orders that match amount and currency
  let matching = list.filter(orders, fn(order_dyn) {
    let order_currency = get_map_string_ffi(order_dyn, "fiat")
    let order_amount = get_map_float_ffi(order_dyn, "fiat_amount")
    let amount_diff = case order_amount >. notification.amount {
      True -> order_amount -. notification.amount
      False -> notification.amount -. order_amount
    }

    // Match if same currency and amount within tolerance
    order_currency == currency_str && amount_diff <=. tolerance
  })

  case matching {
    [first_match, ..] -> decode_p2p_order(first_match)
    [] -> Error("No locked orders matching amount " <> float.to_string(notification.amount) <> " " <> currency_str)
  }
}

/// Decode P2POrder from Dynamic Erlang map
fn decode_p2p_order(order_dyn: Dynamic) -> Result(P2POrder, String) {
  let id = get_map_string_ffi(order_dyn, "id")
  let status_str = get_map_string_ffi(order_dyn, "status")
  let crypto_str = get_map_string_ffi(order_dyn, "crypto")
  let fiat_str = get_map_string_ffi(order_dyn, "fiat")

  // These functions return types directly, not Result
  let status = types.status_from_string(status_str)
  let crypto = types.crypto_from_string(crypto_str)
  let fiat = fiat_from_string(fiat_str)

  Ok(types.P2POrder(
    id: id,
    seller_telegram_id: get_map_int_ffi(order_dyn, "seller_telegram_id"),
    seller_wallet: get_map_string_ffi(order_dyn, "seller_wallet"),
    buyer_telegram_id: Some(get_map_int_ffi(order_dyn, "buyer_telegram_id")),
    buyer_wallet: Some(get_map_string_ffi(order_dyn, "buyer_wallet")),
    crypto: crypto,
    crypto_amount: get_map_float_ffi(order_dyn, "crypto_amount"),
    fiat: fiat,
    fiat_amount: get_map_float_ffi(order_dyn, "fiat_amount"),
    payment_method: types.payment_method_from_string(get_map_string_ffi(order_dyn, "payment_method")),
    payment_details: get_map_string_ffi(order_dyn, "payment_details"),
    status: status,
    escrow_tx_hash: Some(get_map_string_ffi(order_dyn, "escrow_tx_hash")),
    release_tx_hash: None,
    escrow_address: Some(get_map_string_ffi(order_dyn, "escrow_address")),
    created_at: get_map_int_ffi(order_dyn, "created_at"),
    locked_at: Some(get_map_int_ffi(order_dyn, "locked_at")),
    completed_at: None,
    expires_in_minutes: 30,
    seller_rating: 5.0,
    seller_trades: 0,
  ))
}

/// Verify amount matches (with tolerance)
fn verify_amount(received: Float, expected: Float) -> Bool {
  let config = default_oracle_config()
  let tolerance = expected *. config.amount_tolerance

  let diff = case received >. expected {
    True -> received -. expected
    False -> expected -. received
  }

  // Accept if within tolerance OR if received is greater (overpayment OK)
  diff <=. tolerance || received >=. expected
}

/// Auto-confirm order via oracle
fn auto_confirm_order(order_id: String) -> Result(String, String) {
  let config = default_oracle_config()

  // Generate oracle signature
  let signature = sign_confirmation(order_id, config.private_key)

  case escrow_client.oracle_confirm(order_id, signature) {
    Ok(order) -> {
      logging.info("[ORACLE] Auto-confirmed order " <> order_id)
      Ok("Order " <> order_id <> " confirmed automatically")
    }
    Error(e) -> Error(e)
  }
}

/// Sign confirmation with oracle private key
fn sign_confirmation(order_id: String, _private_key: String) -> String {
  // In production, this would sign with ED25519
  // For now, return mock signature
  "oracle_sig_" <> order_id
}

// =============================================================================
// SMS PARSING
// =============================================================================

/// Parse Bangkok Bank SMS format
/// Example: "รับโอน 3,500.00 บาท จาก บัญชี xxx เข้า xxx เมื่อ 13/12/67 20:30"
pub fn parse_bangkok_bank_sms(message: String) -> Result(BankNotification, String) {
  let now = current_timestamp()

  // Try to extract amount
  case extract_amount_thb(message) {
    Ok(amount) -> {
      // Try to extract reference (if any)
      let reference = extract_reference(message)

      Ok(BankNotification(
        source: "sms",
        bank: "bangkok_bank",
        amount: amount,
        currency: THB,
        sender_ref: "",
        tx_reference: reference,
        raw_message: message,
        received_at: now,
      ))
    }
    Error(_) -> Error("Could not parse amount from SMS")
  }
}

/// Parse PromptPay notification
/// Example: "PromptPay: รับเงิน 3,500.00 บาท จาก 0812345678 เมื่อ..."
pub fn parse_promptpay_sms(message: String) -> Result(BankNotification, String) {
  let now = current_timestamp()

  case extract_amount_thb(message) {
    Ok(amount) -> {
      let reference = extract_reference(message)

      Ok(BankNotification(
        source: "sms",
        bank: "promptpay",
        amount: amount,
        currency: THB,
        sender_ref: "",
        tx_reference: reference,
        raw_message: message,
        received_at: now,
      ))
    }
    Error(_) -> Error("Could not parse PromptPay notification")
  }
}

/// Parse Sberbank SMS (Russian)
/// Example: "СБЕРБАНК: зачисление 5000р от ИВАН И. Баланс: 12345.67р"
pub fn parse_sberbank_sms(message: String) -> Result(BankNotification, String) {
  let now = current_timestamp()

  case extract_amount_rub(message) {
    Ok(amount) -> {
      let reference = extract_reference(message)

      Ok(BankNotification(
        source: "sms",
        bank: "sberbank",
        amount: amount,
        currency: RUB,
        sender_ref: "",
        tx_reference: reference,
        raw_message: message,
        received_at: now,
      ))
    }
    Error(_) -> Error("Could not parse Sberbank SMS")
  }
}

/// Parse Tinkoff SMS (Russian)
/// Example: "Tinkoff: +5000р на карту *1234 от Иван И."
pub fn parse_tinkoff_sms(message: String) -> Result(BankNotification, String) {
  let now = current_timestamp()

  case extract_amount_rub(message) {
    Ok(amount) -> {
      let reference = extract_reference(message)

      Ok(BankNotification(
        source: "sms",
        bank: "tinkoff",
        amount: amount,
        currency: RUB,
        sender_ref: "",
        tx_reference: reference,
        raw_message: message,
        received_at: now,
      ))
    }
    Error(_) -> Error("Could not parse Tinkoff SMS")
  }
}

/// Generic SMS parser - auto-detect bank
pub fn parse_sms(message: String) -> Result(BankNotification, String) {
  let lower_msg = string.lowercase(message)

  case string.contains(lower_msg, "bangkok") {
    True -> parse_bangkok_bank_sms(message)
    False -> case string.contains(lower_msg, "promptpay") {
      True -> parse_promptpay_sms(message)
      False -> case string.contains(lower_msg, "сбербанк") || string.contains(lower_msg, "sberbank") {
        True -> parse_sberbank_sms(message)
        False -> case string.contains(lower_msg, "tinkoff") || string.contains(lower_msg, "тинькофф") {
          True -> parse_tinkoff_sms(message)
          False -> Error("Unknown bank format")
        }
      }
    }
  }
}

// =============================================================================
// AMOUNT EXTRACTION HELPERS
// =============================================================================

/// Extract THB amount from text
fn extract_amount_thb(text: String) -> Result(Float, String) {
  // Look for patterns like "3,500.00", "3500", "3,500"
  extract_amount_ffi(text, "thb")
}

/// Extract RUB amount from text
fn extract_amount_rub(text: String) -> Result(Float, String) {
  // Look for patterns like "5000р", "5 000", "5000.00"
  extract_amount_ffi(text, "rub")
}

/// Extract order reference from text
/// Looks for patterns like "P2P-123456", "ref: ABC123", etc.
fn extract_reference(text: String) -> Option(String) {
  // Look for P2P order ID pattern
  case string.contains(text, "P2P-") {
    True -> {
      // Extract P2P-XXXXX pattern
      extract_p2p_reference_ffi(text)
    }
    False -> None
  }
}

// =============================================================================
// WEBHOOK HANDLER
// =============================================================================

/// Handle incoming webhook from SMS gateway or bank
pub fn handle_webhook(payload: Dynamic) -> Result(String, String) {
  // Extract message from webhook payload
  case get_webhook_message(payload) {
    Ok(message) -> {
      case parse_sms(message) {
        Ok(notification) -> process_notification(notification)
        Error(e) -> Error("Failed to parse SMS: " <> e)
      }
    }
    Error(e) -> Error("Invalid webhook payload: " <> e)
  }
}

fn get_webhook_message(payload: Dynamic) -> Result(String, String) {
  // Try common webhook formats
  case get_field_ffi(payload, "message") {
    Ok(msg_dyn) -> {
      case decode_string_ffi(msg_dyn) {
        Ok(msg) -> Ok(msg)
        Error(_) -> Error("message field is not a string")
      }
    }
    Error(_) -> {
      // Try "text" field
      case get_field_ffi(payload, "text") {
        Ok(text_dyn) -> {
          case decode_string_ffi(text_dyn) {
            Ok(text) -> Ok(text)
            Error(_) -> Error("text field is not a string")
          }
        }
        Error(_) -> Error("No message or text field in payload")
      }
    }
  }
}

@external(erlang, "vibee_payment_tools_ffi", "get_string_value")
fn decode_string_ffi(value: Dynamic) -> Result(String, Nil)

// =============================================================================
// MANUAL CONFIRMATION (for testing)
// =============================================================================

/// Manually confirm a payment (for testing or fallback)
pub fn manual_confirm(
  order_id: String,
  confirmer_telegram_id: Int,
  amount_received: Float,
) -> Result(String, String) {
  logging.info("[ORACLE] Manual confirmation for order " <> order_id <> " by user " <> int.to_string(confirmer_telegram_id))

  // In production, verify the confirmer is authorized (admin or seller)
  auto_confirm_order(order_id)
}

// =============================================================================
// FFI BINDINGS
// =============================================================================

@external(erlang, "vibee_config_ffi", "get_env")
fn get_env(key: String, default: String) -> String

@external(erlang, "vibee_p2p_ffi", "current_timestamp")
fn current_timestamp() -> Int

@external(erlang, "vibee_oracle_ffi", "extract_amount")
fn extract_amount_ffi(text: String, currency: String) -> Result(Float, String)

@external(erlang, "vibee_oracle_ffi", "extract_p2p_reference")
fn extract_p2p_reference_ffi(text: String) -> Option(String)

@external(erlang, "vibee_payment_tools_ffi", "get_map_field")
fn get_field_ffi(map: Dynamic, key: String) -> Result(Dynamic, Nil)

@external(erlang, "vibee_p2p_ffi", "get_order")
fn get_order_by_id_ffi(order_id: String) -> Result(Dynamic, Nil)

@external(erlang, "vibee_p2p_ffi", "list_orders_by_status")
fn list_orders_by_status_ffi(status: String) -> List(Dynamic)

@external(erlang, "vibee_p2p_ffi", "get_map_string")
fn get_map_string_ffi(map: Dynamic, key: String) -> String

@external(erlang, "vibee_p2p_ffi", "get_map_float")
fn get_map_float_ffi(map: Dynamic, key: String) -> Float

@external(erlang, "vibee_p2p_ffi", "get_map_int")
fn get_map_int_ffi(map: Dynamic, key: String) -> Int
