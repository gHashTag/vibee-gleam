// VIBEE Invoice Service
// Unified service for creating and sending payment invoices to Telegram users
// Supports: xRocket (@tonRocketBot), CryptoBot (@CryptoBot)
//
// Use cases:
// 1. P2P Trading - collect payment from buyer
// 2. Subscription payments - recurring billing
// 3. One-time payments - service fees, tips
// 4. Multi-cheques - bulk distribution/airdrops

import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/p2p/xrocket
import vibee/p2p/cryptobot
import vibee/p2p/types.{type CryptoCurrency, TON, USDT, USDC, NOT}

// =============================================================================
// INVOICE PROVIDER
// =============================================================================

/// Supported payment providers
pub type InvoiceProvider {
  /// @tonRocketBot - supports TON, USDT, NOT
  XRocket
  /// @CryptoBot - supports TON, USDT, BTC, ETH
  CryptoBot
}

/// Convert provider to string
pub fn provider_to_string(provider: InvoiceProvider) -> String {
  case provider {
    XRocket -> "xrocket"
    CryptoBot -> "cryptobot"
  }
}

/// Parse provider from string
pub fn provider_from_string(s: String) -> InvoiceProvider {
  case s {
    "xrocket" -> XRocket
    "cryptobot" -> CryptoBot
    _ -> XRocket
  }
}

// =============================================================================
// INVOICE TYPES
// =============================================================================

/// Unified invoice type (works with any provider)
pub type UnifiedInvoice {
  UnifiedInvoice(
    id: String,
    provider: InvoiceProvider,
    amount: Float,
    currency: String,
    description: String,
    status: InvoiceStatus,
    pay_url: String,
    /// Deep link for Telegram (t.me/...)
    telegram_link: String,
    created_at: String,
    paid_at: Option(String),
    /// Custom payload for callbacks
    payload: Option(String),
    /// Telegram user ID who should pay (optional)
    payer_telegram_id: Option(Int),
  )
}

/// Invoice status
pub type InvoiceStatus {
  InvoicePending
  InvoicePaid
  InvoiceExpired
  InvoiceCancelled
}

/// Convert status to string
pub fn status_to_string(status: InvoiceStatus) -> String {
  case status {
    InvoicePending -> "pending"
    InvoicePaid -> "paid"
    InvoiceExpired -> "expired"
    InvoiceCancelled -> "cancelled"
  }
}

/// Parse status from string
pub fn status_from_string(s: String) -> InvoiceStatus {
  case s {
    "pending" | "active" -> InvoicePending
    "paid" | "completed" -> InvoicePaid
    "expired" -> InvoiceExpired
    "cancelled" -> InvoiceCancelled
    _ -> InvoicePending
  }
}

/// Multi-cheque for bulk payments (airdrops, rewards)
pub type UnifiedCheque {
  UnifiedCheque(
    id: String,
    provider: InvoiceProvider,
    amount_per_user: Float,
    currency: String,
    total_activations: Int,
    activations_left: Int,
    link: String,
    description: Option(String),
  )
}

/// Invoice creation error
pub type InvoiceError {
  ProviderNotConfigured(String)
  InsufficientBalance
  InvalidAmount(String)
  ApiError(String)
  UnsupportedCurrency(String)
}

/// Convert error to string
pub fn error_to_string(err: InvoiceError) -> String {
  case err {
    ProviderNotConfigured(p) -> "Provider not configured: " <> p
    InsufficientBalance -> "Insufficient balance for this operation"
    InvalidAmount(msg) -> "Invalid amount: " <> msg
    ApiError(msg) -> "API error: " <> msg
    UnsupportedCurrency(c) -> "Currency not supported: " <> c
  }
}

// =============================================================================
// INVOICE CREATION
// =============================================================================

/// Create invoice using xRocket
pub fn create_xrocket_invoice(
  amount: Float,
  currency: CryptoCurrency,
  description: String,
  callback_url: Option(String),
) -> Result(UnifiedInvoice, InvoiceError) {
  case xrocket.config_from_env() {
    Ok(config) -> {
      let currency_code = xrocket.crypto_to_xrocket_code(currency)
      case xrocket.create_invoice(config, amount, currency_code, description, callback_url) {
        Ok(invoice) -> {
          Ok(UnifiedInvoice(
            id: invoice.id,
            provider: XRocket,
            amount: invoice.amount,
            currency: invoice.currency,
            description: description,
            status: status_from_string(invoice.status),
            pay_url: invoice.pay_url,
            telegram_link: convert_to_telegram_link(invoice.pay_url),
            created_at: invoice.created_at,
            paid_at: None,
            payload: None,
            payer_telegram_id: None,
          ))
        }
        Error(err) -> Error(ApiError(xrocket_error_to_string(err)))
      }
    }
    Error(msg) -> Error(ProviderNotConfigured(msg))
  }
}

/// Create invoice using CryptoBot
pub fn create_cryptobot_invoice(
  amount: Float,
  currency: CryptoCurrency,
  description: String,
  expires_in: Option(Int),
  payload: Option(String),
) -> Result(UnifiedInvoice, InvoiceError) {
  case cryptobot.config_from_env() {
    Ok(config) -> {
      let asset = crypto_to_cryptobot_asset(currency)
      case cryptobot.create_invoice(config, asset, amount, Some(description), expires_in, payload) {
        Ok(invoice) -> {
          Ok(UnifiedInvoice(
            id: int.to_string(invoice.invoice_id),
            provider: CryptoBot,
            amount: invoice.amount,
            currency: invoice.currency_code,
            description: case invoice.description {
              Some(d) -> d
              None -> description
            },
            status: status_from_string(invoice.status),
            pay_url: invoice.pay_url,
            telegram_link: invoice.pay_url,  // CryptoBot URLs are already Telegram links
            created_at: invoice.created_at,
            paid_at: invoice.paid_at,
            payload: payload,
            payer_telegram_id: None,
          ))
        }
        Error(err) -> Error(ApiError(cryptobot_error_to_string(err)))
      }
    }
    Error(msg) -> Error(ProviderNotConfigured(msg))
  }
}

/// Create invoice with preferred provider, fallback to other
pub fn create_invoice(
  amount: Float,
  currency: CryptoCurrency,
  description: String,
  preferred_provider: Option(InvoiceProvider),
) -> Result(UnifiedInvoice, InvoiceError) {
  let provider = case preferred_provider {
    Some(p) -> p
    None -> XRocket  // Default to xRocket
  }

  case provider {
    XRocket -> {
      case create_xrocket_invoice(amount, currency, description, None) {
        Ok(inv) -> Ok(inv)
        Error(_) -> {
          // Fallback to CryptoBot
          create_cryptobot_invoice(amount, currency, description, None, None)
        }
      }
    }
    CryptoBot -> {
      case create_cryptobot_invoice(amount, currency, description, None, None) {
        Ok(inv) -> Ok(inv)
        Error(_) -> {
          // Fallback to xRocket
          create_xrocket_invoice(amount, currency, description, None)
        }
      }
    }
  }
}

// =============================================================================
// MULTI-CHEQUE (AIRDROPS)
// =============================================================================

/// Create multi-cheque for bulk distribution
pub fn create_multi_cheque(
  amount_per_user: Float,
  currency: CryptoCurrency,
  total_users: Int,
  description: Option(String),
) -> Result(UnifiedCheque, InvoiceError) {
  case xrocket.config_from_env() {
    Ok(config) -> {
      let currency_code = xrocket.crypto_to_xrocket_code(currency)
      let total_amount = amount_per_user *. int.to_float(total_users)

      case xrocket.create_multi_cheque(config, total_amount, currency_code, total_users, description) {
        Ok(cheque) -> {
          Ok(UnifiedCheque(
            id: cheque.id,
            provider: XRocket,
            amount_per_user: amount_per_user,
            currency: cheque.currency,
            total_activations: cheque.total_activations,
            activations_left: cheque.activations_left,
            link: cheque.link,
            description: description,
          ))
        }
        Error(err) -> Error(ApiError(xrocket_error_to_string(err)))
      }
    }
    Error(msg) -> Error(ProviderNotConfigured(msg))
  }
}

// =============================================================================
// DIRECT TRANSFERS (NO INVOICE NEEDED)
// =============================================================================

/// Send crypto directly to Telegram user (instant, no invoice)
pub fn send_to_user(
  telegram_id: Int,
  amount: Float,
  currency: CryptoCurrency,
  comment: Option(String),
  preferred_provider: Option(InvoiceProvider),
) -> Result(String, InvoiceError) {
  let provider = case preferred_provider {
    Some(p) -> p
    None -> XRocket
  }

  case provider {
    XRocket -> {
      case xrocket.config_from_env() {
        Ok(config) -> {
          let currency_code = xrocket.crypto_to_xrocket_code(currency)
          case xrocket.transfer(config, telegram_id, amount, currency_code, comment) {
            Ok(result) -> Ok(result.id)
            Error(err) -> Error(ApiError(xrocket_error_to_string(err)))
          }
        }
        Error(msg) -> Error(ProviderNotConfigured(msg))
      }
    }
    CryptoBot -> {
      case cryptobot.config_from_env() {
        Ok(config) -> {
          let asset = crypto_to_cryptobot_asset(currency)
          let spend_id = "VIBEE-" <> int.to_string(current_timestamp())
          case cryptobot.transfer(config, telegram_id, asset, amount, spend_id, comment) {
            Ok(result) -> Ok(int.to_string(result.transfer_id))
            Error(err) -> Error(ApiError(cryptobot_error_to_string(err)))
          }
        }
        Error(msg) -> Error(ProviderNotConfigured(msg))
      }
    }
  }
}

// =============================================================================
// TELEGRAM MESSAGE FORMATTING
// =============================================================================

/// Format invoice for sending in Telegram message
pub fn format_invoice_message(
  invoice: UnifiedInvoice,
  language: String,
) -> String {
  let amount_str = float.to_string(invoice.amount) <> " " <> invoice.currency

  case language {
    "ru" -> {
      "💰 *Счёт на оплату*\n\n"
      <> "Сумма: *" <> amount_str <> "*\n"
      <> "Описание: " <> invoice.description <> "\n\n"
      <> "📲 Нажмите для оплаты:\n"
      <> invoice.telegram_link
    }
    _ -> {
      "💰 *Payment Invoice*\n\n"
      <> "Amount: *" <> amount_str <> "*\n"
      <> "Description: " <> invoice.description <> "\n\n"
      <> "📲 Click to pay:\n"
      <> invoice.telegram_link
    }
  }
}

/// Format multi-cheque message
pub fn format_cheque_message(
  cheque: UnifiedCheque,
  language: String,
) -> String {
  let amount_str = float.to_string(cheque.amount_per_user) <> " " <> cheque.currency
  let left_str = int.to_string(cheque.activations_left) <> "/" <> int.to_string(cheque.total_activations)

  case language {
    "ru" -> {
      "🎁 *Мульти-чек*\n\n"
      <> "Сумма на человека: *" <> amount_str <> "*\n"
      <> "Осталось: " <> left_str <> "\n\n"
      <> "📲 Нажмите чтобы получить:\n"
      <> cheque.link
    }
    _ -> {
      "🎁 *Multi-Cheque*\n\n"
      <> "Amount per user: *" <> amount_str <> "*\n"
      <> "Remaining: " <> left_str <> "\n\n"
      <> "📲 Click to claim:\n"
      <> cheque.link
    }
  }
}

// =============================================================================
// INVOICE TO JSON
// =============================================================================

/// Convert invoice to JSON for API response
pub fn invoice_to_json(invoice: UnifiedInvoice) -> json.Json {
  json.object([
    #("id", json.string(invoice.id)),
    #("provider", json.string(provider_to_string(invoice.provider))),
    #("amount", json.float(invoice.amount)),
    #("currency", json.string(invoice.currency)),
    #("description", json.string(invoice.description)),
    #("status", json.string(status_to_string(invoice.status))),
    #("pay_url", json.string(invoice.pay_url)),
    #("telegram_link", json.string(invoice.telegram_link)),
    #("created_at", json.string(invoice.created_at)),
    #("paid_at", case invoice.paid_at {
      Some(t) -> json.string(t)
      None -> json.null()
    }),
    #("payload", case invoice.payload {
      Some(p) -> json.string(p)
      None -> json.null()
    }),
    #("payer_telegram_id", case invoice.payer_telegram_id {
      Some(id) -> json.int(id)
      None -> json.null()
    }),
  ])
}

/// Convert cheque to JSON
pub fn cheque_to_json(cheque: UnifiedCheque) -> json.Json {
  json.object([
    #("id", json.string(cheque.id)),
    #("provider", json.string(provider_to_string(cheque.provider))),
    #("amount_per_user", json.float(cheque.amount_per_user)),
    #("currency", json.string(cheque.currency)),
    #("total_activations", json.int(cheque.total_activations)),
    #("activations_left", json.int(cheque.activations_left)),
    #("link", json.string(cheque.link)),
    #("description", case cheque.description {
      Some(d) -> json.string(d)
      None -> json.null()
    }),
  ])
}

// =============================================================================
// HELPERS
// =============================================================================

/// Convert pay URL to Telegram deep link
fn convert_to_telegram_link(url: String) -> String {
  // xRocket URLs are like: https://t.me/xrocket?start=...
  // Already Telegram links, just return as-is
  url
}

/// Convert crypto to CryptoBot asset
fn crypto_to_cryptobot_asset(crypto: CryptoCurrency) -> cryptobot.CryptoBotAsset {
  case crypto {
    TON -> cryptobot.TON
    USDT -> cryptobot.USDT
    USDC -> cryptobot.USDC
    NOT -> cryptobot.USDT  // NOT not supported, fallback to USDT
  }
}

/// Convert xRocket error to string
fn xrocket_error_to_string(err: xrocket.XRocketError) -> String {
  case err {
    xrocket.XRocketHttpError(msg) -> "HTTP: " <> msg
    xrocket.XRocketApiError(code, msg) -> "API " <> int.to_string(code) <> ": " <> msg
    xrocket.XRocketParseError(msg) -> "Parse: " <> msg
    xrocket.XRocketAuthError -> "Authentication failed"
  }
}

/// Convert CryptoBot error to string
fn cryptobot_error_to_string(err: cryptobot.CryptoBotError) -> String {
  case err {
    cryptobot.CryptoBotHttpError(msg) -> "HTTP: " <> msg
    cryptobot.CryptoBotApiError(code, msg) -> "API " <> code <> ": " <> msg
    cryptobot.CryptoBotParseError(msg) -> "Parse: " <> msg
    cryptobot.CryptoBotAuthError -> "Authentication failed"
  }
}

/// Get current timestamp
@external(erlang, "vibee_p2p_ffi", "current_timestamp")
fn current_timestamp() -> Int
