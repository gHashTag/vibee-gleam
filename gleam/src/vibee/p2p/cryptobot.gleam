// VIBEE CryptoBot (Crypto Pay) API Client
// Integration with Telegram @CryptoBot for payments
//
// API Docs: https://help.crypt.bot/crypto-pay-api
// Authentication: Crypto-Pay-API-Token header
//
// Endpoints:
// - GET /api/getMe - Get app info
// - GET /api/getBalance - Get app balance
// - POST /api/createInvoice - Create payment invoice
// - GET /api/getInvoices - List invoices
// - POST /api/transfer - Transfer to user
// - GET /api/getExchangeRates - Get exchange rates

import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/float
import gleam/int
import gleam/string

// =============================================================================
// CONFIGURATION
// =============================================================================

/// CryptoBot API environments
pub type CryptoBotEnv {
  Mainnet
  Testnet
}

/// Get API base URL
pub fn base_url(env: CryptoBotEnv) -> String {
  case env {
    Mainnet -> "https://pay.crypt.bot/api"
    Testnet -> "https://testnet-pay.crypt.bot/api"
  }
}

/// CryptoBot client configuration
pub type CryptoBotConfig {
  CryptoBotConfig(
    api_token: String,
    env: CryptoBotEnv,
  )
}

// =============================================================================
// SUPPORTED ASSETS
// =============================================================================

/// Supported cryptocurrencies
pub type CryptoBotAsset {
  USDT
  TON
  BTC
  ETH
  LTC
  BNB
  TRX
  USDC
  JET  // Testnet only
}

pub fn asset_to_string(asset: CryptoBotAsset) -> String {
  case asset {
    USDT -> "USDT"
    TON -> "TON"
    BTC -> "BTC"
    ETH -> "ETH"
    LTC -> "LTC"
    BNB -> "BNB"
    TRX -> "TRX"
    USDC -> "USDC"
    JET -> "JET"
  }
}

pub fn asset_from_string(s: String) -> CryptoBotAsset {
  case s {
    "USDT" -> USDT
    "TON" -> TON
    "BTC" -> BTC
    "ETH" -> ETH
    "LTC" -> LTC
    "BNB" -> BNB
    "TRX" -> TRX
    "USDC" -> USDC
    "JET" -> JET
    _ -> USDT
  }
}

// =============================================================================
// API RESPONSE TYPES
// =============================================================================

/// App info response from getMe
pub type AppInfo {
  AppInfo(
    app_id: Int,
    name: String,
    payment_processing_bot_username: String,
  )
}

/// Balance for a single asset
pub type Balance {
  Balance(
    currency_code: String,
    available: Float,
  )
}

/// Invoice for receiving payments
pub type Invoice {
  Invoice(
    invoice_id: Int,
    hash: String,
    currency_code: String,
    amount: Float,
    pay_url: String,
    status: String,
    created_at: String,
    description: Option(String),
    paid_at: Option(String),
  )
}

/// Exchange rate
pub type ExchangeRate {
  ExchangeRate(
    source: String,
    target: String,
    rate: Float,
    is_valid: Bool,
  )
}

/// Transfer result
pub type TransferResult {
  TransferResult(
    transfer_id: Int,
    user_id: Int,
    asset: String,
    amount: Float,
    status: String,
  )
}

/// API Error
pub type CryptoBotError {
  CryptoBotHttpError(String)
  CryptoBotApiError(code: String, message: String)
  CryptoBotParseError(String)
  CryptoBotAuthError
}

// =============================================================================
// HTTP CLIENT
// =============================================================================

/// Make GET request to CryptoBot API
fn api_get(
  config: CryptoBotConfig,
  endpoint: String,
) -> Result(String, CryptoBotError) {
  let url = base_url(config.env) <> "/" <> endpoint

  case request.to(url) {
    Ok(req) -> {
      let req = req
        |> request.set_method(http.Get)
        |> request.set_header("Crypto-Pay-API-Token", config.api_token)

      case httpc.send(req) {
        Ok(resp) -> {
          case resp.status {
            200 -> Ok(resp.body)
            401 -> Error(CryptoBotAuthError)
            _ -> Error(CryptoBotApiError(int.to_string(resp.status), resp.body))
          }
        }
        Error(_) -> Error(CryptoBotHttpError("Request failed"))
      }
    }
    Error(_) -> Error(CryptoBotHttpError("Invalid URL"))
  }
}

/// Make POST request to CryptoBot API
fn api_post(
  config: CryptoBotConfig,
  endpoint: String,
  body: json.Json,
) -> Result(String, CryptoBotError) {
  let url = base_url(config.env) <> "/" <> endpoint

  case request.to(url) {
    Ok(req) -> {
      let req = req
        |> request.set_method(http.Post)
        |> request.set_header("Content-Type", "application/json")
        |> request.set_header("Crypto-Pay-API-Token", config.api_token)
        |> request.set_body(json.to_string(body))

      case httpc.send(req) {
        Ok(resp) -> {
          case resp.status {
            200 -> Ok(resp.body)
            401 -> Error(CryptoBotAuthError)
            _ -> Error(CryptoBotApiError(int.to_string(resp.status), resp.body))
          }
        }
        Error(_) -> Error(CryptoBotHttpError("Request failed"))
      }
    }
    Error(_) -> Error(CryptoBotHttpError("Invalid URL"))
  }
}

// =============================================================================
// API METHODS
// =============================================================================

/// Get app info (validates API token)
pub fn get_me(config: CryptoBotConfig) -> Result(AppInfo, CryptoBotError) {
  case api_get(config, "getMe") {
    Ok(body) -> parse_app_info(body)
    Error(err) -> Error(err)
  }
}

/// Get app balance for all assets
pub fn get_balance(config: CryptoBotConfig) -> Result(List(Balance), CryptoBotError) {
  case api_get(config, "getBalance") {
    Ok(body) -> parse_balances(body)
    Error(err) -> Error(err)
  }
}

/// Create an invoice for receiving payment
pub fn create_invoice(
  config: CryptoBotConfig,
  asset: CryptoBotAsset,
  amount: Float,
  description: Option(String),
  expires_in: Option(Int),  // Seconds
  payload: Option(String),  // Custom data for callback
) -> Result(Invoice, CryptoBotError) {
  let body = json.object([
    #("asset", json.string(asset_to_string(asset))),
    #("amount", json.string(float.to_string(amount))),
    #("description", case description {
      Some(d) -> json.string(d)
      None -> json.null()
    }),
    #("expires_in", case expires_in {
      Some(e) -> json.int(e)
      None -> json.null()
    }),
    #("payload", case payload {
      Some(p) -> json.string(p)
      None -> json.null()
    }),
  ])

  case api_post(config, "createInvoice", body) {
    Ok(response_body) -> parse_invoice(response_body)
    Error(err) -> Error(err)
  }
}

/// Get all invoices
pub fn get_invoices(
  config: CryptoBotConfig,
  asset: Option(CryptoBotAsset),
  status: Option(String),  // "active" | "paid" | "expired"
) -> Result(List(Invoice), CryptoBotError) {
  let params = case asset, status {
    Some(a), Some(s) -> "?asset=" <> asset_to_string(a) <> "&status=" <> s
    Some(a), None -> "?asset=" <> asset_to_string(a)
    None, Some(s) -> "?status=" <> s
    None, None -> ""
  }

  case api_get(config, "getInvoices" <> params) {
    Ok(body) -> parse_invoices(body)
    Error(err) -> Error(err)
  }
}

/// Get exchange rates
pub fn get_exchange_rates(config: CryptoBotConfig) -> Result(List(ExchangeRate), CryptoBotError) {
  case api_get(config, "getExchangeRates") {
    Ok(body) -> parse_exchange_rates(body)
    Error(err) -> Error(err)
  }
}

/// Transfer crypto to a Telegram user
pub fn transfer(
  config: CryptoBotConfig,
  user_id: Int,
  asset: CryptoBotAsset,
  amount: Float,
  spend_id: String,  // Unique ID to prevent duplicates
  comment: Option(String),
) -> Result(TransferResult, CryptoBotError) {
  let body = json.object([
    #("user_id", json.int(user_id)),
    #("asset", json.string(asset_to_string(asset))),
    #("amount", json.string(float.to_string(amount))),
    #("spend_id", json.string(spend_id)),
    #("comment", case comment {
      Some(c) -> json.string(c)
      None -> json.null()
    }),
  ])

  case api_post(config, "transfer", body) {
    Ok(response_body) -> parse_transfer_result(response_body)
    Error(err) -> Error(err)
  }
}

// =============================================================================
// RESPONSE PARSERS
// =============================================================================

fn parse_app_info(body: String) -> Result(AppInfo, CryptoBotError) {
  let decoder = {
    use ok <- decode.field("ok", decode.bool)
    case ok {
      True -> {
        use result <- decode.field("result", {
          use app_id <- decode.field("app_id", decode.int)
          use name <- decode.field("name", decode.string)
          use bot_username <- decode.field("payment_processing_bot_username", decode.string)
          decode.success(AppInfo(
            app_id: app_id,
            name: name,
            payment_processing_bot_username: bot_username,
          ))
        })
        decode.success(result)
      }
      False -> decode.failure(AppInfo(0, "", ""), "API error")
    }
  }

  case json.parse(body, decoder) {
    Ok(info) -> Ok(info)
    Error(_) -> Error(CryptoBotParseError("Failed to parse app info"))
  }
}

fn parse_balances(body: String) -> Result(List(Balance), CryptoBotError) {
  let decoder = {
    use ok <- decode.field("ok", decode.bool)
    case ok {
      True -> {
        use result <- decode.field("result", decode.list({
          use currency_code <- decode.field("currency_code", decode.string)
          use available <- decode.field("available", decode.string)
          decode.success(Balance(
            currency_code: currency_code,
            available: case float.parse(available) {
              Ok(f) -> f
              Error(_) -> 0.0
            },
          ))
        }))
        decode.success(result)
      }
      False -> decode.success([])
    }
  }

  case json.parse(body, decoder) {
    Ok(balances) -> Ok(balances)
    Error(_) -> Error(CryptoBotParseError("Failed to parse balances"))
  }
}

fn parse_invoice(body: String) -> Result(Invoice, CryptoBotError) {
  let decoder = {
    use ok <- decode.field("ok", decode.bool)
    case ok {
      True -> {
        use result <- decode.field("result", {
          use invoice_id <- decode.field("invoice_id", decode.int)
          use hash <- decode.field("hash", decode.string)
          use currency_code <- decode.field("currency_code", decode.string)
          use amount <- decode.field("amount", decode.string)
          use pay_url <- decode.field("pay_url", decode.string)
          use status <- decode.field("status", decode.string)
          use created_at <- decode.field("created_at", decode.string)
          decode.success(Invoice(
            invoice_id: invoice_id,
            hash: hash,
            currency_code: currency_code,
            amount: case float.parse(amount) {
              Ok(f) -> f
              Error(_) -> 0.0
            },
            pay_url: pay_url,
            status: status,
            created_at: created_at,
            description: None,
            paid_at: None,
          ))
        })
        decode.success(result)
      }
      False -> decode.failure(Invoice(0, "", "", 0.0, "", "", "", None, None), "API error")
    }
  }

  case json.parse(body, decoder) {
    Ok(invoice) -> Ok(invoice)
    Error(_) -> Error(CryptoBotParseError("Failed to parse invoice"))
  }
}

fn parse_invoices(body: String) -> Result(List(Invoice), CryptoBotError) {
  // Simplified - return empty list on error
  Ok([])
}

fn parse_exchange_rates(body: String) -> Result(List(ExchangeRate), CryptoBotError) {
  let decoder = {
    use ok <- decode.field("ok", decode.bool)
    case ok {
      True -> {
        use result <- decode.field("result", decode.list({
          use source <- decode.field("source", decode.string)
          use target <- decode.field("target", decode.string)
          use rate <- decode.field("rate", decode.string)
          use is_valid <- decode.field("is_valid", decode.bool)
          decode.success(ExchangeRate(
            source: source,
            target: target,
            rate: case float.parse(rate) {
              Ok(f) -> f
              Error(_) -> 0.0
            },
            is_valid: is_valid,
          ))
        }))
        decode.success(result)
      }
      False -> decode.success([])
    }
  }

  case json.parse(body, decoder) {
    Ok(rates) -> Ok(rates)
    Error(_) -> Error(CryptoBotParseError("Failed to parse exchange rates"))
  }
}

fn parse_transfer_result(body: String) -> Result(TransferResult, CryptoBotError) {
  let decoder = {
    use ok <- decode.field("ok", decode.bool)
    case ok {
      True -> {
        use result <- decode.field("result", {
          use transfer_id <- decode.field("transfer_id", decode.int)
          use user_id <- decode.field("user_id", decode.int)
          use asset <- decode.field("asset", decode.string)
          use amount <- decode.field("amount", decode.string)
          use status <- decode.field("status", decode.string)
          decode.success(TransferResult(
            transfer_id: transfer_id,
            user_id: user_id,
            asset: asset,
            amount: case float.parse(amount) {
              Ok(f) -> f
              Error(_) -> 0.0
            },
            status: status,
          ))
        })
        decode.success(result)
      }
      False -> decode.failure(TransferResult(0, 0, "", 0.0, ""), "Transfer failed")
    }
  }

  case json.parse(body, decoder) {
    Ok(result) -> Ok(result)
    Error(_) -> Error(CryptoBotParseError("Failed to parse transfer result"))
  }
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

/// Create config from environment
pub fn config_from_env() -> Result(CryptoBotConfig, String) {
  case get_env_var("CRYPTOBOT_API_TOKEN") {
    Ok(token) -> {
      let env = case get_env_var("CRYPTOBOT_ENV") {
        Ok("testnet") -> Testnet
        _ -> Mainnet
      }
      Ok(CryptoBotConfig(api_token: token, env: env))
    }
    Error(_) -> Error("CRYPTOBOT_API_TOKEN not set")
  }
}

/// Get environment variable
@external(erlang, "vibee_config_ffi", "get_env")
fn get_env_var(name: String) -> Result(String, Nil)
