// VIBEE Exchange Credentials Manager
// Secure storage for API keys in ETS

import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option, None, Some}
import vibee/earning/types.{type PriceSource, BinanceP2P, OkxP2P, BybitP2P, CryptoBot, TonAPI, GarantexP2P, VibeeP2P}

// =============================================================================
// TYPES
// =============================================================================

/// Exchange credentials for API access
pub type Credentials {
  Credentials(
    exchange: PriceSource,
    api_key: String,
    api_secret: String,
    passphrase: Option(String),  // OKX requires passphrase
    enabled: Bool,
  )
}

/// Credential storage result
pub type CredentialResult {
  CredentialOk
  CredentialError(String)
}

// =============================================================================
// PUBLIC API
// =============================================================================

/// Initialize credentials storage
pub fn init() -> Nil {
  ffi_init()
}

/// Store credentials for an exchange
pub fn store(creds: Credentials) -> CredentialResult {
  let exchange_key = source_to_key(creds.exchange)
  case ffi_store_credentials(
    exchange_key,
    creds.api_key,
    creds.api_secret,
    option_to_string(creds.passphrase),
    creds.enabled,
  ) {
    True -> CredentialOk
    False -> CredentialError("Failed to store credentials")
  }
}

/// Get credentials for an exchange
pub fn get(exchange: PriceSource) -> Option(Credentials) {
  let exchange_key = source_to_key(exchange)
  case ffi_get_credentials(exchange_key) {
    Ok(tuple) -> {
      let #(api_key, api_secret, passphrase, enabled) = tuple
      Some(Credentials(
        exchange: exchange,
        api_key: api_key,
        api_secret: api_secret,
        passphrase: case passphrase {
          "" -> None
          p -> Some(p)
        },
        enabled: enabled,
      ))
    }
    Error(_) -> None
  }
}

/// Check if exchange has credentials configured
pub fn has_credentials(exchange: PriceSource) -> Bool {
  let exchange_key = source_to_key(exchange)
  ffi_has_credentials(exchange_key)
}

/// Remove credentials for an exchange
pub fn remove(exchange: PriceSource) -> Bool {
  let exchange_key = source_to_key(exchange)
  ffi_remove_credentials(exchange_key)
}

/// List all configured exchanges
pub fn list_configured() -> List(PriceSource) {
  ffi_list_configured()
}

// =============================================================================
// HELPERS
// =============================================================================

fn source_to_key(source: PriceSource) -> String {
  case source {
    BinanceP2P -> "binance"
    OkxP2P -> "okx"
    BybitP2P -> "bybit"
    CryptoBot -> "cryptobot"
    TonAPI -> "tonapi"
    GarantexP2P -> "garantex"
    VibeeP2P -> "vibee"
    _ -> "unknown"
  }
}

fn option_to_string(opt: Option(String)) -> String {
  case opt {
    Some(s) -> s
    None -> ""
  }
}

// =============================================================================
// FFI DECLARATIONS
// =============================================================================

@external(erlang, "vibee_credentials_ffi", "init")
fn ffi_init() -> Nil

@external(erlang, "vibee_credentials_ffi", "store_credentials")
fn ffi_store_credentials(
  exchange: String,
  api_key: String,
  api_secret: String,
  passphrase: String,
  enabled: Bool,
) -> Bool

@external(erlang, "vibee_credentials_ffi", "get_credentials")
fn ffi_get_credentials(
  exchange: String,
) -> Result(#(String, String, String, Bool), String)

@external(erlang, "vibee_credentials_ffi", "has_credentials")
fn ffi_has_credentials(exchange: String) -> Bool

@external(erlang, "vibee_credentials_ffi", "remove_credentials")
fn ffi_remove_credentials(exchange: String) -> Bool

@external(erlang, "vibee_credentials_ffi", "list_configured")
fn ffi_list_configured() -> List(PriceSource)
