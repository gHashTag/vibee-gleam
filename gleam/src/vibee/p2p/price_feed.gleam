/// VIBEE P2P Price Feed Module
/// Fetches live cryptocurrency prices from CoinGecko API
///
/// Supported pairs:
/// - TON/USD, TON/RUB, TON/THB, TON/EUR
/// - USDT is pegged 1:1 to USD

import gleam/dynamic/decode
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/float
import vibee/p2p/types.{type CryptoCurrency, type FiatCurrency, TON, USDT, USDC, NOT, THB, RUB, USD, EUR}

/// Price data from CoinGecko
pub type PriceData {
  PriceData(
    crypto: CryptoCurrency,
    fiat: FiatCurrency,
    price: Float,
    change_24h: Option(Float),
    source: String,
  )
}

/// Price feed errors
pub type PriceFeedError {
  PriceFeedHttpError(String)
  PriceFeedParseError(String)
  PriceFeedNotFound
}

/// CoinGecko API base URL
const coingecko_api = "https://api.coingecko.com/api/v3"

/// Map crypto to CoinGecko ID
fn crypto_to_coingecko_id(crypto: CryptoCurrency) -> String {
  case crypto {
    TON -> "the-open-network"
    USDT -> "tether"
    USDC -> "usd-coin"
    NOT -> "notcoin"
  }
}

/// Map fiat to CoinGecko currency code
fn fiat_to_coingecko_code(fiat: FiatCurrency) -> String {
  case fiat {
    USD -> "usd"
    RUB -> "rub"
    THB -> "thb"
    EUR -> "eur"
  }
}

/// Fetch price for a single pair
pub fn fetch_price(
  crypto: CryptoCurrency,
  fiat: FiatCurrency,
) -> Result(PriceData, PriceFeedError) {
  // USDT and USDC are pegged to USD
  case crypto {
    USDT -> {
      let rate = get_usd_to_fiat_rate(fiat)
      Ok(PriceData(
        crypto: USDT,
        fiat: fiat,
        price: rate,
        change_24h: Some(0.0),
        source: "pegged",
      ))
    }
    USDC -> {
      let rate = get_usd_to_fiat_rate(fiat)
      Ok(PriceData(
        crypto: USDC,
        fiat: fiat,
        price: rate,
        change_24h: Some(0.0),
        source: "pegged",
      ))
    }
    TON -> fetch_coingecko_price(crypto, fiat)
    NOT -> {
      // NOT (Notcoin) - use fallback rate for now
      let price = get_fallback_rate(NOT, fiat)
      Ok(PriceData(
        crypto: NOT,
        fiat: fiat,
        price: price,
        change_24h: None,
        source: "fallback",
      ))
    }
  }
}

/// Get USD to fiat rate (approximate, for USDT)
fn get_usd_to_fiat_rate(fiat: FiatCurrency) -> Float {
  case fiat {
    USD -> 1.0
    RUB -> 95.0   // ~95 RUB per USD
    THB -> 35.0   // ~35 THB per USD
    EUR -> 0.93   // ~0.93 EUR per USD
  }
}

/// Fetch price from CoinGecko API
fn fetch_coingecko_price(
  crypto: CryptoCurrency,
  fiat: FiatCurrency,
) -> Result(PriceData, PriceFeedError) {
  let coin_id = crypto_to_coingecko_id(crypto)
  let currency = fiat_to_coingecko_code(fiat)

  let url = coingecko_api
    <> "/simple/price?ids=" <> coin_id
    <> "&vs_currencies=" <> currency
    <> "&include_24hr_change=true"

  case request.to(url) {
    Ok(req) -> {
      case httpc.send(req) {
        Ok(response) -> {
          parse_coingecko_response(response.body, crypto, fiat)
        }
        Error(_) -> Error(PriceFeedHttpError("Failed to fetch from CoinGecko"))
      }
    }
    Error(_) -> Error(PriceFeedHttpError("Invalid URL"))
  }
}

/// Parse CoinGecko simple/price response for TON
/// Example: {"the-open-network":{"usd":5.23,"usd_24h_change":2.5}}
fn parse_coingecko_response(
  body: String,
  crypto: CryptoCurrency,
  fiat: FiatCurrency,
) -> Result(PriceData, PriceFeedError) {
  let currency = fiat_to_coingecko_code(fiat)

  // Decoder for nested price structure
  let price_decoder = {
    use price <- decode.field(currency, decode.float)
    decode.success(price)
  }

  let ton_decoder = {
    use price <- decode.field("the-open-network", price_decoder)
    decode.success(price)
  }

  case json.parse(body, ton_decoder) {
    Ok(price) -> {
      Ok(PriceData(
        crypto: crypto,
        fiat: fiat,
        price: price,
        change_24h: None,  // Simplified - skip 24h change parsing
        source: "coingecko",
      ))
    }
    Error(_) -> Error(PriceFeedParseError("Failed to parse CoinGecko response"))
  }
}

/// Fetch all supported pairs
pub fn fetch_all_prices() -> List(Result(PriceData, PriceFeedError)) {
  let cryptos = [TON, USDT]
  let fiats = [USD, RUB, THB, EUR]

  list.flat_map(cryptos, fn(crypto) {
    list.map(fiats, fn(fiat) {
      fetch_price(crypto, fiat)
    })
  })
}

/// Calculate buy/sell rates with spread
pub fn calculate_rates(
  price: Float,
  spread_percent: Float,
) -> #(Float, Float) {
  let half_spread = spread_percent /. 200.0
  let buy_rate = price *. { 1.0 +. half_spread }
  let sell_rate = price *. { 1.0 -. half_spread }
  #(buy_rate, sell_rate)
}

/// Format price for display
pub fn format_price(price: Float, fiat: FiatCurrency) -> String {
  let symbol = case fiat {
    USD -> "$"
    RUB -> " RUB"
    THB -> " THB"
    EUR -> " EUR"
  }
  let formatted = float.to_string(float.floor(price *. 100.0) /. 100.0)
  case fiat {
    USD -> symbol <> formatted
    _ -> formatted <> symbol
  }
}

/// Get hardcoded fallback rates (when API is unavailable)
pub fn get_fallback_rate(crypto: CryptoCurrency, fiat: FiatCurrency) -> Float {
  case crypto, fiat {
    TON, USD -> 5.50
    TON, RUB -> 523.0
    TON, THB -> 192.0
    TON, EUR -> 5.10
    USDT, USD -> 1.0
    USDT, RUB -> 95.0
    USDT, THB -> 35.0
    USDT, EUR -> 0.93
    USDC, USD -> 1.0
    USDC, RUB -> 95.0
    USDC, THB -> 35.0
    USDC, EUR -> 0.93
    NOT, USD -> 0.008
    NOT, RUB -> 0.76
    NOT, THB -> 0.28
    NOT, EUR -> 0.0075
  }
}
