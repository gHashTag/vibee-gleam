// VIBEE TonAPI Client
// Fetches real TON prices from TonAPI
//
// Endpoint: https://tonapi.io/v2/rates
// Method: GET (no authentication required for basic rates)
//
// This provides real market rates for TON token

import gleam/dict
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/float
import gleam/int
import gleam/string

// =============================================================================
// TYPES
// =============================================================================

/// TON price data
pub type TonRate {
  TonRate(
    currency: String,
    price: Float,
    diff_24h: Float,
    diff_7d: Float,
    diff_30d: Float,
  )
}

/// Error types
pub type TonApiError {
  TonApiHttpError(String)
  TonApiParseError(String)
  TonApiNoData
}

// =============================================================================
// PUBLIC API
// =============================================================================

/// Get TON price in multiple currencies
/// currencies: List of currency codes like "usd", "rub", "thb"
pub fn get_rates(currencies: List(String)) -> Result(List(TonRate), TonApiError) {
  let currencies_str = string.join(currencies, ",")
  let url = "https://tonapi.io/v2/rates?tokens=ton&currencies=" <> currencies_str

  case request.to(url) {
    Ok(req) -> {
      let req = req
        |> request.set_method(http.Get)
        |> request.set_header("Accept", "application/json")
        |> request.set_header("User-Agent", "VIBEE/1.0")

      case httpc.send(req) {
        Ok(resp) -> {
          case resp.status {
            200 -> parse_rates_response(resp.body, currencies)
            _ -> {
              io.println("[TonAPI] HTTP error: " <> int.to_string(resp.status))
              Error(TonApiHttpError("Status: " <> int.to_string(resp.status)))
            }
          }
        }
        Error(e) -> {
          io.println("[TonAPI] Request failed: " <> string.inspect(e))
          Error(TonApiHttpError("Request failed"))
        }
      }
    }
    Error(_) -> Error(TonApiHttpError("Invalid URL"))
  }
}

/// Get TON/USD price
pub fn get_ton_usd() -> Result(Float, TonApiError) {
  case get_rates(["usd"]) {
    Ok(rates) -> {
      case list.find(rates, fn(r) { r.currency == "usd" }) {
        Ok(rate) -> Ok(rate.price)
        Error(_) -> Error(TonApiNoData)
      }
    }
    Error(err) -> Error(err)
  }
}

/// Get TON prices for arbitrage (USD, RUB, THB)
pub fn get_arbitrage_prices() -> Result(#(Float, Float, Float), TonApiError) {
  case get_rates(["usd", "rub", "thb"]) {
    Ok(rates) -> {
      let usd = list.find(rates, fn(r) { r.currency == "usd" })
        |> result.map(fn(r) { r.price })
        |> result.unwrap(0.0)
      let rub = list.find(rates, fn(r) { r.currency == "rub" })
        |> result.map(fn(r) { r.price })
        |> result.unwrap(0.0)
      let thb = list.find(rates, fn(r) { r.currency == "thb" })
        |> result.map(fn(r) { r.price })
        |> result.unwrap(0.0)

      case usd >. 0.0 && rub >. 0.0 && thb >. 0.0 {
        True -> Ok(#(usd, rub, thb))
        False -> Error(TonApiNoData)
      }
    }
    Error(err) -> Error(err)
  }
}

// =============================================================================
// RESPONSE PARSING
// =============================================================================

fn parse_rates_response(body: String, currencies: List(String)) -> Result(List(TonRate), TonApiError) {
  // TonAPI response structure:
  // {
  //   "rates": {
  //     "TON": {
  //       "prices": {
  //         "USD": 1.61,
  //         "RUB": 129.11,
  //         "THB": 50.94
  //       },
  //       "diff_24h": { "USD": "-0.13%", ... },
  //       "diff_7d": { "USD": "0.03%", ... },
  //       "diff_30d": { "USD": "-10.66%", ... }
  //     }
  //   }
  // }

  let decoder = {
    use rates <- decode.field("rates", {
      use ton <- decode.field("TON", {
        use prices <- decode.field("prices", decode.dict(decode.string, decode.float))
        use diff_24h <- decode.optional_field("diff_24h", dict.new(), decode.dict(decode.string, decode.string))
        use diff_7d <- decode.optional_field("diff_7d", dict.new(), decode.dict(decode.string, decode.string))
        use diff_30d <- decode.optional_field("diff_30d", dict.new(), decode.dict(decode.string, decode.string))
        decode.success(#(prices, diff_24h, diff_7d, diff_30d))
      })
      decode.success(ton)
    })
    decode.success(rates)
  }

  case json.parse(body, decoder) {
    Ok(#(prices, diff_24h, diff_7d, diff_30d)) -> {
      let rates = currencies
        |> list.filter_map(fn(currency) {
          let upper = string.uppercase(currency)
          case dict.get(prices, upper) {
            Ok(price) -> {
              Ok(TonRate(
                currency: string.lowercase(currency),
                price: price,
                diff_24h: parse_percent(diff_24h, upper),
                diff_7d: parse_percent(diff_7d, upper),
                diff_30d: parse_percent(diff_30d, upper),
              ))
            }
            Error(_) -> Error(Nil)
          }
        })

      case rates != [] {
        True -> Ok(rates)
        False -> Error(TonApiNoData)
      }
    }
    Error(e) -> {
      io.println("[TonAPI] Parse error: " <> string.inspect(e))
      io.println("[TonAPI] Body: " <> string.slice(body, 0, 500))
      Error(TonApiParseError("Failed to parse response"))
    }
  }
}

fn parse_percent(diffs: dict.Dict(String, String), currency: String) -> Float {
  case dict.get(diffs, currency) {
    Ok(pct_str) -> {
      // Remove "%" and parse
      let cleaned = pct_str
        |> string.replace("%", "")
        |> string.trim()
      case float.parse(cleaned) {
        Ok(f) -> f
        Error(_) -> 0.0
      }
    }
    Error(_) -> 0.0
  }
}
