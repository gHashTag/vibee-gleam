// VIBEE OKX P2P Client
// Fetches real P2P order book from OKX
//
// Endpoint: https://www.okx.com/v3/c2c/tradingOrders/books
// Method: GET (no authentication required for public ads)

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

/// P2P Advertisement from OKX
pub type P2PAd {
  P2PAd(
    advertiser_name: String,
    price: Float,
    min_amount: Float,
    max_amount: Float,
    available_amount: Float,
    payment_methods: List(String),
    completion_rate: Float,
    order_count: Int,
  )
}

/// Trade side
pub type Side {
  Buy   // User wants to buy crypto
  Sell  // User wants to sell crypto
}

/// Error types
pub type OkxP2PError {
  OkxHttpError(String)
  OkxParseError(String)
  OkxNoAdsFound
}

// =============================================================================
// PUBLIC API
// =============================================================================

/// Get P2P advertisements from OKX
pub fn get_ads(
  crypto: String,     // "USDT", "BTC", etc.
  fiat: String,       // "THB", "RUB", "USD", etc.
  side: Side,
  limit: Int,
) -> Result(List(P2PAd), OkxP2PError) {
  // OKX P2P API endpoint
  let side_str = case side {
    Buy -> "buy"
    Sell -> "sell"
  }

  let url = "https://www.okx.com/v3/c2c/tradingOrders/books"
    <> "?quoteCurrency=" <> fiat
    <> "&baseCurrency=" <> crypto
    <> "&side=" <> side_str
    <> "&paymentMethod=all"
    <> "&userType=all"
    <> "&showTrade=false"
    <> "&showFollow=false"
    <> "&showAlreadyTraded=false"
    <> "&isAbleFilter=false"
    <> "&receivingAds=false"
    <> "&urlId=0"

  case request.to(url) {
    Ok(req) -> {
      let req = req
        |> request.set_method(http.Get)
        |> request.set_header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)")
        |> request.set_header("Accept", "application/json")
        |> request.set_header("Accept-Language", "en-US,en;q=0.9")

      case httpc.send(req) {
        Ok(resp) -> {
          case resp.status {
            200 -> {
              let result = parse_ads_response(resp.body, limit)
              case result {
                Error(OkxParseError(msg)) -> {
                  io.println("[OKX] Parse error: " <> msg)
                  Error(OkxParseError(msg))
                }
                _ -> result
              }
            }
            _ -> {
              io.println("[OKX] HTTP error: " <> int.to_string(resp.status) <> " body: " <> string.slice(resp.body, 0, 200))
              Error(OkxHttpError("Status: " <> int.to_string(resp.status)))
            }
          }
        }
        Error(e) -> {
          io.println("[OKX] Request failed: " <> string.inspect(e))
          Error(OkxHttpError("Request failed"))
        }
      }
    }
    Error(_) -> Error(OkxHttpError("Invalid URL"))
  }
}

/// Get buy and sell prices together
pub fn get_prices(
  crypto: String,
  fiat: String,
) -> Result(#(Float, Float), OkxP2PError) {
  // Get buy ads (when user wants to buy crypto)
  case get_ads(crypto, fiat, Buy, 3) {
    Ok(buy_ads) -> {
      // Get sell ads (when user wants to sell crypto)
      case get_ads(crypto, fiat, Sell, 3) {
        Ok(sell_ads) -> {
          let buy_price = case list.first(buy_ads) {
            Ok(ad) -> ad.price
            Error(_) -> 0.0
          }
          let sell_price = case list.first(sell_ads) {
            Ok(ad) -> ad.price
            Error(_) -> 0.0
          }
          case buy_price >. 0.0 && sell_price >. 0.0 {
            True -> Ok(#(buy_price, sell_price))
            False -> Error(OkxNoAdsFound)
          }
        }
        Error(err) -> Error(err)
      }
    }
    Error(err) -> Error(err)
  }
}

// =============================================================================
// RESPONSE PARSING
// =============================================================================

fn parse_ads_response(body: String, limit: Int) -> Result(List(P2PAd), OkxP2PError) {
  // OKX response structure:
  // {
  //   "code": 0,
  //   "data": {
  //     "buy": [...] or "sell": [...]
  //   }
  // }

  let decoder = {
    use code <- decode.field("code", decode.int)
    case code == 0 {
      True -> {
        use data <- decode.field("data", {
          // Try to get buy or sell array
          use buy_ads <- decode.optional_field("buy", [], decode.list(decode_ad()))
          use sell_ads <- decode.optional_field("sell", [], decode.list(decode_ad()))
          decode.success(list.append(buy_ads, sell_ads))
        })
        decode.success(list.take(data, limit))
      }
      False -> decode.success([])
    }
  }

  case json.parse(body, decoder) {
    Ok(ads) -> {
      case list.length(ads) > 0 {
        True -> Ok(ads)
        False -> Error(OkxNoAdsFound)
      }
    }
    Error(_) -> Error(OkxParseError("Failed to parse response: " <> string.slice(body, 0, 200)))
  }
}

fn decode_ad() -> decode.Decoder(P2PAd) {
  // OKX ad structure varies, handle flexibly
  use price <- decode.optional_field("price", "0", decode.string)
  use min_amount <- decode.optional_field("quoteMinAmountPerOrder", "0", decode.string)
  use max_amount <- decode.optional_field("quoteMaxAmountPerOrder", "0", decode.string)
  use available <- decode.optional_field("availableAmount", "0", decode.string)
  use nick_name <- decode.optional_field("nickName", "Unknown", decode.string)
  use completion_rate <- decode.optional_field("completedRate", "0", decode.string)
  use order_count <- decode.optional_field("completedOrderQuantity", 0, decode.int)

  decode.success(P2PAd(
    advertiser_name: nick_name,
    price: parse_float_safe(price),
    min_amount: parse_float_safe(min_amount),
    max_amount: parse_float_safe(max_amount),
    available_amount: parse_float_safe(available),
    payment_methods: [],
    completion_rate: parse_float_safe(completion_rate),
    order_count: order_count,
  ))
}

fn parse_float_safe(s: String) -> Float {
  case float.parse(s) {
    Ok(f) -> f
    Error(_) -> {
      case int.parse(s) {
        Ok(i) -> int.to_float(i)
        Error(_) -> 0.0
      }
    }
  }
}
