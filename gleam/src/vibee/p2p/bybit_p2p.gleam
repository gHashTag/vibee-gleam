// VIBEE Bybit P2P Client
// Fetches real P2P order book from Bybit
//
// Endpoint: https://api2.bybit.com/fiat/otc/item/online
// Method: POST (no authentication required for public ads)

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

/// P2P Advertisement from Bybit
pub type P2PAd {
  P2PAd(
    advertiser_name: String,
    price: Float,
    min_amount: Float,
    max_amount: Float,
    quantity: Float,
    payment_methods: List(String),
    order_count: Int,
    finish_rate: Float,
  )
}

/// Trade side
pub type Side {
  Buy   // User wants to buy crypto (side=1)
  Sell  // User wants to sell crypto (side=0)
}

/// Error types
pub type BybitP2PError {
  BybitHttpError(String)
  BybitParseError(String)
  BybitNoAdsFound
}

// =============================================================================
// PUBLIC API
// =============================================================================

/// Get P2P advertisements from Bybit
pub fn get_ads(
  token_id: String,   // "USDT", "BTC", etc.
  currency_id: String, // "THB", "RUB", "USD", etc.
  side: Side,
  size: Int,
) -> Result(List(P2PAd), BybitP2PError) {
  let url = "https://api2.bybit.com/fiat/otc/item/online"

  // side: 1 = buy crypto (sell fiat), 0 = sell crypto (buy fiat)
  let side_int = case side {
    Buy -> "1"
    Sell -> "0"
  }

  let body = json.object([
    #("tokenId", json.string(token_id)),
    #("currencyId", json.string(currency_id)),
    #("side", json.string(side_int)),
    #("size", json.string(int.to_string(size))),
    #("page", json.string("1")),
    #("amount", json.string("")),
    #("authMaker", json.bool(False)),
    #("canTrade", json.bool(False)),
  ])

  case request.to(url) {
    Ok(req) -> {
      let req = req
        |> request.set_method(http.Post)
        |> request.set_header("Content-Type", "application/json")
        |> request.set_header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)")
        |> request.set_header("Accept", "application/json")
        |> request.set_body(json.to_string(body))

      case httpc.send(req) {
        Ok(resp) -> {
          case resp.status {
            200 -> {
              let result = parse_ads_response(resp.body)
              case result {
                Error(BybitParseError(msg)) -> {
                  io.println("[Bybit] Parse error: " <> msg)
                  Error(BybitParseError(msg))
                }
                Error(BybitNoAdsFound) -> {
                  io.println("[Bybit] No ads found, body: " <> string.slice(resp.body, 0, 300))
                  Error(BybitNoAdsFound)
                }
                _ -> result
              }
            }
            _ -> {
              io.println("[Bybit] HTTP error: " <> int.to_string(resp.status) <> " body: " <> string.slice(resp.body, 0, 200))
              Error(BybitHttpError("Status: " <> int.to_string(resp.status)))
            }
          }
        }
        Error(e) -> {
          io.println("[Bybit] Request failed: " <> string.inspect(e))
          Error(BybitHttpError("Request failed"))
        }
      }
    }
    Error(_) -> Error(BybitHttpError("Invalid URL"))
  }
}

/// Get buy and sell prices together
pub fn get_prices(
  token_id: String,
  currency_id: String,
) -> Result(#(Float, Float), BybitP2PError) {
  // Get buy ads
  case get_ads(token_id, currency_id, Buy, 3) {
    Ok(buy_ads) -> {
      // Get sell ads
      case get_ads(token_id, currency_id, Sell, 3) {
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
            False -> Error(BybitNoAdsFound)
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

fn parse_ads_response(body: String) -> Result(List(P2PAd), BybitP2PError) {
  // Bybit response structure:
  // {
  //   "ret_code": 0,
  //   "result": {
  //     "items": [
  //       {
  //         "price": "34.78",
  //         "minAmount": "100",
  //         "maxAmount": "50000",
  //         "quantity": "1234.56",
  //         "nickName": "TraderName",
  //         "recentOrderNum": 100,
  //         "recentExecuteRate": 98,
  //         ...
  //       }
  //     ]
  //   }
  // }

  let decoder = {
    use ret_code <- decode.field("ret_code", decode.int)
    case ret_code == 0 {
      True -> {
        use result <- decode.field("result", {
          use items <- decode.optional_field("items", [], decode.list(decode_ad()))
          decode.success(items)
        })
        decode.success(result)
      }
      False -> decode.success([])
    }
  }

  case json.parse(body, decoder) {
    Ok(ads) -> {
      case list.length(ads) > 0 {
        True -> Ok(ads)
        False -> Error(BybitNoAdsFound)
      }
    }
    Error(_) -> Error(BybitParseError("Failed to parse response: " <> string.slice(body, 0, 200)))
  }
}

fn decode_ad() -> decode.Decoder(P2PAd) {
  use price <- decode.optional_field("price", "0", decode.string)
  use min_amount <- decode.optional_field("minAmount", "0", decode.string)
  use max_amount <- decode.optional_field("maxAmount", "0", decode.string)
  use quantity <- decode.optional_field("quantity", "0", decode.string)
  use nick_name <- decode.optional_field("nickName", "Unknown", decode.string)
  use order_num <- decode.optional_field("recentOrderNum", 0, decode.int)
  use execute_rate <- decode.optional_field("recentExecuteRate", 0, decode.int)

  decode.success(P2PAd(
    advertiser_name: nick_name,
    price: parse_float_safe(price),
    min_amount: parse_float_safe(min_amount),
    max_amount: parse_float_safe(max_amount),
    quantity: parse_float_safe(quantity),
    payment_methods: [],
    order_count: order_num,
    finish_rate: int.to_float(execute_rate),
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
