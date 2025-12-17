// VIBEE Binance P2P Client
// Fetches real P2P order book from Binance
//
// Endpoint: https://p2p.binance.com/bapi/c2c/v2/friendly/c2c/adv/search
// Method: POST (no authentication required for public ads)
//
// NOTE: This is an unofficial API that may change without notice

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
// TYPES
// =============================================================================

/// P2P Advertisement from Binance
pub type P2PAd {
  P2PAd(
    advertiser_name: String,
    price: Float,
    min_amount: Float,
    max_amount: Float,
    tradable_amount: Float,
    payment_methods: List(String),
    completion_rate: Float,
    trade_count: Int,
  )
}

/// Trade type
pub type TradeType {
  Buy   // User wants to buy crypto (seller ads)
  Sell  // User wants to sell crypto (buyer ads)
}

/// Error types
pub type BinanceP2PError {
  BinanceHttpError(String)
  BinanceParseError(String)
  BinanceNoAdsFound
}

// =============================================================================
// PUBLIC API
// =============================================================================

/// Get P2P advertisements from Binance
/// Returns sorted list of ads (best price first)
pub fn get_ads(
  asset: String,      // "USDT", "BTC", "ETH", etc.
  fiat: String,       // "THB", "USD", "EUR", etc. (NOT RUB!)
  trade_type: TradeType,
  rows: Int,          // Number of ads to fetch (1-20)
) -> Result(List(P2PAd), BinanceP2PError) {
  let url = "https://p2p.binance.com/bapi/c2c/v2/friendly/c2c/adv/search"

  let trade_type_str = case trade_type {
    Buy -> "BUY"
    Sell -> "SELL"
  }

  let body = json.object([
    #("asset", json.string(asset)),
    #("fiat", json.string(fiat)),
    #("tradeType", json.string(trade_type_str)),
    #("page", json.int(1)),
    #("rows", json.int(int.min(rows, 20))),
    #("publisherType", json.null()),
    #("payTypes", json.array([], fn(x) { x })),
  ])

  case request.to(url) {
    Ok(req) -> {
      let req = req
        |> request.set_method(http.Post)
        |> request.set_header("Content-Type", "application/json")
        |> request.set_header("User-Agent", "Mozilla/5.0")
        |> request.set_body(json.to_string(body))

      case httpc.send(req) {
        Ok(resp) -> {
          case resp.status {
            200 -> parse_ads_response(resp.body)
            _ -> Error(BinanceHttpError("Status: " <> int.to_string(resp.status)))
          }
        }
        Error(_) -> Error(BinanceHttpError("Request failed"))
      }
    }
    Error(_) -> Error(BinanceHttpError("Invalid URL"))
  }
}

/// Get best buy price (lowest price to buy crypto)
pub fn get_best_buy_price(
  asset: String,
  fiat: String,
) -> Result(Float, BinanceP2PError) {
  case get_ads(asset, fiat, Buy, 5) {
    Ok(ads) -> {
      case list.first(ads) {
        Ok(ad) -> Ok(ad.price)
        Error(_) -> Error(BinanceNoAdsFound)
      }
    }
    Error(err) -> Error(err)
  }
}

/// Get best sell price (highest price to sell crypto)
pub fn get_best_sell_price(
  asset: String,
  fiat: String,
) -> Result(Float, BinanceP2PError) {
  case get_ads(asset, fiat, Sell, 5) {
    Ok(ads) -> {
      case list.first(ads) {
        Ok(ad) -> Ok(ad.price)
        Error(_) -> Error(BinanceNoAdsFound)
      }
    }
    Error(err) -> Error(err)
  }
}

/// Get buy and sell prices together
pub fn get_prices(
  asset: String,
  fiat: String,
) -> Result(#(Float, Float), BinanceP2PError) {
  // Get buy ads (seller ads - when user buys)
  case get_ads(asset, fiat, Buy, 3) {
    Ok(buy_ads) -> {
      // Get sell ads (buyer ads - when user sells)
      case get_ads(asset, fiat, Sell, 3) {
        Ok(sell_ads) -> {
          let buy_price = case list.first(buy_ads) {
            Ok(ad) -> ad.price
            Error(_) -> 0.0
          }
          let sell_price = case list.first(sell_ads) {
            Ok(ad) -> ad.price
            Error(_) -> 0.0
          }
          // Return error if no valid prices
          case buy_price >. 0.0 && sell_price >. 0.0 {
            True -> Ok(#(buy_price, sell_price))
            False -> Error(BinanceNoAdsFound)
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

fn parse_ads_response(body: String) -> Result(List(P2PAd), BinanceP2PError) {
  // Response structure:
  // {
  //   "code": "000000",
  //   "data": [
  //     {
  //       "adv": {
  //         "price": "34.78",
  //         "minSingleTransAmount": "100",
  //         "maxSingleTransAmount": "50000",
  //         "tradableQuantity": "1234.56",
  //         ...
  //       },
  //       "advertiser": {
  //         "nickName": "TraderName",
  //         "monthFinishRate": 0.98,
  //         "monthOrderCount": 1234,
  //         ...
  //       }
  //     }
  //   ]
  // }

  let decoder = {
    use code <- decode.field("code", decode.string)
    case code == "000000" {
      True -> {
        use data <- decode.field("data", decode.list(decode_ad()))
        decode.success(data)
      }
      False -> decode.success([])
    }
  }

  case json.parse(body, decoder) {
    Ok(ads) -> Ok(ads)
    Error(_) -> Error(BinanceParseError("Failed to parse response"))
  }
}

fn decode_ad() -> decode.Decoder(P2PAd) {
  use adv <- decode.field("adv", {
    use price_str <- decode.field("price", decode.string)
    use min_str <- decode.field("minSingleTransAmount", decode.string)
    use max_str <- decode.field("maxSingleTransAmount", decode.string)
    use tradable_str <- decode.field("tradableQuantity", decode.string)
    decode.success(#(
      parse_float_safe(price_str),
      parse_float_safe(min_str),
      parse_float_safe(max_str),
      parse_float_safe(tradable_str),
    ))
  })

  use advertiser <- decode.field("advertiser", {
    use nick_name <- decode.field("nickName", decode.string)
    use month_finish_rate <- decode.optional_field("monthFinishRate", 0.0, decode.float)
    use month_order_count <- decode.optional_field("monthOrderCount", 0, decode.int)
    decode.success(#(
      nick_name,
      month_finish_rate,
      month_order_count,
    ))
  })

  let #(price, min_amount, max_amount, tradable) = adv
  let #(name, completion_rate, trade_count) = advertiser

  decode.success(P2PAd(
    advertiser_name: name,
    price: price,
    min_amount: min_amount,
    max_amount: max_amount,
    tradable_amount: tradable,
    payment_methods: [],  // Could parse from tradeMethods
    completion_rate: completion_rate,
    trade_count: trade_count,
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
