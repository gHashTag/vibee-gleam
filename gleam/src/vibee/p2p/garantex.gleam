// VIBEE Garantex API Client
// Russian crypto exchange with public API
//
// API Docs: https://garantex.org/api/v2
// No authentication required for public endpoints
//
// Main use: USDT/RUB trading (Binance P2P dropped RUB support in Jan 2024)

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

/// Order book entry
pub type OrderBookEntry {
  OrderBookEntry(
    price: Float,
    volume: Float,
    amount: Float,
  )
}

/// Order book with bids and asks
pub type OrderBook {
  OrderBook(
    bids: List(OrderBookEntry),  // Buy orders (sorted by price desc)
    asks: List(OrderBookEntry),  // Sell orders (sorted by price asc)
    timestamp: Int,
  )
}

/// Ticker data
pub type Ticker {
  Ticker(
    market: String,
    buy: Float,    // Best bid price
    sell: Float,   // Best ask price
    low: Float,
    high: Float,
    last: Float,
    volume: Float,
  )
}

/// Market info
pub type Market {
  Market(
    id: String,           // e.g., "usdtrub"
    base_unit: String,    // e.g., "usdt"
    quote_unit: String,   // e.g., "rub"
  )
}

/// Error types
pub type GarantexError {
  GarantexHttpError(String)
  GarantexParseError(String)
  GarantexMarketNotFound
}

// =============================================================================
// SUPPORTED MARKETS
// =============================================================================

/// Get market ID for pair
pub fn market_id(base: String, quote: String) -> String {
  string.lowercase(base) <> string.lowercase(quote)
}

// Common markets:
// - usdtrub - USDT/RUB (main for arbitrage)
// - btcrub - BTC/RUB
// - ethrub - ETH/RUB
// - usdcusdt - USDC/USDT

// =============================================================================
// PUBLIC API
// =============================================================================

/// Get order book depth
pub fn get_depth(
  market: String,  // e.g., "usdtrub"
  limit: Int,      // Number of orders per side (default 20)
) -> Result(OrderBook, GarantexError) {
  let url = "https://garantex.org/api/v2/depth?market=" <> market <> "&limit=" <> int.to_string(limit)

  case request.to(url) {
    Ok(req) -> {
      let req = req
        |> request.set_method(http.Get)
        |> request.set_header("User-Agent", "VIBEE/1.0")

      case httpc.send(req) {
        Ok(resp) -> {
          case resp.status {
            200 -> parse_depth_response(resp.body)
            _ -> Error(GarantexHttpError("Status: " <> int.to_string(resp.status)))
          }
        }
        Error(_) -> Error(GarantexHttpError("Request failed"))
      }
    }
    Error(_) -> Error(GarantexHttpError("Invalid URL"))
  }
}

/// Get ticker (best prices)
pub fn get_ticker(
  market: String,
) -> Result(Ticker, GarantexError) {
  let url = "https://garantex.org/api/v2/tickers/" <> market

  case request.to(url) {
    Ok(req) -> {
      let req = req
        |> request.set_method(http.Get)
        |> request.set_header("User-Agent", "VIBEE/1.0")

      case httpc.send(req) {
        Ok(resp) -> {
          case resp.status {
            200 -> parse_ticker_response(resp.body, market)
            404 -> Error(GarantexMarketNotFound)
            _ -> Error(GarantexHttpError("Status: " <> int.to_string(resp.status)))
          }
        }
        Error(_) -> Error(GarantexHttpError("Request failed"))
      }
    }
    Error(_) -> Error(GarantexHttpError("Invalid URL"))
  }
}

/// Get all tickers
pub fn get_all_tickers() -> Result(List(Ticker), GarantexError) {
  let url = "https://garantex.org/api/v2/tickers"

  case request.to(url) {
    Ok(req) -> {
      let req = req
        |> request.set_method(http.Get)
        |> request.set_header("User-Agent", "VIBEE/1.0")

      case httpc.send(req) {
        Ok(resp) -> {
          case resp.status {
            200 -> parse_all_tickers_response(resp.body)
            _ -> Error(GarantexHttpError("Status: " <> int.to_string(resp.status)))
          }
        }
        Error(_) -> Error(GarantexHttpError("Request failed"))
      }
    }
    Error(_) -> Error(GarantexHttpError("Invalid URL"))
  }
}

/// Get USDT/RUB prices (convenience function)
pub fn get_usdt_rub_prices() -> Result(#(Float, Float), GarantexError) {
  case get_ticker("usdtrub") {
    Ok(ticker) -> Ok(#(ticker.buy, ticker.sell))
    Error(err) -> Error(err)
  }
}

/// Get best buy and sell prices for any pair
pub fn get_prices(
  base: String,   // e.g., "USDT"
  quote: String,  // e.g., "RUB"
) -> Result(#(Float, Float), GarantexError) {
  let market = market_id(base, quote)
  case get_ticker(market) {
    Ok(ticker) -> Ok(#(ticker.buy, ticker.sell))
    Error(err) -> Error(err)
  }
}

// =============================================================================
// RESPONSE PARSING
// =============================================================================

fn parse_depth_response(body: String) -> Result(OrderBook, GarantexError) {
  // Response:
  // {
  //   "timestamp": 1234567890,
  //   "asks": [{"price": "95.5", "volume": "100", "amount": "9550"}],
  //   "bids": [{"price": "95.0", "volume": "200", "amount": "19000"}]
  // }

  let decoder = {
    use timestamp <- decode.field("timestamp", decode.int)
    use asks <- decode.field("asks", decode.list(decode_order_entry()))
    use bids <- decode.field("bids", decode.list(decode_order_entry()))
    decode.success(OrderBook(
      bids: bids,
      asks: asks,
      timestamp: timestamp,
    ))
  }

  case json.parse(body, decoder) {
    Ok(book) -> Ok(book)
    Error(_) -> Error(GarantexParseError("Failed to parse depth"))
  }
}

fn decode_order_entry() -> decode.Decoder(OrderBookEntry) {
  use price_str <- decode.field("price", decode.string)
  use volume_str <- decode.field("volume", decode.string)
  use amount_str <- decode.field("amount", decode.string)
  decode.success(OrderBookEntry(
    price: parse_float_safe(price_str),
    volume: parse_float_safe(volume_str),
    amount: parse_float_safe(amount_str),
  ))
}

fn parse_ticker_response(body: String, market: String) -> Result(Ticker, GarantexError) {
  // Response for single ticker:
  // {
  //   "usdtrub": {
  //     "buy": "95.0",
  //     "sell": "95.5",
  //     "low": "94.0",
  //     "high": "96.0",
  //     "last": "95.25",
  //     "vol": "123456.78"
  //   }
  // }

  // Parse the nested structure using the use pattern
  let decoder = {
    use ticker <- decode.field(market, decode_ticker_inner(market))
    decode.success(ticker)
  }

  case json.parse(body, decoder) {
    Ok(ticker) -> Ok(ticker)
    Error(_) -> Error(GarantexParseError("Failed to parse ticker"))
  }
}

fn decode_ticker_inner(market: String) -> decode.Decoder(Ticker) {
  use buy_str <- decode.field("buy", decode.string)
  use sell_str <- decode.field("sell", decode.string)
  use low_str <- decode.field("low", decode.string)
  use high_str <- decode.field("high", decode.string)
  use last_str <- decode.field("last", decode.string)
  use vol_str <- decode.field("vol", decode.string)
  decode.success(Ticker(
    market: market,
    buy: parse_float_safe(buy_str),
    sell: parse_float_safe(sell_str),
    low: parse_float_safe(low_str),
    high: parse_float_safe(high_str),
    last: parse_float_safe(last_str),
    volume: parse_float_safe(vol_str),
  ))
}

fn parse_all_tickers_response(body: String) -> Result(List(Ticker), GarantexError) {
  // Response contains all tickers as object with market keys
  // For now, just return empty list - would need dynamic key parsing
  Ok([])
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
