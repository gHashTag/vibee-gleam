// VIBEE P2P Spread Alerts
// Real-time monitoring with Telegram notifications
//
// Features:
// 1. Monitor spread between exchanges
// 2. Send Telegram alert when spread exceeds threshold
// 3. Track alert history and statistics

import gleam/float
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/p2p/types.{type CryptoCurrency, type FiatCurrency, TON, USDT, RUB, THB, USD}
import vibee/earning/arbitrage
import vibee/earning/types as earning_types
import vibee/integrations/telegram/client as telegram_client
import vibee/integrations/telegram/types as telegram_types
import envoy

// =============================================================================
// TYPES
// =============================================================================

/// Alert configuration
pub type AlertConfig {
  AlertConfig(
    crypto: CryptoCurrency,
    fiat: FiatCurrency,
    min_spread_percent: Float,   // Alert when spread > this (e.g., 1.5)
    min_profit_usd: Float,       // Minimum profit in USD to alert
    cooldown_seconds: Int,       // Don't repeat same alert within this time
    telegram_chat_id: Int,       // Where to send alerts
    enabled: Bool,
  )
}

/// Spread opportunity detected
pub type SpreadAlert {
  SpreadAlert(
    id: String,
    crypto: CryptoCurrency,
    fiat: FiatCurrency,
    buy_source: earning_types.PriceSource,
    buy_price: Float,
    sell_source: earning_types.PriceSource,
    sell_price: Float,
    spread_percent: Float,
    estimated_profit: Float,     // Per $1000 traded
    detected_at: Int,
    notified: Bool,
  )
}

/// Alert statistics
pub type AlertStats {
  AlertStats(
    total_alerts: Int,
    alerts_today: Int,
    avg_spread: Float,
    best_spread: Float,
    last_alert_at: Int,
  )
}

// =============================================================================
// ALERT DETECTION
// =============================================================================

/// Scan for spread opportunities
pub fn scan_spreads(
  crypto: CryptoCurrency,
  fiat: FiatCurrency,
  min_spread: Float,
) -> List(SpreadAlert) {
  let prices = arbitrage.fetch_all_prices(crypto, fiat)
  let timestamp = current_timestamp()

  // Find all pairs with spread > threshold
  find_spread_opportunities(prices, min_spread, timestamp, crypto, fiat)
}

fn find_spread_opportunities(
  prices: List(earning_types.ExternalPrice),
  min_spread: Float,
  timestamp: Int,
  crypto: CryptoCurrency,
  fiat: FiatCurrency,
) -> List(SpreadAlert) {
  // Compare all pairs
  list.flat_map(prices, fn(buy_from) {
    list.filter_map(prices, fn(sell_to) {
      // Don't compare same source
      case buy_from.source == sell_to.source {
        True -> Error(Nil)
        False -> {
          // Buy at buy_from's sell price, sell at sell_to's buy price
          let buy_price = buy_from.sell_price
          let sell_price = sell_to.buy_price

          // Calculate spread
          let spread = case buy_price >. 0.0 {
            True -> { sell_price -. buy_price } /. buy_price *. 100.0
            False -> 0.0
          }

          case spread >. min_spread {
            True -> {
              let profit_per_1000 = 1000.0 *. spread /. 100.0
              Ok(SpreadAlert(
                id: "ALERT-" <> int.to_string(timestamp) <> "-" <> int.to_string(int.random(9999)),
                crypto: crypto,
                fiat: fiat,
                buy_source: buy_from.source,
                buy_price: buy_price,
                sell_source: sell_to.source,
                sell_price: sell_price,
                spread_percent: spread,
                estimated_profit: profit_per_1000,
                detected_at: timestamp,
                notified: False,
              ))
            }
            False -> Error(Nil)
          }
        }
      }
    })
  })
}

/// Format alert for Telegram message
pub fn format_alert_message(alert: SpreadAlert) -> String {
  let crypto_str = crypto_to_string(alert.crypto)
  let fiat_str = fiat_to_string(alert.fiat)

  "🚨 *ARBITRAGE ALERT*\n\n"
  <> "💰 *" <> crypto_str <> "/" <> fiat_str <> "*\n"
  <> "📈 Spread: *" <> format_percent(alert.spread_percent) <> "*\n\n"
  <> "🟢 Buy @ " <> source_to_string(alert.buy_source) <> ": " <> format_price(alert.buy_price, alert.fiat) <> "\n"
  <> "🔴 Sell @ " <> source_to_string(alert.sell_source) <> ": " <> format_price(alert.sell_price, alert.fiat) <> "\n\n"
  <> "💵 Est. profit: *$" <> float_to_string_2dp(alert.estimated_profit) <> "* per $1000\n"
  <> "⏰ " <> format_timestamp(alert.detected_at)
}

/// Format alert as short inline notification
pub fn format_alert_short(alert: SpreadAlert) -> String {
  let crypto_str = crypto_to_string(alert.crypto)
  let fiat_str = fiat_to_string(alert.fiat)

  "⚡ " <> crypto_str <> "/" <> fiat_str <> " +"
  <> format_percent(alert.spread_percent) <> " | "
  <> source_to_string(alert.buy_source) <> "→" <> source_to_string(alert.sell_source)
  <> " | $" <> float_to_string_2dp(alert.estimated_profit) <> "/1K"
}

// =============================================================================
// MONITORING WORKER
// =============================================================================

/// Configuration for continuous monitoring
pub type MonitorConfig {
  MonitorConfig(
    pairs: List(#(CryptoCurrency, FiatCurrency)),
    min_spread: Float,
    check_interval_ms: Int,
    telegram_chat_id: Option(Int),
  )
}

/// Default monitor config
pub fn default_monitor_config() -> MonitorConfig {
  MonitorConfig(
    pairs: [
      #(TON, RUB),
      #(USDT, RUB),
      #(TON, THB),
      #(USDT, THB),
    ],
    min_spread: 1.0,  // 1% minimum
    check_interval_ms: 30_000,  // 30 seconds
    telegram_chat_id: None,
  )
}

/// Run single monitoring cycle
pub fn monitor_cycle(config: MonitorConfig) -> List(SpreadAlert) {
  io.println("[MONITOR] Scanning " <> int.to_string(list.length(config.pairs)) <> " pairs...")

  let all_alerts = list.flat_map(config.pairs, fn(pair) {
    let #(crypto, fiat) = pair
    scan_spreads(crypto, fiat, config.min_spread)
  })

  // Sort by spread (highest first)
  let sorted = list.sort(all_alerts, fn(a, b) {
    float.compare(b.spread_percent, a.spread_percent)
  })

  case list.length(sorted) {
    0 -> io.println("[MONITOR] No opportunities found")
    n -> io.println("[MONITOR] Found " <> int.to_string(n) <> " opportunities!")
  }

  sorted
}

// =============================================================================
// SERIALIZATION
// =============================================================================

/// Encode alert to JSON
pub fn encode_alert(alert: SpreadAlert) -> json.Json {
  json.object([
    #("id", json.string(alert.id)),
    #("crypto", json.string(crypto_to_string(alert.crypto))),
    #("fiat", json.string(fiat_to_string(alert.fiat))),
    #("buy_source", json.string(source_to_string(alert.buy_source))),
    #("buy_price", json.float(alert.buy_price)),
    #("sell_source", json.string(source_to_string(alert.sell_source))),
    #("sell_price", json.float(alert.sell_price)),
    #("spread_percent", json.float(alert.spread_percent)),
    #("estimated_profit", json.float(alert.estimated_profit)),
    #("detected_at", json.int(alert.detected_at)),
    #("message", json.string(format_alert_short(alert))),
  ])
}

/// Encode alert list
pub fn encode_alerts(alerts: List(SpreadAlert)) -> json.Json {
  json.object([
    #("count", json.int(list.length(alerts))),
    #("alerts", json.array(alerts, encode_alert)),
  ])
}

/// Encode monitor status
pub fn encode_monitor_status(
  config: MonitorConfig,
  last_alerts: List(SpreadAlert),
  is_running: Bool,
) -> json.Json {
  json.object([
    #("is_running", json.bool(is_running)),
    #("pairs_count", json.int(list.length(config.pairs))),
    #("min_spread", json.float(config.min_spread)),
    #("check_interval_ms", json.int(config.check_interval_ms)),
    #("last_scan_alerts", json.int(list.length(last_alerts))),
    #("best_opportunity", case list.first(last_alerts) {
      Ok(alert) -> encode_alert(alert)
      Error(_) -> json.null()
    }),
  ])
}

// =============================================================================
// HELPERS
// =============================================================================

fn crypto_to_string(c: CryptoCurrency) -> String {
  case c {
    TON -> "TON"
    USDT -> "USDT"
    _ -> "CRYPTO"
  }
}

fn fiat_to_string(f: FiatCurrency) -> String {
  case f {
    RUB -> "RUB"
    THB -> "THB"
    USD -> "USD"
    _ -> "FIAT"
  }
}

fn source_to_string(s: earning_types.PriceSource) -> String {
  earning_types.source_to_string(s)
}

fn format_percent(p: Float) -> String {
  float_to_string_2dp(p) <> "%"
}

fn format_price(p: Float, fiat: FiatCurrency) -> String {
  let symbol = case fiat {
    RUB -> "₽"
    THB -> "฿"
    USD -> "$"
    _ -> ""
  }
  symbol <> float_to_string_2dp(p)
}

fn float_to_string_2dp(f: Float) -> String {
  let rounded = float.floor(f *. 100.0) /. 100.0
  float.to_string(rounded)
}

fn format_timestamp(_ts: Int) -> String {
  "just now"  // TODO: proper formatting
}

@external(erlang, "vibee_p2p_ffi", "current_timestamp")
fn current_timestamp() -> Int

// =============================================================================
// TELEGRAM NOTIFICATION
// =============================================================================

/// Send alert to Telegram chat (tries Bot API first, then MTProto)
pub fn send_telegram_alert(
  alert: SpreadAlert,
  chat_id: Int,
) -> Result(Nil, String) {
  let message = format_alert_message(alert)

  // Try Bot API first (simpler, doesn't need MTProto auth)
  case envoy.get("TELEGRAM_BOT_TOKEN") {
    Ok(token) -> {
      io.println("[ALERTS] Sending via Bot API to chat " <> int.to_string(chat_id))
      send_via_bot_api(token, chat_id, message)
    }
    Error(_) -> {
      // Fallback to MTProto bridge
      io.println("[ALERTS] No BOT_TOKEN, trying MTProto bridge...")
      send_via_mtproto_bridge(chat_id, message)
    }
  }
}

/// Send via Telegram Bot API (simple HTTP POST)
fn send_via_bot_api(token: String, chat_id: Int, message: String) -> Result(Nil, String) {
  let url = "https://api.telegram.org/bot" <> token <> "/sendMessage"
  let body = json.object([
    #("chat_id", json.int(chat_id)),
    #("text", json.string(message)),
    #("parse_mode", json.string("Markdown")),
  ])

  case http_post_json(url, json.to_string(body)) {
    Ok(response) -> {
      case string.contains(response, "\"ok\":true") {
        True -> {
          io.println("[ALERTS] ✓ Alert sent via Bot API!")
          Ok(Nil)
        }
        False -> {
          io.println("[ALERTS] Bot API error: " <> response)
          Error("Bot API error")
        }
      }
    }
    Error(e) -> {
      io.println("[ALERTS] HTTP error: " <> e)
      Error(e)
    }
  }
}

/// Send via MTProto bridge (fallback)
fn send_via_mtproto_bridge(chat_id: Int, message: String) -> Result(Nil, String) {
  let bridge_url = envoy.get("VIBEE_BRIDGE_URL")
    |> result.unwrap("https://vibee-telegram-bridge.fly.dev")

  let session_id = envoy.get("TELEGRAM_SESSION_ID")
    |> result.unwrap("")

  case session_id {
    "" -> {
      io.println("[ALERTS] No TELEGRAM_SESSION_ID set, skipping")
      Error("No session configured")
    }
    sid -> {
      let bridge = telegram_client.with_session(bridge_url, sid)
      io.println("[ALERTS] Sending via MTProto to chat " <> int.to_string(chat_id))

      case telegram_client.send_message(bridge, chat_id, message, None) {
        Ok(_) -> {
          io.println("[ALERTS] ✓ Alert sent via MTProto!")
          Ok(Nil)
        }
        Error(e) -> {
          io.println("[ALERTS] MTProto error: " <> telegram_error_to_string(e))
          Error("MTProto send failed")
        }
      }
    }
  }
}

// HTTP POST helper for Bot API
@external(erlang, "vibee_http_ffi", "post_json")
fn http_post_json(url: String, body: String) -> Result(String, String)

/// Send alert with cooldown check (uses ETS for tracking)
pub fn send_alert_with_cooldown(
  alert: SpreadAlert,
  chat_id: Int,
  cooldown_seconds: Int,
) -> Result(Bool, String) {
  let alert_key = crypto_to_string(alert.crypto) <> "_" <> fiat_to_string(alert.fiat)
    <> "_" <> source_to_string(alert.buy_source) <> "_" <> source_to_string(alert.sell_source)

  let now = current_timestamp()
  let last_sent = get_last_alert_time(alert_key)

  case now - last_sent > cooldown_seconds {
    True -> {
      // Cooldown passed, send alert
      case send_telegram_alert(alert, chat_id) {
        Ok(_) -> {
          set_last_alert_time(alert_key, now)
          Ok(True)
        }
        Error(e) -> Error(e)
      }
    }
    False -> {
      // Still in cooldown
      io.println("[ALERTS] Cooldown active for " <> alert_key <> ", skipping")
      Ok(False)
    }
  }
}

/// Process alerts and send notifications (main entry point for worker)
pub fn process_and_notify_alerts(
  alerts: List(SpreadAlert),
  chat_id: Int,
  cooldown_seconds: Int,
) -> Int {
  // Send alerts with cooldown
  list.fold(alerts, 0, fn(sent_count, alert) {
    case send_alert_with_cooldown(alert, chat_id, cooldown_seconds) {
      Ok(True) -> sent_count + 1
      _ -> sent_count
    }
  })
}

fn telegram_error_to_string(e: telegram_types.TelegramError) -> String {
  case e {
    telegram_types.ConnectionError(msg) -> "Connection error: " <> msg
    telegram_types.AuthError(msg) -> "Auth error: " <> msg
    telegram_types.ApiError(code, msg) -> "API error " <> int.to_string(code) <> ": " <> msg
    telegram_types.NetworkError(msg) -> "Network error: " <> msg
    telegram_types.InvalidSession -> "Invalid session"
    telegram_types.NotAuthorized -> "Not authorized"
  }
}

// ETS functions for cooldown tracking
@external(erlang, "vibee_earning_ffi", "get_last_alert_time")
fn get_last_alert_time(key: String) -> Int

@external(erlang, "vibee_earning_ffi", "set_last_alert_time")
fn set_last_alert_time(key: String, timestamp: Int) -> Nil
