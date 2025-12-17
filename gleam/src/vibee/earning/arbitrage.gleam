// VIBEE P2P Arbitrage Scanner
// Multi-platform price scanning and arbitrage opportunity detection

import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/earning/types.{
  type ArbitrageOpportunity, type ExternalPrice,
  ArbitrageOpportunity, ExternalPrice,
  BinanceP2P, BybitP2P, OkxP2P, GarantexP2P, CryptoBot, TonAPI, GarantexCross,
}
import vibee/p2p/types as p2p_types
import vibee/p2p/binance_p2p
import vibee/p2p/garantex
import vibee/p2p/cryptobot
import vibee/p2p/okx_p2p
import vibee/p2p/bybit_p2p
import vibee/p2p/tonapi

// =============================================================================
// PRICE FETCHING
// =============================================================================

/// Fetch prices from all available sources
pub fn fetch_all_prices(
  crypto: p2p_types.CryptoCurrency,
  fiat: p2p_types.FiatCurrency,
) -> List(ExternalPrice) {
  let now = current_timestamp()

  // Only fetch real external prices - VibeeP2P removed (it was simulated/hardcoded)
  fetch_external_prices(crypto, fiat, now)
}

/// Fetch prices from external exchanges
/// Uses ONLY REAL APIs - no simulated data
fn fetch_external_prices(
  crypto: p2p_types.CryptoCurrency,
  fiat: p2p_types.FiatCurrency,
  now: Int,
) -> List(ExternalPrice) {
  let fiat_str = p2p_types.fiat_to_string(fiat)
  let crypto_str = p2p_types.crypto_to_string(crypto)

  io.println("[ARB] Fetching prices for " <> crypto_str <> "/" <> fiat_str)

  // === REAL API CALLS ONLY ===

  // 1. Binance P2P (not available for RUB since Jan 2024)
  let binance_price = case fiat {
    p2p_types.RUB -> {
      io.println("[ARB] Binance: skipped (no RUB support)")
      None
    }
    _ -> {
      case binance_p2p.get_prices(crypto_str, fiat_str) {
        Ok(#(buy, sell)) -> {
          io.println("[ARB] Binance: buy=" <> float.to_string(buy) <> " sell=" <> float.to_string(sell))
          Some(ExternalPrice(
            source: BinanceP2P,
            crypto: crypto,
            fiat: fiat,
            buy_price: buy,
            sell_price: sell,
            volume_24h: 50000.0,
            updated_at: now,
          ))
        }
        Error(_) -> {
          io.println("[ARB] Binance: ERROR - no data")
          None
        }
      }
    }
  }

  // 2. OKX P2P (real API)
  let okx_price = case okx_p2p.get_prices(crypto_str, fiat_str) {
    Ok(#(buy, sell)) -> {
      io.println("[ARB] OKX: buy=" <> float.to_string(buy) <> " sell=" <> float.to_string(sell))
      Some(ExternalPrice(
        source: OkxP2P,
        crypto: crypto,
        fiat: fiat,
        buy_price: buy,
        sell_price: sell,
        volume_24h: 25000.0,
        updated_at: now,
      ))
    }
    Error(_) -> {
      io.println("[ARB] OKX: ERROR - no data")
      None
    }
  }

  // 3. Bybit P2P (real API)
  let bybit_price = case bybit_p2p.get_prices(crypto_str, fiat_str) {
    Ok(#(buy, sell)) -> {
      io.println("[ARB] Bybit: buy=" <> float.to_string(buy) <> " sell=" <> float.to_string(sell))
      Some(ExternalPrice(
        source: BybitP2P,
        crypto: crypto,
        fiat: fiat,
        buy_price: buy,
        sell_price: sell,
        volume_24h: 30000.0,
        updated_at: now,
      ))
    }
    Error(_) -> {
      io.println("[ARB] Bybit: ERROR - no data")
      None
    }
  }

  // 4. Garantex (real for RUB only)
  let garantex_price = case fiat {
    p2p_types.RUB -> {
      case garantex.get_prices(crypto_str, fiat_str) {
        Ok(#(buy, sell)) -> {
          io.println("[ARB] Garantex: buy=" <> float.to_string(buy) <> " sell=" <> float.to_string(sell))
          Some(ExternalPrice(
            source: GarantexP2P,
            crypto: crypto,
            fiat: fiat,
            buy_price: buy,
            sell_price: sell,
            volume_24h: 15000.0,
            updated_at: now,
          ))
        }
        Error(_) -> {
          io.println("[ARB] Garantex: ERROR - no data")
          None
        }
      }
    }
    _ -> None  // Garantex only for RUB
  }

  // 5. CryptoBot exchange rates (real)
  let cryptobot_price = case cryptobot.config_from_env() {
    Ok(config) -> {
      case cryptobot.get_exchange_rates(config) {
        Ok(rates) -> {
          // Find rate for this pair (source=crypto, target=fiat)
          let matching_rate = list.find(rates, fn(r) {
            r.source == crypto_str && r.target == fiat_str && r.is_valid
          })
          case matching_rate {
            Ok(rate) -> {
              io.println("[ARB] CryptoBot: rate=" <> float.to_string(rate.rate))
              // CryptoBot gives single rate, estimate spread
              let spread = 0.006  // 0.6% typical spread
              Some(ExternalPrice(
                source: CryptoBot,
                crypto: crypto,
                fiat: fiat,
                buy_price: rate.rate *. { 1.0 -. spread /. 2.0 },
                sell_price: rate.rate *. { 1.0 +. spread /. 2.0 },
                volume_24h: 20000.0,
                updated_at: now,
              ))
            }
            Error(_) -> {
              io.println("[ARB] CryptoBot: no matching rate for " <> crypto_str <> "/" <> fiat_str)
              None
            }
          }
        }
        Error(_) -> {
          io.println("[ARB] CryptoBot: ERROR - API call failed")
          None
        }
      }
    }
    Error(_) -> {
      io.println("[ARB] CryptoBot: no config")
      None
    }
  }

  // 6. TonAPI (real TON market rates - works for TON only)
  let tonapi_price = case crypto {
    p2p_types.TON -> {
      let fiat_lower = string.lowercase(fiat_str)
      case tonapi.get_rates([fiat_lower]) {
        Ok(rates) -> {
          case list.find(rates, fn(r) { r.currency == fiat_lower }) {
            Ok(rate) -> {
              io.println("[ARB] TonAPI: TON/" <> fiat_str <> " = " <> float.to_string(rate.price))
              // TonAPI gives spot price, add typical P2P spread
              let spread = 0.008  // 0.8% typical spread
              Some(ExternalPrice(
                source: TonAPI,
                crypto: crypto,
                fiat: fiat,
                buy_price: rate.price *. { 1.0 +. spread /. 2.0 },
                sell_price: rate.price *. { 1.0 -. spread /. 2.0 },
                volume_24h: 100000.0,  // TON has high liquidity
                updated_at: now,
              ))
            }
            Error(_) -> {
              io.println("[ARB] TonAPI: no rate for " <> fiat_str)
              None
            }
          }
        }
        Error(_) -> {
          io.println("[ARB] TonAPI: ERROR - API call failed")
          None
        }
      }
    }
    _ -> None  // TonAPI only for TON
  }

  // 7. Cross-pair calculation: TON/RUB via TON/USD × USDT/RUB (Garantex)
  // This provides an alternative TON/RUB price for arbitrage detection
  let cross_price = case crypto, fiat {
    p2p_types.TON, p2p_types.RUB -> {
      // Get TON/USD from TonAPI
      case tonapi.get_rates(["usd"]) {
        Ok(ton_rates) -> {
          case list.find(ton_rates, fn(r) { r.currency == "usd" }) {
            Ok(ton_usd) -> {
              // Get USDT/RUB from Garantex
              case garantex.get_ticker("usdtrub") {
                Ok(usdt_rub) -> {
                  // Calculate TON/RUB = TON/USD × USDT/RUB
                  // (assuming TON/USD ≈ TON/USDT for simplicity)
                  let ton_rub_buy = ton_usd.price *. usdt_rub.buy
                  let ton_rub_sell = ton_usd.price *. usdt_rub.sell
                  io.println("[ARB] Cross-pair: TON/USD=" <> float.to_string(ton_usd.price) <> " × USDT/RUB=" <> float.to_string(usdt_rub.buy) <> " = " <> float.to_string(ton_rub_buy))
                  Some(ExternalPrice(
                    source: GarantexCross,
                    crypto: crypto,
                    fiat: fiat,
                    buy_price: ton_rub_buy,
                    sell_price: ton_rub_sell,
                    volume_24h: 50000.0,  // Estimated
                    updated_at: now,
                  ))
                }
                Error(_) -> {
                  io.println("[ARB] Cross-pair: Garantex USDT/RUB failed")
                  None
                }
              }
            }
            Error(_) -> None
          }
        }
        Error(_) -> None
      }
    }
    _, _ -> None  // Cross-pair only for TON/RUB
  }

  // === COLLECT ONLY REAL PRICES (no fallbacks, no simulation) ===
  let prices = [binance_price, okx_price, bybit_price, garantex_price, cryptobot_price, tonapi_price, cross_price]
    |> list.filter_map(fn(opt) {
      case opt {
        Some(p) -> Ok(p)
        None -> Error(Nil)
      }
    })

  io.println("[ARB] Total prices collected: " <> int.to_string(list.length(prices)))
  prices
}

// NOTE: xRocket Pay API doesn't have P2P orderbook endpoint
// It's only for payments, not for price discovery
// So we don't include it in arbitrage sources

@external(erlang, "vibee_p2p_ffi", "current_timestamp")
fn current_timestamp() -> Int

// =============================================================================
// ARBITRAGE DETECTION
// =============================================================================

/// Scan for arbitrage opportunities between price sources
pub fn scan_opportunities(
  crypto: p2p_types.CryptoCurrency,
  fiat: p2p_types.FiatCurrency,
  min_spread_percent: Float,
) -> List(ArbitrageOpportunity) {
  let prices = fetch_all_prices(crypto, fiat)
  let now = current_timestamp()

  // Find all profitable pairs
  find_arbitrage_pairs(prices, min_spread_percent, now)
}

/// Find all arbitrage pairs from price list
fn find_arbitrage_pairs(
  prices: List(ExternalPrice),
  min_spread: Float,
  now: Int,
) -> List(ArbitrageOpportunity) {
  // Compare each pair of exchanges
  list.flat_map(prices, fn(buy_from) {
    list.filter_map(prices, fn(sell_to) {
      case buy_from.source != sell_to.source {
        False -> Error(Nil)
        True -> {
          // Can we buy from buy_from and sell to sell_to?
          // Buy at sell_price (we're taking someone's sell order)
          // Sell at buy_price (we're taking someone's buy order)
          let spread = types.calculate_spread(buy_from.sell_price, sell_to.buy_price)

          case spread >. min_spread {
            False -> Error(Nil)
            True -> {
              let max_volume = float.min(buy_from.volume_24h, sell_to.volume_24h) /. 10.0
              let estimated_profit = max_volume *. spread /. 100.0

              Ok(ArbitrageOpportunity(
                id: generate_arb_id(now),
                crypto: buy_from.crypto,
                fiat: buy_from.fiat,
                buy_source: buy_from.source,
                buy_price: buy_from.sell_price,
                buy_volume: buy_from.volume_24h,
                sell_source: sell_to.source,
                sell_price: sell_to.buy_price,
                sell_volume: sell_to.volume_24h,
                spread_percent: spread,
                potential_profit_percent: spread -. 0.5,  // Minus platform fee
                max_trade_size: max_volume,
                estimated_profit: estimated_profit,
                detected_at: now,
                expires_at: now + 300,  // 5 minutes validity
              ))
            }
          }
        }
      }
    })
  })
  |> list.sort(fn(a, b) { float.compare(b.spread_percent, a.spread_percent) })
}

/// Generate unique arbitrage opportunity ID
fn generate_arb_id(timestamp: Int) -> String {
  "ARB-" <> int.to_string(timestamp) <> "-" <> int.to_string(timestamp % 10000)
}

// =============================================================================
// OPPORTUNITY EVALUATION
// =============================================================================

/// Evaluate if opportunity is still valid and profitable
pub fn evaluate_opportunity(
  opp: ArbitrageOpportunity,
  current_prices: List(ExternalPrice),
  min_profit: Float,
) -> Result(ArbitrageOpportunity, String) {
  let now = current_timestamp()

  // Check if expired
  case opp.expires_at < now {
    True -> Error("Opportunity expired")
    False -> {
      // Find current prices for both sources
      let buy_price = list.find(current_prices, fn(p) {
        p.source == opp.buy_source && p.crypto == opp.crypto && p.fiat == opp.fiat
      })
      let sell_price = list.find(current_prices, fn(p) {
        p.source == opp.sell_source && p.crypto == opp.crypto && p.fiat == opp.fiat
      })

      case buy_price, sell_price {
        Ok(buy), Ok(sell) -> {
          let current_spread = types.calculate_spread(buy.sell_price, sell.buy_price)
          case current_spread >. min_profit {
            True -> {
              // Update opportunity with current prices
              Ok(ArbitrageOpportunity(
                ..opp,
                buy_price: buy.sell_price,
                sell_price: sell.buy_price,
                spread_percent: current_spread,
                potential_profit_percent: current_spread -. 0.5,
              ))
            }
            False -> Error("Spread too low: " <> float.to_string(current_spread) <> "%")
          }
        }
        _, _ -> Error("Price sources unavailable")
      }
    }
  }
}

/// Calculate execution risk score (0-100, lower is better)
pub fn calculate_risk_score(opp: ArbitrageOpportunity) -> Int {
  let base_score = 50

  // Lower spread = higher risk (might disappear)
  let spread_risk = case opp.spread_percent {
    s if s >. 3.0 -> -20
    s if s >. 2.0 -> -10
    s if s >. 1.0 -> 0
    _ -> 20
  }

  // Lower volume = higher risk
  let volume_risk = case float.min(opp.buy_volume, opp.sell_volume) {
    v if v >. 100000.0 -> -15
    v if v >. 50000.0 -> -5
    v if v >. 10000.0 -> 0
    _ -> 15
  }

  // Cross-platform transfers are risky (all external now)
  let transfer_risk = 25

  let total = base_score + spread_risk + volume_risk + transfer_risk
  int.max(0, int.min(100, total))
}

// =============================================================================
// EXECUTION HELPERS
// =============================================================================

/// Estimate time to complete arbitrage
pub fn estimate_execution_time(_opp: ArbitrageOpportunity) -> Int {
  // All transfers are external now, so base time + transfer time
  5 + 15  // 20 minutes for cross-platform transfers
}

/// Check if arbitrage amount is viable
pub fn is_amount_viable(
  opp: ArbitrageOpportunity,
  amount: Float,
  min_profit_usd: Float,
) -> Bool {
  case amount <=. opp.max_trade_size {
    False -> False
    True -> {
      let expected_profit = amount *. opp.potential_profit_percent /. 100.0
      expected_profit >=. min_profit_usd
    }
  }
}

/// Format opportunity for display
pub fn format_opportunity(opp: ArbitrageOpportunity) -> String {
  let crypto_str = p2p_types.crypto_to_string(opp.crypto)
  let fiat_str = p2p_types.fiat_to_string(opp.fiat)

  "Buy " <> crypto_str <> " @ " <>
  types.source_to_string(opp.buy_source) <> " for " <>
  float.to_string(opp.buy_price) <> " " <> fiat_str <>
  " → Sell @ " <> types.source_to_string(opp.sell_source) <> " for " <>
  float.to_string(opp.sell_price) <> " " <> fiat_str <>
  " | Spread: " <> float.to_string(opp.spread_percent) <> "%"
}
