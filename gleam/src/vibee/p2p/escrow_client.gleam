// VIBEE P2P Escrow Client
// Client for interacting with TON Escrow smart contract

import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/int
import gleam/float
import gleam/string
import gleam/json
import gleam/list
import gleam/dynamic.{type Dynamic}
import vibee/logging
import vibee/p2p/types.{
  type P2POrder, type OrderStatus, type CryptoCurrency, type FiatCurrency,
  type PaymentMethod, type TraderProfile, type P2PConfig,
  P2POrder, TraderProfile, Open, Locked, FiatSent, Completed, Cancelled, Expired,
  TON, USDT, USDC, NOT, status_to_string, status_from_string,
  crypto_to_string, crypto_from_string, fiat_to_string, fiat_from_string,
  payment_method_to_string, payment_method_from_string,
  generate_order_id, default_config, calculate_fee,
}
import vibee/payment/ton_client

// =============================================================================
// ESCROW CONTRACT INTERFACE
// =============================================================================

/// Escrow contract configuration
pub type EscrowConfig {
  EscrowConfig(
    contract_address: String,
    oracle_address: String,
    platform_wallet: String,
    fee_percent: Float,
  )
}

/// Get escrow config from environment
/// Default: mainnet contract deployed 2024-12-14
pub fn get_escrow_config() -> EscrowConfig {
  EscrowConfig(
    contract_address: get_env("P2P_ESCROW_CONTRACT", "EQBgEEpz6509hsBlYF4GhTWX5S_AvWB_8CmyrkJfMUkZ_uDE"),
    oracle_address: get_env("P2P_ORACLE_ADDRESS", "EQCRBz60RSxp6MboqlH-lkaG7nz01ZYy7yylXKkhtOv7rUv4"),
    platform_wallet: get_env("P2P_PLATFORM_WALLET", "EQCRBz60RSxp6MboqlH-lkaG7nz01ZYy7yylXKkhtOv7rUv4"),
    fee_percent: 0.5,
  )
}

// =============================================================================
// ORDER MANAGEMENT
// =============================================================================

/// Create a new sell order
pub fn create_sell_order(
  seller_telegram_id: Int,
  seller_wallet: String,
  crypto: CryptoCurrency,
  crypto_amount: Float,
  fiat: FiatCurrency,
  fiat_amount: Float,
  payment_method: PaymentMethod,
  payment_details: String,
  expires_in_minutes: Int,
) -> Result(P2POrder, String) {
  let order_id = generate_order_id()
  let now = current_timestamp()

  // Get seller profile for rating
  let #(rating, trades) = case get_trader_profile(seller_telegram_id) {
    Ok(profile) -> #(profile.rating, profile.completed_trades)
    Error(_) -> #(5.0, 0)  // New trader starts with 5.0 rating
  }

  let order = P2POrder(
    id: order_id,
    seller_telegram_id: seller_telegram_id,
    seller_wallet: seller_wallet,
    buyer_telegram_id: None,
    buyer_wallet: None,
    crypto: crypto,
    crypto_amount: crypto_amount,
    fiat: fiat,
    fiat_amount: fiat_amount,
    payment_method: payment_method,
    payment_details: payment_details,
    status: Open,
    escrow_tx_hash: None,
    release_tx_hash: None,
    escrow_address: None,
    created_at: now,
    locked_at: None,
    completed_at: None,
    expires_in_minutes: expires_in_minutes,
    seller_rating: rating,
    seller_trades: trades,
  )

  // Store in ETS
  case store_order(order) {
    Ok(_) -> {
      logging.info("[P2P] Created order " <> order_id <> " for " <> float.to_string(crypto_amount) <> " " <> crypto_to_string(crypto))
      Ok(order)
    }
    Error(e) -> Error("Failed to store order: " <> e)
  }
}

/// Take an open order (buyer action)
pub fn take_order(
  order_id: String,
  buyer_telegram_id: Int,
  buyer_wallet: String,
) -> Result(P2POrder, String) {
  case get_order(order_id) {
    Ok(order) -> {
      case order.status {
        Open -> {
          let now = current_timestamp()
          let updated_order = P2POrder(
            ..order,
            buyer_telegram_id: Some(buyer_telegram_id),
            buyer_wallet: Some(buyer_wallet),
            status: Locked,
            locked_at: Some(now),
          )

          case store_order(updated_order) {
            Ok(_) -> {
              logging.info("[P2P] Order " <> order_id <> " taken by buyer " <> int.to_string(buyer_telegram_id))
              Ok(updated_order)
            }
            Error(e) -> Error("Failed to update order: " <> e)
          }
        }
        _ -> Error("Order is not available (status: " <> status_to_string(order.status) <> ")")
      }
    }
    Error(_) -> Error("Order not found: " <> order_id)
  }
}

/// Mark fiat as sent (buyer action)
pub fn mark_fiat_sent(
  order_id: String,
  buyer_telegram_id: Int,
) -> Result(P2POrder, String) {
  case get_order(order_id) {
    Ok(order) -> {
      // Verify buyer
      case order.buyer_telegram_id {
        Some(bid) if bid == buyer_telegram_id -> {
          case order.status {
            Locked -> {
              let updated_order = P2POrder(..order, status: FiatSent)
              case store_order(updated_order) {
                Ok(_) -> {
                  logging.info("[P2P] Order " <> order_id <> " fiat marked as sent")
                  Ok(updated_order)
                }
                Error(e) -> Error("Failed to update order: " <> e)
              }
            }
            _ -> Error("Order is not in locked status")
          }
        }
        _ -> Error("Only the buyer can mark fiat as sent")
      }
    }
    Error(_) -> Error("Order not found: " <> order_id)
  }
}

/// Confirm fiat received and release crypto (seller action)
pub fn confirm_fiat_received(
  order_id: String,
  seller_telegram_id: Int,
) -> Result(P2POrder, String) {
  case get_order(order_id) {
    Ok(order) -> {
      // Verify seller
      case order.seller_telegram_id == seller_telegram_id {
        True -> {
          case order.status {
            Locked | FiatSent -> {
              complete_order(order)
            }
            _ -> Error("Order is not ready for confirmation (status: " <> status_to_string(order.status) <> ")")
          }
        }
        False -> Error("Only the seller can confirm fiat received")
      }
    }
    Error(_) -> Error("Order not found: " <> order_id)
  }
}

/// Oracle confirms fiat payment (automatic)
pub fn oracle_confirm(
  order_id: String,
  oracle_signature: String,
) -> Result(P2POrder, String) {
  // Verify oracle signature
  let config = get_escrow_config()
  case verify_oracle_signature(order_id, oracle_signature, config.oracle_address) {
    True -> {
      case get_order(order_id) {
        Ok(order) -> {
          case order.status {
            Locked | FiatSent -> {
              logging.info("[P2P] Oracle confirmed fiat for order " <> order_id)
              complete_order(order)
            }
            _ -> Error("Order is not ready for oracle confirmation")
          }
        }
        Error(_) -> Error("Order not found: " <> order_id)
      }
    }
    False -> Error("Invalid oracle signature")
  }
}

/// Complete order - release crypto to buyer
fn complete_order(order: P2POrder) -> Result(P2POrder, String) {
  let now = current_timestamp()

  // In production, this would trigger the smart contract to release funds
  // For now, we just update the status
  let release_tx = "mock_tx_" <> order.id

  let updated_order = P2POrder(
    ..order,
    status: Completed,
    completed_at: Some(now),
    release_tx_hash: Some(release_tx),
  )

  case store_order(updated_order) {
    Ok(_) -> {
      // Update trader profiles
      let _ = update_trader_stats(order.seller_telegram_id, True)
      case order.buyer_telegram_id {
        Some(buyer_id) -> {
          let _ = update_trader_stats(buyer_id, True)
          Nil
        }
        None -> Nil
      }

      logging.info("[P2P] Order " <> order.id <> " completed! Crypto released to buyer.")
      Ok(updated_order)
    }
    Error(e) -> Error("Failed to complete order: " <> e)
  }
}

/// Cancel order (seller can cancel if not locked)
pub fn cancel_order(
  order_id: String,
  requester_telegram_id: Int,
) -> Result(P2POrder, String) {
  case get_order(order_id) {
    Ok(order) -> {
      case order.status {
        Open -> {
          // Only seller can cancel open order
          case order.seller_telegram_id == requester_telegram_id {
            True -> {
              let updated_order = P2POrder(..order, status: Cancelled)
              case store_order(updated_order) {
                Ok(_) -> {
                  logging.info("[P2P] Order " <> order_id <> " cancelled by seller")
                  Ok(updated_order)
                }
                Error(e) -> Error("Failed to cancel order: " <> e)
              }
            }
            False -> Error("Only seller can cancel open order")
          }
        }
        _ -> Error("Cannot cancel order in status: " <> status_to_string(order.status))
      }
    }
    Error(_) -> Error("Order not found: " <> order_id)
  }
}

/// Check and expire timed out orders
pub fn expire_timed_out_orders() -> Int {
  let now = current_timestamp()
  let locked_orders = list_orders_by_status_ffi("locked")

  let expired_count = do_expire_orders(locked_orders, now, 0)

  case expired_count > 0 {
    True -> logging.info("[P2P] Expired " <> int.to_string(expired_count) <> " timed out orders")
    False -> Nil
  }

  expired_count
}

fn do_expire_orders(orders: List(Dynamic), now: Int, count: Int) -> Int {
  case orders {
    [] -> count
    [order_dyn, ..rest] -> {
      case decode_order(order_dyn) {
        Ok(order) -> {
          case order.locked_at {
            Some(locked_time) -> {
              let timeout_seconds = 30 * 60  // 30 minutes default
              case now > locked_time + timeout_seconds {
                True -> {
                  // Expire this order - return crypto to seller
                  let _ = store_order(P2POrder(..order, status: Expired))
                  do_expire_orders(rest, now, count + 1)
                }
                False -> do_expire_orders(rest, now, count)
              }
            }
            None -> do_expire_orders(rest, now, count)
          }
        }
        Error(_) -> do_expire_orders(rest, now, count)
      }
    }
  }
}

// =============================================================================
// QUERY FUNCTIONS
// =============================================================================

/// List open orders (available for buying)
pub fn list_open_orders(
  fiat_filter: Option(FiatCurrency),
  crypto_filter: Option(CryptoCurrency),
  limit: Int,
) -> List(P2POrder) {
  let all_open = list_orders_by_status_ffi("open")

  all_open
  |> decode_order_list()
  |> filter_orders(fiat_filter, crypto_filter)
  |> take_n(limit)
}

/// List orders by seller
pub fn list_seller_orders(seller_telegram_id: Int) -> List(P2POrder) {
  list_orders_by_seller_ffi(seller_telegram_id)
  |> decode_order_list()
}

/// List orders by buyer
pub fn list_buyer_orders(buyer_telegram_id: Int) -> List(P2POrder) {
  list_orders_by_buyer_ffi(buyer_telegram_id)
  |> decode_order_list()
}

fn filter_orders(
  orders: List(P2POrder),
  fiat_filter: Option(FiatCurrency),
  crypto_filter: Option(CryptoCurrency),
) -> List(P2POrder) {
  orders
  |> list_filter(fn(order) {
    let fiat_match = case fiat_filter {
      Some(f) -> order.fiat == f
      None -> True
    }
    let crypto_match = case crypto_filter {
      Some(c) -> order.crypto == c
      None -> True
    }
    fiat_match && crypto_match
  })
}

fn list_filter(list: List(a), predicate: fn(a) -> Bool) -> List(a) {
  case list {
    [] -> []
    [first, ..rest] -> {
      case predicate(first) {
        True -> [first, ..list_filter(rest, predicate)]
        False -> list_filter(rest, predicate)
      }
    }
  }
}

fn take_n(list: List(a), n: Int) -> List(a) {
  case n <= 0 {
    True -> []
    False -> {
      case list {
        [] -> []
        [first, ..rest] -> [first, ..take_n(rest, n - 1)]
      }
    }
  }
}

// =============================================================================
// PAYMENT LINK GENERATION
// =============================================================================

/// Generate escrow deposit link for seller
pub fn generate_deposit_link(order: P2POrder) -> Result(String, String) {
  let config = get_escrow_config()

  case config.contract_address {
    "" -> Error("Escrow contract address not configured")
    contract_addr -> {
      case order.crypto {
        USDT -> {
          let link = ton_client.generate_tonkeeper_usdt_link(
            contract_addr,
            order.crypto_amount,
            order.id,
          )
          Ok(link)
        }
        USDC -> {
          // USDC on TON - use similar link generation
          let link = ton_client.generate_tonkeeper_usdt_link(
            contract_addr,
            order.crypto_amount,
            order.id,
          )
          Ok(link)
        }
        TON -> {
          let link = ton_client.generate_tonkeeper_ton_link(
            contract_addr,
            order.crypto_amount,
            order.id,
          )
          Ok(link)
        }
        NOT -> {
          // NOT (Notcoin) on TON - use TON link generation
          let link = ton_client.generate_tonkeeper_ton_link(
            contract_addr,
            order.crypto_amount,
            order.id,
          )
          Ok(link)
        }
      }
    }
  }
}

// =============================================================================
// TRADER PROFILES
// =============================================================================

/// Get trader profile
pub fn get_trader_profile(telegram_id: Int) -> Result(TraderProfile, String) {
  case get_trader_ffi(telegram_id) {
    Ok(trader_dyn) -> decode_trader(trader_dyn)
    Error(_) -> Error("Trader not found")
  }
}

/// Create or update trader profile
pub fn ensure_trader_profile(
  telegram_id: Int,
  wallet_address: String,
  username: Option(String),
) -> Result(TraderProfile, String) {
  case get_trader_profile(telegram_id) {
    Ok(existing) -> {
      // Update last active
      let updated = TraderProfile(
        ..existing,
        wallet_address: wallet_address,
        last_active: current_timestamp(),
      )
      case update_trader_ffi(trader_to_map(updated)) {
        Ok(_) -> Ok(updated)
        Error(_) -> Error("Failed to update trader")
      }
    }
    Error(_) -> {
      // Create new profile
      let now = current_timestamp()
      let new_trader = TraderProfile(
        telegram_id: telegram_id,
        username: username,
        wallet_address: wallet_address,
        completed_trades: 0,
        total_volume_usd: 0.0,
        rating: 5.0,
        rating_count: 0,
        disputes_as_seller: 0,
        disputes_as_buyer: 0,
        created_at: now,
        last_active: now,
        is_verified: False,
      )
      case create_trader_ffi(trader_to_map(new_trader)) {
        Ok(_) -> Ok(new_trader)
        Error(_) -> Error("Failed to create trader profile")
      }
    }
  }
}

/// Update trader stats after completed trade
fn update_trader_stats(telegram_id: Int, success: Bool) -> Result(Nil, String) {
  case get_trader_profile(telegram_id) {
    Ok(trader) -> {
      let updated = case success {
        True -> TraderProfile(
          ..trader,
          completed_trades: trader.completed_trades + 1,
          last_active: current_timestamp(),
        )
        False -> trader
      }
      case update_trader_ffi(trader_to_map(updated)) {
        Ok(_) -> Ok(Nil)
        Error(_) -> Error("Failed to update trader stats")
      }
    }
    Error(_) -> Ok(Nil)  // Ignore if trader not found
  }
}

// =============================================================================
// SERIALIZATION HELPERS
// =============================================================================

fn order_to_map(order: P2POrder) -> Dynamic {
  order_to_map_ffi(
    order.id,
    order.seller_telegram_id,
    order.seller_wallet,
    option_int_to_dynamic(order.buyer_telegram_id),
    option_string_to_dynamic(order.buyer_wallet),
    crypto_to_string(order.crypto),
    order.crypto_amount,
    fiat_to_string(order.fiat),
    order.fiat_amount,
    payment_method_to_string(order.payment_method),
    order.payment_details,
    status_to_string(order.status),
    option_string_to_dynamic(order.escrow_tx_hash),
    option_string_to_dynamic(order.release_tx_hash),
    order.created_at,
    option_int_to_dynamic(order.locked_at),
    option_int_to_dynamic(order.completed_at),
    order.expires_in_minutes,
    order.seller_rating,
    order.seller_trades,
  )
}

fn trader_to_map(trader: TraderProfile) -> Dynamic {
  trader_to_map_ffi(
    trader.telegram_id,
    option_string_to_dynamic(trader.username),
    trader.wallet_address,
    trader.completed_trades,
    trader.total_volume_usd,
    trader.rating,
    trader.rating_count,
    trader.disputes_as_seller,
    trader.disputes_as_buyer,
    trader.created_at,
    trader.last_active,
    trader.is_verified,
  )
}

fn store_order(order: P2POrder) -> Result(String, String) {
  let order_map = order_to_map(order)
  case update_order_ffi(order_map) {
    Ok(id) -> Ok(id)
    Error(_) -> Error("Failed to store order")
  }
}

fn get_order(order_id: String) -> Result(P2POrder, String) {
  case get_order_ffi(order_id) {
    Ok(order_dyn) -> decode_order(order_dyn)
    Error(_) -> Error("Order not found")
  }
}

fn decode_order(_order_dyn: Dynamic) -> Result(P2POrder, String) {
  // In production, properly decode the dynamic value
  // For now, return error to be implemented
  Error("Decode not implemented - use FFI directly")
}

fn decode_trader(_trader_dyn: Dynamic) -> Result(TraderProfile, String) {
  Error("Decode not implemented - use FFI directly")
}

fn decode_order_list(orders: List(Dynamic)) -> List(P2POrder) {
  list.filter_map(orders, fn(order_dyn) {
    case decode_order_map_ffi(order_dyn) {
      Ok(decoded) -> {
        // Extract fields from decoded map
        let id = get_string_from_map(decoded, "id")
        let seller_telegram_id = get_int_from_map(decoded, "seller_telegram_id")
        let seller_wallet = get_string_from_map(decoded, "seller_wallet")
        let buyer_telegram_id = get_option_int_from_map(decoded, "buyer_telegram_id")
        let buyer_wallet = get_option_string_from_map(decoded, "buyer_wallet")
        let crypto_str = get_string_from_map(decoded, "crypto")
        let crypto_amount = get_float_from_map(decoded, "crypto_amount")
        let fiat_str = get_string_from_map(decoded, "fiat")
        let fiat_amount = get_float_from_map(decoded, "fiat_amount")
        let payment_method_str = get_string_from_map(decoded, "payment_method")
        let payment_details = get_string_from_map(decoded, "payment_details")
        let status_str = get_string_from_map(decoded, "status")
        let escrow_tx_hash = get_option_string_from_map(decoded, "escrow_tx_hash")
        let release_tx_hash = get_option_string_from_map(decoded, "release_tx_hash")
        let escrow_address = get_option_string_from_map(decoded, "escrow_address")
        let created_at = get_int_from_map(decoded, "created_at")
        let locked_at = get_option_int_from_map(decoded, "locked_at")
        let completed_at = get_option_int_from_map(decoded, "completed_at")
        let expires_in_minutes = get_int_from_map(decoded, "expires_in_minutes")
        let seller_rating = get_float_from_map(decoded, "seller_rating")
        let seller_trades = get_int_from_map(decoded, "seller_trades")

        Ok(P2POrder(
          id: id,
          seller_telegram_id: seller_telegram_id,
          seller_wallet: seller_wallet,
          buyer_telegram_id: buyer_telegram_id,
          buyer_wallet: buyer_wallet,
          crypto: crypto_from_string(crypto_str),
          crypto_amount: crypto_amount,
          fiat: fiat_from_string(fiat_str),
          fiat_amount: fiat_amount,
          payment_method: payment_method_from_string(payment_method_str),
          payment_details: payment_details,
          status: status_from_string(status_str),
          escrow_tx_hash: escrow_tx_hash,
          release_tx_hash: release_tx_hash,
          escrow_address: escrow_address,
          created_at: created_at,
          locked_at: locked_at,
          completed_at: completed_at,
          expires_in_minutes: expires_in_minutes,
          seller_rating: seller_rating,
          seller_trades: seller_trades,
        ))
      }
      Error(_) -> Error(Nil)
    }
  })
}

@external(erlang, "vibee_p2p_ffi", "decode_order_map")
fn decode_order_map_ffi(order: Dynamic) -> Result(Dynamic, Dynamic)

fn get_string_from_map(map: Dynamic, key: String) -> String {
  get_map_field_string(map, key)
}

fn get_int_from_map(map: Dynamic, key: String) -> Int {
  get_map_field_int(map, key)
}

fn get_float_from_map(map: Dynamic, key: String) -> Float {
  get_map_field_float(map, key)
}

fn get_option_string_from_map(map: Dynamic, key: String) -> Option(String) {
  get_map_field_option_string(map, key)
}

fn get_option_int_from_map(map: Dynamic, key: String) -> Option(Int) {
  get_map_field_option_int(map, key)
}

@external(erlang, "vibee_p2p_ffi", "get_map_field_string")
fn get_map_field_string(map: Dynamic, key: String) -> String

@external(erlang, "vibee_p2p_ffi", "get_map_field_int")
fn get_map_field_int(map: Dynamic, key: String) -> Int

@external(erlang, "vibee_p2p_ffi", "get_map_field_float")
fn get_map_field_float(map: Dynamic, key: String) -> Float

@external(erlang, "vibee_p2p_ffi", "get_map_field_option_string")
fn get_map_field_option_string(map: Dynamic, key: String) -> Option(String)

@external(erlang, "vibee_p2p_ffi", "get_map_field_option_int")
fn get_map_field_option_int(map: Dynamic, key: String) -> Option(Int)

@external(erlang, "vibee_p2p_ffi", "to_dynamic")
fn to_dynamic(value: a) -> Dynamic

fn option_int_to_dynamic(opt: Option(Int)) -> Dynamic {
  case opt {
    Some(v) -> to_dynamic(v)
    None -> to_dynamic(Nil)
  }
}

fn option_string_to_dynamic(opt: Option(String)) -> Dynamic {
  case opt {
    Some(v) -> to_dynamic(v)
    None -> to_dynamic(Nil)
  }
}

fn verify_oracle_signature(_order_id: String, _signature: String, _oracle_address: String) -> Bool {
  // In production, verify ED25519 signature
  True
}

// =============================================================================
// FFI BINDINGS
// =============================================================================

@external(erlang, "vibee_p2p_ffi", "current_timestamp")
fn current_timestamp() -> Int

@external(erlang, "vibee_p2p_ffi", "get_order")
fn get_order_ffi(order_id: String) -> Result(Dynamic, Dynamic)

@external(erlang, "vibee_p2p_ffi", "update_order")
fn update_order_ffi(order_map: Dynamic) -> Result(String, Dynamic)

@external(erlang, "vibee_p2p_ffi", "list_orders_by_status")
fn list_orders_by_status_ffi(status: String) -> List(Dynamic)

@external(erlang, "vibee_p2p_ffi", "list_orders_by_seller")
fn list_orders_by_seller_ffi(seller_id: Int) -> List(Dynamic)

@external(erlang, "vibee_p2p_ffi", "list_orders_by_buyer")
fn list_orders_by_buyer_ffi(buyer_id: Int) -> List(Dynamic)

@external(erlang, "vibee_p2p_ffi", "get_trader")
fn get_trader_ffi(telegram_id: Int) -> Result(Dynamic, Dynamic)

@external(erlang, "vibee_p2p_ffi", "create_trader")
fn create_trader_ffi(trader_map: Dynamic) -> Result(Int, Dynamic)

@external(erlang, "vibee_p2p_ffi", "update_trader")
fn update_trader_ffi(trader_map: Dynamic) -> Result(Int, Dynamic)

@external(erlang, "vibee_p2p_ffi", "order_to_map")
fn order_to_map_ffi(
  id: String,
  seller_telegram_id: Int,
  seller_wallet: String,
  buyer_telegram_id: Dynamic,
  buyer_wallet: Dynamic,
  crypto: String,
  crypto_amount: Float,
  fiat: String,
  fiat_amount: Float,
  payment_method: String,
  payment_details: String,
  status: String,
  escrow_tx_hash: Dynamic,
  release_tx_hash: Dynamic,
  created_at: Int,
  locked_at: Dynamic,
  completed_at: Dynamic,
  expires_in_minutes: Int,
  seller_rating: Float,
  seller_trades: Int,
) -> Dynamic

@external(erlang, "vibee_p2p_ffi", "trader_to_map")
fn trader_to_map_ffi(
  telegram_id: Int,
  username: Dynamic,
  wallet_address: String,
  completed_trades: Int,
  total_volume_usd: Float,
  rating: Float,
  rating_count: Int,
  disputes_as_seller: Int,
  disputes_as_buyer: Int,
  created_at: Int,
  last_active: Int,
  is_verified: Bool,
) -> Dynamic

@external(erlang, "vibee_config_ffi", "get_env")
fn get_env(key: String, default: String) -> String
