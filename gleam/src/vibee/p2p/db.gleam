// P2P PostgreSQL Database Operations
// CRUD for p2p_orders, p2p_traders, p2p_disputes, p2p_rates

import gleam/dynamic/decode.{type Decoder}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import pog
import vibee/p2p/types.{
  type CryptoCurrency, type FiatCurrency, type OrderStatus, type P2POrder,
  type PaymentMethod, P2POrder,
  crypto_from_string, crypto_to_string, fiat_from_string, fiat_to_string,
  payment_method_from_string, payment_method_to_string, status_from_string,
  status_to_string,
}

// =============================================================================
// Types
// =============================================================================

pub type P2PDbError {
  P2PDbConnectionError(String)
  P2PDbQueryError(String)
  P2PDbNotFound
}

/// Market rate from database
pub type MarketRate {
  MarketRate(
    crypto: CryptoCurrency,
    fiat: FiatCurrency,
    buy_rate: Float,
    sell_rate: Float,
    spread_percent: Float,
    source: String,
  )
}

/// P2P Statistics
pub type P2PStats {
  P2PStats(
    total_orders: Int,
    open_orders: Int,
    completed_orders: Int,
    total_volume_usd: Float,
    total_traders: Int,
    pending_disputes: Int,
  )
}

/// Trader profile from PostgreSQL
pub type DbTraderProfile {
  DbTraderProfile(
    telegram_id: Int,
    username: Option(String),
    wallet_address: Option(String),
    completed_trades: Int,
    total_volume_usd: Float,
    avg_rating: Float,
    disputes_as_seller: Int,
    disputes_as_buyer: Int,
    is_verified: Bool,
    is_banned: Bool,
    first_trade_at: Option(String),
    last_trade_at: Option(String),
  )
}

// =============================================================================
// Order CRUD
// =============================================================================

/// Create a new P2P order
pub fn create_order(
  pool: pog.Connection,
  order: P2POrder,
) -> Result(String, P2PDbError) {
  let sql =
    "INSERT INTO p2p_orders (
       order_id, seller_telegram_id, seller_wallet, crypto, crypto_amount,
       fiat, fiat_amount, rate, payment_method, payment_details, status,
       escrow_address, expires_at
     ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, NOW() + INTERVAL '24 hours')
     RETURNING order_id"

  let rate = order.fiat_amount /. order.crypto_amount

  let params = [
    pog.text(order.id),
    pog.int(order.seller_telegram_id),
    pog.text(order.seller_wallet),
    pog.text(crypto_to_string(order.crypto)),
    pog.float(order.crypto_amount),
    pog.text(fiat_to_string(order.fiat)),
    pog.float(order.fiat_amount),
    pog.float(rate),
    pog.text(payment_method_to_string(order.payment_method)),
    pog.text(order.payment_details),
    pog.text(status_to_string(order.status)),
    pog.nullable(pog.text, order.escrow_address),
  ]

  let id_decoder = {
    use id <- decode.field(0, decode.string)
    decode.success(id)
  }

  case
    pog.query(sql)
    |> add_parameters(params)
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [id])) -> Ok(id)
    Ok(_) -> Error(P2PDbQueryError("Failed to create order"))
    Error(e) -> Error(P2PDbQueryError(pog_error_to_string(e)))
  }
}

/// Get order by ID
pub fn get_order(
  pool: pog.Connection,
  order_id: String,
) -> Result(P2POrder, P2PDbError) {
  let sql =
    "SELECT order_id, seller_telegram_id, seller_wallet, buyer_telegram_id, buyer_wallet,
            crypto, crypto_amount, fiat, fiat_amount, payment_method, payment_details,
            status, escrow_tx_hash, release_tx_hash, escrow_address,
            created_at::text, locked_at::text, completed_at::text
     FROM p2p_orders WHERE order_id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.text(order_id)])
    |> pog.returning(decode_order())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [order])) -> Ok(order)
    Ok(pog.Returned(_, [])) -> Error(P2PDbNotFound)
    Ok(_) -> Error(P2PDbNotFound)
    Error(e) -> Error(P2PDbQueryError(pog_error_to_string(e)))
  }
}

/// List open orders with optional filters
pub fn list_open_orders(
  pool: pog.Connection,
  crypto_filter: Option(CryptoCurrency),
  fiat_filter: Option(FiatCurrency),
  limit: Int,
) -> Result(List(P2POrder), P2PDbError) {
  let #(sql, params) = case crypto_filter, fiat_filter {
    Some(crypto), Some(fiat) -> #(
      "SELECT order_id, seller_telegram_id, seller_wallet, buyer_telegram_id, buyer_wallet,
              crypto, crypto_amount, fiat, fiat_amount, payment_method, payment_details,
              status, escrow_tx_hash, release_tx_hash, escrow_address,
              created_at::text, locked_at::text, completed_at::text
       FROM p2p_orders WHERE status = 'open' AND crypto = $1 AND fiat = $2
       ORDER BY created_at DESC LIMIT $3",
      [
        pog.text(crypto_to_string(crypto)),
        pog.text(fiat_to_string(fiat)),
        pog.int(limit),
      ],
    )
    Some(crypto), None -> #(
      "SELECT order_id, seller_telegram_id, seller_wallet, buyer_telegram_id, buyer_wallet,
              crypto, crypto_amount, fiat, fiat_amount, payment_method, payment_details,
              status, escrow_tx_hash, release_tx_hash, escrow_address,
              created_at::text, locked_at::text, completed_at::text
       FROM p2p_orders WHERE status = 'open' AND crypto = $1
       ORDER BY created_at DESC LIMIT $2",
      [pog.text(crypto_to_string(crypto)), pog.int(limit)],
    )
    None, Some(fiat) -> #(
      "SELECT order_id, seller_telegram_id, seller_wallet, buyer_telegram_id, buyer_wallet,
              crypto, crypto_amount, fiat, fiat_amount, payment_method, payment_details,
              status, escrow_tx_hash, release_tx_hash, escrow_address,
              created_at::text, locked_at::text, completed_at::text
       FROM p2p_orders WHERE status = 'open' AND fiat = $1
       ORDER BY created_at DESC LIMIT $2",
      [pog.text(fiat_to_string(fiat)), pog.int(limit)],
    )
    None, None -> #(
      "SELECT order_id, seller_telegram_id, seller_wallet, buyer_telegram_id, buyer_wallet,
              crypto, crypto_amount, fiat, fiat_amount, payment_method, payment_details,
              status, escrow_tx_hash, release_tx_hash, escrow_address,
              created_at::text, locked_at::text, completed_at::text
       FROM p2p_orders WHERE status = 'open'
       ORDER BY created_at DESC LIMIT $1",
      [pog.int(limit)],
    )
  }

  case
    pog.query(sql)
    |> add_parameters(params)
    |> pog.returning(decode_order())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, orders)) -> Ok(orders)
    Error(e) -> Error(P2PDbQueryError(pog_error_to_string(e)))
  }
}

/// Get orders for a user (as seller or buyer)
pub fn get_user_orders(
  pool: pog.Connection,
  telegram_id: Int,
  limit: Int,
) -> Result(#(List(P2POrder), List(P2POrder)), P2PDbError) {
  // Get as seller
  let seller_sql =
    "SELECT order_id, seller_telegram_id, seller_wallet, buyer_telegram_id, buyer_wallet,
            crypto, crypto_amount, fiat, fiat_amount, payment_method, payment_details,
            status, escrow_tx_hash, release_tx_hash, escrow_address,
            created_at::text, locked_at::text, completed_at::text
     FROM p2p_orders WHERE seller_telegram_id = $1
     ORDER BY created_at DESC LIMIT $2"

  let seller_result =
    pog.query(seller_sql)
    |> add_parameters([pog.int(telegram_id), pog.int(limit)])
    |> pog.returning(decode_order())
    |> pog.execute(pool)

  // Get as buyer
  let buyer_sql =
    "SELECT order_id, seller_telegram_id, seller_wallet, buyer_telegram_id, buyer_wallet,
            crypto, crypto_amount, fiat, fiat_amount, payment_method, payment_details,
            status, escrow_tx_hash, release_tx_hash, escrow_address,
            created_at::text, locked_at::text, completed_at::text
     FROM p2p_orders WHERE buyer_telegram_id = $1
     ORDER BY created_at DESC LIMIT $2"

  let buyer_result =
    pog.query(buyer_sql)
    |> add_parameters([pog.int(telegram_id), pog.int(limit)])
    |> pog.returning(decode_order())
    |> pog.execute(pool)

  case seller_result, buyer_result {
    Ok(pog.Returned(_, sellers)), Ok(pog.Returned(_, buyers)) ->
      Ok(#(sellers, buyers))
    Error(e), _ -> Error(P2PDbQueryError(pog_error_to_string(e)))
    _, Error(e) -> Error(P2PDbQueryError(pog_error_to_string(e)))
  }
}

/// Update order status
pub fn update_order_status(
  pool: pog.Connection,
  order_id: String,
  new_status: OrderStatus,
) -> Result(Nil, P2PDbError) {
  let extra_fields = case new_status {
    types.Locked -> ", locked_at = NOW(), payment_deadline = NOW() + INTERVAL '30 minutes'"
    types.FiatSent -> ", fiat_sent_at = NOW()"
    types.Completed -> ", completed_at = NOW()"
    _ -> ""
  }

  let sql =
    "UPDATE p2p_orders SET status = $2" <> extra_fields <> " WHERE order_id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.text(order_id), pog.text(status_to_string(new_status))])
    |> pog.execute(pool)
  {
    Ok(pog.Returned(count, _)) if count > 0 -> Ok(Nil)
    Ok(_) -> Error(P2PDbNotFound)
    Error(e) -> Error(P2PDbQueryError(pog_error_to_string(e)))
  }
}

/// Lock order by buyer
pub fn lock_order(
  pool: pog.Connection,
  order_id: String,
  buyer_telegram_id: Int,
  buyer_wallet: Option(String),
) -> Result(Nil, P2PDbError) {
  let sql =
    "UPDATE p2p_orders
     SET status = 'locked', buyer_telegram_id = $2, buyer_wallet = $3,
         locked_at = NOW(), payment_deadline = NOW() + INTERVAL '30 minutes'
     WHERE order_id = $1 AND status = 'open'"

  case
    pog.query(sql)
    |> add_parameters([
      pog.text(order_id),
      pog.int(buyer_telegram_id),
      pog.nullable(pog.text, buyer_wallet),
    ])
    |> pog.execute(pool)
  {
    Ok(pog.Returned(count, _)) if count > 0 -> Ok(Nil)
    Ok(_) -> Error(P2PDbNotFound)
    Error(e) -> Error(P2PDbQueryError(pog_error_to_string(e)))
  }
}

/// Complete order with release tx
pub fn complete_order(
  pool: pog.Connection,
  order_id: String,
  release_tx_hash: String,
) -> Result(Nil, P2PDbError) {
  let sql =
    "UPDATE p2p_orders
     SET status = 'completed', release_tx_hash = $2, completed_at = NOW()
     WHERE order_id = $1 AND status = 'fiat_sent'"

  case
    pog.query(sql)
    |> add_parameters([pog.text(order_id), pog.text(release_tx_hash)])
    |> pog.execute(pool)
  {
    Ok(pog.Returned(count, _)) if count > 0 -> Ok(Nil)
    Ok(_) -> Error(P2PDbNotFound)
    Error(e) -> Error(P2PDbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Trader Profile CRUD
// =============================================================================

/// Get or create trader profile
pub fn get_or_create_trader(
  pool: pog.Connection,
  telegram_id: Int,
) -> Result(DbTraderProfile, P2PDbError) {
  let sql =
    "INSERT INTO p2p_traders (telegram_id)
     VALUES ($1)
     ON CONFLICT (telegram_id) DO UPDATE SET updated_at = NOW()
     RETURNING telegram_id, username, wallet_address, completed_trades,
               total_volume_usd, avg_rating, disputes_as_seller, disputes_as_buyer,
               is_verified, is_banned, first_trade_at::text, last_trade_at::text"

  case
    pog.query(sql)
    |> add_parameters([pog.int(telegram_id)])
    |> pog.returning(decode_trader())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [trader])) -> Ok(trader)
    Ok(_) -> Error(P2PDbQueryError("Failed to get/create trader"))
    Error(e) -> Error(P2PDbQueryError(pog_error_to_string(e)))
  }
}

/// Get trader profile
pub fn get_trader(
  pool: pog.Connection,
  telegram_id: Int,
) -> Result(DbTraderProfile, P2PDbError) {
  let sql =
    "SELECT telegram_id, username, wallet_address, completed_trades,
            total_volume_usd, avg_rating, disputes_as_seller, disputes_as_buyer,
            is_verified, is_banned, first_trade_at::text, last_trade_at::text
     FROM p2p_traders WHERE telegram_id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.int(telegram_id)])
    |> pog.returning(decode_trader())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [trader])) -> Ok(trader)
    Ok(pog.Returned(_, [])) -> Error(P2PDbNotFound)
    Ok(_) -> Error(P2PDbNotFound)
    Error(e) -> Error(P2PDbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Market Rates
// =============================================================================

/// Get all market rates
pub fn get_rates(pool: pog.Connection) -> Result(List(MarketRate), P2PDbError) {
  let sql =
    "SELECT crypto, fiat, buy_rate, sell_rate, spread_percent, source
     FROM p2p_rates ORDER BY crypto, fiat"

  case
    pog.query(sql)
    |> pog.returning(decode_rate())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, rates)) -> Ok(rates)
    Error(e) -> Error(P2PDbQueryError(pog_error_to_string(e)))
  }
}

/// Get rate for specific pair
pub fn get_rate(
  pool: pog.Connection,
  crypto: CryptoCurrency,
  fiat: FiatCurrency,
) -> Result(MarketRate, P2PDbError) {
  let sql =
    "SELECT crypto, fiat, buy_rate, sell_rate, spread_percent, source
     FROM p2p_rates WHERE crypto = $1 AND fiat = $2"

  case
    pog.query(sql)
    |> add_parameters([
      pog.text(crypto_to_string(crypto)),
      pog.text(fiat_to_string(fiat)),
    ])
    |> pog.returning(decode_rate())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [rate])) -> Ok(rate)
    Ok(pog.Returned(_, [])) -> Error(P2PDbNotFound)
    Ok(_) -> Error(P2PDbNotFound)
    Error(e) -> Error(P2PDbQueryError(pog_error_to_string(e)))
  }
}

/// Update market rate
pub fn update_rate(
  pool: pog.Connection,
  crypto: CryptoCurrency,
  fiat: FiatCurrency,
  buy_rate: Float,
  sell_rate: Float,
  source: String,
) -> Result(Nil, P2PDbError) {
  let spread = { buy_rate -. sell_rate } /. sell_rate *. 100.0

  let sql =
    "INSERT INTO p2p_rates (crypto, fiat, buy_rate, sell_rate, spread_percent, source, updated_at)
     VALUES ($1, $2, $3, $4, $5, $6, NOW())
     ON CONFLICT (crypto, fiat) DO UPDATE SET
       buy_rate = $3, sell_rate = $4, spread_percent = $5, source = $6, updated_at = NOW()"

  case
    pog.query(sql)
    |> add_parameters([
      pog.text(crypto_to_string(crypto)),
      pog.text(fiat_to_string(fiat)),
      pog.float(buy_rate),
      pog.float(sell_rate),
      pog.float(spread),
      pog.text(source),
    ])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(P2PDbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Statistics
// =============================================================================

/// Get P2P platform statistics
pub fn get_stats(pool: pog.Connection) -> Result(P2PStats, P2PDbError) {
  let sql = "SELECT * FROM get_p2p_stats()"

  case
    pog.query(sql)
    |> pog.returning(decode_stats())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [stats])) -> Ok(stats)
    Ok(_) -> Ok(P2PStats(0, 0, 0, 0.0, 0, 0))
    Error(e) -> Error(P2PDbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Helpers
// =============================================================================

fn add_parameters(query: pog.Query(t), params: List(pog.Value)) -> pog.Query(t) {
  list.fold(params, query, fn(q, p) { pog.parameter(q, p) })
}

fn pog_error_to_string(err: pog.QueryError) -> String {
  case err {
    pog.ConnectionUnavailable -> "Connection unavailable"
    pog.ConstraintViolated(msg, constraint, _) ->
      "Constraint violated: " <> msg <> " (" <> constraint <> ")"
    pog.PostgresqlError(code, name, msg) ->
      "PostgreSQL error " <> code <> " [" <> name <> "]: " <> msg
    pog.UnexpectedArgumentCount(expected, got) ->
      "Unexpected argument count: expected "
      <> int.to_string(expected)
      <> ", got "
      <> int.to_string(got)
    pog.UnexpectedArgumentType(expected, got) ->
      "Unexpected argument type: expected " <> expected <> ", got " <> got
    pog.UnexpectedResultType(_) -> "Unexpected result type"
    pog.QueryTimeout -> "Query timeout"
  }
}

// =============================================================================
// Decoders
// =============================================================================

fn decode_order() -> Decoder(P2POrder) {
  use order_id <- decode.field(0, decode.string)
  use seller_id <- decode.field(1, decode.int)
  use seller_wallet <- decode.field(2, decode.string)
  use buyer_id <- decode.field(3, decode.optional(decode.int))
  use buyer_wallet <- decode.field(4, decode.optional(decode.string))
  use crypto <- decode.field(5, decode.string)
  use crypto_amount <- decode.field(6, decode.float)
  use fiat <- decode.field(7, decode.string)
  use fiat_amount <- decode.field(8, decode.float)
  use payment_method <- decode.field(9, decode.string)
  use payment_details <- decode.field(10, decode.string)
  use status <- decode.field(11, decode.string)
  use escrow_tx <- decode.field(12, decode.optional(decode.string))
  use release_tx <- decode.field(13, decode.optional(decode.string))
  use escrow_addr <- decode.field(14, decode.optional(decode.string))
  use _created_at <- decode.field(15, decode.string)
  use _locked_at <- decode.field(16, decode.optional(decode.string))
  use _completed_at <- decode.field(17, decode.optional(decode.string))

  // Convert ISO timestamps to Unix - for now use 0 as placeholder
  // Real implementation would parse ISO string to Unix timestamp
  decode.success(P2POrder(
    id: order_id,
    seller_telegram_id: seller_id,
    seller_wallet: seller_wallet,
    buyer_telegram_id: buyer_id,
    buyer_wallet: buyer_wallet,
    crypto: crypto_from_string(crypto),
    crypto_amount: crypto_amount,
    fiat: fiat_from_string(fiat),
    fiat_amount: fiat_amount,
    payment_method: payment_method_from_string(payment_method),
    payment_details: payment_details,
    status: status_from_string(status),
    escrow_tx_hash: escrow_tx,
    release_tx_hash: release_tx,
    escrow_address: escrow_addr,
    created_at: 0,  // Will be set from EPOCH in real impl
    locked_at: None,
    completed_at: None,
    expires_in_minutes: 1440,  // 24 hours default
    seller_rating: 5.0,
    seller_trades: 0,
  ))
}

fn decode_trader() -> Decoder(DbTraderProfile) {
  use telegram_id <- decode.field(0, decode.int)
  use username <- decode.field(1, decode.optional(decode.string))
  use wallet <- decode.field(2, decode.optional(decode.string))
  use completed <- decode.field(3, decode.int)
  use volume <- decode.field(4, decode.float)
  use rating <- decode.field(5, decode.float)
  use disputes_seller <- decode.field(6, decode.int)
  use disputes_buyer <- decode.field(7, decode.int)
  use verified <- decode.field(8, decode.bool)
  use banned <- decode.field(9, decode.bool)
  use first_trade <- decode.field(10, decode.optional(decode.string))
  use last_trade <- decode.field(11, decode.optional(decode.string))

  decode.success(DbTraderProfile(
    telegram_id: telegram_id,
    username: username,
    wallet_address: wallet,
    completed_trades: completed,
    total_volume_usd: volume,
    avg_rating: rating,
    disputes_as_seller: disputes_seller,
    disputes_as_buyer: disputes_buyer,
    is_verified: verified,
    is_banned: banned,
    first_trade_at: first_trade,
    last_trade_at: last_trade,
  ))
}

fn decode_rate() -> Decoder(MarketRate) {
  use crypto <- decode.field(0, decode.string)
  use fiat <- decode.field(1, decode.string)
  use buy_rate <- decode.field(2, decode.float)
  use sell_rate <- decode.field(3, decode.float)
  use spread <- decode.field(4, decode.float)
  use source <- decode.field(5, decode.string)

  decode.success(MarketRate(
    crypto: crypto_from_string(crypto),
    fiat: fiat_from_string(fiat),
    buy_rate: buy_rate,
    sell_rate: sell_rate,
    spread_percent: spread,
    source: source,
  ))
}

fn decode_stats() -> Decoder(P2PStats) {
  use total <- decode.field(0, decode.int)
  use open <- decode.field(1, decode.int)
  use completed <- decode.field(2, decode.int)
  use volume <- decode.field(3, decode.float)
  use traders <- decode.field(4, decode.int)
  use disputes <- decode.field(5, decode.int)

  decode.success(P2PStats(
    total_orders: total,
    open_orders: open,
    completed_orders: completed,
    total_volume_usd: volume,
    total_traders: traders,
    pending_disputes: disputes,
  ))
}
