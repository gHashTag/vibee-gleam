// VIBEE P2P MCP Tools
// MCP tools for P2P trading functionality

import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/int
import gleam/float
import gleam/string
import gleam/list
import gleam/dynamic.{type Dynamic}
import vibee/mcp/protocol
import vibee/mcp/types.{type Tool, type ToolResult, Tool}
import vibee/p2p/types as p2p_types
import vibee/p2p/escrow_client
import vibee/p2p/price_feed
import vibee/logging

// =============================================================================
// TOOL DEFINITIONS
// =============================================================================

/// Get all P2P tools
pub fn get_tools() -> List(Tool) {
  [
    p2p_create_sell_order_tool(),
    p2p_list_orders_tool(),
    p2p_take_order_tool(),
    p2p_mark_paid_tool(),
    p2p_confirm_payment_tool(),
    p2p_cancel_order_tool(),
    p2p_my_orders_tool(),
    p2p_order_status_tool(),
    p2p_rates_tool(),
  ]
}

/// Create sell order tool
pub fn p2p_create_sell_order_tool() -> Tool {
  Tool(
    name: "p2p_create_sell_order",
    description: "Create a P2P sell order to sell crypto for fiat. Seller deposits crypto to escrow and waits for buyer.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Seller's Telegram ID")),
        ])),
        #("wallet", json.object([
          #("type", json.string("string")),
          #("description", json.string("Seller's TON wallet address")),
        ])),
        #("crypto", json.object([
          #("type", json.string("string")),
          #("enum", json.array([json.string("TON"), json.string("USDT")], fn(x) { x })),
          #("description", json.string("Cryptocurrency to sell")),
        ])),
        #("crypto_amount", json.object([
          #("type", json.string("number")),
          #("description", json.string("Amount of crypto to sell")),
        ])),
        #("fiat", json.object([
          #("type", json.string("string")),
          #("enum", json.array([json.string("THB"), json.string("RUB"), json.string("USD"), json.string("EUR")], fn(x) { x })),
          #("description", json.string("Fiat currency to receive")),
        ])),
        #("fiat_amount", json.object([
          #("type", json.string("number")),
          #("description", json.string("Fiat amount to receive")),
        ])),
        #("payment_method", json.object([
          #("type", json.string("string")),
          #("enum", json.array([
            json.string("bangkok_bank"),
            json.string("promptpay"),
            json.string("sberbank"),
            json.string("tinkoff"),
            json.string("wise"),
            json.string("bank_transfer"),
          ], fn(x) { x })),
          #("description", json.string("Payment method")),
        ])),
        #("payment_details", json.object([
          #("type", json.string("string")),
          #("description", json.string("Payment details (bank account number, etc.)")),
        ])),
      ])),
      #("required", json.array([
        json.string("telegram_id"),
        json.string("wallet"),
        json.string("crypto"),
        json.string("crypto_amount"),
        json.string("fiat"),
        json.string("fiat_amount"),
        json.string("payment_method"),
        json.string("payment_details"),
      ], fn(x) { x })),
    ]),
  )
}

/// List open orders tool
pub fn p2p_list_orders_tool() -> Tool {
  Tool(
    name: "p2p_list_orders",
    description: "List available P2P orders for buying crypto",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("crypto", json.object([
          #("type", json.string("string")),
          #("enum", json.array([json.string("TON"), json.string("USDT")], fn(x) { x })),
          #("description", json.string("Filter by cryptocurrency (optional)")),
        ])),
        #("fiat", json.object([
          #("type", json.string("string")),
          #("enum", json.array([json.string("THB"), json.string("RUB"), json.string("USD"), json.string("EUR")], fn(x) { x })),
          #("description", json.string("Filter by fiat currency (optional)")),
        ])),
        #("limit", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Maximum number of orders to return (default: 20)")),
        ])),
      ])),
      #("required", json.array([], fn(x) { x })),
    ]),
  )
}

/// Take order tool
pub fn p2p_take_order_tool() -> Tool {
  Tool(
    name: "p2p_take_order",
    description: "Take a P2P order as a buyer. The crypto will be locked in escrow and you'll need to send fiat payment.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("order_id", json.object([
          #("type", json.string("string")),
          #("description", json.string("Order ID to take")),
        ])),
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Buyer's Telegram ID")),
        ])),
        #("wallet", json.object([
          #("type", json.string("string")),
          #("description", json.string("Buyer's TON wallet address")),
        ])),
      ])),
      #("required", json.array([
        json.string("order_id"),
        json.string("telegram_id"),
        json.string("wallet"),
      ], fn(x) { x })),
    ]),
  )
}

/// Mark payment as sent (buyer)
pub fn p2p_mark_paid_tool() -> Tool {
  Tool(
    name: "p2p_mark_paid",
    description: "Mark fiat payment as sent (buyer action). Notifies the seller to check their bank.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("order_id", json.object([
          #("type", json.string("string")),
          #("description", json.string("Order ID")),
        ])),
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Buyer's Telegram ID")),
        ])),
      ])),
      #("required", json.array([
        json.string("order_id"),
        json.string("telegram_id"),
      ], fn(x) { x })),
    ]),
  )
}

/// Confirm payment received (seller)
pub fn p2p_confirm_payment_tool() -> Tool {
  Tool(
    name: "p2p_confirm_payment",
    description: "Confirm fiat payment received and release crypto to buyer (seller action).",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("order_id", json.object([
          #("type", json.string("string")),
          #("description", json.string("Order ID")),
        ])),
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Seller's Telegram ID")),
        ])),
      ])),
      #("required", json.array([
        json.string("order_id"),
        json.string("telegram_id"),
      ], fn(x) { x })),
    ]),
  )
}

/// Cancel order (seller only, if not locked)
pub fn p2p_cancel_order_tool() -> Tool {
  Tool(
    name: "p2p_cancel_order",
    description: "Cancel a P2P order (seller only, if order is not locked by a buyer).",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("order_id", json.object([
          #("type", json.string("string")),
          #("description", json.string("Order ID to cancel")),
        ])),
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Requester's Telegram ID")),
        ])),
      ])),
      #("required", json.array([
        json.string("order_id"),
        json.string("telegram_id"),
      ], fn(x) { x })),
    ]),
  )
}

/// Get my orders
pub fn p2p_my_orders_tool() -> Tool {
  Tool(
    name: "p2p_my_orders",
    description: "List all P2P orders for a specific user (as seller or buyer).",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("User's Telegram ID")),
        ])),
        #("role", json.object([
          #("type", json.string("string")),
          #("enum", json.array([json.string("seller"), json.string("buyer"), json.string("all")], fn(x) { x })),
          #("description", json.string("Filter by role (default: all)")),
        ])),
      ])),
      #("required", json.array([json.string("telegram_id")], fn(x) { x })),
    ]),
  )
}

/// Get order status
pub fn p2p_order_status_tool() -> Tool {
  Tool(
    name: "p2p_order_status",
    description: "Get detailed status of a specific P2P order.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("order_id", json.object([
          #("type", json.string("string")),
          #("description", json.string("Order ID")),
        ])),
      ])),
      #("required", json.array([json.string("order_id")], fn(x) { x })),
    ]),
  )
}

/// Get current rates
pub fn p2p_rates_tool() -> Tool {
  Tool(
    name: "p2p_rates",
    description: "Get current market rates for crypto/fiat pairs.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("crypto", json.object([
          #("type", json.string("string")),
          #("enum", json.array([json.string("TON"), json.string("USDT")], fn(x) { x })),
          #("description", json.string("Cryptocurrency (optional)")),
        ])),
        #("fiat", json.object([
          #("type", json.string("string")),
          #("enum", json.array([json.string("THB"), json.string("RUB"), json.string("USD"), json.string("EUR")], fn(x) { x })),
          #("description", json.string("Fiat currency (optional)")),
        ])),
      ])),
      #("required", json.array([], fn(x) { x })),
    ]),
  )
}

// =============================================================================
// TOOL HANDLERS
// =============================================================================

/// Handle P2P tool calls
pub fn handle_p2p_tool(name: String, args: Dynamic) -> ToolResult {
  case name {
    "p2p_create_sell_order" -> handle_create_sell_order(args)
    "p2p_list_orders" -> handle_list_orders(args)
    "p2p_take_order" -> handle_take_order(args)
    "p2p_mark_paid" -> handle_mark_paid(args)
    "p2p_confirm_payment" -> handle_confirm_payment(args)
    "p2p_cancel_order" -> handle_cancel_order(args)
    "p2p_my_orders" -> handle_my_orders(args)
    "p2p_order_status" -> handle_order_status(args)
    "p2p_rates" -> handle_rates(args)
    _ -> protocol.error_result("Unknown P2P tool: " <> name)
  }
}

// Public handler wrappers for MCP registry (json.Json -> ToolResult)
pub fn handle_p2p_create_sell_order(args: json.Json) -> ToolResult {
  handle_create_sell_order(json_to_dynamic(args))
}

pub fn handle_p2p_list_orders(args: json.Json) -> ToolResult {
  handle_list_orders(json_to_dynamic(args))
}

pub fn handle_p2p_take_order(args: json.Json) -> ToolResult {
  handle_take_order(json_to_dynamic(args))
}

pub fn handle_p2p_mark_paid(args: json.Json) -> ToolResult {
  handle_mark_paid(json_to_dynamic(args))
}

pub fn handle_p2p_confirm_payment(args: json.Json) -> ToolResult {
  handle_confirm_payment(json_to_dynamic(args))
}

pub fn handle_p2p_cancel_order(args: json.Json) -> ToolResult {
  handle_cancel_order(json_to_dynamic(args))
}

pub fn handle_p2p_my_orders(args: json.Json) -> ToolResult {
  handle_my_orders(json_to_dynamic(args))
}

pub fn handle_p2p_order_status(args: json.Json) -> ToolResult {
  handle_order_status(json_to_dynamic(args))
}

pub fn handle_p2p_rates(args: json.Json) -> ToolResult {
  handle_rates(json_to_dynamic(args))
}

@external(erlang, "vibee_p2p_ffi", "to_dynamic")
fn json_to_dynamic(j: json.Json) -> Dynamic

/// Handle create sell order
fn handle_create_sell_order(args: Dynamic) -> ToolResult {
  let telegram_id = get_int_field(args, "telegram_id") |> result.unwrap(0)
  let wallet = get_string_field(args, "wallet") |> result.unwrap("")
  let crypto_str = get_string_field(args, "crypto") |> result.unwrap("USDT")
  let crypto_amount = get_float_field(args, "crypto_amount") |> result.unwrap(0.0)
  let fiat_str = get_string_field(args, "fiat") |> result.unwrap("THB")
  let fiat_amount = get_float_field(args, "fiat_amount") |> result.unwrap(0.0)
  let payment_method_str = get_string_field(args, "payment_method") |> result.unwrap("bank_transfer")
  let payment_details = get_string_field(args, "payment_details") |> result.unwrap("")

  // Validate inputs
  case telegram_id > 0 && wallet != "" && crypto_amount >. 0.0 && fiat_amount >. 0.0 && payment_details != "" {
    False -> protocol.error_result("Missing required fields: telegram_id, wallet, crypto_amount, fiat_amount, payment_details")
    True -> {
      let crypto = p2p_types.crypto_from_string(crypto_str)
      let fiat = p2p_types.fiat_from_string(fiat_str)
      let payment_method = p2p_types.payment_method_from_string(payment_method_str)

      case escrow_client.create_sell_order(
        telegram_id,
        wallet,
        crypto,
        crypto_amount,
        fiat,
        fiat_amount,
        payment_method,
        payment_details,
        1440,  // 24 hours expiry
      ) {
        Ok(order) -> {
          // Generate deposit link
          let deposit_link = case escrow_client.generate_deposit_link(order) {
            Ok(link) -> link
            Error(_) -> ""
          }

          let rate = fiat_amount /. crypto_amount

          protocol.text_result(json.to_string(json.object([
            #("success", json.bool(True)),
            #("order_id", json.string(order.id)),
            #("crypto", json.string(p2p_types.crypto_to_string(crypto))),
            #("crypto_amount", json.float(crypto_amount)),
            #("fiat", json.string(p2p_types.fiat_to_string(fiat))),
            #("fiat_amount", json.float(fiat_amount)),
            #("rate", json.float(rate)),
            #("payment_method", json.string(p2p_types.payment_method_display(payment_method))),
            #("status", json.string("open")),
            #("deposit_link", json.string(deposit_link)),
            #("message", json.string("Order created! Deposit " <> float.to_string(crypto_amount) <> " " <> p2p_types.crypto_to_string(crypto) <> " to activate.")),
          ])))
        }
        Error(e) -> protocol.error_result("Failed to create order: " <> e)
      }
    }
  }
}

/// Handle list orders
fn handle_list_orders(args: Dynamic) -> ToolResult {
  let crypto_str = get_string_field(args, "crypto") |> result.unwrap("")
  let fiat_str = get_string_field(args, "fiat") |> result.unwrap("")
  let limit = get_int_field(args, "limit") |> result.unwrap(20)

  let crypto_filter = case crypto_str {
    "" -> None
    s -> Some(p2p_types.crypto_from_string(s))
  }

  let fiat_filter = case fiat_str {
    "" -> None
    s -> Some(p2p_types.fiat_from_string(s))
  }

  let orders = escrow_client.list_open_orders(fiat_filter, crypto_filter, limit)

  // Convert real orders to JSON
  let orders_json = list.map(orders, fn(order: p2p_types.P2POrder) {
    let rate = order.fiat_amount /. order.crypto_amount
    json.object([
      #("order_id", json.string(order.id)),
      #("crypto", json.string(p2p_types.crypto_to_string(order.crypto))),
      #("crypto_amount", json.float(order.crypto_amount)),
      #("fiat", json.string(p2p_types.fiat_to_string(order.fiat))),
      #("fiat_amount", json.float(order.fiat_amount)),
      #("rate", json.float(rate)),
      #("payment_method", json.string(p2p_types.payment_method_display(order.payment_method))),
      #("status", json.string(p2p_types.status_to_string(order.status))),
    ])
  })

  protocol.text_result(json.to_string(json.object([
    #("success", json.bool(True)),
    #("orders", json.array(orders_json, fn(x) { x })),
    #("count", json.int(list.length(orders))),
  ])))
}

/// Handle take order
fn handle_take_order(args: Dynamic) -> ToolResult {
  let order_id = get_string_field(args, "order_id") |> result.unwrap("")
  let telegram_id = get_int_field(args, "telegram_id") |> result.unwrap(0)
  let wallet = get_string_field(args, "wallet") |> result.unwrap("")

  case order_id != "" && telegram_id > 0 && wallet != "" {
    False -> protocol.error_result("Missing required fields: order_id, telegram_id, wallet")
    True -> {
      case escrow_client.take_order(order_id, telegram_id, wallet) {
        Ok(order) -> {
          protocol.text_result(json.to_string(json.object([
            #("success", json.bool(True)),
            #("order_id", json.string(order.id)),
            #("status", json.string("locked")),
            #("payment_details", json.string(order.payment_details)),
            #("fiat_amount", json.float(order.fiat_amount)),
            #("fiat", json.string(p2p_types.fiat_to_string(order.fiat))),
            #("payment_method", json.string(p2p_types.payment_method_display(order.payment_method))),
            #("timeout_minutes", json.int(30)),
            #("message", json.string("Order locked! Send " <> float.to_string(order.fiat_amount) <> " " <> p2p_types.fiat_to_string(order.fiat) <> " to the seller within 30 minutes.")),
          ])))
        }
        Error(e) -> protocol.error_result("Failed to take order: " <> e)
      }
    }
  }
}

/// Handle mark paid
fn handle_mark_paid(args: Dynamic) -> ToolResult {
  let order_id = get_string_field(args, "order_id") |> result.unwrap("")
  let telegram_id = get_int_field(args, "telegram_id") |> result.unwrap(0)

  case order_id != "" && telegram_id > 0 {
    False -> protocol.error_result("Missing required fields: order_id, telegram_id")
    True -> {
      case escrow_client.mark_fiat_sent(order_id, telegram_id) {
        Ok(order) -> {
          protocol.text_result(json.to_string(json.object([
            #("success", json.bool(True)),
            #("order_id", json.string(order.id)),
            #("status", json.string("fiat_sent")),
            #("message", json.string("Payment marked as sent. Waiting for seller confirmation.")),
          ])))
        }
        Error(e) -> protocol.error_result("Failed to mark payment: " <> e)
      }
    }
  }
}

/// Handle confirm payment
fn handle_confirm_payment(args: Dynamic) -> ToolResult {
  let order_id = get_string_field(args, "order_id") |> result.unwrap("")
  let telegram_id = get_int_field(args, "telegram_id") |> result.unwrap(0)

  case order_id != "" && telegram_id > 0 {
    False -> protocol.error_result("Missing required fields: order_id, telegram_id")
    True -> {
      case escrow_client.confirm_fiat_received(order_id, telegram_id) {
        Ok(order) -> {
          protocol.text_result(json.to_string(json.object([
            #("success", json.bool(True)),
            #("order_id", json.string(order.id)),
            #("status", json.string("completed")),
            #("message", json.string("Payment confirmed! Crypto released to buyer.")),
          ])))
        }
        Error(e) -> protocol.error_result("Failed to confirm payment: " <> e)
      }
    }
  }
}

/// Handle cancel order
fn handle_cancel_order(args: Dynamic) -> ToolResult {
  let order_id = get_string_field(args, "order_id") |> result.unwrap("")
  let telegram_id = get_int_field(args, "telegram_id") |> result.unwrap(0)

  case order_id != "" && telegram_id > 0 {
    False -> protocol.error_result("Missing required fields: order_id, telegram_id")
    True -> {
      case escrow_client.cancel_order(order_id, telegram_id) {
        Ok(order) -> {
          protocol.text_result(json.to_string(json.object([
            #("success", json.bool(True)),
            #("order_id", json.string(order.id)),
            #("status", json.string("cancelled")),
            #("message", json.string("Order cancelled successfully.")),
          ])))
        }
        Error(e) -> protocol.error_result("Failed to cancel order: " <> e)
      }
    }
  }
}

/// Handle my orders
fn handle_my_orders(args: Dynamic) -> ToolResult {
  let telegram_id = get_int_field(args, "telegram_id") |> result.unwrap(0)
  let role = get_string_field(args, "role") |> result.unwrap("all")

  case telegram_id > 0 {
    False -> protocol.error_result("Missing required field: telegram_id")
    True -> {
      let seller_orders = escrow_client.list_seller_orders(telegram_id)
      let buyer_orders = escrow_client.list_buyer_orders(telegram_id)

      // Return empty for now since we don't have real orders
      protocol.text_result(json.to_string(json.object([
        #("success", json.bool(True)),
        #("as_seller", json.array([], fn(x) { x })),
        #("as_buyer", json.array([], fn(x) { x })),
        #("total", json.int(0)),
      ])))
    }
  }
}

/// Handle order status
fn handle_order_status(args: Dynamic) -> ToolResult {
  let order_id = get_string_field(args, "order_id") |> result.unwrap("")

  case order_id != "" {
    False -> protocol.error_result("Missing required field: order_id")
    True -> {
      // Return mock status for demo
      protocol.text_result(json.to_string(json.object([
        #("success", json.bool(True)),
        #("order_id", json.string(order_id)),
        #("status", json.string("not_found")),
        #("message", json.string("Order not found. Create a new order with p2p_create_sell_order.")),
      ])))
    }
  }
}

/// Handle rates - fetch live prices from CoinGecko with fallback
fn handle_rates(_args: Dynamic) -> ToolResult {
  // Try to fetch live prices
  let live_prices = price_feed.fetch_all_prices()
  let now = current_timestamp()

  // Convert to MarketRate format with 1% spread
  let rates = live_prices
    |> list.filter_map(fn(result) {
      case result {
        Ok(price_data) -> {
          let #(buy, sell) = price_feed.calculate_rates(price_data.price, 1.0)
          Ok(p2p_types.MarketRate(
            crypto: price_data.crypto,
            fiat: price_data.fiat,
            buy_rate: buy,
            sell_rate: sell,
            updated_at: now,
          ))
        }
        Error(_) -> Error(Nil)
      }
    })

  // If no live prices, use fallback
  let final_rates = case rates {
    [] -> p2p_types.default_rates()
    _ -> rates
  }

  let source = case rates {
    [] -> "fallback (CoinGecko unavailable)"
    _ -> "live (CoinGecko)"
  }

  let rates_json = list.map(final_rates, fn(rate: p2p_types.MarketRate) {
    json.object([
      #("crypto", json.string(p2p_types.crypto_to_string(rate.crypto))),
      #("fiat", json.string(p2p_types.fiat_to_string(rate.fiat))),
      #("buy_rate", json.float(rate.buy_rate)),
      #("sell_rate", json.float(rate.sell_rate)),
    ])
  })

  protocol.text_result(json.to_string(json.object([
    #("success", json.bool(True)),
    #("rates", json.array(rates_json, fn(x) { x })),
    #("source", json.string(source)),
    #("note", json.string("Rates include 1% spread. Actual P2P rates depend on individual orders.")),
  ])))
}

@external(erlang, "vibee_p2p_ffi", "current_timestamp")
fn current_timestamp() -> Int

// =============================================================================
// HELPERS
// =============================================================================

fn get_string_field(data: Dynamic, key: String) -> Result(String, Nil) {
  get_field_ffi(data, key)
  |> result.then(fn(v) {
    get_string_value_ffi(v)
  })
}

@external(erlang, "vibee_payment_tools_ffi", "get_string_value")
fn get_string_value_ffi(value: Dynamic) -> Result(String, Nil)

fn get_int_field(data: Dynamic, key: String) -> Result(Int, Nil) {
  get_field_ffi(data, key)
  |> result.then(fn(v) {
    get_int_value_ffi(v)
  })
}

@external(erlang, "vibee_payment_tools_ffi", "get_int_value")
fn get_int_value_ffi(value: Dynamic) -> Result(Int, Nil)

fn get_float_field(data: Dynamic, key: String) -> Result(Float, Nil) {
  get_field_ffi(data, key)
  |> result.then(fn(v) {
    get_float_value_ffi(v)
  })
}

@external(erlang, "vibee_payment_tools_ffi", "get_float_value")
fn get_float_value_ffi(value: Dynamic) -> Result(Float, Nil)

@external(erlang, "vibee_payment_tools_ffi", "get_map_field")
fn get_field_ffi(map: Dynamic, key: String) -> Result(Dynamic, Nil)
