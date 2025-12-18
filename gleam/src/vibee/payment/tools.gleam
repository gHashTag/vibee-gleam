// Payment MCP Tools for VIBEE
// Handles payment creation, verification, and balance management

import gleam/dynamic.{type Dynamic}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/int
import gleam/float
import gleam/string
import vibee/logging
import vibee/mcp/protocol
import vibee/mcp/types.{type Tool, type ToolResult, Tool}
import vibee/payment/types as payment_types
import vibee/payment/robokassa
import vibee/payment/ton_client

// =============================================================================
// TOOL DEFINITIONS
// =============================================================================

/// Tool: payment_create
pub fn payment_create_tool() -> Tool {
  Tool(
    name: "payment_create",
    description: "Create a new payment. Returns invoice URL/deep link for the selected payment method. Supported methods: telegram, robokassa, ton_usdt, ton_native",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("method", json.object([
          #("type", json.string("string")),
          #("enum", json.array(["telegram", "robokassa", "ton_usdt", "ton_native"], json.string)),
          #("description", json.string("Payment method")),
        ])),
        #("amount", json.object([
          #("type", json.string("number")),
          #("description", json.string("Payment amount in the method's currency (RUB for robokassa, USD for ton_usdt, TON for ton_native, stars for telegram)")),
        ])),
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Telegram user ID")),
        ])),
        #("description", json.object([
          #("type", json.string("string")),
          #("description", json.string("Payment description (optional)")),
        ])),
      ])),
      #("required", json.array(["method", "amount", "telegram_id"], json.string)),
    ]),
  )
}

/// Tool: payment_status
pub fn payment_status_tool() -> Tool {
  Tool(
    name: "payment_status",
    description: "Get payment status by invoice ID. Returns current status, amount, and stars if completed.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("inv_id", json.object([
          #("type", json.string("string")),
          #("description", json.string("Invoice ID returned from payment_create")),
        ])),
      ])),
      #("required", json.array(["inv_id"], json.string)),
    ]),
  )
}

/// Tool: payment_verify
pub fn payment_verify_tool() -> Tool {
  Tool(
    name: "payment_verify",
    description: "Verify a payment. For Robokassa: validates webhook signature. For TON: checks blockchain for transaction. Marks payment as completed and updates user balance.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("method", json.object([
          #("type", json.string("string")),
          #("enum", json.array(["robokassa", "ton_usdt", "ton_native"], json.string)),
          #("description", json.string("Payment method to verify")),
        ])),
        #("inv_id", json.object([
          #("type", json.string("string")),
          #("description", json.string("Invoice ID")),
        ])),
        #("out_sum", json.object([
          #("type", json.string("string")),
          #("description", json.string("Payment sum (for Robokassa webhook)")),
        ])),
        #("signature", json.object([
          #("type", json.string("string")),
          #("description", json.string("Signature value (for Robokassa webhook)")),
        ])),
      ])),
      #("required", json.array(["method", "inv_id"], json.string)),
    ]),
  )
}

/// Tool: balance_get
pub fn balance_get_tool() -> Tool {
  Tool(
    name: "balance_get",
    description: "Get user's current star balance.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Telegram user ID")),
        ])),
      ])),
      #("required", json.array(["telegram_id"], json.string)),
    ]),
  )
}

/// Tool: balance_topup_options
pub fn balance_topup_options_tool() -> Tool {
  Tool(
    name: "balance_topup_options",
    description: "Get available top-up options for a payment method. Returns list of amounts with corresponding star values.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("method", json.object([
          #("type", json.string("string")),
          #("enum", json.array(["telegram", "robokassa", "ton_usdt", "ton_native"], json.string)),
          #("description", json.string("Payment method")),
        ])),
      ])),
      #("required", json.array(["method"], json.string)),
    ]),
  )
}

// =============================================================================
// HANDLERS
// =============================================================================

/// Handle payment_create
pub fn handle_payment_create(args: json.Json) -> ToolResult {
  let method_opt = get_string_field(args, "method")
  let amount_opt = get_float_field(args, "amount")
  let telegram_id_opt = get_int_field(args, "telegram_id")
  let description_opt = get_string_field(args, "description")

  case method_opt, amount_opt, telegram_id_opt {
    Some(method_str), Some(amount), Some(telegram_id) -> {
      let method = payment_types.method_from_string(method_str)
      let description = option.unwrap(description_opt, "VIBEE top-up")

      logging.quick_info("[PAYMENT] Creating payment: method=" <> method_str <> ", amount=" <> float.to_string(amount))

      // Initialize payment store
      init_payment_store()

      // Generate invoice ID
      let inv_id = generate_inv_id()

      // Create payment based on method
      case method {
        payment_types.Robokassa -> create_robokassa_payment(inv_id, telegram_id, amount, description)
        payment_types.TonUsdt -> create_ton_usdt_payment(inv_id, telegram_id, amount, description)
        payment_types.TonNative -> create_ton_native_payment(inv_id, telegram_id, amount, description)
        payment_types.Telegram -> create_telegram_payment(inv_id, telegram_id, amount, description)
      }
    }
    _, _, _ -> protocol.error_result("Missing required fields: method, amount, telegram_id")
  }
}

/// Handle payment_status
pub fn handle_payment_status(args: json.Json) -> ToolResult {
  case get_string_field(args, "inv_id") {
    None -> protocol.error_result("Missing required field: inv_id")
    Some(inv_id) -> {
      logging.quick_info("[PAYMENT] Getting status for: " <> inv_id)

      case get_payment(inv_id) {
        Ok(payment_map) -> {
          protocol.text_result(json.to_string(payment_map_to_json(payment_map)))
        }
        Error(_) -> protocol.error_result("Payment not found: " <> inv_id)
      }
    }
  }
}

/// Handle payment_verify
pub fn handle_payment_verify(args: json.Json) -> ToolResult {
  let method_opt = get_string_field(args, "method")
  let inv_id_opt = get_string_field(args, "inv_id")

  case method_opt, inv_id_opt {
    Some(method_str), Some(inv_id) -> {
      logging.quick_info("[PAYMENT] Verifying payment: " <> inv_id <> " via " <> method_str)

      case method_str {
        "robokassa" -> verify_robokassa_payment(args, inv_id)
        "ton_usdt" -> verify_ton_payment(inv_id, True)
        "ton_native" -> verify_ton_payment(inv_id, False)
        _ -> protocol.error_result("Unsupported verification method: " <> method_str)
      }
    }
    _, _ -> protocol.error_result("Missing required fields: method, inv_id")
  }
}

/// Handle balance_get
pub fn handle_balance_get(args: json.Json) -> ToolResult {
  case get_int_field(args, "telegram_id") {
    None -> protocol.error_result("Missing required field: telegram_id")
    Some(telegram_id) -> {
      logging.quick_info("[PAYMENT] Getting balance for telegram_id: " <> int.to_string(telegram_id))

      let balance = get_user_balance(telegram_id)

      protocol.text_result(json.to_string(json.object([
        #("telegram_id", json.int(telegram_id)),
        #("stars_balance", json.int(balance)),
      ])))
    }
  }
}

/// Handle balance_topup_options
pub fn handle_balance_topup_options(args: json.Json) -> ToolResult {
  case get_string_field(args, "method") {
    None -> protocol.error_result("Missing required field: method")
    Some(method_str) -> {
      let method = payment_types.method_from_string(method_str)
      let options = payment_types.get_options_for_method(method)

      let options_json = list.map(options, payment_types.option_to_json)

      protocol.text_result(json.to_string(json.object([
        #("method", json.string(method_str)),
        #("options", json.array(options_json, fn(x) { x })),
      ])))
    }
  }
}

// =============================================================================
// PAYMENT CREATION HELPERS
// =============================================================================

fn create_robokassa_payment(inv_id: String, telegram_id: Int, amount: Float, description: String) -> ToolResult {
  case robokassa.generate_payment_url(amount, inv_id, description) {
    Ok(url) -> {
      // Find stars for this amount
      let stars = find_stars_for_amount(payment_types.rub_options(), amount)

      // Store payment
      let payment_data = create_payment_map(inv_id, telegram_id, amount, stars, "RUB", "robokassa", Some(url), Some(description))
      let _ = store_payment(payment_data)

      protocol.text_result(json.to_string(json.object([
        #("inv_id", json.string(inv_id)),
        #("invoice_url", json.string(url)),
        #("status", json.string("PENDING")),
        #("amount", json.float(amount)),
        #("stars", json.int(stars)),
        #("currency", json.string("RUB")),
        #("method", json.string("robokassa")),
      ])))
    }
    Error(err) -> protocol.error_result(err)
  }
}

fn create_ton_usdt_payment(inv_id: String, telegram_id: Int, amount: Float, description: String) -> ToolResult {
  case ton_client.create_usdt_payment(amount, inv_id) {
    Ok(result) -> {
      // Find stars for this amount
      let stars = find_stars_for_amount(payment_types.usdt_options(), amount)

      // Store payment
      let payment_data = create_payment_map(inv_id, telegram_id, amount, stars, "USDT", "ton_usdt", Some(result.deep_link), Some(description))
      let _ = store_payment(payment_data)

      protocol.text_result(json.to_string(json.object([
        #("inv_id", json.string(inv_id)),
        #("deep_link", json.string(result.deep_link)),
        #("web_link", json.string(result.web_link)),
        #("wallet_address", json.string(result.wallet_address)),
        #("status", json.string("PENDING")),
        #("amount", json.float(amount)),
        #("stars", json.int(stars)),
        #("currency", json.string("USDT")),
        #("method", json.string("ton_usdt")),
      ])))
    }
    Error(err) -> protocol.error_result(err)
  }
}

fn create_ton_native_payment(inv_id: String, telegram_id: Int, amount: Float, description: String) -> ToolResult {
  case ton_client.create_ton_payment(amount, inv_id) {
    Ok(result) -> {
      // Find stars for this amount
      let stars = find_stars_for_amount(payment_types.ton_options(), amount)

      // Store payment
      let payment_data = create_payment_map(inv_id, telegram_id, amount, stars, "TON", "ton_native", Some(result.deep_link), Some(description))
      let _ = store_payment(payment_data)

      protocol.text_result(json.to_string(json.object([
        #("inv_id", json.string(inv_id)),
        #("deep_link", json.string(result.deep_link)),
        #("web_link", json.string(result.web_link)),
        #("wallet_address", json.string(result.wallet_address)),
        #("status", json.string("PENDING")),
        #("amount", json.float(amount)),
        #("stars", json.int(stars)),
        #("currency", json.string("TON")),
        #("method", json.string("ton_native")),
      ])))
    }
    Error(err) -> protocol.error_result(err)
  }
}

fn create_telegram_payment(inv_id: String, telegram_id: Int, amount: Float, description: String) -> ToolResult {
  // Telegram Stars are 1:1 with amount
  let stars = float.round(amount)

  // Store payment (no URL for Telegram - handled via bot)
  let payment_data = create_payment_map(inv_id, telegram_id, amount, stars, "XTR", "telegram", None, Some(description))
  let _ = store_payment(payment_data)

  protocol.text_result(json.to_string(json.object([
    #("inv_id", json.string(inv_id)),
    #("status", json.string("PENDING")),
    #("amount", json.float(amount)),
    #("stars", json.int(stars)),
    #("currency", json.string("XTR")),
    #("method", json.string("telegram")),
    #("message", json.string("Use Telegram bot to complete payment with stars")),
  ])))
}

// =============================================================================
// PAYMENT VERIFICATION HELPERS
// =============================================================================

fn verify_robokassa_payment(args: json.Json, inv_id: String) -> ToolResult {
  let out_sum_opt = get_string_field(args, "out_sum")
  let signature_opt = get_string_field(args, "signature")

  case out_sum_opt, signature_opt {
    Some(out_sum), Some(signature) -> {
      case robokassa.validate_signature(out_sum, inv_id, signature) {
        True -> {
          // Mark payment as completed
          case complete_payment(inv_id) {
            Ok(payment) -> {
              // Update user balance
              let telegram_id = get_map_int(payment, "telegram_id") |> result.unwrap(0)
              let stars = get_map_int(payment, "stars") |> result.unwrap(0)
              let _ = update_user_balance(telegram_id, stars)

              protocol.text_result(json.to_string(json.object([
                #("valid", json.bool(True)),
                #("inv_id", json.string(inv_id)),
                #("status", json.string("COMPLETED")),
                #("stars_added", json.int(stars)),
              ])))
            }
            Error(_) -> protocol.error_result("Payment not found: " <> inv_id)
          }
        }
        False -> {
          protocol.text_result(json.to_string(json.object([
            #("valid", json.bool(False)),
            #("inv_id", json.string(inv_id)),
            #("error", json.string("Invalid signature")),
          ])))
        }
      }
    }
    _, _ -> protocol.error_result("Missing required fields for Robokassa verification: out_sum, signature")
  }
}

fn verify_ton_payment(inv_id: String, is_usdt: Bool) -> ToolResult {
  // Get payment from store
  case get_payment(inv_id) {
    Ok(payment) -> {
      let wallet = ton_client.get_config().wallet_address
      let amount = get_map_float(payment, "amount") |> result.unwrap(0.0)

      // Try to find the transaction
      case ton_client.find_payment_by_comment(wallet, inv_id, amount, is_usdt) {
        Ok(Some(_tx)) -> {
          // Transaction found! Mark as completed
          case complete_payment(inv_id) {
            Ok(updated_payment) -> {
              let telegram_id = get_map_int(updated_payment, "telegram_id") |> result.unwrap(0)
              let stars = get_map_int(updated_payment, "stars") |> result.unwrap(0)
              let _ = update_user_balance(telegram_id, stars)

              protocol.text_result(json.to_string(json.object([
                #("verified", json.bool(True)),
                #("inv_id", json.string(inv_id)),
                #("status", json.string("COMPLETED")),
                #("stars_added", json.int(stars)),
              ])))
            }
            Error(_) -> protocol.error_result("Failed to update payment status")
          }
        }
        Ok(None) -> {
          protocol.text_result(json.to_string(json.object([
            #("verified", json.bool(False)),
            #("inv_id", json.string(inv_id)),
            #("status", json.string("PENDING")),
            #("message", json.string("Transaction not found yet. Please wait and try again.")),
          ])))
        }
        Error(err) -> protocol.error_result(err)
      }
    }
    Error(_) -> protocol.error_result("Payment not found: " <> inv_id)
  }
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

fn find_stars_for_amount(options: List(payment_types.TopUpOption), amount: Float) -> Int {
  // Find exact match or interpolate
  case list.find(options, fn(opt) { opt.amount == amount }) {
    Ok(opt) -> opt.stars
    Error(_) -> {
      // Interpolate based on first option's rate
      case options {
        [first, ..] -> float.round(amount *. int.to_float(first.stars) /. first.amount)
        [] -> 0
      }
    }
  }
}

fn create_payment_map(
  inv_id: String,
  telegram_id: Int,
  amount: Float,
  stars: Int,
  currency: String,
  method: String,
  invoice_url: Option(String),
  description: Option(String),
) -> Dynamic {
  // This creates an Erlang map for the FFI
  create_payment_map_ffi(inv_id, telegram_id, amount, stars, currency, method, invoice_url, description)
}

fn payment_map_to_json(payment: Dynamic) -> json.Json {
  let inv_id = get_map_string(payment, "inv_id") |> result.unwrap("")
  let telegram_id = get_map_int(payment, "telegram_id") |> result.unwrap(0)
  let amount = get_map_float(payment, "amount") |> result.unwrap(0.0)
  let stars = get_map_int(payment, "stars") |> result.unwrap(0)
  let currency = get_map_string(payment, "currency") |> result.unwrap("")
  let status = get_map_string(payment, "status") |> result.unwrap("PENDING")
  let method = get_map_string(payment, "method") |> result.unwrap("")

  json.object([
    #("inv_id", json.string(inv_id)),
    #("telegram_id", json.int(telegram_id)),
    #("amount", json.float(amount)),
    #("stars", json.int(stars)),
    #("currency", json.string(currency)),
    #("status", json.string(status)),
    #("method", json.string(method)),
  ])
}

// =============================================================================
// JSON FIELD EXTRACTION HELPERS
// =============================================================================

/// Extract string field from JSON - uses string manipulation on json.to_string output
/// This works with gleam_json 2.0+ where constructors aren't exposed
fn get_string_field(args: json.Json, field: String) -> Option(String) {
  let json_str = json.to_string(args)
  extract_json_string_field(json_str, field)
}

/// Extract int field from JSON
fn get_int_field(args: json.Json, field: String) -> Option(Int) {
  let json_str = json.to_string(args)
  case extract_json_value(json_str, field) {
    Some(value_str) -> {
      let trimmed = string.trim(value_str)
      case int.parse(trimmed) {
        Ok(n) -> Some(n)
        Error(_) -> None
      }
    }
    None -> None
  }
}

/// Extract float field from JSON
fn get_float_field(args: json.Json, field: String) -> Option(Float) {
  let json_str = json.to_string(args)
  case extract_json_value(json_str, field) {
    Some(value_str) -> {
      let trimmed = string.trim(value_str)
      case float.parse(trimmed) {
        Ok(f) -> Some(f)
        Error(_) -> {
          // Try parsing as int and converting
          case int.parse(trimmed) {
            Ok(n) -> Some(int.to_float(n))
            Error(_) -> None
          }
        }
      }
    }
    None -> None
  }
}

/// Extract string field value from JSON string
fn extract_json_string_field(json_str: String, field: String) -> Option(String) {
  // Look for "field":"value" pattern
  let pattern = "\"" <> field <> "\":"
  case string.split_once(json_str, pattern) {
    Ok(#(_, rest)) -> {
      // Rest starts after the colon
      let trimmed = string.trim(rest)
      case string.first(trimmed) {
        Ok("\"") -> {
          // String value - extract until closing quote
          let without_quote = string.drop_start(trimmed, 1)
          case string.split_once(without_quote, "\"") {
            Ok(#(value, _)) -> Some(value)
            Error(_) -> None
          }
        }
        _ -> None
      }
    }
    Error(_) -> None
  }
}

/// Extract raw value from JSON string (for numbers, booleans)
fn extract_json_value(json_str: String, field: String) -> Option(String) {
  let pattern = "\"" <> field <> "\":"
  case string.split_once(json_str, pattern) {
    Ok(#(_, rest)) -> {
      let trimmed = string.trim(rest)
      case string.first(trimmed) {
        Ok("\"") -> {
          // String value - extract including quotes
          let without_quote = string.drop_start(trimmed, 1)
          case string.split_once(without_quote, "\"") {
            Ok(#(value, _)) -> Some(value)
            Error(_) -> None
          }
        }
        _ -> {
          // Number or other - extract until comma, }, or end
          let chars = string.to_graphemes(trimmed)
          let value_chars = list.take_while(chars, fn(c) {
            c != "," && c != "}" && c != "]" && c != " " && c != "\n"
          })
          Some(string.join(value_chars, ""))
        }
      }
    }
    Error(_) -> None
  }
}

/// Extract string from Dynamic - simplified decoder
fn get_map_string(_map: Dynamic, _key: String) -> Result(String, Nil) {
  // These functions are placeholders for potential future use
  // Main parsing uses get_*_field functions with json.Json
  Error(Nil)
}

/// Extract int from Dynamic
fn get_map_int(_map: Dynamic, _key: String) -> Result(Int, Nil) {
  Error(Nil)
}

/// Extract float from Dynamic
fn get_map_float(_map: Dynamic, _key: String) -> Result(Float, Nil) {
  Error(Nil)
}

// =============================================================================
// FFI BINDINGS
// =============================================================================

@external(erlang, "vibee_payment_ffi", "init")
fn init_payment_store() -> Nil

@external(erlang, "vibee_payment_ffi", "generate_inv_id")
fn generate_inv_id() -> String

@external(erlang, "vibee_payment_ffi", "create")
fn store_payment(payment: Dynamic) -> Dynamic

@external(erlang, "vibee_payment_ffi", "get")
fn get_payment(inv_id: String) -> Result(Dynamic, Dynamic)

@external(erlang, "vibee_payment_ffi", "update_status_completed")
fn complete_payment(inv_id: String) -> Result(Dynamic, Dynamic)

@external(erlang, "vibee_payment_ffi", "get_balance")
fn get_user_balance(telegram_id: Int) -> Int

@external(erlang, "vibee_payment_ffi", "update_balance")
fn update_user_balance(telegram_id: Int, delta_stars: Int) -> Dynamic

// Helper to create Erlang map
@external(erlang, "vibee_payment_tools_ffi", "create_payment_map")
fn create_payment_map_ffi(
  inv_id: String,
  telegram_id: Int,
  amount: Float,
  stars: Int,
  currency: String,
  method: String,
  invoice_url: Option(String),
  description: Option(String),
) -> Dynamic
