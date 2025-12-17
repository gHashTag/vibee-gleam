// Invoice MCP Tools for VIBEE
// Create and manage payment invoices for Telegram users via xRocket and CryptoBot
//
// Use cases:
// - P2P Trading: seller creates invoice, buyer pays via link
// - Services: charge users for premium features
// - Group payments: share invoice in chat
// - Airdrops: create multi-cheques for distribution

import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/logging
import vibee/mcp/protocol
import vibee/mcp/types.{type Tool, type ToolResult, Tool}
import vibee/p2p/types as p2p_types
import vibee/payment/invoice_service

// =============================================================================
// TOOL DEFINITIONS
// =============================================================================

/// Tool: invoice_create - Create payment invoice for Telegram user
pub fn invoice_create_tool() -> Tool {
  Tool(
    name: "invoice_create",
    description: "Create a payment invoice that can be sent to users in Telegram chats. Returns a pay URL that users can click to pay. Supports xRocket (@tonRocketBot) and CryptoBot (@CryptoBot). Perfect for P2P trading - send invoice in DM or group.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("amount", json.object([
          #("type", json.string("number")),
          #("description", json.string("Payment amount in the specified currency")),
        ])),
        #("currency", json.object([
          #("type", json.string("string")),
          #("enum", json.array(["TON", "USDT", "USDC", "NOT"], json.string)),
          #("description", json.string("Cryptocurrency for payment")),
        ])),
        #("description", json.object([
          #("type", json.string("string")),
          #("description", json.string("Description/reason for payment (shown to payer)")),
        ])),
        #("provider", json.object([
          #("type", json.string("string")),
          #("enum", json.array(["xrocket", "cryptobot"], json.string)),
          #("description", json.string("Payment provider: xrocket (@tonRocketBot) or cryptobot (@CryptoBot). Default: xrocket")),
        ])),
        #("expires_in", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Invoice expiration in seconds (optional, CryptoBot only)")),
        ])),
        #("payload", json.object([
          #("type", json.string("string")),
          #("description", json.string("Custom payload data for webhook/callback (optional)")),
        ])),
      ])),
      #("required", json.array(["amount", "currency", "description"], json.string)),
    ]),
  )
}

/// Tool: invoice_send - Send payment directly to Telegram user (no invoice)
pub fn invoice_send_tool() -> Tool {
  Tool(
    name: "invoice_send",
    description: "Send crypto directly to a Telegram user without creating an invoice. User receives funds instantly. Requires sufficient balance in your xRocket/CryptoBot account.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Telegram user ID to send funds to")),
        ])),
        #("amount", json.object([
          #("type", json.string("number")),
          #("description", json.string("Amount to send")),
        ])),
        #("currency", json.object([
          #("type", json.string("string")),
          #("enum", json.array(["TON", "USDT", "USDC", "NOT"], json.string)),
          #("description", json.string("Cryptocurrency to send")),
        ])),
        #("comment", json.object([
          #("type", json.string("string")),
          #("description", json.string("Optional comment/message for recipient")),
        ])),
        #("provider", json.object([
          #("type", json.string("string")),
          #("enum", json.array(["xrocket", "cryptobot"], json.string)),
          #("description", json.string("Payment provider (default: xrocket)")),
        ])),
      ])),
      #("required", json.array(["telegram_id", "amount", "currency"], json.string)),
    ]),
  )
}

/// Tool: invoice_cheque - Create multi-cheque for airdrops/giveaways
pub fn invoice_cheque_tool() -> Tool {
  Tool(
    name: "invoice_cheque",
    description: "Create a multi-cheque that can be claimed by multiple users. Perfect for airdrops, giveaways, and rewards in groups. Share the link in chat - first N users to click get the crypto.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("amount_per_user", json.object([
          #("type", json.string("number")),
          #("description", json.string("Amount each user receives when claiming")),
        ])),
        #("currency", json.object([
          #("type", json.string("string")),
          #("enum", json.array(["TON", "USDT", "USDC", "NOT"], json.string)),
          #("description", json.string("Cryptocurrency")),
        ])),
        #("total_users", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Maximum number of users who can claim")),
        ])),
        #("description", json.object([
          #("type", json.string("string")),
          #("description", json.string("Description shown to users (optional)")),
        ])),
      ])),
      #("required", json.array(["amount_per_user", "currency", "total_users"], json.string)),
    ]),
  )
}

/// Tool: invoice_format - Format invoice as Telegram message
pub fn invoice_format_tool() -> Tool {
  Tool(
    name: "invoice_format",
    description: "Format an invoice or cheque as a ready-to-send Telegram message with Markdown formatting. Useful for bots sending invoices in chats.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("invoice_id", json.object([
          #("type", json.string("string")),
          #("description", json.string("Invoice ID from invoice_create")),
        ])),
        #("pay_url", json.object([
          #("type", json.string("string")),
          #("description", json.string("Payment URL to include")),
        ])),
        #("amount", json.object([
          #("type", json.string("number")),
          #("description", json.string("Payment amount")),
        ])),
        #("currency", json.object([
          #("type", json.string("string")),
          #("description", json.string("Currency code")),
        ])),
        #("description", json.object([
          #("type", json.string("string")),
          #("description", json.string("Invoice description")),
        ])),
        #("language", json.object([
          #("type", json.string("string")),
          #("enum", json.array(["en", "ru"], json.string)),
          #("description", json.string("Message language (default: en)")),
        ])),
      ])),
      #("required", json.array(["pay_url", "amount", "currency"], json.string)),
    ]),
  )
}

// =============================================================================
// TOOL LIST
// =============================================================================

/// Get all invoice tools
pub fn get_tools() -> List(Tool) {
  [
    invoice_create_tool(),
    invoice_send_tool(),
    invoice_cheque_tool(),
    invoice_format_tool(),
  ]
}

// =============================================================================
// HANDLERS
// =============================================================================

/// Handle invoice_create
pub fn handle_invoice_create(args: json.Json) -> ToolResult {
  let amount_opt = get_float_field(args, "amount")
  let currency_opt = get_string_field(args, "currency")
  let description_opt = get_string_field(args, "description")
  let provider_opt = get_string_field(args, "provider")
  let expires_opt = get_int_field(args, "expires_in")
  let payload_opt = get_string_field(args, "payload")

  case amount_opt, currency_opt, description_opt {
    Some(amount), Some(currency_str), Some(description) -> {
      logging.info("[INVOICE] Creating invoice: " <> float.to_string(amount) <> " " <> currency_str)

      let currency = parse_currency(currency_str)
      let provider = case provider_opt {
        Some("cryptobot") -> Some(invoice_service.CryptoBot)
        Some("xrocket") -> Some(invoice_service.XRocket)
        _ -> None  // Will use default (xRocket)
      }

      // Create invoice based on provider preference
      let result = case provider {
        Some(invoice_service.CryptoBot) -> {
          invoice_service.create_cryptobot_invoice(
            amount,
            currency,
            description,
            expires_opt,
            payload_opt,
          )
        }
        _ -> {
          invoice_service.create_xrocket_invoice(amount, currency, description, None)
        }
      }

      case result {
        Ok(invoice) -> {
          protocol.text_result(json.to_string(json.object([
            #("success", json.bool(True)),
            #("invoice", invoice_service.invoice_to_json(invoice)),
            #("message_template", json.string(
              invoice_service.format_invoice_message(invoice, "en")
            )),
          ])))
        }
        Error(err) -> {
          protocol.error_result(invoice_service.error_to_string(err))
        }
      }
    }
    _, _, _ -> protocol.error_result("Missing required fields: amount, currency, description")
  }
}

/// Handle invoice_send (direct transfer)
pub fn handle_invoice_send(args: json.Json) -> ToolResult {
  let telegram_id_opt = get_int_field(args, "telegram_id")
  let amount_opt = get_float_field(args, "amount")
  let currency_opt = get_string_field(args, "currency")
  let comment_opt = get_string_field(args, "comment")
  let provider_opt = get_string_field(args, "provider")

  case telegram_id_opt, amount_opt, currency_opt {
    Some(telegram_id), Some(amount), Some(currency_str) -> {
      logging.info("[INVOICE] Sending " <> float.to_string(amount) <> " " <> currency_str <> " to user " <> int.to_string(telegram_id))

      let currency = parse_currency(currency_str)
      let provider = case provider_opt {
        Some("cryptobot") -> Some(invoice_service.CryptoBot)
        Some("xrocket") -> Some(invoice_service.XRocket)
        _ -> None
      }

      case invoice_service.send_to_user(telegram_id, amount, currency, comment_opt, provider) {
        Ok(transfer_id) -> {
          protocol.text_result(json.to_string(json.object([
            #("success", json.bool(True)),
            #("transfer_id", json.string(transfer_id)),
            #("telegram_id", json.int(telegram_id)),
            #("amount", json.float(amount)),
            #("currency", json.string(currency_str)),
            #("message", json.string("Funds sent successfully")),
          ])))
        }
        Error(err) -> {
          protocol.error_result(invoice_service.error_to_string(err))
        }
      }
    }
    _, _, _ -> protocol.error_result("Missing required fields: telegram_id, amount, currency")
  }
}

/// Handle invoice_cheque (multi-cheque for airdrops)
pub fn handle_invoice_cheque(args: json.Json) -> ToolResult {
  let amount_opt = get_float_field(args, "amount_per_user")
  let currency_opt = get_string_field(args, "currency")
  let users_opt = get_int_field(args, "total_users")
  let description_opt = get_string_field(args, "description")

  case amount_opt, currency_opt, users_opt {
    Some(amount), Some(currency_str), Some(total_users) -> {
      logging.info("[INVOICE] Creating multi-cheque: " <> float.to_string(amount) <> " " <> currency_str <> " x " <> int.to_string(total_users))

      let currency = parse_currency(currency_str)

      case invoice_service.create_multi_cheque(amount, currency, total_users, description_opt) {
        Ok(cheque) -> {
          protocol.text_result(json.to_string(json.object([
            #("success", json.bool(True)),
            #("cheque", invoice_service.cheque_to_json(cheque)),
            #("total_amount", json.float(amount *. int.to_float(total_users))),
            #("message_template", json.string(
              invoice_service.format_cheque_message(cheque, "en")
            )),
          ])))
        }
        Error(err) -> {
          protocol.error_result(invoice_service.error_to_string(err))
        }
      }
    }
    _, _, _ -> protocol.error_result("Missing required fields: amount_per_user, currency, total_users")
  }
}

/// Handle invoice_format
pub fn handle_invoice_format(args: json.Json) -> ToolResult {
  let pay_url_opt = get_string_field(args, "pay_url")
  let amount_opt = get_float_field(args, "amount")
  let currency_opt = get_string_field(args, "currency")
  let description_opt = get_string_field(args, "description")
  let language_opt = get_string_field(args, "language")
  let invoice_id_opt = get_string_field(args, "invoice_id")

  case pay_url_opt, amount_opt, currency_opt {
    Some(pay_url), Some(amount), Some(currency) -> {
      let language = option.unwrap(language_opt, "en")
      let description = option.unwrap(description_opt, "Payment")

      // Create a temporary invoice for formatting
      let invoice = invoice_service.UnifiedInvoice(
        id: option.unwrap(invoice_id_opt, ""),
        provider: invoice_service.XRocket,
        amount: amount,
        currency: currency,
        description: description,
        status: invoice_service.InvoicePending,
        pay_url: pay_url,
        telegram_link: pay_url,
        created_at: "",
        paid_at: None,
        payload: None,
        payer_telegram_id: None,
      )

      let message = invoice_service.format_invoice_message(invoice, language)

      protocol.text_result(json.to_string(json.object([
        #("message", json.string(message)),
        #("language", json.string(language)),
      ])))
    }
    _, _, _ -> protocol.error_result("Missing required fields: pay_url, amount, currency")
  }
}

/// Route tool call to handler
pub fn handle_tool(name: String, args: json.Json) -> Option(ToolResult) {
  case name {
    "invoice_create" -> Some(handle_invoice_create(args))
    "invoice_send" -> Some(handle_invoice_send(args))
    "invoice_cheque" -> Some(handle_invoice_cheque(args))
    "invoice_format" -> Some(handle_invoice_format(args))
    _ -> None
  }
}

// =============================================================================
// HELPERS
// =============================================================================

/// Parse currency string to type
fn parse_currency(s: String) -> p2p_types.CryptoCurrency {
  case string.uppercase(s) {
    "TON" | "TONCOIN" -> p2p_types.TON
    "USDT" -> p2p_types.USDT
    "USDC" -> p2p_types.USDC
    "NOT" -> p2p_types.NOT
    _ -> p2p_types.USDT
  }
}

/// Extract string field from JSON
fn get_string_field(args: json.Json, field: String) -> Option(String) {
  let json_str = json.to_string(args)
  extract_json_string_field(json_str, field)
}

/// Extract int field from JSON
fn get_int_field(args: json.Json, field: String) -> Option(Int) {
  let json_str = json.to_string(args)
  case extract_json_value(json_str, field) {
    Some(value_str) -> {
      case int.parse(string.trim(value_str)) {
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
  let pattern = "\"" <> field <> "\":"
  case string.split_once(json_str, pattern) {
    Ok(#(_, rest)) -> {
      let trimmed = string.trim(rest)
      case string.first(trimmed) {
        Ok("\"") -> {
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

/// Extract raw value from JSON string
fn extract_json_value(json_str: String, field: String) -> Option(String) {
  let pattern = "\"" <> field <> "\":"
  case string.split_once(json_str, pattern) {
    Ok(#(_, rest)) -> {
      let trimmed = string.trim(rest)
      case string.first(trimmed) {
        Ok("\"") -> {
          let without_quote = string.drop_start(trimmed, 1)
          case string.split_once(without_quote, "\"") {
            Ok(#(value, _)) -> Some(value)
            Error(_) -> None
          }
        }
        _ -> {
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
