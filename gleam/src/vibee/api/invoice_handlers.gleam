// Invoice REST API Handlers
// HTTP endpoints for creating and managing payment invoices via xRocket/CryptoBot

import gleam/bytes_tree
import gleam/float
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import mist.{type Connection, type ResponseData}
import vibee/logging
import vibee/p2p/types.{type CryptoCurrency, TON, USDT, USDC, NOT}
import vibee/payment/invoice_service

// =============================================================================
// CREATE INVOICE
// =============================================================================

/// POST /api/v1/invoice/create - Create payment invoice
/// Body: {"amount": 10.0, "currency": "USDT", "description": "...", "provider": "xrocket"}
pub fn create_handler(req: Request(Connection)) -> Response(ResponseData) {
  logging.quick_info("[INVOICE API] Create invoice request")

  // Parse body - for now use defaults
  let amount = 10.0
  let currency = USDT
  let description = "VIBEE P2P Payment"
  let provider = Some(invoice_service.XRocket)

  case invoice_service.create_invoice(amount, currency, description, provider) {
    Ok(invoice) -> {
      json_response(200, json.object([
        #("success", json.bool(True)),
        #("invoice", invoice_service.invoice_to_json(invoice)),
        #("message_ru", json.string(invoice_service.format_invoice_message(invoice, "ru"))),
        #("message_en", json.string(invoice_service.format_invoice_message(invoice, "en"))),
      ]))
    }
    Error(err) -> {
      json_response(400, json.object([
        #("success", json.bool(False)),
        #("error", json.string(invoice_service.error_to_string(err))),
      ]))
    }
  }
}

/// GET /api/v1/invoice/create?amount=X&currency=Y&description=Z
pub fn create_get_handler(
  amount_str: Option(String),
  currency_str: Option(String),
  description_str: Option(String),
  provider_str: Option(String),
) -> Response(ResponseData) {
  logging.quick_info("[INVOICE API] Create invoice GET request")

  let amount = case amount_str {
    Some(s) -> case float.parse(s) {
      Ok(f) -> f
      Error(_) -> case int.parse(s) {
        Ok(i) -> int.to_float(i)
        Error(_) -> 10.0
      }
    }
    None -> 10.0
  }

  let currency = parse_currency(currency_str)
  let description = option.unwrap(description_str, "VIBEE Payment")
  let provider = case provider_str {
    Some("cryptobot") -> Some(invoice_service.CryptoBot)
    Some("xrocket") -> Some(invoice_service.XRocket)
    _ -> None
  }

  case invoice_service.create_invoice(amount, currency, description, provider) {
    Ok(invoice) -> {
      json_response(200, json.object([
        #("success", json.bool(True)),
        #("invoice", invoice_service.invoice_to_json(invoice)),
        #("pay_url", json.string(invoice.pay_url)),
        #("telegram_link", json.string(invoice.telegram_link)),
        #("message_template", json.object([
          #("ru", json.string(invoice_service.format_invoice_message(invoice, "ru"))),
          #("en", json.string(invoice_service.format_invoice_message(invoice, "en"))),
        ])),
      ]))
    }
    Error(err) -> {
      json_response(400, json.object([
        #("success", json.bool(False)),
        #("error", json.string(invoice_service.error_to_string(err))),
      ]))
    }
  }
}

// =============================================================================
// CREATE MULTI-CHEQUE (AIRDROP)
// =============================================================================

/// GET /api/v1/invoice/cheque?amount=X&currency=Y&users=N
pub fn cheque_handler(
  amount_str: Option(String),
  currency_str: Option(String),
  users_str: Option(String),
  description_str: Option(String),
) -> Response(ResponseData) {
  logging.quick_info("[INVOICE API] Create multi-cheque request")

  let amount = case amount_str {
    Some(s) -> case float.parse(s) {
      Ok(f) -> f
      Error(_) -> 1.0
    }
    None -> 1.0
  }

  let currency = parse_currency(currency_str)
  let users = case users_str {
    Some(s) -> case int.parse(s) {
      Ok(n) -> n
      Error(_) -> 10
    }
    None -> 10
  }

  case invoice_service.create_multi_cheque(amount, currency, users, description_str) {
    Ok(cheque) -> {
      json_response(200, json.object([
        #("success", json.bool(True)),
        #("cheque", invoice_service.cheque_to_json(cheque)),
        #("claim_link", json.string(cheque.link)),
        #("total_amount", json.float(amount *. int.to_float(users))),
        #("message_template", json.object([
          #("ru", json.string(invoice_service.format_cheque_message(cheque, "ru"))),
          #("en", json.string(invoice_service.format_cheque_message(cheque, "en"))),
        ])),
      ]))
    }
    Error(err) -> {
      json_response(400, json.object([
        #("success", json.bool(False)),
        #("error", json.string(invoice_service.error_to_string(err))),
      ]))
    }
  }
}

// =============================================================================
// SEND DIRECT TRANSFER
// =============================================================================

/// GET /api/v1/invoice/send?telegram_id=X&amount=Y&currency=Z
pub fn send_handler(
  telegram_id_str: Option(String),
  amount_str: Option(String),
  currency_str: Option(String),
  comment_str: Option(String),
  provider_str: Option(String),
) -> Response(ResponseData) {
  logging.quick_info("[INVOICE API] Send direct transfer request")

  case telegram_id_str, amount_str {
    Some(tg_str), Some(amt_str) -> {
      case int.parse(tg_str) {
        Ok(telegram_id) -> {
          let amount = case float.parse(amt_str) {
            Ok(f) -> f
            Error(_) -> case int.parse(amt_str) {
              Ok(i) -> int.to_float(i)
              Error(_) -> 0.0
            }
          }

          let currency = parse_currency(currency_str)
          let provider = case provider_str {
            Some("cryptobot") -> Some(invoice_service.CryptoBot)
            _ -> Some(invoice_service.XRocket)
          }

          case invoice_service.send_to_user(telegram_id, amount, currency, comment_str, provider) {
            Ok(transfer_id) -> {
              json_response(200, json.object([
                #("success", json.bool(True)),
                #("transfer_id", json.string(transfer_id)),
                #("telegram_id", json.int(telegram_id)),
                #("amount", json.float(amount)),
                #("currency", json.string(currency_to_string(currency))),
                #("message", json.string("Funds sent successfully")),
              ]))
            }
            Error(err) -> {
              json_response(400, json.object([
                #("success", json.bool(False)),
                #("error", json.string(invoice_service.error_to_string(err))),
              ]))
            }
          }
        }
        Error(_) -> {
          json_response(400, json.object([
            #("success", json.bool(False)),
            #("error", json.string("Invalid telegram_id")),
          ]))
        }
      }
    }
    _, _ -> {
      json_response(400, json.object([
        #("success", json.bool(False)),
        #("error", json.string("Missing required parameters: telegram_id, amount")),
      ]))
    }
  }
}

// =============================================================================
// HELPERS
// =============================================================================

fn json_response(status: Int, body: json.Json) -> Response(ResponseData) {
  let body_string = json.to_string(body)
  let body_bytes = bytes_tree.from_string(body_string)

  response.new(status)
  |> response.set_header("content-type", "application/json")
  |> response.set_header("access-control-allow-origin", "*")
  |> response.set_body(mist.Bytes(body_bytes))
}

fn parse_currency(s: Option(String)) -> CryptoCurrency {
  case s {
    Some(str) -> {
      case string.uppercase(str) {
        "TON" | "TONCOIN" -> TON
        "USDT" -> USDT
        "USDC" -> USDC
        "NOT" -> NOT
        _ -> USDT
      }
    }
    None -> USDT
  }
}

fn currency_to_string(c: CryptoCurrency) -> String {
  case c {
    TON -> "TON"
    USDT -> "USDT"
    USDC -> "USDC"
    NOT -> "NOT"
  }
}
