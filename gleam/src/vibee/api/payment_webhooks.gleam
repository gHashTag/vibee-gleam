// Payment Webhook Handlers
// Robokassa, TON, Telegram Stars callbacks

import gleam/bit_array
import gleam/bytes_tree
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri
import mist.{type Connection, type ResponseData}
import vibee/db/postgres
import vibee/logging
import vibee/payment/robokassa
import vibee/sales/db as sales_db
import vibee/sales/subscription
import vibee/sales/types.{Robokassa as RobokassaMethod, TelegramStars, TonNative}

// =============================================================================
// Types
// =============================================================================

pub type PaymentWebhookResult {
  PaymentSuccess(inv_id: String, amount: String)
  PaymentFailed(reason: String)
  PaymentPending(inv_id: String)
}

// =============================================================================
// Robokassa Webhook Handler
// =============================================================================

/// Handle Robokassa result callback
/// POST /api/robokassa-result
/// POST /api/payment-success
pub fn handle_robokassa(req: Request(Connection)) -> Response(ResponseData) {
  logging.quick_info("[PAYMENT:ROBOKASSA] Webhook received")

  // Read and parse form body
  case read_form_body(req) {
    Error(err) -> {
      logging.quick_error("[PAYMENT:ROBOKASSA] Failed to read body: " <> err)
      error_response(400, "Bad request")
    }
    Ok(params) -> {
      // Extract required parameters
      let out_sum = get_param(params, "OutSum")
      let inv_id = get_param(params, "InvId")
      let signature = get_param(params, "SignatureValue")

      logging.quick_info("[PAYMENT:ROBOKASSA] InvId=" <> option.unwrap(inv_id, "?")
        <> ", OutSum=" <> option.unwrap(out_sum, "?"))

      case out_sum, inv_id, signature {
        Some(sum), Some(id), Some(sig) -> {
          // Validate signature
          case robokassa.validate_signature(sum, id, sig) {
            False -> {
              logging.quick_warn("[PAYMENT:ROBOKASSA] Invalid signature for InvId=" <> id)
              error_response(403, "Invalid signature")
            }
            True -> {
              logging.quick_info("[PAYMENT:ROBOKASSA] Signature valid, processing payment")
              process_robokassa_payment(id, sum)
            }
          }
        }
        _, _, _ -> {
          logging.quick_warn("[PAYMENT:ROBOKASSA] Missing required params")
          error_response(400, "Missing OutSum, InvId, or SignatureValue")
        }
      }
    }
  }
}

/// Process successful Robokassa payment
fn process_robokassa_payment(inv_id: String, amount: String) -> Response(ResponseData) {
  // inv_id format: "sub_{telegram_id}_{product_id}_{timestamp}"
  case parse_invoice_id(inv_id) {
    Error(err) -> {
      logging.quick_warn("[PAYMENT:ROBOKASSA] Invalid inv_id format: " <> err)
      // Return OK anyway to prevent Robokassa retries
      robokassa_ok_response(inv_id)
    }
    Ok(#(telegram_id, product_id)) -> {
      case postgres.get_global_pool() {
        None -> {
          logging.quick_error("[PAYMENT:ROBOKASSA] No DB pool")
          robokassa_ok_response(inv_id)
        }
        Some(pool) -> {
          // Create and activate subscription
          case sales_db.create_subscription(pool, telegram_id, product_id, RobokassaMethod, inv_id) {
            Error(err) -> {
              logging.quick_error("[PAYMENT:ROBOKASSA] Failed to create subscription: "
                <> sales_db_error_to_string(err))
              robokassa_ok_response(inv_id)
            }
            Ok(sub) -> {
              // Activate immediately
              case sub.id {
                Some(sub_id) -> {
                  case sales_db.activate_subscription(pool, sub_id) {
                    Ok(_) -> {
                      logging.quick_info("[PAYMENT:ROBOKASSA] Subscription activated: "
                        <> int.to_string(sub_id) <> " for user " <> int.to_string(telegram_id))

                      // Log usage
                      let _ = sales_db.log_usage(pool, telegram_id, Some(sub_id), "subscription_activated",
                        Some("product_id=" <> int.to_string(product_id) <> ", amount=" <> amount), 0,
                        parse_amount_cents(amount))

                      robokassa_ok_response(inv_id)
                    }
                    Error(_) -> {
                      logging.quick_error("[PAYMENT:ROBOKASSA] Failed to activate subscription")
                      robokassa_ok_response(inv_id)
                    }
                  }
                }
                None -> {
                  logging.quick_error("[PAYMENT:ROBOKASSA] Subscription has no ID")
                  robokassa_ok_response(inv_id)
                }
              }
            }
          }
        }
      }
    }
  }
}

// =============================================================================
// TON Webhook Handler
// =============================================================================

/// Handle TON payment callback (from xRocket or CryptoBot)
/// POST /api/webhooks/ton
pub fn handle_ton(req: Request(Connection)) -> Response(ResponseData) {
  logging.quick_info("[PAYMENT:TON] Webhook received")

  case read_json_body(req) {
    Error(err) -> {
      logging.quick_error("[PAYMENT:TON] Failed to read body: " <> err)
      error_response(400, "Bad request")
    }
    Ok(body) -> {
      // Parse TON webhook payload
      // Format depends on provider (xRocket, CryptoBot, etc.)
      logging.quick_info("[PAYMENT:TON] Payload: " <> string.slice(body, 0, 500))

      // Extract payment info from JSON
      case parse_ton_webhook(body) {
        Error(err) -> {
          logging.quick_warn("[PAYMENT:TON] Invalid payload: " <> err)
          success_response("ok")
        }
        Ok(#(inv_id, amount_nano, status)) -> {
          case status {
            "paid" | "completed" -> {
              process_ton_payment(inv_id, amount_nano)
            }
            _ -> {
              logging.quick_info("[PAYMENT:TON] Status: " <> status <> " for " <> inv_id)
              success_response("ok")
            }
          }
        }
      }
    }
  }
}

/// Process successful TON payment
fn process_ton_payment(inv_id: String, amount_nano: Int) -> Response(ResponseData) {
  case parse_invoice_id(inv_id) {
    Error(_) -> success_response("ok")
    Ok(#(telegram_id, product_id)) -> {
      case postgres.get_global_pool() {
        None -> success_response("ok")
        Some(pool) -> {
          // Create and activate subscription
          case sales_db.create_subscription(pool, telegram_id, product_id, TonNative, inv_id) {
            Error(_) -> success_response("ok")
            Ok(sub) -> {
              case sub.id {
                Some(sub_id) -> {
                  let _ = sales_db.activate_subscription(pool, sub_id)
                  let _ = sales_db.log_usage(pool, telegram_id, Some(sub_id), "subscription_activated",
                    Some("TON payment, amount_nano=" <> int.to_string(amount_nano)), 0, 0)
                  logging.quick_info("[PAYMENT:TON] Subscription activated for user "
                    <> int.to_string(telegram_id))
                  success_response("ok")
                }
                None -> success_response("ok")
              }
            }
          }
        }
      }
    }
  }
}

// =============================================================================
// Telegram Stars Webhook Handler
// =============================================================================

/// Handle Telegram Stars payment callback
/// POST /api/webhooks/stars
pub fn handle_stars(req: Request(Connection)) -> Response(ResponseData) {
  logging.quick_info("[PAYMENT:STARS] Webhook received")

  case read_json_body(req) {
    Error(err) -> {
      logging.quick_error("[PAYMENT:STARS] Failed to read body: " <> err)
      error_response(400, "Bad request")
    }
    Ok(body) -> {
      logging.quick_info("[PAYMENT:STARS] Payload: " <> string.slice(body, 0, 500))

      // Parse Telegram Stars webhook
      // Format: {"update_id": ..., "pre_checkout_query": {...}} or {"successful_payment": {...}}
      case parse_stars_webhook(body) {
        Error(err) -> {
          logging.quick_warn("[PAYMENT:STARS] Invalid payload: " <> err)
          success_response("ok")
        }
        Ok(#(telegram_id, product_id, amount_stars)) -> {
          process_stars_payment(telegram_id, product_id, amount_stars)
        }
      }
    }
  }
}

/// Process successful Stars payment
fn process_stars_payment(telegram_id: Int, product_id: Int, amount_stars: Int) -> Response(ResponseData) {
  case postgres.get_global_pool() {
    None -> success_response("ok")
    Some(pool) -> {
      let inv_id = "stars_" <> int.to_string(telegram_id) <> "_" <> int.to_string(product_id)
      case sales_db.create_subscription(pool, telegram_id, product_id, TelegramStars, inv_id) {
        Error(_) -> success_response("ok")
        Ok(sub) -> {
          case sub.id {
            Some(sub_id) -> {
              let _ = sales_db.activate_subscription(pool, sub_id)
              let _ = sales_db.log_usage(pool, telegram_id, Some(sub_id), "subscription_activated",
                Some("Telegram Stars, amount=" <> int.to_string(amount_stars)), 0, 0)
              logging.quick_info("[PAYMENT:STARS] Subscription activated for user "
                <> int.to_string(telegram_id))
              success_response("ok")
            }
            None -> success_response("ok")
          }
        }
      }
    }
  }
}

// =============================================================================
// Helpers
// =============================================================================

/// Parse invoice ID format: "sub_{telegram_id}_{product_id}_{timestamp}"
fn parse_invoice_id(inv_id: String) -> Result(#(Int, Int), String) {
  case string.split(inv_id, "_") {
    ["sub", telegram_id_str, product_id_str, ..] -> {
      case int.parse(telegram_id_str), int.parse(product_id_str) {
        Ok(tid), Ok(pid) -> Ok(#(tid, pid))
        _, _ -> Error("Invalid telegram_id or product_id")
      }
    }
    _ -> Error("Invalid invoice ID format")
  }
}

/// Parse amount in kopecks/cents
fn parse_amount_cents(amount: String) -> Int {
  // amount format: "299.00" (rubles) -> 29900 (kopecks)
  case string.split(amount, ".") {
    [rubles_str, kopecks_str] -> {
      let rubles = int.parse(rubles_str) |> result.unwrap(0)
      let kopecks = int.parse(kopecks_str) |> result.unwrap(0)
      rubles * 100 + kopecks
    }
    [rubles_str] -> {
      let rubles = int.parse(rubles_str) |> result.unwrap(0)
      rubles * 100
    }
    _ -> 0
  }
}

/// Parse TON webhook payload
fn parse_ton_webhook(_body: String) -> Result(#(String, Int, String), String) {
  // Simplified parsing - in production use proper JSON decoder
  // Expected format: {"invoice_id": "...", "amount": 1000000000, "status": "paid"}
  Error("TON webhook parsing not implemented")
}

/// Parse Telegram Stars webhook payload
fn parse_stars_webhook(_body: String) -> Result(#(Int, Int, Int), String) {
  // Simplified parsing - in production use proper JSON decoder
  // Expected format: {"from": {"id": 123}, "invoice_payload": "...", "total_amount": 100}
  Error("Stars webhook parsing not implemented")
}

/// Read form-urlencoded body
fn read_form_body(req: Request(Connection)) -> Result(List(#(String, String)), String) {
  case mist.read_body(req, 100_000) {
    Error(_) -> Error("Failed to read body")
    Ok(body_result) -> {
      case bit_array.to_string(body_result.body) {
        Error(_) -> Error("Body is not UTF-8")
        Ok(body_str) -> {
          // Parse form-urlencoded
          let pairs = string.split(body_str, "&")
          let params = list.filter_map(pairs, fn(pair) {
            case string.split(pair, "=") {
              [key, value] -> {
                let decoded_value = uri.percent_decode(value) |> result.unwrap(value)
                Ok(#(key, decoded_value))
              }
              _ -> Error(Nil)
            }
          })
          Ok(params)
        }
      }
    }
  }
}

/// Read JSON body as string
fn read_json_body(req: Request(Connection)) -> Result(String, String) {
  case mist.read_body(req, 100_000) {
    Error(_) -> Error("Failed to read body")
    Ok(body_result) -> {
      case bit_array.to_string(body_result.body) {
        Error(_) -> Error("Body is not UTF-8")
        Ok(body_str) -> Ok(body_str)
      }
    }
  }
}

/// Get parameter from form params
fn get_param(params: List(#(String, String)), key: String) -> Option(String) {
  case list.find(params, fn(p) { p.0 == key }) {
    Ok(#(_, value)) -> Some(value)
    Error(_) -> None
  }
}

fn sales_db_error_to_string(err: sales_db.SalesDbError) -> String {
  case err {
    sales_db.SalesDbConnectionError(msg) -> "Connection error: " <> msg
    sales_db.SalesDbQueryError(msg) -> "Query error: " <> msg
    sales_db.SalesDbNotFound -> "Not found"
  }
}

// =============================================================================
// Responses
// =============================================================================

/// Robokassa expects "OK{inv_id}" response
fn robokassa_ok_response(inv_id: String) -> Response(ResponseData) {
  response.new(200)
  |> response.set_header("content-type", "text/plain")
  |> response.set_body(mist.Bytes(bytes_tree.from_string("OK" <> inv_id)))
}

fn success_response(message: String) -> Response(ResponseData) {
  let body = json.object([
    #("status", json.string("ok")),
    #("message", json.string(message)),
  ])
  response.new(200)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(json.to_string(body))))
}

fn error_response(status: Int, message: String) -> Response(ResponseData) {
  let body = json.object([
    #("status", json.string("error")),
    #("message", json.string(message)),
  ])
  response.new(status)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(json.to_string(body))))
}
