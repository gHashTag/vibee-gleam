// Render Quota API Handlers
// Freemium model: 3 free renders, then subscription required

import gleam/bit_array
import gleam/bytes_tree
import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/string
import mist.{type Connection, type ResponseData}
import pog
import vibee/db/postgres
import vibee/logging

/// Free renders limit
const free_renders_limit = 3

/// Response type for render quota check
pub type RenderQuota {
  RenderQuota(
    telegram_id: Int,
    total_renders: Int,
    free_remaining: Int,
    subscription: Option(SubscriptionInfo),
  )
}

/// Subscription info
pub type SubscriptionInfo {
  SubscriptionInfo(
    plan: String,
    generations_limit: Option(Int),
    generations_used: Int,
    remaining: Option(Int),
  )
}

// ============================================================
// GET /api/render-quota?telegram_id=123
// Check render quota for a user
// ============================================================

pub fn check_quota_handler(req: Request(Connection)) -> Response(ResponseData) {
  // Parse telegram_id from query string
  let query = request.get_query(req)
  let telegram_id_result = case query {
    Ok(params) -> {
      case find_param(params, "telegram_id") {
        Some(id_str) -> int.parse(id_str)
        None -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }

  case telegram_id_result {
    Error(_) -> {
      json_error_response(400, "Missing or invalid telegram_id parameter")
    }
    Ok(telegram_id) -> {
      case postgres.get_global_pool() {
        None -> json_error_response(500, "Database not connected")
        Some(pool) -> {
          // Get render count from database
          let total_renders = get_render_count(pool, telegram_id)
          let free_remaining = int.max(0, free_renders_limit - total_renders)

          // Check subscription
          let subscription = get_subscription(pool, telegram_id)

          json_success_response(json.object([
            #("telegram_id", json.int(telegram_id)),
            #("total_renders", json.int(total_renders)),
            #("free_remaining", json.int(free_remaining)),
            #("subscription", case subscription {
              Some(sub) -> json.object([
                #("plan", json.string(sub.plan)),
                #("generations_limit", case sub.generations_limit {
                  Some(l) -> json.int(l)
                  None -> json.null()
                }),
                #("generations_used", json.int(sub.generations_used)),
                #("remaining", case sub.remaining {
                  Some(r) -> json.int(r)
                  None -> json.null()
                }),
              ])
              None -> json.null()
            }),
          ]))
        }
      }
    }
  }
}

// ============================================================
// POST /api/render-log
// Log a render for a user
// ============================================================

pub fn log_render_handler(req: Request(Connection)) -> Response(ResponseData) {
  // Read body
  case mist.read_body(req, 1024 * 1024) {
    Ok(body_req) -> {
      let body_str = case bit_array.to_string(body_req.body) {
        Ok(s) -> s
        Error(_) -> ""
      }

      // Parse JSON body - decode telegram_id
      let telegram_id_decoder = {
        use telegram_id <- decode.field("telegram_id", decode.int)
        decode.success(telegram_id)
      }

      case json.parse(body_str, telegram_id_decoder) {
        Ok(telegram_id) -> {
          case postgres.get_global_pool() {
            None -> json_error_response(500, "Database not connected")
            Some(pool) -> {
              case log_render(pool, telegram_id) {
                Ok(total_renders) -> {
                  json_success_response(json.object([
                    #("success", json.bool(True)),
                    #("telegram_id", json.int(telegram_id)),
                    #("renders_used", json.int(total_renders)),
                  ]))
                }
                Error(err) -> {
                  logging.quick_error("Failed to log render: " <> err)
                  json_error_response(500, "Failed to log render: " <> err)
                }
              }
            }
          }
        }
        Error(_) -> {
          json_error_response(400, "Invalid JSON body. Expected: {\"telegram_id\": 123}")
        }
      }
    }
    Error(_) -> {
      json_error_response(400, "Failed to read request body")
    }
  }
}

// ============================================================
// Database functions
// ============================================================

fn get_render_count(pool: pog.Connection, telegram_id: Int) -> Int {
  let sql = "SELECT COUNT(*)::int as count FROM render_logs WHERE telegram_id = $1"

  let count_decoder = {
    use count <- decode.field("count", decode.int)
    decode.success(count)
  }

  case
    pog.query(sql)
    |> pog.parameter(pog.int(telegram_id))
    |> pog.returning(count_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [count])) -> count
    _ -> 0  // Table may not exist yet or no rows
  }
}

fn get_subscription(pool: pog.Connection, telegram_id: Int) -> Option(SubscriptionInfo) {
  let sql = "
    SELECT p.code, p.generation_limit, s.generations_used
    FROM subscriptions s
    JOIN products p ON s.product_id = p.id
    WHERE s.telegram_id = $1 AND s.status = 'active'
    ORDER BY s.created_at DESC
    LIMIT 1
  "

  let sub_decoder = {
    use plan <- decode.field("code", decode.string)
    use limit <- decode.field("generation_limit", decode.optional(decode.int))
    use used <- decode.field("generations_used", decode.int)
    decode.success(SubscriptionInfo(
      plan: plan,
      generations_limit: limit,
      generations_used: used,
      remaining: case limit {
        Some(l) -> Some(l - used)
        None -> None
      },
    ))
  }

  case
    pog.query(sql)
    |> pog.parameter(pog.int(telegram_id))
    |> pog.returning(sub_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [sub])) -> Some(sub)
    _ -> None
  }
}

fn log_render(pool: pog.Connection, telegram_id: Int) -> Result(Int, String) {
  let sql = "
    INSERT INTO render_logs (telegram_id, composition_id)
    VALUES ($1, 'SplitTalkingHead')
  "

  case
    pog.query(sql)
    |> pog.parameter(pog.int(telegram_id))
    |> pog.execute(pool)
  {
    Ok(_) -> {
      // Get updated count
      Ok(get_render_count(pool, telegram_id))
    }
    Error(e) -> Error("Insert failed: " <> pog_error_to_string(e))
  }
}

fn pog_error_to_string(err: pog.QueryError) -> String {
  case err {
    pog.ConstraintViolated(msg, constraint, _) ->
      "Constraint violated: " <> constraint <> " - " <> msg
    pog.PostgresqlError(code, name, msg) ->
      "PostgreSQL error " <> code <> " (" <> name <> "): " <> msg
    pog.UnexpectedArgumentCount(expected, got) ->
      "Unexpected argument count: expected "
      <> int.to_string(expected)
      <> ", got "
      <> int.to_string(got)
    pog.UnexpectedArgumentType(expected, got) ->
      "Unexpected argument type: expected " <> expected <> ", got " <> got
    pog.UnexpectedResultType(errors) ->
      "Unexpected result type: " <> string.inspect(errors)
    pog.ConnectionUnavailable -> "Connection unavailable"
    pog.QueryTimeout -> "Query timeout"
  }
}

// ============================================================
// Helpers
// ============================================================

fn find_param(params: List(#(String, String)), key: String) -> Option(String) {
  case params {
    [] -> None
    [#(k, v), ..rest] -> {
      case k == key {
        True -> Some(v)
        False -> find_param(rest, key)
      }
    }
  }
}

fn json_success_response(data: json.Json) -> Response(ResponseData) {
  response.new(200)
  |> response.set_header("content-type", "application/json")
  |> response.set_header("access-control-allow-origin", "*")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(json.to_string(data))))
}

fn json_error_response(status: Int, message: String) -> Response(ResponseData) {
  let body = json.object([
    #("error", json.string(message)),
  ])
  response.new(status)
  |> response.set_header("content-type", "application/json")
  |> response.set_header("access-control-allow-origin", "*")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(json.to_string(body))))
}
