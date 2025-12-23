// Web Authentication Handlers for VIBEE
// Phone + OTP authentication via Telegram Bridge

import gleam/bit_array
import gleam/bytes_tree
import gleam/dynamic/decode
import gleam/http
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/httpc
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import mist.{type Connection, type ResponseData}
import vibee/logging
import vibee/mcp/config

// =============================================================================
// TYPES
// =============================================================================

/// Web session stored in ETS
pub type WebSession {
  WebSession(
    token: String,
    telegram_id: Int,
    first_name: String,
    username: Option(String),
    session_id: String,
    created_at: Int,
    wallet: Option(String),
  )
}

// =============================================================================
// HANDLERS
// =============================================================================

/// POST /api/v1/auth/web/send-code
/// Request: { "phone": "+79001234567" }
/// Response: { "session_id": "sess_xxx", "phone_code_hash": "abc123" }
pub fn send_code_handler(req: Request(Connection)) -> Response(ResponseData) {
  logging.quick_info("[WEB AUTH] Send code request received")

  case mist.read_body(req, 1024 * 64) {
    Ok(req_with_body) -> {
      let body_str = bit_array_to_string(req_with_body.body)
      logging.quick_info("[WEB AUTH] Request body: " <> body_str)

      // Parse phone from request
      case parse_phone(body_str) {
        Error(_) -> {
          logging.quick_error("[WEB AUTH] Failed to parse phone from: " <> body_str)
          json_response(400, json.object([
            #("error", json.string("Invalid request. Required: phone")),
          ]))
        }
        Ok(phone) -> {
          logging.quick_info("[WEB AUTH] Parsed phone: " <> mask_phone(phone))
          // First create a session with phone for proper DB storage
          case create_bridge_session(phone) {
            Error(e) -> {
              logging.quick_error("[WEB AUTH] Bridge session creation failed: " <> e)
              json_response(500, json.object([
                #("error", json.string("Failed to create session: " <> e)),
              ]))
            }
            Ok(session_id) -> {
              logging.quick_info("[WEB AUTH] Created session: " <> session_id)
              // Then send code
              case send_code_to_bridge(phone, session_id) {
                Error(e) -> {
                  logging.quick_error("[WEB AUTH] Send code failed: " <> e)
                  json_response(400, json.object([
                    #("error", json.string(e)),
                  ]))
                }
                Ok(phone_code_hash) -> {
                  logging.quick_info("[WEB AUTH] Code sent successfully, hash: " <> phone_code_hash)
                  json_response(200, json.object([
                    #("success", json.bool(True)),
                    #("session_id", json.string(session_id)),
                    #("phone_code_hash", json.string(phone_code_hash)),
                    #("message", json.string("Code sent to Telegram")),
                  ]))
                }
              }
            }
          }
        }
      }
    }
    Error(_) -> {
      logging.quick_error("[WEB AUTH] Failed to read request body")
      json_response(400, json.object([
        #("error", json.string("Failed to read request body")),
      ]))
    }
  }
}

/// POST /api/v1/auth/web/verify-code
/// Request: { "phone": "...", "code": "12345", "phone_code_hash": "...", "session_id": "..." }
/// Response: { "user": {...} } + Set-Cookie
pub fn verify_code_handler(req: Request(Connection)) -> Response(ResponseData) {
  logging.quick_info("[WEB AUTH] Verify code request")

  case mist.read_body(req, 1024 * 64) {
    Ok(req_with_body) -> {
      let body_str = bit_array_to_string(req_with_body.body)
      logging.quick_info("[WEB AUTH] Verify body: " <> body_str)

      // Parse all required fields
      case parse_verify_request(body_str) {
        Error(e) -> {
          logging.quick_error("[WEB AUTH] Parse error: " <> e)
          json_response(400, json.object([
            #("error", json.string("Invalid request: " <> e)),
          ]))
        }
        Ok(#(phone, code, phone_code_hash, session_id)) -> {
          logging.quick_info("[WEB AUTH] Verifying code for phone: " <> mask_phone(phone))
          // Verify code via bridge
          case verify_code_on_bridge(phone, code, phone_code_hash, session_id) {
            Error(e) -> {
              logging.quick_error("[WEB AUTH] Bridge verify error: " <> e)
              json_response(400, json.object([
                #("error", json.string(e)),
              ]))
            }
            Ok(#(telegram_id, first_name, username)) -> {
              logging.quick_info("[WEB AUTH] Verified! telegram_id=" <> int.to_string(telegram_id))
              // Create session token
              let token = generate_session_token()
              logging.quick_info("[WEB AUTH] Token generated: " <> string.slice(token, 0, 10) <> "...")

              // Store session in ETS
              let session = WebSession(
                token: token,
                telegram_id: telegram_id,
                first_name: first_name,
                username: username,
                session_id: session_id,
                created_at: current_timestamp(),
                wallet: None,
              )
              store_session(session)
              logging.quick_info("[WEB AUTH] Session stored")

              // Return with Set-Cookie header
              let body = json.object([
                #("success", json.bool(True)),
                #("user", json.object([
                  #("id", json.int(telegram_id)),
                  #("first_name", json.string(first_name)),
                  #("username", json.string(option.unwrap(username, ""))),
                ])),
              ])

              logging.quick_info("[WEB AUTH] Sending success response")
              json_response_with_cookie(200, body, token)
            }
          }
        }
      }
    }
    Error(_) -> {
      logging.quick_error("[WEB AUTH] Failed to read request body")
      json_response(400, json.object([
        #("error", json.string("Failed to read request body")),
      ]))
    }
  }
}

/// GET /api/v1/auth/web/me
/// Returns current user if authenticated, 401 otherwise
pub fn me_handler(req: Request(Connection)) -> Response(ResponseData) {
  // Get cookie from request
  case get_session_from_cookie(req) {
    None -> json_response(401, json.object([
      #("authenticated", json.bool(False)),
      #("error", json.string("Not authenticated")),
    ]))
    Some(session) -> {
      json_response(200, json.object([
        #("authenticated", json.bool(True)),
        #("user", json.object([
          #("id", json.int(session.telegram_id)),
          #("first_name", json.string(session.first_name)),
          #("username", json.string(option.unwrap(session.username, ""))),
        ])),
        #("wallet", json.string(option.unwrap(session.wallet, ""))),
      ]))
    }
  }
}

/// POST /api/v1/auth/web/logout
/// Clears session cookie
pub fn logout_handler(_req: Request(Connection)) -> Response(ResponseData) {
  logging.quick_info("[WEB AUTH] Logout request")

  // Clear cookie by setting expired date
  let body = json.object([
    #("success", json.bool(True)),
    #("message", json.string("Logged out")),
  ])

  json_response_clear_cookie(200, body)
}

// =============================================================================
// BRIDGE COMMUNICATION
// =============================================================================

/// Create a new session on the bridge
fn create_bridge_session(phone: String) -> Result(String, String) {
  let base = bridge_url()
  let #(scheme, host, port) = parse_bridge_url(base)
  let api_key = config.get_env("VIBEE_API_KEY")
  let api_id = config.get_env("TELEGRAM_API_ID")
  let api_hash = config.get_env("TELEGRAM_API_HASH")

  logging.quick_info("[WEB AUTH] Bridge URL: " <> base)
  logging.quick_info("[WEB AUTH] API_KEY present: " <> case api_key { "" -> "NO" _ -> "YES (len=" <> int.to_string(string.length(api_key)) <> ")" })
  logging.quick_info("[WEB AUTH] API_ID: " <> api_id)
  logging.quick_info("[WEB AUTH] API_HASH present: " <> case api_hash { "" -> "NO" _ -> "YES" })

  let body = json.object([
    #("app_id", json.int(result.unwrap(int.parse(api_id), 0))),
    #("app_hash", json.string(api_hash)),
    #("phone", json.string(phone)),
  ])
  |> json.to_string()

  logging.quick_info("[WEB AUTH] Creating bridge session with body: " <> body)

  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_scheme(scheme)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path("/api/v1/connect")
    |> request.set_header("content-type", "application/json")
    |> request.set_header("authorization", "Bearer " <> api_key)
    |> request.set_body(body)

  case httpc.send(req) {
    Error(e) -> {
      logging.quick_error("[WEB AUTH] HTTP error connecting to bridge: " <> string.inspect(e))
      Error("Failed to connect to bridge: " <> string.inspect(e))
    }
    Ok(resp) -> {
      logging.quick_info("[WEB AUTH] Bridge /connect response: " <> int.to_string(resp.status) <> " - " <> resp.body)
      case resp.status {
        200 | 201 -> {
          // Parse session_id from response
          case parse_session_id(resp.body) {
            Ok(sid) -> Ok(sid)
            Error(_) -> Error("Failed to parse session_id from: " <> resp.body)
          }
        }
        401 -> Error("Bridge auth failed (401) - check VIBEE_API_KEY: " <> resp.body)
        503 -> Error("Bridge not configured (503) - set secrets on telegram-bridge: " <> resp.body)
        status -> Error("Bridge error " <> int.to_string(status) <> ": " <> resp.body)
      }
    }
  }
}

/// Send auth code via bridge
fn send_code_to_bridge(phone: String, session_id: String) -> Result(String, String) {
  let base = bridge_url()
  let #(scheme, host, port) = parse_bridge_url(base)
  let api_key = config.get_env("VIBEE_API_KEY")

  let body = json.object([
    #("phone", json.string(phone)),
  ])
  |> json.to_string()

  logging.quick_info("[WEB AUTH] Sending code to bridge for session: " <> session_id)

  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_scheme(scheme)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path("/api/v1/auth/phone")
    |> request.set_header("content-type", "application/json")
    |> request.set_header("authorization", "Bearer " <> api_key)
    |> request.set_header("x-session-id", session_id)
    |> request.set_body(body)

  case httpc.send(req) {
    Error(e) -> {
      logging.quick_error("[WEB AUTH] HTTP error sending code: " <> string.inspect(e))
      Error("Failed to send code: " <> string.inspect(e))
    }
    Ok(resp) -> {
      logging.quick_info("[WEB AUTH] Bridge /auth/phone response: " <> int.to_string(resp.status) <> " - " <> resp.body)
      case resp.status {
        200 -> {
          case parse_code_hash(resp.body) {
            Ok(hash) -> Ok(hash)
            Error(_) -> Error("Failed to parse code_hash from: " <> resp.body)
          }
        }
        401 -> Error("Session invalid (401) - " <> resp.body)
        500 -> Error("Telegram error (500) - " <> resp.body)
        status -> Error("Send code failed (" <> int.to_string(status) <> "): " <> resp.body)
      }
    }
  }
}

/// Verify code via bridge
fn verify_code_on_bridge(
  phone: String,
  code: String,
  phone_code_hash: String,
  session_id: String,
) -> Result(#(Int, String, Option(String)), String) {
  let base = bridge_url()
  let #(scheme, host, port) = parse_bridge_url(base)
  let api_key = config.get_env("VIBEE_API_KEY")

  logging.quick_info("[WEB AUTH BRIDGE] Verifying with session_id=" <> session_id)
  logging.quick_info("[WEB AUTH BRIDGE] API key present: " <> case string.length(api_key) > 0 {
    True -> "yes (" <> int.to_string(string.length(api_key)) <> " chars)"
    False -> "NO!"
  })

  let body = json.object([
    #("phone", json.string(phone)),
    #("code", json.string(code)),
    #("phone_code_hash", json.string(phone_code_hash)),
  ])
  |> json.to_string()

  logging.quick_info("[WEB AUTH BRIDGE] Sending to: " <> base <> "/api/v1/auth/code")

  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_scheme(scheme)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path("/api/v1/auth/code")
    |> request.set_header("content-type", "application/json")
    |> request.set_header("authorization", "Bearer " <> api_key)
    |> request.set_header("x-session-id", session_id)
    |> request.set_body(body)

  case httpc.send(req) {
    Error(e) -> {
      logging.quick_error("[WEB AUTH BRIDGE] HTTP request failed: " <> string.inspect(e))
      Error("Failed to verify code: HTTP error")
    }
    Ok(resp) -> {
      logging.quick_info("[WEB AUTH BRIDGE] Response status: " <> int.to_string(resp.status))
      logging.quick_info("[WEB AUTH BRIDGE] Response body: " <> string.slice(resp.body, 0, 500))
      case resp.status {
        200 -> {
          case parse_user_response(resp.body) {
            Ok(user) -> {
              logging.quick_info("[WEB AUTH BRIDGE] Parsed user successfully")
              Ok(user)
            }
            Error(_) -> {
              logging.quick_error("[WEB AUTH BRIDGE] Failed to parse user from: " <> string.slice(resp.body, 0, 200))
              Error("Failed to parse user response")
            }
          }
        }
        _ -> {
          logging.quick_error("[WEB AUTH BRIDGE] Non-200 status: " <> int.to_string(resp.status))
          Error("Verification failed: " <> resp.body)
        }
      }
    }
  }
}

// =============================================================================
// SESSION MANAGEMENT (ETS)
// =============================================================================

/// Initialize ETS table for web sessions
pub fn init_sessions() -> Nil {
  init_ets()
}

@external(erlang, "vibee_web_auth_ffi", "init_ets")
fn init_ets() -> Nil

@external(erlang, "vibee_web_auth_ffi", "store_session")
fn store_session_ffi(token: String, data: String) -> Nil

@external(erlang, "vibee_web_auth_ffi", "get_session")
fn get_session_ffi(token: String) -> Result(String, Nil)

@external(erlang, "vibee_web_auth_ffi", "delete_session")
fn delete_session_ffi(token: String) -> Nil

fn store_session(session: WebSession) -> Nil {
  let data = json.object([
    #("telegram_id", json.int(session.telegram_id)),
    #("first_name", json.string(session.first_name)),
    #("username", json.string(option.unwrap(session.username, ""))),
    #("session_id", json.string(session.session_id)),
    #("created_at", json.int(session.created_at)),
    #("wallet", json.string(option.unwrap(session.wallet, ""))),
  ])
  |> json.to_string()

  store_session_ffi(session.token, data)
}

fn get_session(token: String) -> Option(WebSession) {
  case get_session_ffi(token) {
    Error(_) -> None
    Ok(data) -> {
      case parse_session_data(data, token) {
        Ok(session) -> Some(session)
        Error(_) -> None
      }
    }
  }
}

fn parse_session_data(data: String, token: String) -> Result(WebSession, Nil) {
  let decoder = {
    use telegram_id <- decode.field("telegram_id", decode.int)
    use first_name <- decode.field("first_name", decode.string)
    use username <- decode.field("username", decode.string)
    use session_id <- decode.field("session_id", decode.string)
    use created_at <- decode.field("created_at", decode.int)
    use wallet <- decode.optional_field("wallet", "", decode.string)
    decode.success(WebSession(
      token: token,
      telegram_id: telegram_id,
      first_name: first_name,
      username: case username {
        "" -> None
        _ -> Some(username)
      },
      session_id: session_id,
      created_at: created_at,
      wallet: case wallet {
        "" -> None
        w -> Some(w)
      },
    ))
  }

  case json.parse(data, decoder) {
    Ok(session) -> Ok(session)
    Error(_) -> Error(Nil)
  }
}

// =============================================================================
// COOKIE HELPERS
// =============================================================================

fn get_session_from_cookie(req: Request(Connection)) -> Option(WebSession) {
  // Find Cookie header
  let cookie_header = list.find(req.headers, fn(h) {
    string.lowercase(h.0) == "cookie"
  })

  case cookie_header {
    Error(_) -> None
    Ok(#(_, cookie_value)) -> {
      // Parse vibee_session from cookies
      case parse_cookie_value(cookie_value, "vibee_session") {
        None -> None
        Some(token) -> get_session(token)
      }
    }
  }
}

fn parse_cookie_value(cookie_str: String, name: String) -> Option(String) {
  let parts = string.split(cookie_str, ";")
  list.find_map(parts, fn(part) {
    let trimmed = string.trim(part)
    case string.split(trimmed, "=") {
      [key, value] if key == name -> Ok(value)
      _ -> Error(Nil)
    }
  })
  |> option.from_result()
}

// =============================================================================
// RESPONSE HELPERS
// =============================================================================

/// Get allowed origin for CORS - defaults to production URL
fn get_allowed_origin() -> String {
  case config.get_env("FRONTEND_URL") {
    "" -> "https://vibee-mcp.fly.dev"
    url -> url
  }
}

fn json_response(status: Int, body: json.Json) -> Response(ResponseData) {
  let body_string = json.to_string(body)
  let body_bytes = bytes_tree.from_string(body_string)
  let origin = get_allowed_origin()

  response.new(status)
  |> response.set_header("content-type", "application/json")
  |> response.set_header("access-control-allow-origin", origin)
  |> response.set_header("access-control-allow-credentials", "true")
  |> response.set_body(mist.Bytes(body_bytes))
}

fn json_response_with_cookie(status: Int, body: json.Json, token: String) -> Response(ResponseData) {
  logging.quick_info("[WEB AUTH] json_response_with_cookie called")
  let body_string = json.to_string(body)
  logging.quick_info("[WEB AUTH] Body string created, length=" <> int.to_string(string.length(body_string)))
  let body_bytes = bytes_tree.from_string(body_string)
  logging.quick_info("[WEB AUTH] Body bytes created")
  let origin = get_allowed_origin()
  logging.quick_info("[WEB AUTH] Origin: " <> origin)

  logging.quick_info("[WEB AUTH] Token length: " <> int.to_string(string.length(token)))
  // Cookie valid for 7 days, SameSite=Lax for better compatibility
  let cookie = "vibee_session=" <> token <> "; HttpOnly; Secure; SameSite=Lax; Path=/; Max-Age=604800"
  logging.quick_info("[WEB AUTH] Cookie created, length=" <> int.to_string(string.length(cookie)))

  logging.quick_info("[WEB AUTH] Building response with status " <> int.to_string(status))
  response.new(status)
  |> response.set_header("content-type", "application/json")
  |> response.set_header("access-control-allow-origin", origin)
  |> response.set_header("access-control-allow-credentials", "true")
  |> response.set_header("set-cookie", cookie)
  |> response.set_body(mist.Bytes(body_bytes))
}

fn json_response_clear_cookie(status: Int, body: json.Json) -> Response(ResponseData) {
  let body_string = json.to_string(body)
  let body_bytes = bytes_tree.from_string(body_string)
  let origin = get_allowed_origin()

  // Clear cookie by setting expired date
  let cookie = "vibee_session=; HttpOnly; Secure; SameSite=Lax; Path=/; Max-Age=0"

  response.new(status)
  |> response.set_header("content-type", "application/json")
  |> response.set_header("access-control-allow-origin", origin)
  |> response.set_header("access-control-allow-credentials", "true")
  |> response.set_header("set-cookie", cookie)
  |> response.set_body(mist.Bytes(body_bytes))
}

// =============================================================================
// PARSING HELPERS
// =============================================================================

fn parse_phone(json_str: String) -> Result(String, Nil) {
  let decoder = {
    use phone <- decode.field("phone", decode.string)
    decode.success(phone)
  }

  case json.parse(json_str, decoder) {
    Ok(phone) -> Ok(phone)
    Error(_) -> Error(Nil)
  }
}

fn parse_verify_request(json_str: String) -> Result(#(String, String, String, String), String) {
  let decoder = {
    use phone <- decode.field("phone", decode.string)
    use code <- decode.field("code", decode.string)
    use phone_code_hash <- decode.field("phone_code_hash", decode.string)
    use session_id <- decode.field("session_id", decode.string)
    decode.success(#(phone, code, phone_code_hash, session_id))
  }

  case json.parse(json_str, decoder) {
    Ok(data) -> Ok(data)
    Error(_) -> Error("Missing required fields: phone, code, phone_code_hash, session_id")
  }
}

fn parse_session_id(json_str: String) -> Result(String, Nil) {
  let decoder = {
    use session_id <- decode.field("session_id", decode.string)
    decode.success(session_id)
  }

  case json.parse(json_str, decoder) {
    Ok(sid) -> Ok(sid)
    Error(_) -> Error(Nil)
  }
}

fn parse_code_hash(json_str: String) -> Result(String, Nil) {
  let decoder = {
    use hash <- decode.field("phone_code_hash", decode.string)
    decode.success(hash)
  }

  // Try phone_code_hash first, then code_hash
  case json.parse(json_str, decoder) {
    Ok(hash) -> Ok(hash)
    Error(_) -> {
      let decoder2 = {
        use hash <- decode.field("code_hash", decode.string)
        decode.success(hash)
      }
      case json.parse(json_str, decoder2) {
        Ok(hash) -> Ok(hash)
        Error(_) -> Error(Nil)
      }
    }
  }
}

fn parse_user_response(json_str: String) -> Result(#(Int, String, Option(String)), Nil) {
  // Response format: { "user": { "id": 123, "first_name": "John", "username": "john" } }
  let decoder = {
    use user <- decode.field("user", {
      use id <- decode.field("id", decode.int)
      use first_name <- decode.field("first_name", decode.string)
      use username <- decode.optional_field("username", None, decode.optional(decode.string))
      decode.success(#(id, first_name, username))
    })
    decode.success(user)
  }

  case json.parse(json_str, decoder) {
    Ok(user) -> Ok(user)
    Error(_) -> Error(Nil)
  }
}

// =============================================================================
// UTILITY HELPERS
// =============================================================================

fn bit_array_to_string(bits: BitArray) -> String {
  case bit_array.to_string(bits) {
    Ok(s) -> s
    Error(_) -> ""
  }
}

/// Mask phone number for logging - shows only last 4 digits
/// Example: "+79001234567" -> "+7...4567"
fn mask_phone(phone: String) -> String {
  let len = string.length(phone)
  case len > 4 {
    True -> {
      let prefix = string.slice(phone, 0, 2)
      let suffix = string.slice(phone, len - 4, 4)
      prefix <> "..." <> suffix
    }
    False -> "***"
  }
}

fn bridge_url() -> String {
  // No localhost fallback - VIBEE_BRIDGE_URL must be set for production
  config.get_env("VIBEE_BRIDGE_URL")
}

fn parse_bridge_url(url: String) -> #(http.Scheme, String, Int) {
  case string.starts_with(url, "https://") {
    True -> {
      let host = string.drop_start(url, 8)
        |> string.split("/")
        |> list.first
        |> result.unwrap("localhost")
      #(http.Https, host, 443)
    }
    False -> {
      // http://host:port
      let rest = string.drop_start(url, 7)
      case string.split(rest, ":") {
        [host, port_str] -> {
          let port = int.parse(port_str) |> result.unwrap(8081)
          #(http.Http, host, port)
        }
        [host] -> #(http.Http, host, 80)
        _ -> #(http.Http, "localhost", 8081)
      }
    }
  }
}

@external(erlang, "vibee_web_auth_ffi", "generate_token")
fn generate_session_token() -> String

@external(erlang, "vibee_web_auth_ffi", "current_timestamp")
fn current_timestamp() -> Int

// =============================================================================
// WALLET HANDLERS
// =============================================================================

/// POST /api/v1/auth/web/wallet
/// Saves wallet address to session
pub fn save_wallet_handler(req: Request(Connection)) -> Response(ResponseData) {
  // First check if user is authenticated
  case get_session_from_cookie(req) {
    None -> json_response(401, json.object([
      #("error", json.string("Not authenticated")),
    ]))
    Some(session) -> {
      case mist.read_body(req, 1024 * 64) {
        Error(_) -> json_response(400, json.object([
          #("error", json.string("Failed to read request body")),
        ]))
        Ok(req_with_body) -> {
          let body_str = bit_array_to_string(req_with_body.body)
          case parse_wallet(body_str) {
            Error(_) -> json_response(400, json.object([
              #("error", json.string("Invalid request. Required: wallet")),
            ]))
            Ok(wallet) -> {
              // Update session with wallet
              let updated_session = WebSession(
                ..session,
                wallet: Some(wallet),
              )
              store_session(updated_session)
              logging.quick_info("[WEB AUTH] Wallet saved: " <> wallet)

              json_response(200, json.object([
                #("success", json.bool(True)),
                #("wallet", json.string(wallet)),
              ]))
            }
          }
        }
      }
    }
  }
}

/// DELETE /api/v1/auth/web/wallet
/// Removes wallet from session
pub fn delete_wallet_handler(req: Request(Connection)) -> Response(ResponseData) {
  case get_session_from_cookie(req) {
    None -> json_response(401, json.object([
      #("error", json.string("Not authenticated")),
    ]))
    Some(session) -> {
      // Update session to remove wallet
      let updated_session = WebSession(
        ..session,
        wallet: None,
      )
      store_session(updated_session)
      logging.quick_info("[WEB AUTH] Wallet disconnected")

      json_response(200, json.object([
        #("success", json.bool(True)),
        #("message", json.string("Wallet disconnected")),
      ]))
    }
  }
}

fn parse_wallet(json_str: String) -> Result(String, Nil) {
  let decoder = {
    use wallet <- decode.field("wallet", decode.string)
    decode.success(wallet)
  }

  case json.parse(json_str, decoder) {
    Ok(wallet) -> Ok(wallet)
    Error(_) -> Error(Nil)
  }
}

// =============================================================================
// TON CONNECT MANIFEST
// =============================================================================

/// GET /tonconnect-manifest.json
/// Serves the TON Connect manifest required for wallet connection
pub fn tonconnect_manifest_handler(_req: Request(Connection)) -> Response(ResponseData) {
  let manifest = json.object([
    #("url", json.string("https://vibee-mcp.fly.dev")),
    #("name", json.string("VIBEE P2P")),
    #("iconUrl", json.string("https://vibee-mcp.fly.dev/icon.png")),
    #("termsOfUseUrl", json.string("https://vibee-mcp.fly.dev/terms")),
    #("privacyPolicyUrl", json.string("https://vibee-mcp.fly.dev/privacy")),
  ])

  json_response(200, manifest)
}
