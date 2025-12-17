// AI Service Webhook Handlers
// Receives callbacks from AI services when jobs complete
// Updates ai_jobs table and logs webhooks

import gleam/bit_array
import gleam/bytes_tree
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/string
import mist.{type Connection, type ResponseData}
import pog
import vibee/db/ai_jobs
import vibee/logging
import vibee/mcp/config

// =============================================================================
// Types
// =============================================================================

/// Webhook source service
pub type WebhookService {
  ServiceReplicate
  ServiceBfl
  ServiceHedra
  ServiceHeyGen
  ServiceKling
  ServiceKieai
  ServiceElevenLabs
}

// =============================================================================
// Main Webhook Handler
// =============================================================================

/// Handle POST /api/webhooks/{service}
pub fn handle_webhook(
  req: Request(Connection),
  service: String,
) -> Response(ResponseData) {
  logging.info("[WEBHOOK] Received webhook from service: " <> service)

  // Parse service type
  let webhook_service = case string.lowercase(service) {
    "replicate" -> Some(ServiceReplicate)
    "bfl" | "flux" -> Some(ServiceBfl)
    "hedra" -> Some(ServiceHedra)
    "heygen" -> Some(ServiceHeyGen)
    "kling" -> Some(ServiceKling)
    "kieai" | "veo3" -> Some(ServiceKieai)
    "elevenlabs" -> Some(ServiceElevenLabs)
    _ -> None
  }

  case webhook_service {
    None -> {
      logging.warn("[WEBHOOK] Unknown service: " <> service)
      error_response(400, "Unknown webhook service: " <> service)
    }
    Some(svc) -> {
      // Read request body
      case read_request_body(req) {
        Ok(body) -> {
          logging.info("[WEBHOOK] " <> service_to_string(svc) <> " body length: " <> string.inspect(string.length(body)))
          // Log the webhook payload for debugging (first 500 chars)
          logging.debug("[WEBHOOK] Payload: " <> string.slice(body, 0, 500))

          // Process the webhook - extract info using Erlang FFI
          process_webhook(svc, body)
        }
        Error(err) -> {
          logging.error("[WEBHOOK] Failed to read body: " <> err)
          error_response(400, "Failed to read request body")
        }
      }
    }
  }
}

// =============================================================================
// Webhook Processing
// =============================================================================

/// Process webhook based on service - extract key info and update database
fn process_webhook(service: WebhookService, body: String) -> Response(ResponseData) {
  // Use Erlang FFI to parse JSON and extract fields
  let info = parse_webhook_ffi(body)

  logging.info("[WEBHOOK] " <> service_to_string(service) <> " - ID: " <> info.id <> ", Status: " <> info.status)

  case info.output_url {
    "" -> Nil
    url -> logging.info("[WEBHOOK] Output URL: " <> url)
  }

  case info.error {
    "" -> Nil
    err -> logging.warn("[WEBHOOK] Error: " <> err)
  }

  // Update ai_jobs table based on webhook status
  update_job_status(info)

  success_response("Webhook processed for " <> service_to_string(service))
}

/// Update job status in database based on webhook info
fn update_job_status(info: WebhookInfo) -> Nil {
  case info.id {
    "unknown" -> {
      logging.warn("[WEBHOOK] Cannot update job: no ID in webhook")
      Nil
    }
    external_id -> {
      case get_db_pool() {
        Error(err) -> {
          logging.warn("[WEBHOOK] DB not available: " <> err)
          Nil
        }
        Ok(pool) -> {
          // Map webhook status to job status and update
          let result = case normalize_status(info.status) {
            "completed" | "succeeded" | "success" -> {
              let output = json.to_string(json.object([
                #("url", json.string(info.output_url)),
                #("raw_status", json.string(info.status)),
              ]))
              ai_jobs.complete_job_by_external_id(pool, external_id, output)
            }
            "failed" | "error" | "cancelled" -> {
              let error_msg = case info.error {
                "" -> "Job failed: " <> info.status
                e -> e
              }
              ai_jobs.fail_job_by_external_id(pool, external_id, error_msg)
            }
            "processing" | "starting" | "in_progress" -> {
              ai_jobs.update_status_by_external_id(pool, external_id, ai_jobs.JobProcessing)
            }
            _ -> {
              logging.debug("[WEBHOOK] Unknown status: " <> info.status <> ", treating as processing")
              ai_jobs.update_status_by_external_id(pool, external_id, ai_jobs.JobProcessing)
            }
          }
          case result {
            Ok(_) -> logging.info("[WEBHOOK] Updated job " <> external_id <> " in database")
            Error(_) -> logging.warn("[WEBHOOK] Job " <> external_id <> " not found in database (may be external job)")
          }
          Nil
        }
      }
    }
  }
}

/// Normalize status string to common format
fn normalize_status(status: String) -> String {
  string.lowercase(status)
}

/// Webhook info extracted from JSON
pub type WebhookInfo {
  WebhookInfo(
    id: String,
    status: String,
    output_url: String,
    error: String,
  )
}

/// Parse webhook JSON using Erlang FFI
@external(erlang, "vibee_webhook_ffi", "parse_webhook")
fn parse_webhook_ffi(body: String) -> WebhookInfo

// =============================================================================
// Helper Functions
// =============================================================================

/// Read request body as string
fn read_request_body(req: Request(Connection)) -> Result(String, String) {
  case mist.read_body(req, 1_000_000) {
    Ok(body_result) -> {
      case bit_array.to_string(body_result.body) {
        Ok(str) -> Ok(str)
        Error(_) -> Error("Body is not valid UTF-8")
      }
    }
    Error(_) -> Error("Failed to read request body")
  }
}

fn service_to_string(service: WebhookService) -> String {
  case service {
    ServiceReplicate -> "replicate"
    ServiceBfl -> "bfl"
    ServiceHedra -> "hedra"
    ServiceHeyGen -> "heygen"
    ServiceKling -> "kling"
    ServiceKieai -> "kieai"
    ServiceElevenLabs -> "elevenlabs"
  }
}

/// Success response (always 200 to prevent webhook retries)
fn success_response(message: String) -> Response(ResponseData) {
  let body = json.object([
    #("status", json.string("ok")),
    #("message", json.string(message)),
  ])
  let body_bytes = bytes_tree.from_string(json.to_string(body))

  response.new(200)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(body_bytes))
}

/// Error response
fn error_response(status: Int, message: String) -> Response(ResponseData) {
  let body = json.object([
    #("status", json.string("error")),
    #("message", json.string(message)),
  ])
  let body_bytes = bytes_tree.from_string(json.to_string(body))

  response.new(status)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(body_bytes))
}

// =============================================================================
// Webhook URL Helper
// =============================================================================

/// Get webhook URL for a service
/// Used when creating jobs to pass webhook_url to AI services
pub fn get_webhook_url(service: WebhookService) -> String {
  let base_url = get_base_url()
  base_url <> "/api/webhooks/" <> service_to_string(service)
}

/// Get base URL from environment
fn get_base_url() -> String {
  let fly_app = config.get_env("FLY_APP_NAME")
  case fly_app {
    "" -> config.get_env_or("WEBHOOK_BASE_URL", "http://localhost:8080")
    app_name -> "https://" <> app_name <> ".fly.dev"
  }
}

// =============================================================================
// Database Pool Management
// =============================================================================

const pool_name_str = "vibee_webhook_db_pool"

/// Get or create the database connection pool
fn get_db_pool() -> Result(pog.Connection, String) {
  case get_cached_pool() {
    Some(conn) -> Ok(conn)
    None -> {
      clear_pool_cache()
      create_and_cache_pool()
    }
  }
}

@external(erlang, "vibee_db_pool_ffi", "get_cached")
fn get_cached_pool() -> Option(pog.Connection)

@external(erlang, "vibee_db_pool_ffi", "cache_pool")
fn cache_pool(conn: pog.Connection) -> Nil

@external(erlang, "vibee_db_pool_ffi", "clear_cache")
fn clear_pool_cache() -> Nil

fn create_and_cache_pool() -> Result(pog.Connection, String) {
  let db_url = config.get_env_or("DATABASE_URL", "")
  case db_url {
    "" -> Error("DATABASE_URL not set")
    url -> {
      case parse_database_url(url) {
        Error(e) -> Error("Invalid DATABASE_URL: " <> e)
        Ok(#(host, port, user, password, database)) -> {
          let pool_name: process.Name(pog.Message) = process.new_name(prefix: pool_name_str)
          let db_config = pog.Config(
            pool_name: pool_name,
            host: host,
            port: port,
            database: database,
            user: user,
            password: Some(password),
            ssl: pog.SslVerified,
            connection_parameters: [],
            pool_size: 5,
            queue_target: 50,
            queue_interval: 1000,
            idle_interval: 30_000,
            trace: False,
            ip_version: pog.Ipv4,
            rows_as_map: False,
          )
          case pog.start(db_config) {
            Ok(actor.Started(_, connection)) -> {
              cache_pool(connection)
              Ok(connection)
            }
            Error(actor.InitExited(reason)) ->
              Error("Failed to start database pool: " <> string.inspect(reason))
            Error(actor.InitTimeout) ->
              Error("Database connection timeout")
            Error(actor.InitFailed(reason)) ->
              Error("Database pool init failed: " <> reason)
          }
        }
      }
    }
  }
}

/// Parse DATABASE_URL into components
fn parse_database_url(url: String) -> Result(#(String, Int, String, String, String), String) {
  // Format: postgres://user:password@host:port/database
  let url = string.replace(url, "postgresql://", "")
  let url = string.replace(url, "postgres://", "")

  case string.split(url, "@") {
    [credentials, host_part] -> {
      case string.split(credentials, ":") {
        [user, password] -> {
          case string.split(host_part, "/") {
            [host_port, database] -> {
              let database = case string.split(database, "?") {
                [db, _] -> db
                _ -> database
              }
              case string.split(host_port, ":") {
                [host, port_str] -> {
                  case int.parse(port_str) {
                    Ok(port) -> Ok(#(host, port, user, password, database))
                    Error(_) -> Error("Invalid port")
                  }
                }
                [host] -> Ok(#(host, 5432, user, password, database))
                _ -> Error("Invalid host format")
              }
            }
            _ -> Error("Invalid database path")
          }
        }
        _ -> Error("Invalid credentials format")
      }
    }
    _ -> Error("Invalid URL format")
  }
}
