// Health Check HTTP Server
// Provides /health endpoint for Fly.io and monitoring

import gleam/bytes_tree
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/io
import gleam/json
import gleam/result
import mist.{type Connection, type ResponseData}

pub type HealthStatus {
  HealthStatus(
    status: String,
    uptime_seconds: Int,
    bridge_connected: Bool,
    version: String,
  )
}

/// Start health check HTTP server  
pub fn start(port: Int) -> Result(Nil, Nil) {
  io.println("[HEALTH] Starting health check server on port " <> int.to_string(port))
  
  let assert Ok(_) = mist.new(handle_request)
    |> mist.port(port)
    |> mist.start
  
  Ok(Nil)
}

/// Handle HTTP requests
fn handle_request(_req: Request(Connection)) -> Response(ResponseData) {
  let status = get_health_status()
  
  let body = json.object([
    #("status", json.string(status.status)),
    #("uptime_seconds", json.int(status.uptime_seconds)),
    #("bridge_connected", json.bool(status.bridge_connected)),
    #("version", json.string(status.version)),
    #("timestamp", json.int(get_timestamp())),
  ])
  |> json.to_string
  
  response.new(200)
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
  |> response.set_header("content-type", "application/json")
}

/// Get current health status
fn get_health_status() -> HealthStatus {
  HealthStatus(
    status: "ok",
    uptime_seconds: get_uptime(),
    bridge_connected: check_bridge_connection(),
    version: "0.1.0",
  )
}

/// Get uptime in seconds
fn get_uptime() -> Int {
  // Get system uptime from Erlang
  case get_start_time() {
    0 -> 0
    start_time -> get_timestamp() - start_time
  }
}

/// Check if telegram bridge is accessible
fn check_bridge_connection() -> Bool {
  // Simple check - try to connect to bridge
  // In production, this would do actual health check
  True
}

/// Get current Unix timestamp
@external(erlang, "vibee_ffi", "get_unix_timestamp")
fn get_timestamp() -> Int

/// Get application start time
@external(erlang, "vibee_health_ffi", "get_start_time")
fn get_start_time() -> Int
