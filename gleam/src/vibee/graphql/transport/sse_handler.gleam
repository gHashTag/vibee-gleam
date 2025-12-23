// GraphQL SSE Handler - Minimal stub
// Server-Sent Events support

import gleam/bytes_tree
import gleam/dict.{type Dict}
import gleam/http/response.{type Response}
import gleam/int
import gleam/json.{type Json}
import gleam/option.{type Option, None, Some}
import mist.{type ResponseData}
import vibee/graphql/types.{type GraphQLError}

pub type SseState {
  SseState(
    client_id: String,
    subscription_id: String,
    topic: String,
    query: String,
    variables: Dict(String, Json),
    poll_interval_ms: Int,
  )
}

pub fn format_sse_event(event_type: String, data: String) -> String {
  "event: " <> event_type <> "\n" <> "data: " <> data <> "\n\n"
}

pub fn format_data_event(subscription_id: String, payload: Json) -> String {
  let data =
    json.object([
      #("id", json.string(subscription_id)),
      #("type", json.string("next")),
      #("payload", json.object([#("data", payload)])),
    ])
    |> json.to_string
  format_sse_event("next", data)
}

pub fn format_error_event(
  subscription_id: String,
  errors: List(GraphQLError),
) -> String {
  let data =
    json.object([
      #("id", json.string(subscription_id)),
      #("type", json.string("error")),
      #("payload", json.array(errors, fn(e) {
        json.object([#("message", json.string(e.message))])
      })),
    ])
    |> json.to_string
  format_sse_event("error", data)
}

pub fn format_ping() -> String {
  ": ping\n\n"
}

pub fn sse_headers() -> List(#(String, String)) {
  [
    #("Content-Type", "text/event-stream"),
    #("Cache-Control", "no-cache"),
    #("Connection", "keep-alive"),
    #("Access-Control-Allow-Origin", "*"),
  ]
}

pub fn error_response(message: String) -> Response(ResponseData) {
  let body =
    json.object([#("error", json.string(message))])
    |> json.to_string
    |> bytes_tree.from_string

  response.new(400)
  |> response.set_header("Content-Type", "application/json")
  |> response.set_body(mist.Bytes(body))
}
