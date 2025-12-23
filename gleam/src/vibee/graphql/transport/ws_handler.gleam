// GraphQL WebSocket Handler - Minimal stub
// graphql-ws protocol support

import gleam/dict.{type Dict}
import gleam/int
import gleam/json.{type Json}
import gleam/option.{type Option, None, Some}
import vibee/graphql/types.{type GraphQLError, GraphQLError}

pub type ClientMessage {
  ClientConnectionInit(payload: Option(Json))
  ClientSubscribe(id: String, payload: SubscribePayload)
  ClientComplete(id: String)
  ClientPing(payload: Option(Json))
  ClientPong(payload: Option(Json))
}

pub type ServerMessage {
  ServerConnectionAck(payload: Option(Json))
  ServerNext(id: String, payload: Json)
  ServerError(id: String, errors: List(GraphQLError))
  ServerComplete(id: String)
  ServerPing(payload: Option(Json))
  ServerPong(payload: Option(Json))
}

pub type SubscribePayload {
  SubscribePayload(
    query: String,
    operation_name: Option(String),
    variables: Option(Dict(String, Json)),
  )
}

pub type WsState {
  WsState(
    client_id: String,
    initialized: Bool,
    subscriptions: Dict(String, SubscriptionInfo),
    auth_token: Option(String),
  )
}

pub type SubscriptionInfo {
  SubscriptionInfo(
    id: String,
    topic: String,
    query: String,
    operation_name: Option(String),
    variables: Dict(String, Json),
  )
}

pub fn encode_message(msg: ServerMessage) -> String {
  let json_value = case msg {
    ServerConnectionAck(payload) ->
      json.object([
        #("type", json.string("connection_ack")),
        #("payload", case payload { Some(p) -> p None -> json.object([]) }),
      ])
    ServerNext(id, payload) ->
      json.object([
        #("type", json.string("next")),
        #("id", json.string(id)),
        #("payload", json.object([#("data", payload)])),
      ])
    ServerError(id, errors) ->
      json.object([
        #("type", json.string("error")),
        #("id", json.string(id)),
        #("payload", json.array(errors, fn(e) {
          json.object([#("message", json.string(e.message))])
        })),
      ])
    ServerComplete(id) ->
      json.object([
        #("type", json.string("complete")),
        #("id", json.string(id)),
      ])
    ServerPing(payload) ->
      json.object([
        #("type", json.string("ping")),
        #("payload", case payload { Some(p) -> p None -> json.null() }),
      ])
    ServerPong(payload) ->
      json.object([
        #("type", json.string("pong")),
        #("payload", case payload { Some(p) -> p None -> json.null() }),
      ])
  }
  json.to_string(json_value)
}

pub fn generate_client_id() -> String {
  let timestamp = erlang_system_time_ms()
  let random = erlang_unique_integer()
  "ws_" <> int.to_string(timestamp) <> "_" <> int.to_string(random)
}

@external(erlang, "erlang", "system_time")
fn erlang_system_time_ms() -> Int

@external(erlang, "erlang", "unique_integer")
fn erlang_unique_integer() -> Int

pub fn init_state() -> WsState {
  WsState(
    client_id: generate_client_id(),
    initialized: False,
    subscriptions: dict.new(),
    auth_token: None,
  )
}

pub fn handle_message(
  state: WsState,
  msg: ClientMessage,
) -> #(WsState, List(ServerMessage)) {
  case msg {
    ClientConnectionInit(_) -> #(WsState(..state, initialized: True), [ServerConnectionAck(None)])
    ClientSubscribe(_id, _payload) -> #(state, [])
    ClientComplete(_id) -> #(state, [])
    ClientPing(payload) -> #(state, [ServerPong(payload)])
    ClientPong(_) -> #(state, [])
  }
}

pub fn handle_close(_state: WsState) -> Nil {
  Nil
}
