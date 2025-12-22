// Leads API Handlers
// REST endpoints for leads management

import gleam/bit_array
import gleam/bytes_tree
import gleam/dynamic/decode
import gleam/http
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import mist.{type Connection, type ResponseData}
import vibee/db/postgres
import vibee/sales/db as sales_db
import vibee/sales/types as sales_types
import vibee/web/leads_panel

/// GET /leads - List all leads
pub fn list_leads() -> Response(ResponseData) {
  // TODO: Fetch from database
  let sample_leads = [
    leads_panel.Lead(
      id: 1,
      telegram_user_id: 144022504,
      username: Some("neuro_sage"),
      first_name: Some("Федор"),
      last_name: Some("Иванов"),
      phone: Some("+79933420465"),
      first_message: "Привет! Хочу купить крипту, подскажите где лучше?",
      first_message_date: "2025-12-18 10:30",
      source_chat_name: "Aimly.io dev",
      status: leads_panel.New,
      priority: leads_panel.High,
      intent: Some("buy"),
      crypto_interest: ["Bitcoin", "USDT"],
      trigger_words: ["купить крипту", "подскажите где"],
      agent_response: "Привет! Я могу помочь с покупкой крипты. Пишите в личку для деталей.",
      last_activity: "2025-12-18 10:35",
      message_count: 3,
    ),
  ]
  
  let html = leads_panel.render_leads_panel(sample_leads)
  
  response.new(200)
  |> response.set_body(mist.Bytes(bytes_tree.from_string(html)))
  |> response.set_header("content-type", "text/html; charset=utf-8")
}

/// GET /leads/:id - Get lead details
pub fn get_lead(lead_id: String) -> Response(ResponseData) {
  case int.parse(lead_id) {
    Ok(id) -> {
      // TODO: Fetch from database
      let lead = leads_panel.Lead(
        id: id,
        telegram_user_id: 144022504,
        username: Some("neuro_sage"),
        first_name: Some("Федор"),
        last_name: Some("Иванов"),
        phone: Some("+79933420465"),
        first_message: "Привет! Хочу купить крипту, подскажите где лучше?",
        first_message_date: "2025-12-18 10:30",
        source_chat_name: "Aimly.io dev",
        status: leads_panel.New,
        priority: leads_panel.High,
        intent: Some("buy"),
        crypto_interest: ["Bitcoin", "USDT"],
        trigger_words: ["купить крипту", "подскажите где"],
        agent_response: "Привет! Я могу помочь с покупкой крипты. Пишите в личку для деталей.",
        last_activity: "2025-12-18 10:35",
        message_count: 3,
      )
      
      let messages = [
        leads_panel.LeadMessage(
          id: 1,
          message_text: "Привет! Хочу купить крипту, подскажите где лучше?",
          message_date: "2025-12-18 10:30",
          direction: "incoming",
          sentiment: Some("positive"),
        ),
        leads_panel.LeadMessage(
          id: 2,
          message_text: "Привет! Я могу помочь с покупкой крипты. Пишите в личку для деталей.",
          message_date: "2025-12-18 10:31",
          direction: "outgoing",
          sentiment: None,
        ),
        leads_panel.LeadMessage(
          id: 3,
          message_text: "Отлично! Сколько стоит Bitcoin сейчас?",
          message_date: "2025-12-18 10:35",
          direction: "incoming",
          sentiment: Some("positive"),
        ),
      ]
      
      let notes = [
        leads_panel.LeadNote(
          id: 1,
          note: "Клиент заинтересован в покупке Bitcoin. Высокий приоритет.",
          note_type: "important",
          created_by: "agent",
          created_at: "2025-12-18 10:32",
        ),
      ]
      
      // TODO: Implement detail view
      let html = "<html><body><h1>Lead #" <> int.to_string(id) <> "</h1><p>Detail view coming soon...</p></body></html>"
      
      response.new(200)
      |> response.set_body(mist.Bytes(bytes_tree.from_string(html)))
      |> response.set_header("content-type", "text/html; charset=utf-8")
    }
    Error(_) -> {
      json_error("Invalid lead ID", 400)
    }
  }
}

/// PUT /api/v1/leads/:id/status - Update lead status
/// Body: { "status": "contacted" } or { "telegram_user_id": 12345, "status": "contacted" }
pub fn update_lead_status(req: Request(Connection), lead_id: String) -> Response(ResponseData) {
  // Read request body
  case mist.read_body(req, 10_000) {
    Error(_) -> json_error("Failed to read body", 400)
    Ok(body_result) -> {
      case bit_array.to_string(body_result.body) {
        Error(_) -> json_error("Invalid UTF-8", 400)
        Ok(body) -> update_lead_status_with_body(lead_id, body)
      }
    }
  }
}

/// Internal: Update lead status with parsed body
fn update_lead_status_with_body(lead_id: String, body: String) -> Response(ResponseData) {
  // Parse JSON body
  case json.parse(body, decode_status_update()) {
    Ok(update) -> {
      case postgres.get_global_pool() {
        Some(pool) -> {
          // Parse status string to enum
          case sales_types.lead_status_from_string(update.status) {
            Ok(status_enum) -> {
              // Update by telegram_user_id if provided, otherwise by lead_id
              let result = case update.telegram_user_id {
                Some(tg_id) -> sales_db.update_lead_status_by_telegram_id(pool, tg_id, status_enum)
                None -> {
                  case int.parse(lead_id) {
                    Ok(id) -> sales_db.update_lead_status(pool, id, status_enum)
                    Error(_) -> Error(sales_db.SalesDbQueryError("Invalid lead_id"))
                  }
                }
              }

              case result {
                Ok(lead) -> {
                  json_response(json.object([
                    #("status", json.string("ok")),
                    #("message", json.string("Status updated to " <> update.status)),
                    #("lead", encode_lead_json(lead)),
                  ]), 200)
                }
                Error(sales_db.SalesDbNotFound) -> json_error("Lead not found", 404)
                Error(err) -> json_error("Database error: " <> sales_db_error_to_string(err), 500)
              }
            }
            Error(_) -> json_error("Invalid status value: " <> update.status, 400)
          }
        }
        None -> json_error("Database not available", 503)
      }
    }
    Error(_) -> json_error("Invalid JSON body. Expected: {\"status\": \"contacted\"}", 400)
  }
}

/// Status update request decoder
fn decode_status_update() -> decode.Decoder(StatusUpdate) {
  use status <- decode.field("status", decode.string)
  use telegram_user_id <- decode.optional_field("telegram_user_id", 0, decode.int)
  // Convert 0 to None, otherwise Some
  let tg_id = case telegram_user_id {
    0 -> None
    id -> Some(id)
  }
  decode.success(StatusUpdate(status: status, telegram_user_id: tg_id))
}

/// Status update request type
type StatusUpdate {
  StatusUpdate(status: String, telegram_user_id: Option(Int))
}

/// GET /api/v1/leads/:id/history - Get lead status history
pub fn get_lead_history(lead_id: String) -> Response(ResponseData) {
  case int.parse(lead_id) {
    Ok(telegram_user_id) -> {
      case postgres.get_global_pool() {
        Some(pool) -> {
          case sales_db.get_lead_status_history(pool, telegram_user_id) {
            Ok(changes) -> {
              let history_json = json.array(changes, fn(change) {
                json.object([
                  #("id", json.int(change.id)),
                  #("old_status", json.nullable(change.old_status, json.string)),
                  #("new_status", json.string(change.new_status)),
                  #("old_funnel_stage", json.nullable(change.old_funnel_stage, json.string)),
                  #("new_funnel_stage", json.nullable(change.new_funnel_stage, json.string)),
                  #("changed_by", json.nullable(change.changed_by, json.string)),
                  #("change_reason", json.nullable(change.change_reason, json.string)),
                  #("created_at", json.nullable(change.created_at, json.string)),
                ])
              })

              json_response(json.object([
                #("status", json.string("ok")),
                #("telegram_user_id", json.int(telegram_user_id)),
                #("history", history_json),
                #("total", json.int(list.length(changes))),
              ]), 200)
            }
            Error(err) -> json_error("Database error: " <> sales_db_error_to_string(err), 500)
          }
        }
        None -> json_error("Database not available", 503)
      }
    }
    Error(_) -> json_error("Invalid telegram_user_id", 400)
  }
}

/// Helper: Encode Lead to JSON
fn encode_lead_json(lead: sales_types.Lead) -> json.Json {
  json.object([
    #("id", json.nullable(lead.id, json.int)),
    #("telegram_user_id", json.int(lead.telegram_user_id)),
    #("username", json.nullable(lead.username, json.string)),
    #("first_name", json.nullable(lead.first_name, json.string)),
    #("status", json.string(sales_types.lead_status_to_string(lead.status))),
    #("funnel_stage", json.string(sales_types.funnel_stage_to_string(lead.funnel_stage))),
    #("priority", json.string(sales_types.lead_priority_to_string(lead.priority))),
    #("last_contact_at", json.nullable(lead.last_contact_at, json.string)),
    #("created_at", json.nullable(lead.created_at, json.string)),
  ])
}

/// Helper: Convert SalesDbError to string
fn sales_db_error_to_string(err: sales_db.SalesDbError) -> String {
  case err {
    sales_db.SalesDbConnectionError(msg) -> "Connection: " <> msg
    sales_db.SalesDbQueryError(msg) -> "Query: " <> msg
    sales_db.SalesDbNotFound -> "Not found"
  }
}

/// POST /api/v1/leads/:id/notes - Add note to lead
pub fn add_lead_note(lead_id: String, body: String) -> Response(ResponseData) {
  case int.parse(lead_id) {
    Ok(id) -> {
      // TODO: Parse body and insert into database
      json_response(json.object([
        #("status", json.string("ok")),
        #("message", json.string("Note added")),
        #("lead_id", json.int(id)),
      ]), 201)
    }
    Error(_) -> json_error("Invalid lead ID", 400)
  }
}

/// GET /api/v1/leads - List leads as JSON
pub fn list_leads_json() -> Response(ResponseData) {
  // TODO: Fetch from database
  let leads_json = json.array([
    json.object([
      #("id", json.int(1)),
      #("telegram_user_id", json.int(144022504)),
      #("username", json.string("neuro_sage")),
      #("first_name", json.string("Федор")),
      #("status", json.string("new")),
      #("priority", json.string("high")),
      #("first_message", json.string("Привет! Хочу купить крипту")),
    ]),
  ], fn(x) { x })
  
  json_response(json.object([
    #("status", json.string("ok")),
    #("leads", leads_json),
    #("total", json.int(1)),
  ]), 200)
}

/// GET /api/v1/leads/:id - Get lead as JSON
pub fn get_lead_json(lead_id: String) -> Response(ResponseData) {
  case int.parse(lead_id) {
    Ok(id) -> {
      // TODO: Fetch from database
      json_response(json.object([
        #("status", json.string("ok")),
        #("lead", json.object([
          #("id", json.int(id)),
          #("telegram_user_id", json.int(144022504)),
          #("username", json.string("neuro_sage")),
          #("first_name", json.string("Федор")),
          #("status", json.string("new")),
          #("priority", json.string("high")),
        ])),
      ]), 200)
    }
    Error(_) -> json_error("Invalid lead ID", 400)
  }
}

/// POST /api/v1/leads/:id/message - Send message to lead
pub fn send_message_to_lead(lead_id: String, body: String) -> Response(ResponseData) {
  case int.parse(lead_id) {
    Ok(id) -> {
      // TODO: Parse body, send message via Telegram, save to database
      json_response(json.object([
        #("status", json.string("ok")),
        #("message", json.string("Message sent")),
        #("lead_id", json.int(id)),
      ]), 200)
    }
    Error(_) -> json_error("Invalid lead ID", 400)
  }
}

/// Helper: Create JSON response
fn json_response(data: json.Json, status: Int) -> Response(ResponseData) {
  response.new(status)
  |> response.set_body(mist.Bytes(bytes_tree.from_string(json.to_string(data))))
  |> response.set_header("content-type", "application/json")
}

/// Helper: Create JSON error response
fn json_error(message: String, status: Int) -> Response(ResponseData) {
  json_response(json.object([
    #("error", json.string(message)),
  ]), status)
}
