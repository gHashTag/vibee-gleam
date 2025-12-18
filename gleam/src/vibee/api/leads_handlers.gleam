// Leads API Handlers
// REST endpoints for leads management

import gleam/bytes_tree
import gleam/http
import gleam/http/response.{type Response}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import mist.{type ResponseData}
import vibee/db/postgres
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
pub fn update_lead_status(lead_id: String, body: String) -> Response(ResponseData) {
  case int.parse(lead_id) {
    Ok(id) -> {
      // TODO: Parse body and update database
      json_response(json.object([
        #("status", json.string("ok")),
        #("message", json.string("Status updated")),
        #("lead_id", json.int(id)),
      ]), 200)
    }
    Error(_) -> json_error("Invalid lead ID", 400)
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
