// Lead Service - Управление лидами
// Использует PostgreSQL для хранения данных

import gleam/json
import gleam/option.{type Option, None, Some}
import vibee/db/postgres
import vibee/sales/db as sales_db
import vibee/sales/types.{
  type FunnelStage, type Lead, type LeadIntent, type LeadPriority, type LeadStatus,
  IntentPurchase, Lead, PriorityMedium, StageAwareness, StatusNew,
}

// =============================================================================
// Errors
// =============================================================================

pub type LeadError {
  DatabaseError(String)
  NotFoundError(String)
  ValidationError(String)
}

pub fn error_to_string(err: LeadError) -> String {
  case err {
    DatabaseError(msg) -> "Database error: " <> msg
    NotFoundError(msg) -> "Not found: " <> msg
    ValidationError(msg) -> "Validation error: " <> msg
  }
}

// =============================================================================
// Mock Lead (для fallback)
// =============================================================================

fn mock_lead(
  telegram_user_id: Int,
  username: Option(String),
  first_name: Option(String),
  first_message: Option(String),
  source: Option(String),
) -> Lead {
  Lead(
    id: Some(1),
    telegram_user_id: telegram_user_id,
    username: username,
    first_name: first_name,
    last_name: None,
    phone: None,
    email: None,
    first_message: first_message,
    intent: Some(IntentPurchase),
    priority: PriorityMedium,
    status: StatusNew,
    funnel_stage: StageAwareness,
    source: source,
    utm_source: None,
    utm_medium: None,
    utm_campaign: None,
    quiz_score: None,
    recommended_product_id: None,
    assigned_to: None,
    notes: None,
    last_contact_at: None,
    created_at: Some("2024-01-01T00:00:00Z"),
  )
}

// =============================================================================
// Lead CRUD
// =============================================================================

/// Создать нового лида
pub fn create_lead(
  telegram_user_id: Int,
  username: Option(String),
  first_name: Option(String),
  first_message: Option(String),
  source: Option(String),
) -> Result(Lead, LeadError) {
  case postgres.get_global_pool() {
    Some(pool) -> {
      case sales_db.create_lead(pool, telegram_user_id, username, first_name, first_message, source) {
        Ok(lead) -> Ok(lead)
        Error(sales_db.SalesDbQueryError(msg)) -> Error(DatabaseError(msg))
        Error(sales_db.SalesDbConnectionError(msg)) -> Error(DatabaseError(msg))
        Error(_) -> Error(DatabaseError("Failed to create lead"))
      }
    }
    None -> {
      // Fallback to mock
      Ok(mock_lead(telegram_user_id, username, first_name, first_message, source))
    }
  }
}

/// Получить или создать лида
pub fn get_or_create_lead(
  telegram_user_id: Int,
  username: Option(String),
  first_name: Option(String),
  first_message: Option(String),
  source: Option(String),
) -> Result(Lead, LeadError) {
  case postgres.get_global_pool() {
    Some(pool) -> {
      case sales_db.get_or_create_lead(pool, telegram_user_id, username, first_name, first_message, source) {
        Ok(lead) -> Ok(lead)
        Error(sales_db.SalesDbQueryError(msg)) -> Error(DatabaseError(msg))
        Error(sales_db.SalesDbConnectionError(msg)) -> Error(DatabaseError(msg))
        Error(_) -> Error(DatabaseError("Failed to get or create lead"))
      }
    }
    None -> {
      // Fallback to mock
      Ok(mock_lead(telegram_user_id, username, first_name, first_message, source))
    }
  }
}

/// Обновить статус лида
pub fn update_lead_status(
  lead_id: Int,
  status: LeadStatus,
) -> Result(Lead, LeadError) {
  case postgres.get_global_pool() {
    Some(pool) -> {
      case sales_db.update_lead_status(pool, lead_id, status) {
        Ok(lead) -> Ok(lead)
        Error(sales_db.SalesDbNotFound) ->
          Error(NotFoundError("Lead not found"))
        Error(sales_db.SalesDbQueryError(msg)) -> Error(DatabaseError(msg))
        Error(sales_db.SalesDbConnectionError(msg)) -> Error(DatabaseError(msg))
      }
    }
    None -> {
      // Stub
      Ok(Lead(
        id: Some(lead_id),
        telegram_user_id: 0,
        username: None,
        first_name: None,
        last_name: None,
        phone: None,
        email: None,
        first_message: None,
        intent: None,
        priority: PriorityMedium,
        status: status,
        funnel_stage: StageAwareness,
        source: None,
        utm_source: None,
        utm_medium: None,
        utm_campaign: None,
        quiz_score: None,
        recommended_product_id: None,
        assigned_to: None,
        notes: None,
        last_contact_at: None,
        created_at: None,
      ))
    }
  }
}

/// Обновить этап воронки
pub fn update_funnel_stage(
  lead_id: Int,
  stage: FunnelStage,
) -> Result(Lead, LeadError) {
  case postgres.get_global_pool() {
    Some(pool) -> {
      case sales_db.update_funnel_stage(pool, lead_id, stage) {
        Ok(lead) -> Ok(lead)
        Error(sales_db.SalesDbNotFound) ->
          Error(NotFoundError("Lead not found"))
        Error(sales_db.SalesDbQueryError(msg)) -> Error(DatabaseError(msg))
        Error(sales_db.SalesDbConnectionError(msg)) -> Error(DatabaseError(msg))
      }
    }
    None -> {
      // Stub
      Ok(Lead(
        id: Some(lead_id),
        telegram_user_id: 0,
        username: None,
        first_name: None,
        last_name: None,
        phone: None,
        email: None,
        first_message: None,
        intent: None,
        priority: PriorityMedium,
        status: StatusNew,
        funnel_stage: stage,
        source: None,
        utm_source: None,
        utm_medium: None,
        utm_campaign: None,
        quiz_score: None,
        recommended_product_id: None,
        assigned_to: None,
        notes: None,
        last_contact_at: None,
        created_at: None,
      ))
    }
  }
}

/// Обновить результат квиза
pub fn update_quiz_result(
  lead_id: Int,
  score: Int,
  product_id: Int,
) -> Result(Lead, LeadError) {
  case postgres.get_global_pool() {
    Some(pool) -> {
      case sales_db.update_quiz_result(pool, lead_id, score, product_id) {
        Ok(lead) -> Ok(lead)
        Error(sales_db.SalesDbNotFound) ->
          Error(NotFoundError("Lead not found"))
        Error(sales_db.SalesDbQueryError(msg)) -> Error(DatabaseError(msg))
        Error(sales_db.SalesDbConnectionError(msg)) -> Error(DatabaseError(msg))
      }
    }
    None -> {
      // Stub
      Ok(Lead(
        id: Some(lead_id),
        telegram_user_id: 0,
        username: None,
        first_name: None,
        last_name: None,
        phone: None,
        email: None,
        first_message: None,
        intent: None,
        priority: PriorityMedium,
        status: StatusNew,
        funnel_stage: StageAwareness,
        source: None,
        utm_source: None,
        utm_medium: None,
        utm_campaign: None,
        quiz_score: Some(score),
        recommended_product_id: Some(product_id),
        assigned_to: None,
        notes: None,
        last_contact_at: None,
        created_at: None,
      ))
    }
  }
}

/// Статистика воронки
pub type FunnelStats {
  FunnelStats(
    awareness: Int,
    interest: Int,
    consideration: Int,
    intent: Int,
    evaluation: Int,
    purchase: Int,
  )
}

pub fn get_funnel_stats() -> Result(FunnelStats, LeadError) {
  case postgres.get_global_pool() {
    Some(pool) -> {
      case sales_db.get_funnel_stats(pool) {
        Ok(stats) -> Ok(FunnelStats(
          awareness: stats.awareness,
          interest: stats.interest,
          consideration: stats.consideration,
          intent: stats.intent,
          evaluation: stats.evaluation,
          purchase: stats.purchase,
        ))
        Error(sales_db.SalesDbQueryError(msg)) -> Error(DatabaseError(msg))
        Error(sales_db.SalesDbConnectionError(msg)) -> Error(DatabaseError(msg))
        Error(_) -> Error(DatabaseError("Failed to get funnel stats"))
      }
    }
    None -> {
      // Stub
      Ok(FunnelStats(
        awareness: 10,
        interest: 5,
        consideration: 3,
        intent: 2,
        evaluation: 1,
        purchase: 1,
      ))
    }
  }
}

/// Обновить приоритет лида
pub fn update_lead_priority(
  lead_id: Int,
  priority: LeadPriority,
) -> Result(Lead, LeadError) {
  case postgres.get_global_pool() {
    Some(pool) -> {
      case sales_db.update_lead_priority(pool, lead_id, priority) {
        Ok(lead) -> Ok(lead)
        Error(sales_db.SalesDbNotFound) ->
          Error(NotFoundError("Lead not found"))
        Error(sales_db.SalesDbQueryError(msg)) -> Error(DatabaseError(msg))
        Error(sales_db.SalesDbConnectionError(msg)) -> Error(DatabaseError(msg))
      }
    }
    None -> {
      Ok(Lead(
        id: Some(lead_id),
        telegram_user_id: 0,
        username: None,
        first_name: None,
        last_name: None,
        phone: None,
        email: None,
        first_message: None,
        intent: None,
        priority: priority,
        status: StatusNew,
        funnel_stage: StageAwareness,
        source: None,
        utm_source: None,
        utm_medium: None,
        utm_campaign: None,
        quiz_score: None,
        recommended_product_id: None,
        assigned_to: None,
        notes: None,
        last_contact_at: None,
        created_at: None,
      ))
    }
  }
}

/// Добавить заметку к лиду
pub fn add_lead_note(
  lead_id: Int,
  note: String,
) -> Result(Lead, LeadError) {
  case postgres.get_global_pool() {
    Some(pool) -> {
      case sales_db.add_lead_note(pool, lead_id, note) {
        Ok(lead) -> Ok(lead)
        Error(sales_db.SalesDbNotFound) ->
          Error(NotFoundError("Lead not found"))
        Error(sales_db.SalesDbQueryError(msg)) -> Error(DatabaseError(msg))
        Error(sales_db.SalesDbConnectionError(msg)) -> Error(DatabaseError(msg))
      }
    }
    None -> {
      Ok(Lead(
        id: Some(lead_id),
        telegram_user_id: 0,
        username: None,
        first_name: None,
        last_name: None,
        phone: None,
        email: None,
        first_message: None,
        intent: None,
        priority: PriorityMedium,
        status: StatusNew,
        funnel_stage: StageAwareness,
        source: None,
        utm_source: None,
        utm_medium: None,
        utm_campaign: None,
        quiz_score: None,
        recommended_product_id: None,
        assigned_to: None,
        notes: Some(note),
        last_contact_at: None,
        created_at: None,
      ))
    }
  }
}

/// Назначить лида на агента
pub fn assign_lead(
  lead_id: Int,
  agent_id: String,
) -> Result(Lead, LeadError) {
  case postgres.get_global_pool() {
    Some(pool) -> {
      case sales_db.assign_lead(pool, lead_id, agent_id) {
        Ok(lead) -> Ok(lead)
        Error(sales_db.SalesDbNotFound) ->
          Error(NotFoundError("Lead not found"))
        Error(sales_db.SalesDbQueryError(msg)) -> Error(DatabaseError(msg))
        Error(sales_db.SalesDbConnectionError(msg)) -> Error(DatabaseError(msg))
      }
    }
    None -> {
      Ok(Lead(
        id: Some(lead_id),
        telegram_user_id: 0,
        username: None,
        first_name: None,
        last_name: None,
        phone: None,
        email: None,
        first_message: None,
        intent: None,
        priority: PriorityMedium,
        status: StatusNew,
        funnel_stage: StageAwareness,
        source: None,
        utm_source: None,
        utm_medium: None,
        utm_campaign: None,
        quiz_score: None,
        recommended_product_id: None,
        assigned_to: Some(agent_id),
        notes: None,
        last_contact_at: None,
        created_at: None,
      ))
    }
  }
}

/// Удалить лида
pub fn delete_lead(lead_id: Int) -> Result(Bool, LeadError) {
  case postgres.get_global_pool() {
    Some(pool) -> {
      case sales_db.delete_lead(pool, lead_id) {
        Ok(deleted) -> Ok(deleted)
        Error(sales_db.SalesDbQueryError(msg)) -> Error(DatabaseError(msg))
        Error(sales_db.SalesDbConnectionError(msg)) -> Error(DatabaseError(msg))
        Error(_) -> Error(DatabaseError("Failed to delete lead"))
      }
    }
    None -> Ok(True)
  }
}

// =============================================================================
// JSON Serialization
// =============================================================================

pub fn lead_to_json(lead: Lead) -> json.Json {
  json.object([
    #("id", case lead.id {
      Some(id) -> json.int(id)
      None -> json.null()
    }),
    #("telegram_user_id", json.int(lead.telegram_user_id)),
    #("username", case lead.username {
      Some(u) -> json.string(u)
      None -> json.null()
    }),
    #("first_name", case lead.first_name {
      Some(n) -> json.string(n)
      None -> json.null()
    }),
    #("status", json.string(types.lead_status_to_string(lead.status))),
    #("funnel_stage", json.string(types.funnel_stage_to_string(lead.funnel_stage))),
    #("priority", json.string(types.lead_priority_to_string(lead.priority))),
    #("quiz_score", case lead.quiz_score {
      Some(s) -> json.int(s)
      None -> json.null()
    }),
    #("recommended_product_id", case lead.recommended_product_id {
      Some(p) -> json.int(p)
      None -> json.null()
    }),
  ])
}

pub fn funnel_stats_to_json(stats: FunnelStats) -> json.Json {
  json.object([
    #("awareness", json.int(stats.awareness)),
    #("interest", json.int(stats.interest)),
    #("consideration", json.int(stats.consideration)),
    #("intent", json.int(stats.intent)),
    #("evaluation", json.int(stats.evaluation)),
    #("purchase", json.int(stats.purchase)),
  ])
}
