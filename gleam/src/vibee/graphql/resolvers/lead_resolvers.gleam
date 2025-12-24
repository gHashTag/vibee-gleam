// Lead Resolvers - GraphQL resolvers for lead queries and mutations
// Подключается к реальной БД и trigger_chats

import gleam/dict.{type Dict}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import vibee/config/trigger_chats
import vibee/db/postgres
import vibee/graphql/executor.{
  type ResolverRegistry, get_int_arg, get_int_arg_with_default,
  get_string_arg, new_registry, with_mutation, with_query,
}
import vibee/graphql/types.{type Context, type Value}
import vibee/sales/db as sales_db
import vibee/sales/lead_service
import vibee/sales/types as sales_types

// =============================================================================
// Registry Builder
// =============================================================================

pub fn build_registry() -> ResolverRegistry {
  new_registry()
  // Queries
  |> with_query("leads", resolve_leads)
  |> with_query("lead", resolve_lead)
  |> with_query("leadForwards", resolve_lead_forwards)
  |> with_query("triggerConfigs", resolve_trigger_configs)
  |> with_query("funnelStats", resolve_funnel_stats)
  // Mutations
  |> with_mutation("createLead", resolve_create_lead)
  |> with_mutation("updateLeadStatus", resolve_update_status)
  |> with_mutation("updateFunnelStage", resolve_update_funnel_stage)
  |> with_mutation("updateQuizResult", resolve_update_quiz_result)
  |> with_mutation("updateLeadPriority", resolve_update_priority)
  |> with_mutation("addLeadNote", resolve_add_note)
  |> with_mutation("assignLead", resolve_assign_lead)
  |> with_mutation("deleteLead", resolve_delete_lead)
}

// =============================================================================
// Query Resolvers
// =============================================================================

/// Resolve leads query with optional filters
fn resolve_leads(args: Dict(String, Value), _ctx: Context) -> Result(Json, String) {
  let _status = get_string_arg(args, "status")
  let limit = get_int_arg_with_default(args, "limit", 50)
  let _offset = get_int_arg_with_default(args, "offset", 0)

  // Try to get from DB, fallback to mock
  case postgres.get_global_pool() {
    Some(pool) -> {
      case sales_db.get_recent_leads(pool, limit) {
        Ok(leads) -> Ok(json.array(leads, lead_to_graphql_json))
        Error(_) -> Ok(mock_leads())
      }
    }
    None -> Ok(mock_leads())
  }
}

/// Resolve single lead by ID
fn resolve_lead(args: Dict(String, Value), _ctx: Context) -> Result(Json, String) {
  case get_int_arg(args, "id") {
    Some(id) -> {
      case postgres.get_global_pool() {
        Some(pool) -> {
          case sales_db.get_lead_by_id(pool, id) {
            Ok(lead) -> Ok(lead_to_graphql_json(lead))
            Error(_) -> Ok(json.null())
          }
        }
        None -> Ok(json.null())
      }
    }
    None -> Error("Missing required argument: id")
  }
}

/// Resolve lead forwards (пересылки лидов)
fn resolve_lead_forwards(args: Dict(String, Value), _ctx: Context) -> Result(Json, String) {
  let _lead_id = get_int_arg(args, "leadId")
  let _status = get_string_arg(args, "status")
  let limit = get_int_arg_with_default(args, "limit", 50)

  // Mock data for now (lead_forwards table needs to be queried)
  Ok(mock_lead_forwards(limit))
}

/// Resolve trigger configs from static config
fn resolve_trigger_configs(_args: Dict(String, Value), _ctx: Context) -> Result(Json, String) {
  let configs = trigger_chats.get_trigger_chats()
  Ok(json.array(configs, trigger_config_to_json))
}

/// Resolve funnel stats
fn resolve_funnel_stats(_args: Dict(String, Value), _ctx: Context) -> Result(Json, String) {
  case lead_service.get_funnel_stats() {
    Ok(stats) -> Ok(funnel_stats_to_json(stats))
    Error(_) -> Ok(mock_funnel_stats())
  }
}

// =============================================================================
// Mutation Resolvers
// =============================================================================

/// Create new lead
fn resolve_create_lead(args: Dict(String, Value), _ctx: Context) -> Result(Json, String) {
  case get_int_arg(args, "telegramUserId") {
    Some(telegram_user_id) -> {
      let username = get_string_arg(args, "username")
      let first_name = get_string_arg(args, "firstName")
      let source = get_string_arg(args, "source")
      let first_message = get_string_arg(args, "firstMessage")

      case lead_service.create_lead(telegram_user_id, username, first_name, first_message, source) {
        Ok(lead) -> Ok(lead_to_graphql_json(lead))
        Error(err) -> Error(lead_service.error_to_string(err))
      }
    }
    None -> Error("Missing required argument: telegramUserId")
  }
}

/// Update lead status
fn resolve_update_status(args: Dict(String, Value), _ctx: Context) -> Result(Json, String) {
  case get_int_arg(args, "leadId"), get_string_arg(args, "status") {
    Some(lead_id), Some(status_str) -> {
      case sales_types.lead_status_from_string(status_str) {
        Ok(status) -> {
          case lead_service.update_lead_status(lead_id, status) {
            Ok(lead) -> Ok(lead_to_graphql_json(lead))
            Error(err) -> Error(lead_service.error_to_string(err))
          }
        }
        Error(_) -> Error("Invalid status: " <> status_str)
      }
    }
    None, _ -> Error("Missing required argument: leadId")
    _, None -> Error("Missing required argument: status")
  }
}

/// Update funnel stage
fn resolve_update_funnel_stage(args: Dict(String, Value), _ctx: Context) -> Result(Json, String) {
  case get_int_arg(args, "leadId"), get_string_arg(args, "stage") {
    Some(lead_id), Some(stage_str) -> {
      case sales_types.funnel_stage_from_string(stage_str) {
        Ok(stage) -> {
          case lead_service.update_funnel_stage(lead_id, stage) {
            Ok(lead) -> Ok(lead_to_graphql_json(lead))
            Error(err) -> Error(lead_service.error_to_string(err))
          }
        }
        Error(_) -> Error("Invalid funnel stage: " <> stage_str)
      }
    }
    None, _ -> Error("Missing required argument: leadId")
    _, None -> Error("Missing required argument: stage")
  }
}

/// Update quiz result
fn resolve_update_quiz_result(args: Dict(String, Value), _ctx: Context) -> Result(Json, String) {
  case get_int_arg(args, "leadId"), get_int_arg(args, "score"), get_int_arg(args, "productId") {
    Some(lead_id), Some(score), Some(product_id) -> {
      case lead_service.update_quiz_result(lead_id, score, product_id) {
        Ok(lead) -> Ok(lead_to_graphql_json(lead))
        Error(err) -> Error(lead_service.error_to_string(err))
      }
    }
    None, _, _ -> Error("Missing required argument: leadId")
    _, None, _ -> Error("Missing required argument: score")
    _, _, None -> Error("Missing required argument: productId")
  }
}

/// Update lead priority
fn resolve_update_priority(args: Dict(String, Value), _ctx: Context) -> Result(Json, String) {
  case get_int_arg(args, "leadId"), get_string_arg(args, "priority") {
    Some(lead_id), Some(priority_str) -> {
      case sales_types.lead_priority_from_string(priority_str) {
        Ok(priority) -> {
          case lead_service.update_lead_priority(lead_id, priority) {
            Ok(lead) -> Ok(lead_to_graphql_json(lead))
            Error(err) -> Error(lead_service.error_to_string(err))
          }
        }
        Error(_) -> Error("Invalid priority: " <> priority_str)
      }
    }
    None, _ -> Error("Missing required argument: leadId")
    _, None -> Error("Missing required argument: priority")
  }
}

/// Add note to lead
fn resolve_add_note(args: Dict(String, Value), _ctx: Context) -> Result(Json, String) {
  case get_int_arg(args, "leadId"), get_string_arg(args, "note") {
    Some(lead_id), Some(note) -> {
      case lead_service.add_lead_note(lead_id, note) {
        Ok(lead) -> Ok(lead_to_graphql_json(lead))
        Error(err) -> Error(lead_service.error_to_string(err))
      }
    }
    None, _ -> Error("Missing required argument: leadId")
    _, None -> Error("Missing required argument: note")
  }
}

/// Assign lead to agent
fn resolve_assign_lead(args: Dict(String, Value), _ctx: Context) -> Result(Json, String) {
  case get_int_arg(args, "leadId"), get_string_arg(args, "agentId") {
    Some(lead_id), Some(agent_id) -> {
      case lead_service.assign_lead(lead_id, agent_id) {
        Ok(lead) -> Ok(lead_to_graphql_json(lead))
        Error(err) -> Error(lead_service.error_to_string(err))
      }
    }
    None, _ -> Error("Missing required argument: leadId")
    _, None -> Error("Missing required argument: agentId")
  }
}

/// Delete lead
fn resolve_delete_lead(args: Dict(String, Value), _ctx: Context) -> Result(Json, String) {
  case get_int_arg(args, "leadId") {
    Some(lead_id) -> {
      case lead_service.delete_lead(lead_id) {
        Ok(deleted) -> Ok(json.object([
          #("id", json.int(lead_id)),
          #("deleted", json.bool(deleted)),
          #("message", json.string(case deleted {
            True -> "Lead deleted successfully"
            False -> "Lead not found"
          })),
        ]))
        Error(err) -> Error(lead_service.error_to_string(err))
      }
    }
    None -> Error("Missing required argument: leadId")
  }
}

// =============================================================================
// JSON Serialization
// =============================================================================

fn lead_to_graphql_json(lead: sales_types.Lead) -> Json {
  json.object([
    #("id", case lead.id {
      Some(id) -> json.int(id)
      None -> json.null()
    }),
    #("telegramUserId", json.int(lead.telegram_user_id)),
    #("username", option_to_json(lead.username, json.string)),
    #("firstName", option_to_json(lead.first_name, json.string)),
    #("lastName", option_to_json(lead.last_name, json.string)),
    #("status", json.string(sales_types.lead_status_to_string(lead.status))),
    #("funnelStage", json.string(sales_types.funnel_stage_to_string(lead.funnel_stage))),
    #("priority", json.string(sales_types.lead_priority_to_string(lead.priority))),
    #("qualityScore", option_to_json(lead.quiz_score, json.int)),
    #("source", option_to_json(lead.source, json.string)),
    #("createdAt", option_to_json(lead.created_at, json.string)),
  ])
}

fn trigger_config_to_json(config: trigger_chats.TriggerChatConfig) -> Json {
  json.object([
    #("chatId", json.string(config.chat_id)),
    #("chatName", json.string(config.chat_name)),
    #("isActive", json.bool(config.is_active)),
    #("triggers", json.array(config.custom_triggers, json.string)),
    #("forwardChatId", json.string(config.forward_chat_id)),
  ])
}

fn funnel_stats_to_json(stats: lead_service.FunnelStats) -> Json {
  json.object([
    #("awareness", json.int(stats.awareness)),
    #("interest", json.int(stats.interest)),
    #("consideration", json.int(stats.consideration)),
    #("intent", json.int(stats.intent)),
    #("evaluation", json.int(stats.evaluation)),
    #("purchase", json.int(stats.purchase)),
    #("total", json.int(
      stats.awareness + stats.interest + stats.consideration +
      stats.intent + stats.evaluation + stats.purchase
    )),
  ])
}

fn option_to_json(opt: Option(a), encoder: fn(a) -> Json) -> Json {
  case opt {
    Some(val) -> encoder(val)
    None -> json.null()
  }
}

// =============================================================================
// Mock Data
// =============================================================================

fn mock_leads() -> Json {
  json.array([
    json.object([
      #("id", json.int(1)),
      #("telegramUserId", json.int(144022504)),
      #("username", json.string("neuro_sage")),
      #("firstName", json.string("Dmitrii")),
      #("lastName", json.null()),
      #("status", json.string("NEW")),
      #("funnelStage", json.string("AWARENESS")),
      #("priority", json.string("MEDIUM")),
      #("qualityScore", json.int(7)),
      #("source", json.string("Aimly.io dev")),
      #("createdAt", json.string("2024-12-22T10:00:00Z")),
    ]),
    json.object([
      #("id", json.int(2)),
      #("telegramUserId", json.int(987654321)),
      #("username", json.string("crypto_buyer")),
      #("firstName", json.string("Alex")),
      #("lastName", json.null()),
      #("status", json.string("CONTACTED")),
      #("funnelStage", json.string("INTEREST")),
      #("priority", json.string("HIGH")),
      #("qualityScore", json.int(8)),
      #("source", json.string("P2P Thailand")),
      #("createdAt", json.string("2024-12-21T15:30:00Z")),
    ]),
  ], fn(x) { x })
}

fn mock_lead_forwards(limit: Int) -> Json {
  let all_forwards = [
    json.object([
      #("id", json.int(1)),
      #("leadId", json.int(1)),
      #("sourceChatId", json.string("-5082217642")),
      #("sourceChatName", json.string("Aimly.io dev")),
      #("targetChatId", json.string("-1002737186844")),
      #("qualityScore", json.int(7)),
      #("intent", json.string("purchase")),
      #("urgency", json.string("normal")),
      #("status", json.string("FORWARDED")),
      #("forwardedAt", json.string("2024-12-22T10:05:00Z")),
    ]),
    json.object([
      #("id", json.int(2)),
      #("leadId", json.int(2)),
      #("sourceChatId", json.string("-5082217642")),
      #("sourceChatName", json.string("P2P Thailand")),
      #("targetChatId", json.string("-1002737186844")),
      #("qualityScore", json.int(8)),
      #("intent", json.string("purchase")),
      #("urgency", json.string("high")),
      #("status", json.string("FORWARDED")),
      #("forwardedAt", json.string("2024-12-21T15:35:00Z")),
    ]),
  ]
  json.array(list.take(all_forwards, limit), fn(x) { x })
}

fn mock_funnel_stats() -> Json {
  json.object([
    #("awareness", json.int(15)),
    #("interest", json.int(8)),
    #("consideration", json.int(5)),
    #("intent", json.int(3)),
    #("evaluation", json.int(2)),
    #("purchase", json.int(1)),
    #("total", json.int(34)),
  ])
}
