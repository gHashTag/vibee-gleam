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
  |> with_mutation("updateLeadStatus", resolve_update_status)
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
