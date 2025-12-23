// GraphQL PubSub
// ETS-backed pub/sub for GraphQL subscriptions

import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}

// === TOPIC CONSTANTS ===

pub const topic_lead_forwarded = "leadForwarded"
pub const topic_lead_status_changed = "leadStatusChanged"
pub const topic_trigger_detected = "triggerDetected"
pub const topic_forward_failed = "forwardFailed"
pub const topic_funnel_stats = "funnelStatsUpdated"

// === EVENT TYPES ===

pub type PubSubEvent {
  LeadForwardedEvent(
    forward_id: Int,
    lead_id: Option(Int),
    source_chat_id: String,
    source_chat_name: String,
    target_chat_id: String,
    user_id: Int,
    username: String,
    user_name: String,
    quality_score: Int,
    intent: String,
    urgency: String,
    message_id: Int,
    timestamp: Int,
  )
  LeadStatusChangedEvent(
    lead_id: Int,
    old_status: String,
    new_status: String,
    old_funnel_stage: Option(String),
    new_funnel_stage: Option(String),
    changed_by: String,
    timestamp: Int,
  )
  TriggerDetectedEvent(
    chat_id: String,
    chat_name: String,
    trigger: String,
    user_id: Int,
    username: Option(String),
    message_text: String,
    timestamp: Int,
  )
  ForwardFailedEvent(
    user_id: Int,
    username: String,
    source_chat_id: String,
    source_chat_name: String,
    target_chat_id: String,
    reason: String,
    error_message: String,
    timestamp: Int,
  )
  FunnelStatsUpdatedEvent(
    awareness: Int,
    interest: Int,
    consideration: Int,
    intent: Int,
    evaluation: Int,
    purchase: Int,
    total: Int,
  )
}

// === SUBSCRIPTION MANAGEMENT ===

pub fn subscribe_lead_forwarded(
  client_id: String,
  subscription_id: String,
  filter_chat_id: Option(String),
) -> Nil {
  ffi_subscribe(client_id, subscription_id, topic_lead_forwarded, filter_chat_id)
}

pub fn subscribe_lead_status_changed(
  client_id: String,
  subscription_id: String,
  filter_lead_id: Option(Int),
) -> Nil {
  let filter = case filter_lead_id {
    Some(id) -> Some(int.to_string(id))
    None -> None
  }
  ffi_subscribe(client_id, subscription_id, topic_lead_status_changed, filter)
}

pub fn subscribe_trigger_detected(
  client_id: String,
  subscription_id: String,
  filter_chat_id: Option(String),
) -> Nil {
  ffi_subscribe(client_id, subscription_id, topic_trigger_detected, filter_chat_id)
}

pub fn subscribe_forward_failed(
  client_id: String,
  subscription_id: String,
  filter_chat_id: Option(String),
) -> Nil {
  ffi_subscribe(client_id, subscription_id, topic_forward_failed, filter_chat_id)
}

pub fn subscribe_funnel_stats(client_id: String, subscription_id: String) -> Nil {
  ffi_subscribe(client_id, subscription_id, topic_funnel_stats, None)
}

pub fn unsubscribe(client_id: String, subscription_id: String) -> Nil {
  ffi_unsubscribe(client_id, subscription_id)
}

pub fn unsubscribe_client(client_id: String) -> Nil {
  ffi_unsubscribe_client(client_id)
}

// === PUBLISHING ===

pub fn publish_lead_forwarded(event: PubSubEvent) -> Nil {
  ffi_publish(topic_lead_forwarded, event)
}

pub fn publish_lead_status_changed(event: PubSubEvent) -> Nil {
  ffi_publish(topic_lead_status_changed, event)
}

pub fn publish_trigger_detected(event: PubSubEvent) -> Nil {
  ffi_publish(topic_trigger_detected, event)
}

pub fn publish_forward_failed(event: PubSubEvent) -> Nil {
  ffi_publish(topic_forward_failed, event)
}

pub fn publish_funnel_stats_updated(event: PubSubEvent) -> Nil {
  ffi_publish(topic_funnel_stats, event)
}

// === EVENT RETRIEVAL ===

pub fn get_pending_events(
  client_id: String,
  subscription_id: String,
) -> List(Dynamic) {
  ffi_get_pending_events(client_id, subscription_id)
}

pub fn clear_pending_events(client_id: String, subscription_id: String) -> Nil {
  ffi_clear_pending_events(client_id, subscription_id)
}

// === JSON ENCODING ===

pub fn encode_event(event: PubSubEvent) -> Json {
  case event {
    LeadForwardedEvent(
      forward_id, lead_id, source_chat_id, source_chat_name, target_chat_id,
      user_id, username, user_name, quality_score, intent, urgency,
      message_id, timestamp,
    ) ->
      json.object([
        #("__typename", json.string("LeadForward")),
        #("id", json.int(forward_id)),
        #("leadId", case lead_id { Some(id) -> json.int(id) None -> json.null() }),
        #("sourceChat", json.object([
          #("id", json.string(source_chat_id)),
          #("name", json.string(source_chat_name)),
        ])),
        #("targetChatId", json.string(target_chat_id)),
        #("userId", json.int(user_id)),
        #("username", json.string(username)),
        #("userName", json.string(user_name)),
        #("qualityScore", json.int(quality_score)),
        #("intent", json.string(intent)),
        #("urgency", json.string(urgency)),
        #("messageId", json.int(message_id)),
        #("timestamp", json.int(timestamp)),
      ])

    LeadStatusChangedEvent(
      lead_id, old_status, new_status, old_funnel_stage, new_funnel_stage,
      changed_by, timestamp,
    ) ->
      json.object([
        #("__typename", json.string("LeadStatusChange")),
        #("leadId", json.int(lead_id)),
        #("oldStatus", json.string(old_status)),
        #("newStatus", json.string(new_status)),
        #("oldFunnelStage", case old_funnel_stage { Some(s) -> json.string(s) None -> json.null() }),
        #("newFunnelStage", case new_funnel_stage { Some(s) -> json.string(s) None -> json.null() }),
        #("changedBy", json.string(changed_by)),
        #("timestamp", json.int(timestamp)),
      ])

    TriggerDetectedEvent(
      chat_id, chat_name, trigger, user_id, username, message_text, timestamp,
    ) ->
      json.object([
        #("__typename", json.string("TriggerEvent")),
        #("chat", json.object([
          #("id", json.string(chat_id)),
          #("name", json.string(chat_name)),
        ])),
        #("trigger", json.string(trigger)),
        #("userId", json.int(user_id)),
        #("username", case username { Some(u) -> json.string(u) None -> json.null() }),
        #("messageText", json.string(message_text)),
        #("timestamp", json.int(timestamp)),
      ])

    ForwardFailedEvent(
      user_id, username, source_chat_id, source_chat_name, target_chat_id,
      reason, error_message, timestamp,
    ) ->
      json.object([
        #("__typename", json.string("ForwardError")),
        #("userId", json.int(user_id)),
        #("username", json.string(username)),
        #("sourceChat", json.object([
          #("id", json.string(source_chat_id)),
          #("name", json.string(source_chat_name)),
        ])),
        #("targetChatId", json.string(target_chat_id)),
        #("reason", json.string(reason)),
        #("errorMessage", json.string(error_message)),
        #("timestamp", json.int(timestamp)),
      ])

    FunnelStatsUpdatedEvent(
      awareness, interest, consideration, intent, evaluation, purchase, total,
    ) ->
      json.object([
        #("__typename", json.string("FunnelStats")),
        #("awareness", json.int(awareness)),
        #("interest", json.int(interest)),
        #("consideration", json.int(consideration)),
        #("intent", json.int(intent)),
        #("evaluation", json.int(evaluation)),
        #("purchase", json.int(purchase)),
        #("total", json.int(total)),
      ])
  }
}

// === FFI DECLARATIONS ===

@external(erlang, "vibee_pubsub_ffi", "subscribe")
fn ffi_subscribe(
  client_id: String,
  subscription_id: String,
  topic: String,
  filter: Option(String),
) -> Nil

@external(erlang, "vibee_pubsub_ffi", "unsubscribe")
fn ffi_unsubscribe(client_id: String, subscription_id: String) -> Nil

@external(erlang, "vibee_pubsub_ffi", "unsubscribe_client")
fn ffi_unsubscribe_client(client_id: String) -> Nil

@external(erlang, "vibee_pubsub_ffi", "publish")
fn ffi_publish(topic: String, event: PubSubEvent) -> Nil

@external(erlang, "vibee_pubsub_ffi", "get_pending_events")
fn ffi_get_pending_events(
  client_id: String,
  subscription_id: String,
) -> List(Dynamic)

@external(erlang, "vibee_pubsub_ffi", "clear_pending_events")
fn ffi_clear_pending_events(client_id: String, subscription_id: String) -> Nil
