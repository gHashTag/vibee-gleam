// Sales PostgreSQL Database Operations
// CRUD for subscriptions, leads, quiz_responses, usage_logs, proposals

import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import pog
import vibee/sales/types.{
  type FunnelStage, type Lead, type LeadIntent, type LeadPriority,
  type LeadStatus, type PaymentMethod, type Subscription, type SubscriptionStatus,
  Lead, PriorityMedium, StageAwareness, StatusNew, SubActive, SubPending,
  Subscription,
  funnel_stage_from_string, funnel_stage_to_string,
  lead_intent_from_string, lead_intent_to_string,
  lead_priority_from_string, lead_priority_to_string,
  lead_status_from_string, lead_status_to_string,
  payment_method_from_string, payment_method_to_string,
  subscription_status_from_string, subscription_status_to_string,
}

// =============================================================================
// Types
// =============================================================================

pub type SalesDbError {
  SalesDbConnectionError(String)
  SalesDbQueryError(String)
  SalesDbNotFound
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

// =============================================================================
// Subscription CRUD
// =============================================================================

/// Получить активную подписку пользователя
pub fn get_active_subscription(
  pool: pog.Connection,
  telegram_id: Int,
) -> Result(Subscription, SalesDbError) {
  let sql =
    "SELECT id, telegram_id, product_id, status, payment_method, payment_id,
            current_period_start::text, current_period_end::text,
            generations_used, cancel_at_period_end, cancelled_at::text
     FROM subscriptions
     WHERE telegram_id = $1 AND status = 'active'
     ORDER BY created_at DESC LIMIT 1"

  case
    pog.query(sql)
    |> add_parameters([pog.int(telegram_id)])
    |> pog.returning(decode_subscription())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [sub])) -> Ok(sub)
    Ok(pog.Returned(_, [])) -> Error(SalesDbNotFound)
    Ok(_) -> Error(SalesDbNotFound)
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// Получить любую подписку пользователя (включая неактивные)
pub fn get_subscription_by_telegram_id(
  pool: pog.Connection,
  telegram_id: Int,
) -> Result(Subscription, SalesDbError) {
  let sql =
    "SELECT id, telegram_id, product_id, status, payment_method, payment_id,
            current_period_start::text, current_period_end::text,
            generations_used, cancel_at_period_end, cancelled_at::text
     FROM subscriptions
     WHERE telegram_id = $1
     ORDER BY created_at DESC LIMIT 1"

  case
    pog.query(sql)
    |> add_parameters([pog.int(telegram_id)])
    |> pog.returning(decode_subscription())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [sub])) -> Ok(sub)
    Ok(pog.Returned(_, [])) -> Error(SalesDbNotFound)
    Ok(_) -> Error(SalesDbNotFound)
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// Создать новую подписку
pub fn create_subscription(
  pool: pog.Connection,
  telegram_id: Int,
  product_id: Int,
  payment_method: PaymentMethod,
  payment_id: String,
) -> Result(Subscription, SalesDbError) {
  let sql =
    "INSERT INTO subscriptions (telegram_id, product_id, status, payment_method, payment_id)
     VALUES ($1, $2, 'pending', $3, $4)
     RETURNING id, telegram_id, product_id, status, payment_method, payment_id,
               current_period_start::text, current_period_end::text,
               generations_used, cancel_at_period_end, cancelled_at::text"

  case
    pog.query(sql)
    |> add_parameters([
      pog.int(telegram_id),
      pog.int(product_id),
      pog.text(payment_method_to_string(payment_method)),
      pog.text(payment_id),
    ])
    |> pog.returning(decode_subscription())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [sub])) -> Ok(sub)
    Ok(_) -> Error(SalesDbQueryError("Failed to create subscription"))
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// Активировать подписку
pub fn activate_subscription(
  pool: pog.Connection,
  subscription_id: Int,
) -> Result(Subscription, SalesDbError) {
  let sql =
    "UPDATE subscriptions
     SET status = 'active',
         current_period_start = NOW(),
         current_period_end = NOW() + INTERVAL '1 month'
     WHERE id = $1
     RETURNING id, telegram_id, product_id, status, payment_method, payment_id,
               current_period_start::text, current_period_end::text,
               generations_used, cancel_at_period_end, cancelled_at::text"

  case
    pog.query(sql)
    |> add_parameters([pog.int(subscription_id)])
    |> pog.returning(decode_subscription())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [sub])) -> Ok(sub)
    Ok(pog.Returned(_, [])) -> Error(SalesDbNotFound)
    Ok(_) -> Error(SalesDbNotFound)
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// Отменить подписку
pub fn cancel_subscription(
  pool: pog.Connection,
  subscription_id: Int,
) -> Result(Subscription, SalesDbError) {
  let sql =
    "UPDATE subscriptions
     SET cancel_at_period_end = TRUE, cancelled_at = NOW()
     WHERE id = $1
     RETURNING id, telegram_id, product_id, status, payment_method, payment_id,
               current_period_start::text, current_period_end::text,
               generations_used, cancel_at_period_end, cancelled_at::text"

  case
    pog.query(sql)
    |> add_parameters([pog.int(subscription_id)])
    |> pog.returning(decode_subscription())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [sub])) -> Ok(sub)
    Ok(pog.Returned(_, [])) -> Error(SalesDbNotFound)
    Ok(_) -> Error(SalesDbNotFound)
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// Увеличить счётчик использования
pub fn increment_usage(
  pool: pog.Connection,
  subscription_id: Int,
) -> Result(Subscription, SalesDbError) {
  let sql =
    "UPDATE subscriptions
     SET generations_used = generations_used + 1
     WHERE id = $1
     RETURNING id, telegram_id, product_id, status, payment_method, payment_id,
               current_period_start::text, current_period_end::text,
               generations_used, cancel_at_period_end, cancelled_at::text"

  case
    pog.query(sql)
    |> add_parameters([pog.int(subscription_id)])
    |> pog.returning(decode_subscription())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [sub])) -> Ok(sub)
    Ok(pog.Returned(_, [])) -> Error(SalesDbNotFound)
    Ok(_) -> Error(SalesDbNotFound)
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// Получить количество использований за текущий период
pub fn get_current_usage(
  pool: pog.Connection,
  telegram_id: Int,
) -> Result(Int, SalesDbError) {
  let sql =
    "SELECT COALESCE(generations_used, 0)
     FROM subscriptions
     WHERE telegram_id = $1 AND status = 'active'
     LIMIT 1"

  let int_decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(count)
  }

  case
    pog.query(sql)
    |> add_parameters([pog.int(telegram_id)])
    |> pog.returning(int_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [count])) -> Ok(count)
    Ok(pog.Returned(_, [])) -> Ok(0)
    Ok(_) -> Ok(0)
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Lead CRUD
// =============================================================================

/// Создать нового лида
pub fn create_lead(
  pool: pog.Connection,
  telegram_user_id: Int,
  username: Option(String),
  first_name: Option(String),
  first_message: Option(String),
  source: Option(String),
) -> Result(Lead, SalesDbError) {
  let sql =
    "INSERT INTO leads (telegram_user_id, username, first_name, first_message, source)
     VALUES ($1, $2, $3, $4, $5)
     RETURNING id, telegram_user_id, username, first_name, last_name, phone, email,
               first_message, intent, priority, status, funnel_stage, source,
               utm_source, utm_medium, utm_campaign, quiz_score, recommended_product_id,
               assigned_to, notes, last_contact_at::text, created_at::text"

  case
    pog.query(sql)
    |> add_parameters([
      pog.int(telegram_user_id),
      pog.nullable(pog.text, username),
      pog.nullable(pog.text, first_name),
      pog.nullable(pog.text, first_message),
      pog.nullable(pog.text, source),
    ])
    |> pog.returning(decode_lead())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [lead])) -> Ok(lead)
    Ok(_) -> Error(SalesDbQueryError("Failed to create lead"))
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// Получить лида по ID (внутренний идентификатор БД)
pub fn get_lead_by_id(
  pool: pog.Connection,
  lead_id: Int,
) -> Result(Lead, SalesDbError) {
  let sql =
    "SELECT id, telegram_user_id, username, first_name, last_name, phone, email,
            first_message, intent, priority, status, funnel_stage, source,
            utm_source, utm_medium, utm_campaign, quiz_score, recommended_product_id,
            assigned_to, notes, last_contact_at::text, created_at::text
     FROM leads
     WHERE id = $1"

  case
    pog.query(sql)
    |> add_parameters([pog.int(lead_id)])
    |> pog.returning(decode_lead())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [lead])) -> Ok(lead)
    Ok(pog.Returned(_, [])) -> Error(SalesDbNotFound)
    Ok(_) -> Error(SalesDbNotFound)
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// Получить последние лиды
pub fn get_recent_leads(
  pool: pog.Connection,
  limit: Int,
) -> Result(List(Lead), SalesDbError) {
  let sql =
    "SELECT id, telegram_user_id, username, first_name, last_name, phone, email,
            first_message, intent, priority, status, funnel_stage, source,
            utm_source, utm_medium, utm_campaign, quiz_score, recommended_product_id,
            assigned_to, notes, last_contact_at::text, created_at::text
     FROM leads
     ORDER BY created_at DESC
     LIMIT $1"

  case
    pog.query(sql)
    |> add_parameters([pog.int(limit)])
    |> pog.returning(decode_lead())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, leads)) -> Ok(leads)
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// Получить лида по telegram_user_id
pub fn get_lead_by_telegram_id(
  pool: pog.Connection,
  telegram_user_id: Int,
) -> Result(Lead, SalesDbError) {
  let sql =
    "SELECT id, telegram_user_id, username, first_name, last_name, phone, email,
            first_message, intent, priority, status, funnel_stage, source,
            utm_source, utm_medium, utm_campaign, quiz_score, recommended_product_id,
            assigned_to, notes, last_contact_at::text, created_at::text
     FROM leads
     WHERE telegram_user_id = $1
     ORDER BY created_at DESC LIMIT 1"

  case
    pog.query(sql)
    |> add_parameters([pog.int(telegram_user_id)])
    |> pog.returning(decode_lead())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [lead])) -> Ok(lead)
    Ok(pog.Returned(_, [])) -> Error(SalesDbNotFound)
    Ok(_) -> Error(SalesDbNotFound)
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// Получить или создать лида
pub fn get_or_create_lead(
  pool: pog.Connection,
  telegram_user_id: Int,
  username: Option(String),
  first_name: Option(String),
  first_message: Option(String),
  source: Option(String),
) -> Result(Lead, SalesDbError) {
  // Используем UPSERT
  let sql =
    "INSERT INTO leads (telegram_user_id, username, first_name, first_message, source)
     VALUES ($1, $2, $3, $4, $5)
     ON CONFLICT (telegram_user_id) DO UPDATE SET
       username = COALESCE($2, leads.username),
       first_name = COALESCE($3, leads.first_name),
       last_contact_at = NOW()
     RETURNING id, telegram_user_id, username, first_name, last_name, phone, email,
               first_message, intent, priority, status, funnel_stage, source,
               utm_source, utm_medium, utm_campaign, quiz_score, recommended_product_id,
               assigned_to, notes, last_contact_at::text, created_at::text"

  case
    pog.query(sql)
    |> add_parameters([
      pog.int(telegram_user_id),
      pog.nullable(pog.text, username),
      pog.nullable(pog.text, first_name),
      pog.nullable(pog.text, first_message),
      pog.nullable(pog.text, source),
    ])
    |> pog.returning(decode_lead())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [lead])) -> Ok(lead)
    Ok(_) -> Error(SalesDbQueryError("Failed to get or create lead"))
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// Обновить статус лида
pub fn update_lead_status(
  pool: pog.Connection,
  lead_id: Int,
  status: LeadStatus,
) -> Result(Lead, SalesDbError) {
  let sql =
    "UPDATE leads SET status = $2
     WHERE id = $1
     RETURNING id, telegram_user_id, username, first_name, last_name, phone, email,
               first_message, intent, priority, status, funnel_stage, source,
               utm_source, utm_medium, utm_campaign, quiz_score, recommended_product_id,
               assigned_to, notes, last_contact_at::text, created_at::text"

  case
    pog.query(sql)
    |> add_parameters([pog.int(lead_id), pog.text(lead_status_to_string(status))])
    |> pog.returning(decode_lead())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [lead])) -> Ok(lead)
    Ok(pog.Returned(_, [])) -> Error(SalesDbNotFound)
    Ok(_) -> Error(SalesDbNotFound)
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// Обновить этап воронки
pub fn update_funnel_stage(
  pool: pog.Connection,
  lead_id: Int,
  stage: FunnelStage,
) -> Result(Lead, SalesDbError) {
  let sql =
    "UPDATE leads SET funnel_stage = $2
     WHERE id = $1
     RETURNING id, telegram_user_id, username, first_name, last_name, phone, email,
               first_message, intent, priority, status, funnel_stage, source,
               utm_source, utm_medium, utm_campaign, quiz_score, recommended_product_id,
               assigned_to, notes, last_contact_at::text, created_at::text"

  case
    pog.query(sql)
    |> add_parameters([pog.int(lead_id), pog.text(funnel_stage_to_string(stage))])
    |> pog.returning(decode_lead())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [lead])) -> Ok(lead)
    Ok(pog.Returned(_, [])) -> Error(SalesDbNotFound)
    Ok(_) -> Error(SalesDbNotFound)
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// Обновить результат квиза
pub fn update_quiz_result(
  pool: pog.Connection,
  lead_id: Int,
  score: Int,
  product_id: Int,
) -> Result(Lead, SalesDbError) {
  let sql =
    "UPDATE leads SET quiz_score = $2, recommended_product_id = $3
     WHERE id = $1
     RETURNING id, telegram_user_id, username, first_name, last_name, phone, email,
               first_message, intent, priority, status, funnel_stage, source,
               utm_source, utm_medium, utm_campaign, quiz_score, recommended_product_id,
               assigned_to, notes, last_contact_at::text, created_at::text"

  case
    pog.query(sql)
    |> add_parameters([pog.int(lead_id), pog.int(score), pog.int(product_id)])
    |> pog.returning(decode_lead())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [lead])) -> Ok(lead)
    Ok(pog.Returned(_, [])) -> Error(SalesDbNotFound)
    Ok(_) -> Error(SalesDbNotFound)
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// История статусов лида
pub type LeadStatusChange {
  LeadStatusChange(
    id: Int,
    lead_id: Int,
    old_status: Option(String),
    new_status: String,
    old_funnel_stage: Option(String),
    new_funnel_stage: Option(String),
    changed_by: Option(String),
    change_reason: Option(String),
    created_at: Option(String),
  )
}

/// Получить историю статусов лида
pub fn get_lead_status_history(
  pool: pog.Connection,
  telegram_user_id: Int,
) -> Result(List(LeadStatusChange), SalesDbError) {
  let sql =
    "SELECT id, lead_id, old_status, new_status, old_funnel_stage, new_funnel_stage,
            changed_by, change_reason, created_at::text
     FROM lead_status_history
     WHERE telegram_user_id = $1
     ORDER BY created_at DESC
     LIMIT 20"

  case
    pog.query(sql)
    |> add_parameters([pog.int(telegram_user_id)])
    |> pog.returning(decode_status_change())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, changes)) -> Ok(changes)
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// Обновить статус лида по telegram_user_id
pub fn update_lead_status_by_telegram_id(
  pool: pog.Connection,
  telegram_user_id: Int,
  status: LeadStatus,
) -> Result(Lead, SalesDbError) {
  let sql =
    "UPDATE leads SET status = $2
     WHERE telegram_user_id = $1
     RETURNING id, telegram_user_id, username, first_name, last_name, phone, email,
               first_message, intent, priority, status, funnel_stage, source,
               utm_source, utm_medium, utm_campaign, quiz_score, recommended_product_id,
               assigned_to, notes, last_contact_at::text, created_at::text"

  case
    pog.query(sql)
    |> add_parameters([pog.int(telegram_user_id), pog.text(lead_status_to_string(status))])
    |> pog.returning(decode_lead())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [lead])) -> Ok(lead)
    Ok(pog.Returned(_, [])) -> Error(SalesDbNotFound)
    Ok(_) -> Error(SalesDbNotFound)
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// Обновить этап воронки по telegram_user_id
pub fn update_funnel_stage_by_telegram_id(
  pool: pog.Connection,
  telegram_user_id: Int,
  stage: FunnelStage,
) -> Result(Lead, SalesDbError) {
  let sql =
    "UPDATE leads SET funnel_stage = $2
     WHERE telegram_user_id = $1
     RETURNING id, telegram_user_id, username, first_name, last_name, phone, email,
               first_message, intent, priority, status, funnel_stage, source,
               utm_source, utm_medium, utm_campaign, quiz_score, recommended_product_id,
               assigned_to, notes, last_contact_at::text, created_at::text"

  case
    pog.query(sql)
    |> add_parameters([pog.int(telegram_user_id), pog.text(funnel_stage_to_string(stage))])
    |> pog.returning(decode_lead())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [lead])) -> Ok(lead)
    Ok(pog.Returned(_, [])) -> Error(SalesDbNotFound)
    Ok(_) -> Error(SalesDbNotFound)
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// Статистика воронки
pub fn get_funnel_stats(pool: pog.Connection) -> Result(FunnelStats, SalesDbError) {
  let sql =
    "SELECT
       COUNT(*) FILTER (WHERE funnel_stage = 'awareness') as awareness,
       COUNT(*) FILTER (WHERE funnel_stage = 'interest') as interest,
       COUNT(*) FILTER (WHERE funnel_stage = 'consideration') as consideration,
       COUNT(*) FILTER (WHERE funnel_stage = 'intent') as intent,
       COUNT(*) FILTER (WHERE funnel_stage = 'evaluation') as evaluation,
       COUNT(*) FILTER (WHERE funnel_stage = 'purchase') as purchase
     FROM leads"

  case
    pog.query(sql)
    |> pog.returning(decode_funnel_stats())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [stats])) -> Ok(stats)
    Ok(_) -> Ok(FunnelStats(0, 0, 0, 0, 0, 0))
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Usage Logging
// =============================================================================

/// Записать использование
pub fn log_usage(
  pool: pog.Connection,
  telegram_id: Int,
  subscription_id: Option(Int),
  action_type: String,
  action_details: Option(String),
  tokens_used: Int,
  cost_cents: Int,
) -> Result(Nil, SalesDbError) {
  let sql =
    "INSERT INTO usage_logs (telegram_id, subscription_id, action_type, action_details, tokens_used, cost_cents)
     VALUES ($1, $2, $3, $4, $5, $6)"

  case
    pog.query(sql)
    |> add_parameters([
      pog.int(telegram_id),
      pog.nullable(pog.int, subscription_id),
      pog.text(action_type),
      pog.nullable(pog.text, action_details),
      pog.int(tokens_used),
      pog.int(cost_cents),
    ])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

/// Получить количество использований за период
pub fn get_usage_count(
  pool: pog.Connection,
  telegram_id: Int,
  action_type: String,
  since_days: Int,
) -> Result(Int, SalesDbError) {
  let sql =
    "SELECT COUNT(*)
     FROM usage_logs
     WHERE telegram_id = $1 AND action_type = $2
       AND created_at > NOW() - INTERVAL '1 day' * $3"

  let int_decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(count)
  }

  case
    pog.query(sql)
    |> add_parameters([pog.int(telegram_id), pog.text(action_type), pog.int(since_days)])
    |> pog.returning(int_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [count])) -> Ok(count)
    Ok(_) -> Ok(0)
    Error(e) -> Error(SalesDbQueryError(pog_error_to_string(e)))
  }
}

// =============================================================================
// Helpers
// =============================================================================

fn add_parameters(query: pog.Query(t), params: List(pog.Value)) -> pog.Query(t) {
  list.fold(params, query, fn(q, p) { pog.parameter(q, p) })
}

fn pog_error_to_string(err: pog.QueryError) -> String {
  case err {
    pog.ConnectionUnavailable -> "Connection unavailable"
    pog.ConstraintViolated(msg, constraint, _) ->
      "Constraint violated: " <> msg <> " (" <> constraint <> ")"
    pog.PostgresqlError(code, name, msg) ->
      "PostgreSQL error " <> code <> " [" <> name <> "]: " <> msg
    pog.UnexpectedArgumentCount(expected, got) ->
      "Unexpected argument count: expected "
      <> int.to_string(expected)
      <> ", got "
      <> int.to_string(got)
    pog.UnexpectedArgumentType(expected, got) ->
      "Unexpected argument type: expected " <> expected <> ", got " <> got
    pog.UnexpectedResultType(_) -> "Unexpected result type"
    pog.QueryTimeout -> "Query timeout"
  }
}

// =============================================================================
// Decoders
// =============================================================================

fn decode_subscription() -> Decoder(Subscription) {
  use id <- decode.field(0, decode.int)
  use telegram_id <- decode.field(1, decode.int)
  use product_id <- decode.field(2, decode.int)
  use status_str <- decode.field(3, decode.string)
  use payment_method_str <- decode.field(4, decode.optional(decode.string))
  use payment_id <- decode.field(5, decode.optional(decode.string))
  use period_start <- decode.field(6, decode.optional(decode.string))
  use period_end <- decode.field(7, decode.optional(decode.string))
  use generations_used <- decode.field(8, decode.int)
  use cancel_at_period_end <- decode.field(9, decode.bool)
  use cancelled_at <- decode.field(10, decode.optional(decode.string))

  let status = case subscription_status_from_string(status_str) {
    Ok(s) -> s
    Error(_) -> SubPending
  }

  let payment_method = case payment_method_str {
    Some(pm) -> case payment_method_from_string(pm) {
      Ok(m) -> Some(m)
      Error(_) -> None
    }
    None -> None
  }

  decode.success(Subscription(
    id: Some(id),
    telegram_id: telegram_id,
    product_id: product_id,
    status: status,
    payment_method: payment_method,
    payment_id: payment_id,
    current_period_start: period_start,
    current_period_end: period_end,
    generations_used: generations_used,
    cancel_at_period_end: cancel_at_period_end,
    cancelled_at: cancelled_at,
  ))
}

fn decode_lead() -> Decoder(Lead) {
  use id <- decode.field(0, decode.int)
  use telegram_user_id <- decode.field(1, decode.int)
  use username <- decode.field(2, decode.optional(decode.string))
  use first_name <- decode.field(3, decode.optional(decode.string))
  use last_name <- decode.field(4, decode.optional(decode.string))
  use phone <- decode.field(5, decode.optional(decode.string))
  use email <- decode.field(6, decode.optional(decode.string))
  use first_message <- decode.field(7, decode.optional(decode.string))
  use intent_str <- decode.field(8, decode.optional(decode.string))
  use priority_str <- decode.field(9, decode.string)
  use status_str <- decode.field(10, decode.string)
  use funnel_str <- decode.field(11, decode.string)
  use source <- decode.field(12, decode.optional(decode.string))
  use utm_source <- decode.field(13, decode.optional(decode.string))
  use utm_medium <- decode.field(14, decode.optional(decode.string))
  use utm_campaign <- decode.field(15, decode.optional(decode.string))
  use quiz_score <- decode.field(16, decode.optional(decode.int))
  use recommended_product_id <- decode.field(17, decode.optional(decode.int))
  use assigned_to <- decode.field(18, decode.optional(decode.string))
  use notes <- decode.field(19, decode.optional(decode.string))
  use last_contact_at <- decode.field(20, decode.optional(decode.string))
  use created_at <- decode.field(21, decode.optional(decode.string))

  let intent = case intent_str {
    Some(i) -> case lead_intent_from_string(i) {
      Ok(intent) -> Some(intent)
      Error(_) -> None
    }
    None -> None
  }

  let priority = case lead_priority_from_string(priority_str) {
    Ok(p) -> p
    Error(_) -> PriorityMedium
  }

  let status = case lead_status_from_string(status_str) {
    Ok(s) -> s
    Error(_) -> StatusNew
  }

  let funnel_stage = case funnel_stage_from_string(funnel_str) {
    Ok(f) -> f
    Error(_) -> StageAwareness
  }

  decode.success(Lead(
    id: Some(id),
    telegram_user_id: telegram_user_id,
    username: username,
    first_name: first_name,
    last_name: last_name,
    phone: phone,
    email: email,
    first_message: first_message,
    intent: intent,
    priority: priority,
    status: status,
    funnel_stage: funnel_stage,
    source: source,
    utm_source: utm_source,
    utm_medium: utm_medium,
    utm_campaign: utm_campaign,
    quiz_score: quiz_score,
    recommended_product_id: recommended_product_id,
    assigned_to: assigned_to,
    notes: notes,
    last_contact_at: last_contact_at,
    created_at: created_at,
  ))
}

fn decode_funnel_stats() -> Decoder(FunnelStats) {
  use awareness <- decode.field(0, decode.int)
  use interest <- decode.field(1, decode.int)
  use consideration <- decode.field(2, decode.int)
  use intent <- decode.field(3, decode.int)
  use evaluation <- decode.field(4, decode.int)
  use purchase <- decode.field(5, decode.int)

  decode.success(FunnelStats(
    awareness: awareness,
    interest: interest,
    consideration: consideration,
    intent: intent,
    evaluation: evaluation,
    purchase: purchase,
  ))
}

fn decode_status_change() -> Decoder(LeadStatusChange) {
  use id <- decode.field(0, decode.int)
  use lead_id <- decode.field(1, decode.int)
  use old_status <- decode.field(2, decode.optional(decode.string))
  use new_status <- decode.field(3, decode.string)
  use old_funnel_stage <- decode.field(4, decode.optional(decode.string))
  use new_funnel_stage <- decode.field(5, decode.optional(decode.string))
  use changed_by <- decode.field(6, decode.optional(decode.string))
  use change_reason <- decode.field(7, decode.optional(decode.string))
  use created_at <- decode.field(8, decode.optional(decode.string))

  decode.success(LeadStatusChange(
    id: id,
    lead_id: lead_id,
    old_status: old_status,
    new_status: new_status,
    old_funnel_stage: old_funnel_stage,
    new_funnel_stage: new_funnel_stage,
    changed_by: changed_by,
    change_reason: change_reason,
    created_at: created_at,
  ))
}
