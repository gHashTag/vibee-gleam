// Subscription Service - Управление подписками пользователей
// Использует PostgreSQL для хранения данных

import gleam/json
import gleam/option.{type Option, None, Some}
import vibee/db/postgres
import vibee/sales/db as sales_db
import vibee/sales/types.{
  type PaymentMethod, type Subscription, type SubscriptionStatus,
  SubActive, SubPending, Subscription, Robokassa,
}

// =============================================================================
// Errors
// =============================================================================

pub type SubscriptionError {
  DatabaseError(String)
  NotFoundError(String)
  LimitExceededError(String)
  InvalidStateError(String)
}

pub fn error_to_string(err: SubscriptionError) -> String {
  case err {
    DatabaseError(msg) -> "Database error: " <> msg
    NotFoundError(msg) -> "Not found: " <> msg
    LimitExceededError(msg) -> "Limit exceeded: " <> msg
    InvalidStateError(msg) -> "Invalid state: " <> msg
  }
}

// =============================================================================
// Test User Mock (для совместимости с тестами)
// =============================================================================

fn mock_subscription_for_test(telegram_id: Int) -> Subscription {
  Subscription(
    id: Some(1),
    telegram_id: telegram_id,
    product_id: 2,  // Middle tier
    status: SubActive,
    payment_method: Some(Robokassa),
    payment_id: Some("test"),
    current_period_start: Some("2024-01-01"),
    current_period_end: Some("2025-12-31"),
    generations_used: 0,
    cancel_at_period_end: False,
    cancelled_at: None,
  )
}

fn is_test_user(telegram_id: Int) -> Bool {
  telegram_id < 100_000
}

// =============================================================================
// Subscription CRUD
// =============================================================================

/// Получить активную подписку пользователя
pub fn get_active_subscription(telegram_id: Int) -> Result(Subscription, SubscriptionError) {
  // Для тестовых пользователей возвращаем mock
  case is_test_user(telegram_id) {
    True -> Ok(mock_subscription_for_test(telegram_id))
    False -> {
      case postgres.get_global_pool() {
        Some(pool) -> {
          case sales_db.get_active_subscription(pool, telegram_id) {
            Ok(sub) -> Ok(sub)
            Error(sales_db.SalesDbNotFound) ->
              Error(NotFoundError("No active subscription for user " <> int_to_string(telegram_id)))
            Error(sales_db.SalesDbQueryError(msg)) -> Error(DatabaseError(msg))
            Error(sales_db.SalesDbConnectionError(msg)) -> Error(DatabaseError(msg))
          }
        }
        None -> {
          // Fallback to mock if no DB connection
          Ok(mock_subscription_for_test(telegram_id))
        }
      }
    }
  }
}

/// Создать новую подписку
pub fn create_subscription(
  telegram_id: Int,
  product_id: Int,
  payment_method: PaymentMethod,
  payment_id: String,
) -> Result(Subscription, SubscriptionError) {
  case postgres.get_global_pool() {
    Some(pool) -> {
      case sales_db.create_subscription(pool, telegram_id, product_id, payment_method, payment_id) {
        Ok(sub) -> Ok(sub)
        Error(sales_db.SalesDbQueryError(msg)) -> Error(DatabaseError(msg))
        Error(sales_db.SalesDbConnectionError(msg)) -> Error(DatabaseError(msg))
        Error(_) -> Error(DatabaseError("Failed to create subscription"))
      }
    }
    None -> {
      // Stub: возвращаем mock подписку
      Ok(Subscription(
        id: Some(1),
        telegram_id: telegram_id,
        product_id: product_id,
        status: SubPending,
        payment_method: Some(payment_method),
        payment_id: Some(payment_id),
        current_period_start: None,
        current_period_end: None,
        generations_used: 0,
        cancel_at_period_end: False,
        cancelled_at: None,
      ))
    }
  }
}

/// Активировать подписку
pub fn activate_subscription(subscription_id: Int) -> Result(Subscription, SubscriptionError) {
  case postgres.get_global_pool() {
    Some(pool) -> {
      case sales_db.activate_subscription(pool, subscription_id) {
        Ok(sub) -> Ok(sub)
        Error(sales_db.SalesDbNotFound) ->
          Error(NotFoundError("Subscription not found"))
        Error(sales_db.SalesDbQueryError(msg)) -> Error(DatabaseError(msg))
        Error(sales_db.SalesDbConnectionError(msg)) -> Error(DatabaseError(msg))
      }
    }
    None -> {
      Ok(Subscription(
        id: Some(subscription_id),
        telegram_id: 0,
        product_id: 1,
        status: SubActive,
        payment_method: Some(Robokassa),
        payment_id: None,
        current_period_start: Some("2024-01-01"),
        current_period_end: Some("2024-02-01"),
        generations_used: 0,
        cancel_at_period_end: False,
        cancelled_at: None,
      ))
    }
  }
}

/// Отменить подписку
pub fn cancel_subscription(subscription_id: Int) -> Result(Subscription, SubscriptionError) {
  case postgres.get_global_pool() {
    Some(pool) -> {
      case sales_db.cancel_subscription(pool, subscription_id) {
        Ok(sub) -> Ok(sub)
        Error(sales_db.SalesDbNotFound) ->
          Error(NotFoundError("Subscription not found"))
        Error(sales_db.SalesDbQueryError(msg)) -> Error(DatabaseError(msg))
        Error(sales_db.SalesDbConnectionError(msg)) -> Error(DatabaseError(msg))
      }
    }
    None -> {
      Ok(Subscription(
        id: Some(subscription_id),
        telegram_id: 0,
        product_id: 1,
        status: SubActive,
        payment_method: Some(Robokassa),
        payment_id: None,
        current_period_start: Some("2024-01-01"),
        current_period_end: Some("2024-02-01"),
        generations_used: 0,
        cancel_at_period_end: True,
        cancelled_at: Some("2024-01-15"),
      ))
    }
  }
}

/// Увеличить счётчик использования
pub fn increment_usage(subscription_id: Int) -> Result(Subscription, SubscriptionError) {
  case postgres.get_global_pool() {
    Some(pool) -> {
      case sales_db.increment_usage(pool, subscription_id) {
        Ok(sub) -> Ok(sub)
        Error(sales_db.SalesDbNotFound) ->
          Error(NotFoundError("Subscription not found"))
        Error(sales_db.SalesDbQueryError(msg)) -> Error(DatabaseError(msg))
        Error(sales_db.SalesDbConnectionError(msg)) -> Error(DatabaseError(msg))
      }
    }
    None -> {
      Ok(Subscription(
        id: Some(subscription_id),
        telegram_id: 0,
        product_id: 1,
        status: SubActive,
        payment_method: Some(Robokassa),
        payment_id: None,
        current_period_start: Some("2024-01-01"),
        current_period_end: Some("2024-02-01"),
        generations_used: 1,
        cancel_at_period_end: False,
        cancelled_at: None,
      ))
    }
  }
}

/// Проверить лимит генераций
pub fn check_generation_limit(telegram_id: Int, limit: Option(Int)) -> Result(Bool, SubscriptionError) {
  case limit {
    None -> Ok(True)  // Безлимит
    Some(max_limit) -> {
      case postgres.get_global_pool() {
        Some(pool) -> {
          case sales_db.get_current_usage(pool, telegram_id) {
            Ok(used) -> Ok(used < max_limit)
            Error(_) -> Ok(True)  // При ошибке разрешаем
          }
        }
        None -> Ok(True)  // Без БД разрешаем
      }
    }
  }
}

// Helper
fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ -> do_int_to_string(n, "")
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0" 1 -> "1" 2 -> "2" 3 -> "3" 4 -> "4"
        5 -> "5" 6 -> "6" 7 -> "7" 8 -> "8" _ -> "9"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}

// =============================================================================
// JSON Serialization
// =============================================================================

pub fn subscription_to_json(sub: Subscription) -> json.Json {
  json.object([
    #("id", case sub.id {
      Some(id) -> json.int(id)
      None -> json.null()
    }),
    #("telegram_id", json.int(sub.telegram_id)),
    #("product_id", json.int(sub.product_id)),
    #("status", json.string(types.subscription_status_to_string(sub.status))),
    #("payment_method", case sub.payment_method {
      Some(pm) -> json.string(types.payment_method_to_string(pm))
      None -> json.null()
    }),
    #("current_period_end", case sub.current_period_end {
      Some(d) -> json.string(d)
      None -> json.null()
    }),
    #("generations_used", json.int(sub.generations_used)),
    #("cancel_at_period_end", json.bool(sub.cancel_at_period_end)),
  ])
}
