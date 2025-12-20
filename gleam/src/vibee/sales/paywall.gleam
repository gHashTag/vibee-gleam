// Paywall - Контроль доступа к функциям по подписке
// Проверяет права пользователя перед выполнением действий

import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import vibee/sales/subscription
import vibee/sales/product_catalog

// =============================================================================
// Access Control
// =============================================================================

/// Результат проверки доступа
pub type AccessResult {
  /// Доступ разрешён
  AccessGranted(remaining: Option(Int))
  /// Требуется подписка
  SubscriptionRequired
  /// Лимит исчерпан
  LimitExceeded(used: Int, limit: Int)
  /// Функция недоступна на текущем тарифе
  FeatureNotAvailable(required_tier: String)
  /// Подписка истекла
  SubscriptionExpired
  /// Ошибка проверки
  AccessError(String)
}

/// Действие, требующее проверки
pub type ProtectedAction {
  /// Генерация контента (avatar, video, etc.)
  Generation
  /// Доступ к API
  ApiAccess
  /// Голосовой клон
  VoiceClone
  /// Мультиканальная публикация
  Multichannel
  /// Кастомное обучение
  CustomTraining
  /// White label
  WhiteLabel
}

/// Маппинг действий на требуемые features
fn action_to_feature(action: ProtectedAction) -> String {
  case action {
    Generation -> "basic_responses"  // базовый доступ есть у всех
    ApiAccess -> "api_access"
    VoiceClone -> "voice_clone"
    Multichannel -> "multichannel"
    CustomTraining -> "custom_training"
    WhiteLabel -> "white_label"
  }
}

/// Маппинг features на минимальный тариф
fn feature_to_min_tier(feature: String) -> String {
  case feature {
    "basic_responses" -> "junior"
    "telegram_bot" -> "junior"
    "email_support" -> "junior"
    "1_persona" -> "junior"
    "custom_persona" -> "middle"
    "crm_integration" -> "middle"
    "analytics" -> "middle"
    "priority_support" -> "middle"
    "3_personas" -> "middle"
    "voice_clone" -> "middle"
    "unlimited_generations" -> "senior"
    "multichannel" -> "senior"
    "api_access" -> "senior"
    "dedicated_support" -> "senior"
    "unlimited_personas" -> "senior"
    "white_label" -> "senior"
    "custom_training" -> "senior"
    _ -> "senior"
  }
}

// =============================================================================
// Access Checks
// =============================================================================

/// Проверить доступ к действию
pub fn check_access(
  telegram_id: Int,
  action: ProtectedAction,
) -> AccessResult {
  // Получить активную подписку
  case subscription.get_active_subscription(telegram_id) {
    Error(subscription.NotFoundError(_)) -> SubscriptionRequired
    Error(subscription.DatabaseError(msg)) -> AccessError(msg)
    Error(_) -> AccessError("Unknown error")
    Ok(sub) -> {
      // Проверить статус подписки
      case sub.status {
        subscription_types.SubExpired -> SubscriptionExpired
        subscription_types.SubCancelled -> SubscriptionRequired
        subscription_types.SubPaused -> SubscriptionRequired
        _ -> {
          // Проверить наличие feature
          let required_feature = action_to_feature(action)
          case check_feature(sub.product_id, required_feature) {
            Error(_) -> AccessError("Failed to check feature")
            Ok(False) -> FeatureNotAvailable(feature_to_min_tier(required_feature))
            Ok(True) -> {
              // Для генераций проверяем лимит
              case action {
                Generation -> check_generation_limit(telegram_id, sub)
                _ -> AccessGranted(remaining: None)
              }
            }
          }
        }
      }
    }
  }
}

/// Проверить лимит генераций
fn check_generation_limit(
  telegram_id: Int,
  sub: subscription_types.Subscription,
) -> AccessResult {
  case get_product_limit(sub.product_id) {
    Error(_) -> AccessGranted(remaining: None)
    Ok(None) -> AccessGranted(remaining: None)  // Unlimited
    Ok(Some(limit)) -> {
      case sub.generations_used < limit {
        True -> AccessGranted(remaining: Some(limit - sub.generations_used))
        False -> LimitExceeded(used: sub.generations_used, limit: limit)
      }
    }
  }
}

/// Проверить наличие feature у продукта
fn check_feature(product_id: Int, feature: String) -> Result(Bool, Nil) {
  case product_catalog.get_product_by_id(product_id) {
    Error(_) -> Error(Nil)
    Ok(product) -> Ok(list.contains(product.features, feature))
  }
}

/// Получить лимит генераций продукта
fn get_product_limit(product_id: Int) -> Result(Option(Int), Nil) {
  case product_catalog.get_product_by_id(product_id) {
    Error(_) -> Error(Nil)
    Ok(product) -> Ok(product.generation_limit)
  }
}

// =============================================================================
// Usage Tracking
// =============================================================================

/// Записать использование (после успешной генерации)
pub fn record_usage(_telegram_id: Int, action: ProtectedAction) -> Result(Int, String) {
  case action {
    Generation -> {
      // Stub: всегда возвращаем 1
      Ok(1)
    }
    _ -> Ok(0)  // Другие действия не считаем
  }
}

// =============================================================================
// Paywall Messages
// =============================================================================

/// Получить сообщение для пользователя
pub fn get_access_message(result: AccessResult, lang: String) -> String {
  case result {
    AccessGranted(remaining) -> {
      case remaining {
        Some(r) -> case lang {
          "en" -> "Access granted. Remaining: " <> int.to_string(r)
          _ -> "Доступ разрешён. Осталось: " <> int.to_string(r)
        }
        None -> case lang {
          "en" -> "Access granted (unlimited)"
          _ -> "Доступ разрешён (безлимит)"
        }
      }
    }

    SubscriptionRequired -> case lang {
      "en" -> "Subscription required. Use /pricing to see plans."
      _ -> "Требуется подписка. Используйте /pricing для просмотра тарифов."
    }

    LimitExceeded(used, limit) -> case lang {
      "en" -> "Limit exceeded: " <> int.to_string(used) <> "/" <> int.to_string(limit) <> ". Upgrade your plan."
      _ -> "Лимит исчерпан: " <> int.to_string(used) <> "/" <> int.to_string(limit) <> ". Обновите тариф."
    }

    FeatureNotAvailable(tier) -> case lang {
      "en" -> "This feature requires " <> tier <> " plan or higher."
      _ -> "Эта функция требует тариф " <> tier <> " или выше."
    }

    SubscriptionExpired -> case lang {
      "en" -> "Your subscription has expired. Renew to continue."
      _ -> "Ваша подписка истекла. Продлите для продолжения."
    }

    AccessError(msg) -> case lang {
      "en" -> "Access check error: " <> msg
      _ -> "Ошибка проверки доступа: " <> msg
    }
  }
}

/// Получить JSON с информацией о доступе
pub fn access_result_to_json(result: AccessResult) -> json.Json {
  case result {
    AccessGranted(remaining) -> json.object([
      #("granted", json.bool(True)),
      #("remaining", case remaining {
        Some(r) -> json.int(r)
        None -> json.string("unlimited")
      }),
    ])

    SubscriptionRequired -> json.object([
      #("granted", json.bool(False)),
      #("reason", json.string("subscription_required")),
    ])

    LimitExceeded(used, limit) -> json.object([
      #("granted", json.bool(False)),
      #("reason", json.string("limit_exceeded")),
      #("used", json.int(used)),
      #("limit", json.int(limit)),
    ])

    FeatureNotAvailable(tier) -> json.object([
      #("granted", json.bool(False)),
      #("reason", json.string("feature_not_available")),
      #("required_tier", json.string(tier)),
    ])

    SubscriptionExpired -> json.object([
      #("granted", json.bool(False)),
      #("reason", json.string("subscription_expired")),
    ])

    AccessError(msg) -> json.object([
      #("granted", json.bool(False)),
      #("reason", json.string("error")),
      #("message", json.string(msg)),
    ])
  }
}

// =============================================================================
// Decorator-style helper
// =============================================================================

/// Выполнить действие с проверкой доступа
pub fn with_access_check(
  telegram_id: Int,
  action: ProtectedAction,
  callback: fn() -> Result(a, String),
) -> Result(a, String) {
  case check_access(telegram_id, action) {
    AccessGranted(_) -> {
      case callback() {
        Ok(result) -> {
          // Записать использование
          let _ = record_usage(telegram_id, action)
          Ok(result)
        }
        Error(e) -> Error(e)
      }
    }
    other -> Error(get_access_message(other, "ru"))
  }
}

// Import subscription types
import vibee/sales/types as subscription_types
