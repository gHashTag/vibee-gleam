// Subscription Scene - Управление подпиской в Telegram боте
// /subscribe, /mystatus команды

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/bot/scene.{
  type UserSession, Subscription, SubscriptionStatus, SubscriptionUpgrade,
  SubscriptionCancel, SubscriptionPayment, Pricing, PricingList, Quiz, QuizStart,
  set_scene,
}
import vibee/bot/scene_types
import vibee/sales/subscription
import vibee/sales/product_catalog
import vibee/sales/types as sales_types

// =============================================================================
// Scene Entry
// =============================================================================

/// Обработать команду /subscribe или /mystatus
pub fn handle_status_command(session: UserSession) -> scene_types.SceneResult {
  let new_session = set_scene(session, Subscription(SubscriptionStatus))

  case subscription.get_active_subscription(session.user_id) {
    Error(subscription.NotFoundError(_)) -> {
      // Нет подписки
      let message = "❌ *У вас нет активной подписки*\n\n"
        <> "Оформите подписку чтобы получить доступ к AI-генерациям!\n\n"
        <> "🎯 Пройдите квиз для подбора тарифа или выберите из списка."
      let buttons = no_subscription_buttons()
      scene_types.send_buttons(new_session, message, buttons)
    }
    Error(e) -> scene_types.reply(
      new_session,
      "Ошибка: " <> subscription.error_to_string(e),
    )
    Ok(sub) -> {
      // Есть активная подписка
      case product_catalog.get_product_by_id(sub.product_id) {
        Error(_) -> scene_types.reply(new_session, "Ошибка загрузки продукта")
        Ok(product) -> {
          let message = format_subscription_status(sub, product)
          let buttons = subscription_buttons(sub, product)
          scene_types.send_buttons(new_session, message, buttons)
        }
      }
    }
  }
}

// =============================================================================
// Callback Handlers
// =============================================================================

/// Обработать callback
pub fn handle_callback(
  session: UserSession,
  callback_data: String,
) -> scene_types.SceneResult {
  case string.split(callback_data, ":") {
    ["sub", "upgrade"] -> show_upgrade_options(session)
    ["sub", "cancel"] -> confirm_cancel(session)
    ["sub", "cancel_confirm"] -> process_cancel(session)
    ["sub", "cancel_abort"] -> handle_status_command(session)
    ["sub", "pay", code, method] -> process_payment(session, code, method)
    ["sub", "select_method", code] -> show_payment_methods(session, code)
    ["sub", "pricing"] -> go_to_pricing(session)
    ["sub", "quiz"] -> go_to_quiz(session)
    _ -> scene_types.reply(session, "Неизвестная команда")
  }
}

/// Показать опции апгрейда
fn show_upgrade_options(session: UserSession) -> scene_types.SceneResult {
  let new_session = set_scene(session, Subscription(SubscriptionUpgrade))

  case product_catalog.get_all_products() {
    Error(e) -> scene_types.reply(
      new_session,
      "Ошибка: " <> product_catalog.error_to_string(e),
    )
    Ok(products) -> {
      let message = "🚀 *Апгрейд подписки*\n\nВыберите новый тариф:"
      let buttons = products
        |> list.map(fn(p) {
          [#(p.name_ru <> " " <> sales_types.format_price_usd(p.price_usd_cents),
             "sub:select_method:" <> p.code)]
        })
        |> list.append([[#("◀️ Назад", "sub:status")]])
      scene_types.send_buttons(new_session, message, buttons)
    }
  }
}

/// Подтвердить отмену
fn confirm_cancel(session: UserSession) -> scene_types.SceneResult {
  let new_session = set_scene(session, Subscription(SubscriptionCancel))

  let message = "⚠️ *Отмена подписки*\n\n"
    <> "Вы уверены, что хотите отменить подписку?\n\n"
    <> "Подписка будет активна до конца оплаченного периода."

  let buttons = [
    [#("❌ Да, отменить", "sub:cancel_confirm")],
    [#("◀️ Нет, вернуться", "sub:cancel_abort")],
  ]
  scene_types.send_buttons(new_session, message, buttons)
}

/// Обработать отмену
fn process_cancel(session: UserSession) -> scene_types.SceneResult {
  case subscription.get_active_subscription(session.user_id) {
    Error(_) -> scene_types.reply(session, "Подписка не найдена")
    Ok(sub) -> {
      case sub.id {
        None -> scene_types.reply(session, "Ошибка: ID подписки не найден")
        Some(sub_id) -> {
          case subscription.cancel_subscription(sub_id) {
            Error(e) -> scene_types.reply(
              session,
              "Ошибка отмены: " <> subscription.error_to_string(e),
            )
            Ok(_) -> {
              let new_session = set_scene(session, Subscription(SubscriptionStatus))
              scene_types.reply(
                new_session,
                "✅ Подписка отменена. Она будет активна до конца оплаченного периода.",
              )
            }
          }
        }
      }
    }
  }
}

/// Показать методы оплаты
fn show_payment_methods(session: UserSession, code: String) -> scene_types.SceneResult {
  let new_session = set_scene(session, Subscription(SubscriptionPayment(code, "")))

  let message = "💳 *Выберите способ оплаты*\n\nДля тарифа: " <> code

  let buttons = [
    [#("💳 Карта (Robokassa)", "sub:pay:" <> code <> ":robokassa")],
    [#("💎 TON", "sub:pay:" <> code <> ":ton")],
    [#("⭐ Telegram Stars", "sub:pay:" <> code <> ":stars")],
    [#("🤖 CryptoBot", "sub:pay:" <> code <> ":cryptobot")],
    [#("◀️ Назад", "sub:upgrade")],
  ]
  scene_types.send_buttons(new_session, message, buttons)
}

/// Обработать оплату
fn process_payment(session: UserSession, code: String, method: String) -> scene_types.SceneResult {
  let new_session = set_scene(session, Subscription(SubscriptionPayment(code, method)))

  case product_catalog.get_product_by_code(code) {
    Error(_) -> scene_types.reply(new_session, "Продукт не найден")
    Ok(product) -> {
      let payment_method = case sales_types.payment_method_from_string(method) {
        Ok(pm) -> pm
        Error(_) -> sales_types.Robokassa
      }

      // Создать подписку (pending)
      case subscription.create_subscription(
        session.user_id,
        case product.id { Some(id) -> id _ -> 1 },
        payment_method,
        "pending_" <> code <> "_" <> method,
      ) {
        Error(e) -> scene_types.reply(
          new_session,
          "Ошибка создания подписки: " <> subscription.error_to_string(e),
        )
        Ok(sub) -> {
          let message = "💳 *Оплата*\n\n"
            <> "Тариф: *" <> product.name_ru <> "*\n"
            <> "Сумма: *" <> sales_types.format_price_usd(product.price_usd_cents) <> "*\n"
            <> "Метод: " <> method <> "\n\n"
            <> "⏳ Ожидаем подтверждение оплаты...\n\n"
            <> "_В демо-режиме подписка активируется автоматически._"

          // В реальности здесь создаётся invoice и отправляется ссылка
          // Для демо активируем сразу
          case sub.id {
            Some(sub_id) -> {
              let _ = subscription.activate_subscription(sub_id)
              Nil
            }
            None -> Nil
          }

          scene_types.reply(new_session, message <> "\n\n✅ Подписка активирована!")
        }
      }
    }
  }
}

/// Перейти к ценам
fn go_to_pricing(session: UserSession) -> scene_types.SceneResult {
  let new_session = set_scene(session, Pricing(PricingList))
  scene_types.reply(new_session, "Переходим к тарифам...")
}

/// Перейти к квизу
fn go_to_quiz(session: UserSession) -> scene_types.SceneResult {
  let new_session = set_scene(session, Quiz(QuizStart))
  scene_types.reply(new_session, "Переходим к квизу...")
}

// =============================================================================
// Formatters
// =============================================================================

fn format_subscription_status(
  sub: sales_types.Subscription,
  product: sales_types.Product,
) -> String {
  let status_emoji = case sub.status {
    sales_types.SubActive -> "✅"
    sales_types.SubPending -> "⏳"
    sales_types.SubCancelled -> "❌"
    sales_types.SubExpired -> "⏰"
    sales_types.SubPaused -> "⏸️"
  }

  let limit_text = case product.generation_limit {
    Some(limit) ->
      int.to_string(sub.generations_used) <> " / " <> int.to_string(limit)
    None -> int.to_string(sub.generations_used) <> " (безлимит)"
  }

  status_emoji <> " *Ваша подписка*\n\n"
  <> "📦 Тариф: *" <> product.name_ru <> "*\n"
  <> "💰 Цена: " <> sales_types.format_price_usd(product.price_usd_cents) <> "/мес\n"
  <> "📊 Использовано: " <> limit_text <> "\n"
  <> "📅 Активна до: " <> case sub.current_period_end {
    Some(date) -> date
    None -> "—"
  } <> "\n"
  <> case sub.cancel_at_period_end {
    True -> "\n⚠️ Отмена в конце периода"
    False -> ""
  }
}

// =============================================================================
// Buttons
// =============================================================================

fn no_subscription_buttons() -> List(List(#(String, String))) {
  [
    [#("🎯 Пройти квиз", "sub:quiz")],
    [#("📊 Все тарифы", "sub:pricing")],
  ]
}

fn subscription_buttons(
  sub: sales_types.Subscription,
  product: sales_types.Product,
) -> List(List(#(String, String))) {
  case sub.status {
    sales_types.SubActive -> [
      [#("🚀 Апгрейд", "sub:upgrade")],
      [#("❌ Отменить", "sub:cancel")],
    ]
    _ -> [
      [#("📊 Тарифы", "sub:pricing")],
    ]
  }
}
