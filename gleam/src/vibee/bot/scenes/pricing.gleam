// Pricing Scene - Отображение тарифов VIBEE в Telegram боте
// /pricing команда

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/bot/scene.{
  type UserSession, Pricing, PricingList, PricingDetails,
  PricingCompare, Quiz, QuizStart, Subscription, SubscriptionPayment,
  set_scene,
}
import vibee/bot/scene_types
import vibee/sales/product_catalog
import vibee/sales/types as sales_types

// =============================================================================
// Scene Entry
// =============================================================================

/// Обработать команду /pricing
pub fn handle_pricing_command(session: UserSession) -> scene_types.SceneResult {
  let new_session = set_scene(session, Pricing(PricingList))

  case product_catalog.get_all_products() {
    Error(e) -> scene_types.reply(
      new_session,
      "Ошибка загрузки тарифов: " <> product_catalog.error_to_string(e),
    )
    Ok(products) -> {
      let message = format_pricing_list(products)
      let buttons = pricing_buttons(products)
      scene_types.send_buttons(new_session, message, buttons)
    }
  }
}

// =============================================================================
// Message Handlers
// =============================================================================

/// Обработать callback от кнопки
pub fn handle_callback(
  session: UserSession,
  callback_data: String,
) -> scene_types.SceneResult {
  case string.split(callback_data, ":") {
    ["pricing", "details", code] -> show_product_details(session, code)
    ["pricing", "compare"] -> show_comparison(session)
    ["pricing", "quiz"] -> start_quiz_redirect(session)
    ["pricing", "subscribe", code] -> start_subscription(session, code)
    ["pricing", "back"] -> handle_pricing_command(session)
    _ -> scene_types.reply(session, "Неизвестная команда")
  }
}

/// Показать детали продукта
fn show_product_details(session: UserSession, code: String) -> scene_types.SceneResult {
  let new_session = set_scene(session, Pricing(PricingDetails(code)))

  case product_catalog.get_product_by_code(code) {
    Error(_) -> scene_types.reply(new_session, "Продукт не найден")
    Ok(product) -> {
      let message = format_product_details(product)
      let buttons = product_details_buttons(product)
      scene_types.send_buttons(new_session, message, buttons)
    }
  }
}

/// Показать сравнение тарифов
fn show_comparison(session: UserSession) -> scene_types.SceneResult {
  let new_session = set_scene(session, Pricing(PricingCompare))

  case product_catalog.get_all_products() {
    Error(e) -> scene_types.reply(
      new_session,
      "Ошибка: " <> product_catalog.error_to_string(e),
    )
    Ok(products) -> {
      let message = format_comparison(products)
      let buttons = comparison_buttons()
      scene_types.send_buttons(new_session, message, buttons)
    }
  }
}

/// Перенаправить на квиз
fn start_quiz_redirect(session: UserSession) -> scene_types.SceneResult {
  let new_session = set_scene(session, Quiz(QuizStart))
  scene_types.reply(new_session, "Переходим к квизу для подбора тарифа...")
}

/// Начать подписку
fn start_subscription(session: UserSession, code: String) -> scene_types.SceneResult {
  let new_session = set_scene(session, Subscription(SubscriptionPayment(code, "")))
  scene_types.reply(new_session, "Выберите способ оплаты для " <> code <> "...")
}

// =============================================================================
// Formatters
// =============================================================================

fn format_pricing_list(products: List(sales_types.Product)) -> String {
  let header = "💎 *Тарифы VIBEE*\n\nВыберите план для вашего AI-ассистента:\n\n"

  let products_text = products
    |> list.map(fn(p) {
      let price = sales_types.format_price_usd(p.price_usd_cents)
      let limit = case p.generation_limit {
        Some(l) -> int.to_string(l) <> " генераций"
        None -> "безлимит"
      }
      "• *" <> p.name_ru <> "* — " <> price <> "/мес\n  " <> limit <> "\n"
    })
    |> string.join("")

  let footer = "\n🎯 Не знаете какой выбрать? Пройдите квиз!"

  header <> products_text <> footer
}

fn format_product_details(product: sales_types.Product) -> String {
  let price = sales_types.format_price_usd(product.price_usd_cents)
  let limit = case product.generation_limit {
    Some(l) -> int.to_string(l) <> " генераций/мес"
    None -> "Безлимитные генерации"
  }

  let features_text = product.features
    |> list.map(fn(f) { "✅ " <> feature_to_text(f) })
    |> string.join("\n")

  "💎 *" <> product.name_ru <> "*\n\n"
  <> "💰 Цена: *" <> price <> "/мес*\n"
  <> "📊 Лимит: " <> limit <> "\n\n"
  <> "*Включено:*\n" <> features_text <> "\n\n"
  <> case product.description_ru {
    Some(desc) -> "📝 " <> desc
    None -> ""
  }
}

fn format_comparison(products: List(sales_types.Product)) -> String {
  let header = "📊 *Сравнение тарифов*\n\n"

  let table = products
    |> list.map(fn(p) {
      let price = sales_types.format_price_usd(p.price_usd_cents)
      let limit = case p.generation_limit {
        Some(l) -> int.to_string(l)
        None -> "∞"
      }
      let features_count = list.length(p.features)

      p.name_ru <> "\n"
      <> "  💰 " <> price <> "\n"
      <> "  📊 " <> limit <> " генераций\n"
      <> "  ✨ " <> int.to_string(features_count) <> " функций\n\n"
    })
    |> string.join("")

  header <> table
}

fn feature_to_text(feature: String) -> String {
  case feature {
    "basic_responses" -> "Базовые ответы AI"
    "telegram_bot" -> "Telegram бот"
    "email_support" -> "Email поддержка"
    "1_persona" -> "1 персона"
    "custom_persona" -> "Кастомная персона"
    "crm_integration" -> "Интеграция с CRM"
    "analytics" -> "Аналитика"
    "priority_support" -> "Приоритетная поддержка"
    "3_personas" -> "До 3 персон"
    "voice_clone" -> "Клонирование голоса"
    "unlimited_generations" -> "Безлимит генераций"
    "multichannel" -> "Мультиканальность"
    "api_access" -> "API доступ"
    "dedicated_support" -> "Выделенная поддержка"
    "unlimited_personas" -> "Безлимит персон"
    "white_label" -> "White Label"
    "custom_training" -> "Кастомное обучение"
    _ -> feature
  }
}

// =============================================================================
// Buttons
// =============================================================================

fn pricing_buttons(products: List(sales_types.Product)) -> List(List(#(String, String))) {
  let product_buttons = products
    |> list.map(fn(p) {
      [#(p.name_ru <> " " <> sales_types.format_price_usd(p.price_usd_cents),
         "pricing:details:" <> p.code)]
    })

  list.append(product_buttons, [
    [#("📊 Сравнить", "pricing:compare"), #("🎯 Пройти квиз", "pricing:quiz")],
  ])
}

fn product_details_buttons(product: sales_types.Product) -> List(List(#(String, String))) {
  [
    [#("🛒 Подписаться", "pricing:subscribe:" <> product.code)],
    [#("◀️ Назад к тарифам", "pricing:back")],
  ]
}

fn comparison_buttons() -> List(List(#(String, String))) {
  [
    [#("🎯 Пройти квиз", "pricing:quiz")],
    [#("◀️ Назад к тарифам", "pricing:back")],
  ]
}
