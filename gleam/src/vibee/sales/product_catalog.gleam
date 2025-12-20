// Product Catalog - Каталог тарифов VIBEE
// Временная stub-версия без БД

import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import vibee/sales/types.{
  type BillingPeriod, type Product, Monthly, Product,
}

// =============================================================================
// Errors
// =============================================================================

pub type CatalogError {
  DatabaseError(String)
  NotFoundError(String)
  ValidationError(String)
}

pub fn error_to_string(err: CatalogError) -> String {
  case err {
    DatabaseError(msg) -> "Database error: " <> msg
    NotFoundError(msg) -> "Not found: " <> msg
    ValidationError(msg) -> "Validation error: " <> msg
  }
}

// =============================================================================
// Default Products
// =============================================================================

fn default_products() -> List(Product) {
  [
    Product(
      id: Some(1),
      code: "junior",
      name_ru: "VIBEE Junior",
      name_en: Some("VIBEE Junior"),
      description_ru: Some("Базовый тариф для старта"),
      description_en: Some("Basic tier to get started"),
      price_usd_cents: 9900,
      price_rub_cents: Some(990_000),
      billing_period: Monthly,
      generation_limit: Some(100),
      features: ["basic_responses", "telegram_bot", "email_support", "1_persona"],
      is_active: True,
      sort_order: 1,
    ),
    Product(
      id: Some(2),
      code: "middle",
      name_ru: "VIBEE Middle",
      name_en: Some("VIBEE Middle"),
      description_ru: Some("Оптимальный тариф для бизнеса"),
      description_en: Some("Optimal tier for business"),
      price_usd_cents: 29900,
      price_rub_cents: Some(2_990_000),
      billing_period: Monthly,
      generation_limit: Some(500),
      features: ["custom_persona", "crm_integration", "analytics", "priority_support", "3_personas", "voice_clone"],
      is_active: True,
      sort_order: 2,
    ),
    Product(
      id: Some(3),
      code: "senior",
      name_ru: "VIBEE Senior",
      name_en: Some("VIBEE Senior"),
      description_ru: Some("Безлимитный тариф для масштабирования"),
      description_en: Some("Unlimited tier for scaling"),
      price_usd_cents: 99900,
      price_rub_cents: Some(9_990_000),
      billing_period: Monthly,
      generation_limit: None,
      features: ["unlimited_generations", "multichannel", "api_access", "dedicated_support", "unlimited_personas", "white_label", "custom_training"],
      is_active: True,
      sort_order: 3,
    ),
  ]
}

// =============================================================================
// Product CRUD (Stub implementations)
// =============================================================================

/// Получить все активные продукты
pub fn get_all_products() -> Result(List(Product), CatalogError) {
  Ok(default_products())
}

/// Получить продукт по коду
pub fn get_product_by_code(code: String) -> Result(Product, CatalogError) {
  case list.find(default_products(), fn(p) { p.code == code }) {
    Ok(product) -> Ok(product)
    Error(_) -> Error(NotFoundError("Product not found: " <> code))
  }
}

/// Получить продукт по ID
pub fn get_product_by_id(id: Int) -> Result(Product, CatalogError) {
  case list.find(default_products(), fn(p) {
    case p.id {
      Some(pid) -> pid == id
      None -> False
    }
  }) {
    Ok(product) -> Ok(product)
    Error(_) -> Error(NotFoundError("Product not found: " <> int.to_string(id)))
  }
}

// =============================================================================
// JSON Serialization
// =============================================================================

/// Сериализовать продукт в JSON
pub fn product_to_json(product: Product) -> json.Json {
  json.object([
    #("id", case product.id {
      Some(id) -> json.int(id)
      None -> json.null()
    }),
    #("code", json.string(product.code)),
    #("name_ru", json.string(product.name_ru)),
    #("name_en", case product.name_en {
      Some(name) -> json.string(name)
      None -> json.null()
    }),
    #("description_ru", case product.description_ru {
      Some(desc) -> json.string(desc)
      None -> json.null()
    }),
    #("description_en", case product.description_en {
      Some(desc) -> json.string(desc)
      None -> json.null()
    }),
    #("price_usd_cents", json.int(product.price_usd_cents)),
    #("price_usd", json.string(types.format_price_usd(product.price_usd_cents))),
    #("price_rub_cents", case product.price_rub_cents {
      Some(cents) -> json.int(cents)
      None -> json.null()
    }),
    #("price_rub", case product.price_rub_cents {
      Some(cents) -> json.string(types.format_price_rub(cents))
      None -> json.null()
    }),
    #("billing_period", json.string(types.billing_period_to_string(product.billing_period))),
    #("generation_limit", case product.generation_limit {
      Some(limit) -> json.int(limit)
      None -> json.string("unlimited")
    }),
    #("features", json.array(product.features, json.string)),
    #("is_active", json.bool(product.is_active)),
  ])
}

/// Сериализовать список продуктов
pub fn products_to_json(products: List(Product)) -> json.Json {
  json.array(products, product_to_json)
}

// =============================================================================
// Comparison & Recommendation
// =============================================================================

/// Получить сравнительную таблицу тарифов
pub fn get_comparison_table() -> Result(json.Json, CatalogError) {
  case get_all_products() {
    Error(e) -> Error(e)
    Ok(products) -> {
      let all_features = products
        |> list.flat_map(fn(p) { p.features })
        |> list.unique()

      let table = json.object([
        #("products", products_to_json(products)),
        #("features", json.array(all_features, fn(f) {
          json.object([
            #("code", json.string(f)),
            #("name", json.string(feature_to_name(f))),
            #("availability", json.array(products, fn(p) {
              json.object([
                #("product_code", json.string(p.code)),
                #("available", json.bool(list.contains(p.features, f))),
              ])
            })),
          ])
        })),
      ])

      Ok(table)
    }
  }
}

/// Получить рекомендуемый продукт по лимиту генераций
pub fn recommend_by_usage(generations_needed: Int) -> Result(Product, CatalogError) {
  case generations_needed {
    n if n <= 100 -> get_product_by_code("junior")
    n if n <= 500 -> get_product_by_code("middle")
    _ -> get_product_by_code("senior")
  }
}

// =============================================================================
// Helpers
// =============================================================================

fn feature_to_name(feature: String) -> String {
  case feature {
    "basic_responses" -> "Базовые ответы"
    "telegram_bot" -> "Telegram бот"
    "email_support" -> "Email поддержка"
    "1_persona" -> "1 персона"
    "custom_persona" -> "Кастомная персона"
    "crm_integration" -> "CRM интеграция"
    "analytics" -> "Аналитика"
    "priority_support" -> "Приоритетная поддержка"
    "3_personas" -> "3 персоны"
    "voice_clone" -> "Клон голоса"
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
