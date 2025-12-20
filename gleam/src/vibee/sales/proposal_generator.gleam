// Proposal Generator - Автогенерация коммерческих предложений
// Создаёт персонализированные КП на основе данных лида и квиза

import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/sales/types.{
  type Lead, type Product, type Proposal, type QuizResponse,
  Proposal, ProposalDraft,
  format_price_usd, format_price_rub,
}
import vibee/sales/product_catalog

// =============================================================================
// Types
// =============================================================================

/// Параметры генерации КП
pub type ProposalParams {
  ProposalParams(
    lead: Lead,
    product: Product,
    quiz_response: Option(QuizResponse),
    discount_percent: Int,
    valid_days: Int,
    language: String,
  )
}

/// Результат генерации
pub type GeneratedProposal {
  GeneratedProposal(
    proposal: Proposal,
    text_content: String,
    html_content: String,
  )
}

/// Ошибки генерации
pub type ProposalError {
  InvalidLeadError(String)
  InvalidProductError(String)
  GenerationError(String)
}

// =============================================================================
// Main Generator
// =============================================================================

/// Сгенерировать КП для лида
pub fn generate_proposal(params: ProposalParams) -> Result(GeneratedProposal, ProposalError) {
  // Валидация
  case validate_params(params) {
    Error(e) -> Error(e)
    Ok(_) -> {
      // Генерируем контент
      let content = generate_content(params)
      let html = generate_html(params, content)

      // Рассчитываем цену со скидкой
      let original_price = params.product.price_usd_cents
      let discounted_price = case params.discount_percent > 0 {
        True -> original_price - { original_price * params.discount_percent / 100 }
        False -> original_price
      }

      // Создаём Proposal
      let proposal = Proposal(
        id: None,
        lead_id: case params.lead.id {
          Some(id) -> id
          None -> 0
        },
        product_id: case params.product.id {
          Some(id) -> id
          None -> 0
        },
        title: generate_title(params),
        content: content,
        price_usd_cents: Some(discounted_price),
        discount_percent: params.discount_percent,
        valid_until: None,  // TODO: calculate from valid_days
        status: ProposalDraft,
        sent_at: None,
        viewed_at: None,
        responded_at: None,
      )

      Ok(GeneratedProposal(
        proposal: proposal,
        text_content: content,
        html_content: html,
      ))
    }
  }
}

/// Быстрая генерация КП по product_code
pub fn generate_for_lead(
  lead: Lead,
  product_code: String,
  discount: Int,
  language: String,
) -> Result(GeneratedProposal, ProposalError) {
  case product_catalog.get_product_by_code(product_code) {
    Error(_) -> Error(InvalidProductError("Product not found: " <> product_code))
    Ok(product) -> {
      generate_proposal(ProposalParams(
        lead: lead,
        product: product,
        quiz_response: None,
        discount_percent: discount,
        valid_days: 7,
        language: language,
      ))
    }
  }
}

// =============================================================================
// Content Generation
// =============================================================================

fn generate_title(params: ProposalParams) -> String {
  let name = case params.lead.first_name {
    Some(n) -> n
    None -> case params.lead.username {
      Some(u) -> "@" <> u
      None -> "valued customer"
    }
  }

  case params.language {
    "ru" -> "Коммерческое предложение для " <> name <> " - " <> params.product.name_ru
    _ -> "Commercial Proposal for " <> name <> " - " <> case params.product.name_en {
      Some(en) -> en
      None -> params.product.name_ru
    }
  }
}

fn generate_content(params: ProposalParams) -> String {
  case params.language {
    "ru" -> generate_content_ru(params)
    _ -> generate_content_en(params)
  }
}

fn generate_content_ru(params: ProposalParams) -> String {
  let name = case params.lead.first_name {
    Some(n) -> n
    None -> "Уважаемый клиент"
  }

  let product = params.product
  let price = format_price_usd(product.price_usd_cents)
  let price_rub = case product.price_rub_cents {
    Some(rub) -> " (" <> format_price_rub(rub) <> ")"
    None -> ""
  }

  let discount_text = case params.discount_percent > 0 {
    True -> "\n\nСПЕЦИАЛЬНОЕ ПРЕДЛОЖЕНИЕ: скидка " <> int.to_string(params.discount_percent) <> "%!"
    False -> ""
  }

  let features = generate_features_ru(product.features)

  let quiz_insights = case params.quiz_response {
    Some(_) -> "\n\nНа основе ваших ответов мы подобрали оптимальный тариф."
    None -> ""
  }

  // Main content
  name <> ", здравствуйте!

Благодарим за интерес к VIBEE - вашему персональному AI-ассистенту.
" <> quiz_insights <> "

═══════════════════════════════════════════════════════════════
ТАРИФ: " <> product.name_ru <> "
═══════════════════════════════════════════════════════════════

" <> case product.description_ru {
  Some(desc) -> desc <> "\n\n"
  None -> ""
} <> "ВКЛЮЧЕНО В ТАРИФ:
" <> features <> "

ЛИМИТ ГЕНЕРАЦИЙ: " <> case product.generation_limit {
  Some(limit) -> int.to_string(limit) <> " в месяц"
  None -> "Безлимит"
} <> "

СТОИМОСТЬ: " <> price <> "/месяц" <> price_rub <> discount_text <> "

═══════════════════════════════════════════════════════════════

Для активации тарифа используйте команду /subscribe в боте
или свяжитесь с нами для персональной консультации.

С уважением,
Команда VIBEE"
}

fn generate_content_en(params: ProposalParams) -> String {
  let name = case params.lead.first_name {
    Some(n) -> n
    None -> "Valued Customer"
  }

  let product = params.product
  let price = format_price_usd(product.price_usd_cents)

  let discount_text = case params.discount_percent > 0 {
    True -> "\n\nSPECIAL OFFER: " <> int.to_string(params.discount_percent) <> "% discount!"
    False -> ""
  }

  let features = generate_features_en(product.features)

  let product_name = case product.name_en {
    Some(en) -> en
    None -> product.name_ru
  }

  let product_desc = case product.description_en {
    Some(desc) -> desc <> "\n\n"
    None -> ""
  }

  name <> ", hello!

Thank you for your interest in VIBEE - your personal AI assistant.

═══════════════════════════════════════════════════════════════
PLAN: " <> product_name <> "
═══════════════════════════════════════════════════════════════

" <> product_desc <> "INCLUDED:
" <> features <> "

GENERATION LIMIT: " <> case product.generation_limit {
  Some(limit) -> int.to_string(limit) <> " per month"
  None -> "Unlimited"
} <> "

PRICE: " <> price <> "/month" <> discount_text <> "

═══════════════════════════════════════════════════════════════

To activate, use /subscribe command in the bot
or contact us for personal consultation.

Best regards,
VIBEE Team"
}

fn generate_features_ru(features: List(String)) -> String {
  features
  |> list.map(fn(f) { "• " <> feature_to_ru(f) })
  |> string.join("\n")
}

fn generate_features_en(features: List(String)) -> String {
  features
  |> list.map(fn(f) { "• " <> feature_to_en(f) })
  |> string.join("\n")
}

fn feature_to_ru(feature: String) -> String {
  case feature {
    "basic_responses" -> "Базовые ответы AI"
    "telegram_bot" -> "Telegram бот"
    "email_support" -> "Email поддержка"
    "1_persona" -> "1 персона"
    "custom_persona" -> "Кастомная персона"
    "crm_integration" -> "CRM интеграция"
    "analytics" -> "Аналитика"
    "priority_support" -> "Приоритетная поддержка"
    "3_personas" -> "3 персоны"
    "voice_clone" -> "Клонирование голоса"
    "unlimited_generations" -> "Безлимитные генерации"
    "multichannel" -> "Мультиканальность"
    "api_access" -> "API доступ"
    "dedicated_support" -> "Выделенная поддержка"
    "unlimited_personas" -> "Безлимитные персоны"
    "white_label" -> "White Label"
    "custom_training" -> "Кастомное обучение"
    _ -> feature
  }
}

fn feature_to_en(feature: String) -> String {
  case feature {
    "basic_responses" -> "Basic AI responses"
    "telegram_bot" -> "Telegram bot"
    "email_support" -> "Email support"
    "1_persona" -> "1 persona"
    "custom_persona" -> "Custom persona"
    "crm_integration" -> "CRM integration"
    "analytics" -> "Analytics"
    "priority_support" -> "Priority support"
    "3_personas" -> "3 personas"
    "voice_clone" -> "Voice cloning"
    "unlimited_generations" -> "Unlimited generations"
    "multichannel" -> "Multichannel"
    "api_access" -> "API access"
    "dedicated_support" -> "Dedicated support"
    "unlimited_personas" -> "Unlimited personas"
    "white_label" -> "White Label"
    "custom_training" -> "Custom training"
    _ -> feature
  }
}

// =============================================================================
// HTML Generation
// =============================================================================

fn generate_html(params: ProposalParams, text_content: String) -> String {
  let product = params.product
  let price = format_price_usd(product.price_usd_cents)

  let name = case params.lead.first_name {
    Some(n) -> n
    None -> case params.language {
      "ru" -> "Уважаемый клиент"
      _ -> "Valued Customer"
    }
  }

  let product_name = case params.language {
    "ru" -> product.name_ru
    _ -> case product.name_en {
      Some(en) -> en
      None -> product.name_ru
    }
  }

  "<!DOCTYPE html>
<html>
<head>
  <meta charset=\"UTF-8\">
  <title>" <> product_name <> "</title>
  <style>
    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; max-width: 600px; margin: 0 auto; padding: 20px; }
    .header { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 30px; border-radius: 10px; text-align: center; }
    .content { padding: 20px 0; }
    .price { font-size: 2em; color: #667eea; font-weight: bold; }
    .features { background: #f5f5f5; padding: 20px; border-radius: 10px; margin: 20px 0; }
    .cta { background: #667eea; color: white; padding: 15px 30px; border-radius: 5px; text-decoration: none; display: inline-block; margin-top: 20px; }
    .discount { background: #ff6b6b; color: white; padding: 5px 10px; border-radius: 3px; font-weight: bold; }
  </style>
</head>
<body>
  <div class=\"header\">
    <h1>VIBEE</h1>
    <p>" <> case params.language { "ru" -> "Коммерческое предложение" _ -> "Commercial Proposal" } <> "</p>
  </div>
  <div class=\"content\">
    <h2>" <> case params.language { "ru" -> "Здравствуйте, " _ -> "Hello, " } <> name <> "!</h2>
    <h3>" <> product_name <> "</h3>
    <p class=\"price\">" <> price <> "<span style=\"font-size: 0.5em\">/mo</span></p>
    " <> case params.discount_percent > 0 {
      True -> "<span class=\"discount\">-" <> int.to_string(params.discount_percent) <> "%</span>"
      False -> ""
    } <> "
    <div class=\"features\">
      <h4>" <> case params.language { "ru" -> "Включено:" _ -> "Included:" } <> "</h4>
      <ul>" <> generate_html_features(product.features, params.language) <> "</ul>
    </div>
    <a href=\"https://t.me/vibee_agent?start=subscribe\" class=\"cta\">
      " <> case params.language { "ru" -> "Активировать" _ -> "Activate" } <> "
    </a>
  </div>
</body>
</html>"
}

fn generate_html_features(features: List(String), language: String) -> String {
  features
  |> list.map(fn(f) {
    let label = case language {
      "ru" -> feature_to_ru(f)
      _ -> feature_to_en(f)
    }
    "<li>" <> label <> "</li>"
  })
  |> string.join("\n")
}

// =============================================================================
// Validation
// =============================================================================

fn validate_params(params: ProposalParams) -> Result(Nil, ProposalError) {
  case params.lead.id {
    None -> Error(InvalidLeadError("Lead must have an ID"))
    Some(_) -> {
      case params.product.id {
        None -> Error(InvalidProductError("Product must have an ID"))
        Some(_) -> Ok(Nil)
      }
    }
  }
}

// =============================================================================
// JSON Serialization
// =============================================================================

pub fn proposal_to_json(proposal: Proposal) -> json.Json {
  json.object([
    #("id", case proposal.id {
      Some(id) -> json.int(id)
      None -> json.null()
    }),
    #("lead_id", json.int(proposal.lead_id)),
    #("product_id", json.int(proposal.product_id)),
    #("title", json.string(proposal.title)),
    #("content", json.string(proposal.content)),
    #("price_usd_cents", case proposal.price_usd_cents {
      Some(p) -> json.int(p)
      None -> json.null()
    }),
    #("discount_percent", json.int(proposal.discount_percent)),
    #("status", json.string(types.proposal_status_to_string(proposal.status))),
  ])
}

pub fn generated_proposal_to_json(gp: GeneratedProposal) -> json.Json {
  json.object([
    #("proposal", proposal_to_json(gp.proposal)),
    #("text_content", json.string(gp.text_content)),
    #("html_content", json.string(gp.html_content)),
  ])
}
