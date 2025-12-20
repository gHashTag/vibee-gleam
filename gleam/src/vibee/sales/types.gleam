// Sales Types - Типы для системы продаж VIBEE
// Products, Subscriptions, Leads, Quiz, Proposals

import gleam/option.{type Option, None, Some}

// =============================================================================
// Products
// =============================================================================

/// Продукт VIBEE (тарифный план)
pub type Product {
  Product(
    id: Option(Int),
    code: String,
    name_ru: String,
    name_en: Option(String),
    description_ru: Option(String),
    description_en: Option(String),
    price_usd_cents: Int,
    price_rub_cents: Option(Int),
    billing_period: BillingPeriod,
    generation_limit: Option(Int),
    features: List(String),
    is_active: Bool,
    sort_order: Int,
  )
}

/// Период оплаты
pub type BillingPeriod {
  Monthly
  Yearly
  OneTime
}

/// Дефолтные продукты VIBEE
pub fn junior_product() -> Product {
  Product(
    id: None,
    code: "junior",
    name_ru: "VIBEE Junior",
    name_en: Some("VIBEE Junior"),
    description_ru: Some("Базовый тариф для старта с AI-ассистентом"),
    description_en: Some("Basic tier to start with AI assistant"),
    price_usd_cents: 9900,
    price_rub_cents: Some(990_000),
    billing_period: Monthly,
    generation_limit: Some(100),
    features: [
      "basic_responses", "telegram_bot", "email_support", "1_persona",
    ],
    is_active: True,
    sort_order: 1,
  )
}

pub fn middle_product() -> Product {
  Product(
    id: None,
    code: "middle",
    name_ru: "VIBEE Middle",
    name_en: Some("VIBEE Middle"),
    description_ru: Some("Продвинутый тариф с CRM и аналитикой"),
    description_en: Some("Advanced tier with CRM and analytics"),
    price_usd_cents: 29_900,
    price_rub_cents: Some(2_990_000),
    billing_period: Monthly,
    generation_limit: Some(500),
    features: [
      "custom_persona", "crm_integration", "analytics", "priority_support",
      "3_personas", "voice_clone",
    ],
    is_active: True,
    sort_order: 2,
  )
}

pub fn senior_product() -> Product {
  Product(
    id: None,
    code: "senior",
    name_ru: "VIBEE Senior",
    name_en: Some("VIBEE Senior"),
    description_ru: Some("Безлимитный тариф для масштабирования"),
    description_en: Some("Unlimited tier for scaling"),
    price_usd_cents: 99_900,
    price_rub_cents: Some(9_990_000),
    billing_period: Monthly,
    generation_limit: None,
    features: [
      "unlimited_generations", "multichannel", "api_access", "dedicated_support",
      "unlimited_personas", "white_label", "custom_training",
    ],
    is_active: True,
    sort_order: 3,
  )
}

// =============================================================================
// Subscriptions
// =============================================================================

/// Подписка пользователя
pub type Subscription {
  Subscription(
    id: Option(Int),
    telegram_id: Int,
    product_id: Int,
    status: SubscriptionStatus,
    payment_method: Option(PaymentMethod),
    payment_id: Option(String),
    current_period_start: Option(String),
    current_period_end: Option(String),
    generations_used: Int,
    cancel_at_period_end: Bool,
    cancelled_at: Option(String),
  )
}

/// Статус подписки
pub type SubscriptionStatus {
  SubPending
  SubActive
  SubCancelled
  SubExpired
  SubPaused
}

/// Метод оплаты
pub type PaymentMethod {
  Robokassa
  TonNative
  TonUSDT
  TelegramStars
  CryptoBot
  XRocket
}

// =============================================================================
// Leads
// =============================================================================

/// Лид (потенциальный клиент)
pub type Lead {
  Lead(
    id: Option(Int),
    telegram_user_id: Int,
    username: Option(String),
    first_name: Option(String),
    last_name: Option(String),
    phone: Option(String),
    email: Option(String),
    first_message: Option(String),
    intent: Option(LeadIntent),
    priority: LeadPriority,
    status: LeadStatus,
    funnel_stage: FunnelStage,
    source: Option(String),
    utm_source: Option(String),
    utm_medium: Option(String),
    utm_campaign: Option(String),
    quiz_score: Option(Int),
    recommended_product_id: Option(Int),
    assigned_to: Option(String),
    notes: Option(String),
    last_contact_at: Option(String),
    created_at: Option(String),
  )
}

/// Намерение лида
pub type LeadIntent {
  IntentPurchase
  IntentQuestion
  IntentSupport
  IntentPartnership
}

/// Приоритет лида
pub type LeadPriority {
  PriorityLow
  PriorityMedium
  PriorityHigh
  PriorityUrgent
}

/// Статус лида
pub type LeadStatus {
  StatusNew
  StatusContacted
  StatusQualified
  StatusProposalSent
  StatusNegotiation
  StatusWon
  StatusLost
}

/// Стадия воронки
pub type FunnelStage {
  StageAwareness
  StageInterest
  StageConsideration
  StageIntent
  StageEvaluation
  StagePurchase
}

// =============================================================================
// Quiz
// =============================================================================

/// Ответ на квиз
pub type QuizResponse {
  QuizResponse(
    id: Option(Int),
    telegram_id: Int,
    lead_id: Option(Int),
    quiz_type: QuizType,
    responses: List(QuizAnswer),
    score: Option(Int),
    recommended_product_id: Option(Int),
    completed_at: Option(String),
  )
}

/// Тип квиза
pub type QuizType {
  QuizQualification
  QuizOnboarding
  QuizFeedback
}

/// Ответ на вопрос квиза
pub type QuizAnswer {
  QuizAnswer(question_id: String, answer: String, score: Int)
}

/// Вопрос квиза
pub type QuizQuestion {
  QuizQuestion(
    id: String,
    question_ru: String,
    question_en: String,
    options: List(QuizOption),
    weight: Int,
  )
}

/// Вариант ответа
pub type QuizOption {
  QuizOption(value: String, label_ru: String, label_en: String, score: Int)
}

// =============================================================================
// Proposals (КП)
// =============================================================================

/// Коммерческое предложение
pub type Proposal {
  Proposal(
    id: Option(Int),
    lead_id: Int,
    product_id: Int,
    title: String,
    content: String,
    price_usd_cents: Option(Int),
    discount_percent: Int,
    valid_until: Option(String),
    status: ProposalStatus,
    sent_at: Option(String),
    viewed_at: Option(String),
    responded_at: Option(String),
  )
}

/// Статус КП
pub type ProposalStatus {
  ProposalDraft
  ProposalSent
  ProposalViewed
  ProposalAccepted
  ProposalRejected
  ProposalExpired
}

// =============================================================================
// Usage Tracking
// =============================================================================

/// Запись использования (для paywall)
pub type UsageLog {
  UsageLog(
    id: Option(Int),
    telegram_id: Int,
    subscription_id: Option(Int),
    action_type: String,
    action_details: Option(String),
    tokens_used: Int,
    cost_cents: Int,
    created_at: Option(String),
  )
}

// =============================================================================
// Converters
// =============================================================================

pub fn billing_period_to_string(period: BillingPeriod) -> String {
  case period {
    Monthly -> "monthly"
    Yearly -> "yearly"
    OneTime -> "one_time"
  }
}

pub fn billing_period_from_string(s: String) -> Result(BillingPeriod, Nil) {
  case s {
    "monthly" -> Ok(Monthly)
    "yearly" -> Ok(Yearly)
    "one_time" -> Ok(OneTime)
    _ -> Error(Nil)
  }
}

pub fn subscription_status_to_string(status: SubscriptionStatus) -> String {
  case status {
    SubPending -> "pending"
    SubActive -> "active"
    SubCancelled -> "cancelled"
    SubExpired -> "expired"
    SubPaused -> "paused"
  }
}

pub fn subscription_status_from_string(s: String) -> Result(SubscriptionStatus, Nil) {
  case s {
    "pending" -> Ok(SubPending)
    "active" -> Ok(SubActive)
    "cancelled" -> Ok(SubCancelled)
    "expired" -> Ok(SubExpired)
    "paused" -> Ok(SubPaused)
    _ -> Error(Nil)
  }
}

pub fn payment_method_to_string(method: PaymentMethod) -> String {
  case method {
    Robokassa -> "robokassa"
    TonNative -> "ton"
    TonUSDT -> "ton_usdt"
    TelegramStars -> "stars"
    CryptoBot -> "cryptobot"
    XRocket -> "xrocket"
  }
}

pub fn payment_method_from_string(s: String) -> Result(PaymentMethod, Nil) {
  case s {
    "robokassa" -> Ok(Robokassa)
    "ton" -> Ok(TonNative)
    "ton_usdt" -> Ok(TonUSDT)
    "stars" -> Ok(TelegramStars)
    "cryptobot" -> Ok(CryptoBot)
    "xrocket" -> Ok(XRocket)
    _ -> Error(Nil)
  }
}

pub fn lead_intent_to_string(intent: LeadIntent) -> String {
  case intent {
    IntentPurchase -> "purchase"
    IntentQuestion -> "question"
    IntentSupport -> "support"
    IntentPartnership -> "partnership"
  }
}

pub fn lead_intent_from_string(s: String) -> Result(LeadIntent, Nil) {
  case s {
    "purchase" -> Ok(IntentPurchase)
    "question" -> Ok(IntentQuestion)
    "support" -> Ok(IntentSupport)
    "partnership" -> Ok(IntentPartnership)
    _ -> Error(Nil)
  }
}

pub fn lead_priority_to_string(priority: LeadPriority) -> String {
  case priority {
    PriorityLow -> "low"
    PriorityMedium -> "medium"
    PriorityHigh -> "high"
    PriorityUrgent -> "urgent"
  }
}

pub fn lead_priority_from_string(s: String) -> Result(LeadPriority, Nil) {
  case s {
    "low" -> Ok(PriorityLow)
    "medium" -> Ok(PriorityMedium)
    "high" -> Ok(PriorityHigh)
    "urgent" -> Ok(PriorityUrgent)
    _ -> Error(Nil)
  }
}

pub fn lead_status_to_string(status: LeadStatus) -> String {
  case status {
    StatusNew -> "new"
    StatusContacted -> "contacted"
    StatusQualified -> "qualified"
    StatusProposalSent -> "proposal_sent"
    StatusNegotiation -> "negotiation"
    StatusWon -> "won"
    StatusLost -> "lost"
  }
}

pub fn lead_status_from_string(s: String) -> Result(LeadStatus, Nil) {
  case s {
    "new" -> Ok(StatusNew)
    "contacted" -> Ok(StatusContacted)
    "qualified" -> Ok(StatusQualified)
    "proposal_sent" -> Ok(StatusProposalSent)
    "negotiation" -> Ok(StatusNegotiation)
    "won" -> Ok(StatusWon)
    "lost" -> Ok(StatusLost)
    _ -> Error(Nil)
  }
}

pub fn funnel_stage_to_string(stage: FunnelStage) -> String {
  case stage {
    StageAwareness -> "awareness"
    StageInterest -> "interest"
    StageConsideration -> "consideration"
    StageIntent -> "intent"
    StageEvaluation -> "evaluation"
    StagePurchase -> "purchase"
  }
}

pub fn funnel_stage_from_string(s: String) -> Result(FunnelStage, Nil) {
  case s {
    "awareness" -> Ok(StageAwareness)
    "interest" -> Ok(StageInterest)
    "consideration" -> Ok(StageConsideration)
    "intent" -> Ok(StageIntent)
    "evaluation" -> Ok(StageEvaluation)
    "purchase" -> Ok(StagePurchase)
    _ -> Error(Nil)
  }
}

pub fn quiz_type_to_string(qt: QuizType) -> String {
  case qt {
    QuizQualification -> "qualification"
    QuizOnboarding -> "onboarding"
    QuizFeedback -> "feedback"
  }
}

pub fn quiz_type_from_string(s: String) -> Result(QuizType, Nil) {
  case s {
    "qualification" -> Ok(QuizQualification)
    "onboarding" -> Ok(QuizOnboarding)
    "feedback" -> Ok(QuizFeedback)
    _ -> Error(Nil)
  }
}

pub fn proposal_status_to_string(status: ProposalStatus) -> String {
  case status {
    ProposalDraft -> "draft"
    ProposalSent -> "sent"
    ProposalViewed -> "viewed"
    ProposalAccepted -> "accepted"
    ProposalRejected -> "rejected"
    ProposalExpired -> "expired"
  }
}

pub fn proposal_status_from_string(s: String) -> Result(ProposalStatus, Nil) {
  case s {
    "draft" -> Ok(ProposalDraft)
    "sent" -> Ok(ProposalSent)
    "viewed" -> Ok(ProposalViewed)
    "accepted" -> Ok(ProposalAccepted)
    "rejected" -> Ok(ProposalRejected)
    "expired" -> Ok(ProposalExpired)
    _ -> Error(Nil)
  }
}

// =============================================================================
// Price Helpers
// =============================================================================

/// Форматировать цену в USD
pub fn format_price_usd(cents: Int) -> String {
  let dollars = cents / 100
  let remaining_cents = cents % 100
  "$" <> int_to_string(dollars) <> "." <> pad_cents(remaining_cents)
}

/// Форматировать цену в RUB
pub fn format_price_rub(cents: Int) -> String {
  let rubles = cents / 100
  int_to_string(rubles) <> " ₽"
}

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
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        _ -> "9"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}

fn pad_cents(n: Int) -> String {
  case n < 10 {
    True -> "0" <> int_to_string(n)
    False -> int_to_string(n)
  }
}
