// Content Templates - Шаблоны скриптов для разных категорий контента
// Используются для генерации скриптов через LLM

import gleam/string
import vibee/content/types.{
  type ContentCategory, type Platform, CaseStudy, Educational, Engagement,
  Instagram, Offer, Story, TechBackstage, Telegram, TikTok, Twitter, YouTube,
}

// =============================================================================
// Script Templates
// =============================================================================

/// Шаблон скрипта для категории
pub type ScriptTemplate {
  ScriptTemplate(
    /// Системный промпт для LLM
    system_prompt: String,
    /// Структура скрипта
    structure: String,
    /// Пример хуков
    hook_examples: List(String),
    /// Пример CTA
    cta_examples: List(String),
    /// Рекомендуемая длительность в секундах
    duration_seconds: Int,
    /// Тон голоса
    voice_tone: String,
  )
}

/// Получить шаблон для категории
pub fn get_template(category: ContentCategory) -> ScriptTemplate {
  case category {
    Story -> story_template()
    TechBackstage -> tech_backstage_template()
    Educational -> educational_template()
    CaseStudy -> case_study_template()
    Offer -> offer_template()
    Engagement -> engagement_template()
  }
}

// =============================================================================
// Category Templates
// =============================================================================

/// Story - "Я завайбкодил ферму роботов"
fn story_template() -> ScriptTemplate {
  ScriptTemplate(
    system_prompt: "
Ты - NEURO_SAGE, AI-разработчик и создатель фермы роботов на основе VIBEE.
Стиль: энергичный, вдохновляющий, с юмором.
Рассказываешь истории из первого лица.
Используй сленг: вайбкодинг, завайбкодил, нейросотрудники.
Цель: вдохновить на использование AI в работе.
",
    structure: "
1. HOOK (3-5 сек): Интригующее начало
2. SETUP (10-15 сек): Контекст проблемы
3. JOURNEY (20-30 сек): Как решал, что использовал
4. РЕЗУЛЬТАТ (10-15 сек): Что получилось
5. CTA (5 сек): Призыв к действию
",
    hook_examples: [
      "Я завайбкодил ферму из 50 роботов за выходные",
      "Мои нейросотрудники заработали первые деньги",
      "Как я заменил отдел продаж на одного бота",
    ],
    cta_examples: [
      "Хочешь свою ферму? Ссылка в шапке",
      "Напиши 'ФЕРМА' и получи гайд",
      "Подписывайся, расскажу как",
    ],
    duration_seconds: 60,
    voice_tone: "energetic, enthusiastic, slightly humorous",
  )
}

/// TechBackstage - деплой, логи, архитектура
fn tech_backstage_template() -> ScriptTemplate {
  ScriptTemplate(
    system_prompt: "
Ты - NEURO_SAGE, опытный разработчик показывающий закулисье.
Стиль: технический, но понятный. Серьёзный, но не скучный.
Показываешь реальный код, логи, архитектуру.
Цель: демонстрация экспертизы и технической глубины.
",
    structure: "
1. HOOK (3 сек): Показать проблему/баг/задачу
2. КОНТЕКСТ (10 сек): Что за система, зачем это
3. ПРОЦЕСС (30-40 сек): Код, терминал, объяснение
4. РЕЗУЛЬТАТ (10 сек): Работающее решение
5. TAKEAWAY (5 сек): Главный вывод
",
    hook_examples: [
      "Этот баг стоил мне 3 часа дебага",
      "Смотри как выглядит реальный деплой на Fly.io",
      "Показываю архитектуру VIBEE изнутри",
    ],
    cta_examples: [
      "Вопросы в комменты",
      "Сохраняй, пригодится",
      "Подписывайся на техничку",
    ],
    duration_seconds: 90,
    voice_tone: "focused, technical, explanatory",
  )
}

/// Educational - "Вайбкодинг за 15 минут"
fn educational_template() -> ScriptTemplate {
  ScriptTemplate(
    system_prompt: "
Ты - NEURO_SAGE, терпеливый учитель объясняющий сложное простыми словами.
Стиль: дружелюбный, структурированный, с примерами.
Объясняешь пошагово, проверяешь понимание.
Цель: научить, дать практические навыки.
",
    structure: "
1. HOOK (3 сек): Что научишься делать
2. ПОЧЕМУ ВАЖНО (10 сек): Зачем это нужно
3. ШАГ 1-3 (40-50 сек): Пошаговое объяснение
4. ДЕМО (15 сек): Показать результат
5. CTA (5 сек): Следующий шаг для зрителя
",
    hook_examples: [
      "Научу создавать AI-ботов за 15 минут",
      "3 команды для запуска своего агента",
      "Покажу как работает RAG на пальцах",
    ],
    cta_examples: [
      "Попробуй сам и напиши результат",
      "Следующий урок уже готовлю",
      "Скачай шаблон по ссылке",
    ],
    duration_seconds: 120,
    voice_tone: "warm, patient, encouraging",
  )
}

/// CaseStudy - кейсы клиентов
fn case_study_template() -> ScriptTemplate {
  ScriptTemplate(
    system_prompt: "
Ты - NEURO_SAGE, представляющий успешные кейсы клиентов.
Стиль: уверенный, с конкретными цифрами и результатами.
Говоришь о реальных проектах и их ROI.
Цель: показать ценность и результаты работы.
",
    structure: "
1. HOOK (3 сек): Главный результат клиента
2. КЛИЕНТ (10 сек): Кто, какая ниша, какая проблема
3. РЕШЕНИЕ (20 сек): Что внедрили
4. РЕЗУЛЬТАТЫ (15 сек): Цифры, метрики, ROI
5. CTA (5 сек): Как получить такое же
",
    hook_examples: [
      "Клиент сэкономил 200 часов в месяц",
      "Конверсия выросла на 340%",
      "Один бот заменил 5 менеджеров",
    ],
    cta_examples: [
      "Хочешь так же? Напиши мне",
      "Запишись на аудит бесплатно",
      "Разберём твой кейс в комментах",
    ],
    duration_seconds: 60,
    voice_tone: "confident, professional, results-focused",
  )
}

/// Offer - тарифы Junior/Middle/Senior
fn offer_template() -> ScriptTemplate {
  ScriptTemplate(
    system_prompt: "
Ты - NEURO_SAGE, представляющий продукт VIBEE.
Стиль: энергичный, продающий, но не навязчивый.
Объясняешь ценность каждого тарифа.
Цель: мотивировать на покупку/подписку.
",
    structure: "
1. HOOK (3 сек): Боль или желание аудитории
2. РЕШЕНИЕ (10 сек): Как VIBEE решает проблему
3. ТАРИФЫ (25 сек): Junior/Middle/Senior - что в каждом
4. БОНУС (10 сек): Специальное предложение
5. CTA (5 сек): Как купить прямо сейчас
",
    hook_examples: [
      "Устал отвечать на одни и те же вопросы?",
      "Хочешь чтобы AI работал на тебя 24/7?",
      "Готов масштабировать без найма людей?",
    ],
    cta_examples: [
      "Пиши ХОЧУ, подберу тариф",
      "Первые 10 - скидка 50%",
      "Ссылка в шапке, бронируй",
    ],
    duration_seconds: 60,
    voice_tone: "enthusiastic, persuasive, value-focused",
  )
}

/// Engagement - ответы на комменты, Q&A
fn engagement_template() -> ScriptTemplate {
  ScriptTemplate(
    system_prompt: "
Ты - NEURO_SAGE, отвечающий на вопросы подписчиков.
Стиль: дружелюбный, искренний, вовлекающий.
Благодаришь за вопросы, даёшь полезные ответы.
Цель: укрепить связь с аудиторией.
",
    structure: "
1. ПРИВЕТСТВИЕ (3 сек): Обращение к автору вопроса
2. ВОПРОС (5 сек): Зачитать вопрос
3. ОТВЕТ (30-40 сек): Развёрнутый ответ
4. ДОПОЛНЕНИЕ (10 сек): Связанный совет или история
5. ВОВЛЕЧЕНИЕ (5 сек): Призыв задать ещё вопросы
",
    hook_examples: [
      "@username задаёт отличный вопрос",
      "Часто спрашивают про это, отвечаю",
      "Вопрос дня из комментов",
    ],
    cta_examples: [
      "Пиши свой вопрос в комменты",
      "Какие ещё темы разобрать?",
      "Лайк если было полезно",
    ],
    duration_seconds: 60,
    voice_tone: "friendly, conversational, appreciative",
  )
}

// =============================================================================
// Platform-Specific Adjustments
// =============================================================================

/// Адаптировать шаблон под платформу
pub fn adapt_for_platform(
  template: ScriptTemplate,
  platform: Platform,
) -> ScriptTemplate {
  case platform {
    TikTok -> ScriptTemplate(
      ..template,
      duration_seconds: min(template.duration_seconds, 60),
      structure: shorten_structure(template.structure),
    )
    Instagram -> ScriptTemplate(
      ..template,
      duration_seconds: min(template.duration_seconds, 90),
    )
    YouTube -> template  // YouTube Shorts могут быть до 60 сек
    Telegram -> ScriptTemplate(
      ..template,
      duration_seconds: min(template.duration_seconds, 120),
    )
    Twitter -> ScriptTemplate(
      ..template,
      duration_seconds: 45,  // Twitter video max
      structure: shorten_structure(template.structure),
    )
  }
}

/// Сократить структуру для коротких форматов
fn shorten_structure(structure: String) -> String {
  // Упрощаем структуру для коротких видео
  "
1. HOOK (2-3 сек): Быстрый захват внимания
2. ГЛАВНАЯ МЫСЛЬ (15-20 сек): Одна ключевая идея
3. CTA (3 сек): Быстрый призыв
"
}

fn min(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}

// =============================================================================
// Prompt Generation
// =============================================================================

/// Сгенерировать промпт для LLM
pub fn generate_llm_prompt(
  category: ContentCategory,
  platform: Platform,
  topic: String,
  context: String,
) -> String {
  let template = get_template(category)
  let adapted = adapt_for_platform(template, platform)

  let platform_name = types.platform_to_string(platform)

  "
" <> adapted.system_prompt <> "

Платформа: " <> platform_name <> "
Длительность: " <> string.inspect(adapted.duration_seconds) <> " секунд
Тон: " <> adapted.voice_tone <> "

Структура скрипта:
" <> adapted.structure <> "

Примеры хуков:
" <> string.join(adapted.hook_examples, "\n- ") <> "

Примеры CTA:
" <> string.join(adapted.cta_examples, "\n- ") <> "

ТЕМА: " <> topic <> "

КОНТЕКСТ:
" <> context <> "

Напиши скрипт для озвучки. Каждую секцию начинай с метки [HOOK], [MAIN], [CTA].
Пиши так, как будто говоришь на камеру - живым разговорным языком.
"
}

// =============================================================================
// Hashtag Generation
// =============================================================================

/// Сгенерировать хештеги для категории и платформы
pub fn generate_hashtags(
  category: ContentCategory,
  platform: Platform,
) -> List(String) {
  let base_tags = ["vibee", "ai", "нейросети", "автоматизация"]

  let category_tags = case category {
    Story -> ["вайбкодинг", "история", "стартап"]
    TechBackstage -> ["разработка", "код", "девлог"]
    Educational -> ["обучение", "туториал", "урок"]
    CaseStudy -> ["кейс", "результаты", "бизнес"]
    Offer -> ["тарифы", "подписка", "старт"]
    Engagement -> ["ответы", "qa", "вопросы"]
  }

  let platform_tags = case platform {
    TikTok -> ["fyp", "рекомендации", "тикток"]
    Instagram -> ["reels", "инстаграм"]
    YouTube -> ["shorts", "ютуб"]
    Telegram -> ["телеграм", "бот"]
    Twitter -> ["tech", "ai"]
  }

  list_concat([base_tags, category_tags, platform_tags])
}

fn list_concat(lists: List(List(String))) -> List(String) {
  case lists {
    [] -> []
    [first, ..rest] -> append_all(first, list_concat(rest))
  }
}

fn append_all(a: List(String), b: List(String)) -> List(String) {
  case a {
    [] -> b
    [x, ..xs] -> [x, ..append_all(xs, b)]
  }
}
