// Content Types - Типы для генерации контента цифрового клона
// Платформы, категории, шаблоны и планы контента

import gleam/option.{type Option}

// =============================================================================
// Платформы публикации
// =============================================================================

/// Целевая платформа для публикации
pub type Platform {
  Telegram
  Instagram
  TikTok
  YouTube
  Twitter
}

/// Формат контента по платформе
pub type ContentFormat {
  /// Telegram: текст, фото, видео до 2GB
  TelegramPost
  /// Instagram: Reels (9:16), Stories, Posts (1:1)
  InstagramReel
  InstagramStory
  InstagramPost
  /// TikTok: вертикальное видео 9:16
  TikTokVideo
  /// YouTube: Shorts (9:16) или обычное видео (16:9)
  YouTubeShort
  YouTubeVideo
  /// Twitter/X: текст + медиа
  Tweet
}

// =============================================================================
// Категории контента
// =============================================================================

/// Категория контента для digital clone
pub type ContentCategory {
  /// "Я завайбкодил ферму роботов" - сторителлинг, энергичный
  Story
  /// Деплой, логи, архитектура - технический, серьёзный
  TechBackstage
  /// "Вайбкодинг за 15 минут" - обучающий, дружелюбный
  Educational
  /// Кейсы клиентов - уверенный, профессиональный
  CaseStudy
  /// Тарифы Junior/Middle/Senior - промо
  Offer
  /// Ответы на комменты, Q&A
  Engagement
}

/// Стиль аватара для категории
pub type AvatarStyle {
  /// Энергичный, с эмоциями
  Excited
  /// Серьёзный, технический
  Serious
  /// Дружелюбный, обучающий
  Friendly
  /// Уверенный, профессиональный
  Confident
  /// Промо, продающий
  Promotional
}

// =============================================================================
// Контент план
// =============================================================================

/// План генерации контента
pub type ContentPlan {
  ContentPlan(
    id: Option(Int),
    /// Тема/топик контента
    topic: String,
    /// Категория
    category: ContentCategory,
    /// Целевые платформы
    platforms: List(Platform),
    /// Формат для каждой платформы
    formats: List(ContentFormat),
    /// Стиль аватара
    avatar_style: AvatarStyle,
    /// Язык (ru, en)
    language: String,
    /// Дополнительный контекст (GitHub профиль, кейсы)
    context: Option(String),
    /// Статус
    status: ContentStatus,
    /// Timestamps
    created_at: String,
    updated_at: String,
  )
}

/// Статус плана контента
pub type ContentStatus {
  /// Черновик
  Draft
  /// В процессе генерации
  Generating
  /// Сгенерирован, ожидает проверки
  Generated
  /// Опубликован
  Published
  /// Отменён
  Cancelled
}

// =============================================================================
// Сгенерированный контент
// =============================================================================

/// Сгенерированный контент
pub type GeneratedContent {
  GeneratedContent(
    id: Option(Int),
    /// Ссылка на план
    plan_id: Int,
    /// Платформа
    platform: Platform,
    /// Формат
    format: ContentFormat,
    /// Сгенерированный скрипт/текст
    script: String,
    /// URL аватар-изображения (FAL.ai)
    avatar_image_url: Option(String),
    /// URL TTS аудио (ElevenLabs)
    tts_audio_url: Option(String),
    /// URL lipsync видео (Hedra)
    lipsync_video_url: Option(String),
    /// URL финального видео (Remotion)
    final_video_url: Option(String),
    /// Длительность в секундах
    duration_seconds: Option(Int),
    /// Хештеги
    hashtags: List(String),
    /// Статус
    status: GenerationStatus,
    /// Timestamps
    created_at: String,
  )
}

/// Статус генерации
pub type GenerationStatus {
  /// Ожидает скрипт
  PendingScript
  /// Скрипт готов, генерируется аватар
  GeneratingAvatar
  /// Аватар готов, генерируется TTS
  GeneratingTTS
  /// TTS готов, генерируется lipsync
  GeneratingLipsync
  /// Lipsync готов, рендерится видео
  RenderingVideo
  /// Готово к публикации
  ReadyToPublish
  /// Опубликовано (GenerationStatus)
  ContentPublished
  /// Ошибка
  Failed(String)
}

// =============================================================================
// LoRA конфигурация аватара
// =============================================================================

/// Конфигурация LoRA модели для аватара
pub type LoraConfig {
  LoraConfig(
    /// URL к файлу LoRA weights
    lora_url: String,
    /// Триггер-слово для активации LoRA
    trigger_word: String,
    /// Сила применения LoRA (0.0 - 1.0)
    lora_scale: Float,
  )
}

/// Дефолтный конфиг для NEURO_SAGE
pub fn default_lora_config() -> LoraConfig {
  LoraConfig(
    lora_url: "https://v3b.fal.media/files/b/0a86e4c0/4TISTwQk8pxuu7ZTXsbHF_pytorch_lora_weights.safetensors",
    trigger_word: "NEURO_SAGE",
    lora_scale: 0.85,
  )
}

// =============================================================================
// Промпт для генерации аватара
// =============================================================================

/// Параметры генерации аватара
pub type AvatarGenerationParams {
  AvatarGenerationParams(
    /// LoRA конфигурация
    lora: LoraConfig,
    /// Базовый промпт (описание сцены)
    base_prompt: String,
    /// Стиль (влияет на lighting, expression)
    style: AvatarStyle,
    /// Размер (для разных платформ)
    width: Int,
    height: Int,
    /// Seed для воспроизводимости
    seed: Option(Int),
  )
}

/// Результат генерации аватара
pub type AvatarResult {
  AvatarResult(
    /// URL сгенерированного изображения
    image_url: String,
    /// Seed который использовался
    seed: Int,
    /// Время генерации в секундах
    generation_time: Float,
  )
}

// =============================================================================
// Конвертеры
// =============================================================================

/// Преобразовать категорию в стиль аватара
pub fn category_to_style(category: ContentCategory) -> AvatarStyle {
  case category {
    Story -> Excited
    TechBackstage -> Serious
    Educational -> Friendly
    CaseStudy -> Confident
    Offer -> Promotional
    Engagement -> Friendly
  }
}

/// Преобразовать платформу в строку
pub fn platform_to_string(platform: Platform) -> String {
  case platform {
    Telegram -> "telegram"
    Instagram -> "instagram"
    TikTok -> "tiktok"
    YouTube -> "youtube"
    Twitter -> "twitter"
  }
}

/// Преобразовать строку в платформу
pub fn platform_from_string(s: String) -> Result(Platform, Nil) {
  case s {
    "telegram" -> Ok(Telegram)
    "instagram" -> Ok(Instagram)
    "tiktok" -> Ok(TikTok)
    "youtube" -> Ok(YouTube)
    "twitter" -> Ok(Twitter)
    _ -> Error(Nil)
  }
}

/// Преобразовать категорию в строку
pub fn category_to_string(category: ContentCategory) -> String {
  case category {
    Story -> "story"
    TechBackstage -> "tech_backstage"
    Educational -> "educational"
    CaseStudy -> "case_study"
    Offer -> "offer"
    Engagement -> "engagement"
  }
}

/// Преобразовать строку в категорию
pub fn category_from_string(s: String) -> Result(ContentCategory, Nil) {
  case s {
    "story" -> Ok(Story)
    "tech_backstage" -> Ok(TechBackstage)
    "educational" -> Ok(Educational)
    "case_study" -> Ok(CaseStudy)
    "offer" -> Ok(Offer)
    "engagement" -> Ok(Engagement)
    _ -> Error(Nil)
  }
}

/// Получить размеры для формата
pub fn format_dimensions(format: ContentFormat) -> #(Int, Int) {
  case format {
    // Вертикальные форматы 9:16
    InstagramReel -> #(1080, 1920)
    InstagramStory -> #(1080, 1920)
    TikTokVideo -> #(1080, 1920)
    YouTubeShort -> #(1080, 1920)
    // Квадрат 1:1
    InstagramPost -> #(1080, 1080)
    // Горизонтальные 16:9
    YouTubeVideo -> #(1920, 1080)
    // Телеграм - универсально
    TelegramPost -> #(1280, 720)
    // Twitter
    Tweet -> #(1200, 675)
  }
}
