// Content Pipeline - Оркестратор генерации контента
// Topic → Script → Avatar → TTS → Lipsync → Render → Publish

import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/ai/avatar_face
import vibee/ai/client as http_client
import vibee/content/templates
import vibee/content/types.{
  type ContentCategory, type ContentFormat, type ContentPlan, type ContentStatus,
  type GeneratedContent, type GenerationStatus, type Platform,
  ContentPlan, Draft, Failed, GeneratedContent, GeneratingAvatar,
  GeneratingLipsync, GeneratingTTS, PendingScript, ReadyToPublish,
  RenderingVideo, TikTokVideo,
}
import vibee/mcp/config

// =============================================================================
// Errors
// =============================================================================

pub type PipelineError {
  ScriptError(String)
  AvatarError(String)
  TTSError(String)
  LipsyncError(String)
  RenderError(String)
  PublishError(String)
  ConfigError(String)
}

pub fn error_to_string(err: PipelineError) -> String {
  case err {
    ScriptError(msg) -> "Script generation failed: " <> msg
    AvatarError(msg) -> "Avatar generation failed: " <> msg
    TTSError(msg) -> "TTS failed: " <> msg
    LipsyncError(msg) -> "Lipsync failed: " <> msg
    RenderError(msg) -> "Render failed: " <> msg
    PublishError(msg) -> "Publish failed: " <> msg
    ConfigError(msg) -> "Config error: " <> msg
  }
}

// =============================================================================
// Pipeline Steps
// =============================================================================

/// Шаг 1: Генерация скрипта через LLM
pub fn generate_script(
  topic: String,
  category: ContentCategory,
  platform: Platform,
  context: String,
) -> Result(String, PipelineError) {
  // Получить промпт из шаблона
  let prompt = templates.generate_llm_prompt(category, platform, topic, context)

  // Вызвать LLM (OpenRouter)
  case call_llm(prompt) {
    Error(e) -> Error(ScriptError(e))
    Ok(script) -> Ok(script)
  }
}

/// Шаг 2: Генерация аватара
pub fn generate_avatar(
  style: types.AvatarStyle,
  format: ContentFormat,
) -> Result(String, PipelineError) {
  case avatar_face.quick_generate(style, format) {
    Error(e) -> Error(AvatarError(avatar_face.error_to_string(e)))
    Ok(result) -> Ok(result.image_url)
  }
}

/// Шаг 3: Генерация TTS
/// TODO: Интеграция с ElevenLabs API
pub fn generate_tts(
  _script: String,
  _voice_id: Option(String),
) -> Result(String, PipelineError) {
  // Заглушка - в продакшене вызывать ElevenLabs API
  Error(TTSError("TTS integration pending - use ai_elevenlabs_tts tool directly"))
}

/// Шаг 4: Генерация Lipsync видео
/// TODO: Интеграция с Hedra API
pub fn generate_lipsync(
  _avatar_url: String,
  _audio_url: String,
) -> Result(String, PipelineError) {
  // Заглушка - в продакшене вызывать Hedra API
  Error(LipsyncError("Lipsync integration pending - use ai_hedra_create_avatar tool directly"))
}

/// Шаг 5: Рендеринг финального видео через Remotion
pub fn render_video(
  lipsync_video_url: String,
  format: ContentFormat,
  hashtags: List(String),
) -> Result(String, PipelineError) {
  let #(width, height) = types.format_dimensions(format)

  // Вызов Remotion render server
  let render_url = config.get_env_or(
    "REMOTION_RENDER_URL",
    "https://vibee-remotion.fly.dev/render"
  )

  let body = json.object([
    #("composition", json.string("LipSyncMain")),
    #("inputProps", json.object([
      #("videoUrl", json.string(lipsync_video_url)),
      #("hashtags", json.array(hashtags, json.string)),
    ])),
    #("width", json.int(width)),
    #("height", json.int(height)),
  ])
  |> json.to_string

  let req = http_client.Request(
    url: render_url,
    method: "POST",
    headers: [#("Content-Type", "application/json")],
    body: body,
  )

  case http_client.execute_json(req) {
    Error(_) -> Error(RenderError("Failed to call Remotion"))
    Ok(response) -> {
      // Парсим URL из ответа
      case parse_render_response(response) {
        Error(_) -> Error(RenderError("Failed to parse render response"))
        Ok(url) -> Ok(url)
      }
    }
  }
}

// =============================================================================
// Full Pipeline
// =============================================================================

/// Запустить полный пайплайн генерации
pub fn run_full_pipeline(
  plan: ContentPlan,
  github_context: String,
) -> Result(List(GeneratedContent), PipelineError) {
  // Для каждого формата генерируем контент
  list.try_map(plan.formats, fn(format) {
    run_pipeline_for_format(plan, format, github_context)
  })
}

/// Запустить пайплайн для одного формата
pub fn run_pipeline_for_format(
  plan: ContentPlan,
  format: ContentFormat,
  github_context: String,
) -> Result(GeneratedContent, PipelineError) {
  let platform = format_to_platform(format)
  let style = types.category_to_style(plan.category)

  // Собираем контекст
  let context = case plan.context {
    Some(c) -> c <> "\n\n" <> github_context
    None -> github_context
  }

  // 1. Генерация скрипта
  use script <- result.try(
    generate_script(plan.topic, plan.category, platform, context)
  )

  // 2. Генерация аватара
  use avatar_url <- result.try(generate_avatar(style, format))

  // 3. Генерация TTS
  use audio_url <- result.try(generate_tts(script, None))

  // 4. Генерация Lipsync
  use lipsync_url <- result.try(generate_lipsync(avatar_url, audio_url))

  // 5. Генерация хештегов
  let hashtags = templates.generate_hashtags(plan.category, platform)

  // 6. Рендеринг видео
  use video_url <- result.try(render_video(lipsync_url, format, hashtags))

  // Собираем результат
  Ok(GeneratedContent(
    id: None,
    plan_id: option.unwrap(plan.id, 0),
    platform: platform,
    format: format,
    script: script,
    avatar_image_url: Some(avatar_url),
    tts_audio_url: Some(audio_url),
    lipsync_video_url: Some(lipsync_url),
    final_video_url: Some(video_url),
    duration_seconds: None,
    hashtags: hashtags,
    status: ReadyToPublish,
    created_at: "",
  ))
}

// =============================================================================
// LLM Integration
// =============================================================================

/// Вызвать LLM для генерации скрипта
fn call_llm(prompt: String) -> Result(String, String) {
  let api_key = config.get_env_or("OPENROUTER_API_KEY", "")
  case api_key {
    "" -> Error("OPENROUTER_API_KEY not set")
    key -> {
      let body = json.object([
        #("model", json.string("anthropic/claude-3.5-sonnet")),
        #("messages", json.array([
          json.object([
            #("role", json.string("user")),
            #("content", json.string(prompt)),
          ]),
        ], fn(x) { x })),
        #("max_tokens", json.int(2000)),
      ])
      |> json.to_string

      let req = http_client.Request(
        url: "https://openrouter.ai/api/v1/chat/completions",
        method: "POST",
        headers: [
          #("Authorization", "Bearer " <> key),
          #("Content-Type", "application/json"),
          #("HTTP-Referer", "https://vibee.ai"),
        ],
        body: body,
      )

      case http_client.execute_json(req) {
        Error(_) -> Error("Failed to call OpenRouter")
        Ok(response) -> parse_llm_response(response)
      }
    }
  }
}

/// Парсить ответ LLM
fn parse_llm_response(body: String) -> Result(String, String) {
  // Простой парсинг - ищем content в ответе
  case string.contains(body, "\"content\":") {
    False -> Error("No content in response")
    True -> {
      // Извлекаем content между кавычками
      // Упрощённая реализация, в продакшене использовать json.parse
      Ok(body)  // TODO: proper JSON parsing
    }
  }
}

/// Парсить ответ рендера
fn parse_render_response(body: String) -> Result(String, Nil) {
  case string.contains(body, "\"url\":") {
    False -> Error(Nil)
    True -> Ok(body)  // TODO: proper JSON parsing
  }
}

// =============================================================================
// Helpers
// =============================================================================

/// Конвертировать формат в платформу
fn format_to_platform(format: ContentFormat) -> Platform {
  case format {
    types.InstagramReel -> types.Instagram
    types.InstagramStory -> types.Instagram
    types.InstagramPost -> types.Instagram
    types.TikTokVideo -> types.TikTok
    types.YouTubeShort -> types.YouTube
    types.YouTubeVideo -> types.YouTube
    types.TelegramPost -> types.Telegram
    types.Tweet -> types.Twitter
  }
}

// =============================================================================
// Status Tracking
// =============================================================================

/// Обновить статус генерации
pub fn update_status(
  content: GeneratedContent,
  status: GenerationStatus,
) -> GeneratedContent {
  GeneratedContent(..content, status: status)
}

/// Проверить готовность к публикации
pub fn is_ready_to_publish(content: GeneratedContent) -> Bool {
  case content.status {
    ReadyToPublish -> True
    _ -> False
  }
}

/// Получить прогресс в процентах
pub fn get_progress(status: GenerationStatus) -> Int {
  case status {
    PendingScript -> 0
    GeneratingAvatar -> 20
    GeneratingTTS -> 40
    GeneratingLipsync -> 60
    RenderingVideo -> 80
    ReadyToPublish -> 100
    types.ContentPublished -> 100
    Failed(_) -> 0
  }
}

// =============================================================================
// JSON Serialization
// =============================================================================

/// Сериализовать контент в JSON
pub fn content_to_json(content: GeneratedContent) -> String {
  json.object([
    #("plan_id", json.int(content.plan_id)),
    #("platform", json.string(types.platform_to_string(content.platform))),
    #("script", json.string(content.script)),
    #("avatar_image_url", option_to_json(content.avatar_image_url)),
    #("tts_audio_url", option_to_json(content.tts_audio_url)),
    #("lipsync_video_url", option_to_json(content.lipsync_video_url)),
    #("final_video_url", option_to_json(content.final_video_url)),
    #("hashtags", json.array(content.hashtags, json.string)),
    #("progress", json.int(get_progress(content.status))),
  ])
  |> json.to_string
}

fn option_to_json(opt: Option(String)) -> json.Json {
  case opt {
    Some(s) -> json.string(s)
    None -> json.null()
  }
}
