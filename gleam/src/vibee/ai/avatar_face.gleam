// Avatar Face Service - Централизованная генерация аватара с LoRA
// Использует FAL.ai FLUX LoRA для генерации изображений NEURO_SAGE

import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/ai/client as http_client
import vibee/ai/fal
import vibee/content/types.{
  type AvatarGenerationParams, type AvatarResult, type AvatarStyle,
  type ContentFormat, type LoraConfig, AvatarGenerationParams, AvatarResult,
  Confident, Excited, Friendly, Promotional, Serious,
}
import vibee/mcp/config

// =============================================================================
// Constants
// =============================================================================

/// URL дефолтной LoRA модели NEURO_SAGE
pub const default_lora_url = "https://v3b.fal.media/files/b/0a86e4c0/4TISTwQk8pxuu7ZTXsbHF_pytorch_lora_weights.safetensors"

/// Триггер-слово для LoRA
pub const lora_trigger = "NEURO_SAGE"

// =============================================================================
// Errors
// =============================================================================

pub type AvatarError {
  ConfigError(String)
  GenerationError(String)
  ParseError(String)
  NetworkError(String)
}

pub fn error_to_string(err: AvatarError) -> String {
  case err {
    ConfigError(msg) -> "Config error: " <> msg
    GenerationError(msg) -> "Generation error: " <> msg
    ParseError(msg) -> "Parse error: " <> msg
    NetworkError(msg) -> "Network error: " <> msg
  }
}

// =============================================================================
// Style Prompts
// =============================================================================

/// Получить базовый промпт для стиля аватара
pub fn style_prompt(style: AvatarStyle) -> String {
  case style {
    Excited ->
      "energetic and excited expression, bright eyes, slight smile, dynamic pose, vibrant lighting, tech startup atmosphere"
    Serious ->
      "focused and professional expression, thoughtful look, clean lighting, minimalist tech background, coding environment"
    Friendly ->
      "warm and approachable smile, friendly eyes, casual tech setting, soft natural lighting, welcoming atmosphere"
    Confident ->
      "confident and assured expression, direct eye contact, professional business setting, strong lighting, successful entrepreneur vibe"
    Promotional ->
      "charismatic and engaging expression, enthusiastic pose, premium studio lighting, sleek modern background, promotional style"
  }
}

/// Получить негативный промпт (что избегать)
pub fn negative_prompt() -> String {
  "blurry, low quality, distorted face, extra limbs, bad anatomy, deformed, ugly, duplicate, mutation, cropped, watermark, text, logo"
}

// =============================================================================
// Avatar Generation
// =============================================================================

/// Сгенерировать аватар с LoRA
pub fn generate_avatar(
  params: AvatarGenerationParams,
) -> Result(AvatarResult, AvatarError) {
  // Получить API ключ
  let api_key = config.get_env_or("FAL_API_KEY", "")
  case api_key {
    "" -> Error(ConfigError("FAL_API_KEY not set"))
    key -> {
      // Собрать полный промпт
      let full_prompt = build_prompt(params)

      // Создать запрос
      let fal_config = fal.Config(api_key: key)
      let request = fal.NeuroPhotoRequest(
        prompt: full_prompt,
        lora_url: params.lora.lora_url,
        num_images: 1,
        image_size: fal.ImageSize(width: params.width, height: params.height),
        seed: params.seed,
        guidance_scale: Some(3.5),
        num_inference_steps: Some(28),
        enable_safety_checker: True,
      )

      let http_req = fal.neuro_photo_request(fal_config, request)

      // Выполнить запрос
      case execute_fal_request(http_req) {
        Error(e) -> Error(e)
        Ok(response) -> parse_fal_response(response)
      }
    }
  }
}

/// Быстрая генерация с дефолтными параметрами
pub fn quick_generate(
  style: AvatarStyle,
  format: ContentFormat,
) -> Result(AvatarResult, AvatarError) {
  let #(width, height) = types.format_dimensions(format)

  let params = AvatarGenerationParams(
    lora: types.default_lora_config(),
    base_prompt: "",
    style: style,
    width: width,
    height: height,
    seed: None,
  )

  generate_avatar(params)
}

/// Генерация с кастомным промптом
pub fn generate_with_prompt(
  custom_prompt: String,
  style: AvatarStyle,
  width: Int,
  height: Int,
) -> Result(AvatarResult, AvatarError) {
  let params = AvatarGenerationParams(
    lora: types.default_lora_config(),
    base_prompt: custom_prompt,
    style: style,
    width: width,
    height: height,
    seed: None,
  )

  generate_avatar(params)
}

// =============================================================================
// Prompt Building
// =============================================================================

/// Собрать полный промпт для генерации
fn build_prompt(params: AvatarGenerationParams) -> String {
  let trigger = params.lora.trigger_word
  let style_part = style_prompt(params.style)

  let base = case string.is_empty(params.base_prompt) {
    True -> ""
    False -> params.base_prompt <> ", "
  }

  // Формат: TRIGGER_WORD base_prompt, style_description, quality modifiers
  trigger
  <> " "
  <> base
  <> style_part
  <> ", professional portrait, high quality, sharp focus, 8k resolution"
}

// =============================================================================
// HTTP Execution
// =============================================================================

/// Выполнить запрос к FAL.ai
fn execute_fal_request(req: fal.Request) -> Result(String, AvatarError) {
  let http_req = http_client.Request(
    url: req.url,
    method: req.method,
    headers: req.headers,
    body: req.body,
  )

  case http_client.execute_json(http_req) {
    Ok(body) -> Ok(body)
    Error(http_client.HttpError(status, body)) ->
      Error(GenerationError(
        "FAL API error " <> int.to_string(status) <> ": " <> body
      ))
    Error(http_client.NetworkError(msg)) ->
      Error(NetworkError(msg))
    Error(_) ->
      Error(NetworkError("Unknown error"))
  }
}

/// Парсить ответ FAL.ai
fn parse_fal_response(body: String) -> Result(AvatarResult, AvatarError) {
  // Декодер для ответа FAL
  let image_decoder = {
    use url <- decode.field("url", decode.string)
    decode.success(url)
  }

  let result_decoder = {
    use images <- decode.field("images", decode.list(image_decoder))
    use seed <- decode.optional_field("seed", 0, decode.int)
    use timings <- decode.optional_field("timings", #(0.0, 0.0), timings_decoder())
    decode.success(#(images, seed, timings))
  }

  case json.parse(body, result_decoder) {
    Error(_) -> Error(ParseError("Failed to parse FAL response: " <> body))
    Ok(#(images, seed, #(inference_time, _))) -> {
      case images {
        [] -> Error(GenerationError("No images in response"))
        [url, ..] -> Ok(AvatarResult(
          image_url: url,
          seed: seed,
          generation_time: inference_time,
        ))
      }
    }
  }
}

/// Декодер для timings
fn timings_decoder() -> decode.Decoder(#(Float, Float)) {
  use inference <- decode.optional_field("inference", 0.0, decode.float)
  use total <- decode.optional_field("total", 0.0, decode.float)
  decode.success(#(inference, total))
}

// =============================================================================
// Preset Configurations
// =============================================================================

/// Конфиг для Instagram Reels (9:16)
pub fn instagram_reel_config(style: AvatarStyle) -> AvatarGenerationParams {
  AvatarGenerationParams(
    lora: types.default_lora_config(),
    base_prompt: "",
    style: style,
    width: 1080,
    height: 1920,
    seed: None,
  )
}

/// Конфиг для YouTube Shorts (9:16)
pub fn youtube_short_config(style: AvatarStyle) -> AvatarGenerationParams {
  AvatarGenerationParams(
    lora: types.default_lora_config(),
    base_prompt: "",
    style: style,
    width: 1080,
    height: 1920,
    seed: None,
  )
}

/// Конфиг для TikTok (9:16)
pub fn tiktok_config(style: AvatarStyle) -> AvatarGenerationParams {
  AvatarGenerationParams(
    lora: types.default_lora_config(),
    base_prompt: "",
    style: style,
    width: 1080,
    height: 1920,
    seed: None,
  )
}

/// Конфиг для Telegram (16:9)
pub fn telegram_config(style: AvatarStyle) -> AvatarGenerationParams {
  AvatarGenerationParams(
    lora: types.default_lora_config(),
    base_prompt: "",
    style: style,
    width: 1280,
    height: 720,
    seed: None,
  )
}

/// Конфиг для квадратного формата (Instagram Post)
pub fn square_config(style: AvatarStyle) -> AvatarGenerationParams {
  AvatarGenerationParams(
    lora: types.default_lora_config(),
    base_prompt: "",
    style: style,
    width: 1080,
    height: 1080,
    seed: None,
  )
}

// =============================================================================
// JSON Serialization
// =============================================================================

/// Сериализовать результат в JSON
pub fn result_to_json(result: AvatarResult) -> String {
  json.object([
    #("image_url", json.string(result.image_url)),
    #("seed", json.int(result.seed)),
    #("generation_time", json.float(result.generation_time)),
  ])
  |> json.to_string
}
