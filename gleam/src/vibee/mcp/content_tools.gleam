// MCP Content Tools - Инструменты для генерации контента цифрового клона
// avatar_generate, content_generate_script, content_pipeline_run

import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/ai/avatar_face
import vibee/content/pipeline
import vibee/content/templates
import vibee/content/types as content_types
import vibee/mcp/types.{type Tool, type ToolResult, TextContent, Tool, ToolResult}

// =============================================================================
// Tool Definitions
// =============================================================================

/// Инструмент генерации аватара
pub fn avatar_generate_tool() -> Tool {
  Tool(
    name: "avatar_generate",
    description: "Сгенерировать изображение аватара NEURO_SAGE с помощью LoRA модели. Используется для создания talking head видео.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("style", json.object([
          #("type", json.string("string")),
          #("description", json.string("Стиль аватара")),
          #("enum", json.array([
            "excited", "serious", "friendly", "confident", "promotional"
          ], json.string)),
          #("default", json.string("friendly")),
        ])),
        #("format", json.object([
          #("type", json.string("string")),
          #("description", json.string("Формат/размер изображения")),
          #("enum", json.array([
            "instagram_reel", "tiktok", "youtube_short", "telegram", "square"
          ], json.string)),
          #("default", json.string("instagram_reel")),
        ])),
        #("custom_prompt", json.object([
          #("type", json.string("string")),
          #("description", json.string("Дополнительный промпт для кастомизации (опционально)")),
        ])),
      ])),
      #("required", json.array([], json.string)),
    ]),
  )
}

/// Инструмент генерации скрипта
pub fn content_generate_script_tool() -> Tool {
  Tool(
    name: "content_generate_script",
    description: "Сгенерировать скрипт для видео контента на основе темы, категории и платформы. Использует шаблоны VIBEE.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("topic", json.object([
          #("type", json.string("string")),
          #("description", json.string("Тема контента, например 'Как я завайбкодил ферму роботов'")),
        ])),
        #("category", json.object([
          #("type", json.string("string")),
          #("description", json.string("Категория контента")),
          #("enum", json.array([
            "story", "tech_backstage", "educational", "case_study", "offer", "engagement"
          ], json.string)),
          #("default", json.string("story")),
        ])),
        #("platform", json.object([
          #("type", json.string("string")),
          #("description", json.string("Целевая платформа")),
          #("enum", json.array([
            "telegram", "instagram", "tiktok", "youtube", "twitter"
          ], json.string)),
          #("default", json.string("instagram")),
        ])),
        #("context", json.object([
          #("type", json.string("string")),
          #("description", json.string("Дополнительный контекст (GitHub профиль, кейсы и т.д.)")),
        ])),
      ])),
      #("required", json.array(["topic"], json.string)),
    ]),
  )
}

/// Инструмент полного пайплайна
pub fn content_pipeline_run_tool() -> Tool {
  Tool(
    name: "content_pipeline_run",
    description: "Запустить полный пайплайн генерации контента: скрипт → аватар → TTS → lipsync → рендер. Создаёт готовое видео для публикации.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("topic", json.object([
          #("type", json.string("string")),
          #("description", json.string("Тема контента")),
        ])),
        #("category", json.object([
          #("type", json.string("string")),
          #("description", json.string("Категория")),
          #("enum", json.array([
            "story", "tech_backstage", "educational", "case_study", "offer", "engagement"
          ], json.string)),
          #("default", json.string("story")),
        ])),
        #("platforms", json.object([
          #("type", json.string("array")),
          #("description", json.string("Целевые платформы")),
          #("items", json.object([
            #("type", json.string("string")),
            #("enum", json.array([
              "telegram", "instagram", "tiktok", "youtube", "twitter"
            ], json.string)),
          ])),
          #("default", json.array(["instagram", "tiktok"], json.string)),
        ])),
        #("github_username", json.object([
          #("type", json.string("string")),
          #("description", json.string("GitHub username для контекста (default: gHashTag)")),
          #("default", json.string("gHashTag")),
        ])),
      ])),
      #("required", json.array(["topic"], json.string)),
    ]),
  )
}

/// Инструмент получения шаблона
pub fn content_get_template_tool() -> Tool {
  Tool(
    name: "content_get_template",
    description: "Получить шаблон скрипта для категории контента. Показывает структуру, примеры хуков и CTA.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("category", json.object([
          #("type", json.string("string")),
          #("description", json.string("Категория контента")),
          #("enum", json.array([
            "story", "tech_backstage", "educational", "case_study", "offer", "engagement"
          ], json.string)),
        ])),
      ])),
      #("required", json.array(["category"], json.string)),
    ]),
  )
}

/// Получить все инструменты контента
pub fn get_content_tools() -> List(Tool) {
  [
    avatar_generate_tool(),
    content_generate_script_tool(),
    content_pipeline_run_tool(),
    content_get_template_tool(),
  ]
}

/// Получить все обработчики контента
pub fn get_content_handlers() -> List(#(String, fn(json.Json) -> ToolResult)) {
  [
    #("avatar_generate", handle_avatar_generate),
    #("content_generate_script", handle_content_generate_script),
    #("content_pipeline_run", handle_content_pipeline_run),
    #("content_get_template", handle_content_get_template),
  ]
}

// =============================================================================
// Tool Handlers
// =============================================================================

/// Обработчик генерации аватара
pub fn handle_avatar_generate(args: json.Json) -> ToolResult {
  let style_str = json_get_string(args, "style")
    |> result.unwrap("friendly")
  let format_str = json_get_string(args, "format")
    |> result.unwrap("instagram_reel")
  let custom_prompt = json_get_string(args, "custom_prompt")
    |> option.from_result()

  // Парсим стиль
  let style = parse_avatar_style(style_str)

  // Парсим формат
  let format = parse_content_format(format_str)

  // Генерируем аватар
  case custom_prompt {
    Some(prompt) -> {
      let #(width, height) = content_types.format_dimensions(format)
      case avatar_face.generate_with_prompt(prompt, style, width, height) {
        Error(e) -> error_result(avatar_face.error_to_string(e))
        Ok(result) -> {
          let response = json.object([
            #("success", json.bool(True)),
            #("image_url", json.string(result.image_url)),
            #("seed", json.int(result.seed)),
            #("generation_time_seconds", json.float(result.generation_time)),
          ])
          success_result(json.to_string(response))
        }
      }
    }
    None -> {
      case avatar_face.quick_generate(style, format) {
        Error(e) -> error_result(avatar_face.error_to_string(e))
        Ok(result) -> {
          let response = json.object([
            #("success", json.bool(True)),
            #("image_url", json.string(result.image_url)),
            #("seed", json.int(result.seed)),
            #("generation_time_seconds", json.float(result.generation_time)),
          ])
          success_result(json.to_string(response))
        }
      }
    }
  }
}

/// Обработчик генерации скрипта
pub fn handle_content_generate_script(args: json.Json) -> ToolResult {
  case json_get_string(args, "topic") {
    Error(_) -> error_result("Missing required parameter: topic")
    Ok(topic) -> {
      let category_str = json_get_string(args, "category")
        |> result.unwrap("story")
      let platform_str = json_get_string(args, "platform")
        |> result.unwrap("instagram")
      let context = json_get_string(args, "context")
        |> result.unwrap("")

      let category = parse_category(category_str)
      let platform = parse_platform(platform_str)

      case pipeline.generate_script(topic, category, platform, context) {
        Error(e) -> error_result(pipeline.error_to_string(e))
        Ok(script) -> {
          let template = templates.get_template(category)
          let hashtags = templates.generate_hashtags(category, platform)

          let response = json.object([
            #("success", json.bool(True)),
            #("script", json.string(script)),
            #("duration_seconds", json.int(template.duration_seconds)),
            #("voice_tone", json.string(template.voice_tone)),
            #("hashtags", json.array(hashtags, json.string)),
          ])
          success_result(json.to_string(response))
        }
      }
    }
  }
}

/// Обработчик полного пайплайна
pub fn handle_content_pipeline_run(args: json.Json) -> ToolResult {
  case json_get_string(args, "topic") {
    Error(_) -> error_result("Missing required parameter: topic")
    Ok(topic) -> {
      let category_str = json_get_string(args, "category")
        |> result.unwrap("story")
      let platforms_opt = json_get_string_array(args, "platforms")
      let _github_username = json_get_string(args, "github_username")
        |> result.unwrap("gHashTag")

      let category = parse_category(category_str)
      let platforms = case platforms_opt {
        Ok(ps) -> list.map(ps, parse_platform)
        Error(_) -> [content_types.Instagram, content_types.TikTok]
      }

      // Создаём план
      let formats = list.flat_map(platforms, platform_to_formats)
      let plan = content_types.ContentPlan(
        id: None,
        topic: topic,
        category: category,
        platforms: platforms,
        formats: formats,
        avatar_style: content_types.category_to_style(category),
        language: "ru",
        context: None,
        status: content_types.Draft,
        created_at: "",
        updated_at: "",
      )

      // TODO: Получить GitHub контекст
      let github_context = ""

      case pipeline.run_full_pipeline(plan, github_context) {
        Error(e) -> error_result(pipeline.error_to_string(e))
        Ok(contents) -> {
          let response = json.object([
            #("success", json.bool(True)),
            #("contents_generated", json.int(list.length(contents))),
            #("contents", json.array(contents, fn(c) {
              json.object([
                #("platform", json.string(content_types.platform_to_string(c.platform))),
                #("final_video_url", case c.final_video_url {
                  Some(url) -> json.string(url)
                  None -> json.null()
                }),
                #("hashtags", json.array(c.hashtags, json.string)),
                #("progress", json.int(pipeline.get_progress(c.status))),
              ])
            })),
          ])
          success_result(json.to_string(response))
        }
      }
    }
  }
}

/// Обработчик получения шаблона
pub fn handle_content_get_template(args: json.Json) -> ToolResult {
  case json_get_string(args, "category") {
    Error(_) -> error_result("Missing required parameter: category")
    Ok(category_str) -> {
      let category = parse_category(category_str)
      let template = templates.get_template(category)

      let response = json.object([
        #("success", json.bool(True)),
        #("category", json.string(category_str)),
        #("structure", json.string(template.structure)),
        #("hook_examples", json.array(template.hook_examples, json.string)),
        #("cta_examples", json.array(template.cta_examples, json.string)),
        #("duration_seconds", json.int(template.duration_seconds)),
        #("voice_tone", json.string(template.voice_tone)),
      ])
      success_result(json.to_string(response))
    }
  }
}

// =============================================================================
// Helpers
// =============================================================================

fn success_result(content: String) -> ToolResult {
  ToolResult(content: [TextContent(content)], is_error: False)
}

fn error_result(message: String) -> ToolResult {
  let error_json = json.object([
    #("success", json.bool(False)),
    #("error", json.string(message)),
  ])
  ToolResult(content: [TextContent(json.to_string(error_json))], is_error: True)
}

fn json_get_string(j: json.Json, key: String) -> Result(String, Nil) {
  let decoder = {
    use v <- decode.field(key, decode.string)
    decode.success(v)
  }
  let json_str = json.to_string(j)
  case json.parse(json_str, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

fn json_get_string_array(j: json.Json, key: String) -> Result(List(String), Nil) {
  let decoder = {
    use v <- decode.field(key, decode.list(decode.string))
    decode.success(v)
  }
  let json_str = json.to_string(j)
  case json.parse(json_str, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

fn parse_avatar_style(s: String) -> content_types.AvatarStyle {
  case s {
    "excited" -> content_types.Excited
    "serious" -> content_types.Serious
    "confident" -> content_types.Confident
    "promotional" -> content_types.Promotional
    _ -> content_types.Friendly
  }
}

fn parse_content_format(s: String) -> content_types.ContentFormat {
  case s {
    "tiktok" -> content_types.TikTokVideo
    "youtube_short" -> content_types.YouTubeShort
    "telegram" -> content_types.TelegramPost
    "square" -> content_types.InstagramPost
    _ -> content_types.InstagramReel
  }
}

fn parse_category(s: String) -> content_types.ContentCategory {
  case content_types.category_from_string(s) {
    Ok(c) -> c
    Error(_) -> content_types.Story
  }
}

fn parse_platform(s: String) -> content_types.Platform {
  case content_types.platform_from_string(s) {
    Ok(p) -> p
    Error(_) -> content_types.Instagram
  }
}

fn platform_to_formats(platform: content_types.Platform) -> List(content_types.ContentFormat) {
  case platform {
    content_types.Instagram -> [content_types.InstagramReel]
    content_types.TikTok -> [content_types.TikTokVideo]
    content_types.YouTube -> [content_types.YouTubeShort]
    content_types.Telegram -> [content_types.TelegramPost]
    content_types.Twitter -> [content_types.Tweet]
  }
}
