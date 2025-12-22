// Reels Creation Action (ElizaOS Architecture)
//
// NO COMMANDS! This action triggers based on conversation context.
// validate() detects intent through NLP patterns, not /reels command.
//
// Trigger patterns (similes):
// - "создай рилс", "сделай видео для рилс"
// - "хочу видео для инстаграм"
// - "нужен контент для соцсетей"
// - "генерируй reels", "make a reel"

import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/agent/eliza.{
  type Action, type ActionContext, type ActionResult, Action,
  success_result_with_data, error_result,
}
import vibee/video/pipeline
import vibee/mcp/config

// ============================================================
// Reels Action Definition
// ============================================================

/// Create the Reels Action
pub fn create_reels_action() -> Action {
  Action(
    name: "CREATE_REELS",
    description: "Creates Instagram-style reels video with AI-generated content. " <>
      "Uses user's photo for avatar, generates script, TTS, lipsync, and B-roll.",

    // Similes for NLP matching - all phrases that should trigger this action
    similes: [
      "создай рилс", "сделай рилс", "рилс видео",
      "создай видео для инстаграм", "видео для инсты",
      "make a reel", "create reels", "instagram video",
      "сгенерируй видео", "хочу рилс", "нужен рилс",
      "контент для reels", "reels creator",
    ],

    // Example messages that trigger this action
    examples: [
      "Создай рилс про продуктивность",
      "Хочу видео для инстаграма о бизнесе",
      "Сделай мне reels на тему заработка",
      "Make a reel about productivity tips",
      "Нужен рилс для моего продукта",
    ],

    validate: validate_reels_request,
    handler: handle_reels_creation,
  )
}

// ============================================================
// Validate Function
// ============================================================

/// Validate if message context indicates reels creation intent
/// Returns True if ANY of the similes match in message
fn validate_reels_request(context: ActionContext) -> Bool {
  let message = string.lowercase(context.message)

  // Check for explicit reels keywords
  let reels_keywords = [
    "рилс", "reels", "рилз",
    "видео для инста", "video for insta",
    "видео для соцсет", "social media video",
  ]

  // Check for creation intent
  let creation_intent = [
    "создай", "сделай", "сгенерируй", "хочу",
    "create", "make", "generate", "need",
  ]

  // Must have BOTH: reels keyword AND creation intent
  let has_reels = list.any(reels_keywords, fn(kw) {
    string.contains(message, kw)
  })

  let has_intent = list.any(creation_intent, fn(intent) {
    string.contains(message, intent)
  })

  has_reels && has_intent
}

// ============================================================
// Handler Function
// ============================================================

/// Handle reels creation - full pipeline execution
fn handle_reels_creation(context: ActionContext) -> ActionResult {
  io.println("[REELS_ACTION] Handler started for user: " <> context.chat_id)

  // Extract topic/idea from message
  let idea = extract_idea_from_message(context.message)
  io.println("[REELS_ACTION] Extracted idea: " <> idea)

  // Detect niche from message
  let niche = detect_niche(context.message)
  io.println("[REELS_ACTION] Detected niche: " <> niche)

  // Get photo URL from context or use default
  let photo_url = case context.photo_url {
    Some(url) -> url
    None -> {
      let test_assets = config.get_env_or("TEST_ASSETS_URL", "https://fly.storage.tigris.dev/vibee-assets")
      test_assets <> "/photos/avatar_test.jpg"
    }
  }

  // Generate script from idea
  let script = generate_script(idea, niche)
  io.println("[REELS_ACTION] Generated script: " <> string.slice(script, 0, 50) <> "...")

  // Configure pipeline
  let remotion_url = config.get_env_or("REMOTION_URL", "https://vibee-remotion.fly.dev")
  let test_assets = config.get_env_or("TEST_ASSETS_URL", "https://fly.storage.tigris.dev/vibee-assets")

  let pipeline_config = pipeline.PipelineConfig(
    elevenlabs_api_key: config.get_env_or("ELEVENLABS_API_KEY", ""),
    fal_api_key: config.get_env_or("FAL_API_KEY", ""),
    remotion_url: remotion_url,
    test_assets_url: test_assets,
  )

  let pipeline_request = pipeline.PipelineRequest(
    photo_url: photo_url,
    script_text: script,
    voice_id: None,
    webhook_url: None,
    test_mode: False,  // Use real AI pipeline
  )

  // Start pipeline
  io.println("[REELS_ACTION] Starting AI pipeline...")

  case pipeline.start_ai_pipeline(pipeline_config, pipeline_request) {
    Ok(job) -> {
      io.println("[REELS_ACTION] Pipeline started, job_id: " <> job.id)

      // Poll for completion
      case poll_render_job(remotion_url, job) {
        Ok(video_url) -> {
          io.println("[REELS_ACTION] Video ready: " <> video_url)

          let data = dict.new()
            |> dict.insert("video_url", video_url)
            |> dict.insert("job_id", job.id)
            |> dict.insert("idea", idea)
            |> dict.insert("niche", niche)

          success_result_with_data(
            "Ваш рилс готов! Тема: " <> idea,
            data,
          )
        }
        Error(err) -> {
          io.println("[REELS_ACTION] Render failed: " <> err)
          error_result("Ошибка рендеринга: " <> err)
        }
      }
    }
    Error(err) -> {
      let err_str = pipeline_error_to_string(err)
      io.println("[REELS_ACTION] Pipeline error: " <> err_str)
      error_result("Ошибка пайплайна: " <> err_str)
    }
  }
}

// ============================================================
// Helper Functions
// ============================================================

/// Extract idea/topic from message
fn extract_idea_from_message(message: String) -> String {
  // Remove common prefixes
  let cleaned = message
    |> string.lowercase()
    |> remove_prefix("создай рилс")
    |> remove_prefix("сделай рилс")
    |> remove_prefix("хочу рилс")
    |> remove_prefix("create reels")
    |> remove_prefix("make a reel")
    |> remove_prefix("про ")
    |> remove_prefix("о ")
    |> remove_prefix("на тему ")
    |> remove_prefix("about ")
    |> string.trim()

  case cleaned {
    "" -> "5 полезных советов для продуктивности"  // Default idea
    idea -> idea
  }
}

fn remove_prefix(text: String, prefix: String) -> String {
  case string.starts_with(text, prefix) {
    True -> string.drop_start(text, string.length(prefix))
    False -> text
  }
}

/// Detect niche from message keywords
fn detect_niche(message: String) -> String {
  let msg = string.lowercase(message)

  let niches = [
    #(["бизнес", "business"], "business"),
    #(["продуктив", "productiv"], "productivity"),
    #(["здоров", "health"], "health"),
    #(["финанс", "finance"], "finance"),
    #(["технолог", "tech"], "technology"),
    #(["образован", "educat"], "education"),
    #(["маркетинг", "marketing"], "marketing"),
  ]

  find_niche(msg, niches)
}

fn find_niche(text: String, niches: List(#(List(String), String))) -> String {
  case niches {
    [] -> "lifestyle"  // Default niche
    [#(keywords, niche), ..rest] -> {
      case list.any(keywords, fn(kw) { string.contains(text, kw) }) {
        True -> niche
        False -> find_niche(text, rest)
      }
    }
  }
}

/// Generate script from idea and niche
fn generate_script(idea: String, niche: String) -> String {
  // TODO: Use OpenRouter AI to generate more creative scripts
  // For now, create a simple template
  let intro = case niche {
    "business" -> "Вот что должен знать каждый предприниматель..."
    "productivity" -> "Хотите быть продуктивнее? Вот мой секрет..."
    "health" -> "Ваше здоровье - главный приоритет..."
    "finance" -> "Деньги любят счёт. И вот почему..."
    "technology" -> "Технологии меняют мир. А вы готовы?"
    "education" -> "Учиться никогда не поздно..."
    "marketing" -> "Маркетинг - это не реклама, это стратегия..."
    _ -> "Сегодня я расскажу вам кое-что важное..."
  }

  intro <> "\n\n" <> idea <> "\n\nПодписывайтесь и ставьте лайк!"
}

/// Poll render job for completion
fn poll_render_job(
  remotion_url: String,
  job: pipeline.PipelineJob,
) -> Result(String, String) {
  poll_loop(remotion_url, job.id, 0, 60)  // Max 60 attempts
}

fn poll_loop(
  remotion_url: String,
  job_id: String,
  attempt: Int,
  max_attempts: Int,
) -> Result(String, String) {
  case attempt >= max_attempts {
    True -> Error("Timeout: render job took too long")
    False -> {
      // Wait before polling
      sleep(3000)

      // Check status
      case get_render_status(remotion_url, job_id) {
        Ok(status) -> {
          case status.completed {
            True -> {
              case status.video_url {
                Some(url) -> Ok(url)
                None -> Error("Render completed but no video URL")
              }
            }
            False -> {
              case status.error {
                Some(err) -> Error(err)
                None -> poll_loop(remotion_url, job_id, attempt + 1, max_attempts)
              }
            }
          }
        }
        Error(err) -> Error(err)
      }
    }
  }
}

/// Render status from remotion
pub type RenderStatus {
  RenderStatus(
    completed: Bool,
    video_url: Option(String),
    error: Option(String),
  )
}

/// Get render status from remotion
fn get_render_status(remotion_url: String, job_id: String) -> Result(RenderStatus, String) {
  // TODO: Implement actual HTTP call to remotion
  // For now, simulate successful completion after a few polls
  Ok(RenderStatus(
    completed: True,
    video_url: Some(remotion_url <> "/output/" <> job_id <> ".mp4"),
    error: None,
  ))
}

fn pipeline_error_to_string(err: pipeline.PipelineError) -> String {
  case err {
    pipeline.TTSError(msg) -> "TTS error: " <> msg
    pipeline.LipsyncError(msg) -> "Lipsync error: " <> msg
    pipeline.RenderError(msg) -> "Render error: " <> msg
    pipeline.ConfigError(msg) -> "Config error: " <> msg
    pipeline.NetworkError(msg) -> "Network error: " <> msg
    pipeline.BRollError(msg) -> "B-roll error: " <> msg
  }
}

@external(erlang, "timer", "sleep")
fn sleep(ms: Int) -> Nil
