// Reels Evaluator (ElizaOS Architecture)
//
// Post-processes reels creation results:
// - Extracts insights for memory
// - Determines follow-up actions
// - Tracks user preferences for future recommendations

import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/agent/eliza.{
  type ActionContext, type ActionResult, type Evaluator, type EvaluatorResult,
  Evaluator, EvaluatorResult,
}

// ============================================================
// Reels Evaluator
// ============================================================

/// Create the Reels Evaluator
pub fn create_reels_evaluator() -> Evaluator {
  Evaluator(
    name: "reels_evaluator",
    description: "Analyzes reels creation results, extracts insights, suggests follow-ups",
    handler: evaluate_reels_result,
  )
}

/// Evaluate reels creation result
fn evaluate_reels_result(
  context: ActionContext,
  result: ActionResult,
) -> EvaluatorResult {
  io.println("[REELS_EVALUATOR] Evaluating result for user: " <> context.chat_id)

  case result.success {
    True -> evaluate_success(context, result)
    False -> evaluate_failure(context, result)
  }
}

/// Evaluate successful reels creation
fn evaluate_success(
  context: ActionContext,
  result: ActionResult,
) -> EvaluatorResult {
  // Extract data from result
  let idea = get_string_from_data(result.data, "idea")
  let niche = get_string_from_data(result.data, "niche")
  let video_url = get_string_from_data(result.data, "video_url")

  // Build facts to store
  let facts = build_success_facts(context, idea, niche, video_url)

  // Suggest follow-up actions
  let follow_ups = suggest_follow_ups(niche)

  io.println("[REELS_EVALUATOR] Success! Facts: " <> string.inspect(list.length(facts)))

  EvaluatorResult(
    store_memory: True,  // Store successful creations
    facts: facts,
    suggested_actions: follow_ups,
  )
}

/// Evaluate failed reels creation
fn evaluate_failure(
  context: ActionContext,
  result: ActionResult,
) -> EvaluatorResult {
  let error = result.text

  // Extract failure reason
  let failure_type = categorize_failure(error)

  // Build facts about failure
  let facts = [
    "Reels creation failed for user " <> context.chat_id,
    "Failure type: " <> failure_type,
    "Error: " <> error,
  ]

  // Suggest recovery actions
  let recovery_actions = suggest_recovery_actions(failure_type)

  io.println("[REELS_EVALUATOR] Failure: " <> failure_type)

  EvaluatorResult(
    store_memory: True,  // Store failures to avoid repeating
    facts: facts,
    suggested_actions: recovery_actions,
  )
}

// ============================================================
// Fact Extraction
// ============================================================

/// Build facts from successful reels creation
fn build_success_facts(
  context: ActionContext,
  idea: String,
  niche: String,
  video_url: String,
) -> List(String) {
  let base_facts = [
    "User " <> context.chat_id <> " created a reels video",
    "Topic: " <> idea,
    "Niche: " <> niche,
  ]

  // Add user name if available
  let with_name = case context.user_name {
    Some(name) -> [name <> " likes " <> niche <> " content", ..base_facts]
    None -> base_facts
  }

  // Add video URL fact
  case video_url {
    "" -> with_name
    url -> ["Video URL: " <> url, ..with_name]
  }
}

/// Categorize failure type for analysis
fn categorize_failure(error: String) -> String {
  let lower = string.lowercase(error)

  // Check categories in order
  let categories = [
    #(["timeout"], "timeout"),
    #(["tts", "elevenlabs"], "tts_error"),
    #(["lipsync", "fal"], "lipsync_error"),
    #(["render", "remotion"], "render_error"),
    #(["config", "api key"], "config_error"),
    #(["photo", "image"], "photo_error"),
  ]

  find_category(lower, categories)
}

fn find_category(text: String, categories: List(#(List(String), String))) -> String {
  case categories {
    [] -> "unknown_error"
    [#(keywords, category), ..rest] -> {
      case list.any(keywords, fn(kw) { string.contains(text, kw) }) {
        True -> category
        False -> find_category(text, rest)
      }
    }
  }
}

// ============================================================
// Follow-up Suggestions
// ============================================================

/// Suggest follow-up actions based on niche
fn suggest_follow_ups(niche: String) -> List(String) {
  case niche {
    "business" -> [
      "SHARE_TO_LINKEDIN",
      "CREATE_CAROUSEL",
      "SUGGEST_RELATED_TOPIC",
    ]
    "productivity" -> [
      "CREATE_CHECKLIST",
      "SUGGEST_SERIES",
      "SCHEDULE_REMINDER",
    ]
    "health" -> [
      "SHARE_TO_INSTAGRAM",
      "CREATE_WORKOUT",
      "SUGGEST_MEAL_PLAN",
    ]
    "finance" -> [
      "CREATE_CALCULATOR",
      "SHARE_TO_TWITTER",
      "SUGGEST_INVESTMENT_TIP",
    ]
    "technology" -> [
      "CREATE_TUTORIAL",
      "SHARE_TO_YOUTUBE",
      "SUGGEST_CODE_SNIPPET",
    ]
    _ -> [
      "SHARE_VIDEO",
      "CREATE_ANOTHER",
      "GET_ANALYTICS",
    ]
  }
}

/// Suggest recovery actions for failures
fn suggest_recovery_actions(failure_type: String) -> List(String) {
  case failure_type {
    "timeout" -> [
      "RETRY_WITH_SHORTER_SCRIPT",
      "TRY_SIMPLER_TEMPLATE",
      "CHECK_SERVICE_STATUS",
    ]
    "tts_error" -> [
      "TRY_DIFFERENT_VOICE",
      "SHORTEN_SCRIPT",
      "CHECK_ELEVENLABS_QUOTA",
    ]
    "lipsync_error" -> [
      "USE_DIFFERENT_PHOTO",
      "TRY_LOWER_RESOLUTION",
      "CHECK_FAL_STATUS",
    ]
    "render_error" -> [
      "RETRY_RENDER",
      "USE_SIMPLER_TEMPLATE",
      "CHECK_REMOTION_LOGS",
    ]
    "config_error" -> [
      "CHECK_API_KEYS",
      "VERIFY_CONFIGURATION",
      "CONTACT_SUPPORT",
    ]
    "photo_error" -> [
      "UPLOAD_NEW_PHOTO",
      "USE_DEFAULT_AVATAR",
      "CHECK_PHOTO_FORMAT",
    ]
    _ -> [
      "RETRY_ACTION",
      "SIMPLIFY_REQUEST",
      "CONTACT_SUPPORT",
    ]
  }
}

// ============================================================
// Helper Functions
// ============================================================

/// Extract string from data dictionary
fn get_string_from_data(data: dict.Dict(String, String), key: String) -> String {
  case dict.get(data, key) {
    Ok(value) -> value
    Error(_) -> ""
  }
}

// ============================================================
// Analytics Evaluator
// ============================================================

/// Create analytics evaluator for tracking metrics
pub fn create_analytics_evaluator() -> Evaluator {
  Evaluator(
    name: "reels_analytics",
    description: "Tracks reels creation metrics and patterns",
    handler: evaluate_analytics,
  )
}

fn evaluate_analytics(
  context: ActionContext,
  result: ActionResult,
) -> EvaluatorResult {
  // Track metrics
  let metrics = [
    "reels_created_total",
    "reels_success_rate",
    "average_creation_time",
  ]

  // For now, just note that analytics should be tracked
  let facts = case result.success {
    True -> [
      "Increment reels_created_total for user " <> context.chat_id,
      "Update success rate statistics",
    ]
    False -> [
      "Increment reels_failed_total for user " <> context.chat_id,
      "Log failure for debugging",
    ]
  }

  EvaluatorResult(
    store_memory: False,  // Analytics don't need memory storage
    facts: facts,
    suggested_actions: [],
  )
}
