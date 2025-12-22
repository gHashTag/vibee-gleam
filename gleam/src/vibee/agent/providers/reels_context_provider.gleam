// Reels Context Provider (ElizaOS Architecture)
//
// Provides contextual information for reels creation decisions:
// - User history (previous reels, topics)
// - User preferences (niches, style)
// - Current conversation context
// - Available templates and resources

import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/agent/eliza.{
  type ActionContext, type Provider, type ProviderResult, Provider, ProviderResult,
}

// ============================================================
// Reels Context Provider
// ============================================================

/// Create the Reels Context Provider
pub fn create_reels_context_provider() -> Provider {
  Provider(
    name: "reels_context",
    description: "Provides context for reels creation: user history, preferences, templates",
    dynamic: True,  // Refresh each call
    position: 10,   // High priority - runs first
    get: get_reels_context,
  )
}

/// Get reels context for action decisions
fn get_reels_context(context: ActionContext) -> ProviderResult {
  io.println("[REELS_PROVIDER] Gathering context for user: " <> context.chat_id)

  // Extract user preferences from history
  let preferences = extract_user_preferences(context.history)

  // Detect dominant niche
  let dominant_niche = detect_dominant_niche(context.history)

  // Count previous reels requests
  let reels_count = count_reels_requests(context.history)

  // Get available templates
  let templates = get_available_templates()

  // Build values
  let values = dict.new()
    |> dict.insert("dominant_niche", dominant_niche)
    |> dict.insert("reels_count", string.inspect(reels_count))
    |> dict.insert("has_photo", case context.photo_url {
      Some(_) -> "true"
      None -> "false"
    })
    |> dict.insert("user_name", case context.user_name {
      Some(name) -> name
      None -> "unknown"
    })

  // Build data (as strings for simplicity)
  let data = dict.new()
    |> dict.insert("preferred_niches", string.join(preferences.preferred_niches, ","))
    |> dict.insert("preferred_style", case preferences.preferred_style {
      Some(s) -> s
      None -> ""
    })
    |> dict.insert("language", preferences.language)
    |> dict.insert("templates_count", string.inspect(list.length(templates)))

  // Build summary text
  let summary = build_context_summary(
    context.user_name,
    dominant_niche,
    reels_count,
    preferences,
  )

  ProviderResult(
    text: summary,
    values: values,
    data: data,
  )
}

// ============================================================
// Context Extraction Functions
// ============================================================

/// User preferences type
pub type UserPreferences {
  UserPreferences(
    preferred_niches: List(String),
    preferred_style: Option(String),
    language: String,
  )
}

/// Extract preferences from message history
fn extract_user_preferences(history: List(String)) -> UserPreferences {
  let niches = extract_niches_from_history(history)
  let style = detect_style_preference(history)
  let language = detect_language(history)

  UserPreferences(
    preferred_niches: niches,
    preferred_style: style,
    language: language,
  )
}

/// Extract mentioned niches from history
fn extract_niches_from_history(history: List(String)) -> List(String) {
  let all_text = string.join(history, " ") |> string.lowercase()

  let niche_keywords = [
    #("business", ["бизнес", "business", "предприним", "стартап"]),
    #("productivity", ["продуктив", "productivity", "эффектив", "тайм-менедж"]),
    #("health", ["здоров", "health", "фитнес", "спорт", "wellness"]),
    #("finance", ["финанс", "деньг", "инвести", "крипт", "finance", "money"]),
    #("technology", ["технолог", "tech", "программ", "it", "код"]),
    #("education", ["образован", "учеба", "курс", "обучен", "education"]),
    #("marketing", ["маркетинг", "рекла", "smm", "продвиж", "marketing"]),
  ]

  list.filter_map(niche_keywords, fn(item) {
    let #(niche, keywords) = item
    let found = list.any(keywords, fn(kw) {
      string.contains(all_text, kw)
    })
    case found {
      True -> Ok(niche)
      False -> Error(Nil)
    }
  })
}

/// Detect dominant niche from history
fn detect_dominant_niche(history: List(String)) -> String {
  let niches = extract_niches_from_history(history)
  case niches {
    [first, ..] -> first
    [] -> "lifestyle"  // Default
  }
}

/// Detect style preference
fn detect_style_preference(history: List(String)) -> Option(String) {
  let all_text = string.join(history, " ") |> string.lowercase()

  // Check for style keywords
  let styles = [
    #(["энергичн", "dynamic"], "energetic"),
    #(["спокойн", "calm"], "calm"),
    #(["профессион", "professional"], "professional"),
    #(["развлекат", "fun"], "fun"),
  ]

  find_style(all_text, styles)
}

fn find_style(text: String, styles: List(#(List(String), String))) -> Option(String) {
  case styles {
    [] -> None
    [#(keywords, style), ..rest] -> {
      case list.any(keywords, fn(kw) { string.contains(text, kw) }) {
        True -> Some(style)
        False -> find_style(text, rest)
      }
    }
  }
}

/// Detect primary language
fn detect_language(history: List(String)) -> String {
  let all_text = string.join(history, " ")

  // Count Cyrillic vs Latin characters
  let cyrillic_count = count_cyrillic(all_text)
  let total_count = string.length(all_text)

  case total_count > 0 {
    True -> {
      let total_float = int.to_float(total_count)
      let ratio = cyrillic_count /. { total_float +. 0.0001 }
      case ratio >. 0.3 {
        True -> "ru"
        False -> "en"
      }
    }
    False -> "ru"  // Default to Russian
  }
}

fn count_cyrillic(text: String) -> Float {
  // Simple heuristic: count common Cyrillic letters
  let cyrillic = ["а", "о", "е", "и", "н", "т", "с", "р", "в", "л"]
  let lower = string.lowercase(text)
  list.fold(cyrillic, 0.0, fn(acc, char) {
    acc +. count_char_occurrences(lower, char)
  })
}

fn count_char_occurrences(text: String, char: String) -> Float {
  let parts = string.split(text, char)
  let count = list.length(parts) - 1
  int.to_float(count)
}

/// Count reels requests in history
fn count_reels_requests(history: List(String)) -> Int {
  let reels_keywords = ["рилс", "reels", "создай видео", "сделай видео"]

  list.count(history, fn(msg) {
    let lower = string.lowercase(msg)
    list.any(reels_keywords, fn(kw) {
      string.contains(lower, kw)
    })
  })
}

/// Available reels templates
pub type ReelsTemplate {
  ReelsTemplate(
    id: String,
    name: String,
    description: String,
    best_for: List(String),  // Best for which niches
  )
}

/// Get available templates
fn get_available_templates() -> List(ReelsTemplate) {
  [
    ReelsTemplate(
      id: "split-talking-head",
      name: "Split Talking Head",
      description: "Avatar on left, B-roll on right",
      best_for: ["business", "education", "productivity"],
    ),
    ReelsTemplate(
      id: "fullscreen-avatar",
      name: "Fullscreen Avatar",
      description: "Full-screen talking avatar",
      best_for: ["lifestyle", "personal", "storytelling"],
    ),
    ReelsTemplate(
      id: "picture-in-picture",
      name: "Picture in Picture",
      description: "B-roll with avatar in corner",
      best_for: ["technology", "tutorials", "reviews"],
    ),
  ]
}

/// Build human-readable context summary
fn build_context_summary(
  user_name: Option(String),
  dominant_niche: String,
  reels_count: Int,
  preferences: UserPreferences,
) -> String {
  let name = case user_name {
    Some(n) -> n
    None -> "User"
  }

  let niches_text = case preferences.preferred_niches {
    [] -> "no specific niche"
    niches -> string.join(niches, ", ")
  }

  let style_text = case preferences.preferred_style {
    Some(s) -> s <> " style"
    None -> "no specific style"
  }

  name <> " context:\n" <>
  "- Dominant niche: " <> dominant_niche <> "\n" <>
  "- Previous reels: " <> string.inspect(reels_count) <> "\n" <>
  "- Preferred niches: " <> niches_text <> "\n" <>
  "- Style: " <> style_text <> "\n" <>
  "- Language: " <> preferences.language
}
