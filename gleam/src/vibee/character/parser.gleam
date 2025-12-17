// Character JSON parser for VIBEE
// Compatible with ElizaOS character file format

import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/types.{
  type AgentConfig, type Tone, type Language,
  AgentConfig, Friendly, Professional, Casual, CustomTone,
  En, Ru, CustomLanguage,
}

/// Character definition from JSON file
pub type Character {
  Character(
    id: Option(String),
    name: String,
    tone: Tone,
    language: Language,
    system_prompt: Option(String),
    bio: Option(String),
    lore: List(String),
    plugins: List(String),
  )
}

/// Parse error
pub type ParseError {
  InvalidJson(String)
  MissingField(String)
}

/// Parse a character from JSON string
pub fn parse_json(json_string: String) -> Result(Character, ParseError) {
  case json.parse(json_string, character_decoder()) {
    Ok(character) -> Ok(character)
    Error(err) -> Error(InvalidJson(string_from_json_error(err)))
  }
}

/// Convert Character to AgentConfig
pub fn to_agent_config(character: Character) -> AgentConfig {
  let id = option.unwrap(character.id, generate_id(character.name))

  AgentConfig(
    id: id,
    name: character.name,
    tone: character.tone,
    language: character.language,
    system_prompt: build_system_prompt(character),
    history_limit: 100,
  )
}

/// Build system prompt from character
fn build_system_prompt(character: Character) -> Option(String) {
  let parts = []

  // Add base prompt if exists
  let parts = case character.system_prompt {
    Some(prompt) -> [prompt, ..parts]
    None -> parts
  }

  // Add bio if exists
  let parts = case character.bio {
    Some(bio) -> ["Bio: " <> bio, ..parts]
    None -> parts
  }

  // Add lore
  let parts = case character.lore {
    [] -> parts
    lore -> {
      let lore_text = "Background:\n" <> string.join(lore, "\n")
      [lore_text, ..parts]
    }
  }

  case parts {
    [] -> None
    _ -> Some(string.join(list.reverse(parts), "\n\n"))
  }
}

// Decoders using new API

fn character_decoder() -> decode.Decoder(Character) {
  {
    use id <- decode.optional_field("id", None, decode.optional(decode.string))
    use name <- decode.field("name", decode.string)
    use tone_str <- decode.optional_field("tone", None, decode.optional(decode.string))
    use lang_str <- decode.optional_field("language", None, decode.optional(decode.string))
    use system_prompt <- decode.optional_field("system", None, decode.optional(decode.string))
    use bio <- decode.optional_field("bio", None, decode.optional(decode.string))
    use lore <- decode.optional_field("lore", [], decode.list(decode.string))
    use plugins <- decode.optional_field("plugins", [], decode.list(decode.string))

    let tone = case tone_str {
      Some(t) -> string_to_tone(t)
      None -> Friendly
    }
    let language = case lang_str {
      Some(l) -> string_to_language(l)
      None -> En
    }

    decode.success(Character(id, name, tone, language, system_prompt, bio, lore, plugins))
  }
}

// Helper functions

fn string_to_tone(s: String) -> Tone {
  case s {
    "friendly" -> Friendly
    "professional" -> Professional
    "casual" -> Casual
    custom -> CustomTone(custom)
  }
}

fn string_to_language(s: String) -> Language {
  case s {
    "en" -> En
    "ru" -> Ru
    custom -> CustomLanguage(custom)
  }
}

fn generate_id(name: String) -> String {
  name
  |> string.lowercase()
  |> string.replace(" ", "-")
  |> string.replace("_", "-")
}

fn string_from_json_error(err: json.DecodeError) -> String {
  case err {
    json.UnexpectedEndOfInput -> "Unexpected end of input"
    json.UnexpectedByte(byte) -> "Unexpected byte: " <> byte
    json.UnexpectedSequence(seq) -> "Unexpected sequence: " <> seq
    json.UnableToDecode(errors) -> string_from_decode_errors(errors)
  }
}

fn string_from_decode_errors(errors: List(decode.DecodeError)) -> String {
  case errors {
    [] -> "Unknown decode error"
    [first, ..] -> "Decode error at " <> string.join(first.path, ".") <> ": expected " <> first.expected <> ", got " <> first.found
  }
}
