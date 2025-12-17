import gleeunit
import gleeunit/should
import gleam/option
import vibee/types
import vibee/error
import vibee/character/parser
import vibee/knowledge/types as knowledge_types
import vibee/knowledge/provider
import vibee/knowledge/search

pub fn main() {
  gleeunit.main()
}

// Test Tone type
pub fn tone_friendly_test() {
  let tone = types.Friendly
  should.be_true(case tone {
    types.Friendly -> True
    _ -> False
  })
}

pub fn tone_custom_test() {
  let tone = types.CustomTone("sarcastic")
  should.be_true(case tone {
    types.CustomTone("sarcastic") -> True
    _ -> False
  })
}

// Test Language type
pub fn language_en_test() {
  let lang = types.En
  should.be_true(case lang {
    types.En -> True
    _ -> False
  })
}

pub fn language_ru_test() {
  let lang = types.Ru
  should.be_true(case lang {
    types.Ru -> True
    _ -> False
  })
}

// Test error to_string
pub fn error_agent_not_found_test() {
  let err = error.AgentNotFound("agent-123")
  let str = error.to_string(err)
  should.equal(str, "Agent not found: agent-123")
}

pub fn error_telegram_error_test() {
  let err = error.TelegramError(401, "Unauthorized")
  let str = error.to_string(err)
  should.equal(str, "Telegram error 401: Unauthorized")
}

// Test AgentConfig creation
pub fn agent_config_test() {
  let config =
    types.AgentConfig(
      id: "test-agent",
      name: "Test Agent",
      tone: types.Friendly,
      language: types.En,
      system_prompt: option.None,
      history_limit: 100,
    )
  should.equal(config.id, "test-agent")
  should.equal(config.name, "Test Agent")
}

// ============================================
// Character Parser Tests
// ============================================

pub fn parse_minimal_character_test() {
  let json = "{\"name\": \"TestBot\"}"
  let result = parser.parse_json(json)

  should.be_ok(result)
  let assert Ok(character) = result
  should.equal(character.name, "TestBot")
}

pub fn parse_full_character_test() {
  let json = "
  {
    \"id\": \"bot-123\",
    \"name\": \"HelperBot\",
    \"tone\": \"professional\",
    \"language\": \"ru\",
    \"system\": \"You are a helpful assistant\",
    \"bio\": \"A friendly bot\",
    \"lore\": [\"Created in 2024\", \"Lives in the cloud\"],
    \"plugins\": [\"telegram\", \"knowledge\"]
  }
  "

  let result = parser.parse_json(json)
  should.be_ok(result)

  let assert Ok(character) = result
  should.equal(character.name, "HelperBot")
  should.equal(character.id, option.Some("bot-123"))
  should.equal(character.bio, option.Some("A friendly bot"))
}

pub fn parse_character_default_tone_test() {
  let json = "{\"name\": \"DefaultBot\"}"
  let result = parser.parse_json(json)

  let assert Ok(character) = result
  // Default tone should be Friendly
  should.be_true(case character.tone {
    types.Friendly -> True
    _ -> False
  })
}

pub fn parse_character_custom_tone_test() {
  let json = "{\"name\": \"CustomBot\", \"tone\": \"sarcastic\"}"
  let result = parser.parse_json(json)

  let assert Ok(character) = result
  should.be_true(case character.tone {
    types.CustomTone("sarcastic") -> True
    _ -> False
  })
}

pub fn character_to_agent_config_test() {
  let character = parser.Character(
    id: option.Some("my-agent"),
    name: "MyAgent",
    tone: types.Professional,
    language: types.Ru,
    system_prompt: option.Some("Be helpful"),
    bio: option.Some("Test bio"),
    lore: ["Fact 1", "Fact 2"],
    plugins: ["telegram"],
  )

  let config = parser.to_agent_config(character)
  should.equal(config.id, "my-agent")
  should.equal(config.name, "MyAgent")
  should.equal(config.history_limit, 100)
}

pub fn character_generate_id_test() {
  // When id is None, it should be generated from name
  let character = parser.Character(
    id: option.None,
    name: "My Cool Agent",
    tone: types.Friendly,
    language: types.En,
    system_prompt: option.None,
    bio: option.None,
    lore: [],
    plugins: [],
  )

  let config = parser.to_agent_config(character)
  // Name "My Cool Agent" should become "my-cool-agent"
  should.equal(config.id, "my-cool-agent")
}

pub fn parse_invalid_json_test() {
  let json = "not valid json"
  let result = parser.parse_json(json)

  should.be_error(result)
}

// ============================================
// Knowledge System Tests
// ============================================

pub fn knowledge_provider_new_test() {
  let kp = provider.new("/path/to/knowledge")
  should.equal(provider.document_count(kp), 0)
}

pub fn knowledge_provider_add_document_test() {
  let doc = knowledge_types.Document(
    id: "doc-1",
    title: "Test Document",
    content: "This is test content about Gleam programming",
    source: "/docs/test.md",
    tags: ["test", "gleam"],
  )

  let kp = provider.new("/path")
  let kp_with_doc = provider.add_document(kp, doc)

  should.equal(provider.document_count(kp_with_doc), 1)
}

pub fn knowledge_provider_search_test() {
  let doc1 = knowledge_types.Document(
    id: "doc-1",
    title: "Gleam Tutorial",
    content: "Learn Gleam programming language basics",
    source: "/docs/gleam.md",
    tags: ["gleam", "tutorial"],
  )

  let doc2 = knowledge_types.Document(
    id: "doc-2",
    title: "Erlang OTP",
    content: "Understanding OTP and supervision trees",
    source: "/docs/otp.md",
    tags: ["erlang", "otp"],
  )

  let kp =
    provider.new("/path")
    |> provider.add_document(doc1)
    |> provider.add_document(doc2)

  let results = provider.search(kp, "gleam programming")
  should.be_true(list.length(results) > 0)
}

pub fn knowledge_simple_search_test() {
  let doc1 = knowledge_types.Document(
    id: "doc-1",
    title: "VIBEE Framework",
    content: "VIBEE is a Gleam-based agent framework for BEAM",
    source: "/docs/vibee.md",
    tags: ["vibee"],
  )

  let doc2 = knowledge_types.Document(
    id: "doc-2",
    title: "Other Topic",
    content: "Something completely unrelated",
    source: "/docs/other.md",
    tags: ["other"],
  )

  let results = search.simple_search([doc1, doc2], "VIBEE agent", 5)
  should.be_true(list.length(results) >= 1)

  // First result should be the VIBEE document
  let assert Ok(first) = list.first(results)
  should.equal(first.document.id, "doc-1")
}

pub fn knowledge_build_context_test() {
  let doc = knowledge_types.Document(
    id: "doc-1",
    title: "Test Doc",
    content: "Test content",
    source: "/test.md",
    tags: [],
  )

  let result = knowledge_types.SearchResult(
    document: doc,
    score: 1.0,
    matched_terms: ["test"],
  )

  let context = provider.build_context([result])
  should.be_true(string.contains(context, "Test Doc"))
  should.be_true(string.contains(context, "Relevant Knowledge"))
}

import gleam/list
import gleam/string
