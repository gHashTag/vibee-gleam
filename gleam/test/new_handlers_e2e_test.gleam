// E2E Tests for New Handlers
// Tests: Cache Layer, Webhook Integration, Remotion Tools
// Run with: gleam test

import gleam/json
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import vibee/mcp/cache
import vibee/mcp/tools
import vibee/mcp/types

pub fn main() {
  gleeunit.main()
}

// ============================================================
// Cache Layer Tests
// ============================================================

pub fn cache_is_cacheable_ai_tools_test() {
  // AI tools should be cacheable
  should.be_true(cache.is_cacheable("ai_heygen_list_avatars"))
  should.be_true(cache.is_cacheable("ai_heygen_list_voices"))
  should.be_true(cache.is_cacheable("ai_elevenlabs_list_voices"))
  should.be_true(cache.is_cacheable("ai_hedra_list_jobs"))
  should.be_true(cache.is_cacheable("ai_kling_list_tasks"))
  should.be_true(cache.is_cacheable("ai_kieai_list_videos"))

  // Status check tools should be cacheable (short TTL)
  should.be_true(cache.is_cacheable("ai_hedra_get_status"))
  should.be_true(cache.is_cacheable("ai_heygen_get_video_status"))
  should.be_true(cache.is_cacheable("ai_kling_get_task"))
  should.be_true(cache.is_cacheable("ai_kieai_get_status"))
  should.be_true(cache.is_cacheable("ai_bfl_get_result"))
}

pub fn cache_is_not_cacheable_mutation_tools_test() {
  // Mutation tools should NOT be cacheable
  should.be_false(cache.is_cacheable("ai_elevenlabs_tts"))
  should.be_false(cache.is_cacheable("ai_elevenlabs_clone_voice"))
  should.be_false(cache.is_cacheable("ai_hedra_create_avatar"))
  should.be_false(cache.is_cacheable("ai_kling_create_video"))
  should.be_false(cache.is_cacheable("ai_heygen_create_video"))
  should.be_false(cache.is_cacheable("ai_bfl_generate_image"))
}

pub fn cache_ttl_list_operations_test() {
  // List operations should have 1 hour TTL
  should.equal(cache.get_tool_ttl("ai_heygen_list_avatars"), 3600)
  should.equal(cache.get_tool_ttl("ai_heygen_list_voices"), 3600)
  should.equal(cache.get_tool_ttl("ai_elevenlabs_list_voices"), 3600)
}

pub fn cache_ttl_status_operations_test() {
  // Status operations should have 30 second TTL
  should.equal(cache.get_tool_ttl("ai_hedra_get_status"), 30)
  should.equal(cache.get_tool_ttl("ai_heygen_get_video_status"), 30)
  should.equal(cache.get_tool_ttl("ai_kling_get_task"), 30)
  should.equal(cache.get_tool_ttl("ai_kieai_get_status"), 30)
  should.equal(cache.get_tool_ttl("ai_bfl_get_result"), 30)
}

pub fn cache_make_key_test() {
  let args1 = json.object([#("job_id", json.string("job_123"))])
  let args2 = json.object([#("job_id", json.string("job_456"))])

  let key1 = cache.make_cache_key("ai_hedra_get_status", args1)
  let key2 = cache.make_cache_key("ai_hedra_get_status", args2)

  // Different args should produce different keys
  should.not_equal(key1, key2)

  // Same args should produce same key
  let key1_again = cache.make_cache_key("ai_hedra_get_status", args1)
  should.equal(key1, key1_again)

  // Keys should start with tool name
  should.be_true(string.starts_with(key1, "ai_hedra_get_status:"))
}

pub fn cache_with_cache_non_cacheable_test() {
  // For non-cacheable tools, with_cache should just call the function
  let call_count = 0

  let result1 = cache.with_cache("ai_elevenlabs_tts", json.object([]), fn() {
    "result_1"
  })

  let result2 = cache.with_cache("ai_elevenlabs_tts", json.object([]), fn() {
    "result_2"
  })

  // Each call should produce different results (no caching)
  should.equal(result1, "result_1")
  should.equal(result2, "result_2")
}

// ============================================================
// Remotion Tools Registration Tests
// ============================================================

pub fn remotion_tools_registered_test() {
  let registry = tools.init_registry()
  let tool_names = tools.list_tools(registry)

  // Remotion render tools should be registered
  should.be_true(list.contains(tool_names, "remotion_render_video"))
  should.be_true(list.contains(tool_names, "remotion_render_still"))
  should.be_true(list.contains(tool_names, "remotion_get_render_status"))
  should.be_true(list.contains(tool_names, "remotion_list_compositions"))
}

pub fn remotion_template_tools_registered_test() {
  let registry = tools.init_registry()
  let tool_names = tools.list_tools(registry)

  // Template management tools should be registered
  should.be_true(list.contains(tool_names, "template_list"))
  should.be_true(list.contains(tool_names, "template_read"))
  should.be_true(list.contains(tool_names, "template_create"))
  should.be_true(list.contains(tool_names, "template_modify"))
  should.be_true(list.contains(tool_names, "template_preview"))
  should.be_true(list.contains(tool_names, "props_update"))
}

pub fn remotion_list_compositions_test() {
  let registry = tools.init_registry()
  let args = json.object([])

  let result = tools.execute_tool(registry, "remotion_list_compositions", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "Compositions"))
}

pub fn remotion_template_list_test() {
  let registry = tools.init_registry()
  let args = json.object([])

  let result = tools.execute_tool(registry, "template_list", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "Templates"))
}

pub fn remotion_render_video_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("composition_id", json.string("TextOverlay")),
    #("title", json.string("Test Title")),
  ])

  let result = tools.execute_tool(registry, "remotion_render_video", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "TextOverlay"))
  should.be_true(string.contains(content, "Payload"))
}

pub fn remotion_render_still_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("composition_id", json.string("TextOverlay")),
    #("frame", json.int(0)),
  ])

  let result = tools.execute_tool(registry, "remotion_render_still", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "Still render"))
}

pub fn remotion_category_test() {
  // Remotion tools should be in the remotion category
  should.equal(tools.get_tool_category("remotion_render_video"), tools.CategoryRemotion)
  should.equal(tools.get_tool_category("template_list"), tools.CategoryRemotion)
  should.equal(tools.get_tool_category("template_create"), tools.CategoryRemotion)
  should.equal(tools.get_tool_category("props_update"), tools.CategoryRemotion)
}

pub fn remotion_category_string_test() {
  should.equal(tools.category_to_string(tools.CategoryRemotion), "remotion")

  case tools.parse_category("remotion") {
    Ok(cat) -> should.equal(cat, tools.CategoryRemotion)
    Error(_) -> should.fail()
  }
}

// ============================================================
// Tool Count Test
// ============================================================

pub fn total_tools_count_test() {
  let registry = tools.init_registry()
  let tool_names = tools.list_tools(registry)

  // Should have at least 50 tools now with all additions
  // (36 original + AI tools + RAG tools + Task tools + Storage + Remotion)
  should.be_true(list.length(tool_names) >= 50)
}

// ============================================================
// Helper Functions
// ============================================================

fn get_result_text(result: types.ToolResult) -> String {
  case result.content {
    [types.TextContent(text), ..] -> text
    _ -> ""
  }
}
