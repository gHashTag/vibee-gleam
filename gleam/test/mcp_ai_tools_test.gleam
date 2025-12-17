// MCP AI Tools Tests - TDD approach
// Tests for AI-related MCP tools
// Run with: gleam test

import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import vibee/mcp/ai_tools
import vibee/mcp/types.{Tool, TextContent}

pub fn main() {
  gleeunit.main()
}

// ============================================================
// Tool Definition Tests
// ============================================================

pub fn elevenlabs_tts_tool_definition_test() {
  let tool = ai_tools.elevenlabs_tts_tool()

  should.equal(tool.name, "ai_elevenlabs_tts")
  should.be_true(string.contains(tool.description, "text"))
  should.be_true(string.contains(tool.description, "speech"))
}

pub fn hedra_create_avatar_tool_definition_test() {
  let tool = ai_tools.hedra_create_avatar_tool()

  should.equal(tool.name, "ai_hedra_create_avatar")
  should.be_true(string.contains(tool.description, "avatar"))
}

pub fn hedra_get_status_tool_definition_test() {
  let tool = ai_tools.hedra_get_status_tool()

  should.equal(tool.name, "ai_hedra_get_status")
  should.be_true(string.contains(tool.description, "status"))
}

pub fn bfl_generate_image_tool_definition_test() {
  let tool = ai_tools.bfl_generate_image_tool()

  should.equal(tool.name, "ai_bfl_generate_image")
  should.be_true(string.contains(tool.description, "image"))
}

pub fn bfl_get_result_tool_definition_test() {
  let tool = ai_tools.bfl_get_result_tool()

  should.equal(tool.name, "ai_bfl_get_result")
  should.be_true(string.contains(tool.description, "result"))
}

pub fn kling_create_video_tool_definition_test() {
  let tool = ai_tools.kling_create_video_tool()

  should.equal(tool.name, "ai_kling_create_video")
  should.be_true(string.contains(tool.description, "video"))
}

pub fn kling_image_to_video_tool_definition_test() {
  let tool = ai_tools.kling_image_to_video_tool()

  should.equal(tool.name, "ai_kling_image_to_video")
  should.be_true(string.contains(tool.description, "image"))
}

pub fn kling_get_task_tool_definition_test() {
  let tool = ai_tools.kling_get_task_tool()

  should.equal(tool.name, "ai_kling_get_task")
  should.be_true(string.contains(tool.description, "task"))
}

pub fn heygen_create_video_tool_definition_test() {
  let tool = ai_tools.heygen_create_video_tool()

  should.equal(tool.name, "ai_heygen_create_video")
  should.be_true(string.contains(tool.description, "video"))
}

pub fn heygen_list_avatars_tool_definition_test() {
  let tool = ai_tools.heygen_list_avatars_tool()

  should.equal(tool.name, "ai_heygen_list_avatars")
  should.be_true(string.contains(tool.description, "avatar"))
}

pub fn heygen_get_video_status_tool_definition_test() {
  let tool = ai_tools.heygen_get_video_status_tool()

  should.equal(tool.name, "ai_heygen_get_video_status")
  should.be_true(string.contains(tool.description, "status"))
}

// ============================================================
// All AI Tools List Test
// ============================================================

pub fn all_ai_tools_test() {
  let tools = ai_tools.all_ai_tools()

  // Should have at least 11 tools
  should.be_true(list.length(tools) >= 11)

  // All tools should have ai_ prefix
  should.be_true(list.all(tools, fn(t) { string.starts_with(t.name, "ai_") }))
}

// ============================================================
// AI Tool Category Test
// ============================================================

pub fn ai_tool_category_test() {
  // All AI tools should be in the AI category
  let tools = ai_tools.all_ai_tools()
  let tool_names = list.map(tools, fn(t) { t.name })

  should.be_true(list.all(tool_names, fn(name) {
    string.starts_with(name, "ai_")
  }))
}
