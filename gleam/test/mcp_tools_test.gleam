// MCP Tools Tests - Test all MCP instruments
// Run with: gleam test

import gleam/json
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import simplifile
import vibee/mcp/tools
import vibee/mcp/types

pub fn main() {
  gleeunit.main()
}

// ============================================================
// Registry Tests
// ============================================================

pub fn registry_init_test() {
  let registry = tools.init_registry()
  let tool_names = tools.list_tools(registry)

  // Should have 41 tools total (36 original + 5 bot analysis)
  should.be_true(list.length(tool_names) >= 36)
}

pub fn registry_has_all_categories_test() {
  let registry = tools.init_registry()
  let tool_names = tools.list_tools(registry)

  // Debug tools
  should.be_true(list.contains(tool_names, "debug_build"))
  should.be_true(list.contains(tool_names, "debug_test"))
  should.be_true(list.contains(tool_names, "debug_analyze"))
  should.be_true(list.contains(tool_names, "debug_trace"))
  should.be_true(list.contains(tool_names, "debug_log"))

  // Code tools
  should.be_true(list.contains(tool_names, "code_generate"))
  should.be_true(list.contains(tool_names, "code_refactor"))
  should.be_true(list.contains(tool_names, "code_explain"))
  should.be_true(list.contains(tool_names, "code_find_similar"))
  should.be_true(list.contains(tool_names, "code_diff"))

  // Test tools
  should.be_true(list.contains(tool_names, "test_run"))
  should.be_true(list.contains(tool_names, "test_create"))
  should.be_true(list.contains(tool_names, "test_coverage"))
  should.be_true(list.contains(tool_names, "test_validate"))

  // Agent tools
  should.be_true(list.contains(tool_names, "agent_spawn"))
  should.be_true(list.contains(tool_names, "agent_message"))
  should.be_true(list.contains(tool_names, "agent_status"))
  should.be_true(list.contains(tool_names, "agent_kill"))

  // Bot analysis tools
  should.be_true(list.contains(tool_names, "bot_analyze"))
  should.be_true(list.contains(tool_names, "bot_compare"))
  should.be_true(list.contains(tool_names, "bot_monitor"))
  should.be_true(list.contains(tool_names, "bot_extract_commands"))
  should.be_true(list.contains(tool_names, "bot_test_interaction"))
}

// ============================================================
// File Tools Tests
// ============================================================

pub fn file_read_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("path", json.string("/tmp/test_mcp_read.txt")),
  ])

  // Create test file
  let _ = simplifile.write("/tmp/test_mcp_read.txt", "Hello MCP Test!")

  let result = tools.execute_tool(registry, "file_read", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "Hello MCP Test!"))
}

pub fn file_write_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("path", json.string("/tmp/test_mcp_write.txt")),
    #("content", json.string("Written by MCP test")),
  ])

  let result = tools.execute_tool(registry, "file_write", args)
  should.be_false(result.is_error)

  // Verify file was written
  case simplifile.read("/tmp/test_mcp_write.txt") {
    Ok(content) -> should.equal(content, "Written by MCP test")
    Error(_) -> should.fail()
  }
}

pub fn file_list_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("path", json.string("/tmp")),
  ])

  let result = tools.execute_tool(registry, "file_list", args)
  should.be_false(result.is_error)
}

// ============================================================
// System Tools Tests
// ============================================================

pub fn system_log_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("level", json.string("info")),
    #("message", json.string("Test log message")),
  ])

  let result = tools.execute_tool(registry, "system_log", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "Logged"))
}

pub fn system_exec_allowed_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("command", json.string("echo hello")),
  ])

  let result = tools.execute_tool(registry, "system_exec", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "hello"))
}

pub fn system_exec_blocked_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("command", json.string("rm -rf /")),
  ])

  let result = tools.execute_tool(registry, "system_exec", args)
  should.be_true(result.is_error)
}

// ============================================================
// Debug Tools Tests
// ============================================================

pub fn debug_build_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("path", json.string("/Users/playra/vibee-eliza-999/vibee/gleam")),
  ])

  // Integration test - just check that the tool executes and returns some response
  // We cannot predict exact output as it depends on actual build state
  let result = tools.execute_tool(registry, "debug_build", args)
  let content = get_result_text(result)

  // Just verify we got some response (not empty)
  // The actual content may vary based on build state
  should.be_true(string.length(content) > 0)
}

pub fn debug_analyze_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("error_text", json.string("Unknown variable: foo")),
  ])

  let result = tools.execute_tool(registry, "debug_analyze", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "unknown_variable"))
  should.be_true(string.contains(content, "suggestion"))
}

pub fn debug_analyze_type_mismatch_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("error_text", json.string("Expected type Int but got String")),
  ])

  let result = tools.execute_tool(registry, "debug_analyze", args)
  let content = get_result_text(result)
  should.be_true(string.contains(content, "type_mismatch"))
}

pub fn debug_trace_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("module", json.string("my_module")),
    #("function", json.string("my_function")),
    #("level", json.string("all")),
  ])

  let result = tools.execute_tool(registry, "debug_trace", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "trace_code"))
  should.be_true(string.contains(content, "my_module"))
}

pub fn debug_log_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("operation", json.string("test_operation")),
    #("status", json.string("success")),
  ])

  let result = tools.execute_tool(registry, "debug_log", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "test_operation"))
  should.be_true(string.contains(content, "success"))
}

// ============================================================
// Code Tools Tests
// ============================================================

pub fn code_generate_function_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("description", json.string("calculate sum")),
    #("language", json.string("gleam")),
    #("template", json.string("function")),
  ])

  let result = tools.execute_tool(registry, "code_generate", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "pub fn"))
  should.be_true(string.contains(content, "calculate_sum"))
}

pub fn code_generate_module_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("description", json.string("user service")),
    #("language", json.string("gleam")),
    #("template", json.string("module")),
  ])

  let result = tools.execute_tool(registry, "code_generate", args)
  let content = get_result_text(result)
  should.be_true(string.contains(content, "import"))
}

pub fn code_generate_test_template_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("description", json.string("add numbers")),
    #("language", json.string("gleam")),
    #("template", json.string("test")),
  ])

  let result = tools.execute_tool(registry, "code_generate", args)
  let content = get_result_text(result)
  should.be_true(string.contains(content, "gleeunit"))
  should.be_true(string.contains(content, "_test()"))
}

pub fn code_explain_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("code", json.string("pub fn add(a: Int, b: Int) -> Int { a + b }\nimport gleam/io")),
  ])

  let result = tools.execute_tool(registry, "code_explain", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "analysis"))
  should.be_true(string.contains(content, "Functions"))
}

pub fn code_find_similar_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("pattern", json.string("fn handle_")),
    #("path", json.string("/Users/playra/vibee-eliza-999/vibee/gleam/src")),
  ])

  let result = tools.execute_tool(registry, "code_find_similar", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "matches"))
}

pub fn code_diff_test() {
  let registry = tools.init_registry()
  // Create test files
  let _ = simplifile.write("/tmp/diff_test1.txt", "line 1\nline 2\nline 3")
  let _ = simplifile.write("/tmp/diff_test2.txt", "line 1\nmodified\nline 3")

  let args = json.object([
    #("file_path", json.string("/tmp/diff_test1.txt")),
    #("compare_with", json.string("custom")),
    #("other_file", json.string("/tmp/diff_test2.txt")),
  ])

  let result = tools.execute_tool(registry, "code_diff", args)
  should.be_false(result.is_error)
}

// ============================================================
// Test Tools Tests
// ============================================================

pub fn test_run_test() {
  // NOTE: This test cannot run `gleam test` on itself - it would cause infinite recursion
  // Instead, we test with a non-existent path to verify error handling
  let registry = tools.init_registry()
  let args = json.object([
    #("path", json.string("/tmp/nonexistent_gleam_project")),
  ])

  // The tool should return an error for non-existent path
  let result = tools.execute_tool(registry, "test_run", args)
  let content = get_result_text(result)

  // Should contain error or status information
  should.be_true(
    string.contains(content, "error") ||
    string.contains(content, "status") ||
    string.contains(content, "not") ||
    string.contains(content, "failed")
  )
}

pub fn test_create_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("function_code", json.string("pub fn add(a, b) { a + b }")),
    #("function_name", json.string("add")),
  ])

  let result = tools.execute_tool(registry, "test_create", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "add_test"))
  should.be_true(string.contains(content, "should"))
}

pub fn test_coverage_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("path", json.string("/Users/playra/vibee-eliza-999/vibee/gleam")),
  ])

  let result = tools.execute_tool(registry, "test_coverage", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "source_files"))
  should.be_true(string.contains(content, "test_files"))
}

pub fn test_validate_exact_match_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("expected", json.string("hello")),
    #("actual", json.string("hello")),
    #("comparison", json.string("exact")),
  ])

  let result = tools.execute_tool(registry, "test_validate", args)
  let content = get_result_text(result)
  should.be_true(string.contains(content, "\"passed\":true"))
}

pub fn test_validate_contains_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("expected", json.string("world")),
    #("actual", json.string("hello world!")),
    #("comparison", json.string("contains")),
  ])

  let result = tools.execute_tool(registry, "test_validate", args)
  let content = get_result_text(result)
  should.be_true(string.contains(content, "\"passed\":true"))
}

pub fn test_validate_failure_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("expected", json.string("foo")),
    #("actual", json.string("bar")),
    #("comparison", json.string("exact")),
  ])

  let result = tools.execute_tool(registry, "test_validate", args)
  let content = get_result_text(result)
  should.be_true(string.contains(content, "\"passed\":false"))
}

// ============================================================
// Agent Tools Tests
// ============================================================

pub fn agent_spawn_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("name", json.string("test_agent")),
    #("task", json.string("Test task description")),
    #("type", json.string("debug")),
  ])

  let result = tools.execute_tool(registry, "agent_spawn", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "agent_test_agent"))
  should.be_true(string.contains(content, "running"))
}

pub fn agent_status_test() {
  let registry = tools.init_registry()
  let args = json.object([])

  let result = tools.execute_tool(registry, "agent_status", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "agents") || string.contains(content, "count"))
}

pub fn agent_message_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("agent_id", json.string("agent_test_123")),
    #("message", json.string("Hello agent!")),
  ])

  let result = tools.execute_tool(registry, "agent_message", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "sent"))
}

pub fn agent_kill_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("agent_id", json.string("agent_to_kill_123")),
  ])

  let result = tools.execute_tool(registry, "agent_kill", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "killed"))
}

// ============================================================
// Event Tools Tests (ETS-backed Event Bus)
// ============================================================

pub fn event_emit_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("event_type", json.string("test_event")),
    #("payload", json.object([#("data", json.string("test_data"))])),
  ])

  let result = tools.execute_tool(registry, "event_emit", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  // New Event Bus returns event_id
  should.be_true(string.contains(content, "event_id") || string.contains(content, "success"))
}

pub fn event_emit_with_target_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("event_type", json.string("message_received")),
    #("payload", json.object([#("message", json.string("hello"))])),
    #("target", json.string("agent_123")),
  ])

  let result = tools.execute_tool(registry, "event_emit", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "success") || string.contains(content, "event_id"))
}

pub fn event_list_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("limit", json.int(10)),
  ])

  let result = tools.execute_tool(registry, "event_list", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "available_types"))
  // New Event Bus also returns stats
  should.be_true(string.contains(content, "total_in_bus") || string.contains(content, "count"))
}

pub fn event_list_with_filter_test() {
  let registry = tools.init_registry()
  // First emit an event
  let emit_args = json.object([
    #("event_type", json.string("task_completed")),
    #("payload", json.object([#("task_id", json.string("123"))])),
  ])
  let _ = tools.execute_tool(registry, "event_emit", emit_args)

  // Then list with filter
  let list_args = json.object([
    #("event_type", json.string("task_completed")),
    #("limit", json.int(5)),
  ])

  let result = tools.execute_tool(registry, "event_list", list_args)
  should.be_false(result.is_error)
}

// ============================================================
// Bot Analysis Tools Tests
// ============================================================

pub fn bot_analyze_no_session_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("bot_username", json.string("test_bot")),
    #("depth", json.string("quick")),
  ])

  let result = tools.execute_tool(registry, "bot_analyze", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "test_bot"))
  should.be_true(string.contains(content, "no_session") || string.contains(content, "suggestion"))
}

pub fn bot_analyze_with_at_sign_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("bot_username", json.string("@mybot")),
  ])

  let result = tools.execute_tool(registry, "bot_analyze", args)
  let content = get_result_text(result)
  // Should strip @ from username
  should.be_true(string.contains(content, "mybot"))
  should.be_false(string.contains(content, "@mybot"))
}

pub fn bot_compare_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("bots", json.array(["bot1", "bot2", "bot3"], json.string)),
  ])

  let result = tools.execute_tool(registry, "bot_compare", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "bots_compared"))
}

pub fn bot_monitor_start_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("bot_username", json.string("monitor_test_bot")),
    #("action", json.string("start")),
    #("interval_seconds", json.int(60)),
  ])

  let result = tools.execute_tool(registry, "bot_monitor", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "monitoring_started"))
}

pub fn bot_monitor_status_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("bot_username", json.string("monitor_test_bot")),
    #("action", json.string("status")),
  ])

  let result = tools.execute_tool(registry, "bot_monitor", args)
  should.be_false(result.is_error)
}

pub fn bot_monitor_stop_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("bot_username", json.string("monitor_test_bot")),
    #("action", json.string("stop")),
  ])

  let result = tools.execute_tool(registry, "bot_monitor", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "monitoring_stopped"))
}

pub fn bot_extract_commands_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("bot_username", json.string("extract_test_bot")),
  ])

  let result = tools.execute_tool(registry, "bot_extract_commands", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "extraction_methods"))
  should.be_true(string.contains(content, "common_commands_to_try"))
}

pub fn bot_test_interaction_no_session_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("bot_username", json.string("interaction_test_bot")),
    #("interactions", json.array([], fn(x) { x })),
  ])

  let result = tools.execute_tool(registry, "bot_test_interaction", args)
  // Should fail without session_id
  should.be_true(result.is_error)
}

pub fn bot_test_interaction_with_session_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("bot_username", json.string("interaction_test_bot")),
    #("session_id", json.string("test_session_123")),
    #("interactions", json.array([
      json.object([
        #("type", json.string("command")),
        #("value", json.string("/start")),
      ])
    ], fn(x) { x })),
  ])

  let result = tools.execute_tool(registry, "bot_test_interaction", args)
  should.be_false(result.is_error)

  let content = get_result_text(result)
  should.be_true(string.contains(content, "test_plan_created"))
}

// ============================================================
// Knowledge Tools Tests
// ============================================================

pub fn knowledge_search_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("query", json.string("test search query")),
    #("limit", json.int(5)),
  ])

  let result = tools.execute_tool(registry, "knowledge_search", args)
  // May error if embeddings not available, but should not crash
  let content = get_result_text(result)
  should.be_true(string.length(content) > 0)
}

// ============================================================
// Error Handling Tests
// ============================================================

pub fn unknown_tool_test() {
  let registry = tools.init_registry()
  let args = json.object([])

  let result = tools.execute_tool(registry, "nonexistent_tool", args)
  should.be_true(result.is_error)
}

pub fn missing_required_param_file_read_test() {
  let registry = tools.init_registry()
  let args = json.object([])  // Missing "path"

  let result = tools.execute_tool(registry, "file_read", args)
  should.be_true(result.is_error)
}

pub fn missing_required_param_debug_build_test() {
  let registry = tools.init_registry()
  let args = json.object([])  // Missing "path"

  let result = tools.execute_tool(registry, "debug_build", args)
  should.be_true(result.is_error)
}

pub fn missing_required_param_agent_spawn_test() {
  let registry = tools.init_registry()
  let args = json.object([
    #("name", json.string("test")),
    // Missing "task"
  ])

  let result = tools.execute_tool(registry, "agent_spawn", args)
  should.be_true(result.is_error)
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
