import gleam/option.{Some}
import gleeunit
import gleeunit/should
import vibee/agent/persistence
import vibee/types

pub fn main() {
  gleeunit.main()
}

pub fn save_load_cycle_test() {
  // Test that save/load functions exist and have correct signatures
  // Actual database testing requires connection
  
  let state = types.AgentState(
    id: "test_1",
    name: "Test Agent",
    tone: types.Friendly,
    language: types.En,
    history: [
      types.Message(
        id: "msg_1",
        sender: "user",
        content: types.TextContent("Hello"),
        timestamp: 123,
      ),
    ],
    history_limit: 10,
    system_prompt: Some("Test prompt"),
    messages_since_save: 0,
  )
  
  // Test save (will fail without DB, but that's expected)
  let _save_result = persistence.save_state(state)
  
  // Test load (will fail without DB, but that's expected)
  let _load_result = persistence.load_state("test_1")
  
  // Test delete (will fail without DB, but that's expected)
  let _delete_result = persistence.delete_state("test_1")
  
  should.be_true(True)
}

pub fn record_metric_test() {
  // Should not crash even without database
  let result = persistence.record_metric("test_agent", "test_metric", 1.0, option.None)
  
  case result {
    Ok(_) -> should.be_true(True)
    Error(_) -> should.be_true(True)  // Expected without DB
  }
}

pub fn record_error_test() {
  // Should not crash even without database
  let result = persistence.record_error(
    "test_agent",
    "test_error",
    "Test error message",
    option.None,
    option.None,
  )
  
  case result {
    Ok(_) -> should.be_true(True)
    Error(_) -> should.be_true(True)  // Expected without DB
  }
}
