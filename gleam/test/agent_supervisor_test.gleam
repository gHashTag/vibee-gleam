import gleam/list
import gleam/option.{Some}
import gleeunit
import gleeunit/should
import vibee/agent/supervisor
import vibee/types

pub fn main() {
  gleeunit.main()
}

pub fn supervisor_start_test() {
  let result = supervisor.start_link()
  
  case result {
    Ok(_) -> should.be_true(True)
    Error(_) -> should.be_true(False)
  }
}

pub fn agent_lifecycle_test() {
  let assert Ok(started) = supervisor.start_link()
  let sup = started.data
  
  let config = types.AgentConfig(
    id: "test_agent_lifecycle",
    name: "Test Agent",
    tone: types.Friendly,
    language: types.En,
    system_prompt: Some("Test"),
    history_limit: 10,
  )
  
  // Start agent
  let result = supervisor.start_agent(sup, config)
  should.be_ok(result)
  
  // List agents
  let agents = supervisor.list_agents(sup)
  should.equal(list.length(agents), 1)
  
  // Get agent
  let get_result = supervisor.get_agent(sup, "test_agent_lifecycle")
  should.be_ok(get_result)
  
  // Stop agent
  let stop_result = supervisor.stop_agent(sup, "test_agent_lifecycle")
  should.be_ok(stop_result)
  
  // Verify stopped
  let agents_after = supervisor.list_agents(sup)
  should.equal(list.length(agents_after), 0)
}

pub fn health_check_test() {
  let assert Ok(started) = supervisor.start_link()
  let sup = started.data
  
  let config = types.AgentConfig(
    id: "health_test_agent",
    name: "Health Test Agent",
    tone: types.Friendly,
    language: types.En,
    system_prompt: Some("Test"),
    history_limit: 10,
  )
  
  let assert Ok(_) = supervisor.start_agent(sup, config)
  
  // Run health check
  let health = supervisor.health_check(sup)
  should.equal(list.length(health), 1)
  
  // Check first agent is alive
  let assert [first, ..] = health
  should.be_true(first.is_alive)
  should.equal(first.agent_id, "health_test_agent")
  
  // Cleanup
  let assert Ok(_) = supervisor.stop_agent(sup, "health_test_agent")
}
