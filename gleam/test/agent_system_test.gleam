// Comprehensive test suite for agent system
import gleam/io
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/erlang/process
import gleeunit
import gleeunit/should
import vibee/agent/supervisor
import vibee/types.{type AgentConfig, AgentConfig}
import vibee/db/postgres

pub fn main() {
  gleeunit.main()
}

pub fn supervisor_lifecycle_test() {
  io.println("\n🧪 Testing supervisor lifecycle...")
  
  // Start supervisor
  io.println("1. Starting supervisor...")
  let assert Ok(started) = supervisor.start_link()
  let sup = started.data
  
  io.println("   ✅ Supervisor started")
  
  // Create agent config
  let config = AgentConfig(
    id: "test_agent_1",
    name: "Test Agent 1",
    tone: types.Friendly,
    language: types.En,
    system_prompt: Some("You are a test agent"),
    history_limit: 10,
  )
  
  // Start agent
  io.println("2. Starting agent...")
  let assert Ok(agent_ref) = supervisor.start_agent(sup, config)
  
  io.println("   ✅ Agent started: " <> agent_ref.id)
  
  // List agents
  io.println("3. Listing agents...")
  let agents = supervisor.list_agents(sup)
  should.equal(list.length(agents), 1)
  
  io.println("   ✅ Found " <> int.to_string(list.length(agents)) <> " agent(s)")
  
  // Get specific agent
  io.println("4. Getting specific agent...")
  let assert Ok(retrieved) = supervisor.get_agent(sup, "test_agent_1")
  should.equal(retrieved.id, "test_agent_1")
  
  io.println("   ✅ Retrieved agent: " <> retrieved.id)
  
  // Stop agent
  io.println("5. Stopping agent...")
  let assert Ok(Nil) = supervisor.stop_agent(sup, "test_agent_1")
  
  io.println("   ✅ Agent stopped")
  
  // Verify agent is gone
  io.println("6. Verifying agent is stopped...")
  let agents_after = supervisor.list_agents(sup)
  should.equal(list.length(agents_after), 0)
  
  io.println("   ✅ Agent list is empty")
  
  io.println("\n✅ Supervisor lifecycle test passed!")
}

pub fn multiple_agents_test() {
  io.println("\n🧪 Testing multiple agents...")
  
  // Start supervisor
  let assert Ok(started) = supervisor.start_link()
  let sup = started.data
  
  // Start multiple agents
  io.println("1. Starting 3 agents...")
  
  let config1 = AgentConfig(
    id: "agent_1",
    name: "Agent 1",
    tone: types.Friendly,
    language: types.En,
    system_prompt: Some("Agent 1"),
    history_limit: 10,
  )
  
  let config2 = AgentConfig(
    id: "agent_2",
    name: "Agent 2",
    tone: types.Friendly,
    language: types.En,
    system_prompt: Some("Agent 2"),
    history_limit: 10,
  )
  
  let config3 = AgentConfig(
    id: "agent_3",
    name: "Agent 3",
    tone: types.Friendly,
    language: types.En,
    system_prompt: Some("Agent 3"),
    history_limit: 10,
  )
  
  let assert Ok(_) = supervisor.start_agent(sup, config1)
  let assert Ok(_) = supervisor.start_agent(sup, config2)
  let assert Ok(_) = supervisor.start_agent(sup, config3)
  
  io.println("   ✅ All agents started")
  
  // Verify all agents are running
  io.println("2. Verifying all agents...")
  let agents = supervisor.list_agents(sup)
  should.equal(list.length(agents), 3)
  
  io.println("   ✅ Found " <> int.to_string(list.length(agents)) <> " agents")
  
  // Stop all agents
  io.println("3. Stopping all agents...")
  let assert Ok(Nil) = supervisor.stop_agent(sup, "agent_1")
  let assert Ok(Nil) = supervisor.stop_agent(sup, "agent_2")
  let assert Ok(Nil) = supervisor.stop_agent(sup, "agent_3")
  
  io.println("   ✅ All agents stopped")
  
  io.println("\n✅ Multiple agents test passed!")
}

pub fn persistence_integration_test() {
  io.println("\n🧪 Testing persistence integration...")
  
  // Initialize database
  let db_url = "postgresql://neondb_owner:REDACTED_DB_PASSWORD@ep-bitter-frog-a1bewei7-pooler.ap-southeast-1.aws.neon.tech/neondb?sslmode=require"
  
  io.println("1. Connecting to database...")
  let assert Ok(pool) = postgres.connect(db_url)
  postgres.set_global_pool(pool)
  
  io.println("   ✅ Database connected")
  
  // Start supervisor
  let assert Ok(started) = supervisor.start_link()
  let sup = started.data
  
  // Start agent
  io.println("2. Starting agent...")
  let config = AgentConfig(
    id: "persistent_agent",
    name: "Persistent Agent",
    tone: types.Friendly,
    language: types.En,
    system_prompt: Some("I persist"),
    history_limit: 10,
  )
  
  let assert Ok(agent_ref) = supervisor.start_agent(sup, config)
  
  io.println("   ✅ Agent started: " <> agent_ref.id)
  
  // Send save state message
  io.println("3. Saving agent state...")
  process.send(agent_ref.subject, types.SaveState)
  
  // Give it time to save
  process.sleep(1000)
  
  io.println("   ✅ State save requested")
  
  // Stop agent
  io.println("4. Stopping agent...")
  let assert Ok(Nil) = supervisor.stop_agent(sup, "persistent_agent")
  
  io.println("   ✅ Agent stopped")
  
  // Cleanup
  let _ = postgres.disconnect(pool)
  
  io.println("\n✅ Persistence integration test passed!")
}
