// Load test for agent system
import gleam/io
import gleam/int
import gleam/list
import gleam/string
import gleam/option.{Some}
import gleam/erlang/process
import vibee/agent/supervisor
import vibee/types.{AgentConfig}
import vibee/db/postgres

pub fn main() {
  io.println("\n🚀 Agent System Load Test")
  io.println("=" <> string_repeat("=", 50))
  
  // Initialize database
  let db_url = "postgresql://neondb_owner:REDACTED_DB_PASSWORD@ep-bitter-frog-a1bewei7-pooler.ap-southeast-1.aws.neon.tech/neondb?sslmode=require"
  
  io.println("\n1. Initializing database connection...")
  case postgres.connect(db_url) {
    Ok(pool) -> {
      postgres.set_global_pool(pool)
      io.println("   ✅ Database connected")
      
      // Start supervisor
      io.println("\n2. Starting supervisor...")
      case supervisor.start_link() {
        Ok(started) -> {
          let sup = started.data
          io.println("   ✅ Supervisor started")
          
          // Run load tests
          run_load_tests(sup)
          
          // Cleanup
          io.println("\n6. Cleaning up...")
          let _ = postgres.disconnect(pool)
          io.println("   ✅ Cleanup complete")
        }
        Error(_) -> {
          io.println("   ❌ Failed to start supervisor")
        }
      }
    }
    Error(_) -> {
      io.println("   ❌ Failed to connect to database")
    }
  }
  
  io.println("\n✅ Load test completed!")
}

fn run_load_tests(sup) {
  let agent_counts = [5, 10, 20]
  
  list.each(agent_counts, fn(count) {
    io.println("\n3. Testing with " <> int.to_string(count) <> " agents...")
    
    // Start agents
    let start_time = erlang_system_time()
    let agents = start_n_agents(sup, count)
    let end_time = erlang_system_time()
    
    let duration_ms = {end_time - start_time} / 1_000_000
    
    io.println("   ✅ Started " <> int.to_string(list.length(agents)) <> " agents in " <> int.to_string(duration_ms) <> "ms")
    io.println("   📊 Average: " <> int.to_string(duration_ms / count) <> "ms per agent")
    
    // Verify all agents are running
    let running = supervisor.list_agents(sup)
    io.println("   ℹ️  Running agents: " <> int.to_string(list.length(running)))
    
    // Send messages to all agents
    io.println("\n4. Sending save state to all agents...")
    let msg_start = erlang_system_time()
    
    list.each(agents, fn(agent_ref) {
      process.send(agent_ref.subject, types.SaveState)
    })
    
    // Wait for processing
    process.sleep(2000)
    
    let msg_end = erlang_system_time()
    let msg_duration = {msg_end - msg_start} / 1_000_000
    
    io.println("   ✅ Messages sent in " <> int.to_string(msg_duration) <> "ms")
    
    // Stop all agents
    io.println("\n5. Stopping all agents...")
    let stop_start = erlang_system_time()
    
    list.each(agents, fn(agent_ref) {
      let _ = supervisor.stop_agent(sup, agent_ref.id)
      Nil
    })
    
    let stop_end = erlang_system_time()
    let stop_duration = {stop_end - stop_start} / 1_000_000
    
    io.println("   ✅ Stopped all agents in " <> int.to_string(stop_duration) <> "ms")
    io.println("   📊 Average: " <> int.to_string(stop_duration / count) <> "ms per agent")
    
    // Verify all stopped
    let remaining = supervisor.list_agents(sup)
    io.println("   ℹ️  Remaining agents: " <> int.to_string(list.length(remaining)))
    
    io.println("\n" <> string_repeat("-", 60))
  })
}

fn start_n_agents(sup, count) {
  list.range(1, count)
  |> list.filter_map(fn(i) {
    let config = AgentConfig(
      id: "load_test_agent_" <> int.to_string(i),
      name: "Load Test Agent " <> int.to_string(i),
      tone: types.Friendly,
      language: types.En,
      system_prompt: Some("Load test agent " <> int.to_string(i)),
      history_limit: 10,
    )
    
    supervisor.start_agent(sup, config)
  })
}

@external(erlang, "erlang", "system_time")
fn erlang_system_time() -> Int

fn string_repeat(s: String, n: Int) -> String {
  list.range(1, n)
  |> list.map(fn(_) { s })
  |> string.join("")
}
