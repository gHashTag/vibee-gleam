// Agent Supervisor - manages agent lifecycle with one_for_one strategy
// Updated for gleam_otp 1.2.0 builder API

import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/io
import vibee/types.{type AgentConfig, type AgentRef, AgentRef}
import vibee/error.{type VibeeError, AgentNotFound, AgentStartFailed}
import vibee/agent/agent

/// Health status of an agent
pub type AgentHealth {
  AgentHealth(
    agent_id: String,
    is_alive: Bool,
    message_count: Int,
  )
}

/// Messages for the supervisor
pub type SupervisorMessage {
  StartAgent(config: AgentConfig, reply_to: Subject(Result(AgentRef, VibeeError)))
  StopAgent(agent_id: String, reply_to: Subject(Result(Nil, VibeeError)))
  ListAgents(reply_to: Subject(List(AgentRef)))
  GetAgent(agent_id: String, reply_to: Subject(Result(AgentRef, VibeeError)))
  HealthCheck(reply_to: Subject(List(AgentHealth)))
}

/// Supervisor state
pub type SupervisorState {
  SupervisorState(
    agents: Dict(String, AgentRef),
    configs: Dict(String, AgentConfig),
  )
}

/// Start the supervisor using new builder API
pub fn start_link() -> Result(actor.Started(Subject(SupervisorMessage)), actor.StartError) {
  actor.new(SupervisorState(agents: dict.new(), configs: dict.new()))
  |> actor.on_message(handle_message)
  |> actor.start
}

/// Handle supervisor messages - note: state comes first in new API
fn handle_message(
  state: SupervisorState,
  message: SupervisorMessage,
) -> actor.Next(SupervisorState, SupervisorMessage) {
  case message {
    StartAgent(config, reply_to) -> {
      io.println("[SUPERVISOR] Starting agent: " <> config.name)

      // Actually start the agent actor
      case agent.start(config) {
        Ok(started) -> {
          let agent_ref = AgentRef(id: config.id, subject: started.data)
          
          let new_agents = dict.insert(state.agents, config.id, agent_ref)
          let new_configs = dict.insert(state.configs, config.id, config)
          process.send(reply_to, Ok(agent_ref))
          
          io.println("[SUPERVISOR] ✅ Agent started: " <> config.name)
          actor.continue(SupervisorState(agents: new_agents, configs: new_configs))
        }
        Error(_err) -> {
          io.println("[SUPERVISOR] ❌ Failed to start agent: " <> config.name)
          process.send(reply_to, Error(AgentStartFailed("Actor start failed")))
          actor.continue(state)
        }
      }
    }

    StopAgent(agent_id, reply_to) -> {
      case dict.get(state.agents, agent_id) {
        Ok(agent_ref) -> {
          io.println("[SUPERVISOR] Stopping agent: " <> agent_id)
          
          // Send shutdown message to agent
          process.send(agent_ref.subject, types.Shutdown)
          
          let new_agents = dict.delete(state.agents, agent_id)
          let new_configs = dict.delete(state.configs, agent_id)
          process.send(reply_to, Ok(Nil))
          
          io.println("[SUPERVISOR] ✅ Agent stopped: " <> agent_id)
          actor.continue(SupervisorState(agents: new_agents, configs: new_configs))
        }
        Error(Nil) -> {
          process.send(reply_to, Error(AgentNotFound(agent_id)))
          actor.continue(state)
        }
      }
    }
    ListAgents(reply_to) -> {
      let agents = dict.values(state.agents)
      process.send(reply_to, agents)
      actor.continue(state)
    }

    GetAgent(agent_id, reply_to) -> {
      case dict.get(state.agents, agent_id) {
        Ok(agent_ref) -> {
          process.send(reply_to, Ok(agent_ref))
        }
        Error(Nil) -> {
          process.send(reply_to, Error(AgentNotFound(agent_id)))
        }
      }
      actor.continue(state)
    }
    
    HealthCheck(reply_to) -> {
      io.println("[SUPERVISOR] Running health check...")
      
      // Check health of all agents
      let health_statuses = dict.fold(state.agents, [], fn(acc, _agent_id, agent_ref) {
        // Check if process is alive
        let pid = case process.subject_owner(agent_ref.subject) {
          Ok(p) -> p
          Error(_) -> process.self()  // Fallback, shouldn't happen
        }
        let is_alive = process.is_alive(pid)
        
        let health = AgentHealth(
          agent_id: agent_ref.id,
          is_alive: is_alive,
          message_count: 0,  // TODO: Get actual message count from agent
        )
        
        [health, ..acc]
      })
      
      process.send(reply_to, health_statuses)
      
      io.println("[SUPERVISOR] Health check complete: " <> int.to_string(list.length(health_statuses)) <> " agents")
      actor.continue(state)
    }
  }
}

/// API: Start a new agent - using process.call with labeled arguments
pub fn start_agent(
  supervisor: Subject(SupervisorMessage),
  config: AgentConfig,
) -> Result(AgentRef, VibeeError) {
  process.call(supervisor, waiting: 5000, sending: fn(reply_to) { StartAgent(config, reply_to) })
}

/// API: Stop an agent
pub fn stop_agent(
  supervisor: Subject(SupervisorMessage),
  agent_id: String,
) -> Result(Nil, VibeeError) {
  process.call(supervisor, waiting: 5000, sending: fn(reply_to) { StopAgent(agent_id, reply_to) })
}

/// API: List all agents
pub fn list_agents(supervisor: Subject(SupervisorMessage)) -> List(AgentRef) {
  process.call(supervisor, waiting: 5000, sending: fn(reply_to) { ListAgents(reply_to) })
}

/// API: Get a specific agent
pub fn get_agent(
  supervisor: Subject(SupervisorMessage),
  agent_id: String,
) -> Result(AgentRef, VibeeError) {
  process.call(supervisor, waiting: 5000, sending: fn(reply_to) { GetAgent(agent_id, reply_to) })
}

/// API: Health check for all agents
pub fn health_check(supervisor: Subject(SupervisorMessage)) -> List(AgentHealth) {
  process.call(supervisor, waiting: 5000, sending: fn(reply_to) { HealthCheck(reply_to) })
}
