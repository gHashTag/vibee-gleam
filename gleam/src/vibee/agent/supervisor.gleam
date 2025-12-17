// Agent Supervisor - manages agent lifecycle with one_for_one strategy
// Updated for gleam_otp 1.2.0 builder API

import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/dict.{type Dict}
import gleam/io
import gleam/result
import vibee/types.{type AgentConfig, type AgentRef}
import vibee/error.{type VibeeError, AgentNotFound, AgentStartFailed}

/// Messages for the supervisor
pub type SupervisorMessage {
  StartAgent(config: AgentConfig, reply_to: Subject(Result(AgentRef, VibeeError)))
  StopAgent(agent_id: String, reply_to: Subject(Result(Nil, VibeeError)))
  ListAgents(reply_to: Subject(List(AgentRef)))
  GetAgent(agent_id: String, reply_to: Subject(Result(AgentRef, VibeeError)))
}

/// Supervisor state
pub type SupervisorState {
  SupervisorState(agents: Dict(String, AgentRef))
}

/// Start the supervisor using new builder API
pub fn start_link() -> Result(actor.Started(Subject(SupervisorMessage)), actor.StartError) {
  actor.new(SupervisorState(agents: dict.new()))
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
      io.println("Starting agent: " <> config.name)

      // TODO: Actually start the agent actor
      // For now, create a placeholder reference
      let agent_ref = types.AgentRef(id: config.id, pid_ref: config.id)

      let new_agents = dict.insert(state.agents, config.id, agent_ref)
      process.send(reply_to, Ok(agent_ref))

      actor.continue(SupervisorState(agents: new_agents))
    }

    StopAgent(agent_id, reply_to) -> {
      case dict.get(state.agents, agent_id) {
        Ok(_agent_ref) -> {
          io.println("Stopping agent: " <> agent_id)
          // TODO: Actually stop the agent
          let new_agents = dict.delete(state.agents, agent_id)
          process.send(reply_to, Ok(Nil))
          actor.continue(SupervisorState(agents: new_agents))
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
