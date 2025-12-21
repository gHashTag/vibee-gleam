// Agent Metrics - Prometheus-compatible metrics for VIBEE agents
// Endpoint: /metrics/agents

import gleam/int
import gleam/list
import gleam/string
import vibee/agent/agent_registry

// =============================================================================
// Prometheus Metrics
// =============================================================================

/// Generate Prometheus-compatible metrics for agents
pub fn generate_metrics() -> String {
  agent_registry.init()
  let agents = agent_registry.list_all()

  let lines = [
    // Header comments
    "# HELP vibee_agents_total Total number of VIBEE agents",
    "# TYPE vibee_agents_total gauge",
    build_agents_total_metrics(agents),
    "",
    "# HELP vibee_agent_messages_total Total messages processed by agent",
    "# TYPE vibee_agent_messages_total counter",
    build_messages_metrics(agents),
    "",
    "# HELP vibee_agent_errors_total Total errors by agent",
    "# TYPE vibee_agent_errors_total counter",
    build_errors_metrics(agents),
    "",
    "# HELP vibee_agent_info Agent information",
    "# TYPE vibee_agent_info gauge",
    build_agent_info_metrics(agents),
    "",
  ]

  string.join(lines, "\n")
}

/// Build total agents metrics by status
fn build_agents_total_metrics(agents: List(agent_registry.AgentInfo)) -> String {
  let running =
    list.filter(agents, fn(a) {
      case a.status {
        agent_registry.Running -> True
        _ -> False
      }
    })
    |> list.length

  let starting =
    list.filter(agents, fn(a) {
      case a.status {
        agent_registry.Starting -> True
        _ -> False
      }
    })
    |> list.length

  let stopped =
    list.filter(agents, fn(a) {
      case a.status {
        agent_registry.Stopped -> True
        _ -> False
      }
    })
    |> list.length

  let paused =
    list.filter(agents, fn(a) {
      case a.status {
        agent_registry.Paused -> True
        _ -> False
      }
    })
    |> list.length

  let failed =
    list.filter(agents, fn(a) {
      case a.status {
        agent_registry.Failed(_) -> True
        _ -> False
      }
    })
    |> list.length

  [
    "vibee_agents_total{status=\"running\"} " <> int.to_string(running),
    "vibee_agents_total{status=\"starting\"} " <> int.to_string(starting),
    "vibee_agents_total{status=\"stopped\"} " <> int.to_string(stopped),
    "vibee_agents_total{status=\"paused\"} " <> int.to_string(paused),
    "vibee_agents_total{status=\"failed\"} " <> int.to_string(failed),
  ]
  |> string.join("\n")
}

/// Build messages counter metrics
fn build_messages_metrics(agents: List(agent_registry.AgentInfo)) -> String {
  agents
  |> list.map(fn(agent) {
    let agent_type = agent_type_to_string(agent.agent_type)
    "vibee_agent_messages_total{agent_id=\""
    <> agent.id
    <> "\",agent_type=\""
    <> agent_type
    <> "\"} "
    <> int.to_string(agent.message_count)
  })
  |> string.join("\n")
}

/// Build errors counter metrics
fn build_errors_metrics(agents: List(agent_registry.AgentInfo)) -> String {
  agents
  |> list.map(fn(agent) {
    let agent_type = agent_type_to_string(agent.agent_type)
    "vibee_agent_errors_total{agent_id=\""
    <> agent.id
    <> "\",agent_type=\""
    <> agent_type
    <> "\"} "
    <> int.to_string(agent.error_count)
  })
  |> string.join("\n")
}

/// Build agent info metrics (always 1, used for labels)
fn build_agent_info_metrics(agents: List(agent_registry.AgentInfo)) -> String {
  agents
  |> list.map(fn(agent) {
    let agent_type = agent_type_to_string(agent.agent_type)
    let status = status_to_string(agent.status)
    "vibee_agent_info{agent_id=\""
    <> agent.id
    <> "\",agent_type=\""
    <> agent_type
    <> "\",status=\""
    <> status
    <> "\",started_at=\""
    <> agent.started_at
    <> "\"} 1"
  })
  |> string.join("\n")
}

/// Convert agent type to string
fn agent_type_to_string(t: agent_registry.AgentType) -> String {
  case t {
    agent_registry.PollingAgent -> "polling"
    agent_registry.WebSocketAgent -> "websocket"
    agent_registry.GenericAgent -> "generic"
    agent_registry.SuperAgent -> "super"
    agent_registry.EventBusAgent -> "event_bus"
  }
}

/// Convert status to string
fn status_to_string(s: agent_registry.AgentStatus) -> String {
  case s {
    agent_registry.Starting -> "starting"
    agent_registry.Running -> "running"
    agent_registry.Paused -> "paused"
    agent_registry.Stopping -> "stopping"
    agent_registry.Stopped -> "stopped"
    agent_registry.Failed(_) -> "failed"
  }
}
