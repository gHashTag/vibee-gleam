---
name: agents
description: Monitor VIBEE agent lifecycle and metrics
---

# /agents - Agent Observability

Мониторинг жизненного цикла и метрик VIBEE агентов.

## Usage

```bash
/agents                    # List all agents
/agents list               # Same as above
/agents status <agent_id>  # Get agent details
/agents metrics            # Show Prometheus endpoint
```

## MCP Tools

| Tool | Description |
|------|-------------|
| `agent_list` | List all active agents with status, message counts |
| `agent_status` | Get detailed status of specific agent |

## Endpoints

| Endpoint | Type | Description |
|----------|------|-------------|
| `/ws/agents` | WebSocket | Real-time agent updates (5s interval) |
| `/metrics/agents` | HTTP | Prometheus-compatible metrics |

## WebSocket Messages

Connect to `ws://localhost:8080/ws/agents`:

```json
{
  "type": "agents_update",
  "timestamp": "2025-12-20T12:00:00Z",
  "agents": [...],
  "summary": {
    "total": 3,
    "running": 2,
    "stopped": 1
  }
}
```

Commands:
- `ping` → `pong`
- `refresh` → immediate update
- `get:<agent_id>` → agent details

## Prometheus Metrics

```prometheus
# Agent counts by status
vibee_agents_total{status="running"} 2
vibee_agents_total{status="stopped"} 1

# Messages per agent
vibee_agent_messages_total{agent_id="polling_sess_abc"} 150

# Errors per agent
vibee_agent_errors_total{agent_id="polling_sess_abc"} 2

# Agent info labels
vibee_agent_info{agent_id="...",agent_type="polling",status="running"} 1
```

## Implementation

When user runs `/agents`:

```bash
# Via MCP tool
mcp__vibee__agent_list

# Direct HTTP
curl http://localhost:8080/metrics/agents

# WebSocket (wscat)
wscat -c ws://localhost:8080/ws/agents
```

## Agent Types

| Type | Description |
|------|-------------|
| `polling` | Telegram polling actor |
| `generic` | Generic OTP actor |
| `super` | A2A coordinator |
| `event_bus` | Pub/Sub event bus |

## Agent Statuses

| Status | Description |
|--------|-------------|
| `starting` | Agent initializing |
| `running` | Active and processing |
| `paused` | Temporarily stopped |
| `stopping` | Shutting down |
| `stopped` | Fully stopped |
| `failed` | Error state |

## Agent Registry (ETS)

Агенты хранятся в ETS таблице `vibee_agent_registry`:

```gleam
pub type AgentInfo {
  AgentInfo(
    id: String,
    agent_type: AgentType,
    status: AgentStatus,
    started_at: String,
    last_activity: String,
    message_count: Int,
    error_count: Int,
    session_id: Option(String),
  )
}
```

## Troubleshooting

| Issue | Check |
|-------|-------|
| No agents shown | Is polling actor running? |
| Metrics empty | Check /metrics/agents endpoint |
| WS not connecting | Verify port 8080 is open |
| Agent not registering | Check agent_registry.init() called |
