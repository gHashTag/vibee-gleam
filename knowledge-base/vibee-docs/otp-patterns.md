# OTP Patterns in VIBEE

tags: otp, erlang, patterns, architecture

## Supervision Trees

VIBEE uses OTP supervision trees for fault tolerance.

### One-for-One Strategy

When an agent crashes, only that agent restarts:

```
AgentSupervisor (one_for_one)
├── Agent "alice"
├── Agent "bob"
└── Agent "charlie"
```

If "bob" crashes, only "bob" restarts. Alice and Charlie continue running.

### Why OTP?

1. **Isolation**: Each agent is a separate process
2. **Recovery**: Automatic restart on failure
3. **Monitoring**: Supervisors track child health
4. **Graceful Shutdown**: Proper cleanup on termination

## Actor Model

Each agent is an OTP actor (GenServer in Erlang terms):

### State Management

```gleam
pub type AgentState {
  AgentState(
    id: String,
    name: String,
    history: List(Message),
    config: AgentConfig,
  )
}
```

### Message Protocol

```gleam
pub type AgentMessage {
  TgUpdate(update: TelegramUpdate)
  ProcessText(request_id: String, text: String)
  Reply(request_id: String, text: String)
  SaveState
  Shutdown
}
```

## Error Handling

VIBEE follows the "let it crash" philosophy:

- Unexpected errors → crash → supervisor restarts
- Expected errors → handle gracefully with Result types
- State persisted to PostgreSQL for recovery

## Best Practices

1. Keep actors small and focused
2. Use supervision for all long-running processes
3. Persist important state before risky operations
4. Use timeouts for external calls
