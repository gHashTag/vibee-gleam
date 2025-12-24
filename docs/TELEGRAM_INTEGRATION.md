# Telegram Bot Integration with VIBEE Agent System

## Overview

This guide shows how to integrate the VIBEE Agent System with a Telegram bot to create intelligent, stateful conversational agents.

## Architecture

```
Telegram Bot API
       ↓
  Bot Handler
       ↓
   Supervisor ← manages → Agent Actors (N)
       ↓                        ↓
   PostgreSQL ← persists ← Agent State
```

## Integration Steps

### 1. Setup Agent Supervisor

```gleam
import vibee/agent/supervisor
import vibee/db/postgres

pub fn main() {
  // Initialize database
  let db_url = get_database_url()
  let assert Ok(pool) = postgres.connect(db_url)
  postgres.set_global_pool(pool)
  
  // Start supervisor
  let assert Ok(started) = supervisor.start_link()
  let sup = started.data
  
  // Start bot with supervisor reference
  start_telegram_bot(sup)
}
```

### 2. Create Agent Per User

```gleam
import vibee/types.{AgentConfig}

fn get_or_create_agent(sup, user_id: Int, username: String) {
  let agent_id = "telegram_user_" <> int.to_string(user_id)
  
  // Try to get existing agent
  case supervisor.get_agent(sup, agent_id) {
    Ok(agent_ref) -> agent_ref
    Error(_) -> {
      // Create new agent for this user
      let config = AgentConfig(
        id: agent_id,
        name: "Agent for " <> username,
        tone: types.Friendly,
        language: types.En,
        system_prompt: Some("You are a helpful Telegram assistant"),
        history_limit: 50,
      )
      
      let assert Ok(agent_ref) = supervisor.start_agent(sup, config)
      agent_ref
    }
  }
}
```

### 3. Handle Telegram Updates

```gleam
import vibee/types.{TgUpdate, NewMessage}
import gleam/erlang/process

fn handle_telegram_message(sup, update: TelegramUpdate) {
  case update {
    NewMessage(chat_id, message_id, text, sender_id, sender_name) -> {
      // Get or create agent for this user
      let agent_ref = get_or_create_agent(sup, sender_id, sender_name)
      
      // Forward message to agent
      let tg_update = TgUpdate(NewMessage(
        chat_id: chat_id,
        message_id: message_id,
        text: text,
        sender_id: sender_id,
        sender_name: sender_name,
      ))
      
      process.send(agent_ref.subject, tg_update)
      
      // Agent will process message and update its state
      // Auto-save will trigger after 5 messages
    }
    
    _ -> Nil
  }
}
```

### 4. Send Responses Back to Telegram

Currently, agents process messages but don't send responses automatically. You need to:

1. **Option A: Poll agent for responses**
```gleam
// After sending message to agent, wait for response
process.send(agent_ref.subject, ProcessText("req_123", text))

// TODO: Implement response callback mechanism
```

2. **Option B: Add response callback to agent**
```gleam
// Modify agent to accept a callback function
pub type AgentMessage {
  // ... existing variants
  ProcessTextWithCallback(
    request_id: String,
    text: String,
    callback: fn(String) -> Nil,
  )
}

// In agent handler:
ProcessTextWithCallback(request_id, text, callback) -> {
  // Process text with LLM
  let response = generate_response(text, state)
  
  // Call callback with response
  callback(response)
  
  actor.continue(state)
}
```

### 5. Complete Example

```gleam
import vibee/agent/supervisor
import vibee/types.{AgentConfig, TgUpdate, NewMessage}
import vibee/db/postgres
import gleam/erlang/process
import gleam/option.{Some}

pub fn main() {
  // 1. Initialize database
  let db_url = "postgresql://..."
  let assert Ok(pool) = postgres.connect(db_url)
  postgres.set_global_pool(pool)
  
  // 2. Start supervisor
  let assert Ok(started) = supervisor.start_link()
  let sup = started.data
  
  // 3. Start Telegram bot polling
  start_bot_polling(sup)
}

fn start_bot_polling(sup) {
  // Pseudo-code for Telegram bot
  loop {
    // Get updates from Telegram
    let updates = telegram_get_updates()
    
    // Process each update
    list.each(updates, fn(update) {
      handle_update(sup, update)
    })
    
    // Sleep before next poll
    process.sleep(1000)
  }
}

fn handle_update(sup, update) {
  case update {
    TelegramMessage(chat_id, message_id, text, user_id, username) -> {
      // Get or create agent
      let agent_id = "tg_" <> int.to_string(user_id)
      
      let agent_ref = case supervisor.get_agent(sup, agent_id) {
        Ok(ref) -> ref
        Error(_) -> {
          let config = AgentConfig(
            id: agent_id,
            name: "Agent for " <> username,
            tone: types.Friendly,
            language: types.En,
            system_prompt: Some("You are a helpful assistant"),
            history_limit: 50,
          )
          let assert Ok(ref) = supervisor.start_agent(sup, config)
          ref
        }
      }
      
      // Send message to agent
      let tg_update = TgUpdate(NewMessage(
        chat_id: chat_id,
        message_id: message_id,
        text: text,
        sender_id: user_id,
        sender_name: username,
      ))
      
      process.send(agent_ref.subject, tg_update)
      
      // TODO: Get response from agent and send to Telegram
      // telegram_send_message(chat_id, response)
    }
  }
}
```

## Features Enabled by Agent System

### 1. Persistent Conversations
- Each user gets their own agent
- Conversation history persists across bot restarts
- Auto-save every 5 messages

### 2. Scalability
- Multiple agents run concurrently
- Each agent is an independent actor
- Supervisor manages lifecycle

### 3. Monitoring
```gleam
// Check health of all agents
let health = supervisor.health_check(sup)

list.each(health, fn(h) {
  io.println(h.agent_id <> ": " <> bool_to_string(h.is_alive))
})
```

### 4. Graceful Shutdown
```gleam
// Stop specific agent
supervisor.stop_agent(sup, "tg_123456")

// Agent automatically saves state before shutdown
```

## Database Schema

The agent system uses these tables:

```sql
-- Agent states (JSONB for flexibility)
CREATE TABLE agent_states (
    agent_id VARCHAR(255) PRIMARY KEY,
    state JSONB NOT NULL,
    config JSONB NOT NULL,
    history JSONB NOT NULL,
    last_updated TIMESTAMP DEFAULT NOW()
);

-- Metrics for monitoring
CREATE TABLE agent_metrics (
    id SERIAL PRIMARY KEY,
    agent_id VARCHAR(255) NOT NULL,
    metric_type VARCHAR(50) NOT NULL,
    value NUMERIC NOT NULL,
    metadata JSONB DEFAULT '{}',
    recorded_at TIMESTAMP DEFAULT NOW()
);

-- Errors for debugging
CREATE TABLE agent_errors (
    id SERIAL PRIMARY KEY,
    agent_id VARCHAR(255) NOT NULL,
    error_type VARCHAR(100) NOT NULL,
    error_message TEXT NOT NULL,
    stack_trace TEXT,
    context JSONB DEFAULT '{}',
    occurred_at TIMESTAMP DEFAULT NOW()
);
```

## Metrics Collected

The system automatically records:
- `message_received`: Incremented for each message
- `auto_save`: Incremented when auto-save triggers
- Custom metrics via `persistence.record_metric()`

## Error Handling

Errors are automatically logged to database:
```gleam
case persistence.save_state(state) {
  Ok(_) -> Nil
  Error(err) -> {
    // Automatically recorded to agent_errors table
    persistence.record_error(
      agent_id,
      "save_state_failed",
      err,
      None,
      None,
    )
  }
}
```

## Next Steps

1. **Add LLM Integration**: Connect agents to OpenAI/Anthropic for responses
2. **Add RAG**: Integrate vector database for context retrieval
3. **Add Response Callbacks**: Implement mechanism to send responses back to Telegram
4. **Add Rate Limiting**: Prevent spam and abuse
5. **Add User Preferences**: Store per-user settings in agent state

## Example: Full Bot with LLM

```gleam
// TODO: Add example with actual LLM integration
// This would show:
// 1. Receiving Telegram message
// 2. Forwarding to agent
// 3. Agent processes with LLM
// 4. Response sent back to Telegram
// 5. State auto-saved
```

## Performance Considerations

- **Agent Limit**: Test with 100+ concurrent agents
- **Auto-save Threshold**: Adjust based on message volume (default: 5)
- **History Limit**: Balance context vs memory (default: 50)
- **Database Connection Pool**: Configure based on load

## Monitoring Dashboard

Query metrics for insights:

```sql
-- Messages per agent
SELECT agent_id, COUNT(*) as message_count
FROM agent_metrics
WHERE metric_type = 'message_received'
GROUP BY agent_id
ORDER BY message_count DESC;

-- Auto-save frequency
SELECT agent_id, COUNT(*) as auto_saves
FROM agent_metrics
WHERE metric_type = 'auto_save'
GROUP BY agent_id;

-- Recent errors
SELECT agent_id, error_type, error_message, occurred_at
FROM agent_errors
ORDER BY occurred_at DESC
LIMIT 10;
```

## Conclusion

The VIBEE Agent System provides a production-ready foundation for building stateful Telegram bots with:
- ✅ Persistent conversations
- ✅ Automatic state management
- ✅ Built-in monitoring
- ✅ Graceful error handling
- ✅ Scalable architecture

Ready to integrate with your Telegram bot!
