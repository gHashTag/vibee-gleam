# Getting Started with VIBEE

tags: quickstart, tutorial, beginner

VIBEE is a Gleam/BEAM-based agent framework that leverages OTP for building fault-tolerant, concurrent AI agents.

## Quick Start

### Prerequisites

- Erlang/OTP 26+
- Gleam 1.0+
- PostgreSQL 15+

### Installation

1. Clone the repository
2. Run `gleam build`
3. Start PostgreSQL
4. Configure your agent character file

### Creating Your First Agent

Create a character file `my_agent.character.json`:

```json
{
  "name": "MyAgent",
  "tone": "friendly",
  "language": "en",
  "system": "You are a helpful assistant"
}
```

### Running the Agent

```bash
gleam run
```

## Architecture

VIBEE uses OTP supervision trees to manage agent lifecycles:

- **AgentSupervisor**: Manages all agent actors
- **Agent Actor**: Individual agent with state and message handling
- **TelegramBridge**: Go service for MTProto communication

## Key Features

- **Fault Tolerance**: Automatic recovery from crashes
- **Scalability**: Handle thousands of concurrent agents
- **Type Safety**: Full Gleam type system
- **Telegram Integration**: MTProto user-bot support
- **RAG Support**: Knowledge-base enhanced responses
