# VIBEE - Agent Framework on Gleam/BEAM

VIBEE is a fault-tolerant, concurrent AI agent framework built with Gleam on the BEAM (Erlang VM). It leverages OTP supervision trees for reliability and includes Telegram MTProto integration via a Go microservice.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                       VIBEE System                          │
├─────────────────────────────────────────────────────────────┤
│  ┌────────────────────────────────────────────────────────┐ │
│  │                 Gleam/BEAM Runtime                      │ │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐              │ │
│  │  │ Agent 1  │  │ Agent 2  │  │ Agent N  │              │ │
│  │  └────┬─────┘  └────┬─────┘  └────┬─────┘              │ │
│  │       └─────────────┼─────────────┘                    │ │
│  │                     │                                   │ │
│  │            ┌────────┴────────┐                         │ │
│  │            │ AgentSupervisor │                         │ │
│  │            │  (one_for_one)  │                         │ │
│  │            └────────┬────────┘                         │ │
│  │  ┌──────────┬───────┴───────┬──────────┐              │ │
│  │  │ HTTP API │ TelegramClient│   RAG    │              │ │
│  │  │  (Mist)  │               │ Provider │              │ │
│  │  └──────────┴───────┬───────┴──────────┘              │ │
│  └─────────────────────┼──────────────────────────────────┘ │
│                        │ HTTP/WebSocket                     │
│  ┌─────────────────────┼──────────────────────────────────┐ │
│  │        Go Telegram Bridge (gotd/td)                    │ │
│  └─────────────────────┼──────────────────────────────────┘ │
│                        │ MTProto                            │
│                   [ Telegram ]                              │
│                                                             │
│  ┌─────────────────────────────────────────────────────────┐│
│  │                    PostgreSQL                           ││
│  │  agents │ messages │ sessions │ knowledge               ││
│  └─────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────┘
```

## Prerequisites

- Erlang/OTP 26+
- Gleam 1.0+
- Go 1.21+
- PostgreSQL 15+
- Docker & Docker Compose (optional)

## Quick Start

### 1. Start PostgreSQL

```bash
docker-compose up -d postgres
```

### 2. Build and Run Gleam App

```bash
cd gleam
gleam build
gleam run
```

### 3. Build and Run Telegram Bridge (optional)

```bash
cd telegram-bridge
go build -o telegram-bridge ./cmd/server
./telegram-bridge
```

### Using Docker Compose

```bash
# Start all services
docker-compose up -d

# View logs
docker-compose logs -f

# Stop
docker-compose down
```

## Project Structure

```
vibee/
├── gleam/                        # Gleam/BEAM application
│   ├── src/vibee/
│   │   ├── agent/               # OTP actors
│   │   │   ├── agent.gleam      # Agent actor
│   │   │   └── supervisor.gleam # Supervisor
│   │   ├── api/                 # HTTP API
│   │   │   └── router.gleam     # Mist router
│   │   ├── character/           # Character config
│   │   │   └── parser.gleam     # JSON parser
│   │   ├── integrations/        # External integrations
│   │   │   └── telegram/        # Telegram client
│   │   ├── knowledge/           # RAG system
│   │   │   ├── provider.gleam   # Knowledge provider
│   │   │   ├── loader.gleam     # Markdown loader
│   │   │   └── search.gleam     # Text search
│   │   ├── persistence/         # Database
│   │   │   └── postgres.gleam   # PostgreSQL adapter
│   │   ├── types.gleam          # Core types
│   │   └── error.gleam          # Error types
│   └── test/                    # Tests
│
├── telegram-bridge/              # Go MTProto service
│   ├── cmd/server/              # Entry point
│   ├── internal/
│   │   ├── api/                 # HTTP handlers
│   │   ├── telegram/            # gotd/td client
│   │   └── config/              # Configuration
│   └── Dockerfile
│
├── config/                       # Configuration files
│   ├── example_agent.character.json
│   └── telegram.example.json
│
├── knowledge-base/               # RAG knowledge files
│   └── vibee-docs/
│
├── docker-compose.yml
└── Makefile
```

## API Endpoints

### Health & Status

```
GET /health          - Liveness check
GET /ready           - Readiness check
```

### Agent Management

```
GET    /api/v1/agents           - List all agents
POST   /api/v1/agents           - Create agent
GET    /api/v1/agents/:id       - Get agent
DELETE /api/v1/agents/:id       - Delete agent
POST   /api/v1/agents/:id/message - Send message
```

### Telegram Bridge (Go service)

```
POST /api/v1/connect            - Connect to Telegram
POST /api/v1/auth/phone         - Send auth code
POST /api/v1/auth/code          - Verify code
POST /api/v1/auth/2fa           - 2FA verification
GET  /api/v1/me                 - Get current user
GET  /api/v1/dialogs            - List dialogs
POST /api/v1/send               - Send message
WS   /api/v1/updates            - Real-time updates
```

## Character Files

Create agent personalities using JSON character files:

```json
{
  "id": "my-agent",
  "name": "MyAgent",
  "tone": "friendly",
  "language": "en",
  "system": "You are a helpful assistant",
  "bio": "An AI agent built on VIBEE",
  "lore": [
    "Created in 2024",
    "Runs on BEAM"
  ],
  "plugins": ["telegram", "knowledge"]
}
```

## Development

### Run Tests

```bash
# Gleam tests
cd gleam && gleam test

# Go tests
cd telegram-bridge && go test ./...

# All tests via Make
make test
```

### Build

```bash
make build
```

### Clean

```bash
make clean
```

## Key Features

- **Fault Tolerance**: OTP supervision with automatic recovery
- **Scalability**: Handle thousands of concurrent agents
- **Type Safety**: Full Gleam type system
- **Telegram MTProto**: User-bot support via Go bridge
- **RAG Support**: Knowledge-base enhanced responses
- **PostgreSQL**: Persistent state storage

## License

MIT
# vibee-gleam
