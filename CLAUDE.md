# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

VIBEE is a fault-tolerant AI agent framework built with Gleam on BEAM (Erlang VM). The monorepo contains:

| Service | Tech Stack | Purpose |
|---------|------------|---------|
| `gleam/` | Gleam/Erlang | MCP server, Telegram agent, payments, RAG |
| `telegram-bridge/` | Go (gotd/td) | MTProto bridge for Telegram API |
| `remotion/` | Node.js/React | Video rendering + editor UI |

## Quick Start

```bash
# Gleam MCP server
cd gleam && gleam build && VIBEE_MODE=mcp gleam run

# Telegram agent mode
cd gleam && gleam run

# Go Telegram bridge
cd telegram-bridge && go run ./cmd/server

# Remotion render server
cd remotion && npx tsx render-server.ts

# Video editor UI
cd remotion/player && npm run dev
```

## Build & Test Commands

### Gleam (gleam/)

```bash
gleam build                        # Build
gleam test                         # All tests
gleam test --module mcp_tools_test # Single test module
gleam format src test              # Format
gleam deps download                # Dependencies
fly deploy -a vibee-mcp            # Deploy
```

### Go Bridge (telegram-bridge/)

```bash
go build -o bin/telegram-bridge ./cmd/server  # Build
go run ./cmd/server                            # Run
go test -v ./...                               # Test
go fmt ./...                                   # Format
fly deploy -a vibee-telegram-bridge            # Deploy
```

### Remotion (remotion/)

```bash
npm start                          # Remotion Studio (localhost:3000)
npx tsx render-server.ts           # Render server (port 3333)
npm run build                      # Bundle for production
fly deploy -a vibee-remotion       # Deploy
```

### Player UI (remotion/player/)

```bash
npm run dev                        # Dev server (localhost:5174)
npm run build                      # Production build
npm run lint                       # ESLint
```

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                MCP Client (Claude Code / Cursor)                 │
└────────────────────────────┬────────────────────────────────────┘
                             │ SSE
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                 VIBEE MCP Server (gleam/)                        │
│                      Gleam/BEAM                                  │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐              │
│  │ MCP Tools   │  │ Session Mgr │  │ RAG Search  │              │
│  │ (~100)      │  │   (ETS)     │  │  (pgvector) │              │
│  └─────────────┘  └─────────────┘  └─────────────┘              │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐              │
│  │ P2P Escrow  │  │  Payments   │  │ AI Services │              │
│  │   (TON)     │  │ (Robokassa) │  │ (FLUX,Kling)│              │
│  └─────────────┘  └─────────────┘  └─────────────┘              │
└────────────────────────────┬────────────────────────────────────┘
                             │ HTTP
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│              Go Telegram Bridge (telegram-bridge/)               │
│                    gotd/td MTProto                               │
│           map[session_id]*telegram.Client                        │
└────────────────────────────┬────────────────────────────────────┘
                             │ MTProto
                             ▼
                        [ Telegram ]

┌─────────────────────────────────────────────────────────────────┐
│               Video Service (remotion/)                          │
│  ┌─────────────────┐    ┌──────────────────────────────────┐    │
│  │  Render Server  │    │   Player UI (React/Vite)         │    │
│  │ render-server.ts│◄───│   Timeline, Canvas, Properties   │    │
│  └────────┬────────┘    └──────────────────────────────────┘    │
│           │                                                      │
│  ┌────────▼────────┐                                            │
│  │   Compositions   │   LipSyncMain, TextOverlay, VideoIntro    │
│  └─────────────────┘                                            │
└─────────────────────────────────────────────────────────────────┘
```

## Key Directories

### gleam/src/vibee/

| Directory | Purpose |
|-----------|---------|
| `mcp/` | MCP protocol, tools (~100), session management |
| `agent/` | OTP actors, supervisor, persistence |
| `api/` | HTTP router (Mist), webhook handlers |
| `ai/` | AI integrations (FLUX, Kling, ElevenLabs, Hedra) |
| `p2p/` | P2P escrow (TON, xRocket, CryptoBot) |
| `payment/` | Robokassa, Telegram Stars |
| `bot/` | Telegram bot scenes (neuro_photo, avatar_video) |
| `search/` | Hybrid RAG search (vector + keyword) |

### telegram-bridge/internal/

| Directory | Purpose |
|-----------|---------|
| `api/` | REST handlers (connect, auth, send, dialogs) |
| `telegram/` | MTProto client wrapper |
| `middleware/` | API key authentication |

### remotion/

| File/Dir | Purpose |
|----------|---------|
| `render-server.ts` | Render API + S3 upload |
| `src/compositions/` | Video templates |
| `player/` | React video editor |

## Environment Variables

### Gleam

| Variable | Description |
|----------|-------------|
| `VIBEE_MODE` | "mcp" for MCP server mode |
| `VIBEE_BRIDGE_URL` | Telegram bridge URL |
| `VIBEE_API_KEY` | API authentication key |
| `DATABASE_URL` | PostgreSQL (use direct endpoint, not pooler) |
| `OPENROUTER_API_KEY` | LLM API key |

### Telegram Bridge

| Variable | Description |
|----------|-------------|
| `TELEGRAM_APP_ID` | From my.telegram.org |
| `TELEGRAM_APP_HASH` | From my.telegram.org |
| `VIBEE_API_KEY` | Must match Gleam config |

### Remotion

| Variable | Description |
|----------|-------------|
| `AWS_ENDPOINT_URL_S3` | S3/Tigris endpoint |
| `BUCKET_NAME` | S3 bucket name |
| `AWS_ACCESS_KEY_ID` | S3 credentials |
| `AWS_SECRET_ACCESS_KEY` | S3 credentials |

## Deployment (Fly.io)

| Service | App Name | URL |
|---------|----------|-----|
| Gleam MCP | vibee-mcp | vibee-mcp.fly.dev |
| Telegram Bridge | vibee-telegram-bridge | vibee-telegram-bridge.fly.dev |
| Remotion | vibee-remotion | vibee-remotion.fly.dev |

```bash
fly deploy -a <app-name>
fly secrets set KEY=value -a <app-name>
fly logs -a <app-name>
```

## MCP Tools (Top Categories)

**Telegram**: `telegram_get_dialogs`, `telegram_send_message`, `telegram_get_history`
**Auth**: `session_create`, `auth_send_code`, `auth_verify_code`, `session_set_active`
**RAG**: `telegram_search_history`, `telegram_parse_chat`, `conversation_get_context`
**AI**: `ai_bfl_generate_image`, `ai_kling_create_video`, `ai_elevenlabs_tts`
**Payments**: `invoice_create`, `p2p_create_sell_order`, `balance_get`
**Tasks**: `task_create`, `task_list`, `task_status`

## Erlang FFI Pattern

All ETS-based FFI functions must call `init()` first:

```erlang
% src/vibee_*_ffi.erl
get_data() ->
    init(),  % ALWAYS first
    case ets:lookup(?TABLE, key) of
        [{key, Val}] -> {some, Val};
        [] -> none
    end.

init() ->
    case ets:info(?TABLE) of
        undefined -> ets:new(?TABLE, [named_table, public, set]);
        _ -> ok
    end,
    nil.
```

## Database Notes

Use Neon PostgreSQL **direct endpoint** (not pooler):
```
# Correct
ep-bitter-frog-xxx.ap-southeast-1.aws.neon.tech

# Wrong (causes socket errors)
ep-bitter-frog-xxx-pooler.ap-southeast-1.aws.neon.tech
```

Requires `pog >= 4.1.0` in gleam.toml for SSL fix.

## Subdirectory CLAUDE.md Files

More detailed documentation in:
- `gleam/CLAUDE.md` - MCP tools, Telegram auth flow, troubleshooting
- `remotion/CLAUDE.md` - Video compositions, render API, editor store
