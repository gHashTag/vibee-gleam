# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

## КРИТИЧЕСКОЕ ПРАВИЛО: Rainbow Bridge E2E Testing

**ВСЕ АГЕНТЫ РАЗРАБОТКИ ОБЯЗАНЫ** проверять свою работу через Rainbow Bridge!

### После ЛЮБЫХ изменений в Telegram боте:

1. **Деплой**: `fly deploy -a vibee-mcp`
2. **E2E тесты**: Автоматически запустятся через PostHook
3. **Верификация**: Убедиться что ВСЕ тесты прошли

### Способы запуска E2E тестов

| Способ | Описание |
|--------|----------|
| **MCP Tool** | `e2e_run()` - прямой вызов через MCP (рекомендуется) |
| **HTTP API** | `GET /api/e2e/run` |
| **Bash** | `./.claude/hooks/e2e-rainbow-bridge.sh` |
| **PostHook** | Автоматически после `fly deploy` |

### Быстрая проверка E2E (для Claude Code)

После деплоя:
```
WebFetch: https://vibee-mcp.fly.dev/api/e2e/run
```
Ожидаемый результат: **passed >= 2, failed == 0**

### Тестовые аккаунты (ПРОВЕРЕНО 21.12.2025)

| Роль | Username | User/Chat ID | Телефон | Env Variable |
|------|----------|--------------|---------|--------------|
| **ТЕСТЕР** | @neuro_sage | User: 144022504 | +7 (993) 342-04-65 | `TELEGRAM_SESSION_ID_TESTER` |
| **ЮЗЕР-БОТ** | @vibee_agent | User: 6579515876 | +66 6-2401-4170 | `TELEGRAM_SESSION_ID` |

**ВАЖНО**: @vibee_agent - это ЮЗЕР-БОТ (Digital Twin), НЕ обычный Telegram бот!
Это пользовательский аккаунт который работает через MTProto с автоматизацией.

**Bridge**: 16 сессий (13 авторизованных) на https://vibee-telegram-bridge.fly.dev

### Session Configuration (Single Source of Truth)

**`.env.example`** - шаблон конфигурации, скопировать в `.env` для локальной разработки.
**Fly.io secrets** - production конфигурация.

```bash
# Скопировать шаблон
cp .env.example .env
# Отредактировать с реальными значениями
```

Все shell-скрипты загружают `.env` автоматически. НЕ хардкодить session ID в коде!

### Что проверяется

| Команда | Pattern Matching |
|---------|------------------|
| /help | `neurophoto\|video\|/menu\|Komandy` |
| /pricing | `JUNIOR\|MIDDLE\|Тариф\|$99` |

**Важно**: Тесты используют pattern matching (OR с `|`), ищут ЛЮБОЙ ответ бота в истории.
Это обходит проблемы с задержкой ответов бота.

### Ручной запуск E2E

```bash
VIBEE_API_KEY="vibee-secret-2024-prod" \
VIBEE_BRIDGE_URL="https://vibee-telegram-bridge.fly.dev" \
./.claude/hooks/e2e-rainbow-bridge.sh
```

**РАБОТА НЕ ЗАВЕРШЕНА пока E2E тесты не пройдут!**

---

## Project Overview

VIBEE is a fault-tolerant AI agent framework built with Gleam on BEAM (Erlang VM). It provides:
- MCP (Model Context Protocol) server for AI agents (Claude Code, Cursor)
- Telegram MTProto integration via Go microservice
- P2P payments (TON, Telegram Stars, Robokassa)
- RAG (Retrieval-Augmented Generation) capabilities

## Build & Run Commands

```bash
# Build Gleam project
gleam build

# Run tests
gleam test

# Run a single test file
gleam test --module mcp_tools_test

# Run MCP server (set VIBEE_MODE=mcp)
VIBEE_MODE=mcp gleam run

# Run Telegram agent mode
gleam run

# Format code
gleam format src test

# Download dependencies
gleam deps download

# Deploy to Fly.io
fly deploy -a vibee-mcp
```

### Telegram Bridge (Go)

```bash
cd ../telegram-bridge

# Build
go build -o bin/telegram-bridge ./cmd/server

# Run
go run ./cmd/server

# Test
go test -v ./...

# Format
go fmt ./...

# Deploy
fly deploy -a vibee-telegram-bridge
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│              MCP Client (Claude Code / Cursor)               │
└────────────────────────────┬────────────────────────────────┘
                             │ SSE/WebSocket
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                  VIBEE MCP Server (Gleam/BEAM)               │
│                                                              │
│  src/vibee/mcp/                                              │
│  ├── tools.gleam      # Tool registry (~100 tools)           │
│  ├── protocol.gleam   # MCP JSON-RPC protocol                │
│  ├── server.gleam     # Stdio MCP server                     │
│  ├── session_manager.gleam # Multi-account sessions (ETS)    │
│  ├── rag_tools.gleam  # conversation_search, parse_chat      │
│  └── ai_tools.gleam   # AI service tools                     │
│                                                              │
│  src/vibee/                                                  │
│  ├── api/router.gleam # HTTP/WebSocket router (Mist)         │
│  ├── p2p/             # P2P escrow system (TON)              │
│  ├── payment/         # Robokassa, Stars integration         │
│  └── earning/         # Token economics                      │
└────────────────────────────┬────────────────────────────────┘
                             │ HTTP
                             ▼
┌─────────────────────────────────────────────────────────────┐
│            Go Telegram Bridge (gotd/td MTProto)              │
│                                                              │
│  telegram-bridge/                                            │
│  ├── cmd/server/main.go                                      │
│  └── internal/                                               │
│      ├── api/handlers.go      # REST API handlers            │
│      ├── telegram/client.go   # MTProto client               │
│      ├── middleware/auth.go   # API key auth                 │
│      └── config/config.go     # CORS, env config             │
└────────────────────────────┬────────────────────────────────┘
                             │ MTProto
                             ▼
                        [ Telegram ]
```

## Key Modules

### MCP Tools (src/vibee/mcp/tools.gleam)
Main tool registry with ~100 tools. Tool handlers pattern:
```gleam
fn handle_tool(name: String, args: Dynamic, session_id: Option(String)) -> ToolResult
```

### Session Management
Multi-account Telegram sessions stored in ETS:
```gleam
session_manager.init()           // Initialize ETS table
session_manager.upsert(info)     // Add/update session
session_manager.get_active()     // Get active session_id
session_manager.set_active(id)   // Switch active session
```

### FFI (Erlang interop)
Erlang FFI files in `src/`:
- `vibee_p2p_ffi.erl` - P2P order storage (ETS)
- `vibee_payment_ffi.erl` - Payment/balance storage
- `vibee_session_ffi.erl` - Session ETS operations
- `vibee_db_pool_ffi.erl` - Database pool caching

**Critical ETS Pattern**: All FFI functions using ETS must call `init()` first:
```erlang
get_something() ->
    init(),  % ALWAYS call init() first
    case ets:lookup(?TABLE, key) of
        [{key, Value}] -> {some, Value};
        [] -> none
    end.

init() ->
    case ets:info(?TABLE) of
        undefined -> ets:new(?TABLE, [named_table, public, set]);
        _ -> ok
    end,
    nil.
```

Gleam FFI declaration:
```gleam
@external(erlang, "vibee_p2p_ffi", "create_order")
fn ffi_create_order(order: Dynamic) -> Dynamic
```

## Environment Variables

| Variable | Description |
|----------|-------------|
| `VIBEE_MODE` | "mcp" for MCP server, otherwise Telegram agent |
| `VIBEE_BRIDGE_URL` | Telegram bridge URL (default: https://vibee-telegram-bridge.fly.dev) |
| `VIBEE_API_KEY` | API authentication key (required for bridge access) |
| `TELEGRAM_SESSION_ID` | Primary session ID |
| `OPENROUTER_API_KEY` | LLM API key (optional) |
| `DATABASE_URL` | PostgreSQL connection string (see Database section) |

### Database (Neon PostgreSQL)

**Important**: Use direct endpoint, NOT pooler:
```
# ✅ Correct (direct)
ep-bitter-frog-xxx.ap-southeast-1.aws.neon.tech

# ❌ Wrong (pooler - causes pgo socket errors)
ep-bitter-frog-xxx-pooler.ap-southeast-1.aws.neon.tech
```

Requires `pog >= 4.1.0` (pgo >= 0.18.0) for SSL socket fix.

### Telegram Bridge Variables (Go)

| Variable | Description |
|----------|-------------|
| `TELEGRAM_APP_ID` | Telegram API ID (from my.telegram.org) |
| `TELEGRAM_APP_HASH` | Telegram API Hash (from my.telegram.org) |
| `VIBEE_API_KEY` | API key for authenticating requests |
| `DATABASE_URL` | PostgreSQL for session persistence |
| `SESSION_DIR` | Directory for session files (default: ./sessions) |

---

## MCP Setup for Claude Code

### Quick Start

1. **Add MCP server to Claude Code config** (`~/.claude/settings.json` or project `.mcp.json`):
```json
{
  "mcpServers": {
    "vibee": {
      "command": "/path/to/vibee/gleam/run_mcp.sh",
      "args": []
    }
  }
}
```

2. **Configure `run_mcp.sh`** with required environment variables:
```bash
#!/bin/bash
cd /path/to/vibee/gleam
export PATH="/opt/homebrew/bin:$PATH"
export DATABASE_URL="postgresql://..."
export VIBEE_BRIDGE_URL="https://vibee-telegram-bridge.fly.dev"
export VIBEE_API_KEY="your-api-key"
exec gleam run -m mcp_server 2>/tmp/vibee-mcp.log
```

3. **Restart MCP in Claude Code**: `/mcp` → restart vibee

### Telegram Authorization Flow

```
1. session_create()
   → Creates new session, returns session_id

2. auth_send_code(phone: "+79001234567")
   → Sends SMS/Telegram code, returns phone_code_hash

3. auth_verify_code(phone, code, phone_code_hash)
   → Verifies code, authorizes session

4. session_set_active(session_id)
   → Sets session as active for subsequent calls
```

### Available MCP Tools (~100 tools)

**Telegram:**
- `telegram_get_dialogs` - List chats/groups/channels
- `telegram_get_history` - Get message history
- `telegram_send_message` - Send messages
- `telegram_send_buttons` - Send inline keyboards
- `telegram_send_photo` - Send photos

**Bot Testing:**
- `bot_analyze` - Analyze bot commands and capabilities
- `bot_extract_commands` - Extract bot commands from /help
- `bot_test_interaction` - Test bot responses
- `bot_compare` - Compare multiple bots

**Auth & Sessions:**
- `auth_status` - Check authorization status
- `auth_send_code` - Send auth code to phone
- `auth_verify_code` - Verify authorization code
- `session_list` - List all sessions
- `session_set_active` - Switch active session
- `session_create` - Create new session

**RAG & Search:**
- `telegram_search_history` - Hybrid search (vector + keyword)
- `telegram_parse_chat` - Parse and index chat messages
- `telegram_generate_embeddings` - Generate embeddings for messages
- `conversation_get_context` - Get conversation context for AI

**AI Services:**
- `ai_bfl_generate_image` - Generate images (FLUX)
- `ai_kling_create_video` - Generate videos (Kling AI)
- `ai_elevenlabs_tts` - Text-to-speech
- `ai_openai_transcribe` - Speech-to-text (Whisper)
- `ai_hedra_create_avatar` - Create talking avatars

**Payments:**
- `invoice_create` - Create crypto invoice (xRocket/CryptoBot)
- `invoice_cheque` - Create multi-user cheque
- `balance_get` - Get user balance
- `p2p_create_sell_order` - Create P2P sell order
- `p2p_list_orders` - List P2P orders

**Task Management:**
- `task_create` - Create task linked to Telegram contact
- `task_list` - List tasks with filters
- `task_get` - Get task details
- `task_status` - Update task status
- `task_stats` - Get task statistics
- `task_today` - Tasks due today
- `task_overdue` - Overdue tasks

**Agent Observability:**
- `agent_list` - List all active VIBEE agents with status
- `agent_status` - Get detailed agent metrics by ID

### Agent Observability Endpoints

| Endpoint | Type | Description |
|----------|------|-------------|
| `/ws/agents` | WebSocket | Real-time agent updates (5s interval) |
| `/metrics/agents` | HTTP GET | Prometheus-compatible metrics |

WebSocket commands: `ping`, `refresh`, `get:<agent_id>`

### Troubleshooting

**"Not connected" error:**
- Check if MCP server is running: `/mcp` in Claude Code
- Check logs: `tail -f /tmp/vibee-mcp.log`
- Restart: `/mcp` → restart vibee

**"Authorization header required":**
- Add `VIBEE_API_KEY` to `run_mcp.sh`
- Ensure key matches Fly.io secret

**"app_id and app_hash are required":**
- Set on Fly.io bridge:
```bash
fly secrets set TELEGRAM_APP_ID=xxx TELEGRAM_APP_HASH=xxx -a vibee-telegram-bridge
```

**Session not authorized:**
- Run full auth flow: `auth_send_code` → `auth_verify_code`
- Check `auth_status()` for current state

**"Connection unavailable" / SSL socket errors:**
- Ensure DATABASE_URL uses direct endpoint (no `-pooler`)
- Verify pog >= 4.1.0 in gleam.toml
- Run `gleam deps download` to update pgo to >= 0.18.0

**"undefined_column" SQL errors:**
- Check that `telegram_dialogs` table has correct schema
- Column is `title` not `name` for contact names

---

## Deployment

Services on Fly.io:
- **vibee-mcp.fly.dev** - Gleam MCP server
- **vibee-telegram-bridge.fly.dev** - Go Telegram bridge

```bash
# Set secrets
fly secrets set VIBEE_API_KEY=xxx -a vibee-telegram-bridge
fly secrets set TELEGRAM_API_ID=xxx -a vibee-telegram-bridge

# Deploy
fly deploy -a vibee-mcp
fly deploy -a vibee-telegram-bridge
```

## Testing

```bash
# All tests
gleam test

# Specific module
gleam test --module mcp_tools_test
gleam test --module p2p_test

# Go tests
cd ../telegram-bridge && go test -v ./...
```

## Common Patterns

### Adding a new MCP tool
1. Add tool definition in `src/vibee/mcp/tools.gleam` (in `init_registry()`)
2. Add handler case in `handle_tool()`
3. Add test in `test/mcp_tools_test.gleam`

### Error handling
```gleam
import vibee/mcp/protocol

// Success
protocol.success_result(json.object([...]))

// Error
protocol.error_result("Error message")
```

---

## Rainbow Bridge Testing (E2E)

Автоматизированное end-to-end тестирование через два реальных Telegram аккаунта. Подход аналогичен Selenium для web-тестирования.

### Концепция

| Роль | Аккаунт | Описание |
|------|---------|----------|
| **Тестер** | @neuro_sage | Отправляет команды, проверяет ответы |
| **Бот** | @vibee_agent | Обрабатывает команды, отвечает |

### E2E API Endpoint

VIBEE MCP сервер предоставляет встроенный E2E endpoint:

```
GET https://vibee-mcp.fly.dev/api/e2e/run
```

Возвращает JSON:
```json
{
  "status": "passed|failed",
  "tester_session": "REDACTED_SESSION",
  "tester_username": "neuro_sage",
  "bot_username": "vibee_agent",
  "bot_chat_id": 6579515876,
  "total": 2,
  "passed": 2,
  "failed": 0,
  "tests": [
    {"command": "/help", "expected": "neurophoto|video|/menu|Komandy", "passed": true, "response": "..."},
    {"command": "/pricing", "expected": "JUNIOR|MIDDLE|Тариф|$99", "passed": true, "response": "..."}
  ]
}
```

### Автоматический запуск (PostHook)

E2E тесты запускаются автоматически после каждого `fly deploy` через Claude Code hooks.

Конфигурация: `.claude/settings.json`
Скрипт: `.claude/hooks/e2e-rainbow-bridge.sh`

### Ручной запуск

```bash
# Через API
curl https://vibee-mcp.fly.dev/api/e2e/run | jq

# Через скрипт
./.claude/hooks/e2e-rainbow-bridge.sh
```

### Доступные инструменты

| Инструмент | Описание |
|------------|----------|
| `session_list` | Список сессий с статусом авторизации |
| `session_set_active` | Переключение между аккаунтами |
| `bot_test_interaction` | Отправка команд и проверка ответов |
| `bot_analyze` | Анализ команд и возможностей бота |
| `bot_compare` | Сравнение нескольких ботов |

### Flow теста

```
@neuro_sage                    @vibee_agent (VIBEE)
     │                              │
     │  /help                       │
     │─────────────────────────────►│
     │                         [Bot Router]
     │                         [Scene Handler]
     │                         [process.sleep(10s)]
     │   Response (pattern match)   │
     │◄─────────────────────────────│
```

### Архитектура Pattern Matching

E2E тесты используют подход "find ANY matching response":

1. Отправляем команду через Bridge API
2. Ждём 10 секунд (`process.sleep(10000)`)
3. Получаем историю сообщений (`get_history`, limit=10)
4. Ищем ЛЮБОЙ ответ от VIBEE, содержащий паттерн
5. Паттерн использует `|` как OR: `"JUNIOR|MIDDLE"` = содержит "junior" ИЛИ "middle"

**Почему так?** Бот обрабатывает много чатов параллельно через polling.
Ответы могут приходить с задержкой или в неожиданном порядке.
Pattern matching в истории обходит эту проблему.

### Исходный код E2E

Endpoint: `src/vibee/api/e2e_handlers.gleam`
Shell hook: `.claude/hooks/e2e-rainbow-bridge.sh`
