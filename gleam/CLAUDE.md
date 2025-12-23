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
| **ASYNC HTTP** | `GET /api/e2e/run` → 202 Accepted (мгновенно) |
| **Status Poll** | `GET /api/e2e/status/{test_run_id}` → результаты |
| **Legacy Sync** | `GET /api/e2e/run-sync` → может timeout |
| **Bash** | `./.claude/hooks/e2e-rainbow-bridge.sh` |

### Async E2E Workflow (РЕКОМЕНДУЕТСЯ)

```
1. GET /api/e2e/run → получить test_run_id (мгновенно)
2. Подождать ~35 секунд
3. GET /api/e2e/status/{test_run_id} → получить результаты
```

**Почему async?** Fly.io proxy timeout ~60 сек. Sync endpoint возвращает 502.
Async endpoint возвращает 202 Accepted мгновенно, тесты работают в фоне.

### Быстрая проверка E2E (для Claude Code)

После деплоя используй bash скрипт или polling:
```bash
# Получить test_run_id
TEST_RUN=$(curl -s https://vibee-mcp.fly.dev/api/e2e/run | jq -r '.test_run_id')

# Подождать и проверить статус
sleep 35
curl -s "https://vibee-mcp.fly.dev/api/e2e/status/$TEST_RUN"
```
Ожидаемый результат: **status: "completed", passed >= 2, failed == 0**

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
| /help | `neurophoto\|video\|/menu\|Команды` |
| /pricing | `JUNIOR\|MIDDLE\|Тариф\|$99` |
| /neuro | `Генерирую\|fal.media\|Промпт:` |

**Важно**: Тесты используют pattern matching (OR с `|`), ищут ЛЮБОЙ ответ бота в истории.
Это обходит проблемы с задержкой ответов бота.

### Правило: ВСЕГДА ТЕСТИРУЙ СЕБЯ!

**Claude Code ОБЯЗАН** проверять свои изменения через Rainbow Bridge:

1. **После изменений в telegram_agent.gleam**:
   - `gleam build` - убедиться что билд успешен
   - `fly deploy -a vibee-mcp` - задеплоить
   - `fly logs -a vibee-mcp` - проверить логи

2. **Для AI функций (/neuro, /voice, /video)**:
   - Изменения в FAL.ai polling → увеличить `max_attempts` если timeout
   - Изменения в сообщениях → использовать КИРИЛЛИЦУ, не транслит!
   - После деплоя → проверить логи на ошибки FAL.ai

3. **НЕ ПРОСИТЬ ПОЛЬЗОВАТЕЛЯ ТЕСТИРОВАТЬ!**
   - Использовать `/api/e2e/run` для базовых тестов
   - Использовать `fly logs` для диагностики
   - Если E2E timeout → проверить логи на ошибки

### AI Функции - Таймауты

| Функция | API | Timeout |
|---------|-----|---------|
| /neuro | FAL.ai | 60 попыток × 3 сек = 3 мин |
| /voice | ElevenLabs | 30 сек |
| /video | Kling AI | 5 мин |

**Примечание**: Используй async E2E workflow! Sync endpoint таймаутит (Fly.io ~60 сек).

### Ручной запуск E2E

```bash
VIBEE_API_KEY="vibee-secret-2024-prod" \
VIBEE_BRIDGE_URL="https://vibee-telegram-bridge.fly.dev" \
./.claude/hooks/e2e-rainbow-bridge.sh
```

**РАБОТА НЕ ЗАВЕРШЕНА пока E2E тесты не пройдут!**

---

## КРИТИЧЕСКОЕ ПРАВИЛО: ElizaOS Architecture (User-Bots)

**НИКАКИХ КОМАНД!** VIBEE использует user-bots (Digital Twins), а не обычные Telegram боты.
User-bots работают через понимание контекста разговора, а не через /commands.

### Принцип ElizaOS

Основано на https://docs.elizaos.ai/plugins/architecture

| Компонент | Описание | Пример |
|-----------|----------|--------|
| **Action** | Дискретная задача с `validate()` + `handler()` | CREATE_REELS |
| **Provider** | Агрегация контекста для принятия решений | reels_context_provider |
| **Evaluator** | Анализ результатов, извлечение insights | reels_evaluator |

### Как это работает

```
User: "Создай рилс про продуктивность"
                    │
                    ▼
┌─────────────────────────────────────────┐
│              ActionRegistry              │
│  ┌─────────────────────────────────┐    │
│  │ find_matching_action()          │    │
│  │ → validate() проверяет контекст │    │
│  │ → Находит CREATE_REELS action   │    │
│  └─────────────────────────────────┘    │
│                    │                     │
│                    ▼                     │
│  ┌─────────────────────────────────┐    │
│  │ Providers собирают контекст     │    │
│  │ → user preferences              │    │
│  │ → history                       │    │
│  │ → available templates           │    │
│  └─────────────────────────────────┘    │
│                    │                     │
│                    ▼                     │
│  ┌─────────────────────────────────┐    │
│  │ Action.handler() выполняет      │    │
│  │ → generate script               │    │
│  │ → TTS → lipsync → render        │    │
│  └─────────────────────────────────┘    │
│                    │                     │
│                    ▼                     │
│  ┌─────────────────────────────────┐    │
│  │ Evaluators анализируют          │    │
│  │ → extract facts                 │    │
│  │ → suggest follow-ups            │    │
│  └─────────────────────────────────┘    │
└─────────────────────────────────────────┘
```

### Структура файлов

```
src/vibee/agent/
├── eliza.gleam                  # Base types: Action, Provider, Evaluator
├── reels_plugin.gleam           # Plugin registration
├── actions/
│   └── reels_action.gleam       # CREATE_REELS action
├── providers/
│   └── reels_context_provider.gleam
└── evaluators/
    └── reels_evaluator.gleam
```

### Пример Action (validate + handler)

```gleam
pub fn create_reels_action() -> Action {
  Action(
    name: "CREATE_REELS",
    description: "Creates Instagram-style reels video",

    // Similes для NLP matching (НЕ /commands!)
    similes: [
      "создай рилс", "сделай рилс", "рилс видео",
      "видео для инстаграма", "make a reel", "reels creator",
    ],

    // validate() - когда триггерить action
    validate: fn(context) {
      let message = string.lowercase(context.message)
      // Проверяем наличие ключевых слов
      has_reels_keyword(message) && has_creation_intent(message)
    },

    // handler() - выполнение action
    handler: fn(context) {
      // 1. Extract idea from message
      // 2. Detect niche
      // 3. Generate script
      // 4. Run AI pipeline
      // 5. Return ActionResult
    },
  )
}
```

### Правила для разработчиков

1. **НЕ ИСПОЛЬЗУЙ /commands** - user-bots понимают естественный язык
2. **validate()** определяет КОГДА триггерить action через NLP
3. **handler()** выполняет action и возвращает `ActionResult`
4. **Providers** собирают контекст ДО выполнения action
5. **Evaluators** анализируют результат ПОСЛЕ выполнения

### Триггер patterns (similes)

| Паттерн | Action |
|---------|--------|
| "создай рилс", "сделай рилс", "рилс видео" | CREATE_REELS |
| "сгенерируй фото", "нарисуй", "neuro photo" | GENERATE_PHOTO |
| "озвучь", "голос", "voice clone" | VOICE_CLONE |
| "видео из фото", "анимируй" | IMAGE_TO_VIDEO |

### Интеграция с существующим кодом

Для интеграции с polling_actor.gleam:

```gleam
import vibee/agent/reels_plugin

// В обработчике сообщений
fn handle_incoming_message(message: String) {
  // Проверяем - это для reels?
  case reels_plugin.should_handle_message(message) {
    True -> {
      let registry = reels_plugin.init()
      let context = build_context(message)
      reels_plugin.process_message(registry, context)
    }
    False -> handle_other_message(message)
  }
}
```

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
  "tester_session": "sess_deyyfxjyao6q",
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
