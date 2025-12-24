# VIBEE MCP Server

Model Context Protocol (MCP) сервер для AI-агентов с интеграцией Telegram.

## Быстрый старт

### Claude Code

```bash
claude mcp add vibee --transport sse --url https://vibee-mcp.fly.dev/sse
```

### Cursor

Добавьте в `.cursor/mcp.json`:

```json
{
  "mcpServers": {
    "vibee": {
      "url": "https://vibee-mcp.fly.dev/sse"
    }
  }
}
```

После подключения **перезапустите** редактор.

---

## Первоначальная настройка

### 1. Проверка подключения

```
session_list
```

### 2. Создание сессии

```
session_create phone="+79001234567"
```

### 3. Авторизация

```
auth_send_code phone="+79001234567"
auth_verify_code code="12345" phone="+79001234567"
```

### 4. Проверка

```
telegram_get_me
```

---

## Основные инструменты

| Категория | Инструменты |
|-----------|-------------|
| **Сессии** | `session_list`, `session_create`, `session_set_active` |
| **Telegram** | `telegram_get_dialogs`, `telegram_get_history`, `telegram_send_message` |
| **Авторизация** | `auth_status`, `auth_send_code`, `auth_verify_code` |
| **RAG** | `conversation_search`, `telegram_parse_chat` |

Полный список: см. `CLAUDE.md`

---

## Работа с несколькими аккаунтами

```bash
# Посмотреть все сессии
session_list

# Создать новую сессию
session_create phone="+79009876543" set_active=false

# Переключиться на сессию
session_set_active session_id="sess_abc123"

# Отправить от конкретной сессии
telegram_send_message session_id="sess_xyz789" chat_id="123" text="Hello"
```

---

## Устранение неполадок

| Проблема | Решение |
|----------|---------|
| MCP не подключается | Проверьте URL: `https://vibee-mcp.fly.dev/sse` |
| "No active session" | Создайте сессию: `session_create phone="..."` |
| Код не приходит | Подождите 1-2 мин, повторите `auth_send_code` |
| "Session not found" | Создайте новую сессию |

---

## Локальная разработка

### Требования

- [Gleam](https://gleam.run/) >= 1.0
- Erlang/OTP >= 26
- Go >= 1.21

### Запуск

```bash
# MCP сервер
cd vibee/gleam
gleam run -m mcp_server

# Telegram Bridge
cd vibee/telegram-bridge
go run ./cmd/server
```

---

## Архитектура

```
┌─────────────────────────────────────────────────────┐
│              Claude Code / Cursor                    │
└─────────────────────┬───────────────────────────────┘
                      │ SSE
                      ▼
┌─────────────────────────────────────────────────────┐
│                VIBEE MCP Server                      │
│                  (Gleam/BEAM)                        │
│  ┌─────────────┐  ┌────────────┐  ┌─────────────┐   │
│  │ Session Mgr │  │ Tool Router│  │  RAG Tools  │   │
│  │    (ETS)    │  │            │  │             │   │
│  └─────────────┘  └────────────┘  └─────────────┘   │
└─────────────────────┬───────────────────────────────┘
                      │ HTTP
                      ▼
┌─────────────────────────────────────────────────────┐
│               Go Telegram Bridge                     │
│                  (MTProto)                           │
└─────────────────────┬───────────────────────────────┘
                      │
                      ▼
                [ Telegram API ]
```

---

## Деплой

```bash
fly deploy -a vibee-mcp
```

## Документация

- `CLAUDE.md` — подробный гайд для Claude Code
- `docs/GRAPHQL_API.md` — GraphQL API для Lead CRM
- `docs/ARCHITECTURE.md` — архитектура ElizaOS

## Лицензия

MIT
