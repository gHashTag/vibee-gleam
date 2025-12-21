---
name: logs
description: View structured VIBEE logs with filtering
---

# /logs - Structured Logs

Просмотр структурированных JSON логов VIBEE.

## Формат логов

Все логи теперь в JSON формате с полями:
- `timestamp` - ISO 8601 время
- `level` - TRACE/DEBUG/INFO/WARN/ERROR/FATAL
- `logger` - имя логгера (twin, msg, fal, send, etc)
- `message` - сообщение
- `trace_id`, `session_id`, `chat_id` - контекст (опционально)

## Services

| Service | App Name |
|---------|----------|
| mcp (default) | vibee-mcp |
| bridge | vibee-telegram-bridge |
| remotion | vibee-remotion |

## Usage

```bash
/logs                      # vibee-mcp logs (live)
/logs bridge               # telegram-bridge logs
/logs --level ERROR        # Только ошибки
/logs --level WARN         # WARN и выше
/logs --logger twin        # Digital Twin логи
/logs --logger fal         # FAL.ai генерации
/logs --tail 50            # Последние 50 строк
```

## Implementation

When user runs `/logs`:

```bash
# Live logs (default)
fly logs -a vibee-mcp

# Specific service
fly logs -a vibee-telegram-bridge

# Filter by level (JSON)
fly logs -a vibee-mcp | grep '"level":"ERROR"'

# Filter by logger
fly logs -a vibee-mcp | grep '"logger":"twin"'

# E2E logs in JSON mode
E2E_JSON_LOG=true ./.claude/hooks/e2e-rainbow-bridge.sh
```

## VIBEE_LOG_LEVEL

Настройка уровня логирования через env var:

```bash
# Показывать debug и выше
VIBEE_LOG_LEVEL=debug gleam run

# Только warn и error
VIBEE_LOG_LEVEL=warn gleam run
```

Уровни: trace, debug, info (default), warn, error, fatal

## Log Loggers

| Logger | Description |
|--------|-------------|
| `twin` | Digital Twin обработка |
| `msg` | Входящие сообщения |
| `cmd` | Команды (/pricing, /quiz, etc) |
| `sniper` | Sniper Mode триггеры |
| `fal` | FAL.ai генерации |
| `send` | Отправка сообщений |
| `rag` | RAG контекст |
| `openrouter` | LLM вызовы |
| `e2e` | E2E тесты |

## Common Filters

```bash
# Digital Twin activity
/logs --filter TWIN

# Session management
/logs --filter "Restored session"

# Errors only
/logs --filter ERROR

# Specific user
/logs --filter neuro_sage
```

## Troubleshooting

| Issue | Log Pattern |
|-------|-------------|
| Bot not responding | `[TWIN] Processing` |
| Auth issues | `Session not found` |
| DB connection | `Connection unavailable` |
| Webhook errors | `webhook error` |

## Multi-service Logs

```bash
# Both main services
fly logs -a vibee-mcp &
fly logs -a vibee-telegram-bridge
```
