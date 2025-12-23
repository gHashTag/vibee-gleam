---
name: e2e
description: MANDATORY Rainbow Bridge E2E Testing - исключает человеческий фактор
---

# /e2e - ОБЯЗАТЕЛЬНОЕ E2E Тестирование

**Этот шаг ОБЯЗАТЕЛЕН после любых изменений в боте!**

Исключает человеческий фактор - автоматическая проверка что бот работает правильно.

> Общий паттерн тестирования: см. `/rainbow-bridge`

## Quick Test (1 команда)

```
WebFetch: https://vibee-mcp.fly.dev/api/e2e/run-sync
Prompt: "Show all test results - passed/failed"
```

**Ожидание**: `passed >= 2, failed == 0`

## Способы запуска

| Способ | Команда | Для кого |
|--------|---------|----------|
| **WebFetch** | `WebFetch(url, prompt)` | Claude Code (быстрейший) |
| **MCP Tool** | `e2e_run()` | MCP клиенты |
| **HTTP API** | `GET /api/e2e/run` | Внешние системы |
| **Bash** | `./.claude/hooks/e2e-rainbow-bridge.sh` | Терминал |
| **PostHook** | Автоматически после `fly deploy` | CI/CD |

## Тестовые аккаунты (ПРОВЕРЕНО 21.12.2025)

| Роль | Username | ID | Телефон | Session |
|------|----------|----|---------|---------|
| **ТЕСТЕР** | @neuro_sage | User: 144022504 | +7 (993) 342-04-65 | `sess_deyyfxjyao6q` |
| **ЮЗЕР-БОТ** | @vibee_agent | Chat: 6579515876 | +66 6-2401-4170 | - |

**ВАЖНО**: @vibee_agent - это ЮЗЕР-БОТ (Digital Twin)!
Это НЕ обычный Telegram бот, а пользовательский аккаунт с MTProto автоматизацией.

**Bridge Status**: 16 сессий (13 авторизованных)

## Что тестируется

| Тест | Статус | Описание |
|------|--------|----------|
| `/help` | **Обязательный** | Проверяет список команд бота |
| `/pricing` | **Обязательный** | Проверяет отображение тарифов |
| `lead_forward` | **Опциональный** | Пересылка лидов (SKIPPED если нет доступа) |

**ВАЖНО**: SKIPPED тесты считаются пройденными (graceful degradation)

## Usage

```bash
/e2e              # Запустить все E2E тесты
/e2e pricing      # Тест только /pricing
/e2e quiz         # Тест только /quiz
/e2e help         # Тест только /help
```

## Flow тестирования

```
@neuro_sage                         @vibee_agent
     │                                    │
     │  1. /pricing                       │
     │───────────────────────────────────►│
     │           JUNIOR/MIDDLE/SENIOR     │
     │◄───────────────────────────────────│
     │                                    │
     │  2. /quiz                          │
     │───────────────────────────────────►│
     │              Quiz questions        │
     │◄───────────────────────────────────│
     │                                    │
     │  3. /help                          │
     │───────────────────────────────────►│
     │              Command list          │
     │◄───────────────────────────────────│
```

## Implementation

When user runs `/e2e`:

```bash
VIBEE_API_KEY="vibee-secret-2024-prod" \
VIBEE_BRIDGE_URL="https://vibee-telegram-bridge.fly.dev" \
./.claude/hooks/e2e-rainbow-bridge.sh
```

## Правило для Claude Code

**НЕ СЧИТАТЬ ЗАДАЧУ ВЫПОЛНЕННОЙ** пока:

1. `gleam build` - без ошибок
2. `fly deploy -a vibee-mcp` - успешный деплой
3. `WebFetch(https://vibee-mcp.fly.dev/api/e2e/run)` - проверить результат
4. **ВСЕ тесты PASSED или SKIPPED**

### Быстрый Workflow

```
gleam build
   ↓
fly deploy -a vibee-mcp
   ↓
WebFetch https://vibee-mcp.fly.dev/api/e2e/run
   ↓
Проверить: passed >= 2, failed == 0
   ↓
ГОТОВО!
```

## Что делать если тест не прошёл

1. Проверить логи: `fly logs -a vibee-mcp`
2. Найти ошибку в коде
3. Исправить
4. Повторить деплой
5. Повторить E2E тесты
6. Убедиться что ВСЕ тесты прошли

## Troubleshooting

| Проблема | Решение |
|----------|---------|
| Session not found | Проверить авторизацию @neuro_sage |
| Bot not responding | Проверить логи vibee-mcp |
| Timeout | Увеличить wait time в скрипте |
| Wrong response | Проверить логику в telegram_agent.gleam |
