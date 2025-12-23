---
name: rainbow-bridge
description: Universal E2E testing pattern via Rainbow Bridge
---

# Rainbow Bridge - Universal Testing Pattern

Паттерн автоматизированного E2E тестирования через два реальных Telegram аккаунта.
Исключает человеческий фактор - автоматическая верификация.

## Quick Reference

```
WebFetch: https://vibee-mcp.fly.dev/api/e2e/run-sync
Prompt: "Show all test results"
```

**Ожидание**: `passed >= 2, failed == 0`

## Архитектура

```
┌─────────────────────────────────────────────────────────────────┐
│                     Rainbow Bridge E2E                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  @neuro_sage (Tester)              @vibee_agent (Bot/Agent)     │
│  sess_deyyfxjyao6q                 sess_df30nhu08008            │
│       │                                   │                      │
│       │  1. Send command/trigger          │                      │
│       │──────────────────────────────────►│                      │
│       │                                   │                      │
│       │  2. Wait (15-20 sec)              │                      │
│       │                              [Process]                   │
│       │                                   │                      │
│       │  3. Check response (pattern)      │                      │
│       │◄──────────────────────────────────│                      │
│       │                                   │                      │
│       │  4. Verify side effects           │                      │
│       │   (Lead Card, DB, etc.)           │                      │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## Тестовые аккаунты

| Роль | Username | User ID | Session ID |
|------|----------|---------|------------|
| **Тестер** | @neuro_sage | 144022504 | sess_deyyfxjyao6q |
| **Агент** | @vibee_agent | 6579515876 | sess_df30nhu08008 |

**Bridge**: https://vibee-telegram-bridge.fly.dev (16 сессий)

## Способы запуска

| Способ | Скорость | Команда |
|--------|----------|---------|
| **WebFetch** | ~25 сек | `WebFetch(https://vibee-mcp.fly.dev/api/e2e/run-sync)` |
| **Async API** | мгновенно | `GET /api/e2e/run` → poll `/api/e2e/status/{id}` |
| **Bash Hook** | ~35 сек | `./.claude/hooks/e2e-rainbow-bridge.sh` |
| **PostHook** | авто | После каждого `fly deploy` |

## Pattern Matching

Тесты используют OR-паттерны для гибкой проверки:

```
Pattern: "личку|напиши|помогу|разобраться"
         ↓
Matches: "Привет, давай в личку" ✅
         "Напиши мне в личку"    ✅
         "Я помогу разобраться"  ✅
```

**Почему OR?** Бот может отвечать разными фразами. Pattern matching в истории обходит вариативность.

## Доступные тесты

| Тест | Pattern | Описание |
|------|---------|----------|
| `/help` | `neurophoto\|video\|/menu` | Проверка списка команд |
| `/pricing` | `JUNIOR\|MIDDLE\|Тариф` | Проверка тарифов |
| `lead_forward` | `ЛИД\|Клиент\|крипт` | Пересылка лидов |

## Добавление нового теста

### 1. В e2e_handlers.gleam

```gleam
let tests = [
  // Существующие тесты...

  // Новый тест
  E2ETest(
    name: "my_new_test",
    command: "/mycommand",
    expected_pattern: "expected|response|pattern",
    chat_id: option.None,  // или Some(chat_id) для specific chat
    wait_ms: 15_000,
  ),
]
```

### 2. Проверка

```bash
fly deploy -a vibee-mcp
# Дождаться PostHook или:
curl https://vibee-mcp.fly.dev/api/e2e/run-sync | jq '.tests[] | select(.name == "my_new_test")'
```

## Timing Guidelines

| Операция | Рекомендуемое время |
|----------|---------------------|
| Простой ответ бота | 10-15 сек |
| AI генерация (FAL.ai) | 30-60 сек |
| Lead forwarding | 15-20 сек |
| Video generation | 3-5 мин |

## Response Format

```json
{
  "status": "passed",
  "total": 3,
  "passed": 3,
  "failed": 0,
  "tests": [
    {
      "name": "help",
      "command": "/help",
      "expected": "neurophoto|video",
      "passed": true,
      "response": "Команды бота: /neuro, /video..."
    }
  ]
}
```

## Workflow для Claude Code

### После изменений в боте:

```
1. gleam build
   ↓
2. fly deploy -a vibee-mcp
   ↓
3. WebFetch(https://vibee-mcp.fly.dev/api/e2e/run-sync)
   ↓
4. Проверить: passed >= N, failed == 0
   ↓
5. ГОТОВО!
```

### При ошибке:

```
1. fly logs -a vibee-mcp
   ↓
2. Найти ошибку
   ↓
3. Исправить
   ↓
4. Повторить workflow
```

## Best Practices

### DO:
- Использовать OR-паттерны для вариативных ответов
- Добавлять wait time для AI операций
- Проверять side effects (Lead Card, DB записи)
- Запускать E2E после КАЖДОГО деплоя

### DON'T:
- Хардкодить exact match (бот может менять формулировки)
- Устанавливать слишком короткий wait time
- Пропускать E2E тесты "потому что это маленькое изменение"
- Просить пользователя тестировать вручную

## Troubleshooting

| Симптом | Причина | Решение |
|---------|---------|---------|
| Timeout 502 | Fly proxy timeout | Использовать async API |
| Session not found | Сессия не авторизована | Проверить Bridge sessions |
| Pattern not matched | Бот изменил ответ | Расширить OR-паттерн |
| Test skipped | Нет доступа к чату | Добавить бота в чат |

## Интеграция с другими skills

```
/deploy → автоматически запускает /e2e через PostHook
/p2p → использует lead_forward тест из E2E
/e2e → запускает все тесты Rainbow Bridge
```
