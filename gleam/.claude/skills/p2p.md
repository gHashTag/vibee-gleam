---
name: p2p
description: Test P2P Lead Forwarding (crypto trigger -> Lead Card) (project)
---

# /p2p - P2P Lead Forwarding Test

Тест пересылки лидов из триггер-чата в группу Leads.

## ВАЖНО: Автоматическое выполнение

**Этот skill запускает E2E тест автоматически!**

### Шаг 1: Запустить P2P тест
```
WebFetch: https://vibee-mcp.fly.dev/api/e2e/p2p
Prompt: "Extract test_run_id"
```

### Шаг 2: Подождать 25 секунд
```
Bash: sleep 25
```

### Шаг 3: Получить результаты
```
WebFetch: https://vibee-mcp.fly.dev/api/e2e/status/{test_run_id}
Prompt: "Show lead_forward test results: passed/failed, response, and duration"
```

## Конфигурация

| Параметр | Значение |
|----------|----------|
| Триггер-чат | Aimly.io dev (`-5082217642`) |
| Leads-чат | `-1002737186844` |
| Тестер | @neuro_sage (`REDACTED_SESSION`) |
| Агент | @vibee_agent (`REDACTED_SESSION`) |

## Trigger Words

`куплю`, `купить`, `продам`, `продать`, `крипту`, `btc`, `usdt`, `обменять`, `p2p`

## Flow

```
@neuro_sage                    @vibee_agent                    Leads Group
     |                              |                               |
     | "куплю крипту"               |                               |
     |----------------------------->|                               |
     |                         [Detect trigger]                     |
     |   "Привет, давай в личку"    |                               |
     |<-----------------------------|                               |
     |                              |------------------------------>|
     |                              |    Lead Card #144022504       |
```

## Lead Card формат

```
🔔 НОВЫЙ ЛИД #144022504
━━━━━━━━━━━━━━━

📌 Статус: 🆕 New → Contacted → Qualified → Won
📊 Качество: ⭐✨ (3/10)
🎯 Намерение: 💰 Покупка
⏰ Срочность: 🟢 Обычная

📍 Источник: Aimly.io dev
👤 Клиент: Dmitrii
```

## Success Criteria

- [x] Агент ответил на триггер (~15 сек)
- [x] Ответ содержит приглашение в личку
- [x] Lead Card переслана в Leads группу
- [x] Lead Card содержит CRM поля

## Troubleshooting

| Проблема | Решение |
|----------|---------|
| Агент не ответил | `fly logs -a vibee-mcp \| grep trigger` |
| Lead Card не переслана | `fly logs -a vibee-mcp \| grep forward` |
| Session not found | Проверить авторизацию сессий |
| Pattern не совпал | Проверить формат ответа в истории |

### Диагностика через логи

**Ключевые log tags для поиска:**

```bash
# Проверка триггеров
fly logs -a vibee-mcp | grep -E "trigger|TRIGGER"

# Проверка форвардинга
fly logs -a vibee-mcp | grep -E "forward|FORWARD"

# Полный flow одного сообщения
fly logs -a vibee-mcp | grep -E "trigger|forward|msg"
```

### vibe_logger структура

```gleam
// Trigger check
vibe_logger.new("trigger")
  |> with_data("chat_id", json.string(chat_id))
  |> with_data("has_trigger", json.bool(has_trigger))

// Forward trigger
vibe_logger.new("forward_trigger")
  |> with_data("chat_id", json.string(chat_id))

// Forward execution
vibe_logger.new("forward")
  |> with_data("target_chat_id", json.string(target))
  |> with_data("from_id", json.int(from_id))
```

### E2E тест проверяет

1. **Только НОВЫЕ сообщения** - запоминает last_msg_id перед триггером
2. **Уникальный триггер** - добавляет timestamp `[E2E:1734858123456]`
3. **Два паттерна**:
   - Ответ агента: `личку|напиши|помогу`
   - Lead Card: `ЛИД|Клиент|крипт`

### Критические файлы

| Файл | Назначение |
|------|------------|
| `telegram_agent.gleam:660-760` | Trigger detection + forward logic |
| `dialog_forwarder.gleam:90-190` | Lead Card creation + send |
| `trigger_chats.gleam` | Chat configs + triggers |
| `e2e_handlers.gleam` | E2E test endpoint |

### Common Issues

1. **E2E показывает passed, но Lead Card нет** → Тест нашёл старую карточку. Исправлено через last_msg_id tracking.

2. **io.println не видно в логах** → Использовать vibe_logger! `io.println` не выводится в production.

3. **Триггер найден, forward не вызван** → Проверить `find_chat_config(chat_id)` - нормализация ID может отличаться.
