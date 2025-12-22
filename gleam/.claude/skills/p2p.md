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

4. **Deduplication блокирует Lead Card 24h** → E2E сообщения с `[E2E:...]` автоматически bypass dedup check.

5. **Username пустой в Lead Card** → Проверить логи `username_empty: true/false`. Если true - Go Bridge не получает username.

6. **WebFetch кэширует test_run_id** → Кэш 15 минут. Добавить `?t=timestamp` к URL или использовать curl.

### Verified 22.12.2025

**Логи подтверждают работу:**
```json
{
  "logger": "forward",
  "message": "forward_dialog_with_context CALLED",
  "username": "neuro_sage",
  "username_empty": false,
  "from": "Dmitrii"
}

{
  "logger": "forward",
  "message": "E2E test detected - skipping dedup check"
}

{
  "logger": "forward",
  "message": "Dialog forwarded successfully"
}
```

**Username flow работает:** Go Bridge → polling_actor → telegram_agent → dialog_forwarder

**Lead Card формат с @username:**
```
👤 Клиент: @neuro_sage (Dmitrii)
```

## Архитектура Username Flow

```
Go Bridge (client.go:745)
    userMap[user.ID].Username
              ↓
polling_actor.gleam:795
    extract_json_field("username")
              ↓
telegram_agent.gleam:719
    MessageInfo.username
              ↓
dialog_forwarder.gleam:217
    "@" <> username <> " (" <> name <> ")"
```

## Архитектура Deduplication

```
dialog_forwarder.gleam:112
    check_recent_forward(user_id, target_chat_id)
              ↓
SQL: SELECT COUNT(*) FROM lead_forwards
     WHERE user_id = X
     AND forwarded_at > NOW() - '24 hours'
              ↓
    True = Skip (дубликат)
    False = Forward (новый лид)
```

**E2E Bypass:** Сообщения с `[E2E:...]` в тексте пропускают dedup check.

## Полезные команды диагностики

```bash
# Username в сообщениях
fly logs -a vibee-mcp | grep -E "username.*neuro_sage|username_empty"

# Forward flow
fly logs -a vibee-mcp | grep -E "forward_dialog|Dialog forwarded"

# E2E bypass
fly logs -a vibee-mcp | grep "E2E test detected"

# Полный P2P flow
fly logs -a vibee-mcp | grep -E "trigger.*5082217642|forward|Lead"

# Проверка что триггер-чат обрабатывается
fly logs -a vibee-mcp | grep "Aimly.io dev"
```

## Известные ограничения

1. **ETS не персистентный** - После рестарта Fly.io машины test_run_id теряется
2. **WebFetch кэш 15 мин** - Использовать `?t=timestamp` для bust cache
3. **E2E тест может показать failed** - Если @neuro_sage не имеет доступа к Leads группе

## Изменения 22.12.2025

| Файл | Что добавлено |
|------|---------------|
| `polling_actor.gleam:798` | vibe_logger для username диагностики |
| `dialog_forwarder.gleam:114-125` | E2E bypass для deduplication |
| `dialog_forwarder.gleam:103` | username_empty в структурированных логах |
| `telegram_agent.gleam:598-613` | `is_message_for_other_user()` - фильтр @mentions |
| `telegram_agent.gleam:487-502` | Пропуск триггеров для сообщений `@OtherUser ...` |
| `telegram_agent.gleam:509-524` | Пропуск проактивных ответов для `@OtherUser ...` |

## Fix: @mention Filtering

**Проблема:** Агент отвечал на сообщения вида `@GnothySeaton ты заходишь?` даже если они адресованы другому пользователю.

**Решение:** Добавлена функция `is_message_for_other_user(text)` которая:
- Проверяет начинается ли сообщение с `@`
- Если да и это НЕ `@vibee_agent` - пропускаем
- Применяется и к триггерам, и к проактивному режиму

**Логи при пропуске:**
```json
{
  "logger": "sniper",
  "message": "TRIGGER FOUND but message is for another user (@mention), skipping",
  "trigger": true,
  "skip_reason": "message_for_other_user"
}
```

## Fix: Real Users Not Getting Lead Cards (22.12.2025)

**Проблема:** E2E тесты работали, но реальные пользователи НЕ получали Lead Cards.

**Root Cause:** Код использовал `shellout.command("psql")` для DB операций, который:
1. Падал молча на Fly.io (psql не гарантирован в контейнере)
2. Ошибки игнорировались → `check_recent_forward()` возвращал `False`
3. E2E bypass скрывал проблему (сообщения с `[E2E:]` не проходят dedup check)

**Решение:** Заменить `shellout` на `pog` (Gleam PostgreSQL библиотека).

### Изменения в dialog_forwarder.gleam

```diff
- import shellout
+ import pog
+ import vibee/db/postgres
```

**check_recent_forward()** - теперь использует pog:
```gleam
case postgres.get_global_pool() {
  None -> False  // Нет пула - пропускаем dedup
  Some(pool) -> {
    let sql = "SELECT COUNT(*)::int FROM lead_forwards
               WHERE user_id = $1
               AND target_chat_id = $2
               AND status = 'forwarded'
               AND forwarded_at > NOW() - INTERVAL '1 hour'"

    case pog.query(sql)
      |> pog.parameter(pog.int(user_id))
      |> pog.parameter(pog.int(target_id))
      |> pog.returning(count_decoder)
      |> pog.execute(pool)
    {
      Ok(pog.Returned(_, [count])) -> count > 0
      _ -> False
    }
  }
}
```

**log_forward_to_db()** - тоже переписан на pog.

### Новые логи

```json
{"logger":"dedup_check","message":"Dedup check complete","is_duplicate":false,"count":0,"user_id":412973735}

{"logger":"forward_send","message":"Lead Card sent successfully","msg_id":625}
```

### Dedup Window

Изменено с 24 часов на **1 час** - один пользователь может получить максимум 1 Lead Card в час.

### Коммит

```
4b8195f fix: Use pog instead of shellout for lead_forwards DB operations
```

### Проверено

- Real user @GnothySeaton → `dedup_check: is_duplicate=false` → Lead Card #625 отправлена
- E2E тесты продолжают работать (bypass dedup)

### Урок

**Всегда деплоить изменения!** Git diff показал что код был исправлен локально, но не закоммичен и не задеплоен.
