# Fix: Test Channel 2298297094

## Проблема
Агент не отвечает в тестовом канале 2298297094.

## Причина
Канал не был добавлен в список `target_chats` для мониторинга.

## Решение

### Добавлен канал в конфигурацию

**Файл**: `gleam/src/vibee/config/target_chats.gleam`

```gleam
pub const target_chats = [
  "693774948",    // Личный чат для тестов
  "2737186844",   // VIBEE AGENT (supergroup) - Lead group
  "2298297094",   // Тестовый канал (supergroup) ← ДОБАВЛЕНО
  "6579515876",   // vibee_agent bot
  "-5082217642",  // Aimly.io dev (group) - SNIPER MODE
]
```

## Как работает

### Режим работы: Digital Twin

Канал 2298297094 будет работать в **Digital Twin режиме**:
- ✅ Отвечает на **ВСЕ** сообщения (не только триггеры)
- ✅ Использует AI для генерации ответов
- ✅ Естественный стиль общения

### Если нужен Sniper Mode

Если хотите, чтобы агент отвечал **только на триггеры**, добавьте в `trigger_chats.gleam`:

```gleam
pub fn get_trigger_chats() -> List(TriggerChatConfig) {
  [
    // Существующий Aimly.io dev
    TriggerChatConfig(
      chat_id: "-5082217642",
      chat_name: "Aimly.io dev",
      // ...
    ),
    
    // Новый тестовый канал
    TriggerChatConfig(
      chat_id: "2298297094",
      chat_name: "Тестовый канал",
      chat_type: "supergroup",
      is_active: True,
      can_write: True,
      response_probability: 0.0,  // Sniper Mode
      custom_triggers: [
        "куплю крипту",
        "купить крипту",
        "где купить",
        // ... остальные триггеры
      ],
      response_template: "Привет! Могу помочь с покупкой крипты. Пиши в личку для деталей.",
      forward_chat_id: "2737186844",  // Lead group
    ),
  ]
}
```

## Тестирование

### Test 1: Digital Twin Mode (текущий)

**Отправьте любое сообщение** в канал 2298297094:
```
привет как дела?
```

**Ожидается**: AI ответ

**Логи:**
```bash
fly logs --app vibee-mcp | grep "2298297094"
```

Должно быть:
```
[POLL] Processing chat: 2298297094
[MSG] chat=2298297094 from_id=XXX text=привет как дела?
[DIGITAL_TWIN] Responding to message in chat 2298297094
[DIGITAL_TWIN] Calling OpenRouter...
```

### Test 2: Проверка мониторинга

```bash
fly logs --app vibee-mcp | grep "Processing.*chats"
```

Должно быть:
```
[POLL] Processing 6 chats (Digital Twin: ON)
[POLL] Chats: 693774948, 2737186844, 2298297094, ...
```

## Deploy

```bash
cd /workspaces/vibee-gleam
git add gleam/src/vibee/config/target_chats.gleam
git commit -m "feat: Add test channel 2298297094 to target chats

Co-authored-by: Ona <no-reply@ona.com>"
git push origin main

# Deploy
./deploy.sh
# или
fly deploy --config fly.toml
```

## Проверка после деплоя

```bash
# 1. Проверить, что канал в списке
fly logs --app vibee-mcp | grep "2298297094"

# 2. Отправить тестовое сообщение
# В канале 2298297094: "тест"

# 3. Проверить ответ агента
fly logs --app vibee-mcp | grep "2298297094" -A 5
```

## Формат chat_id

### Для supergroup (канал):
- **В конфигурации**: `2298297094` (без префикса)
- **В Telegram API**: `-1002298297094` (с префиксом -100)
- **Автоматическая нормализация**: `normalize_chat_id()` обрабатывает оба формата

### Примеры:
- `2298297094` → нормализуется в `2298297094`
- `-1002298297094` → нормализуется в `2298297094`
- `-5082217642` → остаётся `-5082217642` (обычная группа)

## Troubleshooting

### Проблема: Агент всё ещё не отвечает

**Проверить:**
```bash
# 1. Канал в списке?
fly logs --app vibee-mcp | grep "Processing.*chats"

# 2. Сообщения приходят?
fly logs --app vibee-mcp | grep "2298297094"

# 3. Digital Twin включён?
fly logs --app vibee-mcp | grep "Digital Twin"
```

**Возможные причины:**

1. **Агент не в канале**
   - Добавьте @neuro_sage в канал
   - Дайте права на отправку сообщений

2. **Канал не в списке диалогов**
   - Увеличьте лимит: `limit: 50` → `limit: 100` в `polling_actor.gleam`

3. **Фильтр по chat_id**
   - Проверьте `should_process_chat_with_mode()`
   - Убедитесь, что Digital Twin включён

### Проблема: Агент отвечает на свои сообщения

**Проверить:**
```bash
fly logs --app vibee-mcp | grep "FILTER"
```

Должно быть:
```
[FILTER] from_id=XXX bot_id=144022504
[FILTER] is_bot=NO
[MSG] ✅ PROCESSING message
```

Если `is_bot=YES` - это сообщение от самого агента, оно правильно пропускается.

---

**Status**: Fixed, ready to deploy
**Last Updated**: 2025-12-18 05:26 UTC
