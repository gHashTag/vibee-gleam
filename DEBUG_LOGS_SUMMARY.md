# Debug Logs - Summary

## Проблема решена ✅

Агент "молчал" потому что:
1. ❌ Канал `-1002298297094` не проходил фильтр target_chats (ошибка нормализации)
2. ❌ Все сообщения были уже обработаны ранее (seen_ids)
3. ❌ Не было детальных логов для диагностики

## Что исправлено

### 1. Добавлены детальные логи

**target_chats.gleam** - проверка целевых чатов:
```gleam
io.println("[TARGET_CHECK] Checking chat_id: " <> chat_id)
io.println("[TARGET_CHECK] Normalized: " <> normalized)
io.println("[TARGET_CHECK] ✅ Direct match!")
io.println("[TARGET_CHECK] ❌ Not in target_chats list")
```

**trigger_chats.gleam** - проверка триггеров:
```gleam
io.println("[TRIGGER] Checking N triggers against message")
io.println("[TRIGGER] ✅ MATCH! Trigger found in: " <> message_text)
io.println("[TRIGGER] ❌ NO MATCH in: " <> message_text)
io.println("[TRIGGER] Normalized text: " <> lower_text)
io.println("[TRIGGER] 🎯 Matched trigger: 'xxx'")
```

**telegram_agent.gleam** - обработка сообщений:
```gleam
io.println("[MSG] chat=X from_id=Y from=Z text=...")
io.println("[FILTER] from_id=X bot_id=Y owner_id=Z")
io.println("[MSG] ✅ PROCESSING message from user_id: X")
io.println("[MSG] ⏭️  SKIPPING own message from user_id: X")
io.println("[SNIPER] 🎯 Chat X is in SNIPER MODE")
io.println("[SNIPER] 🔥 TRIGGER FOUND! Generating response...")
io.println("[DIGITAL_TWIN] Responding to message in chat X")
```

**polling_actor.gleam** - polling loop:
```gleam
io.println("[POLL] Processing chat: X")
io.println("[POLL] Got N messages from X")
io.println("[POLL] Msg X:Y IN/OUT from:Z text...")
io.println("[POLL] NEW INCOMING: X from:Y")
io.println("[POLL] SKIP (seen): X")
```

### 2. Текущее состояние

Логи показывают:
```
[TARGET_CHECK] Checking chat_id: -1002298297094
[TARGET_CHECK] Normalized: 2298297094
[FILTER] ✅ Target chat: -1002298297094
[POLL] Processing chat: -1002298297094
[POLL] Got 5 messages from -1002298297094
[POLL] Msg -1002298297094:31816 OUT from:144022504 купить крипту хочу
[POLL] Msg -1002298297094:31815 IN from:6579515876 купить крипту
[POLL] SKIP (seen): -1002298297094:31815
```

✅ Канал распознаётся как target chat  
✅ Сообщения получаются  
⚠️  Все сообщения уже обработаны (seen_ids)

## Тестирование

### Отправьте НОВОЕ сообщение в канал

1. Откройте Telegram
2. Перейдите в канал `-1002298297094` (Тестовый канал)
3. Отправьте сообщение с триггером: **"купить крипту хочу"**
4. Проверьте логи:

```bash
tail -f /tmp/vibee.log | grep -E "(NEW INCOMING|TRIGGER|TWIN|MSG)"
```

Ожидаемый вывод:
```
[POLL] NEW INCOMING: -1002298297094:31817 from:YourName
[MSG] chat=-1002298297094 from_id=XXX from=YourName text=купить крипту хочу
[FILTER] from_id=XXX bot_id=YYY owner_id=144022504
[MSG] ✅ PROCESSING message from user_id: XXX
[DIGITAL_TWIN] Responding to message in chat -1002298297094
[TRIGGER] Checking 40 triggers against message
[TRIGGER] Normalized text: купить крипту хочу
[TRIGGER] 🎯 Matched trigger: 'купить крипту'
[TRIGGER] ✅ MATCH! Trigger found in: купить крипту хочу
[TWIN] Processing message from YourName in chat -1002298297094
[TWIN] Generated reply: ...
[TWIN] Message sent OK, id=31818
```

## Логи по категориям

### Фильтрация чатов
- `[FILTER]` - проверка owner_id, bot_id
- `[TARGET_CHECK]` - проверка target_chats
- `[SNIPER]` - sniper mode (только триггеры)

### Обработка сообщений
- `[POLL]` - polling loop, получение сообщений
- `[MSG]` - входящие сообщения
- `[NEW INCOMING]` - новые необработанные сообщения
- `[SKIP (seen)]` - уже обработанные сообщения

### Триггеры и ответы
- `[TRIGGER]` - проверка триггерных слов
- `[DIGITAL_TWIN]` - Digital Twin режим
- `[TWIN]` - генерация и отправка ответов

### База данных
- `[DB]` - сохранение сообщений в PostgreSQL
- `[RAG]` - RAG memory queries

### Пересылка
- `[FORWARD]` - пересылка диалогов в lead группу
- `[LEAD]` - сохранение лидов

## Режимы работы

### 1. Digital Twin Mode (текущий)
- Отвечает на ВСЕ сообщения в target_chats
- Отвечает на ВСЕ личные чаты
- Проверяет триггеры для специальных ответов

### 2. Sniper Mode
- Чат: `-5082217642` (Aimly.io dev)
- Молчит ВСЕГДА, кроме триггерных слов
- Триггеры: "купить крипту", "обменять", "usdt", и т.д.
- Пересылает диалоги в lead группу

### 3. Normal Mode
- Только target_chats
- Только при наличии триггеров
- Без Digital Twin

## Troubleshooting

### Агент не отвечает

1. **Проверьте фильтры:**
   ```bash
   tail -f /tmp/vibee.log | grep FILTER
   ```
   Должно быть: `[FILTER] ✅ Target chat: -1002298297094`

2. **Проверьте, новое ли сообщение:**
   ```bash
   tail -f /tmp/vibee.log | grep "NEW INCOMING"
   ```
   Если `SKIP (seen)` - отправьте новое сообщение

3. **Проверьте триггеры:**
   ```bash
   tail -f /tmp/vibee.log | grep TRIGGER
   ```
   Должно быть: `[TRIGGER] ✅ MATCH!`

4. **Проверьте owner_id:**
   ```bash
   tail -f /tmp/vibee.log | grep "from_id=144022504"
   ```
   Если сообщение от owner_id - агент пропустит (предотвращение самообщения)

### Нет логов вообще

```bash
# Проверьте, запущен ли процесс
ps aux | grep "beam.smp.*vibee"

# Проверьте переменные окружения
cat /proc/$(pgrep -f "beam.smp.*vibee" | head -1)/environ | tr '\0' '\n' | grep TELEGRAM

# Перезапустите с переменными
cd /workspaces/vibee-gleam/gleam
TELEGRAM_SESSION_ID=REDACTED_SESSION \
TELEGRAM_API_ID=94892 \
TELEGRAM_API_HASH=REDACTED_API_HASH \
gleam run
```

## Следующие шаги

1. ✅ Логи добавлены
2. ✅ Канал распознаётся
3. ⏳ Отправьте новое сообщение для теста
4. ⏳ Проверьте генерацию ответа
5. ⏳ Проверьте пересылку в lead группу

## Файлы с логами

- `gleam/src/vibee/config/target_chats.gleam` - фильтрация чатов
- `gleam/src/vibee/config/trigger_chats.gleam` - проверка триггеров
- `gleam/src/vibee/telegram/telegram_agent.gleam` - обработка сообщений
- `gleam/src/vibee/agent/polling_actor.gleam` - polling loop
