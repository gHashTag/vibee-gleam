# 🔍 Почему логи не обновляются онлайн

## Проблема

Логи в dashboard не обновляются в реальном времени, хотя WebSocket подключен.

## Причины

### 1. Агент не запущен

**Проверка:**
```bash
curl https://vibee-mcp.fly.dev/api/agent/status
```

**Если status: "stopped"** - агент не работает, логи не генерируются.

**Решение:**
1. Открыть dashboard: https://vibee-mcp.fly.dev/dashboard/agent
2. Нажать "▶ Start Agent"

### 2. Нет активности в Telegram

**Проверка:**
- Открыть https://vibee-mcp.fly.dev/logs
- Посмотреть последнее сообщение

**Если логи старые** - нет новых сообщений в мониторимых чатах.

**Решение:**
1. Отправить тестовое сообщение в целевую группу
2. Или написать боту в личку

### 3. WebSocket не подключается

**Проверка:**
1. Открыть dashboard
2. Нажать F12 (Developer Tools)
3. Перейти в Console
4. Искать "WebSocket connected" или ошибки

**Если ошибка "WebSocket failed":**

**Решение:**
1. Проверить что URL правильный: `wss://vibee-mcp.fly.dev/ws/logs`
2. Обновить страницу (F5)
3. Проверить что сервер работает: `curl https://vibee-mcp.fly.dev/health`

### 4. Логи не отправляются в WebSocket

**Проверка:**
```bash
# Проверить что WebSocket endpoint работает
curl -i -N -H "Connection: Upgrade" -H "Upgrade: websocket" \
  -H "Sec-WebSocket-Version: 13" -H "Sec-WebSocket-Key: test" \
  https://vibee-mcp.fly.dev/ws/logs
```

**Если 404** - endpoint не существует или не работает.

**Решение:**
- Проверить router.gleam:
```gleam
http.Get, ["ws", "logs"] -> logs_websocket_handler(req)
```

### 5. Polling не работает

**Проверка логов сервера:**
```bash
# Через Fly.io
flyctl logs --app vibee-mcp

# Искать:
# [POLL] Processing chat: ...
# [POLL] Got X messages from ...
```

**Если нет логов [POLL]** - polling actor не запущен.

**Решение:**
1. Проверить что TELEGRAM_SESSION_ID установлен
2. Проверить что Go bridge доступен
3. Перезапустить приложение

## Текущий статус

### ✅ Что работает:
- WebSocket endpoint: `/ws/logs`
- API endpoint: `/api/agent/status`
- Dashboard UI
- Логи страница: `/logs`

### ❌ Что НЕ работает:
- Polling actor не запущен (нет логов [POLL])
- Агент не обрабатывает сообщения
- Логи не генерируются

## Как запустить агента

### Вариант 1: Через environment variables

```bash
# Установить переменные
export TELEGRAM_SESSION_ID="REDACTED_SESSION"
export TELEGRAM_API_ID="94892"
export TELEGRAM_API_HASH="REDACTED_API_HASH"
export OPENROUTER_API_KEY="sk-or-xxx"
export VIBEE_BRIDGE_URL="https://vibee-telegram-bridge.fly.dev"

# Перезапустить
flyctl restart --app vibee-mcp
```

### Вариант 2: Через Fly.io secrets

```bash
# Установить secrets
flyctl secrets set TELEGRAM_SESSION_ID="REDACTED_SESSION" --app vibee-mcp
flyctl secrets set OPENROUTER_API_KEY="sk-or-xxx" --app vibee-mcp

# Автоматически перезапустится
```

### Вариант 3: Через код

В `gleam/src/vibee.gleam`:

```gleam
// Убедиться что polling actor запускается
let polling_subject = polling_actor.start_with_events(
  agent_config,
  event_bus_subject,
)
```

## Проверка что все работает

### 1. Проверить health
```bash
curl https://vibee-mcp.fly.dev/health
# Должно быть: {"status":"ok","service":"vibee","version":"0.1.0"}
```

### 2. Проверить agent status
```bash
curl https://vibee-mcp.fly.dev/api/agent/status
# Должно быть: {"id":"vibee_agent_1","status":"running",...}
```

### 3. Проверить логи сервера
```bash
flyctl logs --app vibee-mcp
# Должны быть:
# [POLL] Processing chat: ...
# [POLL] Got X messages from ...
# [LOG_AGG] Broadcasting to X subscribers...
```

### 4. Проверить WebSocket
1. Открыть https://vibee-mcp.fly.dev/dashboard/agent
2. F12 → Console
3. Должно быть: "WebSocket connected"
4. Отправить тестовое сообщение в Telegram
5. Лог должен появиться в dashboard

## Временное решение

Пока агент не запущен, можно:

### 1. Смотреть старые логи
```
https://vibee-mcp.fly.dev/logs
```

### 2. Проверять API напрямую
```bash
curl https://vibee-mcp.fly.dev/api/v1/telegram/dialogs
curl https://vibee-mcp.fly.dev/api/v1/telegram/all-messages
```

### 3. Смотреть логи сервера
```bash
flyctl logs --app vibee-mcp --follow
```

## Следующие шаги

1. **Запустить polling actor**
   - Проверить environment variables
   - Убедиться что session авторизован
   - Проверить что Go bridge доступен

2. **Добавить UI для запуска агента**
   - Кнопка "Start Agent" должна реально запускать
   - Показывать статус (running/stopped)
   - Показывать ошибки если не запускается

3. **Добавить мониторинг**
   - Показывать когда последний раз были логи
   - Алерт если логов нет > 5 минут
   - Автоматический перезапуск при падении

4. **Улучшить debugging**
   - Больше логов в Console
   - Показывать статус WebSocket
   - Показывать количество подписчиков

## Контакты

Если проблема не решается:
- Telegram: @neuro_sage
- GitHub Issues: https://github.com/gHashTag/vibee-gleam/issues
- Логи сервера: `flyctl logs --app vibee-mcp`
