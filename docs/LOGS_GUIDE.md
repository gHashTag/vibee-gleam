# 🐝 Vibee Logs Guide

## Проблема была

Логи из Telegram не отображались в dashboard, потому что:

1. ❌ **Фейковые логи**: Dashboard показывал симулированные логи из JavaScript
2. ❌ **VIBEE_MODE=mcp**: Приложение запускалось в MCP режиме вместо Telegram
3. ❌ **Log aggregator запускался поздно**: После инициализации Telegram агента
4. ❌ **Нет новых сообщений**: Агент работал, но не было новых сообщений для логирования

## Что исправлено

1. ✅ Убрал фейковые логи из dashboard
2. ✅ Изменил `VIBEE_MODE=telegram`
3. ✅ Переместил запуск log_aggregator в начало
4. ✅ Добавил интеграцию `logging.gleam` → `log_aggregator` → WebSocket
5. ✅ Добавил реальные timestamps
6. ✅ Добавил логирование polling каждые 10 циклов

## Где логи

### 📊 Dashboard с real-time логами
**URL**: [https://vibee-mcp.fly.dev/dashboard](https://vibee-mcp.fly.dev/dashboard)

**WebSocket**: `wss://vibee-mcp.fly.dev/ws/logs`

**Что показывает**:
- 🔄 Polling статус каждые 10 циклов (50 секунд)
- 📨 Все входящие сообщения из Telegram
- ⚠️ Ошибки и предупреждения
- ℹ️ Системные события

### 🎯 Aimly CRM
**URL**: [https://vibee-mcp.fly.dev/aimly/leads](https://vibee-mcp.fly.dev/aimly/leads)

Брендированная страница для клиентов с управлением лидами.

### 📋 Leads Management
**URL**: [https://vibee-mcp.fly.dev/leads](https://vibee-mcp.fly.dev/leads)

Техническая страница управления лидами.

## Как работает логирование

### 1. Источник логов

```gleam
// gleam/src/vibee/logging.gleam
pub fn log(level: LogLevel, ctx: LogContext, message: String) -> Nil {
  io.println(line)  // Stdout для Fly.io
  
  // Публикуем в log_aggregator
  case log_aggregator.get_global() {
    Some(aggregator) -> log_aggregator.log(aggregator, entry)
    None -> Nil
  }
}
```

### 2. Log Aggregator

```gleam
// gleam/src/vibee/log_aggregator.gleam
pub type Message {
  Log(LogEntry)
  Subscribe(Subject(String))
  Unsubscribe(Subject(String))
}
```

- Собирает логи от всех компонентов
- Хранит последние 1000 записей
- Рассылает подписчикам через WebSocket

### 3. WebSocket Handler

```gleam
// gleam/src/vibee/api/router.gleam
fn logs_websocket_handler(req: Request(Connection)) -> Response(ResponseData) {
  // Подписываемся на log_aggregator
  log_aggregator.subscribe(aggregator, log_subject)
  
  // Отправляем логи клиенту
  mist.websocket(...)
}
```

### 4. Dashboard Client

```javascript
// dashboard/index.html
const ws = new WebSocket('wss://vibee-mcp.fly.dev/ws/logs');

ws.onmessage = (event) => {
  const data = JSON.parse(event.data);
  addLog(data.level, data.message, data.source);
};
```

## Примеры логов

### Системные логи
```
2025-12-18 11:50:00 INFO 🚀 VIBEE System Starting...
2025-12-18 11:50:00 INFO 📡 Log aggregator initialized and ready
2025-12-18 11:50:01 INFO 🔄 Telegram polling started (Digital Twin: OFF)
```

### Polling логи
```
2025-12-18 11:51:00 INFO 🔄 Polling #10 - checking for new messages...
2025-12-18 11:52:00 INFO 🔄 Polling #20 - checking for new messages...
```

### Telegram сообщения
```
2025-12-18 11:53:15 INFO TG: -1002737186844 Федор: Привет! Как дела?
2025-12-18 11:53:20 INFO TG: -1002737186844 Иван: Отлично, спасибо!
```

### Ошибки
```
2025-12-18 11:54:00 ERROR Failed to connect to Telegram API
2025-12-18 11:54:05 WARNING WebSocket disconnected. Reconnecting...
```

## Тестирование

### 1. Проверить WebSocket
```bash
open test_logs.html
```

### 2. Отправить тестовое сообщение
Напишите в чат **Agent Vibe** (-1002737186844) или любой другой мониторимый чат.

### 3. Проверить API
```bash
curl https://vibee-mcp.fly.dev/api/v1/telegram/dialogs
```

## Troubleshooting

### Логи не появляются

1. **Проверьте WebSocket соединение**
   - Откройте DevTools → Network → WS
   - Должно быть: `wss://vibee-mcp.fly.dev/ws/logs` (Status: 101)

2. **Проверьте режим работы**
   ```bash
   flyctl config show --app vibee-mcp | grep VIBEE_MODE
   ```
   Должно быть: `VIBEE_MODE: telegram`

3. **Проверьте polling**
   ```bash
   flyctl logs --app vibee-mcp | grep POLL
   ```
   Должны быть логи каждые 5 секунд

4. **Отправьте тестовое сообщение**
   Напишите в чат, который мониторит бот

### WebSocket отключается

Это нормально! Reconnect происходит автоматически каждые 3 секунды.

### Нет новых сообщений

Агент работает в режиме polling каждые 5 секунд. Если нет новых сообщений, логи не появятся. Отправьте сообщение в мониторимый чат.

## Архитектура

```
Telegram → Go Bridge → Gleam Polling Actor → telegram_agent.handle_incoming_message()
                                                      ↓
                                              logging.telegram_message()
                                                      ↓
                                              log_aggregator.log()
                                                      ↓
                                              WebSocket subscribers
                                                      ↓
                                              Dashboard (browser)
```

## Конфигурация

### Переменные окружения

```bash
VIBEE_MODE=telegram              # Режим работы (telegram/mcp)
TELEGRAM_SESSION_ID=sess_xxx     # ID сессии Telegram
TELEGRAM_API_ID=94892            # API ID
TELEGRAM_API_HASH=xxx            # API Hash
OPENROUTER_API_KEY=xxx           # LLM API ключ (опционально)
DATABASE_URL=postgres://...      # PostgreSQL (опционально)
```

### Fly.io секреты

```bash
flyctl secrets set VIBEE_MODE=telegram --app vibee-mcp
flyctl secrets set TELEGRAM_SESSION_ID=sess_xxx --app vibee-mcp
```

## Полезные команды

```bash
# Проверить статус
curl https://vibee-mcp.fly.dev/health

# Посмотреть логи
flyctl logs --app vibee-mcp

# Посмотреть конфиг
flyctl config show --app vibee-mcp

# Перезапустить
flyctl apps restart vibee-mcp

# Задеплоить
flyctl deploy --app vibee-mcp
```

## Итог

✅ Логи из Telegram теперь идут в dashboard в реальном времени
✅ WebSocket работает на `wss://vibee-mcp.fly.dev/ws/logs`
✅ Dashboard обновлен и показывает реальные логи
✅ Polling работает каждые 5 секунд
✅ Log aggregator запускается первым

**Откройте**: [https://vibee-mcp.fly.dev/dashboard](https://vibee-mcp.fly.dev/dashboard)

**Отправьте сообщение** в чат Agent Vibe и увидите логи! 🚀
