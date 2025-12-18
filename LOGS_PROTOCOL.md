# 🔌 Vibee Logs Protocol

## Как работают логи

Логи транслируются через **WebSocket** в реальном времени.

### Протокол

- **URL**: `wss://vibee-mcp.fly.dev/ws/logs`
- **Протокол**: WebSocket (RFC 6455)
- **Формат**: JSON
- **Кодировка**: UTF-8

### Формат сообщений

```json
{
  "timestamp": "2025-12-18T12:05:17Z",
  "level": "info",
  "logger": "TG",
  "message": "chat=-1002737186844 NeuroBlogger: 👤 USER | OM_AI_Digital_studio_bot",
  "trace_id": null,
  "request_id": null,
  "session_id": null,
  "span_id": null,
  "tool": null,
  "extra": []
}
```

### Уровни логов

- `trace` - Очень детальные логи (отладка)
- `debug` - Отладочная информация
- `info` - Информационные сообщения
- `warn` - Предупреждения
- `error` - Ошибки
- `critical` - Критические ошибки

## Способы просмотра логов

### 1. 🌐 Web UI (рекомендуется)

Откройте в браузере:
```
https://vibee-mcp.fly.dev/dashboard
```

**Преимущества**:
- ✅ Красивый интерфейс
- ✅ Фильтрация и поиск
- ✅ Цветовая подсветка
- ✅ Статистика
- ✅ Автоматическое переподключение

### 2. 🖥️ HTML Monitor (локально)

Откройте файл в браузере:
```bash
open test_ws.html
```

**Преимущества**:
- ✅ Работает локально
- ✅ Экспорт логов в файл
- ✅ Счетчики сообщений
- ✅ Uptime мониторинг

### 3. 🐍 Python Script

```bash
# Установите websockets
pip3 install websockets

# Запустите скрипт
python3 test_ws.py
```

**Код**:
```python
import asyncio
import websockets
import json

async def listen_logs():
    uri = "wss://vibee-mcp.fly.dev/ws/logs"
    async with websockets.connect(uri) as websocket:
        print("✅ Connected! Listening for logs...")
        async for message in websocket:
            try:
                data = json.loads(message)
                print(f"[{data['timestamp']}] {data['level']}: {data['message']}")
            except:
                print(message)

asyncio.run(listen_logs())
```

### 4. 🦀 websocat (CLI)

```bash
# Установите websocat
cargo install websocat

# Подключитесь
websocat wss://vibee-mcp.fly.dev/ws/logs
```

### 5. 🌐 wscat (Node.js)

```bash
# Установите wscat
npm install -g wscat

# Подключитесь
wscat -c wss://vibee-mcp.fly.dev/ws/logs
```

### 6. 📜 curl + websocat

```bash
# Через websocat
websocat wss://vibee-mcp.fly.dev/ws/logs | jq .
```

### 7. 🔧 JavaScript (Browser Console)

Откройте консоль браузера (F12) и выполните:

```javascript
const ws = new WebSocket('wss://vibee-mcp.fly.dev/ws/logs');

ws.onopen = () => console.log('✅ Connected');
ws.onmessage = (e) => {
    try {
        const data = JSON.parse(e.data);
        console.log(`[${data.level}] ${data.message}`);
    } catch {
        console.log(e.data);
    }
};
ws.onerror = (e) => console.error('❌ Error:', e);
ws.onclose = () => console.log('❌ Disconnected');
```

## Примеры логов

### Системные логи
```json
{
  "timestamp": "2025-12-18T12:00:00Z",
  "level": "info",
  "logger": "SYS",
  "message": "🚀 VIBEE System Starting..."
}
```

### Telegram сообщения
```json
{
  "timestamp": "2025-12-18T12:05:17Z",
  "level": "info",
  "logger": "TG",
  "message": "chat=-1002737186844 NeuroBlogger: 👤 USER | OM_AI_Digital_studio_bot"
}
```

### Polling статус
```json
{
  "timestamp": "2025-12-18T12:00:00Z",
  "level": "info",
  "logger": "SYS",
  "message": "🔄 Polling #10 - checking for new messages..."
}
```

### Ошибки
```json
{
  "timestamp": "2025-12-18T12:00:00Z",
  "level": "error",
  "logger": "API",
  "message": "Failed to connect to Telegram API"
}
```

## Архитектура

```
Telegram → Go Bridge → Gleam App → logging.gleam
                                        ↓
                                  log_aggregator
                                        ↓
                                  WebSocket Handler
                                        ↓
                                  wss://vibee-mcp.fly.dev/ws/logs
                                        ↓
                                  Clients (Browser, CLI, etc)
```

## Поток данных

1. **Источник**: Telegram сообщение приходит в Go Bridge
2. **Polling**: Gleam приложение опрашивает Go Bridge каждые 5 секунд
3. **Обработка**: `telegram_agent.handle_incoming_message()` обрабатывает сообщение
4. **Логирование**: `logging.telegram_message()` создает лог
5. **Публикация**: `log_aggregator.log()` публикует в WebSocket
6. **Трансляция**: WebSocket Handler отправляет всем подписчикам
7. **Отображение**: Клиенты получают и отображают логи

## Подписка на логи

WebSocket endpoint автоматически подписывается на `log_aggregator` при подключении:

```gleam
// gleam/src/vibee/api/router.gleam
fn logs_websocket_handler(req: Request(Connection)) -> Response(ResponseData) {
  let log_subject = process.new_subject()
  
  // Подписываемся на log_aggregator
  case log_aggregator.get_global() {
    Some(aggregator) -> log_aggregator.subscribe(aggregator, log_subject)
    None -> Nil
  }
  
  // Создаем WebSocket
  mist.websocket(
    request: req,
    on_init: fn(_conn) { #(state, Some(selector)) },
    handler: handle_log_ws_message,
  )
}
```

## Отладка

### Проверить, работает ли WebSocket

```bash
curl -i -N \
  -H "Connection: Upgrade" \
  -H "Upgrade: websocket" \
  -H "Sec-WebSocket-Version: 13" \
  -H "Sec-WebSocket-Key: $(openssl rand -base64 16)" \
  https://vibee-mcp.fly.dev/ws/logs
```

Должен вернуть `101 Switching Protocols`.

### Проверить логи на сервере

```bash
flyctl logs --app vibee-mcp | grep -E "INFO|TG:"
```

### Проверить log_aggregator

```bash
flyctl logs --app vibee-mcp | grep "log_aggregator\|LOGS"
```

Должно быть:
```
[LOGS] ✓ Log aggregator started
```

## Troubleshooting

### WebSocket не подключается

1. Проверьте URL: `wss://vibee-mcp.fly.dev/ws/logs` (не `ws://`)
2. Проверьте, что приложение запущено: `curl https://vibee-mcp.fly.dev/health`
3. Проверьте логи: `flyctl logs --app vibee-mcp`

### Логи не приходят

1. Проверьте, что log_aggregator запущен:
   ```bash
   flyctl logs --app vibee-mcp | grep "Log aggregator"
   ```

2. Проверьте, что есть новые сообщения в Telegram:
   ```bash
   flyctl logs --app vibee-mcp | grep "NEW INCOMING"
   ```

3. Отправьте тестовое сообщение в чат **Agent Vibe** (-1002737186844)

### WebSocket отключается

Это нормально! Reconnect происходит автоматически каждые 3 секунды в UI.

## Производительность

- **Буфер**: 1000 последних логов в памяти
- **Частота**: Логи отправляются мгновенно при появлении
- **Подписчики**: Неограниченное количество одновременных подключений
- **Формат**: JSON, ~200-500 байт на сообщение

## Безопасность

- ✅ HTTPS/WSS (TLS 1.3)
- ✅ Нет аутентификации (логи публичные)
- ⚠️ Не содержат секретов или токенов
- ⚠️ Могут содержать chat_id и user_id

## Интеграции

### Grafana

Используйте WebSocket data source:
```
wss://vibee-mcp.fly.dev/ws/logs
```

### Datadog

Используйте custom log forwarder через WebSocket.

### Elasticsearch

Создайте Logstash pipeline с WebSocket input.

## Итог

**Самый простой способ**: Откройте [https://vibee-mcp.fly.dev/dashboard](https://vibee-mcp.fly.dev/dashboard)

**Для разработки**: Используйте `test_ws.html` или `test_ws.py`

**Для автоматизации**: Подключитесь к `wss://vibee-mcp.fly.dev/ws/logs` через WebSocket клиент

Все логи из Telegram идут в реальном времени! 🚀
