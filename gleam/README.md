# VIBEE MCP Server

Model Context Protocol (MCP) сервер для AI-агентов с интеграцией Telegram.

## Быстрый старт

### Подключение к Claude Code

```bash
claude mcp add vibee --transport sse --url https://vibee-mcp.fly.dev/sse
```

После выполнения команды перезапустите Claude Code.

### Подключение к Cursor

Добавьте в `.cursor/mcp.json`:

```json
{
  "mcpServers": {
    "vibee": {
      "url": "https://vibee-mcp.fly.dev/sse"
    }
  }
}
```

### Подключение к другим MCP-клиентам

URL для SSE транспорта:
```
https://vibee-mcp.fly.dev/sse
```

## Доступные инструменты

### Управление сессиями

| Инструмент | Описание |
|------------|----------|
| `session_list` | Показать все Telegram сессии |
| `session_set_active` | Установить активную сессию |
| `session_create` | Создать новую сессию |

### Telegram

| Инструмент | Описание |
|------------|----------|
| `telegram_get_me` | Информация о текущем аккаунте |
| `telegram_get_dialogs` | Список диалогов |
| `telegram_get_history` | История сообщений чата |
| `telegram_send_message` | Отправить сообщение |
| `telegram_send_photo` | Отправить фото |
| `telegram_send_buttons` | Отправить сообщение с кнопками |

### Авторизация

| Инструмент | Описание |
|------------|----------|
| `auth_status` | Статус авторизации |
| `auth_send_code` | Отправить код авторизации |
| `auth_verify_code` | Подтвердить код |

### RAG (Retrieval-Augmented Generation)

| Инструмент | Описание |
|------------|----------|
| `telegram_parse_chat` | Парсинг чата в базу данных |
| `telegram_parse_all_dialogs` | Парсинг всех диалогов |
| `conversation_search` | Поиск по истории сообщений |
| `conversation_get_context` | Получить контекст для AI Digital Clone |

### Файлы и система

| Инструмент | Описание |
|------------|----------|
| `file_read` | Читать файл |
| `file_write` | Записать файл |
| `file_list` | Список файлов |

## Примеры использования

### Работа с несколькими аккаунтами

```
# Посмотреть все сессии
> session_list

# Создать новую сессию
> session_create phone="+79001234567"

# Авторизовать
> auth_send_code phone="+79001234567"
> auth_verify_code code="12345" phone="+79001234567"

# Переключить активную сессию
> session_set_active session_id="sess_xyz789"

# Использовать активную сессию (по умолчанию)
> telegram_get_dialogs limit=10

# Или явно указать сессию
> telegram_send_message session_id="sess_abc123" chat_id="123" text="Hello"
```

### Получение диалогов

```
> telegram_get_dialogs limit=20
```

### Отправка сообщения

```
> telegram_send_message chat_id="-1001234567890" text="Привет!"
```

### Поиск по истории

```
> conversation_search query="важная тема" limit=10
```

## Локальная разработка

### Требования

- [Gleam](https://gleam.run/) >= 1.0
- Erlang/OTP >= 26
- Go >= 1.21 (для Telegram Bridge)

### Сборка

```bash
cd vibee/gleam
gleam build
```

### Запуск MCP сервера

```bash
gleam run -m mcp_server
```

### Запуск Telegram Bridge

```bash
cd vibee/go-bridge
go run .
```

## Архитектура

```
┌─────────────────────────────────────────────────────────┐
│                    Claude Code / Cursor                  │
│                      (MCP Client)                        │
└─────────────────────┬───────────────────────────────────┘
                      │ SSE
                      ▼
┌─────────────────────────────────────────────────────────┐
│                  VIBEE MCP Server                        │
│                    (Gleam/BEAM)                          │
│  ┌───────────────┐  ┌───────────────┐  ┌─────────────┐  │
│  │ Session Mgr   │  │  Tool Router  │  │  RAG Tools  │  │
│  │    (ETS)      │  │               │  │             │  │
│  └───────────────┘  └───────────────┘  └─────────────┘  │
└─────────────────────┬───────────────────────────────────┘
                      │ HTTP
                      ▼
┌─────────────────────────────────────────────────────────┐
│                  Go Telegram Bridge                      │
│                    (MTProto)                             │
│  ┌───────────────────────────────────────────────────┐  │
│  │  map[session_id]*telegram.Client                  │  │
│  │  (Multi-account support)                          │  │
│  └───────────────────────────────────────────────────┘  │
└─────────────────────┬───────────────────────────────────┘
                      │ MTProto
                      ▼
┌─────────────────────────────────────────────────────────┐
│                   Telegram API                           │
└─────────────────────────────────────────────────────────┘
```

## Деплой

Сервер развёрнут на Fly.io:

```bash
cd vibee/gleam
fly deploy
```

## Лицензия

MIT
