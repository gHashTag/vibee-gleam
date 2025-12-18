# 🔧 Почему не идут логи - РЕШЕНИЕ

## Проблема
Агент не подключен к Telegram, поэтому нет событий из ваших чатов и групп.

## Решение (2 минуты)

### Вариант 1: Через Fly.io CLI (быстро)

```bash
# 1. Установить переменные окружения
fly secrets set \
  TELEGRAM_SESSION_ID=REDACTED_SESSION \
  TELEGRAM_API_ID=94892 \
  TELEGRAM_API_HASH=REDACTED_API_HASH \
  DATABASE_URL=postgresql://neondb_owner:REDACTED_DB_PASSWORD@ep-bitter-frog-a1bewei7-pooler.ap-southeast-1.aws.neon.tech/neondb?sslmode=require \
  --app vibee-mcp

# 2. Перезапустить приложение
fly apps restart vibee-mcp

# 3. Проверить логи
fly logs --app vibee-mcp
```

### Вариант 2: Через веб-интерфейс

1. Откройте: https://fly.io/apps/vibee-mcp/secrets
2. Добавьте секреты:
   - `TELEGRAM_SESSION_ID` = `REDACTED_SESSION`
   - `TELEGRAM_API_ID` = `94892`
   - `TELEGRAM_API_HASH` = `REDACTED_API_HASH`
   - `DATABASE_URL` = `postgresql://neondb_owner:REDACTED_DB_PASSWORD@ep-bitter-frog-a1bewei7-pooler.ap-southeast-1.aws.neon.tech/neondb?sslmode=require`
3. Приложение автоматически перезапустится

---

## После настройки

### 1. Откройте страницу событий:
https://vibee-mcp.fly.dev/events

### 2. Вы увидите логи:
```
TELEGRAM_MESSAGE                    12:34:56
Привет! Хочу купить крипту

chat_id: -1002298297094
user_id: 144022504
chat_type: SUPERGROUP
```

### 3. Отправьте сообщение в любой из ваших чатов:
- VIBEE AGENT (-1002737186844)
- Тестовый канал (-1002298297094)
- Aimly.io dev (-5082217642)

### 4. Сообщение появится на странице /events в реальном времени!

---

## Проверка что работает

```bash
# 1. Проверить что секреты установлены
fly secrets list --app vibee-mcp

# 2. Проверить логи
fly logs --app vibee-mcp | grep POLL

# Должны увидеть:
# [POLL] Processing chat: -1002298297094
# [POLL] Got 5 messages from -1002298297094
```

---

## Что будет на странице /events

### Типы событий:
- 📱 **TELEGRAM_MESSAGE** - входящие сообщения из чатов
- 🤖 **AGENT_REPLY** - ответы агента
- 🎯 **TRIGGER_DETECTED** - обнаружены триггерные слова
- 🧠 **LLM_REQUEST** - запросы к AI
- 💾 **DATABASE_SAVE** - сохранение в БД
- ⚠️ **ERROR** - ошибки

### Фильтры:
- **All** - все события
- **Telegram** - только сообщения из Telegram
- **Private** - личные чаты
- **Groups** - группы
- **Supergroups** - супергруппы
- **Agent** - ответы агента
- **Triggers** - триггеры
- **Errors** - ошибки

---

## Быстрая команда (скопируйте и выполните)

```bash
fly secrets set TELEGRAM_SESSION_ID=REDACTED_SESSION TELEGRAM_API_ID=94892 TELEGRAM_API_HASH=REDACTED_API_HASH DATABASE_URL=postgresql://neondb_owner:REDACTED_DB_PASSWORD@ep-bitter-frog-a1bewei7-pooler.ap-southeast-1.aws.neon.tech/neondb?sslmode=require --app vibee-mcp && fly apps restart vibee-mcp
```

Через 30 секунд откройте: https://vibee-mcp.fly.dev/events

И отправьте сообщение в любой чат - оно появится на странице! 🚀
