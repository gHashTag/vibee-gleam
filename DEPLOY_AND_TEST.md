# 🚀 Deploy & Test Guide

## ✅ Всё готово к деплою!

### Что сделано:

1. **Production Infrastructure** ✅
   - Health check endpoint (port 8080)
   - HTTP retry logic (3 attempts, exponential backoff)
   - Graceful shutdown (SIGTERM handling)
   - Structured JSON logging
   - Optimized Dockerfile (~50MB)
   - Fly.io config with auto-rollback

2. **AI Improvements** ✅
   - Temperature 0.9 для разнообразия
   - Дружелюбный тон без эмодзи
   - Уникальные ответы на каждый триггер

3. **Lead Management** ✅
   - Пересылка в Lead группу (-1002737186844)
   - PostgreSQL schema для хранения
   - Автоматический анализ (intent, priority, crypto)
   - Детальное логирование лидов

4. **Deployment** ✅
   - deploy.sh скрипт
   - GitHub Actions workflow
   - Полная документация

---

## 🚀 Деплой (3 способа)

### Способ 1: Локально (Рекомендуется)

**На вашей машине:**

```bash
# 1. Clone repo
git pull origin main

# 2. Login to Fly.io
fly auth login

# 3. Deploy!
./deploy.sh
```

**Время**: 2-3 минуты

---

### Способ 2: GitHub Actions

**Setup (один раз):**

1. Получите Fly.io токен:
```bash
fly auth token
```

2. Добавьте в GitHub Secrets:
   - URL: https://github.com/gHashTag/vibee-gleam/settings/secrets/actions
   - Name: `FLY_API_TOKEN`
   - Value: ваш токен

3. Push в main:
```bash
git push origin main
```

GitHub Actions автоматически задеплоит!

---

### Способ 3: Manual

```bash
fly auth login

fly secrets set \
  OPENROUTER_API_KEY=REDACTED_OPENROUTER_KEY \
  TELEGRAM_API_ID=94892 \
  TELEGRAM_API_HASH=REDACTED_API_HASH \
  TELEGRAM_SESSION_ID=REDACTED_SESSION \
  TELEGRAM_SESSION_1_PHONE=+79933420465 \
  TELEGRAM_SESSION_1_USERNAME=neuro_sage \
  DATABASE_URL=postgresql://neondb_owner:REDACTED_DB_PASSWORD@ep-bitter-frog-a1bewei7-pooler.ap-southeast-1.aws.neon.tech/neondb?sslmode=require \
  --app vibee-mcp

fly deploy --config fly.toml
```

---

## 🧪 Тестирование

### Test 1: Health Check

```bash
curl https://vibee-mcp.fly.dev/health
```

**Ожидается:**
```json
{
  "status": "ok",
  "uptime_seconds": 123,
  "bridge_connected": true,
  "version": "0.1.0",
  "timestamp": 1702857600
}
```

---

### Test 2: AI Response Variety

**Действие**: Отправьте 5 раз триггер в группу Aimly.io dev

**От другого аккаунта** (не @neuro_sage):
```
куплю биткоин
```

**Ожидается**: 5 разных ответов:
1. "Привет! Могу помочь с покупкой крипты. Пиши в личку, всё расскажу."
2. "Здарова! Помогу купить. Напиши в личные сообщения, обсудим."
3. "Хей! С покупкой помогу. Пиши в личку."
4. "Могу помочь купить. Пиши в личку, всё объясню."
5. "Привет! Помогу с криптой. Напиши в ЛС."

**Проверка логов:**
```bash
fly logs --app vibee-mcp | grep "TRIGGER_REPLY"
```

Должно быть:
```
[TRIGGER_REPLY] 🤖 Calling AI to generate variation...
[TRIGGER_REPLY] ✅ Generated variation: Привет! Могу помочь...
```

---

### Test 3: Lead Forwarding

**Действие**: Отправьте триггер в Aimly.io dev

**От другого аккаунта**:
```
где купить usdt?
```

**Ожидается в Lead группе** (https://t.me/c/2737186844/1):
```
🔥 НОВЫЙ ЛИД

👤 Клиент: User
💬 Вопрос: где купить usdt?

✅ Ответ агента:
Привет! Могу помочь. Пиши в личку, обсудим детали.

📱 Действие: Клиент приглашён в личку
```

**Проверка логов:**
```bash
fly logs --app vibee-mcp | grep "FORWARD"
```

Должно быть:
```
[FORWARD] 📤 Forwarding dialog to chat -1002737186844
[FORWARD] Original: User: где купить usdt?
[FORWARD] Reply: Привет! Могу помочь...
[FORWARD] ✅ Message sent successfully
```

---

### Test 4: Lead Logging

**Проверка логов:**
```bash
fly logs --app vibee-mcp | grep "НОВЫЙ ЛИД" -A 30
```

**Ожидается:**
```
============================================================
🔥 НОВЫЙ ЛИД СОХРАНЁН
============================================================

📱 КОНТАКТНАЯ ИНФОРМАЦИЯ:
  Telegram ID: 123456789
  Username: не указан
  Имя: User

💬 ПЕРВОЕ СООБЩЕНИЕ:
  где купить usdt?

📊 АНАЛИЗ:
  Намерение: Покупка
  Интересует: USDT
  Приоритет: 🟡 Средний

🎯 ТРИГГЕРЫ:
  • где купить

✅ ОТВЕТ АГЕНТА:
  Привет! Могу помочь. Пиши в личку.

📍 ИСТОЧНИК:
  Чат: Aimly.io dev
  ID: -5082217642
```

---

### Test 5: Sniper Mode (No Trigger)

**Действие**: Отправьте сообщение БЕЗ триггера

**В Aimly.io dev**:
```
привет всем как дела?
```

**Ожидается**: Агент молчит (Sniper Mode)

**Проверка логов:**
```bash
fly logs --app vibee-mcp | grep "SNIPER"
```

Должно быть:
```
[SNIPER] 🎯 Chat -5082217642 is in SNIPER MODE
[SNIPER] Message text: привет всем как дела?
[TRIGGER] ❌ NO MATCH in: привет всем как дела?
[SNIPER] 🤫 No trigger detected, staying silent
```

---

### Test 6: Personal Chat (Digital Twin)

**Действие**: Напишите агенту в личку

**От другого аккаунта**:
```
привет как дела?
```

**Ожидается**: AI ответ

**Проверка логов:**
```bash
fly logs --app vibee-mcp | grep "DIGITAL_TWIN"
```

Должно быть:
```
[DIGITAL_TWIN] Responding to message in chat 123456789
[DIGITAL_TWIN] Calling OpenRouter with model: x-ai/grok-4.1-fast
[DIGITAL_TWIN] Response status: 200
```

---

## 📊 Мониторинг

### Fly.io Dashboard

URL: https://fly.io/apps/vibee-mcp/monitoring

**Метрики:**
- CPU usage
- Memory usage
- Request rate
- Response time
- Error rate

### Логи в реальном времени

```bash
# All logs
fly logs --app vibee-mcp -f

# Only errors
fly logs --app vibee-mcp --level error

# Specific patterns
fly logs --app vibee-mcp | grep "TRIGGER"
fly logs --app vibee-mcp | grep "FORWARD"
fly logs --app vibee-mcp | grep "LEAD"
```

### Статус

```bash
fly status --app vibee-mcp
```

**Ожидается:**
```
Machines
ID              STATE   REGION  HEALTH  CHECKS  LAST UPDATED
xxx             started ams     passing 1 total 2m ago
```

---

## 🐛 Troubleshooting

### Проблема: Health check fails

```bash
fly ssh console --app vibee-mcp
wget -O- http://localhost:8080/health
```

### Проблема: Агент не отвечает

```bash
# Проверить логи
fly logs --app vibee-mcp | grep ERROR

# Проверить bridge
fly ssh console --app vibee-mcp
ps aux | grep telegram-bridge
```

### Проблема: Лиды не пересылаются

```bash
# Проверить forward логи
fly logs --app vibee-mcp | grep FORWARD

# Проверить chat_id
fly logs --app vibee-mcp | grep "chat -100"
```

### Проблема: Одинаковые ответы

```bash
# Проверить AI вызовы
fly logs --app vibee-mcp | grep "TRIGGER_REPLY"

# Должно быть: "Calling AI to generate variation"
# Если нет - проверить API key
fly secrets list --app vibee-mcp | grep OPENROUTER
```

---

## 📈 Success Criteria

### ✅ Deployment
- [ ] Health check возвращает 200 OK
- [ ] Логи показывают "Polling Actor started"
- [ ] Нет ошибок в логах

### ✅ AI Variety
- [ ] 5 разных ответов на один триггер
- [ ] Логи показывают "Calling AI"
- [ ] Temperature 0.9 используется

### ✅ Lead Forwarding
- [ ] Диалоги приходят в Lead группу
- [ ] Формат сообщения правильный
- [ ] Логи показывают "Message sent successfully"

### ✅ Lead Logging
- [ ] Логи показывают "НОВЫЙ ЛИД СОХРАНЁН"
- [ ] Анализ intent/priority работает
- [ ] Все поля заполнены

### ✅ Sniper Mode
- [ ] Агент отвечает только на триггеры
- [ ] Без триггера молчит
- [ ] Логи показывают "No trigger, staying silent"

---

## 🎯 Next Steps

### После успешного деплоя:

1. **Создать таблицы в PostgreSQL**
```bash
psql 'postgresql://neondb_owner:REDACTED_DB_PASSWORD@ep-bitter-frog-a1bewei7-pooler.ap-southeast-1.aws.neon.tech/neondb?sslmode=require' < schema.sql
```

2. **Настроить alerts**
```bash
fly alerts create \
  --type health_check \
  --email your@email.com \
  --app vibee-mcp
```

3. **Масштабировать при необходимости**
```bash
# Больше RAM
fly scale memory 2048 --app vibee-mcp

# Больше машин
fly scale count 2 --app vibee-mcp
```

---

## 📞 Support

**Документация:**
- [DEPLOYMENT_GUIDE.md](DEPLOYMENT_GUIDE.md)
- [LEADS_SYSTEM.md](LEADS_SYSTEM.md)
- [AI_VARIETY_FIX.md](AI_VARIETY_FIX.md)
- [LEAD_FORWARDING_FIX.md](LEAD_FORWARDING_FIX.md)

**Ссылки:**
- Fly.io: https://fly.io/docs/
- GitHub: https://github.com/gHashTag/vibee-gleam

---

**Status**: Ready for deployment and testing
**Last Updated**: 2025-12-18 05:22 UTC

🚀 **Deploy now and test!**
