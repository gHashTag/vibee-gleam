# 🚀 Deploy NOW - Step by Step

## Проблема
Gitpod не может авторизоваться в Fly.io через браузер.

## Решение: Deploy локально

### Вариант 1: Локальный деплой (Рекомендуется)

**На вашей локальной машине:**

```bash
# 1. Clone repo
git clone https://github.com/gHashTag/vibee-gleam.git
cd vibee-gleam

# 2. Install Fly CLI (если ещё нет)
curl -L https://fly.io/install.sh | sh

# 3. Login to Fly.io
fly auth login

# 4. Run deployment script
./deploy.sh
```

**Готово!** Скрипт автоматически:
- Установит все секреты
- Задеплоит приложение
- Покажет статус

---

### Вариант 2: Manual Deploy

```bash
# 1. Login
fly auth login

# 2. Set secrets
fly secrets set \
  OPENROUTER_API_KEY=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 \
  TELEGRAM_API_ID=94892 \
  TELEGRAM_API_HASH=cacf9ad137d228611b49b2ecc6d68d43 \
  TELEGRAM_SESSION_ID=sess_df0p27qhvzvv \
  TELEGRAM_SESSION_1_PHONE=+79933420465 \
  TELEGRAM_SESSION_1_USERNAME=neuro_sage \
  --app vibee-mcp

# 3. Deploy
fly deploy --config fly.toml

# 4. Check
fly status --app vibee-mcp
curl https://vibee-mcp.fly.dev/health
```

---

### Вариант 3: Использовать существующий токен

Если у вас уже есть Fly.io токен:

```bash
# В Gitpod
export FLY_API_TOKEN=your_token_here

# Deploy
cd /workspaces/vibee-gleam
./deploy.sh
```

**Где взять токен:**
```bash
# На локальной машине (где вы залогинены)
fly auth token
```

---

## Что изменилось

### ✅ Новые возможности

1. **Health Check Endpoint**
   - `GET /health` на порту 8080
   - Fly.io автоматически проверяет здоровье

2. **HTTP Retry Logic**
   - Автоматические повторы при ошибках
   - Exponential backoff (1s → 2s → 4s)
   - Max 3 попытки

3. **Graceful Shutdown**
   - Корректная остановка при SIGTERM
   - 10s timeout для cleanup

4. **Structured JSON Logging**
   - Все логи в JSON формате
   - Легко парсить и фильтровать

5. **Production Dockerfile**
   - Multi-stage build
   - Оптимизирован (~50MB)
   - Non-root user

6. **Auto-rollback**
   - Автоматический откат при ошибках

### 📝 Обновлённые файлы

- `fly.toml` - добавлен graceful shutdown, auto-rollback
- `Dockerfile.production` - новый оптимизированный Dockerfile
- `gleam/src/vibee/health.gleam` - health check endpoint
- `gleam/src/vibee/http_retry.gleam` - retry logic
- `gleam/src/vibee/shutdown.gleam` - graceful shutdown
- `gleam/src/vibee/logger.gleam` - JSON logging
- `gleam/src/vibee.gleam` - интеграция всех улучшений

---

## После деплоя

### Проверить статус

```bash
fly status --app vibee-mcp
```

### Посмотреть логи

```bash
# Live logs
fly logs --app vibee-mcp -f

# Last 100 lines
fly logs --app vibee-mcp --lines 100

# Only errors
fly logs --app vibee-mcp --level error
```

### Health check

```bash
curl https://vibee-mcp.fly.dev/health
```

**Ожидаемый ответ:**
```json
{
  "status": "ok",
  "uptime_seconds": 123,
  "bridge_connected": true,
  "version": "0.1.0",
  "timestamp": 1702857600
}
```

### Тестировать агента

**В личке:**
Напишите агенту от другого аккаунта (не @neuro_sage):
```
привет как дела?
```

**В группе (Sniper Mode):**
Напишите в группу Aimly.io dev:
```
куплю биткоин
```

Агент должен ответить!

---

## Troubleshooting

### Проблема: Deployment fails

```bash
# Check build logs
fly logs --app vibee-mcp

# Try local build
docker build -f Dockerfile.production .
```

### Проблема: Health check fails

```bash
# SSH into container
fly ssh console --app vibee-mcp

# Check if health endpoint works
wget -O- http://localhost:8080/health

# Check processes
ps aux | grep erl
```

### Проблема: Agent not responding

```bash
# Check logs for errors
fly logs --app vibee-mcp | grep ERROR

# Check if bridge is running
fly ssh console --app vibee-mcp
ps aux | grep telegram-bridge
```

---

## Масштабирование

### Увеличить RAM

```bash
fly scale memory 2048 --app vibee-mcp
```

### Добавить машины

```bash
fly scale count 2 --app vibee-mcp
```

### Auto-scaling

```bash
fly autoscale set min=1 max=5 --app vibee-mcp
```

---

## Стоимость

**Текущая конфигурация:**
- 1 машина, 1GB RAM
- ~$10/month

**Рекомендуемая:**
- 2 машины, 2GB RAM
- ~$50/month (High Availability)

---

## Контакты

**Если нужна помощь:**
- GitHub Issues: https://github.com/gHashTag/vibee-gleam/issues
- Fly.io Docs: https://fly.io/docs/

---

**Status**: Ready to deploy
**Last Updated**: 2025-12-18 05:05 UTC
