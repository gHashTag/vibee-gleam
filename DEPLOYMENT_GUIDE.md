# Deployment Guide - Production Ready

## ✅ Что сделано

### 1. Health Check Endpoint
- **Файл**: `gleam/src/vibee/health.gleam`
- **Endpoint**: `GET /health`
- **Порт**: 8080
- **Ответ**:
```json
{
  "status": "ok",
  "uptime_seconds": 123,
  "bridge_connected": true,
  "version": "0.1.0",
  "timestamp": 1702857600
}
```

### 2. HTTP Retry Logic
- **Файл**: `gleam/src/vibee/http_retry.gleam`
- **Конфигурация**:
  - Max attempts: 3
  - Initial delay: 1000ms
  - Backoff multiplier: 2.0
  - Max delay: 10000ms
- **Применено в**:
  - `polling_actor.gleam` - get_dialogs, get_history

### 3. Graceful Shutdown
- **Файл**: `gleam/src/vibee/shutdown.gleam`
- **Функция**: Trap SIGTERM от Fly.io
- **Timeout**: 10s (настроено в fly.toml)

### 4. Structured JSON Logging
- **Файл**: `gleam/src/vibee/logger.gleam`
- **Формат**:
```json
{
  "timestamp": "1702857600",
  "level": "info",
  "message": "Processing message",
  "context": {
    "chat_id": "123",
    "user_id": "456"
  },
  "service": "vibee-agent"
}
```

### 5. Production Dockerfile
- **Файл**: `Dockerfile.production`
- **Multi-stage build**:
  - Stage 1: Build Gleam app
  - Stage 2: Build Go bridge
  - Stage 3: Runtime (Alpine)
- **Размер**: ~50MB (оптимизирован)
- **Security**: Non-root user

### 6. Fly.io Configuration
- **Файл**: `fly.production.toml`
- **Region**: Amsterdam (ams)
- **VM**: 1 CPU, 1GB RAM
- **Health checks**: HTTP + TCP
- **Auto-rollback**: Enabled

---

## 🚀 Deployment Steps

### Шаг 1: Установить Fly CLI

```bash
curl -L https://fly.io/install.sh | sh
fly auth login
```

### Шаг 2: Создать приложение

```bash
cd /workspaces/vibee-gleam

# Create app (don't deploy yet)
fly launch \
  --name vibee-production \
  --region ams \
  --config fly.production.toml \
  --no-deploy
```

### Шаг 3: Добавить секреты

```bash
# Set all secrets at once
fly secrets set \
  OPENROUTER_API_KEY=sk-or-v1-YOUR_KEY \
  TELEGRAM_API_ID=94892 \
  TELEGRAM_API_HASH=cacf9ad137d228611b49b2ecc6d68d43 \
  TELEGRAM_SESSION_ID=sess_df0p27qhvzvv \
  TELEGRAM_SESSION_1_PHONE=+79933420465 \
  TELEGRAM_SESSION_1_USERNAME=neuro_sage

# Verify secrets
fly secrets list
```

### Шаг 4: Deploy!

```bash
# Deploy using production config
fly deploy --config fly.production.toml

# Watch logs
fly logs -f
```

### Шаг 5: Проверить

```bash
# Check status
fly status

# Check health
curl https://vibee-production.fly.dev/health

# SSH into container
fly ssh console
```

---

## 📊 Мониторинг

### Fly.io Dashboard
URL: https://fly.io/apps/vibee-production

**Доступно**:
- CPU/Memory usage
- Request rate
- Response time
- Error rate
- Logs

### Логи в реальном времени

```bash
# All logs
fly logs

# Only errors
fly logs --level error

# Follow (live)
fly logs -f

# Last 100 lines
fly logs --lines 100
```

### Metrics

```bash
# VM metrics
fly status

# Scale info
fly scale show

# App info
fly info
```

---

## 🔧 Управление

### Масштабирование

```bash
# Vertical scaling (more RAM)
fly scale memory 2048

# Vertical scaling (more CPU)
fly scale vm shared-cpu-2x

# Horizontal scaling (more machines)
fly scale count 2

# Auto-scaling
fly autoscale set min=1 max=5
```

### Рестарт

```bash
# Restart all machines
fly apps restart vibee-production

# Restart specific machine
fly machine restart <machine-id>
```

### Откат

```bash
# List releases
fly releases

# Rollback to previous
fly releases rollback

# Rollback to specific version
fly releases rollback --version 5
```

---

## 🐛 Troubleshooting

### Проблема: Health check fails

**Проверить**:
```bash
# SSH into container
fly ssh console

# Check if health endpoint responds
wget -O- http://localhost:8080/health

# Check processes
ps aux | grep erl
```

**Решение**: Убедитесь, что health server запускается до polling actor

### Проблема: High memory usage

**Проверить**:
```bash
fly status
# Look at memory usage
```

**Решение**:
```bash
# Increase RAM
fly scale memory 2048

# Or add more machines
fly scale count 2
```

### Проблема: Slow responses

**Проверить**:
```bash
# Check CPU usage
fly status

# Check logs for slow queries
fly logs | grep "SLOW"
```

**Решение**:
```bash
# Upgrade CPU
fly scale vm shared-cpu-2x

# Or add more machines
fly scale count 3
```

### Проблема: Deployment fails

**Проверить**:
```bash
# Check build logs
fly logs --app vibee-production

# Validate Dockerfile
docker build -f Dockerfile.production .
```

**Решение**: Проверьте, что все зависимости установлены

---

## 💰 Стоимость

### Текущая конфигурация
- **VM**: shared-cpu-1x, 1GB RAM
- **Machines**: 1
- **Стоимость**: ~$10/month

### Рекомендуемая для production
- **VM**: shared-cpu-2x, 2GB RAM
- **Machines**: 2 (для HA)
- **Стоимость**: ~$50/month

### С auto-scaling
- **Min machines**: 2
- **Max machines**: 5
- **Стоимость**: $50-125/month (зависит от нагрузки)

---

## 📋 Checklist перед Production

- [x] Health check endpoint работает
- [x] HTTP retry логика добавлена
- [x] Graceful shutdown реализован
- [x] JSON логирование включено
- [x] Dockerfile оптимизирован
- [x] fly.toml настроен
- [ ] Секреты добавлены в Fly.io
- [ ] Deployed и протестирован
- [ ] Мониторинг настроен
- [ ] Alerts настроены (опционально)
- [ ] Backup стратегия определена

---

## 🎯 Следующие шаги

### Immediate (сегодня)
1. Добавить OPENROUTER_API_KEY в секреты
2. Deploy на Fly.io
3. Протестировать health check
4. Проверить логи

### Short-term (эта неделя)
1. Настроить alerts (email при downtime)
2. Добавить PostgreSQL для persistence
3. Настроить auto-scaling
4. Load testing

### Medium-term (следующий месяц)
1. Multi-user support
2. Rate limiting per user
3. Advanced metrics (Prometheus)
4. Grafana dashboards

---

## 📞 Support

**Fly.io Docs**: https://fly.io/docs/
**Gleam Docs**: https://gleam.run/documentation/
**Issues**: https://github.com/gHashTag/vibee-gleam/issues

---

**Status**: Ready for production deployment
**Last Updated**: 2025-12-18 05:00 UTC
**Version**: 0.1.0
