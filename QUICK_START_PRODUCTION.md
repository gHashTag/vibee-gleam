# Quick Start: Production Deployment

## Минимальные изменения для надёжности (1-2 дня)

### 1. Добавить Supervisor для автоперезапуска

**Текущая проблема**: Агент крашится и не перезапускается автоматически

**Решение**: Уже есть! OTP Supervisor работает, но нужно улучшить:

```gleam
// src/vibee/supervisor.gleam
pub fn start() {
  let children = [
    // Telegram polling actor с автоперезапуском
    supervisor.worker(fn(_) {
      polling_actor.start(config)
    }),
    
    // Event bus
    supervisor.worker(fn(_) {
      event_bus.start()
    }),
    
    // Health check HTTP server
    supervisor.worker(fn(_) {
      health_server.start(8080)
    }),
  ]
  
  supervisor.start(
    supervisor.Spec(
      argument: Nil,
      max_restarts: 10,
      max_seconds: 60,
      init: fn(_) {
        supervisor.Ready(children, supervisor.OneForOne)
      },
    ),
  )
}
```

### 2. Добавить Health Check endpoint

**Зачем**: Fly.io и load balancers должны знать, жив ли сервис

```gleam
// src/vibee/health.gleam
import gleam/http/response
import gleam/json
import mist

pub fn start(port: Int) {
  let handler = fn(req) {
    case req.path {
      "/health" -> {
        let status = check_health()
        let body = json.object([
          #("status", json.string(status.status)),
          #("telegram_bridge", json.bool(status.bridge_ok)),
          #("database", json.bool(status.db_ok)),
          #("uptime_seconds", json.int(status.uptime)),
        ])
        response.new(200)
        |> response.set_body(json.to_string(body))
      }
      _ -> response.new(404)
    }
  }
  
  mist.new(handler)
  |> mist.port(port)
  |> mist.start_http
}

fn check_health() -> HealthStatus {
  HealthStatus(
    status: "ok",
    bridge_ok: ping_bridge(),
    db_ok: ping_database(),
    uptime: get_uptime(),
  )
}
```

### 3. Добавить Retry логику для HTTP запросов

**Текущая проблема**: Один failed request = crash

```gleam
// src/vibee/http_client.gleam
pub fn send_with_retry(
  req: Request,
  max_attempts: Int,
) -> Result(Response, HttpError) {
  send_with_retry_internal(req, max_attempts, 1000)
}

fn send_with_retry_internal(
  req: Request,
  attempts_left: Int,
  delay_ms: Int,
) -> Result(Response, HttpError) {
  case httpc.send(req) {
    Ok(response) -> Ok(response)
    Error(err) -> {
      case attempts_left > 1 {
        True -> {
          io.println("[RETRY] Attempt failed, retrying in " <> int.to_string(delay_ms) <> "ms")
          process.sleep(delay_ms)
          send_with_retry_internal(req, attempts_left - 1, delay_ms * 2)
        }
        False -> Error(err)
      }
    }
  }
}
```

**Использование**:
```gleam
// Вместо:
httpc.send(req)

// Используем:
http_client.send_with_retry(req, 3)
```

### 4. Добавить Graceful Shutdown

**Зачем**: Не терять сообщения при рестарте

```gleam
// src/vibee/shutdown.gleam
pub fn register_shutdown_handler() {
  // Catch SIGTERM from Fly.io
  process.trap_exit(True)
  
  process.spawn(fn() {
    process.receive_forever(fn(msg) {
      case msg {
        Exit(reason) -> {
          io.println("[SHUTDOWN] Received shutdown signal")
          graceful_shutdown()
          process.exit(reason)
        }
        _ -> Nil
      }
    })
  })
}

fn graceful_shutdown() {
  io.println("[SHUTDOWN] Stopping polling...")
  polling_actor.stop()
  
  io.println("[SHUTDOWN] Flushing message queue...")
  flush_queue()
  
  io.println("[SHUTDOWN] Closing database connections...")
  postgres.close_all()
  
  io.println("[SHUTDOWN] Shutdown complete")
}
```

### 5. Переменные окружения через Fly.io Secrets

**Текущая проблема**: Секреты в .env файлах

**Решение**:
```bash
# Set secrets (encrypted)
fly secrets set \
  OPENROUTER_API_KEY=sk-or-v1-xxx \
  TELEGRAM_API_ID=94892 \
  TELEGRAM_API_HASH=xxx \
  TELEGRAM_SESSION_ID=sess_xxx \
  DATABASE_URL=postgres://...

# Verify
fly secrets list
```

**В коде** (уже работает):
```gleam
// Gleam автоматически читает из env
let api_key = config.get_env("OPENROUTER_API_KEY")
```

### 6. Логирование в JSON формате

**Зачем**: Fly.io может парсить и индексировать

```gleam
// src/vibee/logger.gleam
pub fn log_json(level: String, message: String, context: Dict(String, String)) {
  let log = json.object([
    #("timestamp", json.string(timestamp())),
    #("level", json.string(level)),
    #("message", json.string(message)),
    #("context", json.object(dict.to_list(context))),
    #("service", json.string("vibee-agent")),
  ])
  io.println(json.to_string(log))
}

// Usage
logger.log_json("info", "Message processed", dict.from_list([
  #("chat_id", "123"),
  #("user_id", "456"),
]))
```

---

## Deployment на Fly.io (30 минут)

### Шаг 1: Установить Fly CLI

```bash
curl -L https://fly.io/install.sh | sh
fly auth login
```

### Шаг 2: Создать приложение

```bash
cd /workspaces/vibee-gleam
fly launch --name vibee-production --region ams --no-deploy
```

### Шаг 3: Настроить fly.toml

```toml
app = "vibee-production"
primary_region = "ams"

[build]
  dockerfile = "Dockerfile"

[env]
  VIBEE_MODE = "production"

[[services]]
  internal_port = 8080
  protocol = "tcp"
  auto_stop_machines = false
  auto_start_machines = true
  min_machines_running = 1

  [[services.ports]]
    port = 80
    handlers = ["http"]
    force_https = true

  [[services.ports]]
    port = 443
    handlers = ["http", "tls"]

  [[services.http_checks]]
    interval = "10s"
    timeout = "2s"
    method = "GET"
    path = "/health"
```

### Шаг 4: Создать Dockerfile

```dockerfile
FROM ghcr.io/gleam-lang/gleam:v1.0.0-erlang-alpine AS builder

WORKDIR /app
COPY gleam/ .
RUN gleam build --target erlang

FROM alpine:3.19
RUN apk add --no-cache erlang

WORKDIR /app
COPY --from=builder /app/build /app

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=3s \
  CMD wget --no-verbose --tries=1 --spider http://localhost:8080/health || exit 1

CMD ["erl", "-pa", "/app/erlang/*/ebin", "-eval", "vibee@main:main()", "-noshell"]
```

### Шаг 5: Добавить секреты

```bash
fly secrets set \
  OPENROUTER_API_KEY=sk-or-v1-xxx \
  TELEGRAM_API_ID=94892 \
  TELEGRAM_API_HASH=xxx \
  TELEGRAM_SESSION_ID=REDACTED_SESSION
```

### Шаг 6: Deploy!

```bash
fly deploy
```

### Шаг 7: Проверить

```bash
# Логи
fly logs

# Status
fly status

# Health check
curl https://vibee-production.fly.dev/health

# SSH в контейнер
fly ssh console
```

---

## Мониторинг (15 минут)

### 1. Fly.io Dashboard

Автоматически доступно:
- CPU/Memory usage
- Request rate
- Response time
- Error rate

URL: https://fly.io/apps/vibee-production/monitoring

### 2. Логи в реальном времени

```bash
# Все логи
fly logs

# Только errors
fly logs --level error

# Follow (live)
fly logs -f
```

### 3. Alerts через Fly.io

```bash
# Email alerts при downtime
fly alerts create \
  --type health_check \
  --email your@email.com
```

---

## Масштабирование (5 минут)

### Вертикальное (больше ресурсов)

```bash
# Увеличить RAM
fly scale memory 2048

# Увеличить CPU
fly scale vm shared-cpu-2x
```

### Горизонтальное (больше инстансов)

```bash
# Добавить машины
fly scale count 3

# Auto-scaling
fly autoscale set min=2 max=10
```

---

## Backup & Recovery

### 1. Database Backup

```bash
# Если используете Fly Postgres
fly postgres backup create

# Restore
fly postgres backup restore <backup-id>
```

### 2. Session Backup

```bash
# Backup sessions из telegram-bridge
fly ssh console
tar -czf sessions-backup.tar.gz /app/sessions
exit

# Download
fly ssh sftp get sessions-backup.tar.gz
```

---

## Cost Optimization

### 1. Текущие расходы

**Минимальная конфигурация**:
- 1 машина, shared-cpu-1x, 512MB RAM: **$5/month**
- 1 машина, shared-cpu-1x, 1GB RAM: **$10/month**
- 2 машины, shared-cpu-1x, 1GB RAM: **$20/month**

**Рекомендуемая конфигурация**:
- 2 машины, shared-cpu-2x, 2GB RAM: **$50/month**
- Auto-scaling до 5 машин: **$50-125/month**

### 2. Снизить расходы

```bash
# Использовать auto-stop (для dev/staging)
fly scale count 1
fly autoscale set min=0 max=1

# Использовать меньше RAM
fly scale memory 512

# Использовать shared CPU
fly scale vm shared-cpu-1x
```

### 3. Мониторинг расходов

```bash
# Текущие расходы
fly billing show

# История
fly billing history
```

---

## Troubleshooting

### Проблема: Агент не отвечает

**Проверить**:
```bash
# 1. Health check
curl https://vibee-production.fly.dev/health

# 2. Логи
fly logs | grep ERROR

# 3. SSH в контейнер
fly ssh console
ps aux | grep erl
```

### Проблема: High memory usage

**Решение**:
```bash
# Увеличить RAM
fly scale memory 2048

# Или добавить swap
fly scale vm shared-cpu-2x
```

### Проблема: Slow responses

**Проверить**:
```bash
# Metrics
fly status

# Если CPU > 80%
fly scale vm shared-cpu-2x

# Если нужно больше инстансов
fly scale count 3
```

---

## Checklist перед Production

- [ ] Health check endpoint работает
- [ ] Retry логика добавлена
- [ ] Graceful shutdown реализован
- [ ] Секреты в Fly.io Secrets (не в .env)
- [ ] JSON логирование включено
- [ ] Dockerfile оптимизирован
- [ ] fly.toml настроен
- [ ] Минимум 2 машины для HA
- [ ] Auto-scaling настроен
- [ ] Alerts настроены
- [ ] Backup стратегия определена
- [ ] Мониторинг работает
- [ ] Load testing проведён

---

## Следующие шаги

### Неделя 1: Базовая надёжность
1. Добавить health check endpoint
2. Добавить retry логику
3. Добавить graceful shutdown
4. Deploy на Fly.io
5. Настроить мониторинг

### Неделя 2: Multi-user support
1. Добавить PostgreSQL
2. Реализовать user registration
3. Multi-session support в bridge
4. Rate limiting per user

### Неделя 3: Observability
1. Prometheus metrics
2. Grafana dashboards
3. Alert rules
4. Performance tuning

---

## Полезные ссылки

- **Fly.io Docs**: https://fly.io/docs/
- **Gleam Docs**: https://gleam.run/documentation/
- **OTP Supervisor**: https://hexdocs.pm/gleam_otp/gleam/otp/supervisor.html
- **Mist HTTP**: https://hexdocs.pm/mist/

---

**Status**: Ready for production deployment
**Estimated time**: 2-3 hours for basic setup
**Last Updated**: 2025-12-18 04:53 UTC
