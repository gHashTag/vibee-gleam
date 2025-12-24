# Production-Ready Architecture для Multi-User Service

## Текущие проблемы

### 1. Надёжность (Reliability)
❌ **Проблема**: Агент крашится при ошибках HTTP
- Supervisor перезапускает, но теряются сообщения
- Нет retry логики для failed requests
- Нет graceful degradation

❌ **Проблема**: Одна сессия для всех пользователей
- Если сессия падает - весь сервис недоступен
- Нет изоляции между пользователями

❌ **Проблема**: Env vars загружаются вручную
- `export $(cat .env | xargs)` - не production подход
- Секреты в plaintext файле

### 2. Масштабируемость (Scalability)
❌ **Проблема**: Один процесс для всех чатов
- Polling блокирует обработку
- Нет параллелизма
- Bottleneck на одном CPU core

❌ **Проблема**: In-memory state
- При рестарте теряется состояние
- Нет персистентности
- Нет shared state между инстансами

### 3. Мониторинг (Observability)
❌ **Проблема**: Логи только в stdout
- Нет структурированных логов
- Нет метрик
- Нет трейсинга

❌ **Проблема**: Нет health checks
- Невозможно определить, жив ли сервис
- Нет readiness/liveness probes

### 4. Безопасность (Security)
❌ **Проблема**: Секреты в .env файлах
- API ключи в plaintext
- Нет ротации секретов
- Нет encryption at rest

❌ **Проблема**: Нет rate limiting
- Можно заспамить сервис
- Нет защиты от abuse

---

## Production Architecture

### Архитектура: Microservices + Event-Driven

```
┌─────────────────────────────────────────────────────────────┐
│                     Load Balancer (Fly.io)                  │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    API Gateway (Gleam)                       │
│  - Authentication (JWT)                                      │
│  - Rate Limiting (per user)                                  │
│  - Request validation                                        │
│  - Health checks                                             │
└─────────────────────────────────────────────────────────────┘
                              │
                ┌─────────────┼─────────────┐
                ▼             ▼             ▼
┌───────────────────┐ ┌──────────────┐ ┌──────────────┐
│ Telegram Service  │ │ AI Service   │ │ User Service │
│ (Go Bridge)       │ │ (Gleam)      │ │ (Gleam)      │
│                   │ │              │ │              │
│ - Multi-session   │ │ - OpenRouter │ │ - User CRUD  │
│ - WebSocket       │ │ - Grok 4.1   │ │ - Settings   │
│ - Message queue   │ │ - Fallbacks  │ │ - Billing    │
└───────────────────┘ └──────────────┘ └──────────────┘
        │                     │                 │
        └─────────────────────┼─────────────────┘
                              ▼
                    ┌──────────────────┐
                    │  Message Queue   │
                    │  (Redis Streams) │
                    └──────────────────┘
                              │
                ┌─────────────┼─────────────┐
                ▼             ▼             ▼
        ┌──────────┐  ┌──────────┐  ┌──────────┐
        │ Worker 1 │  │ Worker 2 │  │ Worker N │
        │ (Gleam)  │  │ (Gleam)  │  │ (Gleam)  │
        └──────────┘  └──────────┘  └──────────┘
                              │
                              ▼
                ┌──────────────────────────┐
                │   PostgreSQL (Fly.io)    │
                │   - Users                │
                │   - Messages             │
                │   - Sessions             │
                │   - Analytics            │
                └──────────────────────────┘
```

---

## Компоненты

### 1. API Gateway (Gleam)
**Задачи**:
- Аутентификация пользователей (JWT tokens)
- Rate limiting (per user, per endpoint)
- Request validation
- Health checks (liveness, readiness)
- Metrics export (Prometheus)

**Endpoints**:
```
POST   /api/v1/auth/register     - Регистрация
POST   /api/v1/auth/login        - Логин
GET    /api/v1/users/me          - Профиль
POST   /api/v1/sessions/connect  - Подключить Telegram
GET    /api/v1/chats             - Список чатов
POST   /api/v1/chats/:id/send    - Отправить сообщение
GET    /health                   - Health check
GET    /metrics                  - Prometheus metrics
```

**Rate Limits**:
- 100 requests/minute per user (API)
- 10 messages/minute per chat (Telegram)
- 1000 AI requests/day per user

### 2. Telegram Service (Go Bridge)
**Улучшения**:
- **Multi-session support**: Каждый пользователь = своя сессия
- **Session pooling**: Переиспользование соединений
- **WebSocket updates**: Real-time уведомления
- **Message queue**: Отправка через очередь (не блокирует)
- **Retry logic**: Exponential backoff для failed requests
- **Circuit breaker**: Отключение при перегрузке

**Конфигурация**:
```go
type Config struct {
    MaxSessions      int           // 1000
    SessionTimeout   time.Duration // 30m
    RetryAttempts    int           // 3
    RetryDelay       time.Duration // 1s
    CircuitThreshold int           // 5 errors
}
```

### 3. AI Service (Gleam)
**Улучшения**:
- **Multiple providers**: OpenRouter, Anthropic, OpenAI (fallback)
- **Response caching**: Redis cache для похожих запросов
- **Token counting**: Отслеживание usage per user
- **Streaming responses**: Для длинных ответов
- **Content moderation**: Фильтрация токсичного контента

**Fallback chain**:
```
1. OpenRouter (Grok 4.1) - primary
2. Anthropic (Claude 3.5) - fallback 1
3. OpenAI (GPT-4) - fallback 2
4. Predefined responses - fallback 3
```

### 4. Worker Pool (Gleam)
**Задачи**:
- Обработка сообщений из очереди
- Параллельная обработка (N workers)
- Graceful shutdown
- Dead letter queue для failed messages

**Конфигурация**:
```gleam
type WorkerConfig {
  WorkerConfig(
    worker_count: Int,        // 10
    batch_size: Int,          // 100
    timeout_ms: Int,          // 30000
    max_retries: Int,         // 3
    dead_letter_queue: String // "dlq:messages"
  )
}
```

### 5. Message Queue (Redis Streams)
**Зачем**:
- Асинхронная обработка
- Гарантия доставки (at-least-once)
- Backpressure handling
- Replay capability

**Streams**:
```
telegram:incoming   - Входящие сообщения
telegram:outgoing   - Исходящие сообщения
ai:requests         - AI запросы
ai:responses        - AI ответы
analytics:events    - События для аналитики
```

### 6. PostgreSQL Database
**Схема**:
```sql
-- Пользователи
CREATE TABLE users (
    id BIGSERIAL PRIMARY KEY,
    telegram_id BIGINT UNIQUE,
    username TEXT,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    subscription_tier TEXT DEFAULT 'free',
    api_quota_remaining INT DEFAULT 1000
);

-- Сессии Telegram
CREATE TABLE telegram_sessions (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT REFERENCES users(id),
    session_id TEXT UNIQUE,
    phone TEXT,
    authorized BOOLEAN DEFAULT FALSE,
    last_active TIMESTAMPTZ,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Сообщения (для истории и RAG)
CREATE TABLE messages (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT REFERENCES users(id),
    chat_id BIGINT,
    message_id BIGINT,
    sender_id BIGINT,
    text TEXT,
    direction TEXT, -- 'in' or 'out'
    created_at TIMESTAMPTZ DEFAULT NOW(),
    embedding vector(1536) -- для RAG
);

-- Конфигурация чатов
CREATE TABLE chat_configs (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT REFERENCES users(id),
    chat_id BIGINT,
    mode TEXT, -- 'digital_twin', 'sniper', 'disabled'
    trigger_words TEXT[],
    response_probability FLOAT,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Аналитика
CREATE TABLE analytics_events (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT REFERENCES users(id),
    event_type TEXT,
    event_data JSONB,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Индексы
CREATE INDEX idx_messages_user_chat ON messages(user_id, chat_id, created_at DESC);
CREATE INDEX idx_messages_embedding ON messages USING ivfflat (embedding vector_cosine_ops);
CREATE INDEX idx_sessions_user ON telegram_sessions(user_id);
CREATE INDEX idx_analytics_user_type ON analytics_events(user_id, event_type, created_at DESC);
```

---

## Deployment Strategy

### 1. Fly.io Configuration

**fly.toml**:
```toml
app = "vibee-production"
primary_region = "ams"

[build]
  dockerfile = "Dockerfile"

[env]
  VIBEE_MODE = "production"
  LOG_LEVEL = "info"

[[services]]
  internal_port = 8080
  protocol = "tcp"
  auto_stop_machines = false
  auto_start_machines = true
  min_machines_running = 2
  max_machines_running = 10

  [[services.ports]]
    port = 80
    handlers = ["http"]
    force_https = true

  [[services.ports]]
    port = 443
    handlers = ["http", "tls"]

  [services.concurrency]
    type = "requests"
    hard_limit = 250
    soft_limit = 200

  [[services.http_checks]]
    interval = "10s"
    timeout = "2s"
    grace_period = "5s"
    method = "GET"
    path = "/health"

[[vm]]
  cpu_kind = "shared"
  cpus = 2
  memory_mb = 2048
```

### 2. Docker Multi-Stage Build

**Dockerfile**:
```dockerfile
# Stage 1: Build Gleam app
FROM ghcr.io/gleam-lang/gleam:v1.0.0-erlang-alpine AS gleam-builder
WORKDIR /app
COPY gleam/ .
RUN gleam build --target erlang

# Stage 2: Build Go bridge
FROM golang:1.21-alpine AS go-builder
WORKDIR /app
COPY telegram-bridge/ .
RUN go build -o telegram-bridge cmd/server/main.go

# Stage 3: Runtime
FROM alpine:3.19
RUN apk add --no-cache erlang postgresql-client redis

WORKDIR /app

# Copy binaries
COPY --from=gleam-builder /app/build /app/gleam
COPY --from=go-builder /app/telegram-bridge /app/telegram-bridge

# Copy configs
COPY supervisord.conf /etc/supervisord.conf

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD wget --no-verbose --tries=1 --spider http://localhost:8080/health || exit 1

EXPOSE 8080 8081

CMD ["supervisord", "-c", "/etc/supervisord.conf"]
```

### 3. Secrets Management

**Fly.io Secrets**:
```bash
# Set secrets (encrypted at rest)
fly secrets set \
  OPENROUTER_API_KEY=sk-or-v1-xxx \
  TELEGRAM_API_ID=94892 \
  TELEGRAM_API_HASH=xxx \
  DATABASE_URL=postgres://... \
  REDIS_URL=redis://... \
  JWT_SECRET=xxx

# Rotate secrets (zero downtime)
fly secrets set OPENROUTER_API_KEY=sk-or-v1-new
```

### 4. Database Migrations

**Flyway или custom Gleam migrations**:
```gleam
// migrations/001_initial_schema.gleam
pub fn up(db: Database) -> Result(Nil, String) {
  postgres.execute(db, "
    CREATE TABLE users (
      id BIGSERIAL PRIMARY KEY,
      telegram_id BIGINT UNIQUE,
      ...
    )
  ")
}

pub fn down(db: Database) -> Result(Nil, String) {
  postgres.execute(db, "DROP TABLE users")
}
```

---

## Monitoring & Observability

### 1. Metrics (Prometheus)

**Gleam metrics exporter**:
```gleam
// src/vibee/metrics.gleam
pub type Metrics {
  Metrics(
    http_requests_total: Counter,
    http_request_duration: Histogram,
    telegram_messages_total: Counter,
    ai_requests_total: Counter,
    ai_request_duration: Histogram,
    active_sessions: Gauge,
    queue_size: Gauge,
  )
}

pub fn export_metrics() -> String {
  "# HELP http_requests_total Total HTTP requests
# TYPE http_requests_total counter
http_requests_total{method=\"GET\",path=\"/api/v1/chats\"} 1234
...
"
}
```

**Grafana Dashboard**:
- Request rate (req/s)
- Response time (p50, p95, p99)
- Error rate (%)
- Active users
- Queue depth
- AI token usage

### 2. Logging (Structured)

**JSON logs**:
```gleam
pub fn log_info(msg: String, context: Dict(String, String)) {
  let log = json.object([
    #("timestamp", json.string(timestamp())),
    #("level", json.string("info")),
    #("message", json.string(msg)),
    #("context", json.object(dict.to_list(context))),
    #("trace_id", json.string(get_trace_id())),
  ])
  io.println(json.to_string(log))
}
```

**Log aggregation**: Fly.io → Loki → Grafana

### 3. Tracing (OpenTelemetry)

**Distributed tracing**:
```gleam
pub fn with_span(name: String, fn: fn() -> a) -> a {
  let span = otel.start_span(name)
  let result = fn()
  otel.end_span(span)
  result
}

// Usage
with_span("process_message", fn() {
  with_span("fetch_context", fn() { get_context() })
  with_span("generate_reply", fn() { call_ai() })
  with_span("send_message", fn() { send_to_telegram() })
})
```

### 4. Alerting (PagerDuty/Slack)

**Alert rules**:
```yaml
groups:
  - name: vibee_alerts
    rules:
      - alert: HighErrorRate
        expr: rate(http_requests_total{status=~"5.."}[5m]) > 0.05
        for: 5m
        annotations:
          summary: "High error rate detected"
          
      - alert: HighLatency
        expr: histogram_quantile(0.95, http_request_duration_seconds) > 2
        for: 5m
        
      - alert: QueueBacklog
        expr: queue_size > 10000
        for: 10m
```

---

## Error Handling & Resilience

### 1. Retry Logic

```gleam
pub fn retry_with_backoff(
  fn: fn() -> Result(a, e),
  max_attempts: Int,
  initial_delay_ms: Int,
) -> Result(a, e) {
  case fn() {
    Ok(result) -> Ok(result)
    Error(err) -> {
      case max_attempts > 1 {
        True -> {
          process.sleep(initial_delay_ms)
          retry_with_backoff(fn, max_attempts - 1, initial_delay_ms * 2)
        }
        False -> Error(err)
      }
    }
  }
}
```

### 2. Circuit Breaker

```gleam
pub type CircuitState {
  Closed
  Open(opened_at: Int)
  HalfOpen
}

pub fn call_with_circuit_breaker(
  circuit: Circuit,
  fn: fn() -> Result(a, e),
) -> Result(a, CircuitBreakerError) {
  case circuit.state {
    Open(opened_at) -> {
      case now() - opened_at > circuit.timeout_ms {
        True -> {
          // Try half-open
          case fn() {
            Ok(result) -> {
              circuit_close(circuit)
              Ok(result)
            }
            Error(_) -> {
              circuit_open(circuit)
              Error(CircuitOpen)
            }
          }
        }
        False -> Error(CircuitOpen)
      }
    }
    _ -> {
      case fn() {
        Ok(result) -> {
          circuit_success(circuit)
          Ok(result)
        }
        Error(err) -> {
          circuit_failure(circuit)
          Error(ServiceError(err))
        }
      }
    }
  }
}
```

### 3. Graceful Degradation

```gleam
pub fn generate_reply(text: String) -> String {
  // Try primary AI
  case call_openrouter(text) {
    Ok(reply) -> reply
    Error(_) -> {
      // Try fallback AI
      case call_anthropic(text) {
        Ok(reply) -> reply
        Error(_) -> {
          // Use predefined response
          get_fallback_response(text)
        }
      }
    }
  }
}
```

---

## Security

### 1. Authentication (JWT)

```gleam
pub fn generate_jwt(user_id: Int) -> String {
  let payload = json.object([
    #("user_id", json.int(user_id)),
    #("exp", json.int(now() + 86400)), // 24h
    #("iat", json.int(now())),
  ])
  jwt.sign(payload, get_secret())
}

pub fn verify_jwt(token: String) -> Result(Int, JwtError) {
  case jwt.verify(token, get_secret()) {
    Ok(payload) -> {
      case json.get_int(payload, "user_id") {
        Ok(user_id) -> Ok(user_id)
        Error(_) -> Error(InvalidPayload)
      }
    }
    Error(_) -> Error(InvalidToken)
  }
}
```

### 2. Rate Limiting (Token Bucket)

```gleam
pub type RateLimiter {
  RateLimiter(
    capacity: Int,
    refill_rate: Int, // tokens per second
    tokens: Int,
    last_refill: Int,
  )
}

pub fn check_rate_limit(limiter: RateLimiter) -> Result(RateLimiter, RateLimitError) {
  let now = now()
  let elapsed = now - limiter.last_refill
  let new_tokens = min(
    limiter.capacity,
    limiter.tokens + elapsed * limiter.refill_rate
  )
  
  case new_tokens >= 1 {
    True -> Ok(RateLimiter(..limiter, tokens: new_tokens - 1, last_refill: now))
    False -> Error(RateLimitExceeded)
  }
}
```

### 3. Input Validation

```gleam
pub fn validate_message(text: String) -> Result(String, ValidationError) {
  case string.length(text) {
    0 -> Error(EmptyMessage)
    len if len > 4096 -> Error(MessageTooLong)
    _ -> {
      case contains_malicious_content(text) {
        True -> Error(MaliciousContent)
        False -> Ok(text)
      }
    }
  }
}
```

---

## Cost Optimization

### 1. Response Caching

```gleam
// Cache similar questions
pub fn get_cached_response(text: String) -> Option(String) {
  let key = "cache:response:" <> hash(normalize(text))
  redis.get(key)
}

pub fn cache_response(text: String, response: String) {
  let key = "cache:response:" <> hash(normalize(text))
  redis.setex(key, 3600, response) // 1 hour TTL
}
```

### 2. Token Counting

```gleam
pub fn count_tokens(text: String) -> Int {
  // Approximate: 1 token ≈ 4 chars
  string.length(text) / 4
}

pub fn check_quota(user_id: Int, tokens: Int) -> Result(Nil, QuotaError) {
  case get_user_quota(user_id) {
    quota if quota >= tokens -> {
      decrease_quota(user_id, tokens)
      Ok(Nil)
    }
    _ -> Error(QuotaExceeded)
  }
}
```

### 3. Auto-scaling

**Fly.io autoscaling**:
```toml
[services.concurrency]
  type = "requests"
  hard_limit = 250
  soft_limit = 200

[services.autoscaling]
  min_machines = 2
  max_machines = 10
  
  [[services.autoscaling.rules]]
    metric = "cpu"
    target = 70
    
  [[services.autoscaling.rules]]
    metric = "memory"
    target = 80
```

---

## Implementation Roadmap

### Phase 1: Foundation (Week 1-2)
- [ ] Migrate to PostgreSQL (from in-memory)
- [ ] Add Redis for caching/queues
- [ ] Implement structured logging
- [ ] Add health checks
- [ ] Setup Fly.io deployment

### Phase 2: Reliability (Week 3-4)
- [ ] Implement retry logic
- [ ] Add circuit breakers
- [ ] Setup message queue (Redis Streams)
- [ ] Add graceful shutdown
- [ ] Implement dead letter queue

### Phase 3: Multi-User (Week 5-6)
- [ ] User registration/authentication
- [ ] Multi-session support in bridge
- [ ] Per-user rate limiting
- [ ] User settings/preferences
- [ ] Billing integration

### Phase 4: Observability (Week 7-8)
- [ ] Prometheus metrics
- [ ] Grafana dashboards
- [ ] OpenTelemetry tracing
- [ ] Alert rules
- [ ] Log aggregation

### Phase 5: Optimization (Week 9-10)
- [ ] Response caching
- [ ] Token counting/quotas
- [ ] Auto-scaling
- [ ] Performance tuning
- [ ] Load testing

---

## Estimated Costs (Monthly)

### Infrastructure
- **Fly.io**: $50-200 (2-10 machines, 2GB RAM each)
- **PostgreSQL**: $30 (Fly.io Postgres, 10GB)
- **Redis**: $20 (Fly.io Redis, 1GB)
- **Total**: ~$100-250/month

### APIs
- **OpenRouter**: $0.10-0.50 per 1M tokens
- **Anthropic**: $3-15 per 1M tokens (fallback)
- **Estimated**: $50-500/month (depends on usage)

### Monitoring
- **Grafana Cloud**: Free tier (10k series)
- **Sentry**: Free tier (5k events/month)

**Total**: $150-750/month for 1000-10000 users

---

## Next Steps

1. **Immediate** (сейчас):
   - Добавить OPENROUTER_API_KEY
   - Протестировать текущую версию
   
2. **Short-term** (эта неделя):
   - Migrate to PostgreSQL
   - Add Redis
   - Setup Fly.io deployment
   
3. **Medium-term** (следующий месяц):
   - Implement multi-user support
   - Add monitoring
   - Load testing

4. **Long-term** (3 месяца):
   - Full production deployment
   - Marketing/user acquisition
   - Feature expansion

---

**Status**: Architecture designed, ready for implementation
**Last Updated**: 2025-12-18 04:52 UTC
