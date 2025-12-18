# Implementation Roadmap: Production-Ready Multi-User Service

## Приоритеты

### 🔴 P0 - Critical (Must have для production)
- Health checks
- Retry logic
- Graceful shutdown
- Secrets management
- Basic monitoring

### 🟡 P1 - High (Нужно для масштабирования)
- PostgreSQL migration
- Multi-user support
- Rate limiting
- Message queue

### 🟢 P2 - Medium (Nice to have)
- Advanced metrics
- Distributed tracing
- Auto-scaling
- Response caching

### ⚪ P3 - Low (Future)
- Multi-region deployment
- Advanced analytics
- ML-based features

---

## Phase 1: Immediate Fixes (1-2 дня) 🔴

### Цель: Сделать текущую версию стабильной

#### Task 1.1: Health Check Endpoint
**Время**: 2 часа
**Приоритет**: P0

```gleam
// src/vibee/health.gleam
pub fn start_health_server(port: Int) {
  mist.new(health_handler)
  |> mist.port(port)
  |> mist.start_http
}

fn health_handler(req: Request) -> Response {
  case req.path {
    "/health" -> {
      json.object([
        #("status", json.string("ok")),
        #("uptime", json.int(get_uptime())),
        #("bridge_connected", json.bool(check_bridge())),
      ])
      |> json.to_string
      |> response.new(200)
      |> response.set_body
    }
    _ -> response.new(404)
  }
}
```

**Тест**:
```bash
curl http://localhost:8080/health
# {"status":"ok","uptime":123,"bridge_connected":true}
```

#### Task 1.2: HTTP Retry Logic
**Время**: 3 часа
**Приоритет**: P0

```gleam
// src/vibee/http_retry.gleam
pub fn send_with_retry(
  req: Request,
  max_attempts: Int,
) -> Result(Response, HttpError) {
  do_retry(req, max_attempts, 1000)
}

fn do_retry(
  req: Request,
  attempts_left: Int,
  delay_ms: Int,
) -> Result(Response, HttpError) {
  case httpc.send(req) {
    Ok(resp) -> Ok(resp)
    Error(err) -> {
      case attempts_left > 1 {
        True -> {
          process.sleep(delay_ms)
          do_retry(req, attempts_left - 1, delay_ms * 2)
        }
        False -> Error(err)
      }
    }
  }
}
```

**Применить везде**:
- `polling_actor.gleam` - get_dialogs, get_history
- `telegram_agent.gleam` - send_message
- `dialog_forwarder.gleam` - forward_dialog

#### Task 1.3: Graceful Shutdown
**Время**: 2 часа
**Приоритет**: P0

```gleam
// src/vibee/shutdown.gleam
pub fn setup_shutdown_handler(polling_actor: Subject) {
  process.trap_exit(True)
  
  process.spawn(fn() {
    receive_shutdown_signal(polling_actor)
  })
}

fn receive_shutdown_signal(polling_actor: Subject) {
  case process.receive(5000) {
    Ok(Exit(_)) -> {
      io.println("[SHUTDOWN] Graceful shutdown initiated")
      polling_actor.send(polling_actor, Stop)
      process.sleep(2000) // Wait for cleanup
      io.println("[SHUTDOWN] Complete")
    }
    _ -> receive_shutdown_signal(polling_actor)
  }
}
```

#### Task 1.4: JSON Logging
**Время**: 2 часа
**Приоритет**: P0

```gleam
// src/vibee/logger.gleam
pub type LogLevel {
  Debug
  Info
  Warn
  Error
}

pub fn log(level: LogLevel, message: String, context: Dict(String, String)) {
  let log_obj = json.object([
    #("timestamp", json.string(iso8601_now())),
    #("level", json.string(level_to_string(level))),
    #("message", json.string(message)),
    #("context", dict_to_json(context)),
    #("service", json.string("vibee-agent")),
  ])
  
  io.println(json.to_string(log_obj))
}
```

**Заменить все `io.println`** на `logger.log`:
```gleam
// Было:
io.println("[MSG] Processing message")

// Стало:
logger.log(Info, "Processing message", dict.from_list([
  #("chat_id", chat_id),
  #("from_id", int.to_string(from_id)),
]))
```

#### Task 1.5: Fly.io Deployment
**Время**: 3 часа
**Приоритет**: P0

**Создать файлы**:

1. `Dockerfile`:
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
HEALTHCHECK CMD wget -q -O- http://localhost:8080/health || exit 1
CMD ["erl", "-pa", "/app/erlang/*/ebin", "-eval", "vibee@main:main()", "-noshell"]
```

2. `fly.toml`:
```toml
app = "vibee-production"
primary_region = "ams"

[build]
  dockerfile = "Dockerfile"

[[services]]
  internal_port = 8080
  protocol = "tcp"
  
  [[services.http_checks]]
    interval = "10s"
    timeout = "2s"
    method = "GET"
    path = "/health"
```

**Deploy**:
```bash
fly launch --no-deploy
fly secrets set OPENROUTER_API_KEY=xxx TELEGRAM_API_ID=xxx ...
fly deploy
```

**Checklist Phase 1**:
- [ ] Health check работает
- [ ] Retry логика добавлена
- [ ] Graceful shutdown работает
- [ ] JSON логи включены
- [ ] Deployed на Fly.io
- [ ] Секреты в Fly Secrets

---

## Phase 2: Database & Persistence (3-5 дней) 🟡

### Цель: Переход от in-memory к PostgreSQL

#### Task 2.1: PostgreSQL Setup
**Время**: 4 часа
**Приоритет**: P1

```bash
# Create Fly Postgres
fly postgres create --name vibee-db --region ams

# Attach to app
fly postgres attach vibee-db --app vibee-production
```

**Schema**:
```sql
-- migrations/001_initial.sql
CREATE TABLE users (
    id BIGSERIAL PRIMARY KEY,
    telegram_id BIGINT UNIQUE NOT NULL,
    username TEXT,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    subscription_tier TEXT DEFAULT 'free',
    api_quota_remaining INT DEFAULT 1000
);

CREATE TABLE telegram_sessions (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT REFERENCES users(id),
    session_id TEXT UNIQUE NOT NULL,
    phone TEXT,
    authorized BOOLEAN DEFAULT FALSE,
    last_active TIMESTAMPTZ,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE messages (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT REFERENCES users(id),
    chat_id BIGINT NOT NULL,
    message_id BIGINT NOT NULL,
    sender_id BIGINT,
    sender_name TEXT,
    text TEXT,
    direction TEXT CHECK (direction IN ('in', 'out')),
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE chat_configs (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT REFERENCES users(id),
    chat_id BIGINT NOT NULL,
    mode TEXT CHECK (mode IN ('digital_twin', 'sniper', 'disabled')),
    trigger_words TEXT[],
    response_probability FLOAT DEFAULT 0.0,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    UNIQUE(user_id, chat_id)
);

CREATE INDEX idx_messages_user_chat ON messages(user_id, chat_id, created_at DESC);
CREATE INDEX idx_sessions_user ON telegram_sessions(user_id);
```

#### Task 2.2: Migration System
**Время**: 4 часа
**Приоритет**: P1

```gleam
// src/vibee/db/migrations.gleam
pub type Migration {
  Migration(
    version: Int,
    name: String,
    up: fn(Database) -> Result(Nil, String),
    down: fn(Database) -> Result(Nil, String),
  )
}

pub fn run_migrations(db: Database) -> Result(Nil, String) {
  // Create migrations table
  postgres.execute(db, "
    CREATE TABLE IF NOT EXISTS schema_migrations (
      version INT PRIMARY KEY,
      name TEXT NOT NULL,
      applied_at TIMESTAMPTZ DEFAULT NOW()
    )
  ")
  
  // Get applied migrations
  let applied = get_applied_migrations(db)
  
  // Run pending migrations
  list.each(all_migrations(), fn(migration) {
    case list.contains(applied, migration.version) {
      True -> Nil
      False -> {
        io.println("Running migration: " <> migration.name)
        case migration.up(db) {
          Ok(_) -> record_migration(db, migration)
          Error(e) -> io.println("Migration failed: " <> e)
        }
      }
    }
  })
}
```

#### Task 2.3: Repository Pattern
**Время**: 6 часов
**Приоритет**: P1

```gleam
// src/vibee/db/user_repository.gleam
pub fn create_user(db: Database, telegram_id: Int, username: String) -> Result(User, DbError) {
  postgres.query(
    db,
    "INSERT INTO users (telegram_id, username) VALUES ($1, $2) RETURNING *",
    [postgres.int(telegram_id), postgres.text(username)],
  )
  |> result.map(parse_user)
}

pub fn get_user_by_telegram_id(db: Database, telegram_id: Int) -> Result(User, DbError) {
  postgres.query(
    db,
    "SELECT * FROM users WHERE telegram_id = $1",
    [postgres.int(telegram_id)],
  )
  |> result.map(parse_user)
}

// src/vibee/db/message_repository.gleam
pub fn save_message(db: Database, msg: Message) -> Result(Nil, DbError) {
  postgres.execute(
    db,
    "INSERT INTO messages (user_id, chat_id, message_id, sender_id, text, direction) 
     VALUES ($1, $2, $3, $4, $5, $6)",
    [
      postgres.int(msg.user_id),
      postgres.bigint(msg.chat_id),
      postgres.bigint(msg.message_id),
      postgres.bigint(msg.sender_id),
      postgres.text(msg.text),
      postgres.text(msg.direction),
    ],
  )
}

pub fn get_chat_history(
  db: Database,
  user_id: Int,
  chat_id: Int,
  limit: Int,
) -> Result(List(Message), DbError) {
  postgres.query(
    db,
    "SELECT * FROM messages 
     WHERE user_id = $1 AND chat_id = $2 
     ORDER BY created_at DESC 
     LIMIT $3",
    [postgres.int(user_id), postgres.bigint(chat_id), postgres.int(limit)],
  )
  |> result.map(list.map(_, parse_message))
}
```

#### Task 2.4: Migrate Existing Code
**Время**: 8 часов
**Приоритет**: P1

**Изменить**:
- `telegram_agent.gleam` - сохранять сообщения в DB
- `polling_actor.gleam` - загружать конфиг из DB
- `trigger_chats.gleam` - читать trigger words из DB

**Checklist Phase 2**:
- [ ] PostgreSQL deployed
- [ ] Migrations работают
- [ ] Repository pattern реализован
- [ ] Все данные в DB (не in-memory)
- [ ] RAG использует DB для контекста

---

## Phase 3: Multi-User Support (5-7 дней) 🟡

### Цель: Поддержка множества пользователей

#### Task 3.1: User Registration API
**Время**: 6 часов
**Приоритет**: P1

```gleam
// src/vibee/api/auth.gleam
pub fn register(req: Request) -> Response {
  case parse_register_request(req.body) {
    Ok(data) -> {
      case user_repository.create_user(db, data.telegram_id, data.username) {
        Ok(user) -> {
          let token = jwt.generate(user.id)
          json.object([
            #("user", user_to_json(user)),
            #("token", json.string(token)),
          ])
          |> json.to_string
          |> response.new(201)
          |> response.set_body
        }
        Error(e) -> error_response(400, "Registration failed")
      }
    }
    Error(_) -> error_response(400, "Invalid request")
  }
}
```

#### Task 3.2: JWT Authentication
**Время**: 4 часа
**Приоритет**: P1

```gleam
// src/vibee/auth/jwt.gleam
pub fn generate(user_id: Int) -> String {
  let payload = json.object([
    #("user_id", json.int(user_id)),
    #("exp", json.int(now() + 86400)), // 24h
    #("iat", json.int(now())),
  ])
  
  jwt_sign(payload, get_secret())
}

pub fn verify(token: String) -> Result(Int, JwtError) {
  case jwt_verify(token, get_secret()) {
    Ok(payload) -> {
      case json.get_int(payload, "user_id") {
        Ok(user_id) -> Ok(user_id)
        Error(_) -> Error(InvalidPayload)
      }
    }
    Error(_) -> Error(InvalidToken)
  }
}

// Middleware
pub fn require_auth(handler: fn(Request, User) -> Response) -> fn(Request) -> Response {
  fn(req: Request) -> Response {
    case get_auth_header(req) {
      Some(token) -> {
        case verify(token) {
          Ok(user_id) -> {
            case user_repository.get_user(db, user_id) {
              Ok(user) -> handler(req, user)
              Error(_) -> error_response(401, "User not found")
            }
          }
          Error(_) -> error_response(401, "Invalid token")
        }
      }
      None -> error_response(401, "Missing authorization")
    }
  }
}
```

#### Task 3.3: Multi-Session Bridge
**Время**: 12 часов
**Приоритет**: P1

**Изменить Go bridge**:
```go
// internal/telegram/session_manager.go
type SessionManager struct {
    sessions map[string]*telegram.Client
    mu       sync.RWMutex
}

func (sm *SessionManager) GetOrCreate(sessionID string, appID int, appHash string) (*telegram.Client, error) {
    sm.mu.RLock()
    if client, ok := sm.sessions[sessionID]; ok {
        sm.mu.RUnlock()
        return client, nil
    }
    sm.mu.RUnlock()
    
    sm.mu.Lock()
    defer sm.mu.Unlock()
    
    // Create new client
    client := telegram.NewClient(appID, appHash, telegram.Options{
        SessionStorage: &session.FileStorage{
            Path: fmt.Sprintf("sessions/%s.session", sessionID),
        },
    })
    
    sm.sessions[sessionID] = client
    return client, nil
}
```

#### Task 3.4: Rate Limiting
**Время**: 6 часов
**Приоритет**: P1

```gleam
// src/vibee/rate_limit.gleam
pub type RateLimiter {
  RateLimiter(
    capacity: Int,
    refill_rate: Float, // tokens per second
    tokens: Float,
    last_refill: Int,
  )
}

pub fn check(limiter: RateLimiter) -> Result(RateLimiter, RateLimitError) {
  let now = now()
  let elapsed = float.from_int(now - limiter.last_refill)
  let new_tokens = float.min(
    float.from_int(limiter.capacity),
    limiter.tokens +. elapsed *. limiter.refill_rate
  )
  
  case new_tokens >=. 1.0 {
    True -> Ok(RateLimiter(..limiter, tokens: new_tokens -. 1.0, last_refill: now))
    False -> Error(RateLimitExceeded)
  }
}

// Middleware
pub fn rate_limit_middleware(
  handler: fn(Request, User) -> Response,
  limit: Int,
) -> fn(Request, User) -> Response {
  fn(req: Request, user: User) -> Response {
    case check_user_rate_limit(user.id, limit) {
      Ok(_) -> handler(req, user)
      Error(_) -> error_response(429, "Rate limit exceeded")
    }
  }
}
```

**Checklist Phase 3**:
- [ ] User registration работает
- [ ] JWT authentication работает
- [ ] Multi-session support в bridge
- [ ] Rate limiting per user
- [ ] API endpoints защищены

---

## Phase 4: Observability (3-5 дней) 🟢

### Цель: Мониторинг и метрики

#### Task 4.1: Prometheus Metrics
**Время**: 6 часов
**Приоритет**: P2

```gleam
// src/vibee/metrics.gleam
pub type Metrics {
  Metrics(
    http_requests_total: Counter,
    http_request_duration: Histogram,
    telegram_messages_total: Counter,
    ai_requests_total: Counter,
    active_users: Gauge,
  )
}

pub fn export() -> String {
  "# HELP http_requests_total Total HTTP requests
# TYPE http_requests_total counter
http_requests_total{method=\"GET\",path=\"/api/v1/chats\"} 1234

# HELP http_request_duration_seconds HTTP request duration
# TYPE http_request_duration_seconds histogram
http_request_duration_seconds_bucket{le=\"0.1\"} 100
http_request_duration_seconds_bucket{le=\"0.5\"} 200
...
"
}
```

#### Task 4.2: Grafana Dashboard
**Время**: 4 часа
**Приоритет**: P2

**Dashboard JSON** (import в Grafana):
- Request rate
- Response time (p50, p95, p99)
- Error rate
- Active users
- AI token usage

#### Task 4.3: Alerts
**Время**: 3 часа
**Приоритет**: P2

```yaml
# alerts.yml
groups:
  - name: vibee_alerts
    rules:
      - alert: HighErrorRate
        expr: rate(http_requests_total{status=~"5.."}[5m]) > 0.05
        for: 5m
        annotations:
          summary: "High error rate: {{ $value }}"
          
      - alert: HighLatency
        expr: histogram_quantile(0.95, http_request_duration_seconds) > 2
        for: 5m
        
      - alert: LowActiveUsers
        expr: active_users < 10
        for: 30m
```

**Checklist Phase 4**:
- [ ] Prometheus metrics экспортируются
- [ ] Grafana dashboard создан
- [ ] Alerts настроены
- [ ] Logs в Loki (опционально)

---

## Phase 5: Optimization (5-7 дней) 🟢

### Цель: Производительность и стоимость

#### Task 5.1: Response Caching
**Время**: 6 часов
**Приоритет**: P2

```gleam
// src/vibee/cache.gleam
pub fn get_cached_response(text: String) -> Option(String) {
  let key = "cache:response:" <> hash(normalize(text))
  redis.get(key)
}

pub fn cache_response(text: String, response: String, ttl_seconds: Int) {
  let key = "cache:response:" <> hash(normalize(text))
  redis.setex(key, ttl_seconds, response)
}
```

#### Task 5.2: Message Queue (Redis Streams)
**Время**: 8 часов
**Приоритет**: P2

```gleam
// src/vibee/queue.gleam
pub fn enqueue_message(stream: String, data: Dict(String, String)) -> Result(Nil, QueueError) {
  redis.xadd(stream, "*", dict.to_list(data))
}

pub fn consume_messages(
  stream: String,
  consumer_group: String,
  consumer_name: String,
  handler: fn(Message) -> Result(Nil, String),
) {
  case redis.xreadgroup(consumer_group, consumer_name, stream, ">", 1) {
    Ok(messages) -> {
      list.each(messages, fn(msg) {
        case handler(msg) {
          Ok(_) -> redis.xack(stream, consumer_group, msg.id)
          Error(_) -> Nil // Will retry
        }
      })
    }
    Error(_) -> Nil
  }
}
```

#### Task 5.3: Auto-Scaling
**Время**: 4 часа
**Приоритет**: P2

```bash
# fly.toml
[services.autoscaling]
  min_machines = 2
  max_machines = 10
  
  [[services.autoscaling.rules]]
    metric = "cpu"
    target = 70
    
  [[services.autoscaling.rules]]
    metric = "concurrency"
    target = 200
```

**Checklist Phase 5**:
- [ ] Response caching работает
- [ ] Message queue реализована
- [ ] Auto-scaling настроен
- [ ] Load testing проведён

---

## Timeline Summary

| Phase | Duration | Priority | Status |
|-------|----------|----------|--------|
| Phase 1: Immediate Fixes | 1-2 дня | P0 | 🔴 TODO |
| Phase 2: Database | 3-5 дней | P1 | 🟡 TODO |
| Phase 3: Multi-User | 5-7 дней | P1 | 🟡 TODO |
| Phase 4: Observability | 3-5 дней | P2 | 🟢 TODO |
| Phase 5: Optimization | 5-7 дней | P2 | 🟢 TODO |
| **Total** | **17-26 дней** | | |

---

## Success Metrics

### Phase 1
- ✅ Uptime > 99%
- ✅ Zero crashes per day
- ✅ Health check always green

### Phase 2
- ✅ All data persisted
- ✅ Zero data loss
- ✅ Query time < 100ms

### Phase 3
- ✅ 100+ users supported
- ✅ Rate limiting working
- ✅ No user interference

### Phase 4
- ✅ All metrics visible
- ✅ Alerts firing correctly
- ✅ P95 latency < 500ms

### Phase 5
- ✅ Cache hit rate > 30%
- ✅ Auto-scaling working
- ✅ Cost < $200/month for 1000 users

---

## Next Action

**Start with Phase 1, Task 1.1**: Implement health check endpoint

```bash
cd /workspaces/vibee-gleam/gleam
mkdir -p src/vibee/health
touch src/vibee/health.gleam
# Implement health check...
```

---

**Status**: Roadmap complete, ready to start
**Estimated total time**: 3-4 weeks
**Last Updated**: 2025-12-18 04:54 UTC
