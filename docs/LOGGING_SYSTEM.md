// Advanced Logging System Documentation

## Overview

VIBEE uses a structured logging system with:
- **6 log levels** (Trace, Debug, Info, Warn, Error, Critical)
- **11 categories** (System, Telegram, Database, API, etc.)
- **Contextual information** (chat_id, user_id, duration, etc.)
- **Anomaly detection** (automatic issue detection)
- **Performance metrics** (timing, counters, gauges)
- **Health monitoring** (system health checks)

## Log Levels

```gleam
Trace    // 🔍 Very detailed, performance impact
Debug    // 🐛 Development debugging
Info     // ℹ️  General information
Warn     // ⚠️  Warning, potential issues
Error    // ❌ Error, needs attention
Critical // 🚨 Critical, immediate action required
```

## Log Categories

```gleam
System      // System-level events
Telegram    // Telegram operations
Database    // Database queries
API         // API requests/responses
WebSocket   // WebSocket events
Agent       // Agent state changes
Trigger     // Trigger detection
Filter      // Chat filtering
Performance // Performance metrics
Security    // Security events
Anomaly     // Anomaly detection
```

## Usage Examples

### Basic Logging

```gleam
import vibee/logging

// Simple logging (legacy compatibility)
logging.quick_info("Server started")
logging.quick_warn("High memory usage")
logging.quick_error("Connection failed")

// Structured logging with context
let ctx = logging.context()
  |> logging.with_category(logging.Telegram)
  |> logging.with_component("message_handler")
  |> logging.with_chat("-1002298297094")
  |> logging.with_user("144022504")

logging.info(ctx, "Processing message")
logging.error(ctx, "Failed to send message")
```

### Domain-Specific Logging

```gleam
// Telegram message
logging.telegram_message("-1002298297094", "John", "Hello world")
// Output: ℹ️  [INFO] [TG:message] chat=-1002298297094 John: Hello world

// Telegram processing
logging.telegram_processing("-1002298297094", 12345, 144022504, "купить крипту")
// Output: 🐛 [DEBUG] [TG:processing] chat=-1002298297094 user=144022504 msg_id=12345 text=купить крипту

// Filter decision
logging.filter_decision("-1002298297094", "ACCEPTED", "target chat")
// Output: 🐛 [DEBUG] [FILTER:chat_filter] chat=-1002298297094 ACCEPTED - target chat

// Trigger detection
logging.trigger_detected("-5082217642", "купить крипту", True)
// Output: ℹ️  [INFO] [TRIGGER:detection] chat=-5082217642 ✅ MATCHED trigger='купить крипту'

// API request
logging.api_request("POST", "/api/v1/sessions", 200)
// Output: 🐛 [DEBUG] [API:request] op=POST /api/v1/sessions status=200

// Database operation
logging.db_operation("INSERT", "messages", 45, True)
// Output: 🐛 [DEBUG] [DB:messages] op=INSERT duration=45ms ✅ OK

// Performance metric
logging.performance("telegram", "send_message", 1250)
// Output: ℹ️  [INFO] [PERF:telegram] op=send_message duration=1250ms - ACCEPTABLE

// Security event
logging.security_event("auth_failure", "Invalid credentials", logging.Warn)
// Output: ⚠️  [WARN] [SEC:security] op=auth_failure Invalid credentials

// Anomaly detection
logging.anomaly("message_rate", "high_frequency", "100 msg/min detected")
// Output: ⚠️  [WARN] [ANOMALY:message_rate] op=high_frequency 100 msg/min detected
```

### Performance Timing

```gleam
import vibee/monitoring/timer

// Manual timing
let timer = timer.start("telegram", "send_message")
// ... do work ...
let duration = timer.stop(timer)

// Automatic timing with result
let timer = timer.start("database", "query")
let result = db.query("SELECT * FROM messages")
timer.stop_with_result(timer, result)

// Measure function
let #(result, duration) = timer.measure("api", "fetch_dialogs", fn() {
  telegram.get_dialogs(config, 50)
})

// Check for slow operations
timer.check_slow_operation("database", "complex_query", 3500, 1000)
// Output: ⚠️  [WARN] [PERF:database] op=complex_query duration=3500ms ⚠️  SLOW - exceeded threshold of 1000ms
```

### Metrics Collection

```gleam
import vibee/monitoring/metrics

// Create metrics
let metrics = metrics.empty_metrics()

// Update counters
let metrics = metrics
  |> metrics.increment_messages_processed
  |> metrics.increment_messages_sent
  |> metrics.increment_triggers

// Update gauges
let metrics = metrics
  |> metrics.set_active_chats(15)
  |> metrics.set_avg_response_time(850)

// Log metrics summary
metrics.log_metrics(metrics)
// Output: ℹ️  [INFO] [PERF:metrics] processed=100 sent=95 errors=2 api=50 db=120 triggers=5 chats=15 avg_time=850ms

// Health check
let status = metrics.health_check(metrics)
metrics.log_health(status, metrics)
// Output: ✅ [INFO] [SYS:health] ✅ HEALTHY - processed=100 errors=2
```

### Anomaly Detection

```gleam
import vibee/monitoring/anomaly_detector as anomaly

// Check error rate
let check = anomaly.check_error_rate(15, 100, 10)
// Triggers if error rate > 10%

// Check performance
let check = anomaly.check_performance("send_message", 3500, 2000)
// Triggers if duration > 2000ms

// Check message patterns
let check = anomaly.check_message_pattern("-1002298297094", 150, 50)
// Triggers if > 50 messages per minute

// Check repeated failures
let check = anomaly.check_repeated_failures("telegram_bridge", 5, 3)
// Triggers if >= 3 consecutive failures

// Check resource usage
let check = anomaly.check_resource_usage("memory", 92, 80)
// Triggers if usage > 80%

// Check auth attempts
let check = anomaly.check_auth_attempts("144022504", 10, 5)
// Triggers if >= 5 failed attempts

// Check data consistency
let check = anomaly.check_data_consistency("message_count", 100, 85)
// Triggers if difference > 10%

// Batch check
let anomalies = anomaly.check_all([check1, check2, check3])
```

## Monitoring Endpoints

### GET /health
Health check endpoint for load balancers

```bash
curl http://localhost:8080/health
```

Response:
```json
{
  "status": "healthy",
  "metrics": {
    "messages_processed": 1250,
    "messages_sent": 1180,
    "errors_count": 12,
    "api_calls": 450,
    "db_queries": 890,
    "avg_response_time_ms": 850,
    "active_chats": 15,
    "triggers_detected": 45
  }
}
```

### GET /metrics
Prometheus-compatible metrics

```bash
curl http://localhost:8080/metrics
```

Response:
```
# HELP vibee_messages_processed_total Total messages processed
# TYPE vibee_messages_processed_total counter
vibee_messages_processed_total 1250

# HELP vibee_errors_total Total errors
# TYPE vibee_errors_total counter
vibee_errors_total 12
...
```

### GET /status
Detailed status with anomalies

```bash
curl http://localhost:8080/status
```

Response:
```json
{
  "health": "degraded",
  "metrics": { ... },
  "anomalies": [
    {
      "type": "slow_performance",
      "severity": "high",
      "component": "database",
      "description": "Query exceeded threshold"
    }
  ],
  "anomaly_count": 1
}
```

## Log Filtering

### By Level
```bash
# Only errors and critical
tail -f /tmp/vibee.log | grep -E "(ERROR|CRITICAL)"

# Only warnings and above
tail -f /tmp/vibee.log | grep -E "(WARN|ERROR|CRITICAL)"
```

### By Category
```bash
# Only Telegram logs
tail -f /tmp/vibee.log | grep "\[TG:"

# Only Database logs
tail -f /tmp/vibee.log | grep "\[DB:"

# Only Performance logs
tail -f /tmp/vibee.log | grep "\[PERF:"

# Only Anomalies
tail -f /tmp/vibee.log | grep "\[ANOMALY:"
```

### By Chat
```bash
# Specific chat
tail -f /tmp/vibee.log | grep "chat=-1002298297094"

# All chats
tail -f /tmp/vibee.log | grep "chat="
```

### By Operation
```bash
# Message sending
tail -f /tmp/vibee.log | grep "op=send_message"

# Database queries
tail -f /tmp/vibee.log | grep "op=.*query"

# Trigger detection
tail -f /tmp/vibee.log | grep "op=.*trigger"
```

### Combined Filters
```bash
# Errors in Telegram category
tail -f /tmp/vibee.log | grep "ERROR" | grep "\[TG:"

# Slow operations
tail -f /tmp/vibee.log | grep "SLOW"

# Critical issues
tail -f /tmp/vibee.log | grep "CRITICAL"

# Specific chat errors
tail -f /tmp/vibee.log | grep "chat=-1002298297094" | grep "ERROR"
```

## Best Practices

### 1. Use Appropriate Log Levels
- **Trace**: Very detailed debugging (disabled in production)
- **Debug**: Development debugging
- **Info**: Normal operations, important events
- **Warn**: Potential issues, degraded performance
- **Error**: Errors that need attention
- **Critical**: System-critical issues requiring immediate action

### 2. Add Context
Always include relevant context:
```gleam
let ctx = logging.context()
  |> logging.with_category(logging.Telegram)
  |> logging.with_component("message_handler")
  |> logging.with_chat(chat_id)
  |> logging.with_user(user_id)
  |> logging.with_duration(duration_ms)
```

### 3. Use Domain-Specific Functions
Prefer domain-specific functions over generic logging:
```gleam
// ✅ Good
logging.telegram_processing(chat_id, msg_id, from_id, text)

// ❌ Avoid
logging.quick_info("Processing message from " <> chat_id)
```

### 4. Measure Performance
Always measure critical operations:
```gleam
let timer = timer.start("database", "insert_message")
let result = db.insert(message)
timer.stop_with_result(timer, result)
```

### 5. Check for Anomalies
Regularly check metrics for anomalies:
```gleam
let anomalies = metrics.check_metrics_health(metrics)
case list.length(anomalies) {
  0 -> Nil
  n -> logging.quick_warn("Detected " <> int.to_string(n) <> " anomalies")
}
```

## Troubleshooting

### No logs appearing
1. Check log level configuration
2. Verify stdout is not redirected
3. Check if process is running

### Too many logs
1. Increase log level (Debug -> Info -> Warn)
2. Filter by category
3. Use log aggregation tools

### Performance impact
1. Disable Trace level in production
2. Use async logging for high-volume
3. Sample logs (log every Nth event)

### Missing context
1. Always create context with `logging.context()`
2. Add relevant fields with builder methods
3. Pass context through function calls

## Migration from Old System

### Before
```gleam
io.println("[POLL] Processing chat: " <> chat_id)
io.println("[ERROR] Failed to send message")
```

### After
```gleam
let ctx = logging.context()
  |> logging.with_category(logging.Agent)
  |> logging.with_component("polling")
  |> logging.with_chat(chat_id)

logging.info(ctx, "Processing chat")
logging.error(ctx, "Failed to send message")
```

## Integration with External Tools

### Grafana + Loki
Parse structured logs for visualization

### Prometheus
Use `/metrics` endpoint for scraping

### Sentry
Send Critical/Error logs to Sentry

### CloudWatch
Stream logs to AWS CloudWatch

### Datadog
Use Datadog agent for log collection
