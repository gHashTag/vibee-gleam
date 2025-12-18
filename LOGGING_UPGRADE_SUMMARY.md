# Advanced Logging System - Upgrade Summary

## Что создано ✅

### 1. Улучшенный модуль логирования (`vibee/logging.gleam`)

**Новые возможности:**
- ✅ 6 уровней логирования (Trace, Debug, Info, Warn, Error, Critical)
- ✅ 11 категорий (System, Telegram, Database, API, WebSocket, Agent, Trigger, Filter, Performance, Security, Anomaly)
- ✅ Структурированный контекст (chat_id, user_id, duration_ms, error_code, operation)
- ✅ Builder pattern для создания контекста
- ✅ Emoji индикаторы для быстрой визуальной идентификации
- ✅ Domain-specific функции для типичных операций

**Примеры использования:**
```gleam
// Структурированное логирование
let ctx = logging.context()
  |> logging.with_category(logging.Telegram)
  |> logging.with_component("message_handler")
  |> logging.with_chat("-1002298297094")
  |> logging.with_user("144022504")
  |> logging.with_duration(850)

logging.info(ctx, "Message processed successfully")
// Output: ℹ️  [INFO] [TG:message_handler] chat=-1002298297094 user=144022504 duration=850ms Message processed successfully

// Domain-specific функции
logging.telegram_processing(chat_id, msg_id, from_id, text)
logging.filter_decision(chat_id, "ACCEPTED", "target chat")
logging.trigger_detected(chat_id, "купить крипту", True)
logging.db_operation("INSERT", "messages", 45, True)
logging.performance("telegram", "send_message", 1250)
```

### 2. Система детекции аномалий (`vibee/monitoring/anomaly_detector.gleam`)

**Типы аномалий:**
- ✅ HighErrorRate - высокий процент ошибок
- ✅ SlowPerformance - медленные операции
- ✅ UnexpectedBehavior - неожиданное поведение
- ✅ SecurityThreat - угрозы безопасности
- ✅ ResourceExhaustion - исчерпание ресурсов
- ✅ DataInconsistency - несоответствие данных

**Проверки:**
```gleam
// Проверка процента ошибок
anomaly.check_error_rate(errors: 15, total: 100, threshold: 10)
// Срабатывает если > 10% ошибок

// Проверка производительности
anomaly.check_performance("send_message", duration: 3500, threshold: 2000)
// Срабатывает если > 2000ms

// Проверка частоты сообщений
anomaly.check_message_pattern(chat_id, messages_per_min: 150, threshold: 50)
// Срабатывает если > 50 сообщений в минуту

// Проверка повторяющихся ошибок
anomaly.check_repeated_failures("telegram_bridge", failures: 5, threshold: 3)
// Срабатывает если >= 3 последовательных ошибок

// Проверка использования ресурсов
anomaly.check_resource_usage("memory", usage: 92, threshold: 80)
// Срабатывает если > 80% использования

// Проверка попыток аутентификации
anomaly.check_auth_attempts(user_id, failed: 10, threshold: 5)
// Срабатывает если >= 5 неудачных попыток

// Проверка консистентности данных
anomaly.check_data_consistency("message_count", expected: 100, actual: 85)
// Срабатывает если разница > 10%
```

### 3. Система метрик (`vibee/monitoring/metrics.gleam`)

**Метрики:**
- ✅ messages_processed - обработано сообщений
- ✅ messages_sent - отправлено сообщений
- ✅ errors_count - количество ошибок
- ✅ api_calls - API вызовы
- ✅ db_queries - запросы к БД
- ✅ avg_response_time_ms - среднее время ответа
- ✅ active_chats - активные чаты
- ✅ triggers_detected - обнаружено триггеров

**Health Check:**
```gleam
let status = metrics.health_check(metrics)
// Возвращает: Healthy | Degraded | Unhealthy

metrics.log_health(status, metrics)
// Output: ✅ [INFO] [SYS:health] ✅ HEALTHY - processed=100 errors=2
```

### 4. Таймеры производительности (`vibee/monitoring/timer.gleam`)

**Измерение времени:**
```gleam
// Ручное измерение
let timer = timer.start("telegram", "send_message")
// ... выполнение операции ...
let duration = timer.stop(timer)

// С результатом
let timer = timer.start("database", "query")
let result = db.query("SELECT * FROM messages")
timer.stop_with_result(timer, result)

// Измерение функции
let #(result, duration) = timer.measure("api", "fetch_dialogs", fn() {
  telegram.get_dialogs(config, 50)
})

// Проверка медленных операций
timer.check_slow_operation("database", "complex_query", 3500, 1000)
// Output: ⚠️  [WARN] [PERF:database] op=complex_query duration=3500ms ⚠️  SLOW - exceeded threshold of 1000ms
```

### 5. Monitoring API Endpoints (`vibee/api/monitoring_handlers.gleam`)

**Endpoints:**

**GET /health** - Health check для load balancers
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
    "avg_response_time_ms": 850
  }
}
```

**GET /metrics** - Prometheus-совместимые метрики
```bash
curl http://localhost:8080/metrics
```
Response:
```
# HELP vibee_messages_processed_total Total messages processed
# TYPE vibee_messages_processed_total counter
vibee_messages_processed_total 1250
...
```

**GET /status** - Детальный статус с аномалиями
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

## Преимущества новой системы

### 1. Быстрая диагностика
```bash
# Найти все ошибки
tail -f /tmp/vibee.log | grep "ERROR"

# Найти медленные операции
tail -f /tmp/vibee.log | grep "SLOW"

# Найти проблемы в конкретном чате
tail -f /tmp/vibee.log | grep "chat=-1002298297094" | grep "ERROR"

# Найти аномалии
tail -f /tmp/vibee.log | grep "ANOMALY"
```

### 2. Структурированные логи
```
❌ [ERROR] [TG:message_handler] chat=-1002298297094 user=144022504 duration=3500ms op=send_message Failed to send message
```

Легко парсить:
- Уровень: ERROR
- Категория: TG (Telegram)
- Компонент: message_handler
- Контекст: chat, user, duration, operation
- Сообщение: Failed to send message

### 3. Автоматическая детекция проблем
Система автоматически обнаруживает:
- Высокий процент ошибок (> 10%)
- Медленные операции (> 2000ms)
- Необычную активность (> 50 msg/min)
- Повторяющиеся ошибки (>= 3 подряд)
- Высокое использование ресурсов (> 80%)
- Подозрительную активность (>= 5 неудачных попыток)

### 4. Мониторинг в реальном времени
```bash
# Следить за здоровьем системы
watch -n 5 'curl -s http://localhost:8080/health | jq'

# Prometheus scraping
curl http://localhost:8080/metrics

# Детальный статус
curl http://localhost:8080/status | jq
```

### 5. Интеграция с внешними системами
- **Grafana + Loki** - визуализация логов
- **Prometheus** - сбор метрик
- **Sentry** - отслеживание ошибок
- **CloudWatch** - AWS логи
- **Datadog** - полный мониторинг

## Примеры использования

### Пример 1: Логирование обработки сообщения

```gleam
pub fn handle_message(chat_id: String, msg_id: Int, from_id: Int, text: String) -> Result(Nil, String) {
  // Создаём контекст
  let ctx = logging.context()
    |> logging.with_category(logging.Telegram)
    |> logging.with_component("message_handler")
    |> logging.with_chat(chat_id)
    |> logging.with_user(int.to_string(from_id))
  
  // Логируем начало обработки
  logging.debug(ctx, "Processing message: " <> text)
  
  // Измеряем время
  let timer = timer.start("telegram", "handle_message")
  
  // Обрабатываем
  let result = case process_message(text) {
    Ok(reply) -> {
      logging.info(ctx, "Message processed successfully")
      send_reply(chat_id, reply)
    }
    Error(err) -> {
      logging.error(ctx |> logging.with_error(err), "Failed to process message")
      Error(err)
    }
  }
  
  // Останавливаем таймер
  let duration = timer.stop(timer)
  
  // Проверяем на медленную операцию
  timer.check_slow_operation("telegram", "handle_message", duration, 2000)
  
  result
}
```

### Пример 2: Мониторинг системы

```gleam
pub fn monitor_system(metrics: metrics.SystemMetrics) -> Nil {
  // Логируем метрики
  metrics.log_metrics(metrics)
  
  // Проверяем здоровье
  let status = metrics.health_check(metrics)
  metrics.log_health(status, metrics)
  
  // Проверяем аномалии
  let anomalies = metrics.check_metrics_health(metrics)
  
  case list.length(anomalies) {
    0 -> logging.quick_info("✅ No anomalies detected")
    n -> {
      logging.quick_warn("⚠️  Detected " <> int.to_string(n) <> " anomalies")
      
      list.each(anomalies, fn(anomaly) {
        let msg = anomaly_detector.format_anomaly(anomaly)
        logging.quick_warn(msg)
      })
    }
  }
}
```

### Пример 3: Детекция атаки

```gleam
pub fn check_security(user_id: String, failed_attempts: Int) -> Nil {
  case anomaly.check_auth_attempts(user_id, failed_attempts, 5) {
    Some(anomaly) -> {
      // Обнаружена подозрительная активность
      logging.security_event(
        "brute_force_attempt",
        "User " <> user_id <> " exceeded auth attempt threshold",
        logging.Critical
      )
      
      // Блокируем пользователя
      block_user(user_id)
    }
    None -> Nil
  }
}
```

## Миграция

### Шаг 1: Замените io.println на logging

**Было:**
```gleam
io.println("[POLL] Processing chat: " <> chat_id)
io.println("[ERROR] Failed to send message")
```

**Стало:**
```gleam
let ctx = logging.context()
  |> logging.with_category(logging.Agent)
  |> logging.with_component("polling")
  |> logging.with_chat(chat_id)

logging.info(ctx, "Processing chat")
logging.error(ctx, "Failed to send message")
```

### Шаг 2: Добавьте измерение времени

**Было:**
```gleam
let result = send_message(config, chat_id, text)
```

**Стало:**
```gleam
let timer = timer.start("telegram", "send_message")
let result = send_message(config, chat_id, text)
timer.stop_with_result(timer, result)
```

### Шаг 3: Добавьте детекцию аномалий

```gleam
// В конце каждого цикла обработки
let anomalies = metrics.check_metrics_health(metrics)
case list.length(anomalies) > 0 {
  True -> logging.quick_warn("Anomalies detected!")
  False -> Nil
}
```

## Следующие шаги

1. ✅ Система логирования создана
2. ✅ Детекция аномалий реализована
3. ✅ Метрики и мониторинг готовы
4. ⏳ Интегрировать в существующий код
5. ⏳ Добавить endpoints в router
6. ⏳ Настроить Prometheus scraping
7. ⏳ Настроить Grafana dashboards

## Документация

- **Полная документация**: `docs/LOGGING_SYSTEM.md`
- **Примеры использования**: см. выше
- **API Reference**: см. комментарии в коде

## Поддержка

Вопросы? Проблемы? Создайте issue в репозитории.
