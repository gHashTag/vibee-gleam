# 🤖 Стратегия цифрового клона в Telegram

## Что такое цифровой клон (Digital Twin)?

Цифровой клон - это AI-агент, который работает в Telegram от имени пользователя, автоматически отвечая на сообщения в чатах.

## Текущая реализация

### Файлы:
- `gleam/src/vibee/telegram/telegram_agent.gleam` - основной агент
- `gleam/src/vibee/agent/polling_actor.gleam` - polling сообщений
- `gleam/src/vibee/config/target_chats.gleam` - конфигурация чатов
- `gleam/src/vibee/mcp/super_agent.gleam` - супер-агент с AI

### Ключевые параметры:

```gleam
pub type TelegramAgentConfig {
  TelegramAgentConfig(
    bridge_url: String,              // URL Go bridge для MTProto
    session_id: String,              // Telegram session ID
    llm_api_key: Option(String),     // API ключ для LLM
    llm_model: String,               // Модель: "x-ai/grok-4.1-fast"
    auto_reply_enabled: Bool,        // Автоответы вкл/выкл
    cooldown_ms: Int,                // Задержка между ответами (30 сек)
    
    // Digital Twin режим
    digital_twin_enabled: Bool,      // Режим цифрового клона
    owner_id: Int,                   // ID владельца (144022504)
  )
}
```

## Режимы работы

### 1. **Target Chats Mode** (Целевые чаты)
- Отвечает ТОЛЬКО в указанных чатах
- Список в `target_chats.gleam`
- Используется для групп и каналов

**Целевые чаты:**
```gleam
pub const target_chats = [
  "693774948",      // Личный чат для тестов
  "144022504",      // Dmitrii (Owner)
  "2737186844",     // VIBEE AGENT (supergroup)
  "2298297094",     // Тестовый канал
  "6579515876",     // vibee_agent bot
  "-5082217642",    // Aimly.io dev (group)
]
```

### 2. **Digital Twin Mode** (Цифровой клон)
- Отвечает в целевых чатах + ВСЕ личные чаты
- Исключает self-chat (Saved Messages)
- Работает как полноценный цифровой двойник

**Логика:**
```gleam
pub fn should_process_chat(chat_id: String) -> Bool {
  case chat_id == owner_id {
    True -> False  // Self-chat - не обрабатываем
    False -> {
      case is_target_chat(chat_id) {
        True -> True  // Целевой чат - всегда
        False -> is_private_chat(chat_id)  // Личный чат - в Digital Twin режиме
      }
    }
  }
}
```

### 3. **Trigger Words Mode** (Триггерные слова)
- Отвечает только на сообщения с триггерными словами
- Используется в супер-агенте

**Триггерные слова:**
```gleam
trigger_words: [
  "vibee", "vibe", 
  "бот", "агент", 
  "помоги", "сделай", "напиши"
]
```

## Параметры стратегии

### 1. **Auto Reply** (Автоответы)
- `auto_reply_enabled: Bool`
- Включает/выключает автоматические ответы
- По умолчанию: `True`

### 2. **Cooldown** (Задержка)
- `cooldown_ms: Int`
- Минимальная задержка между ответами
- По умолчанию: `30_000` (30 секунд)
- Предотвращает спам

### 3. **Confidence Threshold** (Порог уверенности)
- `confidence_threshold: Float`
- Минимальная уверенность AI для автоответа
- По умолчанию: `0.7` (70%)
- Используется в супер-агенте

### 4. **Target Chats** (Целевые чаты)
- `target_chats: List(Int)`
- Список ID чатов для мониторинга
- Пустой список = все чаты

### 5. **Trigger Words** (Триггерные слова)
- `trigger_words: List(String)`
- Слова, которые активируют ответ
- Используется для фильтрации

### 6. **LLM Model** (Модель AI)
- `llm_model: String`
- Модель для генерации ответов
- По умолчанию: `"x-ai/grok-4.1-fast"`
- Можно менять на другие модели

## Стратегии продвижения

### 1. **Passive Mode** (Пассивный режим)
- Digital Twin: OFF
- Auto Reply: OFF
- Только мониторинг и логирование
- Используется для сбора данных

### 2. **Selective Mode** (Выборочный режим)
- Digital Twin: OFF
- Auto Reply: ON
- Target Chats: указаны
- Trigger Words: указаны
- Отвечает только в целевых чатах на триггерные слова

### 3. **Active Mode** (Активный режим)
- Digital Twin: ON
- Auto Reply: ON
- Target Chats: указаны
- Отвечает в целевых чатах + все личные чаты

### 4. **Aggressive Mode** (Агрессивный режим)
- Digital Twin: ON
- Auto Reply: ON
- Target Chats: [] (пустой = все)
- Cooldown: 10_000 (10 сек)
- Confidence: 0.5 (50%)
- Отвечает везде и быстро

## Настройки для разных целей

### Lead Generation (Генерация лидов)
```gleam
TelegramAgentConfig(
  digital_twin_enabled: False,
  auto_reply_enabled: True,
  cooldown_ms: 60_000,  // 1 минута
  target_chats: ["2737186844"],  // VIBEE AGENT group
  trigger_words: ["купить", "продать", "помоги", "как"],
  confidence_threshold: 0.8,  // Высокая уверенность
)
```

### Personal Assistant (Личный помощник)
```gleam
TelegramAgentConfig(
  digital_twin_enabled: True,
  auto_reply_enabled: True,
  cooldown_ms: 30_000,  // 30 секунд
  target_chats: [],  // Все чаты
  trigger_words: [],  // Без фильтрации
  confidence_threshold: 0.7,
)
```

### Community Manager (Менеджер сообщества)
```gleam
TelegramAgentConfig(
  digital_twin_enabled: False,
  auto_reply_enabled: True,
  cooldown_ms: 120_000,  // 2 минуты
  target_chats: ["2737186844", "2298297094"],  // Группы
  trigger_words: ["вопрос", "помощь", "как", "что"],
  confidence_threshold: 0.75,
)
```

### Sales Bot (Продажи)
```gleam
TelegramAgentConfig(
  digital_twin_enabled: True,
  auto_reply_enabled: True,
  cooldown_ms: 15_000,  // 15 секунд
  target_chats: [],
  trigger_words: ["купить", "цена", "стоимость", "заказать"],
  confidence_threshold: 0.6,  // Ниже порог для продаж
)
```

## Метрики и аналитика

### Отслеживаемые метрики:
```gleam
pub type SuperAgentStats {
  SuperAgentStats(
    events_processed: Int,      // Обработано событий
    tasks_created: Int,         // Создано задач
    tasks_completed: Int,       // Выполнено задач
    tasks_failed: Int,          // Провалено задач
    messages_sent: Int,         // Отправлено сообщений
    uptime_seconds: Int,        // Время работы
    started_at: Int,            // Время запуска
  )
}
```

### Confidence Scores (Уверенность по возможностям):
```gleam
confidence_scores: Dict(String, Float) = {
  "code_generate": 0.8,    // Генерация кода
  "code_refactor": 0.75,   // Рефакторинг
  "test_run": 0.85,        // Запуск тестов
  "test_create": 0.7,      // Создание тестов
  "debug_build": 0.9,      // Отладка сборки
  "debug_analyze": 0.75,   // Анализ ошибок
  "debug_fix": 0.65,       // Исправление ошибок
}
```

## Управление через API

### Endpoints:
- `POST /api/agent/start` - запустить агента
- `POST /api/agent/stop` - остановить агента
- `POST /api/agent/config` - обновить конфигурацию
- `GET /api/agent/status` - получить статус
- `GET /api/agent/stats` - получить статистику

### WebSocket:
- `wss://vibee-mcp.fly.dev/ws/logs` - логи в реальном времени
- `wss://vibee-mcp.fly.dev/ws/agent` - статус агента

## Что нужно для Dashboard

### Левая панель (Control Panel):

#### 1. **Agent Status**
- ON/OFF переключатель
- Текущий статус (Running/Paused/Stopped)
- Uptime
- Последняя активность

#### 2. **Strategy Selector**
- Passive Mode
- Selective Mode
- Active Mode
- Aggressive Mode
- Custom

#### 3. **Configuration**
- Digital Twin: ON/OFF
- Auto Reply: ON/OFF
- Cooldown: slider (10s - 300s)
- Confidence: slider (0.5 - 0.95)

#### 4. **Target Chats**
- Список целевых чатов
- Добавить/удалить чаты
- Поиск по чатам

#### 5. **Trigger Words**
- Список триггерных слов
- Добавить/удалить слова
- Regex поддержка

#### 6. **LLM Settings**
- Выбор модели
- Temperature
- Max tokens
- System prompt

#### 7. **Statistics**
- Messages processed
- Messages sent
- Response rate
- Average confidence
- Success rate

#### 8. **Quick Actions**
- Start/Stop
- Pause/Resume
- Reset stats
- Export config

### Правая панель (Telegram Logs):

#### Как в p2p странице:
- Real-time логи из Telegram
- Фильтры (чаты, пользователи, время)
- Поиск
- Экспорт
- Контекстное меню

#### Типы логов:
- 📨 Incoming message
- 📤 Outgoing message (от агента)
- ⚙️ System event
- ✅ Task completed
- ❌ Task failed
- 🤖 AI decision

## Инфографика

### 1. **Activity Timeline**
- График сообщений по времени
- Входящие vs исходящие
- Пики активности

### 2. **Chat Distribution**
- Pie chart: распределение по чатам
- Топ-5 самых активных чатов

### 3. **Response Rate**
- Gauge: процент ответов
- Target: 80%+

### 4. **Confidence Heatmap**
- Тепловая карта уверенности по возможностям
- Цветовое кодирование

### 5. **Success Rate**
- Line chart: успешность задач
- Тренд по времени

### 6. **Cooldown Timer**
- Countdown до следующего возможного ответа
- Visual indicator

## Пример конфигурации для Dashboard

```json
{
  "agent": {
    "status": "running",
    "uptime": 3600,
    "last_activity": "2025-12-18T14:30:00Z"
  },
  "strategy": {
    "mode": "active",
    "digital_twin_enabled": true,
    "auto_reply_enabled": true,
    "cooldown_ms": 30000,
    "confidence_threshold": 0.7
  },
  "target_chats": [
    {"id": "2737186844", "name": "VIBEE AGENT", "type": "supergroup"},
    {"id": "144022504", "name": "Dmitrii", "type": "private"}
  ],
  "trigger_words": ["vibee", "помоги", "сделай"],
  "llm": {
    "model": "x-ai/grok-4.1-fast",
    "temperature": 0.7,
    "max_tokens": 1000
  },
  "stats": {
    "events_processed": 1234,
    "messages_sent": 567,
    "response_rate": 0.85,
    "avg_confidence": 0.78,
    "success_rate": 0.92
  }
}
```

## Итог

Dashboard должен позволять:
1. ✅ Включать/выключать агента
2. ✅ Выбирать стратегию (preset или custom)
3. ✅ Настраивать параметры (cooldown, confidence, etc)
4. ✅ Управлять целевыми чатами
5. ✅ Управлять триггерными словами
6. ✅ Выбирать LLM модель
7. ✅ Видеть статистику в реальном времени
8. ✅ Видеть логи из Telegram справа
9. ✅ Экспортировать конфигурацию
10. ✅ Быстрые действия (start/stop/pause)
