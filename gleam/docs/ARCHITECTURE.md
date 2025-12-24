# VIBEE Architecture

## ElizaOS Actions-Based Architecture

VIBEE использует **user-bot** (Digital Twin), а не обычный Telegram бот.
Это означает понимание естественного языка вместо `/commands`.

### Принцип работы

```
User: "создай рилс про продуктивность"
              │
              ▼
┌─────────────────────────────────────┐
│         detect_action()              │
│  Анализ естественного языка          │
│  → Находит паттерн "создай рилс"     │
│  → Возвращает CREATE_REELS action    │
└─────────────────────────────────────┘
              │
              ▼
┌─────────────────────────────────────┐
│         handle_reels_action()        │
│  1. Extract idea from message        │
│  2. Generate script                  │
│  3. TTS → Lipsync → Render           │
│  4. Send video to user               │
└─────────────────────────────────────┘
```

### Actions (Действия)

| Action | Триггеры | Описание |
|--------|----------|----------|
| `CREATE_REELS` | "создай рилс", "сделай рилс" | Генерация видео-рилса |
| `CREATE_VIDEO` | "хочу видео", "создай видео" | Генерация видео |
| `GENERATE_IMAGE` | "нарисуй", "сгенерируй фото" | Генерация изображения |
| `HELP` | "помощь", "что умеешь" | Справка |
| `PRICING` | "цены", "тарифы" | Информация о ценах |
| `VOICE_CLONE` | "озвучь", "голос" | Клонирование голоса |

### Структура файлов

```
src/vibee/
├── telegram/
│   └── telegram_agent.gleam    # Главный обработчик сообщений
│       ├── detect_action()     # Определение действия
│       ├── handle_reels_action()
│       ├── handle_video_command()
│       └── handle_neurophoto_command()
│
├── agent/
│   ├── polling_actor.gleam     # Polling новых сообщений
│   └── actions/
│       └── reels_action.gleam  # CREATE_REELS action
│
└── video/
    └── pipeline.gleam          # Video rendering pipeline
```

---

## Message Flow

```
┌──────────────────────────────────────────────────────────────┐
│                    Telegram (MTProto)                         │
└──────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌──────────────────────────────────────────────────────────────┐
│                   polling_actor.gleam                         │
│  • Polling каждые 3 сек                                       │
│  • Обрабатывает 50+ чатов параллельно                        │
│  • Фильтрует: owner messages, trigger chats                  │
└──────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌──────────────────────────────────────────────────────────────┐
│                  telegram_agent.gleam                         │
│  handle_incoming_message()                                    │
│  ┌────────────────────────────────────────────────────────┐  │
│  │  1. detect_action(text) → Some(#("reels", prompt))     │  │
│  │  2. case action:                                        │  │
│  │     • CREATE_REELS → handle_reels_action()             │  │
│  │     • CREATE_VIDEO → handle_video_command()            │  │
│  │     • GENERATE_IMAGE → handle_neurophoto_command()     │  │
│  │     • None → check trigger_chats (P2P sniper)          │  │
│  └────────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌──────────────────────────────────────────────────────────────┐
│                    AI Pipeline                                │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐         │
│  │   TTS   │→ │ Lipsync │→ │ B-Roll  │→ │ Render  │         │
│  │ElevenLabs│  │  FAL.ai │  │ FLUX.1  │  │Remotion │         │
│  └─────────┘  └─────────┘  └─────────┘  └─────────┘         │
└──────────────────────────────────────────────────────────────┘
```

---

## Ключевые модули

### telegram_agent.gleam

Главный обработчик сообщений:

```gleam
fn detect_action(text: String) -> Option(#(String, String)) {
  let lower = string.lowercase(text)

  // CREATE_REELS
  case string.contains(lower, "создай рилс")
    || string.contains(lower, "сделай рилс") {
    True -> Some(#("reels", extract_prompt(text)))
    False -> // check next pattern...
  }
}
```

### polling_actor.gleam

OTP actor для polling сообщений:

```gleam
pub fn start_polling(session_id: String) {
  // Polling loop каждые 3 секунды
  // Обрабатывает только:
  // - Сообщения от owner (144022504)
  // - Сообщения в trigger chats (P2P)
}
```

### pipeline.gleam

Video rendering pipeline:

```gleam
pub type PipelineRequest {
  PipelineRequest(
    photo_url: String,
    script_text: String,
    voice_id: Option(String),
    test_mode: Bool,
    quick_test: Bool,  // 5 sec render for E2E tests
  )
}
```

---

## Trigger Chats (P2P Sniper)

Для P2P торговли криптой бот мониторит специальные чаты:

```gleam
// src/vibee/config/trigger_chats.gleam
pub fn get_trigger_words() -> List(String) {
  ["купить", "куплю", "продать", "продам", "btc", "usdt", "крипту"]
}
```

При обнаружении триггера → отправка Lead Card в CRM.

---

## E2E Testing

Rainbow Bridge — автоматическое тестирование через два Telegram аккаунта:

| Роль | Аккаунт |
|------|---------|
| Tester | @neuro_sage |
| Bot | @vibee_agent |

```bash
# Запуск E2E тестов
curl https://vibee-mcp.fly.dev/api/e2e/run
```

---

## Deployment

| Сервис | Fly.io App | URL |
|--------|------------|-----|
| MCP Server | vibee-mcp | vibee-mcp.fly.dev |
| Telegram Bridge | vibee-telegram-bridge | vibee-telegram-bridge.fly.dev |
| Remotion | vibee-remotion | vibee-remotion.fly.dev |

```bash
fly deploy -a vibee-mcp
fly deploy -a vibee-telegram-bridge
fly deploy -a vibee-remotion
```
