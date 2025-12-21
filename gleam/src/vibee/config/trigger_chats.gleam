// Trigger-based Chat Configuration
// Чаты с триггерными словами и пересылкой диалогов

import gleam/int
import gleam/io
import gleam/list
import gleam/string

/// Конфигурация чата с триггерами
pub type TriggerChatConfig {
  TriggerChatConfig(
    chat_id: String,
    chat_name: String,
    chat_type: String,
    is_active: Bool,
    can_write: Bool,
    response_probability: Float,
    custom_triggers: List(String),
    forward_chat_id: String,
    allow_images: Bool,
    response_template: String,  // Шаблон ответа для этого чата
    // E2E Test patterns (Single Source of Truth)
    expected_response_pattern: String,  // Паттерн для проверки ответа бота
    expected_forward_pattern: String,   // Паттерн для проверки пересылки
  )
}

/// Общий список триггерных слов для крипто-групп
fn crypto_triggers() -> List(String) {
  [
    // Покупка/продажа
    "куплю крипту",
    "купить крипту",
    "куплю крипты",
    "купить крипты",
    "где купить",
    "где куплю",
    "подскажите где купить",
    "как купить",
    "хочу купить",
    "хочу куплю",
    "я бы купил",
    "я бы крипты купил",
    "крипту купить",
    "крипты купить",
    "куплю биткоин",
    "купить биткоин",

    // Обмен
    "обменять крипту",
    "обмен крипты",
    "обменять на",
    "обменник",
    "обмен",
    "п2п",
    "p2p",
    "обменять биткоин",

    // Криптовалюты
    "usdt",
    "баты",
    "купить usdt",
    "куплю usdt",
    "биткоин",
    "эфир",
    "токены",
    "монеты",
    "криптовалюту",
    "валюту",
    "биткоин на",
    "на биткоин",
    "крипта на",
    "на крипту",
    "крипты на",
    "на крипты",

    // Вопросы
    "где взять",
    "где достать",
    "пацаны где",
    "ребята где",
    "где можно купить",
    "где можно обменять",
  ]
}

/// Все чаты с триггерами
pub fn get_trigger_chats() -> List(TriggerChatConfig) {
  [
    // Крипто Группа 1 (Aimly.io dev) - SNIPER MODE
    // Агент молчит ВСЕГДА, кроме случаев с триггерными словами
    // ВАЖНО: Это обычная группа (Chat), НЕ Channel! Формат: -chatID (без -100)
    TriggerChatConfig(
      chat_id: "-5082217642",
      chat_name: "Aimly.io dev",
      chat_type: "group",
      is_active: True,
      can_write: True,
      response_probability: 0.0,  // SNIPER MODE: 0% случайных ответов
      custom_triggers: crypto_triggers(),
      forward_chat_id: "-1002737186844",  // Lead группа для пересылки диалогов (FULL ID с -100 префиксом!)
      allow_images: False,
      response_template: "Напиши мне в личку, помогу разобраться",
      // E2E Test patterns
      expected_response_pattern: "личку|напиши|помогу|разобраться",
      expected_forward_pattern: "ЛИД|Клиент|крипт",
    ),
    // Крипто Группа 2 - SNIPER MODE
    TriggerChatConfig(
      chat_id: "-1002298297094",
      chat_name: "Crypto Group 2",
      chat_type: "group",
      is_active: True,
      can_write: True,
      response_probability: 0.0,  // SNIPER MODE: 0% случайных ответов
      custom_triggers: crypto_triggers(),
      forward_chat_id: "-1002737186844",  // Lead группа для пересылки диалогов (FULL ID с -100 префиксом!)
      allow_images: False,
      response_template: "Напиши мне в личку, помогу разобраться",
      // E2E Test patterns
      expected_response_pattern: "личку|напиши|помогу|разобраться",
      expected_forward_pattern: "ЛИД|Клиент|крипт",
    ),
  ]
}

/// Проверяет, содержит ли текст триггерное слово
pub fn contains_trigger(text: String, triggers: List(String)) -> Bool {
  let lower_text = string.lowercase(text)
  io.println("[TRIGGER] Normalized text: " <> lower_text)
  
  let result = list.any(triggers, fn(trigger) {
    let lower_trigger = string.lowercase(trigger)
    let matches = string.contains(lower_text, lower_trigger)
    case matches {
      True -> {
        io.println("[TRIGGER] 🎯 Matched trigger: '" <> trigger <> "'")
        True
      }
      False -> False
    }
  })
  
  result
}

/// Нормализует chat_id - убирает префикс -100 если есть
/// Bridge отправляет -5082217642, конфиг содержит -1005082217642
fn normalize_chat_id(chat_id: String) -> String {
  case string.starts_with(chat_id, "-100") {
    True -> "-" <> string.drop_start(chat_id, 4)
    False -> chat_id
  }
}

/// Находит конфигурацию чата по ID (с нормализацией)
pub fn find_chat_config(chat_id: String) -> Result(TriggerChatConfig, Nil) {
  let configs = get_trigger_chats()
  let normalized_id = normalize_chat_id(chat_id)

  io.println("[TRIGGER] Looking for chat: " <> chat_id <> " (normalized: " <> normalized_id <> ")")

  case list.find(configs, fn(config) {
    normalize_chat_id(config.chat_id) == normalized_id
  }) {
    Ok(config) -> {
      io.println("[TRIGGER] ✅ Found config for: " <> config.chat_name)
      Ok(config)
    }
    Error(_) -> {
      io.println("[TRIGGER] ❌ No config found for: " <> normalized_id)
      Error(Nil)
    }
  }
}

/// Проверяет, активен ли чат для триггеров
pub fn is_trigger_chat_active(chat_id: String) -> Bool {
  case find_chat_config(chat_id) {
    Ok(config) -> config.is_active
    Error(_) -> False
  }
}

/// Получает ID чата для пересылки
pub fn get_forward_chat_id(chat_id: String) -> Result(String, Nil) {
  case find_chat_config(chat_id) {
    Ok(config) -> Ok(config.forward_chat_id)
    Error(_) -> Error(Nil)
  }
}

/// Получает список ID всех активных trigger-чатов
pub fn get_trigger_chat_ids() -> List(String) {
  get_trigger_chats()
  |> list.filter(fn(c) { c.is_active })
  |> list.map(fn(c) { c.chat_id })
}

/// Проверяет, нужно ли отвечать на сообщение с триггером
pub fn should_respond_to_trigger(
  chat_id: String,
  message_text: String,
) -> Bool {
  case find_chat_config(chat_id) {
    Ok(config) -> {
      let trigger_count = list.length(config.custom_triggers)
      io.println("[TRIGGER] Checking " <> int.to_string(trigger_count) <> " triggers against message")
      let has_trigger = contains_trigger(message_text, config.custom_triggers)
      // Debug
      case has_trigger {
        True -> {
          io.println("[TRIGGER] ✅ MATCH! Trigger found in: " <> message_text)
          io.println("[TRIGGER] Chat is_active: " <> case config.is_active { True -> "YES" False -> "NO" })
        }
        False -> {
          io.println("[TRIGGER] ❌ NO MATCH in: " <> message_text)
        }
      }
      config.is_active
      && config.can_write
      && has_trigger
    }
    Error(_) -> {
      io.println("[TRIGGER_DEBUG] Config not found for chat: " <> chat_id)
      False
    }
  }
}
