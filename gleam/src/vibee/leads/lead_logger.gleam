// Lead Logger
// Логирование лидов (временно без PostgreSQL)

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option}
import gleam/string

/// Сохранить лид (пока только логирование)
pub fn save_lead(
  telegram_user_id: Int,
  username: Option(String),
  first_name: Option(String),
  last_name: Option(String),
  message_text: String,
  source_chat_id: Int,
  source_chat_name: String,
  trigger_words: List(String),
  agent_response: String,
) -> Result(Int, String) {
  io.println("\n" <> "=" <> string.repeat("=", 60))
  io.println("🔥 НОВЫЙ ЛИД СОХРАНЁН")
  io.println("=" <> string.repeat("=", 60))
  
  io.println("\n📱 КОНТАКТНАЯ ИНФОРМАЦИЯ:")
  io.println("  Telegram ID: " <> int.to_string(telegram_user_id))
  io.println("  Username: " <> option.unwrap(username, "не указан"))
  io.println("  Имя: " <> option.unwrap(first_name, "не указано"))
  io.println("  Фамилия: " <> option.unwrap(last_name, "не указана"))
  
  io.println("\n💬 ПЕРВОЕ СООБЩЕНИЕ:")
  io.println("  " <> message_text)
  
  io.println("\n📊 АНАЛИЗ:")
  let intent = detect_intent(trigger_words)
  io.println("  Намерение: " <> intent_to_russian(intent))
  
  let crypto = detect_crypto_interest(message_text, trigger_words)
  io.println("  Интересует: " <> string.join(crypto, ", "))
  
  let priority = detect_priority(message_text, trigger_words)
  io.println("  Приоритет: " <> priority_to_russian(priority))
  
  io.println("\n🎯 ТРИГГЕРЫ:")
  list.each(trigger_words, fn(trigger) {
    io.println("  • " <> trigger)
  })
  
  io.println("\n✅ ОТВЕТ АГЕНТА:")
  io.println("  " <> agent_response)
  
  io.println("\n📍 ИСТОЧНИК:")
  io.println("  Чат: " <> source_chat_name)
  io.println("  ID: " <> int.to_string(source_chat_id))
  
  io.println("\n" <> "=" <> string.repeat("=", 60) <> "\n")
  
  Ok(1)
}

fn detect_intent(triggers: List(String)) -> String {
  let lower_triggers = list.map(triggers, string.lowercase)
  
  case list.any(lower_triggers, fn(t) {
    string.contains(t, "куплю") || string.contains(t, "купить")
  }) {
    True -> "buy"
    False ->
      case list.any(lower_triggers, fn(t) {
        string.contains(t, "продам") || string.contains(t, "продать")
      }) {
        True -> "sell"
        False ->
          case list.any(lower_triggers, fn(t) {
            string.contains(t, "обмен") || string.contains(t, "обменять")
          }) {
            True -> "exchange"
            False -> "info"
          }
      }
  }
}

fn detect_crypto_interest(
  message: String,
  triggers: List(String),
) -> List(String) {
  let lower_text = string.lowercase(message <> " " <> string.join(triggers, " "))
  let mut_interest = []
  
  let interest = case string.contains(lower_text, "биткоин") || string.contains(
    lower_text,
    "bitcoin",
  ) || string.contains(lower_text, "btc") {
    True -> ["Bitcoin", ..mut_interest]
    False -> mut_interest
  }
  
  let interest = case string.contains(lower_text, "usdt") || string.contains(
    lower_text,
    "тезер",
  ) || string.contains(lower_text, "tether") {
    True -> ["USDT", ..interest]
    False -> interest
  }
  
  let interest = case string.contains(lower_text, "эфир") || string.contains(
    lower_text,
    "ethereum",
  ) || string.contains(lower_text, "eth") {
    True -> ["Ethereum", ..interest]
    False -> interest
  }
  
  case list.length(interest) {
    0 -> ["Криптовалюта (общее)"]
    _ -> interest
  }
}

fn detect_priority(message: String, triggers: List(String)) -> String {
  let lower_text = string.lowercase(message <> " " <> string.join(triggers, " "))
  
  case string.contains(lower_text, "срочно") || string.contains(
    lower_text,
    "быстро",
  ) || string.contains(lower_text, "сейчас") {
    True -> "urgent"
    False ->
      case string.contains(lower_text, "много") || string.contains(
        lower_text,
        "большую",
      ) {
        True -> "high"
        False -> "medium"
      }
  }
}

fn intent_to_russian(intent: String) -> String {
  case intent {
    "buy" -> "Покупка"
    "sell" -> "Продажа"
    "exchange" -> "Обмен"
    _ -> "Информация"
  }
}

fn priority_to_russian(priority: String) -> String {
  case priority {
    "urgent" -> "🔴 Срочный"
    "high" -> "🟠 Высокий"
    "medium" -> "🟡 Средний"
    _ -> "🟢 Низкий"
  }
}
