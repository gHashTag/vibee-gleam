// Target Chats Configuration for VIBEE Agent
// Аналог plugin-telegram-craft/config/targetChats.ts

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import vibee/config/dynamic_config
import vibee/db/postgres

/// Дефолтные целевые чаты (fallback если БД недоступна)
/// Форматы ID согласно Telegram API: https://core.telegram.org/api/bots/ids
/// - Users: положительный ID
/// - Basic Groups: отрицательный без -100 (напр. -5082217642)
/// - Supergroups/Channels: отрицательный с -100 (напр. -1002737186844)
const default_target_chats = [
  "693774948",       // User - личный чат для тестов
  "6579515876",      // User - vibee_agent bot
  "144022504",       // User - @neuro_sage E2E Rainbow Bridge
  "-1002737186844",  // Supergroup - VIBEE AGENT Lead group
  "-1002298297094",  // Supergroup - тестовый канал
  "-5082217642",     // Basic Group - Aimly.io dev (SNIPER MODE)
]

/// Целевые чаты для мониторинга и автоответов (из БД)
/// ID без префикса -100 (нормализованные)
pub fn target_chats() -> List(String) {
  case postgres.get_global_pool() {
    Some(pool) -> {
      let db_chats = dynamic_config.get_target_chats(pool)
      case db_chats {
        [] -> default_target_chats  // Если в БД пусто - используем дефолт
        chats -> chats
      }
    }
    None -> default_target_chats
  }
}

/// Информация о целевом чате
pub type TargetChat {
  TargetChat(
    id: String,
    name: String,
    chat_type: ChatType,
  )
}

/// Тип чата
pub type ChatType {
  Supergroup
  Group
  Channel
  Private
}

/// Получить все целевые чаты с информацией
/// Форматы ID согласно Telegram API: https://core.telegram.org/api/bots/ids
pub fn get_target_chats() -> List(TargetChat) {
  [
    TargetChat(id: "693774948", name: "Личный чат для тестов", chat_type: Private),
    TargetChat(id: "6579515876", name: "vibee_agent bot", chat_type: Private),
    TargetChat(id: "144022504", name: "neuro_sage E2E tester", chat_type: Private),
    TargetChat(id: "-1002737186844", name: "VIBEE AGENT Lead", chat_type: Supergroup),
    TargetChat(id: "-1002298297094", name: "Тестовый канал", chat_type: Supergroup),
    TargetChat(id: "-5082217642", name: "Aimly.io dev", chat_type: Group),
  ]
}

/// Проверяет, является ли chatId целевым чатом
/// Учитывает разные форматы ID (Bridge может вернуть без знака минус и без -100)
pub fn is_target_chat(chat_id: String) -> Bool {
  io.println("[TARGET_CHECK] Checking chat_id: " <> chat_id)
  let chats = target_chats()

  // Нормализуем входящий ID
  let normalized = normalize_chat_id(chat_id)
  io.println("[TARGET_CHECK] Normalized input: " <> normalized)

  // Проверяем совпадение с нормализованными значениями из конфига
  let result = list.any(chats, fn(config_id) {
    let config_normalized = normalize_chat_id(config_id)
    normalized == config_normalized
  })

  case result {
    True -> io.println("[TARGET_CHECK] ✅ Match found!")
    False -> io.println("[TARGET_CHECK] ❌ Not in target_chats list")
  }
  result
}

/// Нормализует chat_id к абсолютному значению (только цифры)
/// Bridge может возвращать:
///   - 5082217642 (обычная группа без знака)
///   - 2298297094 (supergroup без -100 префикса)
/// Config содержит:
///   - -5082217642 (обычная группа)
///   - -1002298297094 (supergroup с -100 префиксом)
pub fn normalize_chat_id(chat_id: String) -> String {
  // Убираем знак минус если есть
  let without_sign = case string.starts_with(chat_id, "-") {
    True -> string.drop_start(chat_id, 1)
    False -> chat_id
  }
  // Убираем префикс 100 если есть (для supergroups)
  case string.starts_with(without_sign, "100") {
    True -> string.drop_start(without_sign, 3)
    False -> without_sign
  }
}

/// Проверяет, является ли чат личным (не группа)
/// Личные чаты имеют положительные ID
/// Группы и каналы имеют отрицательные ID (начинаются с -100)
pub fn is_private_chat(chat_id: String) -> Bool {
  case int.parse(chat_id) {
    Ok(id) -> id > 0  // Положительный ID = личный чат
    Error(_) -> False
  }
}

/// Owner ID (Digital Twin owner) - исключаем self-chat
/// Берем из dynamic_config с fallback
fn get_owner_id() -> String {
  case postgres.get_global_pool() {
    Some(pool) -> int.to_string(dynamic_config.get_owner_id(pool))
    None -> "144022504"  // fallback
  }
}

/// Проверяет, нужно ли обрабатывать сообщение из этого чата
/// В Digital Twin режиме: целевые чаты + ВСЕ личные чаты
/// В обычном режиме: ТОЛЬКО целевые чаты
///
/// NOTE: Удалена проверка на owner_id == chat_id, т.к. она блокировала
/// сообщения от владельца когда он пишет боту напрямую.
/// Self-chat (Saved Messages) имеет chat_id = bot_user_id, а не owner_id.
pub fn should_process_chat(chat_id: String) -> Bool {
  // Проверяем целевые чаты
  case is_target_chat(chat_id) {
    True -> True  // Целевой чат - всегда обрабатываем
    False -> {
      // Не целевой чат - проверяем, личный ли он
      // В Digital Twin режиме обрабатываем ВСЕ личные чаты
      is_private_chat(chat_id)
    }
  }
}

/// Проверяет, нужно ли обрабатывать чат (с явным флагом Digital Twin)
/// NOTE: Удалена проверка owner_id - она блокировала сообщения от владельца
pub fn should_process_chat_with_mode(chat_id: String, digital_twin_enabled: Bool) -> Bool {
  case is_target_chat(chat_id) {
    True -> {
      io.println("[FILTER] ✅ Target chat: " <> chat_id)
      True
    }
    False -> {
      // Личные чаты обрабатываем только в Digital Twin режиме
      let is_private = is_private_chat(chat_id)
      case digital_twin_enabled && is_private {
        True -> {
          io.println("[FILTER] ✅ Private chat (Digital Twin ON): " <> chat_id)
          True
        }
        False -> {
          io.println("[FILTER] ⏭️  Skipping chat: " <> chat_id <> " (private=" <> case is_private { True -> "YES" False -> "NO" } <> ", twin=" <> case digital_twin_enabled { True -> "ON" False -> "OFF" } <> ")")
          False
        }
      }
    }
  }
}

/// Добавить целевой чат в runtime (для динамического добавления)
/// Возвращает обновленный список
pub fn add_target_chat(chats: List(String), new_id: String) -> List(String) {
  case list.contains(chats, new_id) {
    True -> chats
    False -> [new_id, ..chats]
  }
}

/// Удалить целевой чат
pub fn remove_target_chat(chats: List(String), id: String) -> List(String) {
  list.filter(chats, fn(chat_id) { chat_id != id })
}
