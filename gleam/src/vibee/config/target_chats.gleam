// Target Chats Configuration for VIBEE Agent
// Аналог plugin-telegram-craft/config/targetChats.ts

import gleam/int
import gleam/list
import gleam/string

/// Целевые чаты для мониторинга и автоответов
/// ID без префикса -100 (нормализованные)
pub const target_chats = [
  "693774948",    // Личный чат для тестов
  "2737186844",   // VIBEE AGENT (supergroup, -1002737186844) - Lead group
  "6579515876",   // vibee_agent bot для тестов
  "-5082217642",  // Aimly.io dev (group) - SNIPER MODE trigger chat
]

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
pub fn get_target_chats() -> List(TargetChat) {
  [
    TargetChat(id: "693774948", name: "Личный чат для тестов", chat_type: Private),
    TargetChat(id: "2737186844", name: "VIBEE AGENT", chat_type: Supergroup),
    TargetChat(id: "6579515876", name: "vibee_agent bot", chat_type: Private),
    TargetChat(id: "-5082217642", name: "Aimly.io dev", chat_type: Group),
  ]
}

/// Проверяет, является ли chatId целевым чатом
/// Учитывает разные форматы ID (с и без префикса -100)
pub fn is_target_chat(chat_id: String) -> Bool {
  // Прямое совпадение
  case list.contains(target_chats, chat_id) {
    True -> True
    False -> {
      // Нормализация: убираем -100 префикс
      let normalized = normalize_chat_id(chat_id)
      list.contains(target_chats, normalized)
    }
  }
}

/// Нормализует chat ID - убирает -100 префикс если есть
/// НЕ убирает обычный минус (для групп типа -5082217642)
pub fn normalize_chat_id(chat_id: String) -> String {
  case string.starts_with(chat_id, "-100") {
    True -> string.drop_start(chat_id, 4)
    False -> chat_id  // Оставляем как есть, включая обычный минус
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
const owner_id = "144022504"

/// Проверяет, нужно ли обрабатывать сообщение из этого чата
/// В Digital Twin режиме: целевые чаты + ВСЕ личные чаты (кроме self-chat)
/// В обычном режиме: ТОЛЬКО целевые чаты
pub fn should_process_chat(chat_id: String) -> Bool {
  // Исключаем self-chat (Saved Messages) - когда chat_id == owner_id
  case chat_id == owner_id {
    True -> False  // Self-chat - не обрабатываем
    False -> {
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
  }
}

/// Проверяет, нужно ли обрабатывать чат (с явным флагом Digital Twin)
pub fn should_process_chat_with_mode(chat_id: String, digital_twin_enabled: Bool) -> Bool {
  case chat_id == owner_id {
    True -> {
      // io.println("[FILTER] Skipping self-chat: " <> chat_id)
      False
    }
    False -> {
      case is_target_chat(chat_id) {
        True -> {
          // io.println("[FILTER] ✅ Target chat: " <> chat_id)
          True
        }
        False -> {
          // Личные чаты обрабатываем только в Digital Twin режиме
          let is_private = is_private_chat(chat_id)
          case digital_twin_enabled && is_private {
            True -> {
              // io.println("[FILTER] ✅ Private chat (Digital Twin ON): " <> chat_id)
              True
            }
            False -> {
              // io.println("[FILTER] ⏭️  Skipping chat: " <> chat_id <> " (private=" <> case is_private { True -> "YES" False -> "NO" } <> ", twin=" <> case digital_twin_enabled { True -> "ON" False -> "OFF" } <> ")")
              False
            }
          }
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
