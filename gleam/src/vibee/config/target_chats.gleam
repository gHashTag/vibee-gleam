// Target Chats Configuration for VIBEE Agent
// Аналог plugin-telegram-craft/config/targetChats.ts

import gleam/int
import gleam/list
import gleam/string

/// Целевые чаты для мониторинга и автоответов
/// ID без префикса -100 (нормализованные)
pub const target_chats = [
  "693774948",    // Личный чат для тестов
  "2737186844",   // VIBEE AGENT (supergroup, -1002737186844)
  "6579515876",   // vibee_agent bot для тестов
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
pub fn normalize_chat_id(chat_id: String) -> String {
  case string.starts_with(chat_id, "-100") {
    True -> string.drop_start(chat_id, 4)
    False -> {
      case string.starts_with(chat_id, "-") {
        True -> string.drop_start(chat_id, 1)
        False -> chat_id
      }
    }
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
/// ТОЛЬКО целевые чаты - для безопасности от бана Telegram
pub fn should_process_chat(chat_id: String) -> Bool {
  // Исключаем self-chat (Saved Messages) - когда chat_id == owner_id
  case chat_id == owner_id {
    True -> False  // Self-chat - не обрабатываем
    False -> {
      // ТОЛЬКО целевые чаты - не все личные!
      is_target_chat(chat_id)
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
