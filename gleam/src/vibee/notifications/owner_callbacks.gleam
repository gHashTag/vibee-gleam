// Owner Callbacks - Обработка inline кнопок из уведомлений owner
// Формат callback_data: owner:<action>:<chat_id>:<extra>

import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import pog
import vibee/config/chat_permissions
import vibee/db/postgres
import vibee/vibe_logger
import gleam/json

// =============================================================================
// Types
// =============================================================================

/// Результат обработки callback
pub type CallbackResult {
  CallbackSuccess(text: String, show_alert: Bool)
  CallbackError(text: String)
  CallbackUrl(url: String)
}

/// Распарсенный callback
pub type OwnerCallback {
  AllowChat(chat_id: Int)
  BlockChat(chat_id: Int)
  MuteChat(chat_id: Int, duration: String)
  ReplyToChat(chat_id: Int, message_id: Int)
  ViewLead(user_id: Int)
  ContactUser(user_id: Int)
  ViewLogs
  RetryLast
}

// =============================================================================
// Public API
// =============================================================================

/// Проверить является ли callback_data owner callback
pub fn is_owner_callback(callback_data: String) -> Bool {
  string.starts_with(callback_data, "owner:")
}

/// Обработать owner callback
pub fn handle(callback_data: String) -> CallbackResult {
  let log = vibe_logger.new("owner_callback")
    |> vibe_logger.with_data("callback_data", json.string(callback_data))

  case parse_callback(callback_data) {
    Error(_) -> {
      vibe_logger.warn(log, "Failed to parse callback")
      CallbackError("Invalid callback format")
    }
    Ok(callback) -> {
      vibe_logger.info(log, "Processing owner callback")
      process_callback(callback)
    }
  }
}

// =============================================================================
// Callback Processing
// =============================================================================

fn process_callback(callback: OwnerCallback) -> CallbackResult {
  case postgres.get_global_pool() {
    None -> CallbackError("Database unavailable")
    Some(pool) -> process_with_pool(pool, callback)
  }
}

fn process_with_pool(pool: pog.Connection, callback: OwnerCallback) -> CallbackResult {
  case callback {
    AllowChat(chat_id) -> {
      // Выдаём права User
      let owner_id = 144_022_504
      case chat_permissions.grant_permission(pool, chat_id, chat_permissions.User, owner_id, None) {
        Ok(_) -> CallbackSuccess("✅ Чат разрешён! Агент теперь будет отвечать.", True)
        Error(_) -> CallbackError("Не удалось выдать права")
      }
    }

    BlockChat(chat_id) -> {
      // Блокируем чат
      let owner_id = 144_022_504
      case chat_permissions.revoke_permission(pool, chat_id, owner_id) {
        Ok(_) -> CallbackSuccess("🚫 Чат заблокирован!", True)
        Error(_) -> CallbackError("Не удалось заблокировать")
      }
    }

    MuteChat(chat_id, duration) -> {
      // Временно отключаем уведомления от этого чата
      let _ = mute_notifications_for_chat(pool, chat_id, duration)
      CallbackSuccess("🔇 Уведомления отключены на " <> duration, False)
    }

    ReplyToChat(chat_id, message_id) -> {
      // Формируем deep link для открытия конкретного сообщения
      let url = make_chat_link(chat_id, message_id)
      CallbackUrl(url)
    }

    ViewLead(user_id) -> {
      // TODO: Показать информацию о лиде
      CallbackSuccess("👁 Lead ID: " <> int.to_string(user_id), False)
    }

    ContactUser(user_id) -> {
      // Формируем deep link для контакта
      let url = "tg://user?id=" <> int.to_string(user_id)
      CallbackUrl(url)
    }

    ViewLogs -> {
      // TODO: Показать последние логи
      CallbackSuccess("📋 Открываю логи...", False)
    }

    RetryLast -> {
      // TODO: Повторить последнюю операцию
      CallbackSuccess("🔄 Повторяю...", False)
    }
  }
}

// =============================================================================
// Parsing
// =============================================================================

fn parse_callback(data: String) -> Result(OwnerCallback, Nil) {
  case string.split(data, ":") {
    ["owner", "allow", chat_id_str] -> {
      case int.parse(chat_id_str) {
        Ok(chat_id) -> Ok(AllowChat(chat_id))
        Error(_) -> Error(Nil)
      }
    }
    ["owner", "block", chat_id_str] -> {
      case int.parse(chat_id_str) {
        Ok(chat_id) -> Ok(BlockChat(chat_id))
        Error(_) -> Error(Nil)
      }
    }
    ["owner", "mute", chat_id_str, duration] -> {
      case int.parse(chat_id_str) {
        Ok(chat_id) -> Ok(MuteChat(chat_id, duration))
        Error(_) -> Error(Nil)
      }
    }
    ["owner", "reply", chat_id_str, message_id_str] -> {
      case int.parse(chat_id_str), int.parse(message_id_str) {
        Ok(chat_id), Ok(message_id) -> Ok(ReplyToChat(chat_id, message_id))
        _, _ -> Error(Nil)
      }
    }
    ["owner", "view", user_id_str] -> {
      case int.parse(user_id_str) {
        Ok(user_id) -> Ok(ViewLead(user_id))
        Error(_) -> Error(Nil)
      }
    }
    ["owner", "contact", user_id_str] -> {
      case int.parse(user_id_str) {
        Ok(user_id) -> Ok(ContactUser(user_id))
        Error(_) -> Error(Nil)
      }
    }
    ["owner", "logs", "recent"] -> Ok(ViewLogs)
    ["owner", "retry", "last"] -> Ok(RetryLast)
    _ -> Error(Nil)
  }
}

// =============================================================================
// Helpers
// =============================================================================

fn mute_notifications_for_chat(pool: pog.Connection, chat_id: Int, duration: String) -> Result(Nil, Nil) {
  // Рассчитываем время до которого mute
  let hours = case duration {
    "1h" -> 1
    "4h" -> 4
    "24h" -> 24
    _ -> 1
  }

  let sql = "UPDATE owner_notification_settings
             SET muted_chats = array_append(
               array_remove(muted_chats, $1), $1
             )
             WHERE owner_id = 144022504"

  case pog.query(sql)
    |> pog.parameter(pog.int(chat_id))
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error(Nil)
  }
  |> result.replace(Nil)
}

fn make_chat_link(chat_id: Int, message_id: Int) -> String {
  case chat_id > 0 {
    // Private chat - use tg://openmessage для конкретного сообщения
    True -> "tg://openmessage?user_id=" <> int.to_string(chat_id) <> "&message_id=" <> int.to_string(message_id)
    // Group/Supergroup - use t.me/c/{channel_id}/{message_id}
    False -> {
      let abs_id = int.absolute_value(chat_id)
      // Remove -100 prefix for supergroups
      let channel_id = case abs_id > 1_000_000_000_000 {
        True -> abs_id - 1_000_000_000_000
        False -> abs_id
      }
      "https://t.me/c/" <> int.to_string(channel_id) <> "/" <> int.to_string(message_id)
    }
  }
}
