// Telegram to Agent message mapper
// Converts Telegram updates to internal agent messages

import gleam/option.{type Option, None, Some}
import gleam/int
import vibee/types as core_types
import vibee/integrations/telegram/types as tg_types

/// Convert Telegram update to Agent message
pub fn telegram_update_to_agent_message(
  update: tg_types.TelegramUpdate,
) -> core_types.AgentMessage {
  case update {
    tg_types.NewMessage(chat_id, message_id, text, sender_id, _timestamp) -> {
      let agent_update = core_types.NewMessage(
        chat_id: chat_id,
        message_id: message_id,
        text: text,
        sender_id: sender_id,
        sender_name: "User " <> int.to_string(sender_id),
      )
      core_types.TgUpdate(agent_update)
    }
    tg_types.EditedMessage(chat_id, message_id, text, _timestamp) -> {
      let agent_update = core_types.EditedMessage(
        chat_id: chat_id,
        message_id: message_id,
        text: text,
      )
      core_types.TgUpdate(agent_update)
    }
    tg_types.DeletedMessage(chat_id, message_id, _timestamp) -> {
      let agent_update = core_types.DeletedMessage(
        chat_id: chat_id,
        message_id: message_id,
      )
      core_types.TgUpdate(agent_update)
    }
  }
}

/// Convert Telegram message to internal Message type
pub fn telegram_message_to_message(
  id: Int,
  from_name: String,
  text: String,
) -> core_types.Message {
  core_types.Message(
    id: int.to_string(id),
    sender: from_name,
    content: core_types.TextContent(text: text),
    timestamp: 0,
  )
}

/// Create a text message for sending
pub fn create_text_message(
  id: String,
  sender: String,
  text: String,
  timestamp: Int,
) -> core_types.Message {
  core_types.Message(
    id: id,
    sender: sender,
    content: core_types.TextContent(text: text),
    timestamp: timestamp,
  )
}

/// Extract text from message content
pub fn get_message_text(msg: core_types.Message) -> String {
  case msg.content {
    core_types.TextContent(text) -> text
    _ -> ""
  }
}

/// Check if update is a new message
pub fn is_new_message(update: tg_types.TelegramUpdate) -> Bool {
  case update {
    tg_types.NewMessage(_, _, _, _, _) -> True
    _ -> False
  }
}

/// Get chat ID from update
pub fn get_chat_id(update: tg_types.TelegramUpdate) -> Int {
  case update {
    tg_types.NewMessage(chat_id, _, _, _, _) -> chat_id
    tg_types.EditedMessage(chat_id, _, _, _) -> chat_id
    tg_types.DeletedMessage(chat_id, _, _) -> chat_id
  }
}

/// Get message text from update
pub fn get_update_text(update: tg_types.TelegramUpdate) -> Option(String) {
  case update {
    tg_types.NewMessage(_, _, text, _, _) -> Some(text)
    tg_types.EditedMessage(_, _, text, _) -> Some(text)
    tg_types.DeletedMessage(_, _, _) -> None
  }
}
