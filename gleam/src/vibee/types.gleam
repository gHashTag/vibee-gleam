// Core types for VIBEE Agent Framework

import gleam/option.{type Option}

/// Tone of agent responses
pub type Tone {
  Friendly
  Professional
  Casual
  CustomTone(String)
}

/// Language of agent communication
pub type Language {
  En
  Ru
  CustomLanguage(String)
}

/// Message content types
pub type Content {
  TextContent(text: String)
  MediaContent(url: String, caption: Option(String))
}

/// A message in the conversation history
pub type Message {
  Message(
    id: String,
    sender: String,
    content: Content,
    timestamp: Int,
  )
}

/// Configuration for creating an agent
pub type AgentConfig {
  AgentConfig(
    id: String,
    name: String,
    tone: Tone,
    language: Language,
    system_prompt: Option(String),
    history_limit: Int,
  )
}

/// State of a running agent
pub type AgentState {
  AgentState(
    id: String,
    name: String,
    tone: Tone,
    language: Language,
    history: List(Message),
    history_limit: Int,
    system_prompt: Option(String),
  )
}

/// Reference to a running agent (pid stored as String for simplicity)
pub type AgentRef {
  AgentRef(id: String, pid_ref: String)
}

/// Telegram update types
pub type TelegramUpdate {
  NewMessage(
    chat_id: Int,
    message_id: Int,
    text: String,
    sender_id: Int,
    sender_name: String,
  )
  EditedMessage(chat_id: Int, message_id: Int, text: String)
  DeletedMessage(chat_id: Int, message_id: Int)
}

/// Messages that agents can receive
pub type AgentMessage {
  /// Incoming Telegram update
  TgUpdate(update: TelegramUpdate)
  /// Process text internally
  ProcessText(request_id: String, text: String)
  /// Reply to a message
  Reply(request_id: String, text: String)
  /// Save current state
  SaveState
  /// Graceful shutdown
  Shutdown
}

/// Result of sending a Telegram message
pub type TelegramResult {
  MessageSent(message_id: Int)
  MessageFailed(error: String)
}

