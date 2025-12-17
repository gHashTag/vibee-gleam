// Telegram types for VIBEE
// These types map to the Go telegram-bridge API

import gleam/option.{type Option}

/// Configuration for Telegram bridge connection
pub type TelegramConfig {
  TelegramConfig(
    base_url: String,
    app_id: Int,
    app_hash: String,
    phone: Option(String),
  )
}

/// Session information returned after connect
pub type SessionInfo {
  SessionInfo(
    session_id: String,
    authorized: Bool,
    message: String,
  )
}

/// Telegram user information
pub type TelegramUser {
  TelegramUser(
    id: Int,
    first_name: String,
    last_name: Option(String),
    username: Option(String),
    phone: Option(String),
  )
}

/// Telegram dialog/chat
pub type TelegramDialog {
  TelegramDialog(
    id: Int,
    title: String,
    dialog_type: DialogType,
    unread_count: Int,
    last_message: Option(String),
  )
}

/// Type of dialog
pub type DialogType {
  UserDialog
  GroupDialog
  SupergroupDialog
  ChannelDialog
}

/// Telegram message
pub type TelegramMessage {
  TelegramMessage(
    id: Int,
    text: String,
    from_id: Int,
    from_name: String,
    date: String,
    reply_to_id: Option(Int),
  )
}

/// Update from Telegram (received via WebSocket)
pub type TelegramUpdate {
  NewMessage(
    chat_id: Int,
    message_id: Int,
    text: String,
    sender_id: Int,
    timestamp: Int,
  )
  EditedMessage(
    chat_id: Int,
    message_id: Int,
    text: String,
    timestamp: Int,
  )
  DeletedMessage(
    chat_id: Int,
    message_id: Int,
    timestamp: Int,
  )
}

/// Send message request
pub type SendMessageRequest {
  SendMessageRequest(
    chat_id: Int,
    text: String,
    reply_to: Option(Int),
  )
}

/// Send message result
pub type SendMessageResult {
  SendMessageResult(
    success: Bool,
    message_id: Int,
  )
}

/// Auth state during login flow
pub type AuthState {
  NotConnected
  Connected(session_id: String)
  CodeSent(session_id: String, code_hash: String)
  TwoFactorRequired(session_id: String)
  Authenticated(session_id: String, user: TelegramUser)
}

/// Errors from Telegram bridge
pub type TelegramError {
  ConnectionError(message: String)
  AuthError(message: String)
  ApiError(code: Int, message: String)
  NetworkError(message: String)
  InvalidSession
  NotAuthorized
}
