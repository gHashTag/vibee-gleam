// Conversation Tracker
// ETS-based tracking of active conversations for proactive mode
// Tracks when agent last responded to determine if conversation is "active"

import gleam/int
import gleam/io
import gleam/json
import gleam/option.{type Option, None, Some}
import vibee/vibe_logger

/// Active conversation entry
pub type ActiveConversation {
  ActiveConversation(
    chat_id: String,
    user_id: Int,
    last_agent_message_id: Int,
    last_agent_message_time: Int,
    last_user_message_time: Int,
    messages_count: Int,
    state: ConversationState,
  )
}

/// Conversation state
pub type ConversationState {
  Active      // Agent responded recently, conversation ongoing
  Waiting     // Agent responded, waiting for user reply
  Closed      // Timed out or ended
}

/// Default conversation timeout: 5 minutes (300 seconds)
const default_timeout_seconds = 300

/// Initialize the ETS table
pub fn init() {
  ffi_init()
}

/// Record that agent sent a message (starts/continues conversation)
pub fn agent_responded(
  chat_id: String,
  user_id: Int,
  message_id: Int,
  timestamp: Int,
) {
  let log = vibe_logger.new("conv_tracker")
    |> vibe_logger.with_data("chat_id", json.string(chat_id))
    |> vibe_logger.with_data("user_id", json.int(user_id))
    |> vibe_logger.with_data("msg_id", json.int(message_id))

  case get_conversation(chat_id, user_id) {
    Some(conv) -> {
      // Update existing conversation
      let updated = ActiveConversation(
        ..conv,
        last_agent_message_id: message_id,
        last_agent_message_time: timestamp,
        messages_count: conv.messages_count + 1,
        state: Waiting,
      )
      ffi_upsert(chat_id, user_id, updated)
      vibe_logger.debug(log, "Conversation updated - agent responded")
    }
    None -> {
      // Start new conversation
      let new_conv = ActiveConversation(
        chat_id: chat_id,
        user_id: user_id,
        last_agent_message_id: message_id,
        last_agent_message_time: timestamp,
        last_user_message_time: 0,
        messages_count: 1,
        state: Waiting,
      )
      ffi_upsert(chat_id, user_id, new_conv)
      vibe_logger.info(log, "New conversation started")
    }
  }
}

/// Record that user sent a message
pub fn user_messaged(
  chat_id: String,
  user_id: Int,
  timestamp: Int,
) {
  case get_conversation(chat_id, user_id) {
    Some(conv) -> {
      let updated = ActiveConversation(
        ..conv,
        last_user_message_time: timestamp,
        state: Active,
      )
      ffi_upsert(chat_id, user_id, updated)
    }
    None -> Nil  // No active conversation
  }
}

/// Check if conversation is active (within timeout window)
pub fn is_active_conversation(
  chat_id: String,
  user_id: Int,
  current_time: Int,
) -> Bool {
  is_active_with_timeout(chat_id, user_id, current_time, default_timeout_seconds)
}

/// Check if conversation is active with custom timeout
pub fn is_active_with_timeout(
  chat_id: String,
  user_id: Int,
  current_time: Int,
  timeout_seconds: Int,
) -> Bool {
  case get_conversation(chat_id, user_id) {
    Some(conv) -> {
      let time_since_agent = current_time - conv.last_agent_message_time
      case time_since_agent < timeout_seconds {
        True -> {
          io.println("[CONV_TRACKER] Active conversation: " <> chat_id <> " user=" <> int.to_string(user_id) <> " (agent responded " <> int.to_string(time_since_agent) <> "s ago)")
          True
        }
        False -> {
          // Conversation timed out - close it
          close_conversation(chat_id, user_id)
          False
        }
      }
    }
    None -> False
  }
}

/// Check if message is a reply to agent's last message
pub fn is_reply_to_agent(
  chat_id: String,
  user_id: Int,
  reply_to_id: Int,
) -> Bool {
  case reply_to_id > 0 {
    False -> False
    True -> {
      case get_conversation(chat_id, user_id) {
        Some(conv) -> {
          let is_reply = reply_to_id == conv.last_agent_message_id
          case is_reply {
            True -> io.println("[CONV_TRACKER] Reply to agent detected! chat=" <> chat_id <> " reply_to=" <> int.to_string(reply_to_id))
            False -> Nil
          }
          is_reply
        }
        None -> False
      }
    }
  }
}

/// Get conversation details
pub fn get_conversation(chat_id: String, user_id: Int) -> Option(ActiveConversation) {
  ffi_get(chat_id, user_id)
}

/// Close a conversation (mark as ended)
pub fn close_conversation(chat_id: String, user_id: Int) {
  let log = vibe_logger.new("conv_tracker")
    |> vibe_logger.with_data("chat_id", json.string(chat_id))
    |> vibe_logger.with_data("user_id", json.int(user_id))
  vibe_logger.debug(log, "Conversation closed (timeout)")
  ffi_delete(chat_id, user_id)
}

/// Get all active conversations (for debugging)
pub fn list_active() -> List(ActiveConversation) {
  ffi_list_all()
}

/// Clean up stale conversations (older than timeout)
pub fn cleanup_stale(current_time: Int) {
  cleanup_stale_with_timeout(current_time, default_timeout_seconds)
}

pub fn cleanup_stale_with_timeout(current_time: Int, timeout_seconds: Int) {
  let all = list_active()
  let log = vibe_logger.new("conv_tracker")

  case all {
    [] -> Nil
    _ -> {
      vibe_logger.debug(log
        |> vibe_logger.with_data("count", json.int(list_length(all))),
        "Cleaning up stale conversations")
      do_cleanup(all, current_time, timeout_seconds)
    }
  }
}

fn do_cleanup(convs: List(ActiveConversation), current_time: Int, timeout_seconds: Int) {
  case convs {
    [] -> Nil
    [conv, ..rest] -> {
      let time_since = current_time - conv.last_agent_message_time
      case time_since > timeout_seconds {
        True -> close_conversation(conv.chat_id, conv.user_id)
        False -> Nil
      }
      do_cleanup(rest, current_time, timeout_seconds)
    }
  }
}

fn list_length(l: List(a)) -> Int {
  case l {
    [] -> 0
    [_, ..rest] -> 1 + list_length(rest)
  }
}

// FFI functions (Erlang ETS)
@external(erlang, "vibee_conversation_tracker_ffi", "init")
fn ffi_init() -> Nil

@external(erlang, "vibee_conversation_tracker_ffi", "upsert")
fn ffi_upsert(chat_id: String, user_id: Int, conv: ActiveConversation) -> Nil

@external(erlang, "vibee_conversation_tracker_ffi", "get")
fn ffi_get(chat_id: String, user_id: Int) -> Option(ActiveConversation)

@external(erlang, "vibee_conversation_tracker_ffi", "delete")
fn ffi_delete(chat_id: String, user_id: Int) -> Nil

@external(erlang, "vibee_conversation_tracker_ffi", "list_all")
fn ffi_list_all() -> List(ActiveConversation)
