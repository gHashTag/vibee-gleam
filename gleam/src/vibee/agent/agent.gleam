// Agent Actor - individual agent implementation
// Updated for gleam_otp 1.2.0 builder API

import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/list
import gleam/io
import gleam/option.{type Option, None, Some}
import vibee/types.{
  type AgentConfig, type AgentMessage, type AgentState, type Message,
  type TelegramUpdate, AgentState, Message, ProcessText, Reply, SaveState,
  Shutdown, TgUpdate, NewMessage, TextContent,
}
import vibee/error.{type VibeeError}
import vibee/agent/persistence

/// Start an agent actor using new builder API
/// Returns Started with data being Subject(AgentMessage)
pub fn start(config: AgentConfig) -> Result(actor.Started(Subject(AgentMessage)), actor.StartError) {
  // Try to load existing state from database
  let initial_state = case persistence.load_state(config.id) {
    Ok(saved_state) -> {
      io.println("[" <> config.name <> "] Loaded saved state")
      // Reset messages_since_save counter
      AgentState(..saved_state, messages_since_save: 0)
    }
    Error(_) -> {
      io.println("[" <> config.name <> "] Creating new state")
      AgentState(
        id: config.id,
        name: config.name,
        tone: config.tone,
        language: config.language,
        history: [],
        history_limit: config.history_limit,
        system_prompt: config.system_prompt,
        messages_since_save: 0,
      )
    }
  }

  actor.new(initial_state)
  |> actor.on_message(handle_message)
  |> actor.start
}

/// Handle agent messages
fn handle_message(
  state: AgentState,
  message: AgentMessage,
) -> actor.Next(AgentState, AgentMessage) {
  case message {
    TgUpdate(update) -> {
      handle_telegram_update(update, state)
    }

    ProcessText(_request_id, text) -> {
      io.println("[" <> state.name <> "] Processing: " <> text)
      // TODO: Process text with LLM
      actor.continue(state)
    }

    Reply(_request_id, text) -> {
      io.println("[" <> state.name <> "] Reply: " <> text)
      // TODO: Send reply via Telegram bridge
      actor.continue(state)
    }

    SaveState -> {
      io.println("[" <> state.name <> "] Saving state...")
      
      // Persist state to PostgreSQL
      let new_state = case persistence.save_state(state) {
        Ok(_) -> {
          io.println("[" <> state.name <> "] State saved successfully")
          // Reset counter after successful save
          AgentState(..state, messages_since_save: 0)
        }
        Error(err) -> {
          io.println("[" <> state.name <> "] Failed to save state: " <> err)
          // Record error for monitoring
          case persistence.record_error(state.id, "save_state_failed", err, None, None) {
            Ok(_) -> Nil
            Error(_) -> Nil
          }
          state
        }
      }
      
      actor.continue(new_state)
    }

    Shutdown -> {
      io.println("[" <> state.name <> "] Shutting down...")
      
      // Save state before shutdown
      case persistence.save_state(state) {
        Ok(_) -> Nil
        Error(_) -> Nil
      }
      
      actor.stop()
    }
  }
}

/// Handle incoming Telegram updates
fn handle_telegram_update(
  update: TelegramUpdate,
  state: AgentState,
) -> actor.Next(AgentState, AgentMessage) {
  case update {
    NewMessage(_chat_id, message_id, text, _sender_id, sender_name) -> {
      io.println(
        "[" <> state.name <> "] New message from " <> sender_name <> ": " <> text,
      )

      // Add message to history
      let msg =
        Message(
          id: int_to_string(message_id),
          sender: sender_name,
          content: TextContent(text),
          timestamp: 0,
        )

      let new_history = add_to_history(state.history, msg, state.history_limit)
      let messages_count = state.messages_since_save + 1
      let new_state = AgentState(..state, history: new_history, messages_since_save: messages_count)

      // Record message received metric
      case persistence.record_metric(state.id, "message_received", 1.0, None) {
        Ok(_) -> Nil
        Error(_) -> Nil
      }

      // Auto-save every 5 messages
      let auto_save_threshold = 5
      let final_state = case messages_count >= auto_save_threshold {
        True -> {
          io.println("[" <> state.name <> "] Auto-saving state (threshold reached)")
          case persistence.save_state(new_state) {
            Ok(_) -> {
              io.println("[" <> state.name <> "] Auto-save successful")
              // Record auto-save metric
              case persistence.record_metric(state.id, "auto_save", 1.0, None) {
                Ok(_) -> Nil
                Error(_) -> Nil
              }
              AgentState(..new_state, messages_since_save: 0)
            }
            Error(err) -> {
              io.println("[" <> state.name <> "] Auto-save failed: " <> err)
              new_state
            }
          }
        }
        False -> new_state
      }

      // TODO: Generate response using LLM + RAG
      // TODO: Send response via Telegram bridge

      actor.continue(final_state)
    }

    _ -> {
      // Handle edited/deleted messages later
      actor.continue(state)
    }
  }
}

/// Add message to history, respecting the limit
fn add_to_history(
  history: List(Message),
  message: Message,
  limit: Int,
) -> List(Message) {
  let new_history = [message, ..history]
  list.take(new_history, limit)
}

/// Send a message to an agent using its subject
pub fn send(subject: Subject(AgentMessage), message: AgentMessage) -> Nil {
  process.send(subject, message)
}

/// Request agent to save state
pub fn save_state(subject: Subject(AgentMessage)) -> Nil {
  process.send(subject, SaveState)
}

/// Request agent to shutdown
pub fn shutdown(subject: Subject(AgentMessage)) -> Nil {
  process.send(subject, Shutdown)
}

/// Helper to convert int to string
fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ -> do_int_to_string(n, "")
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        9 -> "9"
        _ -> "?"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}
