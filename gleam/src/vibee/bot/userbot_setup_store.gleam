// User-Bot Setup Progress Store
// CRUD operations for userbot_setup_progress table
// Stores wizard state for multi-step user-bot onboarding

import gleam/dynamic/decode
import gleam/int
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/string
import pog
import vibee/db/postgres

// =============================================================================
// Types
// =============================================================================

pub type SetupProgress {
  SetupProgress(
    user_id: Int,
    current_step: String,
    phone: Option(String),
    bridge_session_id: Option(String),
    connected_telegram_id: Option(Int),
    connected_username: Option(String),
    selected_chats: String,       // JSON array
    chat_modes: String,           // JSON object
    trigger_words: String,        // JSON array
    trigger_preset: Option(String),
    character_name: Option(String),
    character_style: Option(String),
    character_bio: Option(String),
    character_examples: String,   // JSON array
    is_completed: Bool,
    is_active: Bool,
  )
}

pub type SetupError {
  NotFound
  DatabaseError(String)
}

// =============================================================================
// Public API
// =============================================================================

/// Get setup progress by user_id
pub fn get_progress(user_id: Int) -> Result(SetupProgress, SetupError) {
  case postgres.get_global_pool() {
    None -> Error(DatabaseError("No database connection"))
    Some(pool) -> get_progress_from_db(pool, user_id)
  }
}

/// Create new setup progress (upsert)
pub fn create_progress(user_id: Int) -> Result(SetupProgress, SetupError) {
  case postgres.get_global_pool() {
    None -> Error(DatabaseError("No database connection"))
    Some(pool) -> {
      let sql = "
        INSERT INTO userbot_setup_progress (user_id, current_step)
        VALUES ($1, 'welcome')
        ON CONFLICT (user_id) DO UPDATE SET
          current_step = 'welcome',
          updated_at = NOW()
        RETURNING user_id, current_step, phone, bridge_session_id,
                  connected_telegram_id, connected_username,
                  selected_chats::text, chat_modes::text, trigger_words::text,
                  trigger_preset, character_name, character_style, character_bio,
                  character_examples::text, is_completed, is_active
      "

      case pog.query(sql)
        |> pog.parameter(pog.int(user_id))
        |> pog.returning(progress_decoder())
        |> pog.execute(pool)
      {
        Ok(pog.Returned(_, [progress])) -> {
          io.println("[USERBOT_SETUP] Created progress for user " <> int.to_string(user_id))
          Ok(progress)
        }
        Ok(_) -> Error(DatabaseError("Failed to create progress"))
        Error(e) -> Error(DatabaseError("Database error: " <> string.inspect(e)))
      }
    }
  }
}

/// Update current step
pub fn update_step(user_id: Int, step: String) -> Result(Nil, SetupError) {
  case postgres.get_global_pool() {
    None -> Error(DatabaseError("No database connection"))
    Some(pool) -> {
      let sql = "UPDATE userbot_setup_progress SET current_step = $2, updated_at = NOW() WHERE user_id = $1"

      case pog.query(sql)
        |> pog.parameter(pog.int(user_id))
        |> pog.parameter(pog.text(step))
        |> pog.execute(pool)
      {
        Ok(_) -> {
          io.println("[USERBOT_SETUP] Updated step to " <> step <> " for user " <> int.to_string(user_id))
          Ok(Nil)
        }
        Error(e) -> Error(DatabaseError("Database error: " <> string.inspect(e)))
      }
    }
  }
}

/// Update phone and session info (Step 1)
pub fn update_auth_info(
  user_id: Int,
  phone: String,
  session_id: String,
) -> Result(Nil, SetupError) {
  case postgres.get_global_pool() {
    None -> Error(DatabaseError("No database connection"))
    Some(pool) -> {
      let sql = "
        UPDATE userbot_setup_progress
        SET phone = $2, bridge_session_id = $3, updated_at = NOW()
        WHERE user_id = $1
      "

      case pog.query(sql)
        |> pog.parameter(pog.int(user_id))
        |> pog.parameter(pog.text(phone))
        |> pog.parameter(pog.text(session_id))
        |> pog.execute(pool)
      {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(DatabaseError("Database error: " <> string.inspect(e)))
      }
    }
  }
}

/// Update connected account info after successful auth
pub fn update_connected_account(
  user_id: Int,
  telegram_id: Int,
  username: String,
) -> Result(Nil, SetupError) {
  case postgres.get_global_pool() {
    None -> Error(DatabaseError("No database connection"))
    Some(pool) -> {
      let sql = "
        UPDATE userbot_setup_progress
        SET connected_telegram_id = $2, connected_username = $3,
            current_step = 'select_chats', updated_at = NOW()
        WHERE user_id = $1
      "

      case pog.query(sql)
        |> pog.parameter(pog.int(user_id))
        |> pog.parameter(pog.int(telegram_id))
        |> pog.parameter(pog.text(username))
        |> pog.execute(pool)
      {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(DatabaseError("Database error: " <> string.inspect(e)))
      }
    }
  }
}

/// Update selected chats (Step 2)
pub fn update_selected_chats(
  user_id: Int,
  chats_json: String,
) -> Result(Nil, SetupError) {
  case postgres.get_global_pool() {
    None -> Error(DatabaseError("No database connection"))
    Some(pool) -> {
      let sql = "
        UPDATE userbot_setup_progress
        SET selected_chats = $2::jsonb, current_step = 'configure_modes', updated_at = NOW()
        WHERE user_id = $1
      "

      case pog.query(sql)
        |> pog.parameter(pog.int(user_id))
        |> pog.parameter(pog.text(chats_json))
        |> pog.execute(pool)
      {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(DatabaseError("Database error: " <> string.inspect(e)))
      }
    }
  }
}

/// Update chat modes (Step 3)
pub fn update_chat_modes(
  user_id: Int,
  modes_json: String,
) -> Result(Nil, SetupError) {
  case postgres.get_global_pool() {
    None -> Error(DatabaseError("No database connection"))
    Some(pool) -> {
      let sql = "
        UPDATE userbot_setup_progress
        SET chat_modes = $2::jsonb, current_step = 'setup_triggers', updated_at = NOW()
        WHERE user_id = $1
      "

      case pog.query(sql)
        |> pog.parameter(pog.int(user_id))
        |> pog.parameter(pog.text(modes_json))
        |> pog.execute(pool)
      {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(DatabaseError("Database error: " <> string.inspect(e)))
      }
    }
  }
}

/// Update triggers (Step 4)
pub fn update_triggers(
  user_id: Int,
  triggers_json: String,
  preset: Option(String),
) -> Result(Nil, SetupError) {
  case postgres.get_global_pool() {
    None -> Error(DatabaseError("No database connection"))
    Some(pool) -> {
      let sql = "
        UPDATE userbot_setup_progress
        SET trigger_words = $2::jsonb, trigger_preset = $3,
            current_step = 'setup_character', updated_at = NOW()
        WHERE user_id = $1
      "

      let preset_val = case preset {
        Some(p) -> pog.text(p)
        None -> pog.null()
      }

      case pog.query(sql)
        |> pog.parameter(pog.int(user_id))
        |> pog.parameter(pog.text(triggers_json))
        |> pog.parameter(preset_val)
        |> pog.execute(pool)
      {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(DatabaseError("Database error: " <> string.inspect(e)))
      }
    }
  }
}

/// Update character config (Step 5)
pub fn update_character(
  user_id: Int,
  name: String,
  style: String,
  bio: String,
  examples_json: String,
) -> Result(Nil, SetupError) {
  case postgres.get_global_pool() {
    None -> Error(DatabaseError("No database connection"))
    Some(pool) -> {
      let sql = "
        UPDATE userbot_setup_progress
        SET character_name = $2, character_style = $3, character_bio = $4,
            character_examples = $5::jsonb, current_step = 'summary', updated_at = NOW()
        WHERE user_id = $1
      "

      case pog.query(sql)
        |> pog.parameter(pog.int(user_id))
        |> pog.parameter(pog.text(name))
        |> pog.parameter(pog.text(style))
        |> pog.parameter(pog.text(bio))
        |> pog.parameter(pog.text(examples_json))
        |> pog.execute(pool)
      {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(DatabaseError("Database error: " <> string.inspect(e)))
      }
    }
  }
}

/// Mark setup as completed
pub fn complete_setup(user_id: Int) -> Result(Nil, SetupError) {
  case postgres.get_global_pool() {
    None -> Error(DatabaseError("No database connection"))
    Some(pool) -> {
      let sql = "
        UPDATE userbot_setup_progress
        SET is_completed = true, is_active = true,
            current_step = 'complete', completed_at = NOW(), updated_at = NOW()
        WHERE user_id = $1
      "

      case pog.query(sql)
        |> pog.parameter(pog.int(user_id))
        |> pog.execute(pool)
      {
        Ok(_) -> {
          io.println("[USERBOT_SETUP] Completed setup for user " <> int.to_string(user_id))
          Ok(Nil)
        }
        Error(e) -> Error(DatabaseError("Database error: " <> string.inspect(e)))
      }
    }
  }
}

/// Toggle active state (pause/resume)
pub fn set_active(user_id: Int, active: Bool) -> Result(Nil, SetupError) {
  case postgres.get_global_pool() {
    None -> Error(DatabaseError("No database connection"))
    Some(pool) -> {
      let sql = "
        UPDATE userbot_setup_progress
        SET is_active = $2, updated_at = NOW()
        WHERE user_id = $1 AND is_completed = true
      "

      case pog.query(sql)
        |> pog.parameter(pog.int(user_id))
        |> pog.parameter(pog.bool(active))
        |> pog.execute(pool)
      {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(DatabaseError("Database error: " <> string.inspect(e)))
      }
    }
  }
}

/// Delete setup progress (reset wizard)
pub fn delete_progress(user_id: Int) -> Result(Nil, SetupError) {
  case postgres.get_global_pool() {
    None -> Error(DatabaseError("No database connection"))
    Some(pool) -> {
      let sql = "DELETE FROM userbot_setup_progress WHERE user_id = $1"

      case pog.query(sql)
        |> pog.parameter(pog.int(user_id))
        |> pog.execute(pool)
      {
        Ok(_) -> {
          io.println("[USERBOT_SETUP] Deleted progress for user " <> int.to_string(user_id))
          Ok(Nil)
        }
        Error(e) -> Error(DatabaseError("Database error: " <> string.inspect(e)))
      }
    }
  }
}

/// Get all active user-bots
pub fn get_active_userbots() -> List(SetupProgress) {
  case postgres.get_global_pool() {
    None -> []
    Some(pool) -> {
      let sql = "
        SELECT user_id, current_step, phone, bridge_session_id,
               connected_telegram_id, connected_username,
               selected_chats::text, chat_modes::text, trigger_words::text,
               trigger_preset, character_name, character_style, character_bio,
               character_examples::text, is_completed, is_active
        FROM userbot_setup_progress
        WHERE is_completed = true AND is_active = true
      "

      case pog.query(sql)
        |> pog.returning(progress_decoder())
        |> pog.execute(pool)
      {
        Ok(pog.Returned(_, results)) -> results
        Error(_) -> []
      }
    }
  }
}

// =============================================================================
// Internal
// =============================================================================

fn get_progress_from_db(pool: pog.Connection, user_id: Int) -> Result(SetupProgress, SetupError) {
  let sql = "
    SELECT user_id, current_step, phone, bridge_session_id,
           connected_telegram_id, connected_username,
           selected_chats::text, chat_modes::text, trigger_words::text,
           trigger_preset, character_name, character_style, character_bio,
           character_examples::text, is_completed, is_active
    FROM userbot_setup_progress
    WHERE user_id = $1
  "

  case pog.query(sql)
    |> pog.parameter(pog.int(user_id))
    |> pog.returning(progress_decoder())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [progress])) -> Ok(progress)
    Ok(pog.Returned(_, [])) -> Error(NotFound)
    Ok(_) -> Error(DatabaseError("Multiple rows found"))
    Error(e) -> Error(DatabaseError("Database error: " <> string.inspect(e)))
  }
}

fn progress_decoder() -> decode.Decoder(SetupProgress) {
  use user_id <- decode.field(0, decode.int)
  use current_step <- decode.field(1, decode.string)
  use phone <- decode.field(2, decode.optional(decode.string))
  use bridge_session_id <- decode.field(3, decode.optional(decode.string))
  use connected_telegram_id <- decode.field(4, decode.optional(decode.int))
  use connected_username <- decode.field(5, decode.optional(decode.string))
  use selected_chats <- decode.field(6, decode.string)
  use chat_modes <- decode.field(7, decode.string)
  use trigger_words <- decode.field(8, decode.string)
  use trigger_preset <- decode.field(9, decode.optional(decode.string))
  use character_name <- decode.field(10, decode.optional(decode.string))
  use character_style <- decode.field(11, decode.optional(decode.string))
  use character_bio <- decode.field(12, decode.optional(decode.string))
  use character_examples <- decode.field(13, decode.string)
  use is_completed <- decode.field(14, decode.bool)
  use is_active <- decode.field(15, decode.bool)

  decode.success(SetupProgress(
    user_id: user_id,
    current_step: current_step,
    phone: phone,
    bridge_session_id: bridge_session_id,
    connected_telegram_id: connected_telegram_id,
    connected_username: connected_username,
    selected_chats: selected_chats,
    chat_modes: chat_modes,
    trigger_words: trigger_words,
    trigger_preset: trigger_preset,
    character_name: character_name,
    character_style: character_style,
    character_bio: character_bio,
    character_examples: character_examples,
    is_completed: is_completed,
    is_active: is_active,
  ))
}
