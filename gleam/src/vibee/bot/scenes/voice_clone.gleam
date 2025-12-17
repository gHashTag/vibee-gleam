// Voice Clone Scene Handler
// Clones voice from sample and generates speech using ElevenLabs

import gleam/dict
import gleam/option.{None, Some}
import vibee/bot/scene.{
  type IncomingMessage, type OutgoingMessage, type Scene, type UserSession,
  CallbackQuery, Command, InlineButton, Main, Idle, TextMessage,
  TextReply, VoiceClone, VoiceCloneUploadSample, VoiceCloneEnterText,
  VoiceCloneGenerating, VoiceCloneResult, AudioReply,
  button, set_scene,
}
import vibee/bot/scene_handler.{
  type NextAction, type SceneError, type SceneResult, InvalidInput, InvalidState,
  JobVoiceClone, NoAction, SceneResult, StartJob,
}

// ============================================================
// Types for voice messages
// ============================================================

pub type VoiceMessage {
  VoiceMessage(file_id: String, duration: Int)
}

// ============================================================
// Scene Entry Point
// ============================================================

/// Handle /voice command - start voice clone flow
pub fn enter(session: UserSession) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, VoiceClone(VoiceCloneUploadSample))

  Ok(SceneResult(
    session: new_session,
    response: Some(TextReply(
      "Let's clone a voice!\n\n" <>
      "Send me a **voice message** (at least 10 seconds).\n\n" <>
      "Tips for best results:\n" <>
      "- Clear audio, minimal background noise\n" <>
      "- Natural speaking pace\n" <>
      "- Single speaker only\n\n" <>
      "Type /cancel to go back."
    )),
    next_action: NoAction,
  ))
}

// ============================================================
// Message Handling
// ============================================================

/// Handle messages in VoiceClone scene
pub fn handle_message(
  session: UserSession,
  message: IncomingMessage,
) -> Result(SceneResult, SceneError) {
  case session.scene {
    VoiceClone(VoiceCloneUploadSample) ->
      handle_voice_upload(session, message)

    VoiceClone(VoiceCloneEnterText(voice_id)) ->
      handle_text_input(session, message, voice_id)

    VoiceClone(_) ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please wait for the current operation to complete.")),
        next_action: NoAction,
      ))

    _ -> Error(InvalidState("Not in VoiceClone scene"))
  }
}

/// Handle voice sample upload
fn handle_voice_upload(
  session: UserSession,
  message: IncomingMessage,
) -> Result(SceneResult, SceneError) {
  // Note: In real implementation, we'd check for voice message type
  // For now, we accept any message and simulate voice handling
  case message {
    TextMessage(text) -> {
      // Simulate voice upload with text as placeholder
      // In real impl, this would be a voice message handler
      case text {
        "/cancel" -> cancel_scene(session)
        _ -> {
          // Simulate voice_id generation (would be from ElevenLabs API)
          let voice_id = "cloned_" <> session.chat_id
          let new_session = set_scene(session, VoiceClone(VoiceCloneEnterText(voice_id)))

          Ok(SceneResult(
            session: new_session,
            response: Some(TextReply(
              "Voice sample received and cloned!\n\n" <>
              "Now enter the **text** you want to convert to speech.\n\n" <>
              "The cloned voice will read your text."
            )),
            next_action: NoAction,
          ))
        }
      }
    }
    Command("cancel", _) -> cancel_scene(session)
    _ ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply(
          "Please send a voice message for cloning.\n\n" <>
          "(At least 10 seconds of clear speech)"
        )),
        next_action: NoAction,
      ))
  }
}

/// Handle text input for speech generation
fn handle_text_input(
  session: UserSession,
  message: IncomingMessage,
  voice_id: String,
) -> Result(SceneResult, SceneError) {
  case message {
    TextMessage(text) -> {
      case validate_text(text) {
        Ok(_) -> start_generation(session, voice_id, text)
        Error(e) -> Error(e)
      }
    }
    Command("cancel", _) -> cancel_scene(session)
    _ ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please enter text for speech generation.")),
        next_action: NoAction,
      ))
  }
}

// ============================================================
// Callback Handling
// ============================================================

/// Handle callbacks in VoiceClone scene
pub fn handle_callback(
  session: UserSession,
  data: String,
) -> Result(SceneResult, SceneError) {
  case data {
    "cancel" -> cancel_scene(session)
    "voice:another" -> {
      // Reset to enter text state if we have a voice_id
      case session.scene {
        VoiceClone(VoiceCloneResult(_)) -> {
          // Would need to get voice_id from context
          Ok(SceneResult(
            session: session,
            response: Some(TextReply("Enter another text to generate speech:")),
            next_action: NoAction,
          ))
        }
        _ -> Error(InvalidInput("Cannot generate another without voice clone"))
      }
    }
    _ -> Error(InvalidInput("Unknown callback: " <> data))
  }
}

// ============================================================
// Generation
// ============================================================

/// Start speech generation
fn start_generation(
  session: UserSession,
  voice_id: String,
  text: String,
) -> Result(SceneResult, SceneError) {
  let job_id = "pending"
  let new_session = set_scene(
    session,
    VoiceClone(VoiceCloneGenerating(voice_id, text, job_id)),
  )

  let params = dict.new()
    |> dict.insert("voice_id", voice_id)
    |> dict.insert("text", text)
    |> dict.insert("chat_id", session.chat_id)

  Ok(SceneResult(
    session: new_session,
    response: Some(TextReply(
      "Generating speech with cloned voice...\n\n" <>
      "This usually takes 10-30 seconds."
    )),
    next_action: StartJob(JobVoiceClone, params),
  ))
}

/// Handle generation result
pub fn handle_result(
  session: UserSession,
  audio_url: String,
) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, VoiceClone(VoiceCloneResult(audio_url)))

  Ok(SceneResult(
    session: new_session,
    response: Some(AudioReply(audio_url, Some("Your cloned voice audio is ready!"))),
    next_action: NoAction,
  ))
}

// ============================================================
// Helpers
// ============================================================

fn validate_text(text: String) -> Result(String, SceneError) {
  case text {
    "" -> Error(InvalidInput("Text cannot be empty"))
    "/cancel" -> Error(InvalidInput("Cancelled"))
    t -> Ok(t)
  }
}

fn cancel_scene(session: UserSession) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, Main(Idle))

  Ok(SceneResult(
    session: new_session,
    response: Some(TextReply("Cancelled. Use /menu to see available options.")),
    next_action: NoAction,
  ))
}

// ============================================================
// Voice Clone Settings
// ============================================================

pub type VoiceSettings {
  VoiceSettings(
    stability: Float,
    similarity_boost: Float,
    style: Float,
  )
}

pub fn default_settings() -> VoiceSettings {
  VoiceSettings(
    stability: 0.5,
    similarity_boost: 0.75,
    style: 0.0,
  )
}

pub fn expressive_settings() -> VoiceSettings {
  VoiceSettings(
    stability: 0.3,
    similarity_boost: 0.8,
    style: 0.5,
  )
}
