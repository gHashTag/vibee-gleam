// Avatar Video Scene Handler
// Creates talking head videos using Hedra or HeyGen

import gleam/dict
import gleam/option.{None, Some}
import vibee/bot/scene.{
  type IncomingMessage, type OutgoingMessage, type Scene, type UserSession,
  CallbackQuery, Command, InlineButton, Main, Idle, PhotoMessage, TextMessage,
  TextReply, AvatarVideo, AvatarVideoUploadPortrait, AvatarVideoEnterScript,
  AvatarVideoSelectVoice, AvatarVideoGenerating, AvatarVideoResult,
  TextWithKeyboard, VideoReply,
  button, set_scene,
}
import vibee/bot/scene_handler.{
  type NextAction, type SceneError, type SceneResult, InvalidInput, InvalidState,
  JobAvatarVideo, NoAction, SceneResult, StartJob,
}

// ============================================================
// Scene Entry Point
// ============================================================

/// Handle /talking command - start avatar video flow
pub fn enter(session: UserSession) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, AvatarVideo(AvatarVideoUploadPortrait))

  Ok(SceneResult(
    session: new_session,
    response: Some(TextReply(
      "Let's create a talking avatar video!\n\n" <>
      "Send me a **portrait photo** (face clearly visible).\n\n" <>
      "Tips:\n" <>
      "- Front-facing portrait works best\n" <>
      "- Good lighting\n" <>
      "- Neutral expression\n\n" <>
      "Type /cancel to go back."
    )),
    next_action: NoAction,
  ))
}

// ============================================================
// Message Handling
// ============================================================

/// Handle messages in AvatarVideo scene
pub fn handle_message(
  session: UserSession,
  message: IncomingMessage,
) -> Result(SceneResult, SceneError) {
  case session.scene {
    AvatarVideo(AvatarVideoUploadPortrait) ->
      handle_portrait_upload(session, message)

    AvatarVideo(AvatarVideoEnterScript(portrait_url)) ->
      handle_script_input(session, message, portrait_url)

    AvatarVideo(AvatarVideoSelectVoice(portrait_url, script)) ->
      handle_voice_selection(session, message, portrait_url, script)

    AvatarVideo(_) ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please wait for the current operation to complete.")),
        next_action: NoAction,
      ))

    _ -> Error(InvalidState("Not in AvatarVideo scene"))
  }
}

/// Handle portrait upload
fn handle_portrait_upload(
  session: UserSession,
  message: IncomingMessage,
) -> Result(SceneResult, SceneError) {
  case message {
    PhotoMessage(file_id, _caption) -> {
      let portrait_url = "telegram://file/" <> file_id
      let new_session = set_scene(session, AvatarVideo(AvatarVideoEnterScript(portrait_url)))

      Ok(SceneResult(
        session: new_session,
        response: Some(TextReply(
          "Portrait received!\n\n" <>
          "Now enter the **script** for the avatar to speak.\n\n" <>
          "Example:\n" <>
          "\"Hello! Welcome to my channel. Today I want to share " <>
          "something exciting with you.\""
        )),
        next_action: NoAction,
      ))
    }
    Command("cancel", _) -> cancel_scene(session)
    _ ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please send a portrait photo for the talking avatar.")),
        next_action: NoAction,
      ))
  }
}

/// Handle script input
fn handle_script_input(
  session: UserSession,
  message: IncomingMessage,
  portrait_url: String,
) -> Result(SceneResult, SceneError) {
  case message {
    TextMessage(script) -> {
      case validate_script(script) {
        Ok(_) -> {
          let new_session = set_scene(
            session,
            AvatarVideo(AvatarVideoSelectVoice(portrait_url, script)),
          )

          let keyboard = [
            [button("Sarah (Female, EN)", "voice:sarah")],
            [button("Adam (Male, EN)", "voice:adam")],
            [button("Elena (Female, RU)", "voice:elena")],
            [button("Boris (Male, RU)", "voice:boris")],
            [button("Cancel", "cancel")],
          ]

          Ok(SceneResult(
            session: new_session,
            response: Some(TextWithKeyboard(
              "Script received!\n\n" <>
              "Choose a voice for the avatar:",
              keyboard,
            )),
            next_action: NoAction,
          ))
        }
        Error(e) -> Error(e)
      }
    }
    Command("cancel", _) -> cancel_scene(session)
    _ ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please enter a script for the avatar to speak.")),
        next_action: NoAction,
      ))
  }
}

/// Handle voice selection message
fn handle_voice_selection(
  session: UserSession,
  message: IncomingMessage,
  portrait_url: String,
  script: String,
) -> Result(SceneResult, SceneError) {
  case message {
    Command("cancel", _) -> cancel_scene(session)
    _ ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please select a voice using the buttons.")),
        next_action: NoAction,
      ))
  }
}

// ============================================================
// Callback Handling
// ============================================================

/// Handle callbacks in AvatarVideo scene
pub fn handle_callback(
  session: UserSession,
  data: String,
) -> Result(SceneResult, SceneError) {
  case session.scene, data {
    AvatarVideo(AvatarVideoSelectVoice(portrait_url, script)), "voice:sarah" ->
      start_generation(session, portrait_url, script, "sarah")

    AvatarVideo(AvatarVideoSelectVoice(portrait_url, script)), "voice:adam" ->
      start_generation(session, portrait_url, script, "adam")

    AvatarVideo(AvatarVideoSelectVoice(portrait_url, script)), "voice:elena" ->
      start_generation(session, portrait_url, script, "elena")

    AvatarVideo(AvatarVideoSelectVoice(portrait_url, script)), "voice:boris" ->
      start_generation(session, portrait_url, script, "boris")

    _, "cancel" -> cancel_scene(session)

    _, _ -> Error(InvalidInput("Unknown callback: " <> data))
  }
}

// ============================================================
// Generation
// ============================================================

/// Start avatar video generation
fn start_generation(
  session: UserSession,
  portrait_url: String,
  script: String,
  voice_id: String,
) -> Result(SceneResult, SceneError) {
  let job_id = "pending"
  let new_session = set_scene(
    session,
    AvatarVideo(AvatarVideoGenerating(portrait_url, script, voice_id, job_id)),
  )

  let params = dict.new()
    |> dict.insert("portrait_url", portrait_url)
    |> dict.insert("script", script)
    |> dict.insert("voice_id", voice_id)
    |> dict.insert("chat_id", session.chat_id)

  Ok(SceneResult(
    session: new_session,
    response: Some(TextReply(
      "Creating talking avatar video...\n\n" <>
      "Voice: " <> voice_id <> "\n\n" <>
      "This may take 2-5 minutes. I'll notify you when it's ready."
    )),
    next_action: StartJob(JobAvatarVideo, params),
  ))
}

/// Handle generation result
pub fn handle_result(
  session: UserSession,
  video_url: String,
) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, AvatarVideo(AvatarVideoResult(video_url)))

  Ok(SceneResult(
    session: new_session,
    response: Some(VideoReply(video_url, Some("Your talking avatar video is ready!"))),
    next_action: NoAction,
  ))
}

// ============================================================
// Helpers
// ============================================================

fn validate_script(script: String) -> Result(String, SceneError) {
  case script {
    "" -> Error(InvalidInput("Script cannot be empty"))
    s -> Ok(s)
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
// Available Voices
// ============================================================

pub type AvatarVoice {
  AvatarVoice(id: String, name: String, language: String, gender: String)
}

pub fn available_voices() -> List(AvatarVoice) {
  [
    AvatarVoice(id: "sarah", name: "Sarah", language: "en", gender: "female"),
    AvatarVoice(id: "adam", name: "Adam", language: "en", gender: "male"),
    AvatarVoice(id: "elena", name: "Elena", language: "ru", gender: "female"),
    AvatarVoice(id: "boris", name: "Boris", language: "ru", gender: "male"),
  ]
}
