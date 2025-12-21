// Text to Video Scene Handler
// Converts text prompts to video using Kling or KIE.ai (Veo3)

import gleam/dict
import gleam/option.{None, Some}
import vibee/bot/scene.{
  type IncomingMessage, type OutgoingMessage, type Scene, type UserSession,
  CallbackQuery, Command, InlineButton, Main, Idle, TextMessage,
  TextReply, TextToVideo, TextToVideoEnterPrompt, TextToVideoGenerating,
  TextToVideoResult, TextToVideoSelectProvider, TextWithKeyboard, VideoReply,
  button, set_scene,
}
import vibee/bot/scene_handler.{
  type NextAction, type SceneError, type SceneResult, InvalidInput, InvalidState,
  JobTextToVideo, NoAction, SceneResult, StartJob,
}
import vibee/sales/paywall

// ============================================================
// Scene Entry Point
// ============================================================

/// Handle /video command - start text to video flow
pub fn enter(session: UserSession) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, TextToVideo(TextToVideoSelectProvider))

  let keyboard = [
    [button("Kling AI", "t2v:kling")],
    [button("Veo3 (KIE.ai)", "t2v:veo3")],
    [button("Cancel", "cancel")],
  ]

  Ok(SceneResult(
    session: new_session,
    response: Some(TextWithKeyboard(
      "Choose video generation provider:\n\n" <>
      "**Kling AI** - High quality, stable motion\n" <>
      "**Veo3** - Google's latest, realistic motion",
      keyboard,
    )),
    next_action: NoAction,
  ))
}

// ============================================================
// Message Handling
// ============================================================

/// Handle messages in TextToVideo scene
pub fn handle_message(
  session: UserSession,
  message: IncomingMessage,
) -> Result(SceneResult, SceneError) {
  case session.scene {
    TextToVideo(TextToVideoEnterPrompt(provider)) ->
      handle_prompt_input(session, message, provider)

    TextToVideo(_) ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please use the buttons to navigate.")),
        next_action: NoAction,
      ))

    _ -> Error(InvalidState("Not in TextToVideo scene"))
  }
}

/// Handle prompt input
fn handle_prompt_input(
  session: UserSession,
  message: IncomingMessage,
  provider: String,
) -> Result(SceneResult, SceneError) {
  case message {
    TextMessage(prompt) -> {
      case validate_prompt(prompt) {
        Ok(_) -> start_generation(session, provider, prompt)
        Error(e) -> Error(e)
      }
    }
    Command("cancel", _) -> cancel_scene(session)
    _ ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please enter a text description for your video.")),
        next_action: NoAction,
      ))
  }
}

// ============================================================
// Callback Handling
// ============================================================

/// Handle callbacks in TextToVideo scene
pub fn handle_callback(
  session: UserSession,
  data: String,
) -> Result(SceneResult, SceneError) {
  case data {
    "t2v:kling" -> select_provider(session, "kling")
    "t2v:veo3" -> select_provider(session, "veo3")
    "cancel" -> cancel_scene(session)
    _ -> Error(InvalidInput("Unknown callback: " <> data))
  }
}

/// Handle provider selection
fn select_provider(
  session: UserSession,
  provider: String,
) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, TextToVideo(TextToVideoEnterPrompt(provider)))

  let provider_name = case provider {
    "kling" -> "Kling AI"
    "veo3" -> "Veo3"
    _ -> provider
  }

  Ok(SceneResult(
    session: new_session,
    response: Some(TextReply(
      "Selected: " <> provider_name <> "\n\n" <>
      "Now describe the video you want to create.\n\n" <>
      "Example: \"A golden retriever running through a field of sunflowers, " <>
      "cinematic lighting, slow motion\"\n\n" <>
      "Type /cancel to go back."
    )),
    next_action: NoAction,
  ))
}

// ============================================================
// Generation
// ============================================================

/// Start video generation
fn start_generation(
  session: UserSession,
  provider: String,
  prompt: String,
) -> Result(SceneResult, SceneError) {
  // Проверка paywall перед генерацией
  case paywall.check_access(session.user_id, paywall.Generation) {
    paywall.AccessGranted(_) -> {
      // Job ID will be set by the job system
      let job_id = "pending"
      let new_session = set_scene(
        session,
        TextToVideo(TextToVideoGenerating(provider, prompt, job_id)),
      )

      let params = dict.new()
        |> dict.insert("provider", provider)
        |> dict.insert("prompt", prompt)
        |> dict.insert("chat_id", session.chat_id)

      // Записать использование
      let _ = paywall.record_usage(session.user_id, paywall.Generation)

      Ok(SceneResult(
        session: new_session,
        response: Some(TextReply(
          "Starting video generation...\n\n" <>
          "This may take 2-5 minutes. I'll notify you when it's ready."
        )),
        next_action: StartJob(JobTextToVideo, params),
      ))
    }
    access_result -> {
      let message = paywall.get_access_message(access_result, "ru")
        <> "\n\n💎 /pricing - тарифы\n🎯 /quiz - подобрать тариф"
      Ok(SceneResult(
        session: session,
        response: Some(TextReply(message)),
        next_action: NoAction,
      ))
    }
  }
}

/// Handle generation result
pub fn handle_result(
  session: UserSession,
  video_url: String,
) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, TextToVideo(TextToVideoResult(video_url)))

  let keyboard = [
    [button("Generate Another", "t2v:again")],
    [button("Back to Menu", "menu")],
  ]

  Ok(SceneResult(
    session: new_session,
    response: Some(VideoReply(video_url, Some("Your video is ready!"))),
    next_action: NoAction,
  ))
}

// ============================================================
// Helpers
// ============================================================

fn validate_prompt(prompt: String) -> Result(String, SceneError) {
  case prompt {
    "" -> Error(InvalidInput("Prompt cannot be empty"))
    p -> Ok(p)
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
// Available Providers
// ============================================================

pub type VideoProvider {
  VideoProvider(id: String, name: String, description: String, cost: Float)
}

pub fn available_providers() -> List(VideoProvider) {
  [
    VideoProvider(
      id: "kling",
      name: "Kling AI",
      description: "High quality, stable motion, 5-10 sec videos",
      cost: 10.0,
    ),
    VideoProvider(
      id: "veo3",
      name: "Veo3 (KIE.ai)",
      description: "Google's latest model, realistic motion",
      cost: 15.0,
    ),
  ]
}
