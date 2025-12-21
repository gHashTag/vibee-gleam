// NeuroPhoto Scene - AI Image Generation
// Integrates with FAL.ai for FLUX LoRA, Nano Banana Pro

import gleam/dict
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/ai/fal
import vibee/bot/scene.{
  type IncomingMessage, type OutgoingMessage, type UserSession,
  CallbackQuery, Command, Main, Idle, NeuroPhoto, NeuroPhotoEnterPrompt,
  NeuroPhotoGenerating, NeuroPhotoSelectModel, PhotoMessage,
  TextMessage, TextReply, TextWithKeyboard, button, set_scene,
}
import vibee/bot/scene_handler.{
  type SceneError, type SceneResult, InvalidInput, InvalidState,
  JobNeuroPhoto, NoAction, SceneResult, StartJob,
}
import vibee/sales/paywall

// ============================================================
// Model Definitions
// ============================================================

pub type Model {
  FluxLora
  NanoBanana
  FluxKontext
}

pub fn model_from_string(s: String) -> Model {
  case s {
    "flux-lora" -> FluxLora
    "nano-banana" -> NanoBanana
    "flux-kontext" -> FluxKontext
    _ -> NanoBanana
  }
}

pub fn model_to_string(m: Model) -> String {
  case m {
    FluxLora -> "flux-lora"
    NanoBanana -> "nano-banana"
    FluxKontext -> "flux-kontext"
  }
}

pub fn model_display_name(m: Model) -> String {
  case m {
    FluxLora -> "FLUX LoRA"
    NanoBanana -> "Nano Banana Pro"
    FluxKontext -> "FLUX Kontext"
  }
}

// ============================================================
// Scene Entry Point
// ============================================================

/// Handle /neurophoto command - start image generation flow
pub fn enter(session: UserSession) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, NeuroPhoto(NeuroPhotoSelectModel))

  let keyboard = [
    [button("Nano Banana Pro (Fast)", "np:nano-banana")],
    [button("FLUX LoRA (Best Quality)", "np:flux-lora")],
    [button("FLUX Kontext (Transform)", "np:flux-kontext")],
    [button("Cancel", "cancel")],
  ]

  Ok(SceneResult(
    session: new_session,
    response: Some(TextWithKeyboard(
      "NeuroPhoto - AI Image Generation\n\n" <>
      "Choose a model:\n\n" <>
      "**Nano Banana Pro** - Fast, good quality (~10s)\n" <>
      "**FLUX LoRA** - Best quality, slower (~30s)\n" <>
      "**FLUX Kontext** - Transform existing images",
      keyboard,
    )),
    next_action: NoAction,
  ))
}

// ============================================================
// Message Handling
// ============================================================

/// Handle messages in NeuroPhoto scene
pub fn handle_message(
  session: UserSession,
  message: IncomingMessage,
) -> Result(SceneResult, SceneError) {
  case session.scene {
    NeuroPhoto(NeuroPhotoEnterPrompt(model)) ->
      handle_prompt_input(session, message, model)

    NeuroPhoto(_) ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please use the buttons to navigate.")),
        next_action: NoAction,
      ))

    _ -> Error(InvalidState("Not in NeuroPhoto scene"))
  }
}

/// Handle prompt input
fn handle_prompt_input(
  session: UserSession,
  message: IncomingMessage,
  model: String,
) -> Result(SceneResult, SceneError) {
  case message {
    TextMessage(prompt) -> {
      case validate_prompt(prompt) {
        Ok(_) -> start_generation(session, model, prompt)
        Error(e) -> Error(e)
      }
    }
    Command("cancel", _) -> cancel_scene(session)
    _ ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please enter a text description for your image.")),
        next_action: NoAction,
      ))
  }
}

// ============================================================
// Callback Handling
// ============================================================

/// Handle callbacks in NeuroPhoto scene
pub fn handle_callback(
  session: UserSession,
  data: String,
) -> Result(SceneResult, SceneError) {
  case data {
    "np:nano-banana" -> select_model(session, "nano-banana")
    "np:flux-lora" -> select_model(session, "flux-lora")
    "np:flux-kontext" -> select_model(session, "flux-kontext")
    "np:again" -> enter(session)
    "np:back" -> enter(session)
    "cancel" -> cancel_scene(session)
    "back_menu" -> cancel_scene(session)
    _ -> {
      // Handle old-style callbacks for compatibility
      case string.starts_with(data, "np_model_") {
        True -> {
          let model = string.drop_start(data, 9)
          select_model(session, model)
        }
        False -> Error(InvalidInput("Unknown callback: " <> data))
      }
    }
  }
}

/// Handle model selection
fn select_model(
  session: UserSession,
  model: String,
) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, NeuroPhoto(NeuroPhotoEnterPrompt(model)))

  let model_name = model_display_name(model_from_string(model))

  Ok(SceneResult(
    session: new_session,
    response: Some(TextReply(
      "Selected: " <> model_name <> "\n\n" <>
      "Now describe the image you want to create.\n\n" <>
      "Tips:\n" <>
      "- Be specific and descriptive\n" <>
      "- Include style keywords (cinematic, portrait, anime)\n" <>
      "- Mention lighting and mood\n\n" <>
      "Example: \"A cyberpunk cat wearing neon glasses, futuristic city background, volumetric lighting\"\n\n" <>
      "Type /cancel to go back."
    )),
    next_action: NoAction,
  ))
}

// ============================================================
// Generation
// ============================================================

/// Start image generation
fn start_generation(
  session: UserSession,
  model: String,
  prompt: String,
) -> Result(SceneResult, SceneError) {
  // Check paywall before generation
  case paywall.check_access(session.user_id, paywall.Generation) {
    paywall.AccessGranted(_) -> {
      // Job ID will be set by the job system
      let job_id = "pending"
      let new_session = set_scene(
        session,
        NeuroPhoto(NeuroPhotoGenerating(model, prompt, job_id)),
      )

      let params = dict.new()
        |> dict.insert("model", model)
        |> dict.insert("prompt", prompt)
        |> dict.insert("chat_id", session.chat_id)

      // Record usage
      let _ = paywall.record_usage(session.user_id, paywall.Generation)

      Ok(SceneResult(
        session: new_session,
        response: Some(TextReply(
          "Generating image with " <> model_display_name(model_from_string(model)) <> "...\n\n" <>
          "Prompt: " <> prompt <> "\n\n" <>
          "This may take 10-30 seconds. I'll send the image when it's ready."
        )),
        next_action: StartJob(JobNeuroPhoto, params),
      ))
    }
    access_result -> {
      let message = paywall.get_access_message(access_result, "ru")
        <> "\n\n/pricing - tarify\n/quiz - podobrat' tarif"
      Ok(SceneResult(
        session: session,
        response: Some(TextReply(message)),
        next_action: NoAction,
      ))
    }
  }
}

/// Handle generation result (called when job completes)
pub fn handle_result(
  session: UserSession,
  image_url: String,
) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, NeuroPhoto(NeuroPhotoSelectModel))

  let keyboard = [
    [button("Generate Another", "np:again")],
    [button("Back to Menu", "back_menu")],
  ]

  Ok(SceneResult(
    session: new_session,
    response: Some(TextWithKeyboard(
      "Your image is ready!",
      keyboard,
    )),
    next_action: NoAction,
  ))
}

// ============================================================
// Helpers
// ============================================================

fn validate_prompt(prompt: String) -> Result(String, SceneError) {
  let trimmed = string.trim(prompt)
  case trimmed {
    "" -> Error(InvalidInput("Prompt cannot be empty"))
    p -> {
      case string.length(p) < 3 {
        True -> Error(InvalidInput("Prompt is too short. Please describe the image in more detail."))
        False -> Ok(p)
      }
    }
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
// Legacy Response Functions (for compatibility)
// ============================================================

pub fn select_model_response() -> OutgoingMessage {
  TextWithKeyboard(
    "NeuroPhoto - AI Image Generation\n\n"
    <> "Choose a model for your image:",
    [
      [button("FLUX LoRA (Best Quality)", "np_model_flux-lora")],
      [button("Nano Banana Pro (Fast)", "np_model_nano-banana")],
      [button("FLUX Kontext (Transform)", "np_model_flux-kontext")],
      [button("Back to Menu", "back_menu")],
    ],
  )
}

pub fn enter_prompt_response(model: String) -> OutgoingMessage {
  let model_name = model_display_name(model_from_string(model))
  TextWithKeyboard(
    "Selected: " <> model_name <> "\n\n"
    <> "Now enter your prompt for the image.\n\n"
    <> "Tips:\n"
    <> "- Be specific and descriptive\n"
    <> "- Include style keywords (cinematic, portrait, etc)\n"
    <> "- Mention lighting and mood",
    [[button("Back", "np_back_select")]],
  )
}

pub fn generating_response(model: String, prompt: String) -> OutgoingMessage {
  TextReply(
    "Generating with " <> model_display_name(model_from_string(model)) <> "...\n\n"
    <> "Prompt: " <> prompt <> "\n\n"
    <> "This may take 10-30 seconds.",
  )
}

pub fn error_response(error: String) -> OutgoingMessage {
  TextWithKeyboard(
    "Error: " <> error <> "\n\nPlease try again.",
    [[button("Try Again", "np_again")], [button("Menu", "back_menu")]],
  )
}
