// Image to Video Scene Handler
// Animates static images into video using Kling AI

import gleam/dict
import gleam/option.{None, Some}
import vibee/bot/scene.{
  type IncomingMessage, type OutgoingMessage, type Scene, type UserSession,
  CallbackQuery, Command, InlineButton, Main, Idle, PhotoMessage, TextMessage,
  TextReply, ImageToVideo, ImageToVideoEnterPrompt, ImageToVideoGenerating,
  ImageToVideoResult, ImageToVideoUploadImage, TextWithKeyboard, VideoReply,
  button, set_scene,
}
import vibee/bot/scene_handler.{
  type NextAction, type SceneError, type SceneResult, InvalidInput, InvalidState,
  JobImageToVideo, NoAction, SceneResult, StartJob,
}

// ============================================================
// Scene Entry Point
// ============================================================

/// Handle /animate command - start image to video flow
pub fn enter(session: UserSession) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, ImageToVideo(ImageToVideoUploadImage))

  Ok(SceneResult(
    session: new_session,
    response: Some(TextReply(
      "Send me an image to animate.\n\n" <>
      "The image will be transformed into a short video with motion.\n\n" <>
      "Supported formats: JPG, PNG\n" <>
      "Type /cancel to go back."
    )),
    next_action: NoAction,
  ))
}

// ============================================================
// Message Handling
// ============================================================

/// Handle messages in ImageToVideo scene
pub fn handle_message(
  session: UserSession,
  message: IncomingMessage,
) -> Result(SceneResult, SceneError) {
  case session.scene {
    ImageToVideo(ImageToVideoUploadImage) ->
      handle_image_upload(session, message)

    ImageToVideo(ImageToVideoEnterPrompt(image_url)) ->
      handle_prompt_input(session, message, image_url)

    ImageToVideo(_) ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please wait for the current operation to complete.")),
        next_action: NoAction,
      ))

    _ -> Error(InvalidState("Not in ImageToVideo scene"))
  }
}

/// Handle image upload
fn handle_image_upload(
  session: UserSession,
  message: IncomingMessage,
) -> Result(SceneResult, SceneError) {
  case message {
    PhotoMessage(file_id, _caption) -> {
      // In real implementation, file_id would be converted to URL
      let image_url = "telegram://file/" <> file_id
      let new_session = set_scene(session, ImageToVideo(ImageToVideoEnterPrompt(image_url)))

      let keyboard = [
        [button("Auto Motion", "i2v:auto")],
        [button("Custom Prompt", "i2v:custom")],
        [button("Cancel", "cancel")],
      ]

      Ok(SceneResult(
        session: new_session,
        response: Some(TextWithKeyboard(
          "Image received!\n\n" <>
          "Choose motion type:\n" <>
          "- **Auto Motion**: AI decides the best motion\n" <>
          "- **Custom Prompt**: Describe the motion you want",
          keyboard,
        )),
        next_action: NoAction,
      ))
    }
    Command("cancel", _) -> cancel_scene(session)
    _ ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please send an image to animate.")),
        next_action: NoAction,
      ))
  }
}

/// Handle motion prompt input
fn handle_prompt_input(
  session: UserSession,
  message: IncomingMessage,
  image_url: String,
) -> Result(SceneResult, SceneError) {
  case message {
    TextMessage(prompt) -> start_generation(session, image_url, prompt)
    Command("cancel", _) -> cancel_scene(session)
    _ ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please enter a motion description or use the buttons.")),
        next_action: NoAction,
      ))
  }
}

// ============================================================
// Callback Handling
// ============================================================

/// Handle callbacks in ImageToVideo scene
pub fn handle_callback(
  session: UserSession,
  data: String,
) -> Result(SceneResult, SceneError) {
  case session.scene, data {
    ImageToVideo(ImageToVideoEnterPrompt(image_url)), "i2v:auto" ->
      start_generation(session, image_url, "natural motion, smooth animation")

    ImageToVideo(ImageToVideoEnterPrompt(_)), "i2v:custom" ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply(
          "Describe the motion you want:\n\n" <>
          "Examples:\n" <>
          "- \"zoom in slowly\"\n" <>
          "- \"pan from left to right\"\n" <>
          "- \"gentle wind blowing through hair\"\n" <>
          "- \"water ripples and waves\""
        )),
        next_action: NoAction,
      ))

    _, "cancel" -> cancel_scene(session)

    _, _ -> Error(InvalidInput("Unknown callback: " <> data))
  }
}

// ============================================================
// Generation
// ============================================================

/// Start video generation
fn start_generation(
  session: UserSession,
  image_url: String,
  prompt: String,
) -> Result(SceneResult, SceneError) {
  let job_id = "pending"
  let new_session = set_scene(
    session,
    ImageToVideo(ImageToVideoGenerating(image_url, prompt, job_id)),
  )

  let params = dict.new()
    |> dict.insert("image_url", image_url)
    |> dict.insert("prompt", prompt)
    |> dict.insert("chat_id", session.chat_id)

  Ok(SceneResult(
    session: new_session,
    response: Some(TextReply(
      "Animating your image...\n\n" <>
      "This may take 1-3 minutes. I'll notify you when it's ready."
    )),
    next_action: StartJob(JobImageToVideo, params),
  ))
}

/// Handle generation result
pub fn handle_result(
  session: UserSession,
  video_url: String,
) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, ImageToVideo(ImageToVideoResult(video_url)))

  Ok(SceneResult(
    session: new_session,
    response: Some(VideoReply(video_url, Some("Your animated video is ready!"))),
    next_action: NoAction,
  ))
}

// ============================================================
// Helpers
// ============================================================

fn cancel_scene(session: UserSession) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, Main(Idle))

  Ok(SceneResult(
    session: new_session,
    response: Some(TextReply("Cancelled. Use /menu to see available options.")),
    next_action: NoAction,
  ))
}
