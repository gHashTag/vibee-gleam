// Morphing Scene Handler
// Creates smooth transitions between two images

import gleam/dict
import gleam/option.{None, Some}
import vibee/bot/scene.{
  type IncomingMessage, type OutgoingMessage, type Scene, type UserSession,
  CallbackQuery, Command, InlineButton, Main, Idle, PhotoMessage, TextMessage,
  TextReply, Morphing, MorphingUploadStart, MorphingUploadEnd, MorphingEnterStyle,
  MorphingGenerating, MorphingResult, TextWithKeyboard, VideoReply,
  button, set_scene,
}
import vibee/bot/scene_handler.{
  type NextAction, type SceneError, type SceneResult, InvalidInput, InvalidState,
  JobMorphing, NoAction, SceneResult, StartJob,
}

// ============================================================
// Scene Entry Point
// ============================================================

/// Handle /morph command - start morphing flow
pub fn enter(session: UserSession) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, Morphing(MorphingUploadStart))

  Ok(SceneResult(
    session: new_session,
    response: Some(TextReply(
      "Let's create a morphing video!\n\n" <>
      "Send me the **first image** (starting point).\n\n" <>
      "Type /cancel to go back."
    )),
    next_action: NoAction,
  ))
}

// ============================================================
// Message Handling
// ============================================================

/// Handle messages in Morphing scene
pub fn handle_message(
  session: UserSession,
  message: IncomingMessage,
) -> Result(SceneResult, SceneError) {
  case session.scene {
    Morphing(MorphingUploadStart) ->
      handle_start_image(session, message)

    Morphing(MorphingUploadEnd(start_image)) ->
      handle_end_image(session, message, start_image)

    Morphing(MorphingEnterStyle(start_image, end_image)) ->
      handle_style_input(session, message, start_image, end_image)

    Morphing(_) ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please wait for the current operation to complete.")),
        next_action: NoAction,
      ))

    _ -> Error(InvalidState("Not in Morphing scene"))
  }
}

/// Handle start image upload
fn handle_start_image(
  session: UserSession,
  message: IncomingMessage,
) -> Result(SceneResult, SceneError) {
  case message {
    PhotoMessage(file_id, _caption) -> {
      let start_image = "telegram://file/" <> file_id
      let new_session = set_scene(session, Morphing(MorphingUploadEnd(start_image)))

      Ok(SceneResult(
        session: new_session,
        response: Some(TextReply(
          "First image received!\n\n" <>
          "Now send me the **second image** (ending point)."
        )),
        next_action: NoAction,
      ))
    }
    Command("cancel", _) -> cancel_scene(session)
    _ ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please send the first image for morphing.")),
        next_action: NoAction,
      ))
  }
}

/// Handle end image upload
fn handle_end_image(
  session: UserSession,
  message: IncomingMessage,
  start_image: String,
) -> Result(SceneResult, SceneError) {
  case message {
    PhotoMessage(file_id, _caption) -> {
      let end_image = "telegram://file/" <> file_id
      let new_session = set_scene(session, Morphing(MorphingEnterStyle(start_image, end_image)))

      let keyboard = [
        [button("Smooth (5s)", "morph:smooth")],
        [button("Fast (3s)", "morph:fast")],
        [button("Slow (10s)", "morph:slow")],
        [button("Custom", "morph:custom")],
        [button("Cancel", "cancel")],
      ]

      Ok(SceneResult(
        session: new_session,
        response: Some(TextWithKeyboard(
          "Both images received!\n\n" <>
          "Choose transition style:",
          keyboard,
        )),
        next_action: NoAction,
      ))
    }
    Command("cancel", _) -> cancel_scene(session)
    _ ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please send the second image for morphing.")),
        next_action: NoAction,
      ))
  }
}

/// Handle style input
fn handle_style_input(
  session: UserSession,
  message: IncomingMessage,
  start_image: String,
  end_image: String,
) -> Result(SceneResult, SceneError) {
  case message {
    TextMessage(style) -> start_generation(session, start_image, end_image, style)
    Command("cancel", _) -> cancel_scene(session)
    _ ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please select a style or enter a custom description.")),
        next_action: NoAction,
      ))
  }
}

// ============================================================
// Callback Handling
// ============================================================

/// Handle callbacks in Morphing scene
pub fn handle_callback(
  session: UserSession,
  data: String,
) -> Result(SceneResult, SceneError) {
  case session.scene, data {
    Morphing(MorphingEnterStyle(start_image, end_image)), "morph:smooth" ->
      start_generation(session, start_image, end_image, "smooth transition, 5 seconds")

    Morphing(MorphingEnterStyle(start_image, end_image)), "morph:fast" ->
      start_generation(session, start_image, end_image, "fast transition, 3 seconds")

    Morphing(MorphingEnterStyle(start_image, end_image)), "morph:slow" ->
      start_generation(session, start_image, end_image, "slow dramatic transition, 10 seconds")

    Morphing(MorphingEnterStyle(_, _)), "morph:custom" ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply(
          "Describe the transition style:\n\n" <>
          "Examples:\n" <>
          "- \"smooth fade with color shift\"\n" <>
          "- \"dramatic zoom transition\"\n" <>
          "- \"liquid morphing effect\""
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

/// Start morphing generation
fn start_generation(
  session: UserSession,
  start_image: String,
  end_image: String,
  style: String,
) -> Result(SceneResult, SceneError) {
  let job_id = "pending"
  let new_session = set_scene(
    session,
    Morphing(MorphingGenerating(start_image, end_image, style, job_id)),
  )

  let params = dict.new()
    |> dict.insert("start_image", start_image)
    |> dict.insert("end_image", end_image)
    |> dict.insert("style", style)
    |> dict.insert("chat_id", session.chat_id)

  Ok(SceneResult(
    session: new_session,
    response: Some(TextReply(
      "Creating morphing video...\n\n" <>
      "This may take 2-4 minutes. I'll notify you when it's ready."
    )),
    next_action: StartJob(JobMorphing, params),
  ))
}

/// Handle generation result
pub fn handle_result(
  session: UserSession,
  video_url: String,
) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, Morphing(MorphingResult(video_url)))

  Ok(SceneResult(
    session: new_session,
    response: Some(VideoReply(video_url, Some("Your morphing video is ready!"))),
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
