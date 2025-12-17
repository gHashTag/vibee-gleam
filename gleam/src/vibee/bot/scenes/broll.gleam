// B-Roll Scene Handler
// Generates B-Roll video clips from text script

import gleam/dict
import gleam/option.{None, Some}
import vibee/bot/scene.{
  type IncomingMessage, type OutgoingMessage, type Scene, type UserSession,
  CallbackQuery, Command, InlineButton, Main, Idle, TextMessage,
  TextReply, BRoll, BRollEnterScript, BRollSelectStyle, BRollGenerating,
  BRollResult, TextWithKeyboard,
  button, set_scene,
}
import vibee/bot/scene_handler.{
  type NextAction, type SceneError, type SceneResult, InvalidInput, InvalidState,
  JobBRoll, NoAction, SceneResult, StartJob,
}

// ============================================================
// Scene Entry Point
// ============================================================

/// Handle /broll command - start B-Roll generation flow
pub fn enter(session: UserSession) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, BRoll(BRollEnterScript))

  Ok(SceneResult(
    session: new_session,
    response: Some(TextReply(
      "Let's generate B-Roll clips!\n\n" <>
      "Enter a script or topic, and I'll create matching video clips.\n\n" <>
      "Example:\n" <>
      "\"Morning routine: wake up, coffee, exercise, work\"\n\n" <>
      "Type /cancel to go back."
    )),
    next_action: NoAction,
  ))
}

// ============================================================
// Message Handling
// ============================================================

/// Handle messages in BRoll scene
pub fn handle_message(
  session: UserSession,
  message: IncomingMessage,
) -> Result(SceneResult, SceneError) {
  case session.scene {
    BRoll(BRollEnterScript) ->
      handle_script_input(session, message)

    BRoll(BRollSelectStyle(script)) ->
      handle_style_selection(session, message, script)

    BRoll(_) ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please wait for the current operation to complete.")),
        next_action: NoAction,
      ))

    _ -> Error(InvalidState("Not in BRoll scene"))
  }
}

/// Handle script input
fn handle_script_input(
  session: UserSession,
  message: IncomingMessage,
) -> Result(SceneResult, SceneError) {
  case message {
    TextMessage(script) -> {
      case validate_script(script) {
        Ok(_) -> {
          let new_session = set_scene(session, BRoll(BRollSelectStyle(script)))

          let keyboard = [
            [button("Cinematic", "broll:cinematic")],
            [button("Documentary", "broll:documentary")],
            [button("Social Media", "broll:social")],
            [button("Corporate", "broll:corporate")],
            [button("Cancel", "cancel")],
          ]

          Ok(SceneResult(
            session: new_session,
            response: Some(TextWithKeyboard(
              "Script received!\n\n" <>
              "Choose video style:",
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
        response: Some(TextReply("Please enter a script or topic for B-Roll generation.")),
        next_action: NoAction,
      ))
  }
}

/// Handle style selection message
fn handle_style_selection(
  session: UserSession,
  message: IncomingMessage,
  script: String,
) -> Result(SceneResult, SceneError) {
  case message {
    TextMessage(style) -> start_generation(session, script, style)
    Command("cancel", _) -> cancel_scene(session)
    _ ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Please select a style using the buttons.")),
        next_action: NoAction,
      ))
  }
}

// ============================================================
// Callback Handling
// ============================================================

/// Handle callbacks in BRoll scene
pub fn handle_callback(
  session: UserSession,
  data: String,
) -> Result(SceneResult, SceneError) {
  case session.scene, data {
    BRoll(BRollSelectStyle(script)), "broll:cinematic" ->
      start_generation(session, script, "cinematic")

    BRoll(BRollSelectStyle(script)), "broll:documentary" ->
      start_generation(session, script, "documentary")

    BRoll(BRollSelectStyle(script)), "broll:social" ->
      start_generation(session, script, "social_media")

    BRoll(BRollSelectStyle(script)), "broll:corporate" ->
      start_generation(session, script, "corporate")

    _, "cancel" -> cancel_scene(session)

    _, _ -> Error(InvalidInput("Unknown callback: " <> data))
  }
}

// ============================================================
// Generation
// ============================================================

/// Start B-Roll generation
fn start_generation(
  session: UserSession,
  script: String,
  style: String,
) -> Result(SceneResult, SceneError) {
  let job_id = "pending"
  let new_session = set_scene(
    session,
    BRoll(BRollGenerating(script, style, job_id)),
  )

  let params = dict.new()
    |> dict.insert("script", script)
    |> dict.insert("style", style)
    |> dict.insert("chat_id", session.chat_id)

  Ok(SceneResult(
    session: new_session,
    response: Some(TextReply(
      "Generating B-Roll clips...\n\n" <>
      "Style: " <> style <> "\n\n" <>
      "This may take 5-10 minutes. I'll notify you when all clips are ready."
    )),
    next_action: StartJob(JobBRoll, params),
  ))
}

/// Handle generation result
pub fn handle_result(
  session: UserSession,
  video_urls: List(String),
) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, BRoll(BRollResult(video_urls)))

  // Count the clips
  let clip_count = case video_urls {
    [] -> "0"
    [_] -> "1"
    [_, _] -> "2"
    [_, _, _] -> "3"
    _ -> "multiple"
  }

  Ok(SceneResult(
    session: new_session,
    response: Some(TextReply(
      "Your B-Roll clips are ready!\n\n" <>
      "Generated " <> clip_count <> " clips.\n" <>
      "Videos will be sent separately."
    )),
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
// Available Styles
// ============================================================

pub type BRollStyle {
  BRollStyle(id: String, name: String, description: String)
}

pub fn available_styles() -> List(BRollStyle) {
  [
    BRollStyle(
      id: "cinematic",
      name: "Cinematic",
      description: "Film-like quality, dramatic lighting, slow motion",
    ),
    BRollStyle(
      id: "documentary",
      name: "Documentary",
      description: "Natural, realistic, informative",
    ),
    BRollStyle(
      id: "social_media",
      name: "Social Media",
      description: "Fast-paced, trendy, engaging",
    ),
    BRollStyle(
      id: "corporate",
      name: "Corporate",
      description: "Professional, clean, business-oriented",
    ),
  ]
}
