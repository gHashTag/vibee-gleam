// Scene Handler Trait Pattern
// Provides unified interface for all scene handlers

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import vibee/bot/scene.{
  type IncomingMessage, type OutgoingMessage, type Scene, type UserSession,
}

// ============================================================
// Scene Result Types
// ============================================================

/// Result of handling a scene message/callback
pub type SceneResult {
  SceneResult(
    session: UserSession,
    response: Option(OutgoingMessage),
    next_action: NextAction,
  )
}

/// Next action to take after handling
pub type NextAction {
  NoAction
  StartJob(job_type: JobType, params: Dict(String, String))
  PollJob(job_id: String, job_type: JobType)
  SendNotification(chat_id: String, text: String)
}

/// Job types for AI services
pub type JobType {
  JobNeuroPhoto
  JobTextToVideo
  JobImageToVideo
  JobMorphing
  JobBRoll
  JobAvatarVideo
  JobVoiceClone
  JobLoraTraining
}

/// Scene handler errors
pub type SceneError {
  InvalidState(String)
  InvalidInput(String)
  ServiceError(String)
  NotImplemented
}

// ============================================================
// Scene Handler Trait (via functions)
// ============================================================

/// Scene handler definition
pub type SceneHandler {
  SceneHandler(
    name: String,
    commands: List(String),
    handles_scene: fn(Scene) -> Bool,
    handle_message: fn(UserSession, IncomingMessage) ->
      Result(SceneResult, SceneError),
    handle_callback: fn(UserSession, String) -> Result(SceneResult, SceneError),
  )
}

// ============================================================
// Handler Registry
// ============================================================

/// Get all registered scene handlers
pub fn get_all_handlers() -> List(SceneHandler) {
  [
    // NeuroPhoto handler
    create_neuro_photo_handler(),
    // Text to Video handler
    create_text_to_video_handler(),
    // Image to Video handler
    create_image_to_video_handler(),
    // Morphing handler
    create_morphing_handler(),
    // BRoll handler
    create_broll_handler(),
    // Avatar Video handler
    create_avatar_video_handler(),
    // Voice Clone handler
    create_voice_clone_handler(),
  ]
}

/// Find handler by command
pub fn find_handler_by_command(cmd: String) -> Option(SceneHandler) {
  get_all_handlers()
  |> list.find(fn(h) { list.contains(h.commands, cmd) })
  |> option.from_result
}

/// Find handler by scene type
pub fn find_handler_by_scene(scene: Scene) -> Option(SceneHandler) {
  get_all_handlers()
  |> list.find(fn(h) { h.handles_scene(scene) })
  |> option.from_result
}

// ============================================================
// Handler Constructors (placeholder implementations)
// Each will be implemented in separate files
// ============================================================

fn create_neuro_photo_handler() -> SceneHandler {
  SceneHandler(
    name: "neuro_photo",
    commands: ["/neurophoto", "/photo"],
    handles_scene: fn(scene) {
      case scene {
        scene.NeuroPhoto(_) -> True
        _ -> False
      }
    },
    handle_message: fn(_session, _message) { Error(NotImplemented) },
    handle_callback: fn(_session, _data) { Error(NotImplemented) },
  )
}

fn create_text_to_video_handler() -> SceneHandler {
  SceneHandler(
    name: "text_to_video",
    commands: ["/video", "/t2v"],
    handles_scene: fn(scene) {
      case scene {
        scene.TextToVideo(_) -> True
        _ -> False
      }
    },
    handle_message: fn(_session, _message) { Error(NotImplemented) },
    handle_callback: fn(_session, _data) { Error(NotImplemented) },
  )
}

fn create_image_to_video_handler() -> SceneHandler {
  SceneHandler(
    name: "image_to_video",
    commands: ["/animate", "/i2v"],
    handles_scene: fn(scene) {
      case scene {
        scene.ImageToVideo(_) -> True
        _ -> False
      }
    },
    handle_message: fn(_session, _message) { Error(NotImplemented) },
    handle_callback: fn(_session, _data) { Error(NotImplemented) },
  )
}

fn create_morphing_handler() -> SceneHandler {
  SceneHandler(
    name: "morphing",
    commands: ["/morph", "/morphing"],
    handles_scene: fn(scene) {
      case scene {
        scene.Morphing(_) -> True
        _ -> False
      }
    },
    handle_message: fn(_session, _message) { Error(NotImplemented) },
    handle_callback: fn(_session, _data) { Error(NotImplemented) },
  )
}

fn create_broll_handler() -> SceneHandler {
  SceneHandler(
    name: "broll",
    commands: ["/broll"],
    handles_scene: fn(scene) {
      case scene {
        scene.BRoll(_) -> True
        _ -> False
      }
    },
    handle_message: fn(_session, _message) { Error(NotImplemented) },
    handle_callback: fn(_session, _data) { Error(NotImplemented) },
  )
}

fn create_avatar_video_handler() -> SceneHandler {
  SceneHandler(
    name: "avatar_video",
    commands: ["/talking", "/avatar_video"],
    handles_scene: fn(scene) {
      case scene {
        scene.AvatarVideo(_) -> True
        _ -> False
      }
    },
    handle_message: fn(_session, _message) { Error(NotImplemented) },
    handle_callback: fn(_session, _data) { Error(NotImplemented) },
  )
}

fn create_voice_clone_handler() -> SceneHandler {
  SceneHandler(
    name: "voice_clone",
    commands: ["/voice", "/clone"],
    handles_scene: fn(scene) {
      case scene {
        scene.VoiceClone(_) -> True
        _ -> False
      }
    },
    handle_message: fn(_session, _message) { Error(NotImplemented) },
    handle_callback: fn(_session, _data) { Error(NotImplemented) },
  )
}

// ============================================================
// Helper Functions
// ============================================================

/// Create a simple text response
pub fn text_response(
  session: UserSession,
  text: String,
) -> Result(SceneResult, SceneError) {
  Ok(SceneResult(
    session: session,
    response: Some(scene.TextReply(text)),
    next_action: NoAction,
  ))
}

/// Create response with keyboard
pub fn keyboard_response(
  session: UserSession,
  text: String,
  keyboard: List(List(scene.InlineButton)),
) -> Result(SceneResult, SceneError) {
  Ok(SceneResult(
    session: session,
    response: Some(scene.TextWithKeyboard(text, keyboard)),
    next_action: NoAction,
  ))
}

/// Create response that starts a job
pub fn start_job_response(
  session: UserSession,
  text: String,
  job_type: JobType,
  params: Dict(String, String),
) -> Result(SceneResult, SceneError) {
  Ok(SceneResult(
    session: session,
    response: Some(scene.TextReply(text)),
    next_action: StartJob(job_type, params),
  ))
}

/// Create no response (silent handling)
pub fn no_response(session: UserSession) -> Result(SceneResult, SceneError) {
  Ok(SceneResult(session: session, response: None, next_action: NoAction))
}

/// Job type to string
pub fn job_type_to_string(job_type: JobType) -> String {
  case job_type {
    JobNeuroPhoto -> "neuro_photo"
    JobTextToVideo -> "text_to_video"
    JobImageToVideo -> "image_to_video"
    JobMorphing -> "morphing"
    JobBRoll -> "broll"
    JobAvatarVideo -> "avatar_video"
    JobVoiceClone -> "voice_clone"
    JobLoraTraining -> "lora_training"
  }
}
