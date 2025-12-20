// Message Router for Telegram Bot
// Routes incoming messages to appropriate scene handlers

import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import pog
import vibee/bot/scene.{
  type IncomingMessage, type OutgoingMessage, type Scene, type UserSession,
  Avatar, AvatarEnterPrompt, AvatarEnterTriggerWord, AvatarGenerating,
  AvatarResult, AvatarStart, AvatarTrainingComplete, AvatarTrainingStarted,
  AvatarUploadPhotos, AvatarVideo, BRoll, CallbackQuery, Command, Idle,
  ImageToVideo, Main, MainMenu, Morphing, NeuroPhoto, NeuroPhotoEnterPrompt,
  NeuroPhotoGenerating, NeuroPhotoResult, NeuroPhotoSelectModel, PhotoMessage,
  Pricing, Quiz, Subscription,
  TextMessage, TextReply, TextToVideo, TextWithKeyboard, UserSession,
  VoiceClone, button, button_row,
}
import vibee/bot/scene_handler
import vibee/bot/scenes/avatar_video as avatar_video_scene
import vibee/bot/scenes/broll as broll_scene
import vibee/bot/scenes/image_to_video as image_to_video_scene
import vibee/bot/scenes/morphing as morphing_scene
import vibee/bot/scenes/pricing as pricing_scene
import vibee/bot/scenes/quiz_scene
import vibee/bot/scenes/subscription_scene
import vibee/bot/scenes/text_to_video as text_to_video_scene
import vibee/sales/paywall
import vibee/sales/proposal_generator
import vibee/sales/lead_service
import vibee/bot/scenes/voice_clone as voice_clone_scene
import vibee/bot/session_store

// ============================================================
// Types
// ============================================================

pub type RouterError {
  SessionError(session_store.SessionError)
  SceneError(String)
  UnhandledMessage
}

pub type RouterResult {
  RouterResult(session: UserSession, response: Option(OutgoingMessage))
}

// ============================================================
// Main Router
// ============================================================

/// Main entry point - route message based on current scene
pub fn route_message(
  pool: pog.Connection,
  user_id: Int,
  chat_id: String,
  username: Option(String),
  message: IncomingMessage,
) -> Result(RouterResult, RouterError) {
  // Get or create session
  let session_result =
    session_store.get_or_create_session(pool, user_id, chat_id, username)

  case session_result {
    Error(e) -> Error(SessionError(e))
    Ok(session) -> {
      // Handle commands first (they work from any scene)
      case message {
        Command(cmd, _args) -> handle_command(pool, session, cmd)
        _ -> route_to_scene(pool, session, message)
      }
    }
  }
}

/// Handle global commands
fn handle_command(
  pool: pog.Connection,
  session: UserSession,
  cmd: String,
) -> Result(RouterResult, RouterError) {
  case cmd {
    "start" | "menu" -> {
      let new_session = scene.set_scene(session, Main(MainMenu))
      let _ = session_store.save_session(pool, new_session)
      Ok(RouterResult(
        session: new_session,
        response: Some(main_menu_response()),
      ))
    }

    "neurophoto" | "neuro" -> {
      let new_session = scene.set_scene(session, NeuroPhoto(NeuroPhotoSelectModel))
      let _ = session_store.save_session(pool, new_session)
      Ok(RouterResult(
        session: new_session,
        response: Some(neuro_photo_select_model_response()),
      ))
    }

    "avatar" -> {
      let new_session = scene.set_scene(session, Avatar(AvatarStart))
      let _ = session_store.save_session(pool, new_session)
      Ok(RouterResult(
        session: new_session,
        response: Some(avatar_start_response()),
      ))
    }

    "video" | "t2v" -> {
      case text_to_video_scene.enter(session) {
        Ok(result) -> {
          let _ = session_store.save_session(pool, result.session)
          Ok(RouterResult(
            session: result.session,
            response: result.response,
          ))
        }
        Error(_) -> Error(SceneError("Failed to enter TextToVideo scene"))
      }
    }

    "animate" | "i2v" -> {
      case image_to_video_scene.enter(session) {
        Ok(result) -> {
          let _ = session_store.save_session(pool, result.session)
          Ok(RouterResult(
            session: result.session,
            response: result.response,
          ))
        }
        Error(_) -> Error(SceneError("Failed to enter ImageToVideo scene"))
      }
    }

    "morph" | "morphing" -> {
      case morphing_scene.enter(session) {
        Ok(result) -> {
          let _ = session_store.save_session(pool, result.session)
          Ok(RouterResult(
            session: result.session,
            response: result.response,
          ))
        }
        Error(_) -> Error(SceneError("Failed to enter Morphing scene"))
      }
    }

    "broll" -> {
      case broll_scene.enter(session) {
        Ok(result) -> {
          let _ = session_store.save_session(pool, result.session)
          Ok(RouterResult(
            session: result.session,
            response: result.response,
          ))
        }
        Error(_) -> Error(SceneError("Failed to enter BRoll scene"))
      }
    }

    "talking" | "avatar_video" -> {
      case avatar_video_scene.enter(session) {
        Ok(result) -> {
          let _ = session_store.save_session(pool, result.session)
          Ok(RouterResult(
            session: result.session,
            response: result.response,
          ))
        }
        Error(_) -> Error(SceneError("Failed to enter AvatarVideo scene"))
      }
    }

    "voice" | "clone" -> {
      case voice_clone_scene.enter(session) {
        Ok(result) -> {
          let _ = session_store.save_session(pool, result.session)
          Ok(RouterResult(
            session: result.session,
            response: result.response,
          ))
        }
        Error(_) -> Error(SceneError("Failed to enter VoiceClone scene"))
      }
    }

    "pricing" | "tariffs" | "prices" -> {
      let result = pricing_scene.handle_pricing_command(session)
      let _ = session_store.save_session(pool, result.session)
      Ok(RouterResult(
        session: result.session,
        response: result.response,
      ))
    }

    "quiz" -> {
      let result = quiz_scene.handle_quiz_command(session)
      let _ = session_store.save_session(pool, result.session)
      Ok(RouterResult(
        session: result.session,
        response: result.response,
      ))
    }

    "subscribe" | "mystatus" | "status" -> {
      let result = subscription_scene.handle_status_command(session)
      let _ = session_store.save_session(pool, result.session)
      Ok(RouterResult(
        session: result.session,
        response: result.response,
      ))
    }

    "proposal" | "kp" -> {
      // Генерируем КП для пользователя на основе квиза или дефолтного тарифа
      case lead_service.get_or_create_lead(session.user_id, session.username, None, None, Some("bot")) {
        Error(_) -> {
          Ok(RouterResult(
            session: session,
            response: Some(TextReply("Ошибка создания профиля. Попробуйте позже.")),
          ))
        }
        Ok(lead) -> {
          // Определяем продукт из квиза или дефолтный (middle)
          let product_code = case lead.recommended_product_id {
            Some(1) -> "junior"
            Some(2) -> "middle"
            Some(3) -> "senior"
            _ -> "middle"
          }

          case proposal_generator.generate_for_lead(lead, product_code, 0, "ru") {
            Error(_) -> {
              Ok(RouterResult(
                session: session,
                response: Some(TextReply("Ошибка генерации КП. Попробуйте /quiz для подбора тарифа.")),
              ))
            }
            Ok(generated) -> {
              Ok(RouterResult(
                session: session,
                response: Some(TextReply(generated.text_content)),
              ))
            }
          }
        }
      }
    }

    "cancel" | "reset" -> {
      let new_session = scene.reset_session(session)
      let _ = session_store.save_session(pool, new_session)
      Ok(RouterResult(
        session: new_session,
        response: Some(TextReply("Cancelled. Send /menu to start again.")),
      ))
    }

    "help" -> {
      Ok(RouterResult(session: session, response: Some(help_response())))
    }

    _ -> {
      // Unknown command, show help
      Ok(RouterResult(
        session: session,
        response: Some(TextReply(
          "Unknown command. Send /help for available commands.",
        )),
      ))
    }
  }
}

/// Route to scene handler based on current scene
fn route_to_scene(
  pool: pog.Connection,
  session: UserSession,
  message: IncomingMessage,
) -> Result(RouterResult, RouterError) {
  case session.scene {
    Main(Idle) -> handle_idle(pool, session, message)
    Main(MainMenu) -> handle_main_menu(pool, session, message)
    NeuroPhoto(scene_state) ->
      handle_neuro_photo(pool, session, scene_state, message)
    Avatar(scene_state) -> handle_avatar(pool, session, scene_state, message)
    TextToVideo(_) -> handle_new_scene(pool, session, message, text_to_video_scene.handle_message, text_to_video_scene.handle_callback)
    ImageToVideo(_) -> handle_new_scene(pool, session, message, image_to_video_scene.handle_message, image_to_video_scene.handle_callback)
    Morphing(_) -> handle_new_scene(pool, session, message, morphing_scene.handle_message, morphing_scene.handle_callback)
    BRoll(_) -> handle_new_scene(pool, session, message, broll_scene.handle_message, broll_scene.handle_callback)
    AvatarVideo(_) -> handle_new_scene(pool, session, message, avatar_video_scene.handle_message, avatar_video_scene.handle_callback)
    VoiceClone(_) -> handle_new_scene(pool, session, message, voice_clone_scene.handle_message, voice_clone_scene.handle_callback)
    // Sales scenes
    Pricing(_) -> handle_pricing_scene(pool, session, message)
    Quiz(_) -> handle_quiz_scene(pool, session, message)
    Subscription(_) -> handle_subscription_scene(pool, session, message)
  }
}

/// Generic handler for new scenes using trait pattern
fn handle_new_scene(
  pool: pog.Connection,
  session: UserSession,
  message: IncomingMessage,
  handle_message_fn: fn(UserSession, IncomingMessage) -> Result(scene_handler.SceneResult, scene_handler.SceneError),
  handle_callback_fn: fn(UserSession, String) -> Result(scene_handler.SceneResult, scene_handler.SceneError),
) -> Result(RouterResult, RouterError) {
  let result = case message {
    CallbackQuery(data) -> handle_callback_fn(session, data)
    _ -> handle_message_fn(session, message)
  }

  case result {
    Ok(scene_result) -> {
      let _ = session_store.save_session(pool, scene_result.session)
      Ok(RouterResult(
        session: scene_result.session,
        response: scene_result.response,
      ))
    }
    Error(e) -> Error(SceneError("Scene error: " <> scene_error_to_string(e)))
  }
}

fn scene_error_to_string(e: scene_handler.SceneError) -> String {
  case e {
    scene_handler.InvalidState(msg) -> "Invalid state: " <> msg
    scene_handler.InvalidInput(msg) -> "Invalid input: " <> msg
    scene_handler.ServiceError(msg) -> "Service error: " <> msg
    scene_handler.NotImplemented -> "Not implemented"
  }
}

// ============================================================
// Sales Scene Handlers
// ============================================================

fn handle_pricing_scene(
  pool: pog.Connection,
  session: UserSession,
  message: IncomingMessage,
) -> Result(RouterResult, RouterError) {
  case message {
    CallbackQuery(data) -> {
      let result = pricing_scene.handle_callback(session, data)
      let _ = session_store.save_session(pool, result.session)
      Ok(RouterResult(session: result.session, response: result.response))
    }
    TextMessage(_) -> {
      // Re-show pricing
      let result = pricing_scene.handle_pricing_command(session)
      let _ = session_store.save_session(pool, result.session)
      Ok(RouterResult(session: result.session, response: result.response))
    }
    _ -> Ok(RouterResult(session: session, response: None))
  }
}

fn handle_quiz_scene(
  pool: pog.Connection,
  session: UserSession,
  message: IncomingMessage,
) -> Result(RouterResult, RouterError) {
  case message {
    CallbackQuery(data) -> {
      let result = quiz_scene.handle_callback(session, data)
      let _ = session_store.save_session(pool, result.session)
      Ok(RouterResult(session: result.session, response: result.response))
    }
    TextMessage(_) -> {
      // Re-show current question
      let result = quiz_scene.handle_quiz_command(session)
      let _ = session_store.save_session(pool, result.session)
      Ok(RouterResult(session: result.session, response: result.response))
    }
    _ -> Ok(RouterResult(session: session, response: None))
  }
}

fn handle_subscription_scene(
  pool: pog.Connection,
  session: UserSession,
  message: IncomingMessage,
) -> Result(RouterResult, RouterError) {
  case message {
    CallbackQuery(data) -> {
      let result = subscription_scene.handle_callback(session, data)
      let _ = session_store.save_session(pool, result.session)
      Ok(RouterResult(session: result.session, response: result.response))
    }
    TextMessage(_) -> {
      // Re-show status
      let result = subscription_scene.handle_status_command(session)
      let _ = session_store.save_session(pool, result.session)
      Ok(RouterResult(session: result.session, response: result.response))
    }
    _ -> Ok(RouterResult(session: session, response: None))
  }
}

// ============================================================
// Main Scene Handlers
// ============================================================

fn handle_idle(
  pool: pog.Connection,
  session: UserSession,
  message: IncomingMessage,
) -> Result(RouterResult, RouterError) {
  case message {
    TextMessage(text) -> {
      // Check for keywords
      let lower = string.lowercase(text)
      case
        string.contains(lower, "нейрофото")
        || string.contains(lower, "neuro")
      {
        True -> {
          let new_session =
            scene.set_scene(session, NeuroPhoto(NeuroPhotoSelectModel))
          let _ = session_store.save_session(pool, new_session)
          Ok(RouterResult(
            session: new_session,
            response: Some(neuro_photo_select_model_response()),
          ))
        }
        False ->
          case
            string.contains(lower, "аватар") || string.contains(lower, "avatar")
          {
            True -> {
              let new_session = scene.set_scene(session, Avatar(AvatarStart))
              let _ = session_store.save_session(pool, new_session)
              Ok(RouterResult(
                session: new_session,
                response: Some(avatar_start_response()),
              ))
            }
            False -> {
              // Show menu
              let new_session = scene.set_scene(session, Main(MainMenu))
              let _ = session_store.save_session(pool, new_session)
              Ok(RouterResult(
                session: new_session,
                response: Some(main_menu_response()),
              ))
            }
          }
      }
    }
    CallbackQuery(data) -> handle_main_menu_callback(pool, session, data)
    _ -> Ok(RouterResult(session: session, response: None))
  }
}

fn handle_main_menu(
  pool: pog.Connection,
  session: UserSession,
  message: IncomingMessage,
) -> Result(RouterResult, RouterError) {
  case message {
    CallbackQuery(data) -> handle_main_menu_callback(pool, session, data)
    TextMessage(_) -> {
      Ok(RouterResult(
        session: session,
        response: Some(main_menu_response()),
      ))
    }
    _ -> Ok(RouterResult(session: session, response: None))
  }
}

fn handle_main_menu_callback(
  pool: pog.Connection,
  session: UserSession,
  data: String,
) -> Result(RouterResult, RouterError) {
  case data {
    "neuro_photo" -> {
      let new_session =
        scene.set_scene(session, NeuroPhoto(NeuroPhotoSelectModel))
      let _ = session_store.save_session(pool, new_session)
      Ok(RouterResult(
        session: new_session,
        response: Some(neuro_photo_select_model_response()),
      ))
    }
    "avatar" -> {
      let new_session = scene.set_scene(session, Avatar(AvatarStart))
      let _ = session_store.save_session(pool, new_session)
      Ok(RouterResult(
        session: new_session,
        response: Some(avatar_start_response()),
      ))
    }
    "text_to_video" -> {
      case text_to_video_scene.enter(session) {
        Ok(result) -> {
          let _ = session_store.save_session(pool, result.session)
          Ok(RouterResult(session: result.session, response: result.response))
        }
        Error(_) -> Error(SceneError("Failed to enter TextToVideo scene"))
      }
    }
    "image_to_video" -> {
      case image_to_video_scene.enter(session) {
        Ok(result) -> {
          let _ = session_store.save_session(pool, result.session)
          Ok(RouterResult(session: result.session, response: result.response))
        }
        Error(_) -> Error(SceneError("Failed to enter ImageToVideo scene"))
      }
    }
    "morphing" -> {
      case morphing_scene.enter(session) {
        Ok(result) -> {
          let _ = session_store.save_session(pool, result.session)
          Ok(RouterResult(session: result.session, response: result.response))
        }
        Error(_) -> Error(SceneError("Failed to enter Morphing scene"))
      }
    }
    "broll" -> {
      case broll_scene.enter(session) {
        Ok(result) -> {
          let _ = session_store.save_session(pool, result.session)
          Ok(RouterResult(session: result.session, response: result.response))
        }
        Error(_) -> Error(SceneError("Failed to enter BRoll scene"))
      }
    }
    "avatar_video" -> {
      case avatar_video_scene.enter(session) {
        Ok(result) -> {
          let _ = session_store.save_session(pool, result.session)
          Ok(RouterResult(session: result.session, response: result.response))
        }
        Error(_) -> Error(SceneError("Failed to enter AvatarVideo scene"))
      }
    }
    "voice_clone" -> {
      case voice_clone_scene.enter(session) {
        Ok(result) -> {
          let _ = session_store.save_session(pool, result.session)
          Ok(RouterResult(session: result.session, response: result.response))
        }
        Error(_) -> Error(SceneError("Failed to enter VoiceClone scene"))
      }
    }
    "pricing" -> {
      let result = pricing_scene.handle_pricing_command(session)
      let _ = session_store.save_session(pool, result.session)
      Ok(RouterResult(session: result.session, response: result.response))
    }
    "quiz" -> {
      let result = quiz_scene.handle_quiz_command(session)
      let _ = session_store.save_session(pool, result.session)
      Ok(RouterResult(session: result.session, response: result.response))
    }
    "subscription" -> {
      let result = subscription_scene.handle_status_command(session)
      let _ = session_store.save_session(pool, result.session)
      Ok(RouterResult(session: result.session, response: result.response))
    }
    _ -> Ok(RouterResult(session: session, response: None))
  }
}

// ============================================================
// NeuroPhoto Scene Handlers
// ============================================================

fn handle_neuro_photo(
  pool: pog.Connection,
  session: UserSession,
  scene_state: scene.NeuroPhotoScene,
  message: IncomingMessage,
) -> Result(RouterResult, RouterError) {
  case scene_state {
    NeuroPhotoSelectModel -> handle_neuro_photo_select_model(pool, session, message)
    NeuroPhotoEnterPrompt(model) ->
      handle_neuro_photo_enter_prompt(pool, session, model, message)
    NeuroPhotoGenerating(model, prompt, job_id) -> {
      // Already generating, just acknowledge
      Ok(RouterResult(
        session: session,
        response: Some(TextReply(
          "Still generating your image... Please wait.",
        )),
      ))
    }
    NeuroPhotoResult(_images) -> {
      // Show result and offer to generate more
      let new_session =
        scene.set_scene(session, NeuroPhoto(NeuroPhotoSelectModel))
      let _ = session_store.save_session(pool, new_session)
      Ok(RouterResult(
        session: new_session,
        response: Some(neuro_photo_select_model_response()),
      ))
    }
  }
}

fn handle_neuro_photo_select_model(
  pool: pog.Connection,
  session: UserSession,
  message: IncomingMessage,
) -> Result(RouterResult, RouterError) {
  case message {
    CallbackQuery(data) -> {
      case string.starts_with(data, "np_model_") {
        True -> {
          let model = string.drop_start(data, 9)
          let new_session =
            scene.set_scene(session, NeuroPhoto(NeuroPhotoEnterPrompt(model)))
          let _ = session_store.save_session(pool, new_session)
          Ok(RouterResult(
            session: new_session,
            response: Some(TextReply(
              "Model: " <> model <> "\n\nNow enter your prompt for the image:",
            )),
          ))
        }
        False -> Ok(RouterResult(session: session, response: None))
      }
    }
    _ -> {
      Ok(RouterResult(
        session: session,
        response: Some(neuro_photo_select_model_response()),
      ))
    }
  }
}

fn handle_neuro_photo_enter_prompt(
  pool: pog.Connection,
  session: UserSession,
  model: String,
  message: IncomingMessage,
) -> Result(RouterResult, RouterError) {
  case message {
    TextMessage(prompt) -> {
      // Check paywall before starting generation
      case paywall.check_access(session.user_id, paywall.Generation) {
        paywall.AccessGranted(_) -> {
          // Start generation
          // In real implementation, call AI service and get job_id
          let job_id = "job_" <> model <> "_placeholder"
          let new_session =
            scene.set_scene(
              session,
              NeuroPhoto(NeuroPhotoGenerating(model, prompt, job_id)),
            )
          let _ = session_store.save_session(pool, new_session)

          // Record usage after successful generation start
          let _ = paywall.record_usage(session.user_id, paywall.Generation)

          // TODO: Actually call FAL.ai or other service here
          // For now, just acknowledge
          Ok(RouterResult(
            session: new_session,
            response: Some(TextReply(
              "Generating image with " <> model <> "...\n\nPrompt: " <> prompt,
            )),
          ))
        }
        access_result -> {
          // Access denied - show paywall message
          let message = paywall.get_access_message(access_result, "ru")
            <> "\n\n💎 /pricing - просмотр тарифов\n🎯 /quiz - подобрать тариф"
          Ok(RouterResult(
            session: session,
            response: Some(TextReply(message)),
          ))
        }
      }
    }
    CallbackQuery("back") -> {
      let new_session =
        scene.set_scene(session, NeuroPhoto(NeuroPhotoSelectModel))
      let _ = session_store.save_session(pool, new_session)
      Ok(RouterResult(
        session: new_session,
        response: Some(neuro_photo_select_model_response()),
      ))
    }
    _ -> {
      Ok(RouterResult(
        session: session,
        response: Some(TextReply("Please enter your prompt:")),
      ))
    }
  }
}

// ============================================================
// Avatar Scene Handlers
// ============================================================

fn handle_avatar(
  pool: pog.Connection,
  session: UserSession,
  scene_state: scene.AvatarScene,
  message: IncomingMessage,
) -> Result(RouterResult, RouterError) {
  case scene_state {
    AvatarStart -> handle_avatar_start(pool, session, message)
    AvatarUploadPhotos(collected, required) ->
      handle_avatar_upload_photos(pool, session, collected, required, message)
    AvatarEnterTriggerWord(photos_url) ->
      handle_avatar_enter_trigger(pool, session, photos_url, message)
    AvatarTrainingStarted(training_id, trigger_word) -> {
      Ok(RouterResult(
        session: session,
        response: Some(TextReply(
          "Training in progress... This may take 10-20 minutes.",
        )),
      ))
    }
    AvatarTrainingComplete(lora_url, trigger_word) ->
      handle_avatar_training_complete(
        pool,
        session,
        lora_url,
        trigger_word,
        message,
      )
    AvatarEnterPrompt(lora_url, trigger_word) ->
      handle_avatar_enter_prompt(pool, session, lora_url, trigger_word, message)
    AvatarGenerating(_lora_url, _trigger_word, _prompt, _job_id) -> {
      Ok(RouterResult(
        session: session,
        response: Some(TextReply("Generating your avatar image... Please wait.")),
      ))
    }
    AvatarResult(_images) -> {
      let new_session =
        scene.set_scene(session, Avatar(AvatarEnterPrompt("", "")))
      let _ = session_store.save_session(pool, new_session)
      Ok(RouterResult(
        session: new_session,
        response: Some(TextReply("Enter another prompt or /menu to go back:")),
      ))
    }
  }
}

fn handle_avatar_start(
  pool: pog.Connection,
  session: UserSession,
  message: IncomingMessage,
) -> Result(RouterResult, RouterError) {
  case message {
    CallbackQuery("avatar_start") | TextMessage(_) -> {
      let new_session =
        scene.set_scene(session, Avatar(AvatarUploadPhotos(0, 10)))
      let _ = session_store.save_session(pool, new_session)
      Ok(RouterResult(
        session: new_session,
        response: Some(TextReply(
          "Great! Please upload 10-20 photos of yourself.\n\n"
          <> "Tips for best results:\n"
          <> "- Use clear, well-lit photos\n"
          <> "- Include different angles and expressions\n"
          <> "- Avoid group photos\n\n"
          <> "Uploaded: 0/10",
        )),
      ))
    }
    _ -> {
      Ok(RouterResult(
        session: session,
        response: Some(avatar_start_response()),
      ))
    }
  }
}

fn handle_avatar_upload_photos(
  pool: pog.Connection,
  session: UserSession,
  collected: Int,
  required: Int,
  message: IncomingMessage,
) -> Result(RouterResult, RouterError) {
  case message {
    PhotoMessage(file_id, _caption) -> {
      // TODO: Save photo to database
      let new_collected = collected + 1

      case new_collected >= required {
        True -> {
          // Enough photos, ask for trigger word
          let photos_url = "placeholder_zip_url"
          let new_session =
            scene.set_scene(session, Avatar(AvatarEnterTriggerWord(photos_url)))
          let _ = session_store.save_session(pool, new_session)
          Ok(RouterResult(
            session: new_session,
            response: Some(TextReply(
              "All photos uploaded!\n\n"
              <> "Now enter a trigger word for your avatar.\n"
              <> "This word will activate your style in prompts.\n\n"
              <> "Example: MYAVATAR, JOHNSTYLE, etc.",
            )),
          ))
        }
        False -> {
          let new_session =
            scene.set_scene(
              session,
              Avatar(AvatarUploadPhotos(new_collected, required)),
            )
          let _ = session_store.save_session(pool, new_session)
          Ok(RouterResult(
            session: new_session,
            response: Some(TextReply(
              "Photo received! Uploaded: "
              <> string.inspect(new_collected)
              <> "/"
              <> string.inspect(required),
            )),
          ))
        }
      }
    }
    CallbackQuery("done_photos") -> {
      case collected >= 5 {
        True -> {
          let photos_url = "placeholder_zip_url"
          let new_session =
            scene.set_scene(session, Avatar(AvatarEnterTriggerWord(photos_url)))
          let _ = session_store.save_session(pool, new_session)
          Ok(RouterResult(
            session: new_session,
            response: Some(TextReply(
              "Using " <> string.inspect(collected) <> " photos.\n\n"
              <> "Now enter a trigger word for your avatar:",
            )),
          ))
        }
        False -> {
          Ok(RouterResult(
            session: session,
            response: Some(TextReply(
              "Please upload at least 5 photos. Currently: "
              <> string.inspect(collected),
            )),
          ))
        }
      }
    }
    _ -> {
      Ok(RouterResult(
        session: session,
        response: Some(TextReply(
          "Please upload photos. Uploaded: "
          <> string.inspect(collected)
          <> "/"
          <> string.inspect(required),
        )),
      ))
    }
  }
}

fn handle_avatar_enter_trigger(
  pool: pog.Connection,
  session: UserSession,
  photos_url: String,
  message: IncomingMessage,
) -> Result(RouterResult, RouterError) {
  case message {
    TextMessage(trigger_word) -> {
      // Validate trigger word
      let clean_trigger = string.uppercase(string.trim(trigger_word))
      case string.length(clean_trigger) >= 3 {
        True -> {
          // Start training
          // TODO: Actually call Replicate LoRA training
          let training_id = "training_placeholder"
          let new_session =
            scene.set_scene(
              session,
              Avatar(AvatarTrainingStarted(training_id, clean_trigger)),
            )
          let _ = session_store.save_session(pool, new_session)
          Ok(RouterResult(
            session: new_session,
            response: Some(TextReply(
              "Starting training with trigger word: " <> clean_trigger <> "\n\n"
              <> "This will take 10-20 minutes. "
              <> "I'll notify you when it's ready!",
            )),
          ))
        }
        False -> {
          Ok(RouterResult(
            session: session,
            response: Some(TextReply(
              "Trigger word must be at least 3 characters. Please try again:",
            )),
          ))
        }
      }
    }
    _ -> {
      Ok(RouterResult(
        session: session,
        response: Some(TextReply("Please enter a trigger word:")),
      ))
    }
  }
}

fn handle_avatar_training_complete(
  pool: pog.Connection,
  session: UserSession,
  lora_url: String,
  trigger_word: String,
  message: IncomingMessage,
) -> Result(RouterResult, RouterError) {
  case message {
    CallbackQuery("generate") | TextMessage(_) -> {
      let new_session =
        scene.set_scene(session, Avatar(AvatarEnterPrompt(lora_url, trigger_word)))
      let _ = session_store.save_session(pool, new_session)
      Ok(RouterResult(
        session: new_session,
        response: Some(TextReply(
          "Your avatar model is ready!\n\n"
          <> "Trigger word: "
          <> trigger_word
          <> "\n\n"
          <> "Enter a prompt using your trigger word.\n"
          <> "Example: portrait of "
          <> trigger_word
          <> " as a superhero",
        )),
      ))
    }
    _ -> {
      Ok(RouterResult(
        session: session,
        response: Some(TextWithKeyboard(
          "Training complete! Ready to generate?",
          [[button("Generate Avatar", "generate")]],
        )),
      ))
    }
  }
}

fn handle_avatar_enter_prompt(
  pool: pog.Connection,
  session: UserSession,
  lora_url: String,
  trigger_word: String,
  message: IncomingMessage,
) -> Result(RouterResult, RouterError) {
  case message {
    TextMessage(prompt) -> {
      // Start generation
      let job_id = "avatar_gen_placeholder"
      let new_session =
        scene.set_scene(
          session,
          Avatar(AvatarGenerating(lora_url, trigger_word, prompt, job_id)),
        )
      let _ = session_store.save_session(pool, new_session)

      // TODO: Actually call FAL.ai with LoRA
      Ok(RouterResult(
        session: new_session,
        response: Some(TextReply("Generating avatar image...\n\nPrompt: " <> prompt)),
      ))
    }
    _ -> {
      Ok(RouterResult(
        session: session,
        response: Some(TextReply(
          "Enter a prompt using your trigger word (" <> trigger_word <> "):",
        )),
      ))
    }
  }
}

// ============================================================
// Response Builders
// ============================================================

fn main_menu_response() -> OutgoingMessage {
  TextWithKeyboard(
    "Welcome to VIBEE Bot!\n\nChoose an option:",
    [
      [button("NeuroPhoto", "neuro_photo"), button("Text to Video", "text_to_video")],
      [button("Animate Image", "image_to_video"), button("Morphing", "morphing")],
      [button("B-Roll", "broll"), button("Talking Avatar", "avatar_video")],
      [button("Train Avatar", "avatar"), button("Voice Clone", "voice_clone")],
      [button("💎 Тарифы", "pricing"), button("🎯 Квиз", "quiz")],
      [button("📊 Моя подписка", "subscription")],
    ],
  )
}

fn neuro_photo_select_model_response() -> OutgoingMessage {
  TextWithKeyboard(
    "NeuroPhoto - AI Image Generation\n\nSelect a model:",
    [
      [button("FLUX LoRA (Best)", "np_model_flux-lora")],
      [button("Nano Banana Pro (Fast)", "np_model_nano-banana")],
      [button("FLUX Kontext", "np_model_flux-kontext")],
      [button("Back to Menu", "back")],
    ],
  )
}

fn avatar_start_response() -> OutgoingMessage {
  TextWithKeyboard(
    "Digital Avatar Training\n\n"
    <> "Train a custom AI model with your photos.\n"
    <> "You'll be able to generate images of yourself in any style!\n\n"
    <> "Requirements:\n"
    <> "- 10-20 photos of yourself\n"
    <> "- Clear, well-lit images\n"
    <> "- Different angles and expressions",
    [[button("Start Training", "avatar_start")], [button("Back to Menu", "back")]],
  )
}

fn help_response() -> OutgoingMessage {
  TextReply(
    "VIBEE Bot Commands:\n\n"
    <> "**Images:**\n"
    <> "/neurophoto - Generate AI images\n"
    <> "/avatar - Train your digital avatar\n\n"
    <> "**Videos:**\n"
    <> "/video - Text to video (Kling, Veo3)\n"
    <> "/animate - Image to video animation\n"
    <> "/morph - Morphing transitions\n"
    <> "/broll - Auto B-Roll generation\n"
    <> "/talking - Talking avatar video\n\n"
    <> "**Audio:**\n"
    <> "/voice - Voice cloning\n\n"
    <> "**Subscription:**\n"
    <> "/pricing - View pricing plans\n"
    <> "/quiz - Find best plan for you\n"
    <> "/subscribe - Manage subscription\n"
    <> "/proposal - Get personalized offer\n\n"
    <> "**General:**\n"
    <> "/menu - Main menu\n"
    <> "/cancel - Cancel current action\n"
    <> "/help - Show this help",
  )
}
