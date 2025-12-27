// Session Store for Telegram Bot
// PostgreSQL-backed user session persistence

import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import pog
import vibee/bot/scene.{
  type Scene, type UserBotSetupScene, type UserSession, Avatar, AvatarEnterPrompt,
  AvatarEnterTriggerWord, AvatarGenerating, AvatarResult, AvatarStart,
  AvatarTrainingComplete, AvatarTrainingStarted, AvatarUploadPhotos, AvatarVideo,
  AvatarVideoEnterScript, AvatarVideoGenerating, AvatarVideoResult,
  AvatarVideoSelectVoice, AvatarVideoUploadPortrait, BRoll, BRollEnterScript,
  BRollGenerating, BRollResult, BRollSelectStyle, Idle, ImageToVideo,
  ImageToVideoEnterPrompt, ImageToVideoGenerating, ImageToVideoResult,
  ImageToVideoUploadImage, Main, MainMenu, Morphing, MorphingEnterStyle,
  MorphingGenerating, MorphingResult, MorphingUploadEnd, MorphingUploadStart,
  NeuroPhoto, NeuroPhotoEnterPrompt, NeuroPhotoGenerating, NeuroPhotoResult,
  NeuroPhotoSelectModel, TextToVideo, TextToVideoEnterPrompt, TextToVideoGenerating,
  TextToVideoResult, TextToVideoSelectProvider, UserBotActivating, UserBotAuthSuccess,
  UserBotCharacterConfigured, UserBotChatsSelected, UserBotComplete,
  UserBotConfigureMode, UserBotEnter2FA, UserBotEnterPhone, UserBotModesConfigured,
  UserBotSelectChats, UserBotSetup, UserBotSetupCharacter, UserBotSetupTriggers,
  UserBotSummary, UserBotTriggersConfigured, UserBotWaitingCode, UserBotWelcome,
  UserSession, VoiceClone, VoiceCloneEnterText, VoiceCloneGenerating,
  VoiceCloneResult, VoiceCloneUploadSample,
}

// ============================================================
// Types
// ============================================================

pub type SessionError {
  SessionNotFound
  SessionDatabaseError(String)
  SessionParseError(String)
}

// ============================================================
// Database Operations
// ============================================================

/// Get session by user_id
pub fn get_session(
  pool: pog.Connection,
  user_id: Int,
) -> Result(UserSession, SessionError) {
  let query =
    "SELECT user_id, chat_id, username, scene, context,
            EXTRACT(EPOCH FROM created_at)::bigint as created_at,
            EXTRACT(EPOCH FROM updated_at)::bigint as updated_at
     FROM user_sessions
     WHERE user_id = $1"

  let result =
    pog.query(query)
    |> pog.parameter(pog.int(user_id))
    |> pog.returning(session_decoder())
    |> pog.execute(pool)

  case result {
    Ok(pog.Returned(_, [session])) -> Ok(session)
    Ok(pog.Returned(_, [])) -> Error(SessionNotFound)
    Ok(pog.Returned(_, _)) -> Error(SessionDatabaseError("Multiple sessions"))
    Error(e) -> Error(SessionDatabaseError(pog_error_to_string(e)))
  }
}

/// Create or update session (upsert)
pub fn save_session(
  pool: pog.Connection,
  session: UserSession,
) -> Result(Nil, SessionError) {
  let scene_json = scene.scene_to_json(session.scene)
  let context_json = context_to_json(session.context)
  let username = option.unwrap(session.username, "")

  let query =
    "INSERT INTO user_sessions (user_id, chat_id, username, scene, context, updated_at)
     VALUES ($1, $2, $3, $4::jsonb, $5::jsonb, NOW())
     ON CONFLICT (user_id) DO UPDATE SET
       chat_id = EXCLUDED.chat_id,
       username = EXCLUDED.username,
       scene = EXCLUDED.scene,
       context = EXCLUDED.context,
       updated_at = NOW()"

  let result =
    pog.query(query)
    |> pog.parameter(pog.int(session.user_id))
    |> pog.parameter(pog.text(session.chat_id))
    |> pog.parameter(pog.text(username))
    |> pog.parameter(pog.text(scene_json))
    |> pog.parameter(pog.text(context_json))
    |> pog.execute(pool)

  case result {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(SessionDatabaseError(pog_error_to_string(e)))
  }
}

/// Delete session
pub fn delete_session(
  pool: pog.Connection,
  user_id: Int,
) -> Result(Nil, SessionError) {
  let query = "DELETE FROM user_sessions WHERE user_id = $1"

  let result =
    pog.query(query)
    |> pog.parameter(pog.int(user_id))
    |> pog.execute(pool)

  case result {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(SessionDatabaseError(pog_error_to_string(e)))
  }
}

/// Get or create session
pub fn get_or_create_session(
  pool: pog.Connection,
  user_id: Int,
  chat_id: String,
  username: Option(String),
) -> Result(UserSession, SessionError) {
  case get_session(pool, user_id) {
    Ok(session) -> Ok(session)
    Error(SessionNotFound) -> {
      let new_session = case username {
        Some(u) -> scene.new_session_with_username(user_id, chat_id, u)
        None -> scene.new_session(user_id, chat_id)
      }
      case save_session(pool, new_session) {
        Ok(_) -> Ok(new_session)
        Error(e) -> Error(e)
      }
    }
    Error(e) -> Error(e)
  }
}

/// Update session scene
pub fn update_scene(
  pool: pog.Connection,
  user_id: Int,
  new_scene: Scene,
) -> Result(Nil, SessionError) {
  let scene_json = scene.scene_to_json(new_scene)

  let query =
    "UPDATE user_sessions
     SET scene = $2::jsonb, updated_at = NOW()
     WHERE user_id = $1"

  let result =
    pog.query(query)
    |> pog.parameter(pog.int(user_id))
    |> pog.parameter(pog.text(scene_json))
    |> pog.execute(pool)

  case result {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(SessionDatabaseError(pog_error_to_string(e)))
  }
}

/// Get all active sessions (for recovery)
pub fn get_active_sessions(
  pool: pog.Connection,
) -> Result(List(UserSession), SessionError) {
  let query =
    "SELECT user_id, chat_id, username, scene, context,
            EXTRACT(EPOCH FROM created_at)::bigint as created_at,
            EXTRACT(EPOCH FROM updated_at)::bigint as updated_at
     FROM user_sessions
     WHERE scene::text NOT LIKE '%\"idle\"%'
     ORDER BY updated_at DESC
     LIMIT 100"

  let result =
    pog.query(query)
    |> pog.returning(session_decoder())
    |> pog.execute(pool)

  case result {
    Ok(pog.Returned(_, sessions)) -> Ok(sessions)
    Error(e) -> Error(SessionDatabaseError(pog_error_to_string(e)))
  }
}

/// Get sessions in processing state (for job polling)
pub fn get_processing_sessions(
  pool: pog.Connection,
) -> Result(List(UserSession), SessionError) {
  let query =
    "SELECT user_id, chat_id, username, scene, context,
            EXTRACT(EPOCH FROM created_at)::bigint as created_at,
            EXTRACT(EPOCH FROM updated_at)::bigint as updated_at
     FROM user_sessions
     WHERE scene::text LIKE '%\"generating\"%'
        OR scene::text LIKE '%\"training\"%'
     ORDER BY updated_at ASC"

  let result =
    pog.query(query)
    |> pog.returning(session_decoder())
    |> pog.execute(pool)

  case result {
    Ok(pog.Returned(_, sessions)) -> Ok(sessions)
    Error(e) -> Error(SessionDatabaseError(pog_error_to_string(e)))
  }
}

// ============================================================
// Decoders
// ============================================================

fn session_decoder() -> Decoder(UserSession) {
  use user_id <- decode.field(0, decode.int)
  use chat_id <- decode.field(1, decode.string)
  use username <- decode.field(2, decode.string)
  use scene_json <- decode.field(3, decode.string)
  use context_json <- decode.field(4, decode.string)
  use created_at <- decode.field(5, decode.int)
  use updated_at <- decode.field(6, decode.int)

  let scene = parse_scene_json(scene_json)
  let context = parse_context_json(context_json)
  let username_opt = case username {
    "" -> None
    u -> Some(u)
  }

  decode.success(UserSession(
    user_id: user_id,
    chat_id: chat_id,
    username: username_opt,
    scene: scene,
    context: context,
    created_at: created_at,
    updated_at: updated_at,
  ))
}

// ============================================================
// JSON Parsing
// ============================================================

fn parse_scene_json(json_str: String) -> Scene {
  // Simple JSON parsing for scene state
  // In production, use proper JSON decoder
  case string.contains(json_str, "\"neuro_photo\"") {
    True -> parse_neuro_photo_scene(json_str)
    False ->
      case string.contains(json_str, "\"text_to_video\"") {
        True -> parse_text_to_video_scene(json_str)
        False ->
          case string.contains(json_str, "\"image_to_video\"") {
            True -> parse_image_to_video_scene(json_str)
            False ->
              case string.contains(json_str, "\"morphing\"") {
                True -> parse_morphing_scene(json_str)
                False ->
                  case string.contains(json_str, "\"broll\"") {
                    True -> parse_broll_scene(json_str)
                    False ->
                      case string.contains(json_str, "\"avatar_video\"") {
                        True -> parse_avatar_video_scene(json_str)
                        False ->
                          case string.contains(json_str, "\"voice_clone\"") {
                            True -> parse_voice_clone_scene(json_str)
                            False ->
                              case string.contains(json_str, "\"avatar\"") {
                                True -> parse_avatar_scene(json_str)
                                False ->
                                  case string.contains(json_str, "\"userbot_setup\"") {
                                    True -> parse_userbot_setup_scene(json_str)
                                    False -> Main(Idle)
                                  }
                              }
                          }
                      }
                  }
              }
          }
      }
  }
}

fn parse_neuro_photo_scene(json_str: String) -> Scene {
  case string.contains(json_str, "\"select_model\"") {
    True -> NeuroPhoto(NeuroPhotoSelectModel)
    False ->
      case string.contains(json_str, "\"enter_prompt\"") {
        True -> {
          let model = extract_json_field(json_str, "model")
          NeuroPhoto(NeuroPhotoEnterPrompt(model))
        }
        False ->
          case string.contains(json_str, "\"generating\"") {
            True -> {
              let model = extract_json_field(json_str, "model")
              let prompt = extract_json_field(json_str, "prompt")
              let job_id = extract_json_field(json_str, "job_id")
              NeuroPhoto(NeuroPhotoGenerating(model, prompt, job_id))
            }
            False ->
              case string.contains(json_str, "\"result\"") {
                True -> NeuroPhoto(NeuroPhotoResult([]))
                False -> Main(Idle)
              }
          }
      }
  }
}

fn parse_avatar_scene(json_str: String) -> Scene {
  case string.contains(json_str, "\"start\"") {
    True -> Avatar(AvatarStart)
    False ->
      case string.contains(json_str, "\"upload_photos\"") {
        True -> {
          let collected = extract_json_int(json_str, "collected")
          let required = extract_json_int(json_str, "required")
          Avatar(AvatarUploadPhotos(collected, required))
        }
        False ->
          case string.contains(json_str, "\"enter_trigger\"") {
            True -> {
              let photos_url = extract_json_field(json_str, "photos_url")
              Avatar(AvatarEnterTriggerWord(photos_url))
            }
            False ->
              case string.contains(json_str, "\"training\"") {
                True -> {
                  let training_id = extract_json_field(json_str, "training_id")
                  let trigger_word = extract_json_field(json_str, "trigger_word")
                  Avatar(AvatarTrainingStarted(training_id, trigger_word))
                }
                False ->
                  case string.contains(json_str, "\"training_complete\"") {
                    True -> {
                      let lora_url = extract_json_field(json_str, "lora_url")
                      let trigger_word =
                        extract_json_field(json_str, "trigger_word")
                      Avatar(AvatarTrainingComplete(lora_url, trigger_word))
                    }
                    False ->
                      case string.contains(json_str, "\"enter_prompt\"") {
                        True -> {
                          let lora_url = extract_json_field(json_str, "lora_url")
                          let trigger_word =
                            extract_json_field(json_str, "trigger_word")
                          Avatar(AvatarEnterPrompt(lora_url, trigger_word))
                        }
                        False ->
                          case string.contains(json_str, "\"generating\"") {
                            True -> {
                              let lora_url =
                                extract_json_field(json_str, "lora_url")
                              let trigger_word =
                                extract_json_field(json_str, "trigger_word")
                              let prompt =
                                extract_json_field(json_str, "prompt")
                              let job_id =
                                extract_json_field(json_str, "job_id")
                              Avatar(
                                AvatarGenerating(
                                  lora_url,
                                  trigger_word,
                                  prompt,
                                  job_id,
                                ),
                              )
                            }
                            False -> Avatar(AvatarResult([]))
                          }
                      }
                  }
              }
          }
      }
  }
}

fn parse_text_to_video_scene(json_str: String) -> Scene {
  case string.contains(json_str, "\"select_provider\"") {
    True -> TextToVideo(TextToVideoSelectProvider)
    False ->
      case string.contains(json_str, "\"enter_prompt\"") {
        True -> {
          let provider = extract_json_field(json_str, "provider")
          TextToVideo(TextToVideoEnterPrompt(provider))
        }
        False ->
          case string.contains(json_str, "\"generating\"") {
            True -> {
              let provider = extract_json_field(json_str, "provider")
              let prompt = extract_json_field(json_str, "prompt")
              let job_id = extract_json_field(json_str, "job_id")
              TextToVideo(TextToVideoGenerating(provider, prompt, job_id))
            }
            False -> {
              let video_url = extract_json_field(json_str, "video_url")
              TextToVideo(TextToVideoResult(video_url))
            }
          }
      }
  }
}

fn parse_image_to_video_scene(json_str: String) -> Scene {
  case string.contains(json_str, "\"upload_image\"") {
    True -> ImageToVideo(ImageToVideoUploadImage)
    False ->
      case string.contains(json_str, "\"enter_prompt\"") {
        True -> {
          let image_url = extract_json_field(json_str, "image_url")
          ImageToVideo(ImageToVideoEnterPrompt(image_url))
        }
        False ->
          case string.contains(json_str, "\"generating\"") {
            True -> {
              let image_url = extract_json_field(json_str, "image_url")
              let prompt = extract_json_field(json_str, "prompt")
              let job_id = extract_json_field(json_str, "job_id")
              ImageToVideo(ImageToVideoGenerating(image_url, prompt, job_id))
            }
            False -> {
              let video_url = extract_json_field(json_str, "video_url")
              ImageToVideo(ImageToVideoResult(video_url))
            }
          }
      }
  }
}

fn parse_morphing_scene(json_str: String) -> Scene {
  case string.contains(json_str, "\"upload_start\"") {
    True -> Morphing(MorphingUploadStart)
    False ->
      case string.contains(json_str, "\"upload_end\"") {
        True -> {
          let start_image = extract_json_field(json_str, "start_image")
          Morphing(MorphingUploadEnd(start_image))
        }
        False ->
          case string.contains(json_str, "\"enter_style\"") {
            True -> {
              let start_image = extract_json_field(json_str, "start_image")
              let end_image = extract_json_field(json_str, "end_image")
              Morphing(MorphingEnterStyle(start_image, end_image))
            }
            False ->
              case string.contains(json_str, "\"generating\"") {
                True -> {
                  let start_image = extract_json_field(json_str, "start_image")
                  let end_image = extract_json_field(json_str, "end_image")
                  let style = extract_json_field(json_str, "style")
                  let job_id = extract_json_field(json_str, "job_id")
                  Morphing(MorphingGenerating(start_image, end_image, style, job_id))
                }
                False -> {
                  let video_url = extract_json_field(json_str, "video_url")
                  Morphing(MorphingResult(video_url))
                }
              }
          }
      }
  }
}

fn parse_broll_scene(json_str: String) -> Scene {
  case string.contains(json_str, "\"enter_script\"") {
    True -> BRoll(BRollEnterScript)
    False ->
      case string.contains(json_str, "\"select_style\"") {
        True -> {
          let script = extract_json_field(json_str, "script")
          BRoll(BRollSelectStyle(script))
        }
        False ->
          case string.contains(json_str, "\"generating\"") {
            True -> {
              let script = extract_json_field(json_str, "script")
              let style = extract_json_field(json_str, "style")
              let job_id = extract_json_field(json_str, "job_id")
              BRoll(BRollGenerating(script, style, job_id))
            }
            False -> BRoll(BRollResult([]))
          }
      }
  }
}

fn parse_avatar_video_scene(json_str: String) -> Scene {
  case string.contains(json_str, "\"upload_portrait\"") {
    True -> AvatarVideo(AvatarVideoUploadPortrait)
    False ->
      case string.contains(json_str, "\"enter_script\"") {
        True -> {
          let portrait_url = extract_json_field(json_str, "portrait_url")
          AvatarVideo(AvatarVideoEnterScript(portrait_url))
        }
        False ->
          case string.contains(json_str, "\"select_voice\"") {
            True -> {
              let portrait_url = extract_json_field(json_str, "portrait_url")
              let script = extract_json_field(json_str, "script")
              AvatarVideo(AvatarVideoSelectVoice(portrait_url, script))
            }
            False ->
              case string.contains(json_str, "\"generating\"") {
                True -> {
                  let portrait_url = extract_json_field(json_str, "portrait_url")
                  let script = extract_json_field(json_str, "script")
                  let voice_id = extract_json_field(json_str, "voice_id")
                  let job_id = extract_json_field(json_str, "job_id")
                  AvatarVideo(AvatarVideoGenerating(portrait_url, script, voice_id, job_id))
                }
                False -> {
                  let video_url = extract_json_field(json_str, "video_url")
                  AvatarVideo(AvatarVideoResult(video_url))
                }
              }
          }
      }
  }
}

fn parse_voice_clone_scene(json_str: String) -> Scene {
  case string.contains(json_str, "\"upload_sample\"") {
    True -> VoiceClone(VoiceCloneUploadSample)
    False ->
      case string.contains(json_str, "\"enter_text\"") {
        True -> {
          let voice_id = extract_json_field(json_str, "voice_id")
          VoiceClone(VoiceCloneEnterText(voice_id))
        }
        False ->
          case string.contains(json_str, "\"generating\"") {
            True -> {
              let voice_id = extract_json_field(json_str, "voice_id")
              let text = extract_json_field(json_str, "text")
              let job_id = extract_json_field(json_str, "job_id")
              VoiceClone(VoiceCloneGenerating(voice_id, text, job_id))
            }
            False -> {
              let audio_url = extract_json_field(json_str, "audio_url")
              VoiceClone(VoiceCloneResult(audio_url))
            }
          }
      }
  }
}

fn parse_userbot_setup_scene(json_str: String) -> Scene {
  // Step 1: Auth states
  case string.contains(json_str, "\"welcome\"") {
    True -> UserBotSetup(UserBotWelcome)
    False ->
      case string.contains(json_str, "\"enter_phone\"") {
        True -> UserBotSetup(UserBotEnterPhone)
        False ->
          case string.contains(json_str, "\"waiting_code\"") {
            True -> {
              let phone = extract_json_field(json_str, "phone")
              let session_id = extract_json_field(json_str, "session_id")
              let phone_code_hash = extract_json_field(json_str, "phone_code_hash")
              UserBotSetup(UserBotWaitingCode(phone, session_id, phone_code_hash))
            }
            False ->
              case string.contains(json_str, "\"enter_2fa\"") {
                True -> {
                  let phone = extract_json_field(json_str, "phone")
                  let session_id = extract_json_field(json_str, "session_id")
                  UserBotSetup(UserBotEnter2FA(phone, session_id))
                }
                False ->
                  case string.contains(json_str, "\"auth_success\"") {
                    True -> {
                      let session_id = extract_json_field(json_str, "session_id")
                      let username = extract_json_field(json_str, "username")
                      let first_name = extract_json_field(json_str, "first_name")
                      UserBotSetup(UserBotAuthSuccess(session_id, username, first_name))
                    }
                    False ->
                      // Step 2: Chat selection
                      case string.contains(json_str, "\"select_chats\"") {
                        True -> {
                          let session_id = extract_json_field(json_str, "session_id")
                          let available_chats = extract_json_field(json_str, "available_chats")
                          UserBotSetup(UserBotSelectChats(session_id, available_chats))
                        }
                        False ->
                          case string.contains(json_str, "\"chats_selected\"") {
                            True -> {
                              let session_id = extract_json_field(json_str, "session_id")
                              let selected_chat_ids = extract_json_field(json_str, "selected_chat_ids")
                              UserBotSetup(UserBotChatsSelected(session_id, selected_chat_ids))
                            }
                            False ->
                              // Step 3: Mode configuration
                              case string.contains(json_str, "\"configure_mode\"") {
                                True -> {
                                  let session_id = extract_json_field(json_str, "session_id")
                                  let chat_id = extract_json_int(json_str, "chat_id")
                                  let chat_name = extract_json_field(json_str, "chat_name")
                                  let remaining_chats = extract_json_field(json_str, "remaining_chats")
                                  UserBotSetup(UserBotConfigureMode(session_id, chat_id, chat_name, remaining_chats))
                                }
                                False ->
                                  case string.contains(json_str, "\"modes_configured\"") {
                                    True -> {
                                      let session_id = extract_json_field(json_str, "session_id")
                                      let chat_configs = extract_json_field(json_str, "chat_configs")
                                      UserBotSetup(UserBotModesConfigured(session_id, chat_configs))
                                    }
                                    False ->
                                      // Step 4: Triggers
                                      case string.contains(json_str, "\"setup_triggers\"") {
                                        True -> {
                                          let session_id = extract_json_field(json_str, "session_id")
                                          UserBotSetup(UserBotSetupTriggers(session_id))
                                        }
                                        False ->
                                          case string.contains(json_str, "\"triggers_configured\"") {
                                            True -> {
                                              let session_id = extract_json_field(json_str, "session_id")
                                              let triggers = extract_json_field(json_str, "triggers")
                                              UserBotSetup(UserBotTriggersConfigured(session_id, triggers))
                                            }
                                            False ->
                                              // Step 5: Character
                                              case string.contains(json_str, "\"setup_character\"") {
                                                True -> {
                                                  let session_id = extract_json_field(json_str, "session_id")
                                                  UserBotSetup(UserBotSetupCharacter(session_id))
                                                }
                                                False ->
                                                  case string.contains(json_str, "\"character_configured\"") {
                                                    True -> {
                                                      let session_id = extract_json_field(json_str, "session_id")
                                                      let character_name = extract_json_field(json_str, "character_name")
                                                      let character_style = extract_json_field(json_str, "character_style")
                                                      UserBotSetup(UserBotCharacterConfigured(session_id, character_name, character_style))
                                                    }
                                                    False ->
                                                      // Step 6: Summary & Activation
                                                      case string.contains(json_str, "\"summary\"") {
                                                        True -> {
                                                          let session_id = extract_json_field(json_str, "session_id")
                                                          let full_config = extract_json_field(json_str, "full_config")
                                                          UserBotSetup(UserBotSummary(session_id, full_config))
                                                        }
                                                        False ->
                                                          case string.contains(json_str, "\"activating\"") {
                                                            True -> {
                                                              let session_id = extract_json_field(json_str, "session_id")
                                                              UserBotSetup(UserBotActivating(session_id))
                                                            }
                                                            False ->
                                                              case string.contains(json_str, "\"complete\"") {
                                                                True -> {
                                                                  let session_id = extract_json_field(json_str, "session_id")
                                                                  UserBotSetup(UserBotComplete(session_id))
                                                                }
                                                                False -> UserBotSetup(UserBotWelcome)
                                                              }
                                                          }
                                                      }
                                                  }
                                              }
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
              }
          }
      }
  }
}

fn extract_json_field(json_str: String, field: String) -> String {
  // Simple field extraction: "field":"value" or "field": "value"
  let pattern = "\"" <> field <> "\":"
  case string.split(json_str, pattern) {
    [_, rest] -> {
      let trimmed = string.trim_start(rest)
      case string.starts_with(trimmed, "\"") {
        True -> {
          let without_quote = string.drop_start(trimmed, 1)
          case string.split(without_quote, "\"") {
            [value, ..] -> value
            _ -> ""
          }
        }
        False -> ""
      }
    }
    _ -> ""
  }
}

fn extract_json_int(json_str: String, field: String) -> Int {
  let pattern = "\"" <> field <> "\":"
  case string.split(json_str, pattern) {
    [_, rest] -> {
      let trimmed = string.trim_start(rest)
      let digits =
        trimmed
        |> string.to_graphemes
        |> list.take_while(fn(c) { is_digit(c) })
        |> string.join("")
      case int.parse(digits) {
        Ok(n) -> n
        Error(_) -> 0
      }
    }
    _ -> 0
  }
}

fn is_digit(c: String) -> Bool {
  case c {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

fn parse_context_json(json_str: String) -> Dict(String, String) {
  // Return empty dict for now, can be expanded
  dict.new()
}

fn context_to_json(context: Dict(String, String)) -> String {
  let pairs =
    dict.to_list(context)
    |> list.map(fn(pair) {
      let #(key, value) = pair
      #(key, json.string(value))
    })
  json.to_string(json.object(pairs))
}

fn pog_error_to_string(error: pog.QueryError) -> String {
  case error {
    pog.ConstraintViolated(msg, constraint, _detail) ->
      "Constraint violated: " <> constraint <> " - " <> msg
    pog.PostgresqlError(code, name, msg) ->
      "PostgreSQL error: " <> code <> " " <> name <> " - " <> msg
    pog.UnexpectedArgumentCount(expected, got) ->
      "Unexpected argument count: expected "
      <> int.to_string(expected)
      <> ", got "
      <> int.to_string(got)
    pog.UnexpectedArgumentType(expected, got) ->
      "Unexpected argument type: expected " <> expected <> ", got " <> got
    pog.UnexpectedResultType(_errors) -> "Unexpected result type"
    pog.ConnectionUnavailable -> "Connection unavailable"
    _ -> "Unknown database error"
  }
}
