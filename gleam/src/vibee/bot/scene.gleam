// Scene State Machine for Telegram Bot
// Defines all possible scenes and user session state

import gleam/dict.{type Dict}
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ============================================================
// Scene Types - All possible conversation states
// ============================================================

/// Main menu and idle state
pub type MainScene {
  Idle
  MainMenu
}

/// NeuroPhoto generation flow
pub type NeuroPhotoScene {
  NeuroPhotoSelectModel
  NeuroPhotoEnterPrompt(model: String)
  NeuroPhotoGenerating(model: String, prompt: String, job_id: String)
  NeuroPhotoResult(images: List(String))
}

/// Digital Avatar training and generation flow
pub type AvatarScene {
  AvatarStart
  AvatarUploadPhotos(collected: Int, required: Int)
  AvatarEnterTriggerWord(photos_url: String)
  AvatarTrainingStarted(training_id: String, trigger_word: String)
  AvatarTrainingComplete(lora_url: String, trigger_word: String)
  AvatarEnterPrompt(lora_url: String, trigger_word: String)
  AvatarGenerating(lora_url: String, trigger_word: String, prompt: String, job_id: String)
  AvatarResult(images: List(String))
}

/// Text to Video generation flow
pub type TextToVideoScene {
  TextToVideoSelectProvider
  TextToVideoEnterPrompt(provider: String)
  TextToVideoGenerating(provider: String, prompt: String, job_id: String)
  TextToVideoResult(video_url: String)
}

/// Image to Video animation flow
pub type ImageToVideoScene {
  ImageToVideoUploadImage
  ImageToVideoEnterPrompt(image_url: String)
  ImageToVideoGenerating(image_url: String, prompt: String, job_id: String)
  ImageToVideoResult(video_url: String)
}

/// Morphing transition flow
pub type MorphingScene {
  MorphingUploadStart
  MorphingUploadEnd(start_image: String)
  MorphingEnterStyle(start_image: String, end_image: String)
  MorphingGenerating(start_image: String, end_image: String, style: String, job_id: String)
  MorphingResult(video_url: String)
}

/// B-Roll generation flow
pub type BRollScene {
  BRollEnterScript
  BRollSelectStyle(script: String)
  BRollGenerating(script: String, style: String, job_id: String)
  BRollResult(video_urls: List(String))
}

/// Avatar Video (talking head) flow
pub type AvatarVideoScene {
  AvatarVideoUploadPortrait
  AvatarVideoEnterScript(portrait_url: String)
  AvatarVideoSelectVoice(portrait_url: String, script: String)
  AvatarVideoGenerating(portrait_url: String, script: String, voice_id: String, job_id: String)
  AvatarVideoResult(video_url: String)
}

/// Voice Clone flow
pub type VoiceCloneScene {
  VoiceCloneUploadSample
  VoiceCloneEnterText(voice_id: String)
  VoiceCloneGenerating(voice_id: String, text: String, job_id: String)
  VoiceCloneResult(audio_url: String)
}

/// Combined scene state
pub type Scene {
  Main(MainScene)
  NeuroPhoto(NeuroPhotoScene)
  Avatar(AvatarScene)
  TextToVideo(TextToVideoScene)
  ImageToVideo(ImageToVideoScene)
  Morphing(MorphingScene)
  BRoll(BRollScene)
  AvatarVideo(AvatarVideoScene)
  VoiceClone(VoiceCloneScene)
}

// ============================================================
// User Session
// ============================================================

pub type UserSession {
  UserSession(
    user_id: Int,
    chat_id: String,
    username: Option(String),
    scene: Scene,
    context: Dict(String, String),
    created_at: Int,
    updated_at: Int,
  )
}

/// Create new session for user
pub fn new_session(user_id: Int, chat_id: String) -> UserSession {
  UserSession(
    user_id: user_id,
    chat_id: chat_id,
    username: None,
    scene: Main(Idle),
    context: dict.new(),
    created_at: 0,
    updated_at: 0,
  )
}

/// Create session with username
pub fn new_session_with_username(
  user_id: Int,
  chat_id: String,
  username: String,
) -> UserSession {
  UserSession(
    user_id: user_id,
    chat_id: chat_id,
    username: Some(username),
    scene: Main(Idle),
    context: dict.new(),
    created_at: 0,
    updated_at: 0,
  )
}

// ============================================================
// Scene Transitions
// ============================================================

/// Update scene in session
pub fn set_scene(session: UserSession, scene: Scene) -> UserSession {
  UserSession(..session, scene: scene)
}

/// Add context value
pub fn set_context(
  session: UserSession,
  key: String,
  value: String,
) -> UserSession {
  UserSession(..session, context: dict.insert(session.context, key, value))
}

/// Get context value
pub fn get_context(session: UserSession, key: String) -> Option(String) {
  dict.get(session.context, key)
  |> option.from_result
}

/// Clear context
pub fn clear_context(session: UserSession) -> UserSession {
  UserSession(..session, context: dict.new())
}

/// Reset to idle
pub fn reset_session(session: UserSession) -> UserSession {
  UserSession(..session, scene: Main(Idle), context: dict.new())
}

// ============================================================
// Scene Helpers
// ============================================================

/// Check if session is idle
pub fn is_idle(session: UserSession) -> Bool {
  case session.scene {
    Main(Idle) -> True
    Main(MainMenu) -> True
    _ -> False
  }
}

/// Check if in NeuroPhoto flow
pub fn is_neuro_photo(session: UserSession) -> Bool {
  case session.scene {
    NeuroPhoto(_) -> True
    _ -> False
  }
}

/// Check if in Avatar flow
pub fn is_avatar(session: UserSession) -> Bool {
  case session.scene {
    Avatar(_) -> True
    _ -> False
  }
}

/// Check if waiting for user input
pub fn is_waiting_input(session: UserSession) -> Bool {
  case session.scene {
    Main(Idle) -> True
    Main(MainMenu) -> True
    NeuroPhoto(NeuroPhotoSelectModel) -> True
    NeuroPhoto(NeuroPhotoEnterPrompt(_)) -> True
    Avatar(AvatarStart) -> True
    Avatar(AvatarUploadPhotos(_, _)) -> True
    Avatar(AvatarEnterTriggerWord(_)) -> True
    Avatar(AvatarEnterPrompt(_, _)) -> True
    // New scenes
    TextToVideo(TextToVideoSelectProvider) -> True
    TextToVideo(TextToVideoEnterPrompt(_)) -> True
    ImageToVideo(ImageToVideoUploadImage) -> True
    ImageToVideo(ImageToVideoEnterPrompt(_)) -> True
    Morphing(MorphingUploadStart) -> True
    Morphing(MorphingUploadEnd(_)) -> True
    Morphing(MorphingEnterStyle(_, _)) -> True
    BRoll(BRollEnterScript) -> True
    BRoll(BRollSelectStyle(_)) -> True
    AvatarVideo(AvatarVideoUploadPortrait) -> True
    AvatarVideo(AvatarVideoEnterScript(_)) -> True
    AvatarVideo(AvatarVideoSelectVoice(_, _)) -> True
    VoiceClone(VoiceCloneUploadSample) -> True
    VoiceClone(VoiceCloneEnterText(_)) -> True
    _ -> False
  }
}

/// Check if processing (waiting for AI)
pub fn is_processing(session: UserSession) -> Bool {
  case session.scene {
    NeuroPhoto(NeuroPhotoGenerating(_, _, _)) -> True
    Avatar(AvatarTrainingStarted(_, _)) -> True
    Avatar(AvatarGenerating(_, _, _, _)) -> True
    // New scenes
    TextToVideo(TextToVideoGenerating(_, _, _)) -> True
    ImageToVideo(ImageToVideoGenerating(_, _, _)) -> True
    Morphing(MorphingGenerating(_, _, _, _)) -> True
    BRoll(BRollGenerating(_, _, _)) -> True
    AvatarVideo(AvatarVideoGenerating(_, _, _, _)) -> True
    VoiceClone(VoiceCloneGenerating(_, _, _)) -> True
    _ -> False
  }
}

// ============================================================
// Scene Serialization (for PostgreSQL)
// ============================================================

/// Serialize scene to JSON string
pub fn scene_to_json(scene: Scene) -> String {
  case scene {
    Main(Idle) ->
      json.to_string(json.object([
        #("type", json.string("main")),
        #("state", json.string("idle")),
      ]))

    Main(MainMenu) ->
      json.to_string(json.object([
        #("type", json.string("main")),
        #("state", json.string("menu")),
      ]))

    NeuroPhoto(NeuroPhotoSelectModel) ->
      json.to_string(json.object([
        #("type", json.string("neuro_photo")),
        #("state", json.string("select_model")),
      ]))

    NeuroPhoto(NeuroPhotoEnterPrompt(model)) ->
      json.to_string(json.object([
        #("type", json.string("neuro_photo")),
        #("state", json.string("enter_prompt")),
        #("model", json.string(model)),
      ]))

    NeuroPhoto(NeuroPhotoGenerating(model, prompt, job_id)) ->
      json.to_string(json.object([
        #("type", json.string("neuro_photo")),
        #("state", json.string("generating")),
        #("model", json.string(model)),
        #("prompt", json.string(prompt)),
        #("job_id", json.string(job_id)),
      ]))

    NeuroPhoto(NeuroPhotoResult(images)) ->
      json.to_string(json.object([
        #("type", json.string("neuro_photo")),
        #("state", json.string("result")),
        #("images", json.array(images, json.string)),
      ]))

    Avatar(AvatarStart) ->
      json.to_string(json.object([
        #("type", json.string("avatar")),
        #("state", json.string("start")),
      ]))

    Avatar(AvatarUploadPhotos(collected, required)) ->
      json.to_string(json.object([
        #("type", json.string("avatar")),
        #("state", json.string("upload_photos")),
        #("collected", json.int(collected)),
        #("required", json.int(required)),
      ]))

    Avatar(AvatarEnterTriggerWord(photos_url)) ->
      json.to_string(json.object([
        #("type", json.string("avatar")),
        #("state", json.string("enter_trigger")),
        #("photos_url", json.string(photos_url)),
      ]))

    Avatar(AvatarTrainingStarted(training_id, trigger_word)) ->
      json.to_string(json.object([
        #("type", json.string("avatar")),
        #("state", json.string("training")),
        #("training_id", json.string(training_id)),
        #("trigger_word", json.string(trigger_word)),
      ]))

    Avatar(AvatarTrainingComplete(lora_url, trigger_word)) ->
      json.to_string(json.object([
        #("type", json.string("avatar")),
        #("state", json.string("training_complete")),
        #("lora_url", json.string(lora_url)),
        #("trigger_word", json.string(trigger_word)),
      ]))

    Avatar(AvatarEnterPrompt(lora_url, trigger_word)) ->
      json.to_string(json.object([
        #("type", json.string("avatar")),
        #("state", json.string("enter_prompt")),
        #("lora_url", json.string(lora_url)),
        #("trigger_word", json.string(trigger_word)),
      ]))

    Avatar(AvatarGenerating(lora_url, trigger_word, prompt, job_id)) ->
      json.to_string(json.object([
        #("type", json.string("avatar")),
        #("state", json.string("generating")),
        #("lora_url", json.string(lora_url)),
        #("trigger_word", json.string(trigger_word)),
        #("prompt", json.string(prompt)),
        #("job_id", json.string(job_id)),
      ]))

    Avatar(AvatarResult(images)) ->
      json.to_string(json.object([
        #("type", json.string("avatar")),
        #("state", json.string("result")),
        #("images", json.array(images, json.string)),
      ]))

    // TextToVideo scenes
    TextToVideo(TextToVideoSelectProvider) ->
      json.to_string(json.object([
        #("type", json.string("text_to_video")),
        #("state", json.string("select_provider")),
      ]))

    TextToVideo(TextToVideoEnterPrompt(provider)) ->
      json.to_string(json.object([
        #("type", json.string("text_to_video")),
        #("state", json.string("enter_prompt")),
        #("provider", json.string(provider)),
      ]))

    TextToVideo(TextToVideoGenerating(provider, prompt, job_id)) ->
      json.to_string(json.object([
        #("type", json.string("text_to_video")),
        #("state", json.string("generating")),
        #("provider", json.string(provider)),
        #("prompt", json.string(prompt)),
        #("job_id", json.string(job_id)),
      ]))

    TextToVideo(TextToVideoResult(video_url)) ->
      json.to_string(json.object([
        #("type", json.string("text_to_video")),
        #("state", json.string("result")),
        #("video_url", json.string(video_url)),
      ]))

    // ImageToVideo scenes
    ImageToVideo(ImageToVideoUploadImage) ->
      json.to_string(json.object([
        #("type", json.string("image_to_video")),
        #("state", json.string("upload_image")),
      ]))

    ImageToVideo(ImageToVideoEnterPrompt(image_url)) ->
      json.to_string(json.object([
        #("type", json.string("image_to_video")),
        #("state", json.string("enter_prompt")),
        #("image_url", json.string(image_url)),
      ]))

    ImageToVideo(ImageToVideoGenerating(image_url, prompt, job_id)) ->
      json.to_string(json.object([
        #("type", json.string("image_to_video")),
        #("state", json.string("generating")),
        #("image_url", json.string(image_url)),
        #("prompt", json.string(prompt)),
        #("job_id", json.string(job_id)),
      ]))

    ImageToVideo(ImageToVideoResult(video_url)) ->
      json.to_string(json.object([
        #("type", json.string("image_to_video")),
        #("state", json.string("result")),
        #("video_url", json.string(video_url)),
      ]))

    // Morphing scenes
    Morphing(MorphingUploadStart) ->
      json.to_string(json.object([
        #("type", json.string("morphing")),
        #("state", json.string("upload_start")),
      ]))

    Morphing(MorphingUploadEnd(start_image)) ->
      json.to_string(json.object([
        #("type", json.string("morphing")),
        #("state", json.string("upload_end")),
        #("start_image", json.string(start_image)),
      ]))

    Morphing(MorphingEnterStyle(start_image, end_image)) ->
      json.to_string(json.object([
        #("type", json.string("morphing")),
        #("state", json.string("enter_style")),
        #("start_image", json.string(start_image)),
        #("end_image", json.string(end_image)),
      ]))

    Morphing(MorphingGenerating(start_image, end_image, style, job_id)) ->
      json.to_string(json.object([
        #("type", json.string("morphing")),
        #("state", json.string("generating")),
        #("start_image", json.string(start_image)),
        #("end_image", json.string(end_image)),
        #("style", json.string(style)),
        #("job_id", json.string(job_id)),
      ]))

    Morphing(MorphingResult(video_url)) ->
      json.to_string(json.object([
        #("type", json.string("morphing")),
        #("state", json.string("result")),
        #("video_url", json.string(video_url)),
      ]))

    // BRoll scenes
    BRoll(BRollEnterScript) ->
      json.to_string(json.object([
        #("type", json.string("broll")),
        #("state", json.string("enter_script")),
      ]))

    BRoll(BRollSelectStyle(script)) ->
      json.to_string(json.object([
        #("type", json.string("broll")),
        #("state", json.string("select_style")),
        #("script", json.string(script)),
      ]))

    BRoll(BRollGenerating(script, style, job_id)) ->
      json.to_string(json.object([
        #("type", json.string("broll")),
        #("state", json.string("generating")),
        #("script", json.string(script)),
        #("style", json.string(style)),
        #("job_id", json.string(job_id)),
      ]))

    BRoll(BRollResult(video_urls)) ->
      json.to_string(json.object([
        #("type", json.string("broll")),
        #("state", json.string("result")),
        #("video_urls", json.array(video_urls, json.string)),
      ]))

    // AvatarVideo scenes
    AvatarVideo(AvatarVideoUploadPortrait) ->
      json.to_string(json.object([
        #("type", json.string("avatar_video")),
        #("state", json.string("upload_portrait")),
      ]))

    AvatarVideo(AvatarVideoEnterScript(portrait_url)) ->
      json.to_string(json.object([
        #("type", json.string("avatar_video")),
        #("state", json.string("enter_script")),
        #("portrait_url", json.string(portrait_url)),
      ]))

    AvatarVideo(AvatarVideoSelectVoice(portrait_url, script)) ->
      json.to_string(json.object([
        #("type", json.string("avatar_video")),
        #("state", json.string("select_voice")),
        #("portrait_url", json.string(portrait_url)),
        #("script", json.string(script)),
      ]))

    AvatarVideo(AvatarVideoGenerating(portrait_url, script, voice_id, job_id)) ->
      json.to_string(json.object([
        #("type", json.string("avatar_video")),
        #("state", json.string("generating")),
        #("portrait_url", json.string(portrait_url)),
        #("script", json.string(script)),
        #("voice_id", json.string(voice_id)),
        #("job_id", json.string(job_id)),
      ]))

    AvatarVideo(AvatarVideoResult(video_url)) ->
      json.to_string(json.object([
        #("type", json.string("avatar_video")),
        #("state", json.string("result")),
        #("video_url", json.string(video_url)),
      ]))

    // VoiceClone scenes
    VoiceClone(VoiceCloneUploadSample) ->
      json.to_string(json.object([
        #("type", json.string("voice_clone")),
        #("state", json.string("upload_sample")),
      ]))

    VoiceClone(VoiceCloneEnterText(voice_id)) ->
      json.to_string(json.object([
        #("type", json.string("voice_clone")),
        #("state", json.string("enter_text")),
        #("voice_id", json.string(voice_id)),
      ]))

    VoiceClone(VoiceCloneGenerating(voice_id, text, job_id)) ->
      json.to_string(json.object([
        #("type", json.string("voice_clone")),
        #("state", json.string("generating")),
        #("voice_id", json.string(voice_id)),
        #("text", json.string(text)),
        #("job_id", json.string(job_id)),
      ]))

    VoiceClone(VoiceCloneResult(audio_url)) ->
      json.to_string(json.object([
        #("type", json.string("voice_clone")),
        #("state", json.string("result")),
        #("audio_url", json.string(audio_url)),
      ]))
  }
}

/// Default scene (for parsing errors)
pub fn default_scene() -> Scene {
  Main(Idle)
}

// ============================================================
// Available Models
// ============================================================

pub type NeuroPhotoModel {
  NeuroPhotoModel(id: String, name: String, description: String)
}

pub fn available_neuro_photo_models() -> List(NeuroPhotoModel) {
  [
    NeuroPhotoModel(
      id: "flux-lora",
      name: "FLUX LoRA",
      description: "High quality with custom LoRA",
    ),
    NeuroPhotoModel(
      id: "nano-banana",
      name: "Nano Banana Pro",
      description: "Fast generation, good quality",
    ),
    NeuroPhotoModel(
      id: "flux-kontext",
      name: "FLUX Kontext",
      description: "Image transformation",
    ),
  ]
}

// ============================================================
// Message Types
// ============================================================

pub type IncomingMessage {
  TextMessage(text: String)
  PhotoMessage(file_id: String, caption: Option(String))
  CallbackQuery(data: String)
  Command(cmd: String, args: Option(String))
}

pub type OutgoingMessage {
  TextReply(text: String)
  TextWithKeyboard(text: String, keyboard: List(List(InlineButton)))
  PhotoReply(url: String, caption: Option(String))
  PhotosReply(urls: List(String), caption: Option(String))
  VideoReply(url: String, caption: Option(String))
  AudioReply(url: String, caption: Option(String))
}

pub type InlineButton {
  InlineButton(text: String, callback_data: String)
}

/// Create inline button
pub fn button(text: String, data: String) -> InlineButton {
  InlineButton(text: text, callback_data: data)
}

/// Create button row
pub fn button_row(buttons: List(InlineButton)) -> List(InlineButton) {
  buttons
}
