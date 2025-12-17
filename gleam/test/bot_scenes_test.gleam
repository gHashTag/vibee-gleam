// Tests for Bot Scene Handlers
// Tests scene state machines, transitions, and responses

import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import vibee/bot/scene.{
  type UserSession,
  AvatarVideo, AvatarVideoEnterScript, AvatarVideoGenerating,
  AvatarVideoSelectVoice, AvatarVideoUploadPortrait, BRoll, BRollEnterScript,
  BRollGenerating, BRollSelectStyle, Idle, ImageToVideo,
  ImageToVideoEnterPrompt, ImageToVideoGenerating,
  ImageToVideoUploadImage, Main, Morphing, MorphingEnterStyle, MorphingGenerating,
  MorphingUploadEnd, MorphingUploadStart, TextMessage,
  TextToVideo, TextToVideoEnterPrompt, TextToVideoGenerating,
  TextToVideoSelectProvider, UserSession, VoiceClone, VoiceCloneEnterText,
  VoiceCloneGenerating, VoiceCloneUploadSample,
}
import vibee/bot/scenes/avatar_video as avatar_video_scene
import vibee/bot/scenes/broll as broll_scene
import vibee/bot/scenes/image_to_video as image_to_video_scene
import vibee/bot/scenes/morphing as morphing_scene
import vibee/bot/scenes/text_to_video as text_to_video_scene
import vibee/bot/scenes/voice_clone as voice_clone_scene

// ============================================================
// Test Helpers
// ============================================================

fn create_test_session() -> UserSession {
  UserSession(
    user_id: 12345,
    chat_id: "12345",
    username: Some("testuser"),
    scene: Main(Idle),
    context: dict.new(),
    created_at: 0,
    updated_at: 0,
  )
}

// ============================================================
// TextToVideo Scene Tests
// ============================================================

pub fn text_to_video_enter_test() {
  let session = create_test_session()

  let result = text_to_video_scene.enter(session)

  should.be_ok(result)

  case result {
    Ok(scene_result) -> {
      // Should transition to SelectProvider state
      case scene_result.session.scene {
        TextToVideo(TextToVideoSelectProvider) -> should.be_true(True)
        _ -> should.be_true(False)
      }
      // Should have a response
      should.be_true(option.is_some(scene_result.response))
    }
    Error(_) -> should.be_true(False)
  }
}

pub fn text_to_video_select_provider_callback_test() {
  let session = UserSession(
    ..create_test_session(),
    scene: TextToVideo(TextToVideoSelectProvider),
  )

  let result = text_to_video_scene.handle_callback(session, "t2v:kling")

  should.be_ok(result)

  case result {
    Ok(scene_result) -> {
      // Should transition to EnterPrompt state with kling provider
      case scene_result.session.scene {
        TextToVideo(TextToVideoEnterPrompt("kling")) -> should.be_true(True)
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

pub fn text_to_video_enter_prompt_test() {
  let session = UserSession(
    ..create_test_session(),
    scene: TextToVideo(TextToVideoEnterPrompt("kling")),
  )

  let result = text_to_video_scene.handle_message(
    session,
    TextMessage("A beautiful sunset over the ocean"),
  )

  should.be_ok(result)

  case result {
    Ok(scene_result) -> {
      // Should transition to Generating state
      case scene_result.session.scene {
        TextToVideo(TextToVideoGenerating("kling", "A beautiful sunset over the ocean", _)) ->
          should.be_true(True)
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

// ============================================================
// ImageToVideo Scene Tests
// ============================================================

pub fn image_to_video_enter_test() {
  let session = create_test_session()

  let result = image_to_video_scene.enter(session)

  should.be_ok(result)

  case result {
    Ok(scene_result) -> {
      case scene_result.session.scene {
        ImageToVideo(ImageToVideoUploadImage) -> should.be_true(True)
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

pub fn image_to_video_auto_motion_callback_test() {
  let session = UserSession(
    ..create_test_session(),
    scene: ImageToVideo(ImageToVideoEnterPrompt("test_image_url")),
  )

  let result = image_to_video_scene.handle_callback(session, "i2v:auto")

  should.be_ok(result)

  case result {
    Ok(scene_result) -> {
      case scene_result.session.scene {
        ImageToVideo(ImageToVideoGenerating("test_image_url", _, _)) ->
          should.be_true(True)
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

// ============================================================
// Morphing Scene Tests
// ============================================================

pub fn morphing_enter_test() {
  let session = create_test_session()

  let result = morphing_scene.enter(session)

  should.be_ok(result)

  case result {
    Ok(scene_result) -> {
      case scene_result.session.scene {
        Morphing(MorphingUploadStart) -> should.be_true(True)
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

pub fn morphing_style_callback_test() {
  let session = UserSession(
    ..create_test_session(),
    scene: Morphing(MorphingEnterStyle("start_img", "end_img")),
  )

  let result = morphing_scene.handle_callback(session, "morph:smooth")

  should.be_ok(result)

  case result {
    Ok(scene_result) -> {
      case scene_result.session.scene {
        Morphing(MorphingGenerating("start_img", "end_img", _, _)) ->
          should.be_true(True)
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

// ============================================================
// BRoll Scene Tests
// ============================================================

pub fn broll_enter_test() {
  let session = create_test_session()

  let result = broll_scene.enter(session)

  should.be_ok(result)

  case result {
    Ok(scene_result) -> {
      case scene_result.session.scene {
        BRoll(BRollEnterScript) -> should.be_true(True)
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

pub fn broll_script_input_test() {
  let session = UserSession(
    ..create_test_session(),
    scene: BRoll(BRollEnterScript),
  )

  let result = broll_scene.handle_message(
    session,
    TextMessage("Morning routine: coffee, workout, work"),
  )

  should.be_ok(result)

  case result {
    Ok(scene_result) -> {
      case scene_result.session.scene {
        BRoll(BRollSelectStyle("Morning routine: coffee, workout, work")) ->
          should.be_true(True)
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

pub fn broll_style_callback_test() {
  let session = UserSession(
    ..create_test_session(),
    scene: BRoll(BRollSelectStyle("test script")),
  )

  let result = broll_scene.handle_callback(session, "broll:cinematic")

  should.be_ok(result)

  case result {
    Ok(scene_result) -> {
      case scene_result.session.scene {
        BRoll(BRollGenerating("test script", "cinematic", _)) ->
          should.be_true(True)
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

// ============================================================
// AvatarVideo Scene Tests
// ============================================================

pub fn avatar_video_enter_test() {
  let session = create_test_session()

  let result = avatar_video_scene.enter(session)

  should.be_ok(result)

  case result {
    Ok(scene_result) -> {
      case scene_result.session.scene {
        AvatarVideo(AvatarVideoUploadPortrait) -> should.be_true(True)
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

pub fn avatar_video_script_input_test() {
  let session = UserSession(
    ..create_test_session(),
    scene: AvatarVideo(AvatarVideoEnterScript("portrait_url")),
  )

  let result = avatar_video_scene.handle_message(
    session,
    TextMessage("Hello, welcome to my channel!"),
  )

  should.be_ok(result)

  case result {
    Ok(scene_result) -> {
      case scene_result.session.scene {
        AvatarVideo(AvatarVideoSelectVoice("portrait_url", "Hello, welcome to my channel!")) ->
          should.be_true(True)
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

pub fn avatar_video_voice_callback_test() {
  let session = UserSession(
    ..create_test_session(),
    scene: AvatarVideo(AvatarVideoSelectVoice("portrait", "script")),
  )

  let result = avatar_video_scene.handle_callback(session, "voice:sarah")

  should.be_ok(result)

  case result {
    Ok(scene_result) -> {
      case scene_result.session.scene {
        AvatarVideo(AvatarVideoGenerating("portrait", "script", "sarah", _)) ->
          should.be_true(True)
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

// ============================================================
// VoiceClone Scene Tests
// ============================================================

pub fn voice_clone_enter_test() {
  let session = create_test_session()

  let result = voice_clone_scene.enter(session)

  should.be_ok(result)

  case result {
    Ok(scene_result) -> {
      case scene_result.session.scene {
        VoiceClone(VoiceCloneUploadSample) -> should.be_true(True)
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

pub fn voice_clone_text_input_test() {
  let session = UserSession(
    ..create_test_session(),
    scene: VoiceClone(VoiceCloneEnterText("voice_123")),
  )

  let result = voice_clone_scene.handle_message(
    session,
    TextMessage("This is my cloned voice speaking"),
  )

  should.be_ok(result)

  case result {
    Ok(scene_result) -> {
      case scene_result.session.scene {
        VoiceClone(VoiceCloneGenerating("voice_123", "This is my cloned voice speaking", _)) ->
          should.be_true(True)
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

// ============================================================
// Cancel Flow Tests
// ============================================================

pub fn text_to_video_cancel_test() {
  let session = UserSession(
    ..create_test_session(),
    scene: TextToVideo(TextToVideoEnterPrompt("kling")),
  )

  let result = text_to_video_scene.handle_callback(session, "cancel")

  should.be_ok(result)

  case result {
    Ok(scene_result) -> {
      case scene_result.session.scene {
        Main(Idle) -> should.be_true(True)
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

pub fn morphing_cancel_test() {
  let session = UserSession(
    ..create_test_session(),
    scene: Morphing(MorphingUploadEnd("start_img")),
  )

  let result = morphing_scene.handle_callback(session, "cancel")

  should.be_ok(result)

  case result {
    Ok(scene_result) -> {
      case scene_result.session.scene {
        Main(Idle) -> should.be_true(True)
        _ -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

// ============================================================
// Scene JSON Serialization Tests
// ============================================================

pub fn text_to_video_scene_to_json_test() {
  let scene = TextToVideo(TextToVideoGenerating("kling", "test prompt", "job_123"))
  let json = scene.scene_to_json(scene)

  should.be_true(
    json
    |> string.contains("text_to_video")
  )
  should.be_true(
    json
    |> string.contains("generating")
  )
  should.be_true(
    json
    |> string.contains("kling")
  )
}

pub fn morphing_scene_to_json_test() {
  let scene = Morphing(MorphingEnterStyle("start", "end"))
  let json = scene.scene_to_json(scene)

  should.be_true(
    json
    |> string.contains("morphing")
  )
  should.be_true(
    json
    |> string.contains("enter_style")
  )
}

pub fn broll_scene_to_json_test() {
  let scene = BRoll(BRollGenerating("script", "cinematic", "job_456"))
  let json = scene.scene_to_json(scene)

  should.be_true(
    json
    |> string.contains("broll")
  )
  should.be_true(
    json
    |> string.contains("generating")
  )
}
