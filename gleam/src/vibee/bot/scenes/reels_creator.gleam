// ReelsCreator Scene - Conversational AI agent for creating Instagram-style reels
// Collects idea, niche, product info and generates video using user's avatar

import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/bot/scene.{
  type IncomingMessage, type OutgoingMessage, type UserSession,
  CallbackQuery, Command, Main, Idle, ReelsCreator, ReelsSelectTemplate,
  ReelsEnterIdea, ReelsEnterNiche, ReelsEnterProduct, ReelsConfirmDetails,
  ReelsGenerating, ReelsResult, TextMessage, TextReply, TextWithKeyboard,
  button, set_scene, available_reels_templates, available_reels_niches,
}
import vibee/bot/scene_handler.{
  type SceneError, type SceneResult, InvalidInput, InvalidState,
  JobReelsCreator, NoAction, SceneResult, StartJob,
}

// ============================================================
// Scene Entry Point
// ============================================================

/// Handle /reels command - start reels creation flow
pub fn enter(session: UserSession) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, ReelsCreator(ReelsSelectTemplate))

  let templates = available_reels_templates()
  let keyboard = list.map(templates, fn(t) {
    [button(t.name, "reels:template:" <> t.id)]
  })
  let keyboard_with_cancel = list.append(keyboard, [[button("Отмена", "cancel")]])

  Ok(SceneResult(
    session: new_session,
    response: Some(TextWithKeyboard(
      "Создаём рилс!\n\n" <>
      "Выберите шаблон видео:\n\n" <>
      "**Split Talking Head** - аватар слева, B-roll справа\n" <>
      "**Fullscreen Avatar** - полноэкранный говорящий аватар\n" <>
      "**Picture in Picture** - B-roll с аватаром в углу",
      keyboard_with_cancel,
    )),
    next_action: NoAction,
  ))
}

// ============================================================
// Message Handling
// ============================================================

/// Handle messages in ReelsCreator scene
pub fn handle_message(
  session: UserSession,
  message: IncomingMessage,
) -> Result(SceneResult, SceneError) {
  case session.scene {
    ReelsCreator(ReelsEnterIdea(template_id)) ->
      handle_idea_input(session, message, template_id)

    ReelsCreator(ReelsEnterProduct(template_id, idea, niche)) ->
      handle_product_input(session, message, template_id, idea, niche)

    ReelsCreator(_) ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Используйте кнопки для навигации.")),
        next_action: NoAction,
      ))

    _ -> Error(InvalidState("Not in ReelsCreator scene"))
  }
}

/// Handle idea text input
fn handle_idea_input(
  session: UserSession,
  message: IncomingMessage,
  template_id: String,
) -> Result(SceneResult, SceneError) {
  case message {
    TextMessage(idea) -> {
      let trimmed = string.trim(idea)
      case string.length(trimmed) > 5 {
        True -> show_niche_selection(session, template_id, trimmed)
        False -> Error(InvalidInput("Идея слишком короткая. Опишите подробнее о чём будет рилс."))
      }
    }
    Command("cancel", _) -> cancel_scene(session)
    Command("skip", _) -> Error(InvalidInput("Идея обязательна для создания рилса."))
    _ ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Введите текст идеи для рилса.")),
        next_action: NoAction,
      ))
  }
}

/// Handle product description input
fn handle_product_input(
  session: UserSession,
  message: IncomingMessage,
  template_id: String,
  idea: String,
  niche: String,
) -> Result(SceneResult, SceneError) {
  case message {
    TextMessage(product) -> {
      let trimmed = string.trim(product)
      case trimmed {
        "" -> show_confirmation(session, template_id, idea, niche, None)
        p -> show_confirmation(session, template_id, idea, niche, Some(p))
      }
    }
    Command("cancel", _) -> cancel_scene(session)
    Command("skip", _) -> show_confirmation(session, template_id, idea, niche, None)
    _ ->
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Опишите ваш продукт или введите /skip чтобы пропустить.")),
        next_action: NoAction,
      ))
  }
}

// ============================================================
// Callback Handling
// ============================================================

/// Handle callbacks in ReelsCreator scene
pub fn handle_callback(
  session: UserSession,
  data: String,
) -> Result(SceneResult, SceneError) {
  case data {
    "cancel" -> cancel_scene(session)
    "back_menu" -> cancel_scene(session)
    "reels:again" -> enter(session)
    "reels:skip_product" -> {
      case session.scene {
        ReelsCreator(ReelsEnterProduct(template_id, idea, niche)) ->
          show_confirmation(session, template_id, idea, niche, None)
        _ -> Error(InvalidState("Cannot skip product from current state"))
      }
    }
    "reels:confirm" -> {
      case session.scene {
        ReelsCreator(ReelsConfirmDetails(template_id, idea, niche, product, _photo_url)) ->
          start_generation(session, template_id, idea, niche, product)
        _ -> Error(InvalidState("Cannot confirm from current state"))
      }
    }
    "reels:edit" -> enter(session)
    _ -> {
      // Parse template selection: reels:template:split-talking-head
      case string.starts_with(data, "reels:template:") {
        True -> {
          let template_id = string.drop_start(data, 15)
          select_template(session, template_id)
        }
        False -> {
          // Parse niche selection: reels:niche:business
          case string.starts_with(data, "reels:niche:") {
            True -> {
              let niche = string.drop_start(data, 12)
              case session.scene {
                ReelsCreator(ReelsEnterNiche(template_id, idea)) ->
                  select_niche(session, template_id, idea, niche)
                _ -> Error(InvalidState("Cannot select niche from current state"))
              }
            }
            False -> Error(InvalidInput("Unknown callback: " <> data))
          }
        }
      }
    }
  }
}

// ============================================================
// State Transitions
// ============================================================

/// Handle template selection
fn select_template(
  session: UserSession,
  template_id: String,
) -> Result(SceneResult, SceneError) {
  let templates = available_reels_templates()
  let template = list.find(templates, fn(t) { t.id == template_id })

  case template {
    Ok(t) -> {
      let new_session = set_scene(session, ReelsCreator(ReelsEnterIdea(template_id)))

      Ok(SceneResult(
        session: new_session,
        response: Some(TextWithKeyboard(
          "Шаблон: **" <> t.name <> "**\n\n" <>
          "О чём будет рилс?\n\n" <>
          "Примеры:\n" <>
          "- 5 способов повысить продуктивность\n" <>
          "- Как я заработал первый миллион\n" <>
          "- 3 ошибки начинающих предпринимателей\n\n" <>
          "Введите вашу идею:",
          [[button("Отмена", "cancel")]],
        )),
        next_action: NoAction,
      ))
    }
    Error(_) -> Error(InvalidInput("Unknown template: " <> template_id))
  }
}

/// Show niche selection keyboard
fn show_niche_selection(
  session: UserSession,
  template_id: String,
  idea: String,
) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, ReelsCreator(ReelsEnterNiche(template_id, idea)))

  let niches = available_reels_niches()
  let keyboard = list.map(niches, fn(n) {
    [button(n.emoji <> " " <> n.name, "reels:niche:" <> n.id)]
  })
  let keyboard_with_back = list.append(keyboard, [[button("Назад", "reels:edit")]])

  Ok(SceneResult(
    session: new_session,
    response: Some(TextWithKeyboard(
      "Идея: **" <> idea <> "**\n\n" <>
      "Выберите нишу вашего контента:",
      keyboard_with_back,
    )),
    next_action: NoAction,
  ))
}

/// Handle niche selection
fn select_niche(
  session: UserSession,
  template_id: String,
  idea: String,
  niche: String,
) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, ReelsCreator(ReelsEnterProduct(template_id, idea, niche)))

  let niches = available_reels_niches()
  let niche_name = case list.find(niches, fn(n) { n.id == niche }) {
    Ok(n) -> n.name
    Error(_) -> niche
  }

  Ok(SceneResult(
    session: new_session,
    response: Some(TextWithKeyboard(
      "Ниша: **" <> niche_name <> "**\n\n" <>
      "Опишите ваш продукт или услугу (если есть).\n\n" <>
      "Это поможет сгенерировать более релевантный контент с призывом к действию.\n\n" <>
      "Например:\n" <>
      "- Мой курс \"Продуктивность 2.0\"\n" <>
      "- Консультации по маркетингу\n" <>
      "- Мобильное приложение для трекинга привычек\n\n" <>
      "Введите описание продукта или нажмите Пропустить:",
      [[button("Пропустить", "reels:skip_product")], [button("Назад", "reels:edit")]],
    )),
    next_action: NoAction,
  ))
}

/// Show confirmation with summary
fn show_confirmation(
  session: UserSession,
  template_id: String,
  idea: String,
  niche: String,
  product: Option(String),
) -> Result(SceneResult, SceneError) {
  // Photo URL will be fetched during generation
  let new_session = set_scene(session, ReelsCreator(ReelsConfirmDetails(
    template_id, idea, niche, product, None,
  )))

  let templates = available_reels_templates()
  let template_name = case list.find(templates, fn(t) { t.id == template_id }) {
    Ok(t) -> t.name
    Error(_) -> template_id
  }

  let niches = available_reels_niches()
  let niche_name = case list.find(niches, fn(n) { n.id == niche }) {
    Ok(n) -> n.emoji <> " " <> n.name
    Error(_) -> niche
  }

  let product_text = case product {
    Some(p) -> "Продукт: " <> p <> "\n"
    None -> ""
  }

  Ok(SceneResult(
    session: new_session,
    response: Some(TextWithKeyboard(
      "Готово! Проверьте данные:\n\n" <>
      "Шаблон: **" <> template_name <> "**\n" <>
      "Идея: " <> idea <> "\n" <>
      "Ниша: " <> niche_name <> "\n" <>
      product_text <>
      "Фото: ваш аватар из Telegram\n\n" <>
      "Нажмите **Создать рилс** чтобы начать генерацию.",
      [[button("Создать рилс", "reels:confirm")], [button("Изменить", "reels:edit")], [button("Отмена", "cancel")]],
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
  template_id: String,
  idea: String,
  niche: String,
  product: Option(String),
) -> Result(SceneResult, SceneError) {
  // Job ID will be set by the job system
  let job_id = "pending"
  // Photo URL will be fetched by the pipeline
  let photo_url = "pending"

  let new_session = set_scene(
    session,
    ReelsCreator(ReelsGenerating(template_id, idea, niche, product, photo_url, job_id)),
  )

  let params = dict.new()
    |> dict.insert("template_id", template_id)
    |> dict.insert("idea", idea)
    |> dict.insert("niche", niche)
    |> dict.insert("product", option.unwrap(product, ""))
    |> dict.insert("chat_id", session.chat_id)
    |> dict.insert("user_id", string.inspect(session.user_id))

  Ok(SceneResult(
    session: new_session,
    response: Some(TextReply(
      "Генерация рилса...\n\n" <>
      "Этапы:\n" <>
      "1. Получение фото профиля\n" <>
      "2. Генерация скрипта (AI)\n" <>
      "3. Озвучка (TTS)\n" <>
      "4. Создание аватара\n" <>
      "5. Генерация B-roll видео\n" <>
      "6. Финальный рендер\n\n" <>
      "Это может занять 2-5 минут. Я отправлю видео когда будет готово."
    )),
    next_action: StartJob(JobReelsCreator, params),
  ))
}

/// Handle generation result (called when job completes)
pub fn handle_result(
  session: UserSession,
  video_url: String,
) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, ReelsCreator(ReelsResult(video_url)))

  let keyboard = [
    [button("Ещё один рилс", "reels:again")],
    [button("В меню", "back_menu")],
  ]

  Ok(SceneResult(
    session: new_session,
    response: Some(TextWithKeyboard(
      "Ваш рилс готов!",
      keyboard,
    )),
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
    response: Some(TextReply("Отменено. Используйте /menu для просмотра опций.")),
    next_action: NoAction,
  ))
}
