// User-Bot Setup Scene - Multi-step wizard for connecting user's Telegram account
// Allows clients to configure their own user-bot with Digital Twin or Observer mode

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/config/telegram_config
import vibee/integrations/telegram/client as bridge
import vibee/integrations/telegram/types.{TelegramConfig}
import vibee/bot/scene.{
  type IncomingMessage, type OutgoingMessage, type UserSession,
  CallbackQuery, Main, Idle, TextMessage, TextReply, TextWithKeyboard,
  UserBotSetup, UserBotWelcome, UserBotEnterPhone, UserBotWaitingCode,
  UserBotEnter2FA, UserBotAuthSuccess, UserBotSelectChats, UserBotChatsSelected,
  UserBotConfigureMode, UserBotModesConfigured, UserBotSetupTriggers,
  UserBotTriggersConfigured, UserBotSetupCharacter, UserBotCharacterConfigured,
  UserBotSummary, UserBotActivating, UserBotComplete,
  button, button_row, set_scene,
}
import vibee/bot/scene_handler.{
  type SceneError, type SceneResult, InvalidInput, InvalidState,
  NoAction, SceneResult,
}
import vibee/bot/userbot_setup_store

// ============================================================
// Scene Entry Point
// ============================================================

/// Handle /userbot command - start user-bot setup wizard
pub fn enter(session: UserSession) -> Result(SceneResult, SceneError) {
  // Create or reset progress in DB
  let _ = userbot_setup_store.create_progress(session.user_id)

  let new_session = set_scene(session, UserBotSetup(UserBotWelcome))

  let keyboard = [
    [button("Начать настройку", "ub:start")],
    [button("Отмена", "cancel")],
  ]

  Ok(SceneResult(
    session: new_session,
    response: Some(TextWithKeyboard(
      "User-Bot Setup Wizard\n\n" <>
      "Подключите свой Telegram аккаунт как user-bot для автоматизации:\n\n" <>
      "**Digital Twin** - AI отвечает от вашего имени\n" <>
      "**Observer** - Мониторит сообщения, пересылает лиды\n\n" <>
      "Шаги настройки:\n" <>
      "1. Авторизация вашего Telegram\n" <>
      "2. Выбор чатов для мониторинга\n" <>
      "3. Настройка режимов для каждого чата\n" <>
      "4. Триггерные слова\n" <>
      "5. Персонаж (для Digital Twin)\n" <>
      "6. Активация\n\n" <>
      "Нажмите 'Начать настройку' для продолжения.",
      keyboard,
    )),
    next_action: NoAction,
  ))
}

// ============================================================
// Message Handling
// ============================================================

pub fn handle_message(
  session: UserSession,
  message: IncomingMessage,
) -> Result(SceneResult, SceneError) {
  case session.scene {
    UserBotSetup(UserBotEnterPhone) -> handle_phone_input(session, message)
    UserBotSetup(UserBotWaitingCode(phone, session_id, _hash)) ->
      handle_code_input(session, message, phone, session_id)
    UserBotSetup(UserBotEnter2FA(phone, session_id)) ->
      handle_2fa_input(session, message, phone, session_id)
    UserBotSetup(UserBotSetupTriggers(_session_id)) ->
      handle_trigger_input(session, message)
    UserBotSetup(UserBotSetupCharacter(_session_id)) ->
      handle_character_input(session, message)
    _ -> {
      // Default: show current step message
      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Используйте кнопки для навигации или /menu для выхода.")),
        next_action: NoAction,
      ))
    }
  }
}

pub fn handle_callback(
  session: UserSession,
  data: String,
) -> Result(SceneResult, SceneError) {
  case string.split(data, ":") {
    ["ub", action] -> handle_wizard_action(session, action)
    ["chat", chat_id] -> handle_chat_selection(session, chat_id)
    ["mode", chat_id, mode] -> handle_mode_selection(session, chat_id, mode)
    ["trigger", preset] -> handle_trigger_preset(session, preset)
    ["cancel"] -> handle_cancel(session)
    _ -> Error(InvalidInput("Unknown callback: " <> data))
  }
}

// ============================================================
// Step Handlers
// ============================================================

fn handle_wizard_action(
  session: UserSession,
  action: String,
) -> Result(SceneResult, SceneError) {
  case action {
    "start" -> show_phone_input(session)
    "skip_character" -> show_summary(session)
    "activate" -> activate_userbot(session)
    "back" -> go_back(session)
    _ -> Error(InvalidInput("Unknown action: " <> action))
  }
}

fn show_phone_input(session: UserSession) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, UserBotSetup(UserBotEnterPhone))
  let _ = userbot_setup_store.update_step(session.user_id, "enter_phone")

  let keyboard = [
    [button("Отмена", "cancel")],
  ]

  Ok(SceneResult(
    session: new_session,
    response: Some(TextWithKeyboard(
      "Шаг 1: Авторизация\n\n" <>
      "Введите номер телефона вашего Telegram аккаунта в международном формате:\n\n" <>
      "Пример: +79001234567",
      keyboard,
    )),
    next_action: NoAction,
  ))
}

fn handle_phone_input(
  session: UserSession,
  message: IncomingMessage,
) -> Result(SceneResult, SceneError) {
  case message {
    TextMessage(phone) -> {
      // Validate phone format
      let clean_phone = string.trim(phone)
      case string.starts_with(clean_phone, "+") {
        True -> {
          // Connect to Bridge and send code
          let bridge_url = telegram_config.bridge_url()
          let api_key = telegram_config.bridge_api_key()
          let api_id = telegram_config.api_id()
          let api_hash = telegram_config.api_hash()

          io.println("[USERBOT_SETUP] Connecting to Bridge for phone: " <> clean_phone)

          let telegram_bridge = bridge.with_session_and_key(bridge_url, "", api_key)
          let config = TelegramConfig(
            base_url: bridge_url,
            app_id: api_id,
            app_hash: api_hash,
            phone: Some(clean_phone),
          )

          // Step 1: Connect to get session_id
          case bridge.connect(telegram_bridge, config) {
            Ok(#(connected_bridge, session_info)) -> {
              io.println("[USERBOT_SETUP] Connected, session: " <> session_info.session_id)

              // Step 2: Send code
              case bridge.send_code(connected_bridge, clean_phone) {
                Ok(phone_code_hash) -> {
                  io.println("[USERBOT_SETUP] Code sent, hash: " <> phone_code_hash)

                  let _ = userbot_setup_store.update_auth_info(
                    session.user_id,
                    clean_phone,
                    session_info.session_id,
                  )
                  let _ = userbot_setup_store.update_step(session.user_id, "waiting_code")

                  let new_session = set_scene(session, UserBotSetup(
                    UserBotWaitingCode(clean_phone, session_info.session_id, phone_code_hash)
                  ))

                  let keyboard = [
                    [button("Назад", "ub:back")],
                    [button("Отмена", "cancel")],
                  ]

                  Ok(SceneResult(
                    session: new_session,
                    response: Some(TextWithKeyboard(
                      "Код подтверждения отправлен на " <> clean_phone <> "\n\n" <>
                      "Введите код из SMS или Telegram:",
                      keyboard,
                    )),
                    next_action: NoAction,
                  ))
                }
                Error(e) -> {
                  io.println("[USERBOT_SETUP] Failed to send code: " <> string.inspect(e))
                  Ok(SceneResult(
                    session: session,
                    response: Some(TextReply(
                      "Ошибка отправки кода. Проверьте номер и попробуйте снова."
                    )),
                    next_action: NoAction,
                  ))
                }
              }
            }
            Error(e) -> {
              io.println("[USERBOT_SETUP] Failed to connect: " <> string.inspect(e))
              Ok(SceneResult(
                session: session,
                response: Some(TextReply(
                  "Ошибка подключения к Telegram. Попробуйте позже."
                )),
                next_action: NoAction,
              ))
            }
          }
        }
        False -> {
          Ok(SceneResult(
            session: session,
            response: Some(TextReply(
              "Неверный формат. Введите номер с кодом страны, например: +79001234567"
            )),
            next_action: NoAction,
          ))
        }
      }
    }
    _ -> Error(InvalidInput("Expected phone number"))
  }
}

fn handle_code_input(
  session: UserSession,
  message: IncomingMessage,
  phone: String,
  session_id: String,
) -> Result(SceneResult, SceneError) {
  case message {
    TextMessage(code) -> {
      let clean_code = string.trim(code)

      case string.length(clean_code) >= 5 {
        True -> {
          io.println("[USERBOT_SETUP] Verifying code for session: " <> session_id)

          let bridge_url = telegram_config.bridge_url()
          let api_key = telegram_config.bridge_api_key()
          let telegram_bridge = bridge.with_session_and_key(bridge_url, session_id, api_key)

          case bridge.verify_code(telegram_bridge, clean_code) {
            Ok(user) -> {
              let username = option.unwrap(user.username, "user_" <> int.to_string(user.id))
              io.println("[USERBOT_SETUP] Auth success: " <> username)

              let _ = userbot_setup_store.update_connected_account(
                session.user_id,
                user.id,
                username,
              )

              show_chat_selection(session, session_id)
            }
            Error(types.AuthError("2FA_REQUIRED")) -> {
              io.println("[USERBOT_SETUP] 2FA required")
              let _ = userbot_setup_store.update_step(session.user_id, "enter_2fa")

              let new_session = set_scene(session, UserBotSetup(
                UserBotEnter2FA(phone, session_id)
              ))

              let keyboard = [
                [button("Отмена", "cancel")],
              ]

              Ok(SceneResult(
                session: new_session,
                response: Some(TextWithKeyboard(
                  "Требуется двухфакторная аутентификация.\n\n" <>
                  "Введите ваш пароль 2FA:",
                  keyboard,
                )),
                next_action: NoAction,
              ))
            }
            Error(e) -> {
              io.println("[USERBOT_SETUP] Code verification failed: " <> string.inspect(e))
              Ok(SceneResult(
                session: session,
                response: Some(TextReply("Неверный код. Попробуйте еще раз:")),
                next_action: NoAction,
              ))
            }
          }
        }
        False -> {
          Ok(SceneResult(
            session: session,
            response: Some(TextReply("Неверный код. Попробуйте еще раз:")),
            next_action: NoAction,
          ))
        }
      }
    }
    _ -> Error(InvalidInput("Expected code"))
  }
}

fn handle_2fa_input(
  session: UserSession,
  message: IncomingMessage,
  _phone: String,
  session_id: String,
) -> Result(SceneResult, SceneError) {
  case message {
    TextMessage(password) -> {
      io.println("[USERBOT_SETUP] Verifying 2FA for session: " <> session_id)

      let bridge_url = telegram_config.bridge_url()
      let api_key = telegram_config.bridge_api_key()
      let telegram_bridge = bridge.with_session_and_key(bridge_url, session_id, api_key)

      case bridge.verify_2fa(telegram_bridge, password) {
        Ok(user) -> {
          let username = option.unwrap(user.username, "user_" <> int.to_string(user.id))
          io.println("[USERBOT_SETUP] 2FA success: " <> username)

          let _ = userbot_setup_store.update_connected_account(
            session.user_id,
            user.id,
            username,
          )

          show_chat_selection(session, session_id)
        }
        Error(e) -> {
          io.println("[USERBOT_SETUP] 2FA failed: " <> string.inspect(e))
          Ok(SceneResult(
            session: session,
            response: Some(TextReply("Неверный пароль 2FA. Попробуйте еще раз:")),
            next_action: NoAction,
          ))
        }
      }
    }
    _ -> Error(InvalidInput("Expected 2FA password"))
  }
}

fn show_chat_selection(
  session: UserSession,
  session_id: String,
) -> Result(SceneResult, SceneError) {
  io.println("[USERBOT_SETUP] Loading dialogs for session: " <> session_id)

  let bridge_url = telegram_config.bridge_url()
  let api_key = telegram_config.bridge_api_key()
  let telegram_bridge = bridge.with_session_and_key(bridge_url, session_id, api_key)

  // Get dialogs from Bridge
  let dialogs = case bridge.get_dialogs(telegram_bridge, 50) {
    Ok(d) -> d
    Error(_) -> []
  }

  // Filter only groups and supergroups
  let group_dialogs = list.filter(dialogs, fn(d) {
    case d.dialog_type {
      types.GroupDialog -> True
      types.SupergroupDialog -> True
      _ -> False
    }
  })

  io.println("[USERBOT_SETUP] Found " <> int.to_string(list.length(group_dialogs)) <> " groups")

  // Build keyboard from dialogs
  let chat_buttons = list.map(group_dialogs, fn(d) {
    [button(d.title, "chat:" <> int.to_string(d.id))]
  })

  let keyboard = list.flatten([
    list.take(chat_buttons, 10),  // Limit to 10 chats
    [[button("Готово - далее", "ub:chats_done")]],
    [[button("Отмена", "cancel")]],
  ])

  let available_chats = "[]"  // Will be filled as user selects
  let new_session = set_scene(session, UserBotSetup(
    UserBotSelectChats(session_id, available_chats)
  ))

  let chats_text = case list.length(group_dialogs) {
    0 -> "Групп не найдено. Убедитесь что вы состоите в группах."
    n -> "Найдено " <> int.to_string(n) <> " групп. Выберите для мониторинга:"
  }

  Ok(SceneResult(
    session: new_session,
    response: Some(TextWithKeyboard(
      "Шаг 2: Выбор чатов\n\n" <>
      chats_text <> "\n\n" <>
      "Нажмите на чат, чтобы выбрать.\n" <>
      "Когда закончите, нажмите 'Готово - далее'.",
      keyboard,
    )),
    next_action: NoAction,
  ))
}

fn handle_chat_selection(
  session: UserSession,
  chat_id: String,
) -> Result(SceneResult, SceneError) {
  // TODO: Toggle chat selection in progress
  // For now, just acknowledge

  Ok(SceneResult(
    session: session,
    response: Some(TextReply("Чат " <> chat_id <> " выбран.")),
    next_action: NoAction,
  ))
}

fn show_mode_configuration(
  session: UserSession,
  session_id: String,
) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, UserBotSetup(
    UserBotConfigureMode(session_id, 0, "Chat Name", "[]")
  ))

  let keyboard = [
    [button("Digital Twin (AI отвечает)", "mode:0:digital_twin")],
    [button("Observer (только мониторинг)", "mode:0:observer")],
    [button("Отмена", "cancel")],
  ]

  Ok(SceneResult(
    session: new_session,
    response: Some(TextWithKeyboard(
      "Шаг 3: Настройка режимов\n\n" <>
      "Выберите режим для каждого чата:\n\n" <>
      "**Digital Twin** - AI отвечает от вашего имени\n" <>
      "**Observer** - Только мониторит, не отвечает\n\n" <>
      "Чат: Chat Name",
      keyboard,
    )),
    next_action: NoAction,
  ))
}

fn handle_mode_selection(
  session: UserSession,
  chat_id: String,
  mode: String,
) -> Result(SceneResult, SceneError) {
  // TODO: Save mode selection
  // For now, proceed to next step

  let session_id = "sess_" <> int.to_string(session.user_id)
  show_trigger_setup(session, session_id)
}

fn show_trigger_setup(
  session: UserSession,
  session_id: String,
) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, UserBotSetup(
    UserBotSetupTriggers(session_id)
  ))
  let _ = userbot_setup_store.update_step(session.user_id, "setup_triggers")

  let keyboard = [
    [button("Крипто триггеры", "trigger:crypto")],
    [button("Недвижимость", "trigger:realty")],
    [button("IT/Фриланс", "trigger:it")],
    [button("Свои слова", "trigger:custom")],
    [button("Далее", "ub:triggers_done")],
    [button("Отмена", "cancel")],
  ]

  Ok(SceneResult(
    session: new_session,
    response: Some(TextWithKeyboard(
      "Шаг 4: Триггерные слова\n\n" <>
      "Выберите пресет или введите свои слова:\n\n" <>
      "**Крипто** - BTC, ETH, USDT, обмен, p2p...\n" <>
      "**Недвижимость** - аренда, продажа, квартира...\n" <>
      "**IT** - разработка, вакансия, проект...\n\n" <>
      "Или введите свои слова через запятую:",
      keyboard,
    )),
    next_action: NoAction,
  ))
}

fn handle_trigger_preset(
  session: UserSession,
  preset: String,
) -> Result(SceneResult, SceneError) {
  // Save preset
  let _ = userbot_setup_store.update_triggers(
    session.user_id,
    "[]",
    Some(preset),
  )

  Ok(SceneResult(
    session: session,
    response: Some(TextReply("Пресет '" <> preset <> "' выбран. Нажмите 'Далее' для продолжения.")),
    next_action: NoAction,
  ))
}

fn handle_trigger_input(
  session: UserSession,
  message: IncomingMessage,
) -> Result(SceneResult, SceneError) {
  case message {
    TextMessage(triggers) -> {
      // Parse comma-separated triggers
      let _ = userbot_setup_store.update_triggers(
        session.user_id,
        "[\"" <> triggers <> "\"]",
        Some("custom"),
      )

      Ok(SceneResult(
        session: session,
        response: Some(TextReply("Триггеры сохранены. Нажмите 'Далее' для продолжения.")),
        next_action: NoAction,
      ))
    }
    _ -> Error(InvalidInput("Expected trigger words"))
  }
}

fn show_character_setup(
  session: UserSession,
  session_id: String,
) -> Result(SceneResult, SceneError) {
  let new_session = set_scene(session, UserBotSetup(
    UserBotSetupCharacter(session_id)
  ))
  let _ = userbot_setup_store.update_step(session.user_id, "setup_character")

  let keyboard = [
    [button("Пропустить (только Observer)", "ub:skip_character")],
    [button("Отмена", "cancel")],
  ]

  Ok(SceneResult(
    session: new_session,
    response: Some(TextWithKeyboard(
      "Шаг 5: Настройка персонажа (Digital Twin)\n\n" <>
      "Опишите, как AI должен отвечать от вашего имени:\n\n" <>
      "1. Имя/псевдоним\n" <>
      "2. Стиль общения (формальный/неформальный)\n" <>
      "3. Краткая био\n\n" <>
      "Пример:\n" <>
      "Имя: Алекс\n" <>
      "Стиль: дружелюбный, профессиональный\n" <>
      "Био: Криптотрейдер, помогаю с P2P обменом\n\n" <>
      "Или нажмите 'Пропустить' если используете только режим Observer.",
      keyboard,
    )),
    next_action: NoAction,
  ))
}

fn handle_character_input(
  session: UserSession,
  message: IncomingMessage,
) -> Result(SceneResult, SceneError) {
  case message {
    TextMessage(config) -> {
      // Parse character config
      let _ = userbot_setup_store.update_character(
        session.user_id,
        "Custom",
        config,
        "",
        "[]",
      )

      show_summary(session)
    }
    _ -> Error(InvalidInput("Expected character config"))
  }
}

fn show_summary(session: UserSession) -> Result(SceneResult, SceneError) {
  let session_id = "sess_" <> int.to_string(session.user_id)
  let new_session = set_scene(session, UserBotSetup(
    UserBotSummary(session_id, "{}")
  ))
  let _ = userbot_setup_store.update_step(session.user_id, "summary")

  let keyboard = [
    [button("Активировать User-Bot", "ub:activate")],
    [button("Назад", "ub:back")],
    [button("Отмена", "cancel")],
  ]

  Ok(SceneResult(
    session: new_session,
    response: Some(TextWithKeyboard(
      "Шаг 6: Подтверждение\n\n" <>
      "Ваш User-Bot готов к активации!\n\n" <>
      "Настройки:\n" <>
      "- Подключен аккаунт: @user_xxx\n" <>
      "- Мониторинг: 3 чата\n" <>
      "- Режимы: Digital Twin (1), Observer (2)\n" <>
      "- Триггеры: crypto preset\n\n" <>
      "Нажмите 'Активировать User-Bot' для запуска.",
      keyboard,
    )),
    next_action: NoAction,
  ))
}

fn activate_userbot(session: UserSession) -> Result(SceneResult, SceneError) {
  // Complete setup in DB
  let _ = userbot_setup_store.complete_setup(session.user_id)

  let session_id = "sess_" <> int.to_string(session.user_id)
  let new_session = set_scene(session, UserBotSetup(
    UserBotComplete(session_id)
  ))

  let keyboard = [
    [button("Меню", "menu")],
  ]

  Ok(SceneResult(
    session: new_session,
    response: Some(TextWithKeyboard(
      "User-Bot активирован!\n\n" <>
      "Ваш аккаунт теперь мониторит выбранные чаты.\n\n" <>
      "Команды управления:\n" <>
      "/userbot_status - статус\n" <>
      "/userbot_pause - приостановить\n" <>
      "/userbot_resume - возобновить\n\n" <>
      "Лиды будут пересылаться вам в этот чат.",
      keyboard,
    )),
    next_action: NoAction,
  ))
}

fn go_back(session: UserSession) -> Result(SceneResult, SceneError) {
  // Navigate back based on current step
  case session.scene {
    UserBotSetup(UserBotWaitingCode(_, _, _)) -> show_phone_input(session)
    UserBotSetup(UserBotEnter2FA(_, _)) -> show_phone_input(session)
    UserBotSetup(UserBotSelectChats(session_id, _)) -> {
      let new_session = set_scene(session, UserBotSetup(
        UserBotAuthSuccess(session_id, "", "")
      ))
      show_phone_input(new_session)
    }
    UserBotSetup(UserBotSummary(session_id, _)) ->
      show_character_setup(session, session_id)
    _ -> enter(session)
  }
}

fn handle_cancel(session: UserSession) -> Result(SceneResult, SceneError) {
  // Reset to main menu
  let _ = userbot_setup_store.delete_progress(session.user_id)
  let new_session = set_scene(session, Main(Idle))

  Ok(SceneResult(
    session: new_session,
    response: Some(TextReply("Настройка User-Bot отменена. Используйте /menu для возврата.")),
    next_action: NoAction,
  ))
}
