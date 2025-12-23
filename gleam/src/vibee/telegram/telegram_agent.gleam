// VIBEE Telegram Agent
// Аналог TelegramService из plugin-telegram-craft
// Работает через Go bridge для MTProto

import gleam/dict
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/vibe_logger
import shellout
import vibee/mcp/config
import vibee/config/dynamic_config
import vibee/config/target_chats
import vibee/config/telegram_config
import vibee/config/trigger_chats
import vibee/config/twin_config
import vibee/db/postgres
import vibee/leads/lead_logger
import vibee/logging
import vibee/mcp/session_manager
import vibee/telegram/conversation_tracker
import vibee/telegram/dialog_forwarder
import vibee/integrations/telegram/bot_api
import vibee/integrations/telegram/client as tg_client
import vibee/integrations/telegram/types as tg_types
import vibee/agent/reels_plugin
import vibee/agent/eliza.{ActionContext}

/// Get VIBEE_API_KEY from environment
@external(erlang, "vibee_polling_ffi", "get_api_key")
fn get_api_key() -> String

/// Конфигурация Telegram агента
pub type TelegramAgentConfig {
  TelegramAgentConfig(
    bridge_url: String,
    session_id: String,
    llm_api_key: Option(String),
    llm_model: String,
    auto_reply_enabled: Bool,
    cooldown_ms: Int,
    // Digital Twin mode settings
    digital_twin_enabled: Bool,
    owner_id: Int,
    // 0 = respond to all personal chats
  )
}

/// Digital Twin режим - отвечает на ВСЕ личные сообщения от имени пользователя
pub type DigitalTwinMode {
  TwinDisabled
  TwinAllPersonalChats
  TwinWhitelistOnly(List(Int))
}

/// Состояние агента
pub type AgentState {
  AgentState(
    config: TelegramAgentConfig,
    bot_user_id: Option(Int),  // Real user_id from session
    is_monitoring: Bool,
    total_messages: Int,
    last_reply_time: Int,
    monitored_chats: List(String),
  )
}

/// Сообщения для актора
pub type AgentMessage {
  StartMonitoring
  StopMonitoring
  ProcessMessage(chat_id: String, from_name: String, text: String, message_id: Int)
  SendReply(chat_id: String, text: String, reply_to: Option(Int))
  GetStatus
  Shutdown
}

/// Результат отправки сообщения
pub type SendResult {
  SendOk(message_id: Int)
  SendError(reason: String)
}

/// Создать конфигурацию по умолчанию (используем централизованный конфиг из БД)
pub fn default_config() -> TelegramAgentConfig {
  // Get active session from session manager, fall back to empty string if none
  let session_id = case session_manager.get_active() {
    Some(sid) -> sid
    None -> ""
  }
  TelegramAgentConfig(
    bridge_url: telegram_config.bridge_url(),
    session_id: session_id,
    llm_api_key: None,
    llm_model: get_llm_model_from_db(),
    auto_reply_enabled: True,
    cooldown_ms: 30_000,
    // Digital Twin mode - enabled by default for owner
    digital_twin_enabled: True,
    owner_id: get_owner_id_from_db(),
  )
}

/// Получить модель LLM из базы данных
fn get_llm_model_from_db() -> String {
  case postgres.get_global_pool() {
    Some(pool) ->
      case twin_config.get_active(pool) {
        Ok(cfg) -> cfg.settings.model
        Error(_) -> "x-ai/grok-4.1-fast"
      }
    None -> "x-ai/grok-4.1-fast"
  }
}

/// Получить owner_id из базы данных
fn get_owner_id_from_db() -> Int {
  case postgres.get_global_pool() {
    Some(pool) -> dynamic_config.get_owner_id(pool)
    None -> 144_022_504  // fallback
  }
}

/// Конфигурация для Digital Twin режима
pub fn digital_twin_config(owner_telegram_id: Int) -> TelegramAgentConfig {
  let base = default_config()
  TelegramAgentConfig(
    ..base,
    digital_twin_enabled: True,
    owner_id: owner_telegram_id,
    auto_reply_enabled: True,
  )
}

/// Инициализация агента
pub fn init(config: TelegramAgentConfig) -> AgentState {
  logging.quick_info("Telegram Agent initialized")
  logging.quick_info("Bridge URL: " <> config.bridge_url)
  logging.quick_info("Auto-reply: " <> case config.auto_reply_enabled {
    True -> "enabled"
    False -> "disabled"
  })

  AgentState(
    config: config,
    bot_user_id: None,  // Will be set on first message
    is_monitoring: False,
    total_messages: 0,
    last_reply_time: 0,
    monitored_chats: target_chats.target_chats(),
  )
}

/// Get user_id from session (lazy initialization)
fn get_or_fetch_user_id(state: AgentState) -> #(AgentState, Option(Int)) {
  let log = vibe_logger.new("user_id")
    |> vibe_logger.with_session_id(state.config.session_id)

  case state.bot_user_id {
    Some(id) -> {
      vibe_logger.debug(log |> vibe_logger.with_data("bot_user_id", json.int(id)), "Using cached bot_user_id")
      #(state, Some(id))
    }
    None -> {
      vibe_logger.debug(log, "Fetching bot_user_id from session")
      // Fetch user_id from getMe
      case get_me(state.config.bridge_url, state.config.session_id) {
        Ok(user_id) -> {
          vibe_logger.info(log |> vibe_logger.with_data("user_id", json.int(user_id)), "Bot user_id fetched")
          let new_state = AgentState(..state, bot_user_id: Some(user_id))
          #(new_state, Some(user_id))
        }
        Error(reason) -> {
          vibe_logger.error(log |> vibe_logger.with_data("error", json.string(reason)), "Failed to get user_id")
          #(state, None)
        }
      }
    }
  }
}

/// Get current user info from Telegram
fn get_me(bridge_url: String, session_id: String) -> Result(Int, String) {
  // Parse bridge_url properly for https
  let #(scheme, host, port) = case string.starts_with(bridge_url, "https://") {
    True -> {
      let h = string.drop_start(bridge_url, 8)
        |> string.split("/")
        |> list.first
        |> fn(r) { case r { Ok(v) -> v _ -> "localhost" } }
      #(http.Https, h, 443)
    }
    False -> {
      case string.starts_with(bridge_url, "http://") {
        True -> {
          let rest = string.drop_start(bridge_url, 7)
            |> string.split("/")
            |> list.first
            |> fn(r) { case r { Ok(v) -> v _ -> "localhost:8081" } }
          case string.split(rest, ":") {
            [h, p] -> #(http.Http, h, case int.parse(p) { Ok(n) -> n Error(_) -> 8081 })
            [h] -> #(http.Http, h, 80)
            _ -> #(http.Http, "localhost", 8081)
          }
        }
        False -> #(http.Http, "localhost", 8081)
      }
    }
  }

  let api_key = get_api_key()

  let req = request.new()
    |> request.set_method(http.Get)
    |> request.set_scheme(scheme)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path("/api/v1/me")
    |> request.set_header("X-Session-ID", session_id)
    |> request.set_header("Authorization", "Bearer " <> api_key)

  let log = vibe_logger.new("getme") |> vibe_logger.with_session_id(session_id)

  case httpc.send(req) {
    Ok(response) -> {
      vibe_logger.debug(log |> vibe_logger.with_data("body", json.string(string.slice(response.body, 0, 200))), "Response received")
      // Parse JSON response to get user_id
      // Expected: {"id": 123456789, "username": "...", ...}
      case string.split(response.body, "\"id\":") {
        [_, rest] -> {
          case string.split(rest, ",") {
            [id_str, ..] -> {
              let cleaned = string.replace(id_str, " ", "")
                |> string.replace("}", "")
                |> string.replace("\"", "")
              case int.parse(cleaned) {
                Ok(id) -> {
                  vibe_logger.info(log |> vibe_logger.with_data("user_id", json.int(id)), "Got user_id")
                  Ok(id)
                }
                Error(_) -> Error("Failed to parse user_id")
              }
            }
            _ -> Error("Invalid response format")
          }
        }
        _ -> Error("No id field in response")
      }
    }
    Error(_err) -> {
      vibe_logger.error(log, "HTTP error")
      Error("HTTP request failed")
    }
  }
}

/// Сохранить лид (логирование)
fn save_lead_to_database(
  from_id: Int,
  from_name: String,
  message_text: String,
  chat_id: String,
  agent_response: String,
  trigger_words: List(String),
) {
  // Парсим имя пользователя
  let parts = string.split(from_name, " ")
  let first_name = case list.first(parts) {
    Ok(name) -> Some(name)
    Error(_) -> Some(from_name)
  }
  let last_name = case list.rest(parts) {
    Ok(rest) -> case list.first(rest) {
      Ok(name) -> Some(name)
      Error(_) -> None
    }
    Error(_) -> None
  }
  
  // Парсим chat_id
  let source_chat_id = case int.parse(chat_id) {
    Ok(id) -> id
    Error(_) -> -5082217642
  }
  
  // Сохраняем лид (пока только логирование)
  let _ = lead_logger.save_lead(
    from_id,
    None,  // username (TODO: получать из API)
    first_name,
    last_name,
    message_text,
    source_chat_id,
    "Aimly.io dev",
    trigger_words,
    agent_response,
  )
  
  Nil
}

/// Обработка входящего сообщения
pub fn handle_incoming_message(
  state: AgentState,
  chat_id: String,
  from_id: Int,
  from_name: String,
  username: String,
  phone: String,
  lang_code: String,
  is_premium: Bool,
  text: String,
  message_id: Int,
  reply_to_id: Int,
) -> AgentState {
  let log = vibe_logger.new("msg")
    |> vibe_logger.with_session_id(state.config.session_id)
    |> vibe_logger.with_data("chat_id", json.string(chat_id))
    |> vibe_logger.with_data("from_id", json.int(from_id))
    |> vibe_logger.with_data("from", json.string(from_name))
    |> vibe_logger.with_data("username", json.string(username))
    |> vibe_logger.with_data("phone", json.string(phone))
    |> vibe_logger.with_data("lang", json.string(lang_code))
    |> vibe_logger.with_data("premium", json.bool(is_premium))
    |> vibe_logger.with_data("text", json.string(string.slice(text, 0, 50)))
    |> vibe_logger.with_data("reply_to", json.int(reply_to_id))

  // Логируем если это reply на сообщение
  case reply_to_id > 0 {
    True -> vibe_logger.info(log, "Incoming REPLY message")
    False -> vibe_logger.info(log, "Incoming message")
  }

  // Get or fetch bot user_id
  let #(updated_state, bot_id) = get_or_fetch_user_id(state)

  // Не отвечаем на собственные сообщения (по user_id или owner_id, предотвращаем бесконечный цикл)
  // НО! В trigger-чатах (SNIPER MODE) разрешаем сообщения от owner для тестирования
  let filter_log = vibe_logger.new("filter")
    |> vibe_logger.with_data("from_id", json.int(from_id))
    |> vibe_logger.with_data("owner_id", json.int(updated_state.config.owner_id))
    |> vibe_logger.with_data("chat_id", json.string(chat_id))

  // Проверяем, является ли это trigger-чатом (SNIPER MODE)
  let is_trigger_chat = trigger_chats.is_trigger_chat_active(chat_id)
  io.println("[FILTER] Checking chat " <> chat_id <> " is_trigger_chat=" <> case is_trigger_chat { True -> "YES" False -> "NO" })

  // Проверяем, это команда (начинается с / ИЛИ NLP-распознанная команда)
  let is_slash_command = string.starts_with(text, "/")
  let is_nlp_command = case detect_nlp_command(text) {
    Some(_) -> True
    None -> False
  }
  let is_command = is_slash_command || is_nlp_command
  // Личный чат = положительный chat_id
  let is_private_chat = case int.parse(chat_id) {
    Ok(cid) -> cid > 0
    Error(_) -> False
  }

  let should_skip = case bot_id {
    Some(id) -> {
      let is_bot = from_id == id
      let is_owner = from_id == updated_state.config.owner_id
      vibe_logger.debug(filter_log
        |> vibe_logger.with_data("bot_id", json.int(id))
        |> vibe_logger.with_data("is_bot", json.bool(is_bot))
        |> vibe_logger.with_data("is_owner", json.bool(is_owner))
        |> vibe_logger.with_data("is_command", json.bool(is_command))
        |> vibe_logger.with_data("is_private_chat", json.bool(is_private_chat))
        |> vibe_logger.with_data("is_trigger_chat", json.bool(is_trigger_chat)), "Filter check")
      // В trigger-чатах или при отправке команды в личном чате разрешаем сообщения от owner
      case is_trigger_chat || { is_command && is_private_chat } {
        True -> is_bot  // Только пропускаем сообщения бота, owner разрешён
        False -> is_bot || is_owner  // В обычных чатах пропускаем и бота, и owner (защита от цикла)
      }
    }
    None -> {
      vibe_logger.debug(filter_log
        |> vibe_logger.with_data("is_trigger_chat", json.bool(is_trigger_chat))
        |> vibe_logger.with_data("is_command", json.bool(is_command))
        |> vibe_logger.with_data("is_private_chat", json.bool(is_private_chat)), "No bot_id cached, checking owner_id only")
      // В trigger-чатах или при отправке команды в личном чате разрешаем сообщения от owner
      case is_trigger_chat || { is_command && is_private_chat } {
        True -> False  // Не пропускаем - разрешаем owner
        False -> from_id == updated_state.config.owner_id  // В обычных чатах пропускаем owner
      }
    }
  }

  case should_skip {
    True -> {
      vibe_logger.debug(log |> vibe_logger.with_data("action", json.string("skip")), "Skipping own message")
      updated_state
    }
    False -> {
      vibe_logger.debug(log |> vibe_logger.with_data("action", json.string("process")), "Processing message")
      let cmd_log = vibe_logger.new("cmd")
        |> vibe_logger.with_session_id(updated_state.config.session_id)
        |> vibe_logger.with_data("chat_id", json.string(chat_id))

      // Сначала проверяем команды (работают везде, включая личные чаты)
      case parse_command(text) {
        Some(#("neurophoto", prompt)) -> {
          vibe_logger.info(cmd_log |> vibe_logger.with_data("command", json.string("neurophoto")) |> vibe_logger.with_data("prompt", json.string(prompt)), "Command detected")
          handle_neurophoto_command(updated_state, chat_id, message_id, prompt)
        }
        Some(#("neuro", prompt)) -> {
          vibe_logger.info(cmd_log |> vibe_logger.with_data("command", json.string("neuro")) |> vibe_logger.with_data("prompt", json.string(prompt)), "Command detected")
          handle_neurophoto_command(updated_state, chat_id, message_id, prompt)
        }
        Some(#("start", _)) -> {
          vibe_logger.info(cmd_log |> vibe_logger.with_data("command", json.string("start")), "Command detected")
          let welcome = "Privet! Ya VIBEE - AI agent dlya generacii izobrazhenij.\n\nKomandy:\n/neurophoto <prompt> - generaciya izobrazheniya\n/neuro <prompt> - korotkaya versiya\n\nPrimer: /neurophoto cyberpunk portrait neon lights"
          let _ = send_message(updated_state.config, chat_id, welcome, Some(message_id))
          AgentState(..updated_state, total_messages: updated_state.total_messages + 1)
        }
        Some(#("help", _)) -> {
          vibe_logger.info(cmd_log |> vibe_logger.with_data("command", json.string("help")), "Command detected")
          let is_ru = is_cyrillic_text(text)
          let help_text = case is_ru {
            True -> "🤖 VIBEE Bot - Команды:\n\n📸 Изображения:\n/neurophoto <prompt> - Генерация с FLUX LoRA\n/neuro <prompt> - Короткая версия\n\n🎬 Видео:\n/video <описание> - Text-to-Video (Kling)\n/i2v - Image-to-Video\n/morph - Морфинг изображений\n/broll <тема> - B-Roll генерация\n\n🎤 Аудио:\n/voice <текст> - Голосовой синтез (ElevenLabs)\n/talking <текст> - Говорящий аватар (Hedra)\n\n💰 Тарифы:\n/pricing - Показать тарифы\n/quiz - Подобрать тариф\n\n💡 Trigger слово NEURO_SAGE добавляется автоматически."
            False -> "🤖 VIBEE Bot - Commands:\n\n📸 Images:\n/neurophoto <prompt> - FLUX LoRA generation\n/neuro <prompt> - Short version\n\n🎬 Video:\n/video <description> - Text-to-Video (Kling)\n/i2v - Image-to-Video\n/morph - Image morphing\n/broll <topic> - B-Roll generation\n\n🎤 Audio:\n/voice <text> - Voice synthesis (ElevenLabs)\n/talking <text> - Talking avatar (Hedra)\n\n💰 Pricing:\n/pricing - Show pricing\n/quiz - Find your plan\n\n💡 Trigger word NEURO_SAGE is added automatically."
          }
          let _ = send_message(updated_state.config, chat_id, help_text, Some(message_id))
          AgentState(..updated_state, total_messages: updated_state.total_messages + 1)
        }
        Some(#("pricing", _)) -> {
          vibe_logger.info(cmd_log |> vibe_logger.with_data("command", json.string("pricing")), "Command detected")
          let is_ru = is_cyrillic_text(text)
          let pricing_text = case is_ru {
            True -> "💎 VIBEE Тарифы:\n\n🥉 JUNIOR - $99/мес\n• 100 генераций\n• Telegram бот\n• Email поддержка\n\n🥈 MIDDLE - $299/мес\n• 500 генераций\n• Custom персона\n• CRM + Аналитика\n\n🥇 SENIOR - $999/мес\n• Безлимит генераций\n• Мультиканал\n• API доступ + SLA\n\n👉 /quiz - подобрать тариф"
            False -> "💎 VIBEE Pricing:\n\n🥉 JUNIOR - $99/mo\n• 100 generations\n• Telegram bot\n• Email support\n\n🥈 MIDDLE - $299/mo\n• 500 generations\n• Custom persona\n• CRM + Analytics\n\n🥇 SENIOR - $999/mo\n• Unlimited generations\n• Multichannel\n• API access + SLA\n\n👉 /quiz - find your plan"
          }
          let _ = send_message(updated_state.config, chat_id, pricing_text, Some(message_id))
          AgentState(..updated_state, total_messages: updated_state.total_messages + 1)
        }
        Some(#("quiz", _)) -> {
          vibe_logger.info(cmd_log |> vibe_logger.with_data("command", json.string("quiz")), "Command detected")
          let is_ru = is_cyrillic_text(text)
          let quiz_text = case is_ru {
            True -> "🎯 Quiz: Какой тариф вам подходит?\n\n1️⃣ Сколько генераций в месяц вам нужно?\n   A) До 100\n   B) 100-500\n   C) Больше 500\n\n2️⃣ Нужна ли интеграция с CRM?\n   A) Нет\n   B) Да\n\n3️⃣ Нужен ли API доступ?\n   A) Нет\n   B) Да\n\nОтветьте буквами, например: ABA\n\n💡 Или напишите 'помощь' для консультации"
            False -> "🎯 Quiz: Which plan fits you?\n\n1️⃣ How many generations per month do you need?\n   A) Up to 100\n   B) 100-500\n   C) More than 500\n\n2️⃣ Do you need CRM integration?\n   A) No\n   B) Yes\n\n3️⃣ Do you need API access?\n   A) No\n   B) Yes\n\nAnswer with letters, e.g.: ABA\n\n💡 Or type 'help' for consultation"
          }
          let _ = send_message(updated_state.config, chat_id, quiz_text, Some(message_id))
          AgentState(..updated_state, total_messages: updated_state.total_messages + 1)
        }
        Some(#("voice", prompt)) -> {
          vibe_logger.info(cmd_log |> vibe_logger.with_data("command", json.string("voice")) |> vibe_logger.with_data("prompt", json.string(prompt)), "Command detected")
          handle_voice_command(updated_state, chat_id, message_id, prompt)
        }
        Some(#("video", prompt)) -> {
          vibe_logger.info(cmd_log |> vibe_logger.with_data("command", json.string("video")) |> vibe_logger.with_data("prompt", json.string(prompt)), "Command detected")
          handle_video_command(updated_state, chat_id, message_id, prompt)
        }
        Some(#("talking", prompt)) -> {
          vibe_logger.info(cmd_log |> vibe_logger.with_data("command", json.string("talking")) |> vibe_logger.with_data("prompt", json.string(prompt)), "Command detected")
          handle_talking_command(updated_state, chat_id, message_id, prompt)
        }
        Some(#("morph", prompt)) -> {
          vibe_logger.info(cmd_log |> vibe_logger.with_data("command", json.string("morph")) |> vibe_logger.with_data("prompt", json.string(prompt)), "Command detected")
          handle_morph_command(updated_state, chat_id, message_id, prompt)
        }
        Some(#("i2v", prompt)) -> {
          vibe_logger.info(cmd_log |> vibe_logger.with_data("command", json.string("i2v")) |> vibe_logger.with_data("prompt", json.string(prompt)), "Command detected")
          handle_i2v_command(updated_state, chat_id, message_id, prompt)
        }
        Some(#("broll", prompt)) -> {
          vibe_logger.info(cmd_log |> vibe_logger.with_data("command", json.string("broll")) |> vibe_logger.with_data("prompt", json.string(prompt)), "Command detected")
          handle_broll_command(updated_state, chat_id, message_id, prompt)
        }
        Some(#("reels", prompt)) -> {
          vibe_logger.info(cmd_log |> vibe_logger.with_data("command", json.string("reels")) |> vibe_logger.with_data("prompt", json.string(prompt)), "NLP reels command detected")
          handle_reels_action(updated_state, chat_id, message_id, prompt, from_name, from_id)
        }
        _ -> {
          let sniper_log = vibe_logger.new("sniper")
            |> vibe_logger.with_session_id(updated_state.config.session_id)
            |> vibe_logger.with_data("chat_id", json.string(chat_id))

          // Проверяем, является ли это триггерным чатом (Sniper Mode)
          case trigger_chats.is_trigger_chat_active(chat_id) {
            True -> {
              // SNIPER MODE: отвечаем на триггеры ИЛИ проактивно
              vibe_logger.info(sniper_log |> vibe_logger.with_data("mode", json.string("sniper")), "Chat in SNIPER MODE")

              // 1. Сначала проверяем триггеры (быстрый путь)
              case trigger_chats.should_respond_to_trigger(chat_id, text) {
                True -> {
                  vibe_logger.info(sniper_log |> vibe_logger.with_data("trigger", json.bool(True)), "TRIGGER FOUND! Generating response")
                  process_with_digital_twin(updated_state, chat_id, message_id, text, from_name, username, phone, lang_code, is_premium, from_id)
                }
                False -> {
                  // 2. Нет триггера - проверяем проактивный режим
                  let current_time = get_current_timestamp()
                  case should_respond_proactively(chat_id, from_id, reply_to_id, text, current_time) {
                    True -> {
                      vibe_logger.info(sniper_log |> vibe_logger.with_data("proactive", json.bool(True)), "PROACTIVE MODE: Responding without trigger")
                      process_with_digital_twin(updated_state, chat_id, message_id, text, from_name, username, phone, lang_code, is_premium, from_id)
                    }
                    False -> {
                      vibe_logger.debug(sniper_log |> vibe_logger.with_data("trigger", json.bool(False)), "No trigger/proactive signal, staying silent")
                      AgentState(..updated_state, total_messages: updated_state.total_messages + 1)
                    }
                  }
                }
              }
            }
            False -> {
              // Обычный режим - Digital Twin или normal mode
              case updated_state.config.digital_twin_enabled {
                True -> {
                  // ElizaOS: Сначала проверяем Actions (NLP-based, без /commands)
                  case reels_plugin.should_handle_message(text) {
                    True -> {
                      // Reels Action matched! Обрабатываем через плагин
                      vibe_logger.info(vibe_logger.new("eliza")
                        |> vibe_logger.with_data("action", json.string("CREATE_REELS"))
                        |> vibe_logger.with_data("chat_id", json.string(chat_id)), "ElizaOS Action matched")
                      handle_reels_action(updated_state, chat_id, message_id, text, from_name, from_id)
                    }
                    False -> {
                      // Нет matching action - Digital Twin отвечает как обычно
                      vibe_logger.info(vibe_logger.new("twin") |> vibe_logger.with_data("chat_id", json.string(chat_id)), "Responding to message")
                      process_with_digital_twin(updated_state, chat_id, message_id, text, from_name, username, phone, lang_code, is_premium, from_id)
                    }
                  }
                }
                False -> {
                  // Обычный режим - проверяем target_chats и триггеры
                  handle_normal_mode(updated_state, chat_id, message_id, text)
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Проверяет, нужно ли отвечать проактивно (без триггера)
/// Возвращает True если:
/// 1. Сообщение - ответ на предыдущее сообщение агента
/// 2. Активный диалог (агент ответил в последние 5 минут)
/// 3. Упоминание агента (@vibee_agent, бот, агент)
pub fn should_respond_proactively(
  chat_id: String,
  from_id: Int,
  reply_to_id: Int,
  text: String,
  current_time: Int,
) -> Bool {
  let log = vibe_logger.new("proactive")
    |> vibe_logger.with_data("chat_id", json.string(chat_id))
    |> vibe_logger.with_data("from_id", json.int(from_id))
    |> vibe_logger.with_data("reply_to", json.int(reply_to_id))

  // 1. Проверяем reply на сообщение агента
  case conversation_tracker.is_reply_to_agent(chat_id, from_id, reply_to_id) {
    True -> {
      vibe_logger.info(log, "PROACTIVE: Reply to agent message detected")
      True
    }
    False -> {
      // 2. Проверяем активный диалог (5 минут)
      case conversation_tracker.is_active_conversation(chat_id, from_id, current_time) {
        True -> {
          vibe_logger.info(log, "PROACTIVE: Active conversation detected")
          True
        }
        False -> {
          // 3. Проверяем упоминание агента
          let lower_text = string.lowercase(text)
          let mentions_agent =
            string.contains(lower_text, "@vibee_agent") ||
            string.contains(lower_text, "вибе") ||
            string.contains(lower_text, "vibee") ||
            string.contains(lower_text, "бот") ||
            string.contains(lower_text, "агент")

          case mentions_agent {
            True -> {
              vibe_logger.info(log, "PROACTIVE: Agent mention detected")
              True
            }
            False -> {
              vibe_logger.debug(log, "No proactive trigger found")
              False
            }
          }
        }
      }
    }
  }
}

/// Get current Unix timestamp
@external(erlang, "vibee_ffi", "get_unix_timestamp")
fn get_current_timestamp() -> Int

/// Обработка группового сообщения (проверка триггеров)
fn handle_group_message(state: AgentState, chat_id: String, message_id: Int, text: String) -> AgentState {
  let log = vibe_logger.new("group")
    |> vibe_logger.with_session_id(state.config.session_id)
    |> vibe_logger.with_data("chat_id", json.string(chat_id))

  case target_chats.should_process_chat(chat_id) {
    False -> {
      vibe_logger.debug(log, "Skipping non-target group")
      state
    }
    True -> {
      case should_reply(state, text) {
        False -> {
          vibe_logger.debug(log |> vibe_logger.with_data("text", json.string(string.slice(text, 0, 30))), "No trigger in group")
          AgentState(..state, total_messages: state.total_messages + 1)
        }
        True -> {
          vibe_logger.info(log, "Trigger found in group! Generating reply")
          process_with_llm(state, chat_id, message_id, text)
        }
      }
    }
  }
}

/// Обработка в обычном режиме (без Digital Twin)
fn handle_normal_mode(state: AgentState, chat_id: String, message_id: Int, text: String) -> AgentState {
  let log = vibe_logger.new("normal")
    |> vibe_logger.with_session_id(state.config.session_id)
    |> vibe_logger.with_data("chat_id", json.string(chat_id))

  case target_chats.should_process_chat(chat_id) {
    False -> {
      case int.parse(chat_id) {
        Ok(n) if n > 0 -> {
          vibe_logger.debug(log, "Personal chat, processing without triggers")
          process_with_llm(state, chat_id, message_id, text)
        }
        _ -> {
          vibe_logger.debug(log, "Skipping non-target chat")
          state
        }
      }
    }
    True -> {
      vibe_logger.debug(log, "Processing target chat")
      case should_reply(state, text) {
        False -> {
          vibe_logger.debug(log |> vibe_logger.with_data("text", json.string(string.slice(text, 0, 30))), "No trigger found")
          AgentState(..state, total_messages: state.total_messages + 1)
        }
        True -> {
          vibe_logger.info(log |> vibe_logger.with_data("text", json.string(string.slice(text, 0, 30))), "Trigger found! Generating reply")
          process_with_llm(state, chat_id, message_id, text)
        }
      }
    }
  }
}

/// Digital Twin обработка - отвечает в стиле владельца аккаунта
fn process_with_digital_twin(state: AgentState, chat_id: String, message_id: Int, text: String, from_name: String, username: String, phone: String, lang_code: String, is_premium: Bool, from_id: Int) -> AgentState {
  let log = vibe_logger.new("twin")
    |> vibe_logger.with_session_id(state.config.session_id)
    |> vibe_logger.with_data("chat_id", json.string(chat_id))
    |> vibe_logger.with_data("from", json.string(from_name))
    |> vibe_logger.with_data("username", json.string(username))
    |> vibe_logger.with_data("from_id", json.int(from_id))

  vibe_logger.info(log, "Processing message")

  // Initialize conversation tracker
  conversation_tracker.init()

  // Проверяем триггерные слова для этого чата
  let has_trigger = trigger_chats.should_respond_to_trigger(chat_id, text)
  let trigger_log = vibe_logger.new("trigger")
    |> vibe_logger.with_data("chat_id", json.string(chat_id))
    |> vibe_logger.with_data("has_trigger", json.bool(has_trigger))
    |> vibe_logger.with_data("text", json.string(string.slice(text, 0, 40)))
  vibe_logger.info(trigger_log, "Trigger check")

  case has_trigger {
    True -> {
      vibe_logger.info(trigger_log |> vibe_logger.with_data("matched", json.bool(True)), "TRIGGER MATCHED")
      vibe_logger.info(log |> vibe_logger.with_data("trigger", json.bool(True)), "Trigger word found")

      // Генерируем ответ с учетом шаблона для триггерного чата
      case generate_trigger_reply(state.config, text, from_name, chat_id) {
        Ok(reply) -> {
          vibe_logger.info(log |> vibe_logger.with_data("reply", json.string(string.slice(reply, 0, 80))), "Reply generated")

          // Отправляем ответ
          case send_message(state.config, chat_id, reply, Some(message_id)) {
            Ok(msg_id) -> {
              vibe_logger.info(log |> vibe_logger.with_data("msg_id", json.int(msg_id)), "Message sent")

              // Сохраняем ответ в БД
              let dialog_id = case int.parse(chat_id) {
                Ok(id) -> id
                Error(_) -> 0
              }
              let db_log = vibe_logger.new("db")
                |> vibe_logger.with_data("dialog_id", json.int(dialog_id))
                |> vibe_logger.with_data("msg_id", json.int(msg_id))
              case postgres.insert_message_simple(dialog_id, msg_id, state.config.owner_id, "Я", reply) {
                Ok(_) -> vibe_logger.debug(db_log, "Reply saved")
                Error(e) -> vibe_logger.error(db_log |> vibe_logger.with_data("error", json.string(e)), "Failed to save reply")
              }

              // Трекаем ответ агента для проактивного режима
              let current_time = get_current_timestamp()
              conversation_tracker.agent_responded(chat_id, from_id, msg_id, current_time)
              vibe_logger.debug(log, "Conversation tracked for proactive mode")

              // Пересылаем диалог в целевую группу
              let fwd_init_log = vibe_logger.new("forward_trigger")
                |> vibe_logger.with_data("chat_id", json.string(chat_id))
              vibe_logger.info(fwd_init_log, "Looking for forward config")
              case trigger_chats.find_chat_config(chat_id) {
                Ok(chat_config) -> {
                  let forward_chat_id = chat_config.forward_chat_id
                  let chat_name = chat_config.chat_name
                  let fwd_log = vibe_logger.new("forward")
                    |> vibe_logger.with_data("target", json.string(forward_chat_id))
                    |> vibe_logger.with_data("chat_name", json.string(chat_name))
                  vibe_logger.info(fwd_log, "Found config, initiating forward")

                  let original_msg = dialog_forwarder.MessageInfo(
                    chat_id: chat_id,
                    chat_name: chat_name,
                    message_id: message_id,
                    from_id: from_id,
                    from_name: from_name,
                    username: username,
                    phone: phone,
                    lang_code: lang_code,
                    is_premium: is_premium,
                    text: text,
                    timestamp: 0,
                  )

                  let agent_msg = dialog_forwarder.MessageInfo(
                    chat_id: chat_id,
                    chat_name: chat_name,
                    message_id: msg_id,
                    from_id: state.config.owner_id,
                    from_name: "Agent",
                    username: "",
                    phone: "",
                    lang_code: "",
                    is_premium: False,
                    text: reply,
                    timestamp: 0,
                  )

                  // Собираем контекст из истории сообщений
                  let context = collect_dialog_context(state.config, chat_id, from_id, message_id)

                  case dialog_forwarder.forward_dialog_with_context(
                    state.config.session_id,
                    original_msg,
                    agent_msg,
                    forward_chat_id,
                    context,
                  ) {
                    dialog_forwarder.ForwardSuccess(fwd_id) -> {
                      vibe_logger.info(fwd_log |> vibe_logger.with_data("fwd_msg_id", json.int(fwd_id)), "Dialog forwarded")
                    }
                    dialog_forwarder.ForwardError(reason) -> {
                      vibe_logger.error(fwd_log |> vibe_logger.with_data("error", json.string(reason)), "Forward failed")
                    }
                  }
                }
                Error(_) -> {
                  vibe_logger.debug(log, "No forward target configured")
                }
              }

              AgentState(..state, total_messages: state.total_messages + 1)
            }
            Error(send_err) -> {
              vibe_logger.error(log |> vibe_logger.with_data("error", json.string(send_err)), "Send failed")
              AgentState(..state, total_messages: state.total_messages + 1)
            }
          }
        }
        Error(err) -> {
          vibe_logger.error(log |> vibe_logger.with_data("error", json.string(err)), "Generate failed")
          state
        }
      }
    }
    False -> {
      // Нет триггера - обычная обработка Digital Twin
      case generate_digital_twin_reply(state.config, text, from_name, chat_id) {
        Ok(reply) -> {
          vibe_logger.info(log |> vibe_logger.with_data("reply", json.string(string.slice(reply, 0, 80))), "Reply generated")
          case send_message(state.config, chat_id, reply, Some(message_id)) {
            Ok(msg_id) -> {
              vibe_logger.info(log |> vibe_logger.with_data("msg_id", json.int(msg_id)), "Message sent")
              let dialog_id = case int.parse(chat_id) {
                Ok(id) -> id
                Error(_) -> 0
              }
              let db_log = vibe_logger.new("db")
                |> vibe_logger.with_data("dialog_id", json.int(dialog_id))
                |> vibe_logger.with_data("msg_id", json.int(msg_id))
              case postgres.insert_message_simple(dialog_id, msg_id, state.config.owner_id, "Я", reply) {
                Ok(_) -> vibe_logger.debug(db_log, "Reply saved")
                Error(e) -> vibe_logger.error(db_log |> vibe_logger.with_data("error", json.string(e)), "Failed to save reply")
              }
              AgentState(..state, total_messages: state.total_messages + 1)
            }
            Error(send_err) -> {
              vibe_logger.error(log |> vibe_logger.with_data("error", json.string(send_err)), "Send failed")
              AgentState(..state, total_messages: state.total_messages + 1)
            }
          }
        }
        Error(err) -> {
          vibe_logger.error(log |> vibe_logger.with_data("error", json.string(err)), "Generate failed")
          state
        }
      }
    }
  }
}

// ============================================================
// ElizaOS Actions Handler
// ============================================================

/// Handle reels action through ElizaOS plugin
fn handle_reels_action(
  state: AgentState,
  chat_id: String,
  message_id: Int,
  text: String,
  from_name: String,
  from_id: Int,
) -> AgentState {
  let log = vibe_logger.new("reels_action")
    |> vibe_logger.with_data("chat_id", json.string(chat_id))
    |> vibe_logger.with_data("from_id", json.int(from_id))

  vibe_logger.info(log, "Starting reels action")

  // Send "generating" message
  let _ = send_message(state.config, chat_id,
    "🎬 Создаю рилс...\n\n" <>
    "Этапы:\n" <>
    "1. Генерация скрипта\n" <>
    "2. Озвучка (TTS)\n" <>
    "3. Lipsync аватар\n" <>
    "4. B-roll видео\n" <>
    "5. Финальный рендер\n\n" <>
    "⏱ Это займёт 2-5 минут...",
    Some(message_id))

  // Build action context
  let context = ActionContext(
    user_id: from_id,
    chat_id: chat_id,
    message: text,
    history: [],  // TODO: Get recent history from DB
    user_name: Some(from_name),
    photo_url: None,  // TODO: Get user photo from Telegram
    previous_results: [],
  )

  // Initialize plugin and process message
  let registry = reels_plugin.init()

  case reels_plugin.process_message(registry, context) {
    Ok(result) -> {
      case result.success {
        True -> {
          // Get video URL from result
          let video_url = case list.find(
            result.data |> dict.to_list(),
            fn(pair) { pair.0 == "video_url" }
          ) {
            Ok(#(_, url)) -> url
            Error(_) -> ""
          }

          case video_url {
            "" -> {
              let _ = send_message(state.config, chat_id,
                "✅ " <> result.text,
                Some(message_id))
              AgentState(..state, total_messages: state.total_messages + 1)
            }
            url -> {
              // Send video
              let _ = send_message(state.config, chat_id,
                "✅ Рилс готов!\n\n🎬 " <> url,
                Some(message_id))
              AgentState(..state, total_messages: state.total_messages + 1)
            }
          }
        }
        False -> {
          vibe_logger.error(log |> vibe_logger.with_data("error", json.string(result.text)), "Reels action failed")
          let _ = send_message(state.config, chat_id,
            "❌ Ошибка: " <> result.text,
            Some(message_id))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
      }
    }
    Error(err) -> {
      vibe_logger.error(log |> vibe_logger.with_data("error", json.string(err)), "Reels plugin error")
      let _ = send_message(state.config, chat_id,
        "❌ Ошибка плагина: " <> err,
        Some(message_id))
      AgentState(..state, total_messages: state.total_messages + 1)
    }
  }
}

// ============================================================
// Context Collection for Forwarding
// ============================================================

/// Собирает контекст диалога из истории сообщений
fn collect_dialog_context(
  config: TelegramAgentConfig,
  chat_id: String,
  lead_user_id: Int,
  trigger_message_id: Int,
) -> dialog_forwarder.DialogContext {
  // Создаём bridge для получения истории
  let bridge = tg_client.TelegramBridge(
    base_url: config.bridge_url,
    session_id: Some(config.session_id),
    api_key: Some(get_api_key()),
  )

  // Парсим chat_id
  let chat_id_int = case int.parse(chat_id) {
    Ok(id) -> id
    Error(_) -> 0
  }

  // Получаем последние 10 сообщений
  case tg_client.get_history(bridge, chat_id_int, 10) {
    Ok(messages) -> {
      // Фильтруем: берём только сообщения ДО триггерного сообщения
      // и ограничиваем 5 сообщениями для контекста
      let context_messages = messages
        |> list.filter(fn(msg: tg_types.TelegramMessage) { msg.id < trigger_message_id })
        |> list.take(5)
        |> list.reverse  // Старые сначала
        |> list.map(fn(msg: tg_types.TelegramMessage) {
          dialog_forwarder.ContextMessage(
            from_name: msg.from_name,
            text: msg.text,
            is_from_user: msg.from_id == lead_user_id,
          )
        })

      dialog_forwarder.DialogContext(messages: context_messages)
    }
    Error(_) -> {
      // Если не удалось получить историю - возвращаем пустой контекст
      io.println("[CONTEXT] Failed to get history, using empty context")
      dialog_forwarder.empty_context()
    }
  }
}

// ============================================================
// Command Parsing and Handlers
// ============================================================

/// Определяет язык: True = русский (по умолчанию), False = английский (только если явно латиница)
fn is_cyrillic_text(text: String) -> Bool {
  // Убираем команду из текста
  let clean_text = case string.split(text, " ") {
    [_cmd, ..rest] -> string.join(rest, " ")
    _ -> text
  }

  // Если текст пустой или только команда — русский по умолчанию
  case string.trim(clean_text) {
    "" -> True  // По умолчанию русский
    remaining -> {
      // Проверяем наличие латинских букв (не в команде)
      let latin_chars = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]
      let cyrillic_chars = ["а", "б", "в", "г", "д", "е", "ж", "з", "и", "й", "к", "л", "м", "н", "о", "п", "р", "с", "т", "у", "ф", "х", "ц", "ч", "ш", "щ", "ы", "э", "ю", "я"]
      let lower_remaining = string.lowercase(remaining)

      let has_cyrillic = list.any(cyrillic_chars, fn(char) { string.contains(lower_remaining, char) })
      let has_latin = list.any(latin_chars, fn(char) { string.contains(lower_remaining, char) })

      case has_cyrillic, has_latin {
        True, _ -> True      // Есть кириллица → русский
        False, True -> False // Только латиница → английский
        False, False -> True // Ни того ни другого → русский по умолчанию
      }
    }
  }
}

/// Парсит команду из текста сообщения
/// Возвращает Some(#(command, args)) или None
fn parse_command(text: String) -> Option(#(String, String)) {
  let trimmed = string.trim(text)
  // First check for / commands
  case string.starts_with(trimmed, "/") {
    True -> {
      let without_slash = string.drop_start(trimmed, 1)
      case string.split(without_slash, " ") {
        [] -> None
        [cmd] -> Some(#(string.lowercase(cmd), ""))
        [cmd, ..rest] -> Some(#(string.lowercase(cmd), string.join(rest, " ")))
      }
    }
    False -> {
      // NLP routing: detect natural language commands
      detect_nlp_command(trimmed)
    }
  }
}

/// Detect natural language commands (NLP routing)
/// Maps natural language phrases to command equivalents
fn detect_nlp_command(text: String) -> Option(#(String, String)) {
  let lower = string.lowercase(text)

  // Help / capabilities patterns
  let help_patterns = [
    "что ты умеешь", "что умеешь", "твои возможности", "помощь",
    "как работаешь", "что можешь", "покажи команды", "список команд",
    "what can you do", "help me", "your capabilities",
  ]

  // Pricing patterns
  let pricing_patterns = [
    "покажи тарифы", "тарифы", "цены", "прайс", "сколько стоит",
    "стоимость", "pricing", "prices", "how much", "cost",
  ]

  // Video creation patterns
  let video_patterns = [
    "хочу создать видео", "создай видео", "сделай видео", "генерируй видео",
    "видео из", "make video", "create video", "generate video",
  ]

  // Image generation patterns
  let neuro_patterns = [
    "сгенерируй фото", "создай фото", "нарисуй", "сгенерируй картинку",
    "создай картинку", "сделай фото", "generate image", "create image",
    "generate photo", "draw",
  ]

  // Reels patterns (already handled by reels_plugin, but add fallback)
  let reels_patterns = [
    "создай рилс", "сделай рилс", "рилс про", "хочу рилс",
    "make reel", "create reel",
  ]

  // Voice patterns
  let voice_patterns = [
    "озвучь", "голос", "voice", "tts", "text to speech",
  ]

  // Check each pattern group
  case list.any(help_patterns, fn(p) { string.contains(lower, p) }) {
    True -> Some(#("help", ""))
    False ->
      case list.any(pricing_patterns, fn(p) { string.contains(lower, p) }) {
        True -> Some(#("pricing", ""))
        False ->
          case list.any(video_patterns, fn(p) { string.contains(lower, p) }) {
            True -> {
              // Extract prompt after the pattern
              let prompt = extract_prompt_after_patterns(lower, video_patterns)
              Some(#("video", prompt))
            }
            False ->
              case list.any(neuro_patterns, fn(p) { string.contains(lower, p) }) {
                True -> {
                  let prompt = extract_prompt_after_patterns(lower, neuro_patterns)
                  Some(#("neuro", prompt))
                }
                False ->
                  case list.any(reels_patterns, fn(p) { string.contains(lower, p) }) {
                    True -> {
                      let prompt = extract_prompt_after_patterns(lower, reels_patterns)
                      Some(#("reels", prompt))
                    }
                    False ->
                      case list.any(voice_patterns, fn(p) { string.contains(lower, p) }) {
                        True -> {
                          let prompt = extract_prompt_after_patterns(lower, voice_patterns)
                          Some(#("voice", prompt))
                        }
                        False -> None
                      }
                  }
              }
          }
      }
  }
}

/// Extract prompt text after matching pattern
fn extract_prompt_after_patterns(text: String, patterns: List(String)) -> String {
  // Find which pattern matched and extract text after it
  let matched = list.find(patterns, fn(p) { string.contains(text, p) })
  case matched {
    Ok(pattern) -> {
      case string.split_once(text, pattern) {
        Ok(#(_, after)) -> string.trim(after)
        Error(_) -> ""
      }
    }
    Error(_) -> ""
  }
}

/// Обрабатывает команду /neurophoto с FAL.ai
fn handle_neurophoto_command(
  state: AgentState,
  chat_id: String,
  message_id: Int,
  prompt: String,
) -> AgentState {
  case prompt {
    "" -> {
      // Пустой промпт - показываем подсказку
      let hint = "Ukazhite prompt dlya generacii!\n\nPrimer: /neurophoto cyberpunk portrait, neon lights, cinematic"
      let _ = send_message(state.config, chat_id, hint, Some(message_id))
      AgentState(..state, total_messages: state.total_messages + 1)
    }
    _ -> {
      // Отправляем сообщение "генерируем..."
      let _ = send_message(state.config, chat_id, "🎨 Генерирую изображение NEURO_SAGE...\n\nПромпт: " <> prompt, Some(message_id))

      let neuro_log = vibe_logger.new("neurophoto")
        |> vibe_logger.with_data("chat_id", json.string(chat_id))
        |> vibe_logger.with_data("prompt", json.string(prompt))

      // Вызываем FAL.ai
      case generate_image_fal(prompt) {
        Ok(image_url) -> {
          vibe_logger.info(neuro_log |> vibe_logger.with_data("url", json.string(image_url)), "Image generated")
          // Отправляем изображение
          let _ = send_photo(state.config, chat_id, image_url, Some("Generated: " <> prompt))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
        Error(err) -> {
          vibe_logger.error(neuro_log |> vibe_logger.with_data("error", json.string(err)), "Generation failed")
          let _ = send_message(state.config, chat_id, "❌ Ошибка генерации: " <> err, Some(message_id))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
      }
    }
  }
}

/// Генерация изображения через FAL.ai FLUX LoRA
fn generate_image_fal(prompt: String) -> Result(String, String) {
  // Получаем API ключ из окружения
  let api_key = get_env("FAL_API_KEY")
  case api_key {
    "" -> Error("FAL_API_KEY not configured")
    key -> {
      // Получаем LoRA URL из окружения (или дефолт)
      let lora_url = case get_env("DEFAULT_LORA_URL") {
        "" -> "https://v3b.fal.media/files/b/elephant/YpfnIK7JlNO7vZTsGanfo_pytorch_lora_weights.safetensors"
        url -> url
      }

      // Добавляем триггер NEURO_SAGE
      let full_prompt = "NEURO_SAGE " <> prompt

      // Формируем JSON тело запроса
      let body = json.object([
        #("prompt", json.string(full_prompt)),
        #("loras", json.array([
          json.object([
            #("path", json.string(lora_url)),
            #("scale", json.float(1.0)),
          ]),
        ], fn(x) { x })),
        #("num_images", json.int(1)),
        #("image_size", json.object([
          #("width", json.int(768)),
          #("height", json.int(1365)),
        ])),
        #("enable_safety_checker", json.bool(True)),
        #("output_format", json.string("jpeg")),
        // НЕ используем sync_mode - всегда async с polling для надёжности
        #("guidance_scale", json.float(3.5)),
        #("num_inference_steps", json.int(28)),
      ])
      |> json.to_string()

      let fal_log = vibe_logger.new("fal")
        |> vibe_logger.with_data("prompt", json.string(string.slice(full_prompt, 0, 50)))
      vibe_logger.info(fal_log, "Calling FAL.ai")

      let req = request.new()
        |> request.set_scheme(http.Https)
        |> request.set_method(http.Post)
        |> request.set_host("queue.fal.run")
        |> request.set_path("/fal-ai/flux-lora")
        |> request.set_header("Authorization", "Key " <> key)
        |> request.set_header("Content-Type", "application/json")
        |> request.set_body(body)

      case httpc.send(req) {
        Ok(response) -> {
          vibe_logger.debug(fal_log |> vibe_logger.with_data("status", json.int(response.status)), "Response received")
          case response.status {
            200 -> {
              // Парсим ответ и извлекаем URL изображения
              extract_image_url(response.body, key)
            }
            _ -> Error("FAL.ai HTTP " <> int.to_string(response.status) <> ": " <> string.slice(response.body, 0, 100))
          }
        }
        Error(_) -> Error("HTTP request to FAL.ai failed")
      }
    }
  }
}

/// Извлекает URL изображения из ответа FAL.ai
/// Async режим: сначала получаем request_id, потом делаем polling
fn extract_image_url(body: String, api_key: String) -> Result(String, String) {
  // Сначала проверяем - может изображение уже готово (sync успех)
  let pattern = "\"url\":\""
  case string.split(body, pattern) {
    [_, rest, ..] -> {
      case string.split(rest, "\"") {
        [url, ..] -> Ok(url)
        _ -> Error("Не удалось распарсить URL изображения")
      }
    }
    _ -> {
      // Async режим: FAL.ai вернул request_id, нужен polling
      case extract_request_id(body) {
        Ok(request_id) -> {
          vibe_logger.info(vibe_logger.new("fal") |> vibe_logger.with_data("request_id", json.string(request_id)), "Запрос в очереди, запускаю polling")
          poll_fal_result(request_id, api_key, 60)  // 60 попыток = ~3 мин
        }
        Error(_) -> {
          // Проверяем на ошибку
          case string.contains(body, "error") || string.contains(body, "Error") {
            True -> Error("Ошибка FAL.ai: " <> string.slice(body, 0, 200))
            False -> Error("Неожиданный ответ FAL.ai: " <> string.slice(body, 0, 200))
          }
        }
      }
    }
  }
}

/// Извлекает request_id из ответа FAL.ai
fn extract_request_id(body: String) -> Result(String, String) {
  // Формат может быть: {"request_id":"abc123"} или {"request_id": "abc123"}
  // Пробуем оба варианта
  let pattern1 = "\"request_id\":\""
  let pattern2 = "\"request_id\": \""

  case string.split(body, pattern1) {
    [_, rest, ..] -> {
      case string.split(rest, "\"") {
        [request_id, ..] -> Ok(request_id)
        _ -> Error("Could not parse request_id (pattern1)")
      }
    }
    _ -> {
      // Пробуем второй паттерн (с пробелом)
      case string.split(body, pattern2) {
        [_, rest, ..] -> {
          case string.split(rest, "\"") {
            [request_id, ..] -> Ok(request_id)
            _ -> Error("Could not parse request_id (pattern2)")
          }
        }
        _ -> Error("No request_id in response: " <> string.slice(body, 0, 100))
      }
    }
  }
}

/// Polling для получения результата из FAL.ai очереди
fn poll_fal_result(request_id: String, api_key: String, max_attempts: Int) -> Result(String, String) {
  poll_fal_loop(request_id, api_key, max_attempts, 1)
}

fn poll_fal_loop(request_id: String, api_key: String, max_attempts: Int, attempt: Int) -> Result(String, String) {
  case attempt > max_attempts {
    True -> Error("Таймаут ожидания: " <> int.to_string(max_attempts) <> " попыток (3 мин)")
    False -> {
      // Ждём 3 секунды между попытками
      sleep_ms(3000)

      // Проверяем статус
      let status_req = request.new()
        |> request.set_scheme(http.Https)
        |> request.set_method(http.Get)
        |> request.set_host("queue.fal.run")
        |> request.set_path("/requests/" <> request_id <> "/status")
        |> request.set_header("Authorization", "Key " <> api_key)
        |> request.set_header("Content-Type", "application/json")

      let poll_log = vibe_logger.new("fal_poll")
        |> vibe_logger.with_data("request_id", json.string(request_id))
        |> vibe_logger.with_data("attempt", json.int(attempt))

      case httpc.send(status_req) {
        Ok(status_response) -> {
          vibe_logger.debug(poll_log |> vibe_logger.with_data("body", json.string(string.slice(status_response.body, 0, 100))), "Poll response")

          case string.contains(status_response.body, "COMPLETED") {
            True -> {
              // Получаем результат
              vibe_logger.info(poll_log, "Request COMPLETED, fetching result")
              fetch_fal_result(request_id, api_key)
            }
            False -> {
              case string.contains(status_response.body, "FAILED") {
                True -> Error("FAL.ai request failed")
                False -> {
                  // Продолжаем polling
                  poll_fal_loop(request_id, api_key, max_attempts, attempt + 1)
                }
              }
            }
          }
        }
        Error(_) -> {
          vibe_logger.warn(poll_log, "Poll request failed, retrying")
          poll_fal_loop(request_id, api_key, max_attempts, attempt + 1)
        }
      }
    }
  }
}

/// Получает результат из FAL.ai очереди
fn fetch_fal_result(request_id: String, api_key: String) -> Result(String, String) {
  let result_req = request.new()
    |> request.set_scheme(http.Https)
    |> request.set_method(http.Get)
    |> request.set_host("queue.fal.run")
    |> request.set_path("/requests/" <> request_id)
    |> request.set_header("Authorization", "Key " <> api_key)
    |> request.set_header("Content-Type", "application/json")

  case httpc.send(result_req) {
    Ok(result_response) -> {
      vibe_logger.debug(vibe_logger.new("fal") |> vibe_logger.with_data("body", json.string(string.slice(result_response.body, 0, 200))), "Result response")
      extract_image_url_simple(result_response.body)
    }
    Error(_) -> Error("Failed to fetch FAL.ai result")
  }
}

/// Простое извлечение URL без рекурсии
fn extract_image_url_simple(body: String) -> Result(String, String) {
  let pattern = "\"url\":\""
  case string.split(body, pattern) {
    [_, rest, ..] -> {
      case string.split(rest, "\"") {
        [url, ..] -> Ok(url)
        _ -> Error("Could not parse image URL from result")
      }
    }
    _ -> Error("No image URL in result: " <> string.slice(body, 0, 200))
  }
}

// ============================================================
// AI Command Handlers
// ============================================================

/// Обрабатывает команду /voice - голосовой клон (ElevenLabs)
fn handle_voice_command(
  state: AgentState,
  chat_id: String,
  message_id: Int,
  prompt: String,
) -> AgentState {
  case prompt {
    "" -> {
      let hint = "🎤 /voice <текст> - синтез речи\n\nПример: /voice Привет, это мой голосовой клон!"
      let _ = send_message(state.config, chat_id, hint, Some(message_id))
      AgentState(..state, total_messages: state.total_messages + 1)
    }
    _ -> {
      let api_key = get_env("ELEVENLABS_API_KEY")
      case api_key {
        "" -> {
          let _ = send_message(state.config, chat_id, "❌ ELEVENLABS_API_KEY не настроен. Голосовой синтез недоступен.", Some(message_id))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
        _ -> {
          let _ = send_message(state.config, chat_id, "🎤 Синтезирую голос...\n\nТекст: " <> string.slice(prompt, 0, 50) <> "...", Some(message_id))
          // TODO: Implement ElevenLabs TTS
          let _ = send_message(state.config, chat_id, "✅ Голосовой синтез в разработке. API ключ настроен!", Some(message_id))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
      }
    }
  }
}

/// Обрабатывает команду /video - генерация видео (Kling)
fn handle_video_command(
  state: AgentState,
  chat_id: String,
  message_id: Int,
  prompt: String,
) -> AgentState {
  case prompt {
    "" -> {
      // SMART ROUTING: Use Bot API to send provider selection with inline buttons
      let log = vibe_logger.new("SmartRoute")
        |> vibe_logger.with_data("chat_id", json.string(chat_id))
        |> vibe_logger.with_data("command", json.string("/video"))

      vibe_logger.info(log, "🤖 /video command - sending buttons via Bot API")

      let text = "🎬 Выбери провайдер для видео:\n\n" <>
        "**Kling AI** - Высокое качество, стабильное движение\n" <>
        "**Veo3** - Google, реалистичное движение"

      let keyboard = [
        [bot_api.InlineButton("Kling AI", "t2v:kling")],
        [bot_api.InlineButton("Veo3 (KIE.ai)", "t2v:veo3")],
        [bot_api.InlineButton("Отмена", "cancel")],
      ]

      // Parse chat_id to int for Bot API
      let chat_id_int = case int.parse(chat_id) {
        Ok(id) -> id
        Error(_) -> 0
      }

      // Get Bot API config
      let bridge_url = telegram_config.bridge_url()
      let api_key = telegram_config.bridge_api_key()
      vibe_logger.info(log
        |> vibe_logger.with_data("bridge_url", json.string(bridge_url))
        |> vibe_logger.with_data("api_key_prefix", json.string(string.slice(api_key, 0, 10)))
        |> vibe_logger.with_data("chat_id_int", json.int(chat_id_int)),
        "Bot API config")
      let bot_config = bot_api.with_key(bridge_url, api_key)

      case bot_api.send_with_buttons(bot_config, chat_id_int, text, keyboard) {
        Ok(_) -> {
          vibe_logger.info(log, "✅ Bot API: Buttons sent successfully!")
          AgentState(..state, total_messages: state.total_messages + 1)
        }
        Error(err) -> {
          vibe_logger.error(log
            |> vibe_logger.with_data("error", json.string(bot_api_error_to_string(err))),
            "⚠️ Bot API failed, falling back to MTProto")
          // Fallback to plain text via MTProto
          let hint = "🎬 /video <описание> - генерация видео\n\nПример: /video закат над океаном, волны, 4K cinematic"
          let _ = send_message(state.config, chat_id, hint, Some(message_id))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
      }
    }
    _ -> {
      let access_key = get_env("KLING_ACCESS_KEY")
      case access_key {
        "" -> {
          let _ = send_message(state.config, chat_id, "❌ KLING_ACCESS_KEY не настроен. Генерация видео недоступна.\n\n💡 Для видео нужен Kling AI API ключ: https://klingai.com/", Some(message_id))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
        _ -> {
          let _ = send_message(state.config, chat_id, "🎬 Генерирую видео...\n\nPrompt: " <> string.slice(prompt, 0, 50) <> "...", Some(message_id))
          // TODO: Implement Kling video generation
          let _ = send_message(state.config, chat_id, "✅ Видео генерация в разработке. Kling API настроен!", Some(message_id))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
      }
    }
  }
}

/// Обрабатывает команду /talking - говорящий аватар (Hedra)
fn handle_talking_command(
  state: AgentState,
  chat_id: String,
  message_id: Int,
  prompt: String,
) -> AgentState {
  case prompt {
    "" -> {
      let hint = "🗣️ /talking <текст> - говорящий аватар\n\nПример: /talking Привет! Я цифровой аватар."
      let _ = send_message(state.config, chat_id, hint, Some(message_id))
      AgentState(..state, total_messages: state.total_messages + 1)
    }
    _ -> {
      let api_key = get_env("HEDRA_API_KEY")
      case api_key {
        "" | "<hedra_key>" -> {
          let _ = send_message(state.config, chat_id, "❌ HEDRA_API_KEY не настроен. Говорящий аватар недоступен.\n\n💡 Для аватара нужен Hedra API ключ: https://www.hedra.com/", Some(message_id))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
        _ -> {
          let _ = send_message(state.config, chat_id, "🗣️ Создаю говорящий аватар...\n\nТекст: " <> string.slice(prompt, 0, 50) <> "...", Some(message_id))
          // TODO: Implement Hedra avatar
          let _ = send_message(state.config, chat_id, "✅ Hedra avatar в разработке. API настроен!", Some(message_id))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
      }
    }
  }
}

/// Обрабатывает команду /morph - морфинг изображений (Kling)
fn handle_morph_command(
  state: AgentState,
  chat_id: String,
  message_id: Int,
  _prompt: String,
) -> AgentState {
  let api_key = get_env("KLING_ACCESS_KEY")
  case api_key {
    "" -> {
      let _ = send_message(state.config, chat_id, "❌ KLING_ACCESS_KEY не настроен.\n\n🔄 /morph - отправьте 2 изображения для морфинга\n\n💡 Нужен Kling API: https://klingai.com/", Some(message_id))
      AgentState(..state, total_messages: state.total_messages + 1)
    }
    _ -> {
      let _ = send_message(state.config, chat_id, "🔄 /morph - отправьте 2 изображения для морфинга\n\n✅ Kling API настроен!", Some(message_id))
      AgentState(..state, total_messages: state.total_messages + 1)
    }
  }
}

/// Обрабатывает команду /i2v - image to video (Kling)
fn handle_i2v_command(
  state: AgentState,
  chat_id: String,
  message_id: Int,
  _prompt: String,
) -> AgentState {
  let api_key = get_env("KLING_ACCESS_KEY")
  case api_key {
    "" -> {
      let _ = send_message(state.config, chat_id, "❌ KLING_ACCESS_KEY не настроен.\n\n🎥 /i2v - отправьте изображение для анимации\n\n💡 Нужен Kling API: https://klingai.com/", Some(message_id))
      AgentState(..state, total_messages: state.total_messages + 1)
    }
    _ -> {
      let _ = send_message(state.config, chat_id, "🎥 /i2v - отправьте изображение для анимации в видео\n\n✅ Kling API настроен!", Some(message_id))
      AgentState(..state, total_messages: state.total_messages + 1)
    }
  }
}

/// Обрабатывает команду /broll - B-Roll генерация
fn handle_broll_command(
  state: AgentState,
  chat_id: String,
  message_id: Int,
  prompt: String,
) -> AgentState {
  case prompt {
    "" -> {
      let hint = "📹 /broll <тема> - генерация B-Roll видео\n\nПример: /broll бизнес встреча, офис, 4K"
      let _ = send_message(state.config, chat_id, hint, Some(message_id))
      AgentState(..state, total_messages: state.total_messages + 1)
    }
    _ -> {
      let _ = send_message(state.config, chat_id, "📹 Генерирую B-Roll...\n\nТема: " <> prompt, Some(message_id))
      // TODO: Implement B-Roll generation
      let _ = send_message(state.config, chat_id, "✅ B-Roll генерация в разработке!", Some(message_id))
      AgentState(..state, total_messages: state.total_messages + 1)
    }
  }
}

/// Erlang sleep wrapper
@external(erlang, "timer", "sleep")
fn sleep_ms(ms: Int) -> Nil

/// Получает переменную окружения
/// Использует FFI wrapper для конвертации binary -> charlist -> os:getenv -> binary
@external(erlang, "vibee_ffi", "get_env")
fn get_env(name: String) -> String

/// Обработка через LLM
fn process_with_llm(state: AgentState, chat_id: String, message_id: Int, text: String) -> AgentState {
  let llm_log = vibe_logger.new("llm")
    |> vibe_logger.with_session_id(state.config.session_id)
    |> vibe_logger.with_data("chat_id", json.string(chat_id))

  case generate_reply(state.config, text) {
    Ok(reply) -> {
      vibe_logger.info(llm_log |> vibe_logger.with_data("reply", json.string(string.slice(reply, 0, 50))), "Reply generated")
      let _ = send_message(state.config, chat_id, reply, Some(message_id))
      AgentState(..state, total_messages: state.total_messages + 1)
    }
    Error(err) -> {
      vibe_logger.error(llm_log |> vibe_logger.with_data("error", json.string(err)), "Failed to generate reply")
      AgentState(..state, total_messages: state.total_messages + 1)
    }
  }
}

/// Проверяет, нужно ли отвечать на сообщение
fn should_reply(state: AgentState, text: String) -> Bool {
  case state.config.auto_reply_enabled {
    False -> False
    True -> {
      let lower_text = string.lowercase(text)
      // Проверяем триггеры
      let triggers = ["vibee", "vibe", "@vibee", "бот", "агент", "вайб"]
      list.any(triggers, fn(trigger) {
        string.contains(lower_text, trigger)
      })
    }
  }
}

/// Генерация ответа через LLM (OpenRouter)
fn generate_reply(config: TelegramAgentConfig, user_message: String) -> Result(String, String) {
  case config.llm_api_key {
    None -> {
      // Без LLM - простой fallback ответ
      Ok("Privet! Ya VIBEE agent na Gleam/BEAM. Polnyj LLM otvet budet dostupem posle nastrojki OPENROUTER_API_KEY.")
    }
    Some(api_key) -> {
      // Вызов OpenRouter API
      call_openrouter(api_key, config.llm_model, user_message)
    }
  }
}

/// Генерация ответа Digital Twin - в стиле владельца аккаунта
/// Использует улучшенный промпт с few-shot примерами и RAG контекстом
fn generate_digital_twin_reply(
  config: TelegramAgentConfig,
  user_message: String,
  from_name: String,
  chat_id: String,
) -> Result(String, String) {
  let twin_log = vibe_logger.new("twin_reply")
    |> vibe_logger.with_session_id(config.session_id)
    |> vibe_logger.with_data("chat_id", json.string(chat_id))
    |> vibe_logger.with_data("from", json.string(from_name))

  vibe_logger.debug(twin_log, "Generating reply")

  // Получаем контекст из истории (RAG) - TODO: интегрировать с conversation_get_context
  let conversation_context = get_conversation_context(chat_id, user_message)
  vibe_logger.debug(twin_log |> vibe_logger.with_data("context_len", json.int(string.length(conversation_context))), "Context received")

  // Строим улучшенный промпт с примерами и контекстом
  let system_prompt = build_digital_twin_prompt(from_name, conversation_context)
  vibe_logger.debug(twin_log |> vibe_logger.with_data("prompt_len", json.int(string.length(system_prompt))), "Prompt built")

  let api_key = case config.llm_api_key {
    Some(key) -> key
    None -> get_env("OPENROUTER_API_KEY")
  }

  case api_key {
    "" -> Error("OPENROUTER_API_KEY not configured")
    key -> call_openrouter_with_system(key, config.llm_model, system_prompt, user_message)
  }
}

/// Получает контекст из истории переписки через PostgreSQL
/// Включает: последние 50 сообщений диалога + историю с этим человеком из других чатов
fn get_conversation_context(chat_id: String, _query: String) -> String {
  let db_url = config.get_env_or("DATABASE_URL", "")
  case db_url {
    "" -> {
      logging.quick_info("[RAG] DATABASE_URL not set")
      ""
    }
    url -> {
      // SQL для последних 50 сообщений + история с этим человеком
      // Простой запрос - только сообщения из текущего диалога
      let sql =
        "SELECT CASE WHEN sender_id = 144022504 THEN 'Я' ELSE COALESCE(sender_name, 'Собеседник') END || ': ' || LEFT(text_content, 300) as msg
         FROM telegram_messages
         WHERE dialog_id = " <> chat_id <> "
           AND text_content IS NOT NULL AND text_content != ''
         ORDER BY timestamp DESC
         LIMIT 50"

      let rag_log = vibe_logger.new("rag")
        |> vibe_logger.with_data("chat_id", json.string(chat_id))
      vibe_logger.debug(rag_log, "Getting context via psql")
      // First check message count in DB
      let count_sql = "SELECT COUNT(*) FROM telegram_messages WHERE dialog_id = " <> chat_id
      case shellout.command(run: "psql", with: [url, "-t", "-c", count_sql], in: ".", opt: []) {
        Ok(cnt) -> vibe_logger.debug(rag_log |> vibe_logger.with_data("msg_count", json.string(string.trim(cnt))), "Messages in DB")
        Error(_) -> vibe_logger.warn(rag_log, "Could not count messages")
      }
      case shellout.command(run: "psql", with: [url, "-t", "-c", sql], in: ".", opt: []) {
        Ok(result) -> {
          let ctx = format_context(result)
          let ctx_len = string.length(ctx)
          vibe_logger.debug(rag_log |> vibe_logger.with_data("ctx_len", json.int(ctx_len)), "Context retrieved")
          // Показываем превью контекста для отладки
          case ctx_len > 0 {
            True -> vibe_logger.debug(rag_log |> vibe_logger.with_data("preview", json.string(string.slice(ctx, 0, 150))), "Context preview")
            False -> vibe_logger.warn(rag_log, "Context is empty! No messages saved for this chat yet")
          }
          ctx
        }
        Error(#(code, err)) -> {
          vibe_logger.error(rag_log |> vibe_logger.with_data("code", json.int(code)) |> vibe_logger.with_data("error", json.string(err)), "psql failed")
          ""
        }
      }
    }
  }
}

/// Форматирует сырой вывод psql в читаемый контекст
fn format_context(raw: String) -> String {
  let lines =
    string.split(raw, "\n")
    |> list.filter(fn(l) { string.trim(l) != "" })
    |> list.reverse  // от старых к новым
    |> list.map(fn(l) { "- " <> string.trim(l) })
    |> string.join("\n")

  case lines {
    "" -> ""
    ctx -> "История:\n" <> ctx
  }
}

/// Строит улучшенный Digital Twin промпт из базы данных
fn build_digital_twin_prompt(from_name: String, context: String) -> String {
  // Получаем промпт из БД через twin_config
  let base_prompt = case postgres.get_global_pool() {
    Some(pool) ->
      case twin_config.get_active(pool) {
        Ok(cfg) -> twin_config.build_system_prompt(cfg)
        Error(_) -> fallback_prompt()
      }
    None -> fallback_prompt()
  }

  // Добавляем контекст собеседника
  base_prompt
  <> "\n\n## КОНТЕКСТ РАЗГОВОРА\n\nСобеседник: "
  <> from_name
  <> "\n"
  <> case context {
    "" -> "История: Нет предыдущего контекста с этим человеком"
    ctx -> "Релевантный контекст из истории:\n" <> ctx
  }
}

/// Fallback промпт если БД недоступна
fn fallback_prompt() -> String {
  "Ты - AI ассистент. Отвечай кратко и по делу на русском языке."
}

/// Получить temperature из БД
fn get_temperature_from_db() -> Float {
  case postgres.get_global_pool() {
    Some(pool) ->
      case twin_config.get_active(pool) {
        Ok(cfg) -> cfg.settings.temperature
        Error(_) -> 0.9
      }
    None -> 0.9
  }
}

/// Получить max_tokens из БД
fn get_max_tokens_from_db() -> Int {
  case postgres.get_global_pool() {
    Some(pool) ->
      case twin_config.get_active(pool) {
        Ok(cfg) -> cfg.settings.max_tokens
        Error(_) -> 150
      }
    None -> 150
  }
}

/// Вызов OpenRouter с кастомным системным промптом
fn call_openrouter_with_system(
  api_key: String,
  model: String,
  system_prompt: String,
  user_message: String,
) -> Result(String, String) {
  let body = json.object([
    #("model", json.string(model)),
    #("temperature", json.float(get_temperature_from_db())),
    #("max_tokens", json.int(get_max_tokens_from_db())),
    #("messages", json.array([
      json.object([
        #("role", json.string("system")),
        #("content", json.string(system_prompt)),
      ]),
      json.object([
        #("role", json.string("user")),
        #("content", json.string(user_message)),
      ]),
    ], fn(x) { x })),
  ])
  |> json.to_string()

  let openrouter_log = vibe_logger.new("openrouter")
    |> vibe_logger.with_data("model", json.string(model))
  vibe_logger.info(openrouter_log, "Calling OpenRouter")

  let req = request.new()
    |> request.set_scheme(http.Https)
    |> request.set_method(http.Post)
    |> request.set_host("openrouter.ai")
    |> request.set_path("/api/v1/chat/completions")
    |> request.set_header("Authorization", "Bearer " <> api_key)
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("HTTP-Referer", "https://vibee.ai")
    |> request.set_header("X-Title", "VIBEE Digital Twin")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) -> {
      vibe_logger.debug(openrouter_log |> vibe_logger.with_data("status", json.int(response.status)), "Response received")
      case response.status {
        200 -> {
          case extract_content_from_response(response.body) {
            Ok(content) -> Ok(content)
            Error(err) -> {
              vibe_logger.error(openrouter_log |> vibe_logger.with_data("error", json.string(err)), "Parse error")
              Error("Parse error: " <> err)
            }
          }
        }
        status -> {
          vibe_logger.error(openrouter_log |> vibe_logger.with_data("status", json.int(status)), "HTTP error")
          Error("API error: HTTP " <> int.to_string(status))
        }
      }
    }
    Error(_) -> {
      vibe_logger.error(openrouter_log, "HTTP request failed")
      Error("HTTP request failed")
    }
  }
}

/// Вызов OpenRouter API для генерации ответа
fn call_openrouter(api_key: String, model: String, user_message: String) -> Result(String, String) {
  let system_prompt = "Ты VIBEE - дружелюбный AI-агент на Gleam/BEAM платформе. Отвечай кратко и полезно на русском языке. Ты эксперт по вайбкодингу - программированию с помощью AI-ассистентов."

  let body = json.object([
    #("model", json.string(model)),
    #("messages", json.array([
      json.object([
        #("role", json.string("system")),
        #("content", json.string(system_prompt)),
      ]),
      json.object([
        #("role", json.string("user")),
        #("content", json.string(user_message)),
      ]),
    ], fn(x) { x })),
  ])
  |> json.to_string()

  logging.quick_info("Calling OpenRouter API with model: " <> model)

  let req = request.new()
    |> request.set_scheme(http.Https)
    |> request.set_method(http.Post)
    |> request.set_host("openrouter.ai")
    |> request.set_path("/api/v1/chat/completions")
    |> request.set_header("Authorization", "Bearer " <> api_key)
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("HTTP-Referer", "https://vibee.ai")
    |> request.set_header("X-Title", "VIBEE Agent")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) -> {
      logging.quick_info("OpenRouter response status: " <> int.to_string(response.status))
      // Парсим JSON ответ и извлекаем content
      case response.status {
        200 -> {
          // Ищем "content":" в ответе и извлекаем текст
          case extract_content_from_response(response.body) {
            Ok(content) -> Ok(content)
            Error(err) -> {
              logging.quick_error("Failed to parse OpenRouter response: " <> err)
              logging.quick_info("Response body: " <> string.slice(response.body, 0, 200))
              Error("Parse error: " <> err)
            }
          }
        }
        status -> {
          logging.quick_error("OpenRouter API error: HTTP " <> int.to_string(status))
          logging.quick_info("Error body: " <> string.slice(response.body, 0, 200))
          Error("API error: HTTP " <> int.to_string(status))
        }
      }
    }
    Error(_) -> {
      logging.quick_error("HTTP request to OpenRouter failed")
      Error("HTTP request failed")
    }
  }
}

/// Извлекает content из JSON ответа OpenRouter
/// Формат: {"choices":[{"message":{"content":"..."}}]}
fn extract_content_from_response(body: String) -> Result(String, String) {
  // Ищем "content":" и берём текст до следующей кавычки
  let pattern = "\"content\":\""
  case string.split(body, pattern) {
    [_, rest, ..] -> {
      // Ищем закрывающую кавычку (с учётом escaped quotes)
      case find_closing_quote(rest, "", False) {
        Ok(content) -> {
          // Декодируем escaped символы
          let decoded = content
            |> string.replace("\\n", "\n")
            |> string.replace("\\\"", "\"")
            |> string.replace("\\\\", "\\")
          Ok(decoded)
        }
        Error(e) -> Error(e)
      }
    }
    _ -> Error("No content field found")
  }
}

/// Находит закрывающую кавычку с учётом escape-последовательностей
fn find_closing_quote(s: String, acc: String, escaped: Bool) -> Result(String, String) {
  case string.pop_grapheme(s) {
    Ok(#(char, rest)) -> {
      case escaped {
        True -> find_closing_quote(rest, acc <> char, False)
        False -> {
          case char {
            "\\" -> find_closing_quote(rest, acc <> char, True)
            "\"" -> Ok(acc)
            _ -> find_closing_quote(rest, acc <> char, False)
          }
        }
      }
    }
    Error(_) -> Error("Unexpected end of string")
  }
}

/// Парсит bridge URL в компоненты (scheme, host, port)
fn parse_bridge_url(url: String) -> #(http.Scheme, String, Int) {
  case string.starts_with(url, "https://") {
    True -> {
      let host = string.drop_start(url, 8)
        |> string.split("/")
        |> list.first
        |> fn(r) { case r { Ok(h) -> h _ -> "localhost" } }
      #(http.Https, host, 443)
    }
    False -> {
      case string.starts_with(url, "http://") {
        True -> {
          let rest = string.drop_start(url, 7)
            |> string.split("/")
            |> list.first
            |> fn(r) { case r { Ok(h) -> h _ -> "localhost:8081" } }
          // Проверяем на port
          case string.split(rest, ":") {
            [h, p] -> {
              let port = case int.parse(p) {
                Ok(n) -> n
                Error(_) -> 80
              }
              #(http.Http, h, port)
            }
            _ -> #(http.Http, rest, 80)
          }
        }
        False -> #(http.Http, "localhost", 8081)
      }
    }
  }
}

/// Отправка сообщения через Go bridge
pub fn send_message(
  config: TelegramAgentConfig,
  chat_id: String,
  text: String,
  reply_to: Option(Int),
) -> Result(Int, String) {
  // Parse chat_id to int for Go bridge
  let chat_id_int = case int.parse(chat_id) {
    Ok(id) -> id
    Error(_) -> 0
  }

  let body = json.object([
    #("chat_id", json.int(chat_id_int)),
    #("text", json.string(text)),
    #("reply_to", case reply_to {
      None -> json.null()
      Some(id) -> json.int(id)
    }),
  ])
  |> json.to_string()

  // Parse bridge URL dynamically
  let #(scheme, host, port) = parse_bridge_url(config.bridge_url)

  let send_log = vibe_logger.new("send")
    |> vibe_logger.with_session_id(config.session_id)
    |> vibe_logger.with_data("chat_id", json.string(chat_id))
    |> vibe_logger.with_data("host", json.string(host))
  vibe_logger.debug(send_log, "Sending message")

  // Get bridge API key for authorization
  let api_key = telegram_config.bridge_api_key()

  let req = request.new()
    |> request.set_scheme(scheme)
    |> request.set_method(http.Post)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path("/api/v1/send")
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("Authorization", "Bearer " <> api_key)
    |> request.set_header("X-Session-ID", config.session_id)
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) -> {
      vibe_logger.debug(send_log |> vibe_logger.with_data("status", json.int(response.status)), "Response received")
      case response.status {
        200 -> {
          vibe_logger.info(send_log, "Message sent successfully")
          Ok(0)
        }
        status -> {
          let err = "HTTP " <> int.to_string(status) <> ": " <> response.body
          vibe_logger.error(send_log |> vibe_logger.with_data("error", json.string(err)), "Send failed")
          Error(err)
        }
      }
    }
    Error(_) -> {
      vibe_logger.error(send_log, "HTTP request failed")
      Error("Network error")
    }
  }
}

/// Отправка фото через Go bridge
pub fn send_photo(
  config: TelegramAgentConfig,
  chat_id: String,
  photo_url: String,
  caption: Option(String),
) -> Result(Int, String) {
  let chat_id_int = case int.parse(chat_id) {
    Ok(id) -> id
    Error(_) -> 0
  }

  let body = json.object([
    #("chat_id", json.int(chat_id_int)),
    #("photo_url", json.string(photo_url)),
    #("caption", case caption {
      None -> json.null()
      Some(c) -> json.string(c)
    }),
  ])
  |> json.to_string()

  let #(scheme, host, port) = parse_bridge_url(config.bridge_url)

  let photo_log = vibe_logger.new("photo")
    |> vibe_logger.with_session_id(config.session_id)
    |> vibe_logger.with_data("chat_id", json.string(chat_id))
    |> vibe_logger.with_data("host", json.string(host))
  vibe_logger.debug(photo_log, "Sending photo")

  // Get bridge API key for authorization
  let api_key = telegram_config.bridge_api_key()

  let req = request.new()
    |> request.set_scheme(scheme)
    |> request.set_method(http.Post)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path("/api/v1/send/photo")
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("Authorization", "Bearer " <> api_key)
    |> request.set_header("X-Session-ID", config.session_id)
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) -> {
      vibe_logger.debug(photo_log |> vibe_logger.with_data("status", json.int(response.status)), "Response received")
      case response.status {
        200 -> {
          vibe_logger.info(photo_log, "Photo sent successfully")
          Ok(0)
        }
        status -> {
          let err = "HTTP " <> int.to_string(status) <> ": " <> response.body
          vibe_logger.error(photo_log |> vibe_logger.with_data("error", json.string(err)), "Send failed")
          Error(err)
        }
      }
    }
    Error(_) -> {
      vibe_logger.error(photo_log, "HTTP request failed")
      Error("Network error")
    }
  }
}

/// Получить историю сообщений из чата
pub fn get_history(config: TelegramAgentConfig, chat_id: String, limit: Int) -> Result(String, String) {
  let url = config.bridge_url <> "/api/v1/history/" <> chat_id <> "?limit=" <> int.to_string(limit)

  let req = request.new()
    |> request.set_scheme(http.Http)
    |> request.set_method(http.Get)
    |> request.set_host("localhost")
    |> request.set_port(8081)
    |> request.set_path("/api/v1/history/" <> chat_id)
    |> request.set_header("X-Session-ID", config.session_id)

  case httpc.send(req) {
    Ok(response) -> Ok(response.body)
    Error(_) -> Error("Failed to get history")
  }
}

/// Получить список диалогов
pub fn get_dialogs(config: TelegramAgentConfig, limit: Int) -> Result(String, String) {
  // Parse bridge URL dynamically
  let #(scheme, host, port) = parse_bridge_url(config.bridge_url)
  let api_key = telegram_config.bridge_api_key()

  let req = request.new()
    |> request.set_scheme(scheme)
    |> request.set_method(http.Get)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path("/api/v1/dialogs")
    |> request.set_header("Authorization", "Bearer " <> api_key)
    |> request.set_header("X-Session-ID", config.session_id)

  case httpc.send(req) {
    Ok(response) -> Ok(response.body)
    Error(_) -> Error("Failed to get dialogs")
  }
}

/// Генерирует ответ для триггерного чата с вариациями
fn generate_trigger_reply(
  config: TelegramAgentConfig,
  user_message: String,
  from_name: String,
  chat_id: String,
) -> Result(String, String) {
  // Получаем конфигурацию триггерного чата
  case trigger_chats.find_chat_config(chat_id) {
    Ok(chat_config) -> {
      // Используем AI для генерации вариации на основе шаблона
      let system_prompt =
        "Ты дружелюбный человек. Напиши ТОЛЬКО приглашение в личные сообщения.\n\n"
        <> "СТРОГИЕ ПРАВИЛА:\n"
        <> "- ТОЛЬКО 1 предложение\n"
        <> "- БЕЗ @ и username\n"
        <> "- БЕЗ ссылок\n"
        <> "- БЕЗ markdown форматирования\n"
        <> "- БЕЗ эмодзи\n"
        <> "- БЕЗ кавычек вокруг ответа\n\n"
        <> "Примеры хороших ответов:\n"
        <> "Привет! Могу помочь, напиши в личку.\n"
        <> "Помогу с этим, пиши в ЛС.\n"
        <> "Могу помочь, напиши в личные сообщения.\n"
        <> "Пиши в личку, всё расскажу.\n"
        <> "Напиши мне в ЛС, помогу разобраться.\n\n"
        <> "ЗАПРЕЩЕНО:\n"
        <> "- Упоминать username (типа @name)\n"
        <> "- Использовать ** или __ или любое форматирование\n"
        <> "- Использовать [] или () для ссылок\n\n"
        <> "Просто ответь одним коротким предложением с приглашением в личку."
      
      let user_prompt = "Ответь дружелюбно и естественно. Каждый раз по-разному!"
      
      // Получаем API key из конфига
      let api_key = case config.llm_api_key {
        Some(key) -> key
        None -> ""
      }
      
      let trigger_log = vibe_logger.new("trigger_reply")
        |> vibe_logger.with_data("chat_id", json.string(chat_id))

      // Проверяем API key
      case api_key {
        "" -> {
          vibe_logger.warn(trigger_log, "No API key, using template")
          Ok(chat_config.response_template)
        }
        key -> {
          vibe_logger.info(trigger_log, "Calling AI to generate variation")
          // Вызываем OpenRouter для генерации
          case call_openrouter_with_system(
            key,
            config.llm_model,
            system_prompt,
            user_prompt,
          ) {
            Ok(reply) -> {
              // Очищаем ответ от @ и markdown
              let cleaned_reply = clean_trigger_response(reply)
              vibe_logger.info(trigger_log |> vibe_logger.with_data("reply", json.string(string.slice(cleaned_reply, 0, 60))), "Variation generated")
              Ok(cleaned_reply)
            }
            Error(err) -> {
              // Fallback на шаблон если AI не сработал
              vibe_logger.error(trigger_log |> vibe_logger.with_data("error", json.string(err)), "AI failed, using template")
              Ok(chat_config.response_template)
            }
          }
        }
      }
    }
    Error(_) -> {
      // Если нет конфигурации, используем обычный Digital Twin
      generate_digital_twin_reply(config, user_message, from_name, chat_id)
    }
  }
}

/// Очищает ответ от @ упоминаний, ссылок и markdown форматирования
fn clean_trigger_response(text: String) -> String {
  text
  // Убираем @ упоминания (например @username)
  |> remove_at_mentions()
  // Убираем markdown форматирование
  |> string.replace("**", "")
  |> string.replace("__", "")
  |> string.replace("*", "")
  |> string.replace("_", "")
  |> string.replace("`", "")
  // Убираем ссылки в формате [text](url)
  |> remove_markdown_links()
  // Убираем кавычки вокруг ответа
  |> string.trim()
  |> remove_surrounding_quotes()
}

/// Убирает @ упоминания из текста
fn remove_at_mentions(text: String) -> String {
  // Простой подход: разбиваем по пробелам и фильтруем слова начинающиеся с @
  string.split(text, " ")
  |> list.filter(fn(word) { !string.starts_with(word, "@") })
  |> string.join(" ")
}

/// Убирает markdown ссылки [text](url) -> text
fn remove_markdown_links(text: String) -> String {
  // Если нет markdown ссылок, возвращаем как есть
  case string.contains(text, "](") {
    False -> text
    True -> {
      // Простая замена: убираем всё после [ до ]( и всё в ()
      // Это грубый подход, но работает для большинства случаев
      text
      |> string.replace("[", "")
      |> string.replace("](", " ")
      |> remove_parentheses_content()
    }
  }
}

/// Убирает контент в скобках (ссылки)
fn remove_parentheses_content(text: String) -> String {
  case string.split_once(text, "(") {
    Error(_) -> text
    Ok(#(before, after)) -> {
      case string.split_once(after, ")") {
        Error(_) -> text
        Ok(#(_, rest)) -> before <> remove_parentheses_content(rest)
      }
    }
  }
}

/// Убирает кавычки вокруг текста
fn remove_surrounding_quotes(text: String) -> String {
  let text = string.trim(text)
  case string.starts_with(text, "\"") && string.ends_with(text, "\"") {
    True -> {
      text
      |> string.drop_start(1)
      |> string.drop_end(1)
      |> string.trim()
    }
    False -> text
  }
}

/// Convert Bot API error to string for logging
fn bot_api_error_to_string(err: tg_types.TelegramError) -> String {
  case err {
    tg_types.ConnectionError(msg) -> "ConnectionError: " <> msg
    tg_types.AuthError(msg) -> "AuthError: " <> msg
    tg_types.ApiError(code, msg) -> "ApiError(" <> int.to_string(code) <> "): " <> msg
    tg_types.NetworkError(msg) -> "NetworkError: " <> msg
    tg_types.InvalidSession -> "InvalidSession"
    tg_types.NotAuthorized -> "NotAuthorized"
  }
}
