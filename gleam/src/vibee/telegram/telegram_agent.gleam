// VIBEE Telegram Agent
// Аналог TelegramService из plugin-telegram-craft
// Работает через Go bridge для MTProto

import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}

import gleam/string
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
import vibee/telegram/dialog_forwarder

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
  case state.bot_user_id {
    Some(id) -> {
      io.println("[USER_ID] Using cached bot_user_id: " <> int.to_string(id))
      #(state, Some(id))
    }
    None -> {
      io.println("[USER_ID] Fetching bot_user_id from session...")
      // Fetch user_id from getMe
      case get_me(state.config.bridge_url, state.config.session_id) {
        Ok(user_id) -> {
          io.println("[USER_ID] ✅ Bot user_id fetched: " <> int.to_string(user_id))
          let new_state = AgentState(..state, bot_user_id: Some(user_id))
          #(new_state, Some(user_id))
        }
        Error(reason) -> {
          io.println("[USER_ID] ❌ Failed to get user_id: " <> reason)
          #(state, None)
        }
      }
    }
  }
}

/// Get current user info from Telegram
fn get_me(bridge_url: String, session_id: String) -> Result(Int, String) {
  // Parse bridge_url to get scheme, host, port
  let url_parts = case string.split(bridge_url, "://") {
    [scheme, rest] -> {
      case string.split(rest, ":") {
        [host, port_str] -> #(scheme, host, port_str)
        [host] -> #(scheme, host, "8081")
        _ -> #("http", "localhost", "8081")
      }
    }
    _ -> #("http", "localhost", "8081")
  }
  
  let #(scheme, host, port_str) = url_parts
  let body = "{\"session_id\": \"" <> session_id <> "\"}"
  
  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_scheme(case scheme {
      "https" -> http.Https
      _ -> http.Http
    })
    |> request.set_host(host)
    |> request.set_port(case int.parse(port_str) {
      Ok(p) -> p
      Error(_) -> 8081
    })
    |> request.set_path("/getMe")
    |> request.set_body(body)
    |> request.prepend_header("content-type", "application/json")
  
  case httpc.send(req) {
    Ok(response) -> {
      io.println("[GETME] Response: " <> response.body)
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
                Ok(id) -> Ok(id)
                Error(_) -> Error("Failed to parse user_id")
              }
            }
            _ -> Error("Invalid response format")
          }
        }
        _ -> Error("No id field in response")
      }
    }
    Error(err) -> {
      io.println("[GETME] HTTP error")
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
  text: String,
  message_id: Int,
) -> AgentState {
  // Логируем в stdout для Fly.io видимости
  io.println("[MSG] chat=" <> chat_id <> " from_id=" <> int.to_string(from_id) <> " from=" <> from_name <> " text=" <> string.slice(text, 0, 50))

  // Get or fetch bot user_id
  let #(updated_state, bot_id) = get_or_fetch_user_id(state)

  // Не отвечаем на собственные сообщения (по user_id или owner_id, предотвращаем бесконечный цикл)
  let should_skip = case bot_id {
    Some(id) -> {
      let is_bot = from_id == id
      let is_owner = from_id == updated_state.config.owner_id
      io.println("[FILTER] from_id=" <> int.to_string(from_id) <> " bot_id=" <> int.to_string(id) <> " owner_id=" <> int.to_string(updated_state.config.owner_id))
      io.println("[FILTER] is_bot=" <> case is_bot { True -> "YES" False -> "NO" } <> " is_owner=" <> case is_owner { True -> "YES" False -> "NO" })
      is_bot || is_owner
    }
    None -> {
      io.println("[FILTER] No bot_id cached, checking owner_id only")
      from_id == updated_state.config.owner_id
    }
  }

  case should_skip {
    True -> {
      io.println("[MSG] ⏭️  SKIPPING own message from user_id: " <> int.to_string(from_id))
      updated_state
    }
    False -> {
      io.println("[MSG] ✅ PROCESSING message from user_id: " <> int.to_string(from_id))
      // Сначала проверяем команды (работают везде, включая личные чаты)
      case parse_command(text) {
        Some(#("neurophoto", prompt)) -> {
          io.println("[CMD] /neurophoto detected! Prompt: " <> prompt)
          handle_neurophoto_command(updated_state, chat_id, message_id, prompt)
        }
        Some(#("neuro", prompt)) -> {
          io.println("[CMD] /neuro detected! Prompt: " <> prompt)
          handle_neurophoto_command(updated_state, chat_id, message_id, prompt)
        }
        Some(#("start", _)) -> {
          io.println("[CMD] /start detected!")
          let welcome = "Privet! Ya VIBEE - AI agent dlya generacii izobrazhenij.\n\nKomandy:\n/neurophoto <prompt> - generaciya izobrazheniya\n/neuro <prompt> - korotkaya versiya\n\nPrimer: /neurophoto cyberpunk portrait neon lights"
          let _ = send_message(updated_state.config, chat_id, welcome, Some(message_id))
          AgentState(..updated_state, total_messages: updated_state.total_messages + 1)
        }
        Some(#("help", _)) -> {
          io.println("[CMD] /help detected!")
          let help_text = "VIBEE Bot - Komandy:\n\n/neurophoto <prompt> - Generaciya izobrazheniya s FLUX LoRA\n/neuro <prompt> - Korotkaya versiya\n/pricing - Tarify VIBEE\n/quiz - Podobrat' tarif\n\nTrigger slovo NEURO_SAGE dobavlyaetsya avtomaticheski."
          let _ = send_message(updated_state.config, chat_id, help_text, Some(message_id))
          AgentState(..updated_state, total_messages: updated_state.total_messages + 1)
        }
        Some(#("pricing", _)) -> {
          io.println("[CMD] /pricing detected!")
          let pricing_text = "💎 VIBEE Tarify:\n\n🥉 JUNIOR - $99/mes\n• 100 generacij\n• Telegram bot\n• Email podderzhka\n\n🥈 MIDDLE - $299/mes\n• 500 generacij\n• Custom persona\n• CRM + Analytics\n\n🥇 SENIOR - $999/mes\n• Bezlimit generacij\n• Multichannel\n• API dostup + SLA\n\n👉 /quiz - podobrat' tarif"
          let _ = send_message(updated_state.config, chat_id, pricing_text, Some(message_id))
          AgentState(..updated_state, total_messages: updated_state.total_messages + 1)
        }
        Some(#("quiz", _)) -> {
          io.println("[CMD] /quiz detected!")
          let quiz_text = "🎯 Quiz: Kakoj tarif vam podhodit?\n\n1️⃣ Skolko generacij v mesyac vam nuzhno?\n   A) Do 100\n   B) 100-500\n   C) Bolshe 500\n\n2️⃣ Nuzhna li integracija s CRM?\n   A) Net\n   B) Da\n\n3️⃣ Nuzhен li API dostup?\n   A) Net\n   B) Da\n\nOtvetjte bukvami, naprimer: ABA\n\n💡 Ili napishite 'pomosch' dlya konsultacii"
          let _ = send_message(updated_state.config, chat_id, quiz_text, Some(message_id))
          AgentState(..updated_state, total_messages: updated_state.total_messages + 1)
        }
        _ -> {
          // Проверяем, является ли это триггерным чатом (Sniper Mode)
          case trigger_chats.is_trigger_chat_active(chat_id) {
            True -> {
              // SNIPER MODE: отвечаем ТОЛЬКО на триггеры
              io.println("[SNIPER] 🎯 Chat " <> chat_id <> " is in SNIPER MODE")
              io.println("[SNIPER] Message text: " <> text)
              case trigger_chats.should_respond_to_trigger(chat_id, text) {
                True -> {
                  io.println("[SNIPER] 🔥 TRIGGER FOUND! Generating response...")
                  process_with_digital_twin(updated_state, chat_id, message_id, text, from_name)
                }
                False -> {
                  io.println("[SNIPER] 🤫 No trigger detected, staying silent")
                  AgentState(..updated_state, total_messages: updated_state.total_messages + 1)
                }
              }
            }
            False -> {
              // Обычный режим - Digital Twin или normal mode
              case updated_state.config.digital_twin_enabled {
                True -> {
                  // Digital Twin отвечает на ВСЕ сообщения (кроме sniper чатов)
                  io.println("[DIGITAL_TWIN] Responding to message in chat " <> chat_id)
                  process_with_digital_twin(updated_state, chat_id, message_id, text, from_name)
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

/// Обработка группового сообщения (проверка триггеров)
fn handle_group_message(state: AgentState, chat_id: String, message_id: Int, text: String) -> AgentState {
  case target_chats.should_process_chat(chat_id) {
    False -> {
      io.println("[MSG] Skipping non-target group: " <> chat_id)
      state
    }
    True -> {
      case should_reply(state, text) {
        False -> {
          io.println("[MSG] No trigger in group: " <> string.slice(text, 0, 30))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
        True -> {
          io.println("[TRIGGER] Found in group! Generating reply")
          process_with_llm(state, chat_id, message_id, text)
        }
      }
    }
  }
}

/// Обработка в обычном режиме (без Digital Twin)
fn handle_normal_mode(state: AgentState, chat_id: String, message_id: Int, text: String) -> AgentState {
  case target_chats.should_process_chat(chat_id) {
    False -> {
      case int.parse(chat_id) {
        Ok(n) if n > 0 -> {
          io.println("[MSG] Personal chat, processing without triggers")
          process_with_llm(state, chat_id, message_id, text)
        }
        _ -> {
          io.println("[MSG] Skipping non-target chat: " <> chat_id)
          state
        }
      }
    }
    True -> {
      io.println("[MSG] Processing target chat: " <> chat_id)
      case should_reply(state, text) {
        False -> {
          io.println("[MSG] No trigger found in: " <> string.slice(text, 0, 30))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
        True -> {
          io.println("[TRIGGER] Found! Generating reply for: " <> string.slice(text, 0, 30))
          process_with_llm(state, chat_id, message_id, text)
        }
      }
    }
  }
}

/// Digital Twin обработка - отвечает в стиле владельца аккаунта
fn process_with_digital_twin(state: AgentState, chat_id: String, message_id: Int, text: String, from_name: String) -> AgentState {
  io.println("[TWIN] Processing message from " <> from_name <> " in chat " <> chat_id)
  
  // Проверяем триггерные слова для этого чата
  let has_trigger = trigger_chats.should_respond_to_trigger(chat_id, text)
  
  case has_trigger {
    True -> {
      io.println("[TRIGGER] Found trigger word in chat " <> chat_id)
      
      // Генерируем ответ с учетом шаблона для триггерного чата
      case generate_trigger_reply(state.config, text, from_name, chat_id) {
        Ok(reply) -> {
          io.println("[TWIN] Generated reply: " <> string.slice(reply, 0, 80) <> "...")
          
          // Отправляем ответ
          case send_message(state.config, chat_id, reply, Some(message_id)) {
            Ok(msg_id) -> {
              io.println("[TWIN] Message sent OK, id=" <> int.to_string(msg_id))
              
              // Сохраняем ответ в БД
              let dialog_id = case int.parse(chat_id) {
                Ok(id) -> id
                Error(_) -> 0
              }
              io.println("[DB] Saving reply to dialog=" <> int.to_string(dialog_id) <> " msg_id=" <> int.to_string(msg_id))
              case postgres.insert_message_simple(dialog_id, msg_id, state.config.owner_id, "Я", reply) {
                Ok(_) -> io.println("[DB] Reply saved OK")
                Error(e) -> io.println("[DB] ERROR saving reply: " <> e)
              }
              
              // Пересылаем диалог в целевую группу
              case trigger_chats.get_forward_chat_id(chat_id) {
                Ok(forward_chat_id) -> {
                  io.println("[FORWARD] Forwarding dialog to " <> forward_chat_id)
                  
                  let original_msg = dialog_forwarder.MessageInfo(
                    chat_id: chat_id,
                    message_id: message_id,
                    from_name: from_name,
                    text: text,
                    timestamp: 0,
                  )
                  
                  let agent_msg = dialog_forwarder.MessageInfo(
                    chat_id: chat_id,
                    message_id: msg_id,
                    from_name: "Федор (Agent)",
                    text: reply,
                    timestamp: 0,
                  )
                  
                  case dialog_forwarder.forward_dialog(
                    state.config.session_id,
                    original_msg,
                    agent_msg,
                    forward_chat_id,
                  ) {
                    dialog_forwarder.ForwardSuccess(fwd_id) -> {
                      io.println("[FORWARD] Dialog forwarded successfully, msg_id=" <> int.to_string(fwd_id))
                      
                      // TODO: Сохранить лид в базу данных
                      // Нужен from_id из контекста
                      io.println("[LEAD] 💾 Lead would be saved here")
                    }
                    dialog_forwarder.ForwardError(reason) -> {
                      io.println("[FORWARD] Failed to forward: " <> reason)
                    }
                  }
                }
                Error(_) -> {
                  io.println("[FORWARD] No forward target configured for chat " <> chat_id)
                }
              }
              
              AgentState(..state, total_messages: state.total_messages + 1)
            }
            Error(send_err) -> {
              io.println("[TWIN] SEND FAILED: " <> send_err)
              AgentState(..state, total_messages: state.total_messages + 1)
            }
          }
        }
        Error(err) -> {
          io.println("[TWIN] GENERATE FAILED: " <> err)
          state
        }
      }
    }
    False -> {
      // Нет триггера - обычная обработка Digital Twin
      case generate_digital_twin_reply(state.config, text, from_name, chat_id) {
        Ok(reply) -> {
          io.println("[TWIN] Generated reply: " <> string.slice(reply, 0, 80) <> "...")
          case send_message(state.config, chat_id, reply, Some(message_id)) {
            Ok(msg_id) -> {
              io.println("[TWIN] Message sent OK, id=" <> int.to_string(msg_id))
              let dialog_id = case int.parse(chat_id) {
                Ok(id) -> id
                Error(_) -> 0
              }
              io.println("[DB] Saving reply to dialog=" <> int.to_string(dialog_id) <> " msg_id=" <> int.to_string(msg_id))
              case postgres.insert_message_simple(dialog_id, msg_id, state.config.owner_id, "Я", reply) {
                Ok(_) -> io.println("[DB] Reply saved OK")
                Error(e) -> io.println("[DB] ERROR saving reply: " <> e)
              }
              AgentState(..state, total_messages: state.total_messages + 1)
            }
            Error(send_err) -> {
              io.println("[TWIN] SEND FAILED: " <> send_err)
              AgentState(..state, total_messages: state.total_messages + 1)
            }
          }
        }
        Error(err) -> {
          io.println("[TWIN] GENERATE FAILED: " <> err)
          state
        }
      }
    }
  }
}

// ============================================================
// Command Parsing and Handlers
// ============================================================

/// Парсит команду из текста сообщения
/// Возвращает Some(#(command, args)) или None
fn parse_command(text: String) -> Option(#(String, String)) {
  let trimmed = string.trim(text)
  case string.starts_with(trimmed, "/") {
    False -> None
    True -> {
      let without_slash = string.drop_start(trimmed, 1)
      case string.split(without_slash, " ") {
        [] -> None
        [cmd] -> Some(#(string.lowercase(cmd), ""))
        [cmd, ..rest] -> Some(#(string.lowercase(cmd), string.join(rest, " ")))
      }
    }
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
      let _ = send_message(state.config, chat_id, "Generiruyiu izobrazhenie s NEURO_SAGE...\n\nPrompt: " <> prompt, Some(message_id))

      // Вызываем FAL.ai
      case generate_image_fal(prompt) {
        Ok(image_url) -> {
          io.println("[NEUROPHOTO] Generated: " <> image_url)
          // Отправляем изображение
          let _ = send_photo(state.config, chat_id, image_url, Some("Generated: " <> prompt))
          AgentState(..state, total_messages: state.total_messages + 1)
        }
        Error(err) -> {
          io.println("[NEUROPHOTO ERROR] " <> err)
          let _ = send_message(state.config, chat_id, "Oshibka generacii: " <> err, Some(message_id))
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
        #("sync_mode", json.bool(True)),
        #("guidance_scale", json.float(3.5)),
        #("num_inference_steps", json.int(28)),
      ])
      |> json.to_string()

      io.println("[FAL] Calling FAL.ai with prompt: " <> string.slice(full_prompt, 0, 50))

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
          io.println("[FAL] Response status: " <> int.to_string(response.status))
          case response.status {
            200 -> {
              // Парсим ответ и извлекаем URL изображения
              extract_image_url(response.body)
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
fn extract_image_url(body: String) -> Result(String, String) {
  // Формат: {"images":[{"url":"https://..."}],...}
  let pattern = "\"url\":\""
  case string.split(body, pattern) {
    [_, rest, ..] -> {
      case string.split(rest, "\"") {
        [url, ..] -> Ok(url)
        _ -> Error("Could not parse image URL")
      }
    }
    _ -> {
      // Проверяем на IN_QUEUE статус
      case string.contains(body, "IN_QUEUE") {
        True -> Error("Request queued - try again in 30 seconds")
        False -> Error("No image URL in response: " <> string.slice(body, 0, 200))
      }
    }
  }
}

/// Получает переменную окружения
/// Использует FFI wrapper для конвертации binary -> charlist -> os:getenv -> binary
@external(erlang, "vibee_ffi", "get_env")
fn get_env(name: String) -> String

/// Обработка через LLM
fn process_with_llm(state: AgentState, chat_id: String, message_id: Int, text: String) -> AgentState {
  case generate_reply(state.config, text) {
    Ok(reply) -> {
      io.println("[LLM] Reply: " <> string.slice(reply, 0, 50) <> "...")
      let _ = send_message(state.config, chat_id, reply, Some(message_id))
      AgentState(..state, total_messages: state.total_messages + 1)
    }
    Error(err) -> {
      io.println("[LLM ERROR] Failed to generate reply: " <> err)
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
  io.println("[TWIN] Generating reply for: " <> from_name <> " in chat " <> chat_id)

  // Получаем контекст из истории (RAG) - TODO: интегрировать с conversation_get_context
  let conversation_context = get_conversation_context(chat_id, user_message)
  io.println("[TWIN] Context received, length: " <> int.to_string(string.length(conversation_context)))

  // Строим улучшенный промпт с примерами и контекстом
  let system_prompt = build_digital_twin_prompt(from_name, conversation_context)
  io.println("[TWIN] Prompt built, length: " <> int.to_string(string.length(system_prompt)))

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

      io.println("[RAG] Getting context for chat " <> chat_id <> " via psql...")
      // First check message count in DB
      let count_sql = "SELECT COUNT(*) FROM telegram_messages WHERE dialog_id = " <> chat_id
      case shellout.command(run: "psql", with: [url, "-t", "-c", count_sql], in: ".", opt: []) {
        Ok(cnt) -> io.println("[RAG] Messages in DB for dialog " <> chat_id <> ": " <> string.trim(cnt))
        Error(_) -> io.println("[RAG] Could not count messages")
      }
      case shellout.command(run: "psql", with: [url, "-t", "-c", sql], in: ".", opt: []) {
        Ok(result) -> {
          let ctx = format_context(result)
          let ctx_len = string.length(ctx)
          io.println("[RAG] Got " <> int.to_string(ctx_len) <> " chars of context")
          // Показываем превью контекста для отладки
          case ctx_len > 0 {
            True -> io.println("[RAG] Preview: " <> string.slice(ctx, 0, 150) <> "...")
            False -> io.println("[RAG] WARNING: Context is empty! No messages saved for this chat yet")
          }
          ctx
        }
        Error(#(code, err)) -> {
          io.println("[RAG] ERROR psql failed! code=" <> int.to_string(code) <> " err=" <> err)
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

  io.println("[DIGITAL_TWIN] Calling OpenRouter with model: " <> model)

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
      io.println("[DIGITAL_TWIN] Response status: " <> int.to_string(response.status))
      case response.status {
        200 -> {
          case extract_content_from_response(response.body) {
            Ok(content) -> Ok(content)
            Error(err) -> {
              io.println("[DIGITAL_TWIN ERROR] Parse error: " <> err)
              Error("Parse error: " <> err)
            }
          }
        }
        status -> {
          io.println("[DIGITAL_TWIN ERROR] HTTP " <> int.to_string(status))
          Error("API error: HTTP " <> int.to_string(status))
        }
      }
    }
    Error(_) -> {
      io.println("[DIGITAL_TWIN ERROR] HTTP request failed")
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

  io.println("[SEND] Sending message to chat " <> chat_id <> " via " <> host <> ":" <> int.to_string(port))

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
      io.println("[SEND] Response status: " <> int.to_string(response.status))
      case response.status {
        200 -> {
          io.println("[SEND] Message sent successfully!")
          Ok(0)
        }
        status -> {
          let err = "HTTP " <> int.to_string(status) <> ": " <> response.body
          io.println("[SEND ERROR] " <> err)
          Error(err)
        }
      }
    }
    Error(_) -> {
      io.println("[SEND ERROR] HTTP request failed")
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

  io.println("[PHOTO] Sending photo to chat " <> chat_id <> " via " <> host)

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
      io.println("[PHOTO] Response status: " <> int.to_string(response.status))
      case response.status {
        200 -> {
          io.println("[PHOTO] Photo sent successfully!")
          Ok(0)
        }
        status -> {
          let err = "HTTP " <> int.to_string(status) <> ": " <> response.body
          io.println("[PHOTO ERROR] " <> err)
          Error(err)
        }
      }
    }
    Error(_) -> {
      io.println("[PHOTO ERROR] HTTP request failed")
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
      
      // Проверяем API key
      case api_key {
        "" -> {
          io.println("[TRIGGER_REPLY] ❌ No API key, using template")
          Ok(chat_config.response_template)
        }
        key -> {
          io.println("[TRIGGER_REPLY] 🤖 Calling AI to generate variation...")
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
              io.println("[TRIGGER_REPLY] ✅ Generated variation: " <> string.slice(cleaned_reply, 0, 60) <> "...")
              Ok(cleaned_reply)
            }
            Error(err) -> {
              // Fallback на шаблон если AI не сработал
              io.println("[TRIGGER_REPLY] ❌ AI failed, using template: " <> err)
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
