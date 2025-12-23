// VIBEE Polling Actor
// OTP Actor для polling сообщений из Telegram через Go Bridge
// With Agent Registry integration for observability

import gleam/erlang/process.{type Subject}
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/set.{type Set}
import gleam/string
import vibee/agent/agent_registry
import vibee/config/target_chats
import vibee/config/trigger_chats
import vibee/db/postgres
import vibee/events/event_bus
import vibee/http_retry
import vibee/logging
import vibee/telegram/telegram_agent
import vibee/vibe_logger

/// Состояние Polling актора
pub type PollingState {
  PollingState(
    config: telegram_agent.TelegramAgentConfig,
    agent_state: telegram_agent.AgentState,
    poll_count: Int,
    event_bus: Option(Subject(event_bus.PubSubMessage)),
    seen_ids: Set(String),
    agent_id: String,
    logger: vibe_logger.VibeLogger,
    /// Флаг: первый poll выполнен (seen_ids заполнен)
    /// При первом poll мы только заполняем seen_ids, не обрабатываем сообщения
    initial_poll_done: Bool,
  )
}

/// Сообщения для Polling Actor
pub type PollingMessage {
  Poll
  Stop
}

/// Создать начальное состояние
fn init_state(config: telegram_agent.TelegramAgentConfig) -> PollingState {
  let agent_id = "polling_" <> config.session_id
  let logger = vibe_logger.new("polling")
    |> vibe_logger.with_session_id(config.session_id)
    |> vibe_logger.with_data("agent_id", json.string(agent_id))

  // Initialize agent registry and register this agent
  agent_registry.init()
  let timestamp = get_iso_timestamp()
  agent_registry.register(agent_registry.AgentInfo(
    id: agent_id,
    agent_type: agent_registry.PollingAgent,
    status: agent_registry.Starting,
    started_at: timestamp,
    last_activity: timestamp,
    message_count: 0,
    error_count: 0,
    session_id: Some(config.session_id),
    extra: [],
  ))

  vibe_logger.info(logger, "Polling actor initializing")

  PollingState(
    config: config,
    agent_state: telegram_agent.init(config),
    poll_count: 0,
    event_bus: None,
    seen_ids: set.new(),
    agent_id: agent_id,
    logger: logger,
    initial_poll_done: False,
  )
}

/// Создать начальное состояние с event bus
fn init_state_with_events(
  config: telegram_agent.TelegramAgentConfig,
  bus: Subject(event_bus.PubSubMessage),
) -> PollingState {
  let agent_id = "polling_" <> config.session_id
  let logger = vibe_logger.new("polling")
    |> vibe_logger.with_session_id(config.session_id)
    |> vibe_logger.with_data("agent_id", json.string(agent_id))

  // Initialize agent registry and register this agent
  agent_registry.init()
  let timestamp = get_iso_timestamp()
  agent_registry.register(agent_registry.AgentInfo(
    id: agent_id,
    agent_type: agent_registry.PollingAgent,
    status: agent_registry.Starting,
    started_at: timestamp,
    last_activity: timestamp,
    message_count: 0,
    error_count: 0,
    session_id: Some(config.session_id),
    extra: [],
  ))

  vibe_logger.info(logger, "Polling actor initializing with event bus")

  PollingState(
    config: config,
    agent_state: telegram_agent.init(config),
    poll_count: 0,
    event_bus: Some(bus),
    seen_ids: set.new(),
    agent_id: agent_id,
    logger: logger,
    initial_poll_done: False,
  )
}

/// Запуск polling actor (новый API gleam_otp)
pub fn start(config: telegram_agent.TelegramAgentConfig) -> Result(Subject(PollingMessage), actor.StartError) {
  let initial_state = init_state(config)

  let spec = actor.new(initial_state)
    |> actor.on_message(handle_message)

  case actor.start(spec) {
    Ok(started) -> {
      // Update status to Running
      agent_registry.update_status(initial_state.agent_id, agent_registry.Running)
      vibe_logger.info(initial_state.logger, "Polling actor started")
      Ok(started.data)
    }
    Error(err) -> {
      agent_registry.update_status(initial_state.agent_id, agent_registry.Failed("Failed to start"))
      Error(err)
    }
  }
}

/// Запуск polling actor with shared event bus
pub fn start_with_events(
  config: telegram_agent.TelegramAgentConfig,
  bus: Subject(event_bus.PubSubMessage),
) -> Result(Subject(PollingMessage), actor.StartError) {
  let initial_state = init_state_with_events(config, bus)

  let spec = actor.new(initial_state)
    |> actor.on_message(handle_message)

  case actor.start(spec) {
    Ok(started) -> {
      // Update status to Running
      agent_registry.update_status(initial_state.agent_id, agent_registry.Running)
      vibe_logger.info(initial_state.logger, "Polling actor started with event bus")
      Ok(started.data)
    }
    Error(err) -> {
      agent_registry.update_status(initial_state.agent_id, agent_registry.Failed("Failed to start"))
      Error(err)
    }
  }
}

/// Обработчик сообщений актора
fn handle_message(
  state: PollingState,
  msg: PollingMessage,
) -> actor.Next(PollingState, PollingMessage) {
  case msg {
    Poll -> {
      // Update activity in registry
      agent_registry.update_activity(state.agent_id)

      // Выполняем polling
      let new_state = do_poll(state)

      // Планируем следующий poll через 5 секунд
      let _ = schedule_next_poll()

      actor.continue(new_state)
    }

    Stop -> {
      // Update status to Stopped and unregister
      agent_registry.update_status(state.agent_id, agent_registry.Stopped)
      agent_registry.unregister(state.agent_id)
      vibe_logger.info(state.logger, "Polling actor stopped")
      actor.stop()
    }
  }
}

/// Планирование следующего poll (заглушка - таймер в start_polling)
fn schedule_next_poll() {
  // Таймер запускается в start_polling_loop
  Nil
}

/// Выполнить один цикл polling
fn do_poll(state: PollingState) -> PollingState {
  // Debug log at start of poll cycle
  vibe_logger.info(state.logger, "===== do_poll STARTED =====")

  let poll_num = state.poll_count + 1
  let log = state.logger
    |> vibe_logger.with_data("poll_count", json.int(poll_num))

  // Log on first poll and every 10th
  case poll_num {
    1 -> {
      vibe_logger.info(log, "Telegram polling started")
      vibe_logger.debug(log
        |> vibe_logger.with_data("bridge_url", json.string(state.config.bridge_url))
        |> vibe_logger.with_data("digital_twin", json.bool(state.config.digital_twin_enabled)),
        "Poll configuration")
    }
    _ -> case poll_num % 10 {
      0 -> vibe_logger.debug(log, "Polling heartbeat")
      _ -> Nil
    }
  }

  // Получаем список диалогов
  vibe_logger.trace(log, "Fetching dialogs from bridge")
  case get_dialogs(state.config) {
    Error(err) -> {
      vibe_logger.error(log
        |> vibe_logger.with_data("error", json.string(err)),
        "Failed to get dialogs")
      agent_registry.increment_errors(state.agent_id)
      // Publish error event
      publish_event(state.event_bus, event_bus.error_event(
        "polling_error",
        "Failed to get dialogs from Go bridge",
        get_timestamp(),
      ))
      PollingState(..state, poll_count: poll_num)
    }
    Ok(dialogs_json) -> {
      vibe_logger.trace(log
        |> vibe_logger.with_data("response_length", json.int(string.length(dialogs_json))),
        "Got dialogs response")

      // Log first successful poll
      case poll_num {
        1 -> vibe_logger.info(log, "Connected to Telegram bridge successfully")
        _ -> Nil
      }

      // ВАЖНО: При первом poll только заполняем seen_ids, не обрабатываем сообщения
      // Это предотвращает ответы на старые сообщения после деплоя
      case state.initial_poll_done {
        False -> {
          // Первый poll - только заполнить seen_ids
          vibe_logger.info(log, "Initial poll: populating seen_ids without processing")
          let new_seen_ids = populate_seen_ids(
            dialogs_json,
            state.config,
            state.seen_ids,
            state.agent_id,
            log,
          )
          vibe_logger.info(log
            |> vibe_logger.with_data("seen_count", json.int(set.size(new_seen_ids))),
            "Initial poll complete, ready for new messages")
          PollingState(..state, poll_count: poll_num, seen_ids: new_seen_ids, initial_poll_done: True)
        }
        True -> {
          // Обычный poll - обрабатываем новые сообщения
          let #(new_agent_state, new_seen_ids) = process_dialogs_with_events(
            state.agent_state,
            dialogs_json,
            state.event_bus,
            state.seen_ids,
            state.agent_id,
            log,
          )
          PollingState(..state, agent_state: new_agent_state, poll_count: poll_num, seen_ids: new_seen_ids)
        }
      }
    }
  }
}

/// Helper to publish event if event_bus is available
fn publish_event(
  bus: Option(Subject(event_bus.PubSubMessage)),
  event: event_bus.Event,
) {
  case bus {
    Some(b) -> event_bus.publish(b, event)
    None -> Nil
  }
}

/// Get current Unix timestamp using Erlang's os:system_time/1
@external(erlang, "vibee_ffi", "get_unix_timestamp")
fn get_timestamp() -> Int

/// Get ISO timestamp for agent registry
@external(erlang, "vibee_vibe_logger_ffi", "get_iso_timestamp")
fn get_iso_timestamp() -> String

/// Get VIBEE_API_KEY from environment
@external(erlang, "vibee_polling_ffi", "get_api_key")
fn get_api_key() -> String

/// Получить список диалогов
fn get_dialogs(config: telegram_agent.TelegramAgentConfig) -> Result(String, String) {
  // Парсим bridge_url для определения host/port/scheme
  let #(scheme, host, port) = parse_bridge_url(config.bridge_url)
  let api_key = get_api_key()

  let req = request.new()
    |> request.set_scheme(scheme)
    |> request.set_method(http.Get)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path("/api/v1/dialogs")
    |> request.set_query([#("limit", "50")])  // Увеличено для мониторинга всех групп
    |> request.set_header("X-Session-ID", config.session_id)
    |> request.set_header("Authorization", "Bearer " <> api_key)

  // Use retry logic for resilience
  let retry_config = http_retry.default_config()
  case http_retry.send_with_retry(req, retry_config) {
    Ok(response) -> Ok(response.body)
    Error(_) -> Error("HTTP request failed after retries")
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

/// Получить историю сообщений из чата
fn get_history(config: telegram_agent.TelegramAgentConfig, chat_id: String) -> Result(String, String) {
  let #(scheme, host, port) = parse_bridge_url(config.bridge_url)
  let api_key = get_api_key()

  let req = request.new()
    |> request.set_scheme(scheme)
    |> request.set_method(http.Get)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path("/api/v1/history/" <> chat_id)
    |> request.set_query([#("limit", "5")])
    |> request.set_header("X-Session-ID", config.session_id)
    |> request.set_header("Authorization", "Bearer " <> api_key)

  // Use retry logic for resilience
  let retry_config = http_retry.default_config()
  case http_retry.send_with_retry(req, retry_config) {
    Ok(response) -> {
      // Проверяем на FLOOD_WAIT и другие ошибки
      case string.contains(response.body, "\"error\"") {
        True -> Error(response.body)
        False -> Ok(response.body)
      }
    }
    Error(_) -> Error("HTTP request failed after retries")
  }
}

/// Заполнить seen_ids для всех существующих сообщений БЕЗ обработки
/// Вызывается при первом poll после старта/рестарта актора
/// Предотвращает ответы на старые сообщения после деплоя
fn populate_seen_ids(
  dialogs_json: String,
  config: telegram_agent.TelegramAgentConfig,
  seen_ids: Set(String),
  _agent_id: String,
  logger: vibe_logger.VibeLogger,
) -> Set(String) {
  // Находим все ID чатов
  let group_ids = extract_group_ids(dialogs_json, logger)
  let target_ids = target_chats.target_chats()
  let all_ids = list.unique(list.append(group_ids, target_ids))

  vibe_logger.debug(logger
    |> vibe_logger.with_data("chat_count", json.int(list.length(all_ids))),
    "Populating seen_ids from existing chats")

  // Обходим все чаты и добавляем их сообщения в seen_ids
  list.fold(all_ids, seen_ids, fn(acc_seen, chat_id) {
    case get_history(config, chat_id) {
      Error(_) -> acc_seen
      Ok(history_json) -> {
        let messages = extract_messages(history_json)
        // Добавляем ВСЕ сообщения (входящие и исходящие) в seen_ids
        list.fold(messages, acc_seen, fn(acc, msg) {
          let #(msg_id, _, _, _, _, _, _, _, _, _) = msg
          let unique_id = chat_id <> ":" <> int.to_string(msg_id)
          set.insert(acc, unique_id)
        })
      }
    }
  })
}

/// Обработать список диалогов with event bus и дедупликацией
fn process_dialogs_with_events(
  state: telegram_agent.AgentState,
  dialogs_json: String,
  bus: Option(Subject(event_bus.PubSubMessage)),
  seen_ids: Set(String),
  agent_id: String,
  logger: vibe_logger.VibeLogger,
) -> #(telegram_agent.AgentState, Set(String)) {
  // Находим все ID групп из JSON (диалоги из MTProto)
  let group_ids = extract_group_ids(dialogs_json, logger)

  // ВАЖНО: Добавляем target_chats для ПРИНУДИТЕЛЬНОГО polling
  let target_ids = target_chats.target_chats()

  // Получаем trigger chat IDs для ПРИОРИТИЗАЦИИ
  let trigger_ids = trigger_chats.get_trigger_chat_ids()

  // Объединяем и убираем дубликаты
  let all_ids = list.unique(list.append(group_ids, target_ids))

  // ПРИОРИТИЗАЦИЯ: trigger chats обрабатываются ПЕРВЫМИ!
  // Сначала trigger chats, потом все остальные
  let non_trigger_ids = list.filter(all_ids, fn(id) {
    let normalized = target_chats.normalize_chat_id(id)
    !list.any(trigger_ids, fn(t) {
      target_chats.normalize_chat_id(t) == normalized
    })
  })
  let prioritized_ids = list.append(trigger_ids, non_trigger_ids)

  // Log trigger chats for debugging - ВАЖНО!
  vibe_logger.info(logger
    |> vibe_logger.with_data("trigger_chat_1", json.string(case list.first(trigger_ids) { Ok(id) -> id Error(_) -> "none" }))
    |> vibe_logger.with_data("trigger_count", json.int(list.length(trigger_ids)))
    |> vibe_logger.with_data("total_chats", json.int(list.length(prioritized_ids))),
    "TRIGGER_PRIORITY: Processing trigger chats first")

  vibe_logger.trace(logger
    |> vibe_logger.with_data("dialogs_count", json.int(list.length(group_ids)))
    |> vibe_logger.with_data("target_chats_count", json.int(list.length(target_ids)))
    |> vibe_logger.with_data("trigger_chats_count", json.int(list.length(trigger_ids)))
    |> vibe_logger.with_data("total_unique", json.int(list.length(prioritized_ids))),
    "Processing dialogs (trigger chats first)")

  // Используем ПРИОРИТИЗИРОВАННЫЕ чаты
  let filtered_ids = prioritized_ids

  // DEBUG: Log first 5 chat IDs being processed
  let preview = list.take(filtered_ids, 5)
  io.println("[POLL_DEBUG] Processing " <> int.to_string(list.length(filtered_ids)) <> " chats, first 5: " <> string.join(preview, ", "))

  case list.length(filtered_ids) {
    0 -> {
      vibe_logger.warn(logger
        |> vibe_logger.with_data("digital_twin", json.bool(state.config.digital_twin_enabled)),
        "No chats to process")
    }
    n -> {
      let preview = list.take(filtered_ids, 5)
      vibe_logger.debug(logger
        |> vibe_logger.with_data("chat_count", json.int(n))
        |> vibe_logger.with_data("preview", json.string(string.join(preview, ", "))),
        "Processing chats")
    }
  }

  // Обрабатываем только отфильтрованные чаты
  list.fold(filtered_ids, #(state, seen_ids), fn(acc, group_id) {
    let #(acc_state, acc_seen) = acc

    // Получаем историю для этого чата
    process_chat_messages_with_events(acc_state, group_id, bus, acc_seen, agent_id, logger)
  })
}

/// Логировать сообщения из чата with event bus и дедупликацией
fn log_chat_messages_with_events(
  config: telegram_agent.TelegramAgentConfig,
  chat_id: String,
  bus: Option(Subject(event_bus.PubSubMessage)),
  seen_ids: Set(String),
) -> Set(String) {
  case get_history(config, chat_id) {
    Error(err) -> {
      // Логируем ошибку только для целевых чатов (чтобы не спамить)
      case target_chats.should_process_chat(chat_id) {
        True -> {
          let log = vibe_logger.new("polling")
          vibe_logger.trace(log
            |> vibe_logger.with_data("chat_id", json.string(chat_id))
            |> vibe_logger.with_data("error", json.string(err)),
            "History error")
        }
        False -> Nil
      }
      seen_ids
    }
    Ok(history_json) -> {
      // Парсим сообщения
      let messages = extract_messages(history_json)

      // Фильтруем и логируем только новые ВХОДЯЩИЕ сообщения
      list.fold(messages, seen_ids, fn(acc_seen, msg) {
        let #(msg_id, _from_id, from_name, _username, text, _phone, _lang_code, _is_premium, is_outgoing, _reply_to_id) = msg
        let unique_id = chat_id <> ":" <> int.to_string(msg_id)

        // Пропускаем исходящие сообщения (от нас самих)
        case is_outgoing {
          True -> acc_seen  // Исходящее - пропускаем
          False -> {
            // Проверяем, видели ли мы это сообщение
            case set.contains(acc_seen, unique_id) {
              True -> acc_seen  // Уже видели - пропускаем
              False -> {
                // Новое входящее сообщение - логируем и публикуем
                logging.quick_info("TG: " <> chat_id <> " " <> from_name <> ": " <> text)

                publish_event(bus, event_bus.telegram_message(
                  chat_id,
                  msg_id,
                  from_name,
                  text,
                  get_timestamp(),
                ))

                // Добавляем в seen_ids
                set.insert(acc_seen, unique_id)
              }
            }
          }
        }
      })
    }
  }
}

/// Извлечь ID чатов из JSON ответа (группы И личные чаты)
fn extract_group_ids(json_str: String, logger: vibe_logger.VibeLogger) -> List(String) {
  // Простой парсинг - ищем "id": числа
  let parts = string.split(json_str, "\"id\":")

  let ids = list.filter_map(parts, fn(part) {
    case string.split(part, ",") {
      [first, ..] -> {
        let cleaned = string.trim(first)
        // Включаем как отрицательные (группы) так и положительные (личные чаты) ID
        case string.starts_with(cleaned, "-") || is_digit_string(cleaned) {
          True -> Ok(cleaned)
          False -> Error(Nil)
        }
      }
      [] -> Error(Nil)
    }
  })

  vibe_logger.trace(logger
    |> vibe_logger.with_data("total_ids", json.int(list.length(ids))),
    "Extracted chat IDs")
  ids
}

/// Проверить, является ли строка числом
fn is_digit_string(s: String) -> Bool {
  case int.parse(s) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Обработать сообщения из чата with event bus и дедупликацией
fn process_chat_messages_with_events(
  state: telegram_agent.AgentState,
  chat_id: String,
  bus: Option(Subject(event_bus.PubSubMessage)),
  seen_ids: Set(String),
  agent_id: String,
  logger: vibe_logger.VibeLogger,
) -> #(telegram_agent.AgentState, Set(String)) {
  let chat_log = logger
    |> vibe_logger.with_data("chat_id", json.string(chat_id))

  // Log if this is a trigger chat for debugging
  let is_trigger = case trigger_chats.find_chat_config(chat_id) {
    Ok(_) -> {
      vibe_logger.info(chat_log, "Processing TRIGGER chat")
      True
    }
    Error(_) -> False
  }

  case get_history(state.config, chat_id) {
    Error(err) -> {
      // Log at INFO level for trigger chats, TRACE for others
      case is_trigger {
        True -> vibe_logger.info(chat_log
            |> vibe_logger.with_data("error", json.string(err)),
            "History ERROR for TRIGGER chat")
        False -> vibe_logger.trace(chat_log
          |> vibe_logger.with_data("error", json.string(err)),
          "History error")
      }
      #(state, seen_ids)
    }
    Ok(history_json) -> {
      // Парсим сообщения
      let messages = extract_messages(history_json)

      // ВАЖНО: Сортируем по msg_id от меньшего к большему
      // чтобы старые сообщения обрабатывались первыми
      let sorted_messages = list.sort(messages, fn(a, b) {
        let #(id_a, _, _, _, _, _, _, _, _, _) = a
        let #(id_b, _, _, _, _, _, _, _, _, _) = b
        int.compare(id_a, id_b)
      })

      // For trigger chats - log at INFO level for debugging
      case is_trigger {
        True -> vibe_logger.info(chat_log
          |> vibe_logger.with_data("message_count", json.int(list.length(sorted_messages))),
          "TRIGGER: Got messages from history")
        False -> vibe_logger.trace(chat_log
          |> vibe_logger.with_data("message_count", json.int(list.length(sorted_messages))),
          "Got messages (sorted by msg_id)")
      }

      // Обрабатываем каждое ВХОДЯЩЕЕ сообщение с дедупликацией
      list.fold(sorted_messages, #(state, seen_ids), fn(acc, msg) {
        let #(acc_state, acc_seen) = acc
        let #(msg_id, from_id, from_name, username, text, phone, lang_code, is_premium, is_outgoing, reply_to_id) = msg
        let unique_id = chat_id <> ":" <> int.to_string(msg_id)

        // Пропускаем исходящие сообщения (от нас самих)
        case is_outgoing {
          True -> {
            // Исходящее сообщение - добавляем в seen но не обрабатываем
            case is_trigger {
              True -> vibe_logger.info(chat_log
                |> vibe_logger.with_data("msg_id", json.int(msg_id))
                |> vibe_logger.with_data("from_name", json.string(from_name)),
                "TRIGGER: Skipping OUTGOING message")
              False -> Nil
            }
            #(acc_state, set.insert(acc_seen, unique_id))
          }
          False -> {
            // Проверяем, видели ли мы это сообщение
            case set.contains(acc_seen, unique_id) {
              True -> {
                // Уже обработали - пропускаем
                case is_trigger {
                  True -> vibe_logger.info(chat_log
                    |> vibe_logger.with_data("msg_id", json.int(msg_id)),
                    "TRIGGER: Skipping SEEN message")
                  False -> Nil
                }
                acc
              }
              False -> {
                // Новое входящее сообщение
                let msg_log = chat_log
                  |> vibe_logger.with_data("msg_id", json.int(msg_id))
                  |> vibe_logger.with_data("from_id", json.int(from_id))
                  |> vibe_logger.with_data("from_name", json.string(from_name))
                  |> vibe_logger.with_data("username", json.string(username))
                  |> vibe_logger.with_data("reply_to_id", json.int(reply_to_id))

                // Логируем как REPLY если это ответ на сообщение
                case reply_to_id > 0 {
                  True -> vibe_logger.info(msg_log, "New REPLY message")
                  False -> vibe_logger.info(msg_log, "New incoming message")
                }
                vibe_logger.debug(msg_log
                  |> vibe_logger.with_data("text_preview", json.string(string.slice(text, 0, 100))),
                  "Message content")

                // Increment message count in registry
                agent_registry.increment_messages(agent_id)

                // Publish telegram message event
                publish_event(bus, event_bus.telegram_message(
                  chat_id,
                  msg_id,
                  from_name,
                  text,
                  get_timestamp(),
                ))

                // Обрабатываем через telegram_agent
                let new_state = telegram_agent.handle_incoming_message(
                  acc_state,
                  chat_id,
                  from_id,
                  from_name,
                  username,
                  phone,
                  lang_code,
                  is_premium,
                  text,
                  msg_id,
                  reply_to_id,
                )

                // Сохраняем входящее сообщение в БД для RAG памяти
                let dialog_id = case int.parse(chat_id) {
                  Ok(id) -> id
                  Error(_) -> 0
                }
                case postgres.insert_message_simple(dialog_id, msg_id, from_id, from_name, text) {
                  Ok(_) -> vibe_logger.trace(msg_log, "Message saved to DB")
                  Error(e) -> {
                    vibe_logger.error(msg_log
                      |> vibe_logger.with_data("error", json.string(e)),
                      "Failed to save message to DB")
                    agent_registry.increment_errors(agent_id)
                  }
                }

                // Check if agent replied
                case new_state.total_messages > acc_state.total_messages {
                  True -> {
                    publish_event(bus, event_bus.trigger_detected(
                      chat_id,
                      "trigger_found",
                      get_timestamp(),
                    ))
                  }
                  False -> Nil
                }

                // Возвращаем обновлённое состояние и seen_ids
                #(new_state, set.insert(acc_seen, unique_id))
              }
            }
          }
        }
      })
    }
  }
}

/// Извлечь сообщения из JSON ответа
/// Формат: {"messages":[{"id":123,"text":"...","from_name":"...","from_id":123,"username":"...","phone":"...","lang_code":"ru","premium":true,"out":true,"reply_to_id":456},...]}
/// Returns: List(#(msg_id, from_id, from_name, username, text, phone, lang_code, is_premium, is_outgoing, reply_to_id))
fn extract_messages(json: String) -> List(#(Int, Int, String, String, String, String, String, Bool, Bool, Int)) {
  // Разбиваем по объектам сообщений
  let message_parts = string.split(json, "{\"id\":")

  list.filter_map(list.drop(message_parts, 1), fn(part) {
    // Парсим каждый объект сообщения
    parse_message_object(part)
  })
}

/// Парсит один объект сообщения из строки
fn parse_message_object(part: String) -> Result(#(Int, Int, String, String, String, String, String, Bool, Bool, Int), Nil) {
  // Извлекаем id (первое число до запятой)
  let id = case string.split(part, ",") {
    [id_str, ..] -> {
      case int.parse(string.trim(id_str)) {
        Ok(n) -> n
        Error(_) -> 0
      }
    }
    _ -> 0
  }

  // Извлекаем text
  let text = extract_json_field(part, "text")

  // Извлекаем from_id
  let from_id = extract_json_int_field(part, "from_id")

  // Извлекаем from_name (fallback на "User" если пустое)
  let from_name = case extract_json_field(part, "from_name") {
    "" -> "User"
    name -> name
  }

  // Извлекаем username
  let username = extract_json_field(part, "username")

  // Log extracted user data
  let user_log = vibe_logger.new("poll")
    |> vibe_logger.with_data("username", json.string(username))
    |> vibe_logger.with_data("from_name", json.string(from_name))
    |> vibe_logger.with_data("from_id", json.int(from_id))
  vibe_logger.debug(user_log, "Extracted user data")

  // Извлекаем phone (номер телефона если доступен)
  let phone = extract_json_field(part, "phone")

  // Извлекаем lang_code (язык пользователя: ru, en, etc)
  let lang_code = extract_json_field(part, "lang_code")

  // Извлекаем premium (Telegram Premium статус)
  let is_premium = extract_json_bool_field(part, "premium")

  // Извлекаем out (исходящее сообщение)
  let is_outgoing = extract_json_bool_field(part, "out")

  // Извлекаем reply_to_id (ID сообщения на которое отвечают)
  let reply_to_id = extract_json_int_field(part, "reply_to_id")

  // Пропускаем только сообщения с пустым текстом (медиа-сообщения)
  case text {
    "" -> Error(Nil)
    _ -> Ok(#(id, from_id, from_name, username, text, phone, lang_code, is_premium, is_outgoing, reply_to_id))
  }
}

/// Извлекает integer поле из JSON строки
fn extract_json_int_field(json: String, field: String) -> Int {
  // Ищем "field":число
  let pattern = "\"" <> field <> "\":"
  case string.split(json, pattern) {
    [_, rest, ..] -> {
      // Берём число до запятой или }
      let num_str = rest
        |> string.split(",")
        |> list.first
        |> fn(r) { case r { Ok(s) -> s _ -> "0" } }
        |> string.split("}")
        |> list.first
        |> fn(r) { case r { Ok(s) -> s _ -> "0" } }
        |> string.trim
      case int.parse(num_str) {
        Ok(n) -> n
        Error(_) -> 0
      }
    }
    _ -> 0
  }
}

/// Извлекает значение поля из JSON строки
fn extract_json_field(json: String, field: String) -> String {
  // Ищем "field":"value"
  let pattern = "\"" <> field <> "\":\""
  case string.split(json, pattern) {
    [_, rest, ..] -> {
      // Берём всё до закрывающей кавычки
      case string.split(rest, "\"") {
        [value, ..] -> value
        _ -> ""
      }
    }
    _ -> ""
  }
}

/// Извлекает boolean поле из JSON строки
fn extract_json_bool_field(json: String, field: String) -> Bool {
  // Ищем "field":true или "field":false
  let pattern = "\"" <> field <> "\":"
  case string.split(json, pattern) {
    [_, rest, ..] -> {
      let trimmed = string.trim(rest)
      string.starts_with(trimmed, "true")
    }
    _ -> False
  }
}

/// Запустить polling loop (бесконечный цикл в отдельном процессе)
pub fn start_polling(subject: Subject(PollingMessage)) {
  let log = vibe_logger.new("polling")
  vibe_logger.info(log, "Starting polling loop")

  // Запускаем polling loop в отдельном linked-процессе
  let _ = process.spawn(fn() {
    polling_loop(subject)
  })
  Nil
}

/// Бесконечный цикл polling с защитой от crash
fn polling_loop(subject: Subject(PollingMessage)) {
  // Логируем каждый tick
  // io.println("[POLL LOOP] Sending Poll message...")

  // Отправляем Poll message - защищаем от crash
  let _ = safe_send(subject)

  // Ждём 5 секунд
  process.sleep(5000)

  // Рекурсивный вызов
  polling_loop(subject)
}

/// Безопасная отправка с trap
fn safe_send(subject: Subject(PollingMessage)) {
  // Просто отправляем - если subject мёртв, это не вызовет crash
  process.send(subject, Poll)
}

/// Остановить polling
pub fn stop_polling(subject: Subject(PollingMessage)) {
  process.send(subject, Stop)
}
