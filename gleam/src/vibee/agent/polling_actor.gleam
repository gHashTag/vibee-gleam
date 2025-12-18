// VIBEE Polling Actor
// OTP Actor для polling сообщений из Telegram через Go Bridge
// Аналог TelegramService.poll() из plugin-telegram-craft

import gleam/erlang/process.{type Subject}
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/set.{type Set}
import gleam/string
import vibee/config/target_chats
import vibee/db/postgres
import vibee/events/event_bus
import vibee/http_retry
import vibee/logging
import vibee/telegram/telegram_agent

/// Состояние Polling актора
pub type PollingState {
  PollingState(
    config: telegram_agent.TelegramAgentConfig,
    agent_state: telegram_agent.AgentState,
    poll_count: Int,
    event_bus: Option(Subject(event_bus.PubSubMessage)),
    seen_ids: Set(String),  // ID уже обработанных сообщений (chat_id:msg_id)
  )
}

/// Сообщения для Polling Actor
pub type PollingMessage {
  Poll
  Stop
}

/// Создать начальное состояние
fn init_state(config: telegram_agent.TelegramAgentConfig) -> PollingState {
  PollingState(
    config: config,
    agent_state: telegram_agent.init(config),
    poll_count: 0,
    event_bus: None,
    seen_ids: set.new(),
  )
}

/// Создать начальное состояние с event bus
fn init_state_with_events(
  config: telegram_agent.TelegramAgentConfig,
  bus: Subject(event_bus.PubSubMessage),
) -> PollingState {
  PollingState(
    config: config,
    agent_state: telegram_agent.init(config),
    poll_count: 0,
    event_bus: Some(bus),
    seen_ids: set.new(),
  )
}

/// Запуск polling actor (новый API gleam_otp)
pub fn start(config: telegram_agent.TelegramAgentConfig) -> Result(Subject(PollingMessage), actor.StartError) {
  let initial_state = init_state(config)

  // Новый API: actor.new() |> actor.on_message() |> actor.start
  let spec = actor.new(initial_state)
    |> actor.on_message(handle_message)

  case actor.start(spec) {
    Ok(started) -> Ok(started.data)
    Error(err) -> Error(err)
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
    Ok(started) -> Ok(started.data)
    Error(err) -> Error(err)
  }
}

/// Обработчик сообщений актора
fn handle_message(
  state: PollingState,
  msg: PollingMessage,
) -> actor.Next(PollingState, PollingMessage) {
  case msg {
    Poll -> {
      // Выполняем polling
      let new_state = do_poll(state)

      // Планируем следующий poll через 5 секунд
      let _ = schedule_next_poll()

      actor.continue(new_state)
    }

    Stop -> {
      logging.quick_info("Polling Actor stopped")
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
  let poll_num = state.poll_count + 1

  // UNCONDITIONAL debug log to verify deployment
  io.println("[POLL#" <> int.to_string(poll_num) <> "] RAG_DEBUG_V3 twin=" <> case state.config.digital_twin_enabled { True -> "Y" False -> "N" })

  // Логируем первый poll, каждый 10-й, и любой с ошибками
  case poll_num {
    1 -> {
      io.println("[POLL] === RAG DEBUG v2 === Starting polling from: " <> state.config.bridge_url)
      io.println("[POLL] Digital Twin enabled: " <> case state.config.digital_twin_enabled { True -> "YES" False -> "NO" })
    }
    _ -> case poll_num % 10 {
      0 -> io.println("[POLL] Poll #" <> int.to_string(poll_num) <> " alive")
      _ -> Nil
    }
  }

  // Получаем список диалогов
  case get_dialogs(state.config) {
    Error(err) -> {
      io.println("[POLL ERROR] Failed to get dialogs: " <> err)
      // Publish error event
      publish_event(state.event_bus, event_bus.error_event(
        "polling_error",
        "Failed to get dialogs from Go bridge",
        get_timestamp(),
      ))
      PollingState(..state, poll_count: poll_num)
    }
    Ok(dialogs_json) -> {
      // Логируем первый успешный poll
      case poll_num {
        1 -> {
          io.println("[POLL] Got dialogs response, length: " <> int.to_string(string.length(dialogs_json)))
          io.println("[POLL] Response: " <> dialogs_json)
        }
        _ -> Nil
      }
      // Парсим и обрабатываем диалоги, передаём seen_ids для дедупликации
      let #(new_agent_state, new_seen_ids) = process_dialogs_with_events(
        state.agent_state,
        dialogs_json,
        state.event_bus,
        state.seen_ids,
      )
      PollingState(..state, agent_state: new_agent_state, poll_count: poll_num, seen_ids: new_seen_ids)
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

/// Обработать список диалогов with event bus и дедупликацией
fn process_dialogs_with_events(
  state: telegram_agent.AgentState,
  dialogs_json: String,
  bus: Option(Subject(event_bus.PubSubMessage)),
  seen_ids: Set(String),
) -> #(telegram_agent.AgentState, Set(String)) {
  // Находим все ID групп из JSON
  let group_ids = extract_group_ids(dialogs_json)

  // Фильтруем через should_process_chat_with_mode который учитывает:
  // - целевые чаты (всегда)
  // - личные чаты (только в Digital Twin режиме)
  // - исключает self-chat (owner_id)
  let digital_twin_enabled = state.config.digital_twin_enabled
  let filtered_ids = list.filter(group_ids, fn(id) {
    target_chats.should_process_chat_with_mode(id, digital_twin_enabled)
  })

  // Логируем количество отфильтрованных чатов
  case list.length(filtered_ids) {
    0 -> io.println("[POLL] No chats to process (Digital Twin: " <> case digital_twin_enabled { True -> "ON" False -> "OFF" } <> ")")
    n -> {
      io.println("[POLL] Processing " <> int.to_string(n) <> " chats (Digital Twin: " <> case digital_twin_enabled { True -> "ON" False -> "OFF" } <> ")")
      // Показываем первые 5 чатов для отладки
      let preview = list.take(filtered_ids, 5)
      io.println("[POLL] Chats: " <> string.join(preview, ", "))
    }
  }

  // Обрабатываем только отфильтрованные чаты
  list.fold(filtered_ids, #(state, seen_ids), fn(acc, group_id) {
    let #(acc_state, acc_seen) = acc

    // Логируем какой чат обрабатываем
    io.println("[POLL] Processing chat: " <> group_id)

    // Получаем историю для этого чата
    process_chat_messages_with_events(acc_state, group_id, bus, acc_seen)
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
        True -> io.println("[POLL] History error for " <> chat_id <> ": " <> err)
        False -> Nil
      }
      seen_ids
    }
    Ok(history_json) -> {
      // Парсим сообщения
      let messages = extract_messages(history_json)

      // Фильтруем и логируем только новые ВХОДЯЩИЕ сообщения
      list.fold(messages, seen_ids, fn(acc_seen, msg) {
        let #(msg_id, _from_id, from_name, text, is_outgoing) = msg
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

/// Извлечь ID групп из JSON ответа
fn extract_group_ids(json: String) -> List(String) {
  // Простой парсинг - ищем "id": числа
  let parts = string.split(json, "\"id\":")

  list.filter_map(parts, fn(part) {
    case string.split(part, ",") {
      [first, ..] -> {
        let cleaned = string.trim(first)
        case string.starts_with(cleaned, "-") || is_digit_string(cleaned) {
          True -> Ok(cleaned)
          False -> Error(Nil)
        }
      }
      [] -> Error(Nil)
    }
  })
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
) -> #(telegram_agent.AgentState, Set(String)) {
  case get_history(state.config, chat_id) {
    Error(err) -> {
      io.println("[POLL] History error for " <> chat_id <> ": " <> err)
      #(state, seen_ids)
    }
    Ok(history_json) -> {
      // Парсим сообщения
      let messages = extract_messages(history_json)
      io.println("[POLL] Got " <> int.to_string(list.length(messages)) <> " messages from " <> chat_id)

      // Обрабатываем каждое ВХОДЯЩЕЕ сообщение с дедупликацией
      list.fold(messages, #(state, seen_ids), fn(acc, msg) {
        let #(acc_state, acc_seen) = acc
        let #(msg_id, from_id, from_name, text, is_outgoing) = msg
        let unique_id = chat_id <> ":" <> int.to_string(msg_id)

        // Логируем каждое сообщение для диагностики
        let out_str = case is_outgoing { True -> "OUT" False -> "IN" }
        io.println("[POLL] Msg " <> unique_id <> " " <> out_str <> " from:" <> int.to_string(from_id) <> " " <> string.slice(text, 0, 25))

        // Пропускаем исходящие сообщения (от нас самих) - предотвращает самообщение!
        case is_outgoing {
          True -> {
            // Исходящее сообщение - добавляем в seen но не обрабатываем
            #(acc_state, set.insert(acc_seen, unique_id))
          }
          False -> {
            // Проверяем, видели ли мы это сообщение
            case set.contains(acc_seen, unique_id) {
              True -> {
                io.println("[POLL] SKIP (seen): " <> unique_id)
                acc  // Уже обработали - пропускаем
              }
              False -> {
                io.println("[POLL] NEW INCOMING: " <> unique_id <> " from:" <> from_name)
                // Логируем входящее сообщение
                logging.quick_info("TG: " <> chat_id <> " " <> from_name <> ": " <> text)

                // Publish telegram message event
                publish_event(bus, event_bus.telegram_message(
                  chat_id,
                  msg_id,
                  from_name,
                  text,
                  get_timestamp(),
                ))

                // Обрабатываем через telegram_agent и публикуем события
                let new_state = telegram_agent.handle_incoming_message(
                  acc_state,
                  chat_id,
                  from_id,
                  from_name,
                  text,
                  msg_id,
                )

                // Сохраняем входящее сообщение в БД для RAG памяти
                let dialog_id = case int.parse(chat_id) {
                  Ok(id) -> id
                  Error(_) -> 0
                }
                io.println("[DB] Saving incoming: dialog=" <> int.to_string(dialog_id) <> " msg=" <> int.to_string(msg_id) <> " from=" <> from_name)
                case postgres.insert_message_simple(dialog_id, msg_id, from_id, from_name, text) {
                  Ok(_) -> io.println("[DB] Incoming saved OK")
                  Error(e) -> io.println("[DB] ERROR saving incoming: " <> e)
                }

                // Check if agent replied (state changed - reply was sent)
                case new_state.total_messages > acc_state.total_messages {
                  True -> {
                    // Agent processed and possibly replied - publish trigger event
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
/// Формат: {"messages":[{"id":123,"text":"...","from_name":"...","from_id":123,"out":true},...]}
/// Returns: List(#(msg_id, from_id, from_name, text, is_outgoing))
fn extract_messages(json: String) -> List(#(Int, Int, String, String, Bool)) {
  // Разбиваем по объектам сообщений
  let message_parts = string.split(json, "{\"id\":")

  list.filter_map(list.drop(message_parts, 1), fn(part) {
    // Парсим каждый объект сообщения
    parse_message_object(part)
  })
}

/// Парсит один объект сообщения из строки
fn parse_message_object(part: String) -> Result(#(Int, Int, String, String, Bool), Nil) {
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

  // Извлекаем out (исходящее сообщение)
  let is_outgoing = extract_json_bool_field(part, "out")

  // Пропускаем только сообщения с пустым текстом (медиа-сообщения)
  case text {
    "" -> Error(Nil)
    _ -> Ok(#(id, from_id, from_name, text, is_outgoing))
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
  logging.quick_info("Starting polling loop...")

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
