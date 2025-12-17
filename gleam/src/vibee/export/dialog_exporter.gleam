// VIBEE Dialog Exporter - Скачивание ВСЕХ диалогов из Telegram на Gleam
// Экспортирует полную историю сообщений с пагинацией

import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile
import vibee/logging

/// Конфигурация экспорта
pub type ExportConfig {
  ExportConfig(
    bridge_url: String,
    session_id: String,
    output_dir: String,
    batch_size: Int,
  )
}

/// Диалог
pub type Dialog {
  Dialog(
    id: String,
    title: String,
    dialog_type: String,
    unread_count: Int,
  )
}

/// Сообщение
pub type Message {
  Message(
    id: Int,
    text: String,
    from_name: String,
    date: String,
  )
}

/// Результат экспорта
pub type ExportResult {
  ExportResult(
    dialog_count: Int,
    message_count: Int,
    files_created: List(String),
  )
}

/// Создать конфиг по умолчанию
pub fn default_config() -> ExportConfig {
  ExportConfig(
    bridge_url: "http://localhost:8081",
    session_id: "sess_desvr94fm7k0",
    output_dir: "/Users/playra/vibee-eliza-999/vibee/gleam/data/dialogs",
    batch_size: 100,
  )
}

/// Получить список всех диалогов
pub fn get_all_dialogs(config: ExportConfig) -> Result(List(Dialog), String) {
  let req = request.new()
    |> request.set_scheme(http.Http)
    |> request.set_method(http.Get)
    |> request.set_host("localhost")
    |> request.set_port(8081)
    |> request.set_path("/api/v1/dialogs")
    |> request.set_query([#("limit", "500")])
    |> request.set_header("X-Session-ID", config.session_id)

  case httpc.send(req) {
    Ok(response) -> {
      let dialogs = parse_dialogs(response.body)
      Ok(dialogs)
    }
    Error(_) -> Error("Failed to fetch dialogs")
  }
}

/// Получить историю сообщений с пагинацией
pub fn get_full_history(
  config: ExportConfig,
  chat_id: String,
) -> List(Message) {
  get_history_recursive(config, chat_id, 0, [])
}

/// Рекурсивное скачивание истории с offset
fn get_history_recursive(
  config: ExportConfig,
  chat_id: String,
  offset_id: Int,
  acc: List(Message),
) -> List(Message) {
  let messages = get_history_page(config, chat_id, offset_id)

  case messages {
    [] -> acc
    msgs -> {
      let new_acc = list.append(acc, msgs)

      // Получаем минимальный ID для следующей страницы
      let min_id = list.fold(msgs, 999_999_999, fn(min, msg) {
        case msg.id < min {
          True -> msg.id
          False -> min
        }
      })

      // Продолжаем пока min_id меняется (есть более старые сообщения)
      case min_id == offset_id || min_id == 0 {
        True -> new_acc
        False -> {
          // Небольшая пауза чтобы не спамить API
          sleep_ms(300)
          get_history_recursive(config, chat_id, min_id, new_acc)
        }
      }
    }
  }
}

/// Получить одну страницу истории
fn get_history_page(
  config: ExportConfig,
  chat_id: String,
  offset_id: Int,
) -> List(Message) {
  let query = case offset_id > 0 {
    True -> [
      #("limit", int.to_string(config.batch_size)),
      #("offset_id", int.to_string(offset_id)),
    ]
    False -> [#("limit", int.to_string(config.batch_size))]
  }

  let req = request.new()
    |> request.set_scheme(http.Http)
    |> request.set_method(http.Get)
    |> request.set_host("localhost")
    |> request.set_port(8081)
    |> request.set_path("/api/v1/history/" <> chat_id)
    |> request.set_query(query)
    |> request.set_header("X-Session-ID", config.session_id)

  case httpc.send(req) {
    Ok(response) -> parse_messages(response.body)
    Error(_) -> []
  }
}

/// Экспортировать ВСЕ диалоги с юзерами
pub fn export_all_user_dialogs(config: ExportConfig) -> ExportResult {
  logging.info("Starting full dialog export...")

  // Создаём директорию
  let _ = simplifile.create_directory_all(config.output_dir)

  // Получаем все диалоги
  let dialogs = case get_all_dialogs(config) {
    Ok(d) -> d
    Error(_) -> []
  }

  // Фильтруем только user диалоги
  let user_dialogs = list.filter(dialogs, fn(d) {
    d.dialog_type == "user"
  })

  logging.info("Found " <> int.to_string(list.length(user_dialogs)) <> " user dialogs")

  // Экспортируем каждый диалог
  let results = list.index_map(user_dialogs, fn(dialog, idx) {
    let progress = "[" <> int.to_string(idx + 1) <> "/" <> int.to_string(list.length(user_dialogs)) <> "]"
    logging.info(progress <> " Exporting: " <> dialog.title)

    // Скачиваем полную историю
    let messages = get_full_history(config, dialog.id)
    logging.info("  Downloaded " <> int.to_string(list.length(messages)) <> " messages")

    // Сохраняем в файл
    let filename = config.output_dir <> "/" <> dialog.id <> ".json"
    let json_content = dialog_to_json(dialog, messages)
    let _ = simplifile.write(filename, json_content)

    #(dialog.id, list.length(messages), filename)
  })

  let total_messages = list.fold(results, 0, fn(sum, r) {
    let #(_, count, _) = r
    sum + count
  })

  let files = list.map(results, fn(r) {
    let #(_, _, f) = r
    f
  })

  logging.info("Export complete!")
  logging.info("  Dialogs: " <> int.to_string(list.length(results)))
  logging.info("  Messages: " <> int.to_string(total_messages))

  ExportResult(
    dialog_count: list.length(results),
    message_count: total_messages,
    files_created: files,
  )
}

/// Экспортировать ВСЕ диалоги (включая группы и каналы)
pub fn export_all_dialogs(config: ExportConfig) -> ExportResult {
  logging.info("Starting FULL export (all dialogs)...")

  let _ = simplifile.create_directory_all(config.output_dir)

  let dialogs = case get_all_dialogs(config) {
    Ok(d) -> d
    Error(_) -> []
  }

  logging.info("Found " <> int.to_string(list.length(dialogs)) <> " total dialogs")

  let results = list.index_map(dialogs, fn(dialog, idx) {
    let progress = "[" <> int.to_string(idx + 1) <> "/" <> int.to_string(list.length(dialogs)) <> "]"
    logging.info(progress <> " " <> dialog.title <> " (" <> dialog.dialog_type <> ")")

    // Пропускаем каналы с большим количеством непрочитанных (спам)
    case dialog.unread_count > 5000 {
      True -> {
        logging.info("  [SKIP] Too many unread: " <> int.to_string(dialog.unread_count))
        #(dialog.id, 0, "")
      }
      False -> {
        let messages = get_full_history(config, dialog.id)
        logging.info("  Downloaded " <> int.to_string(list.length(messages)) <> " messages")

        let filename = config.output_dir <> "/" <> dialog.id <> ".json"
        let json_content = dialog_to_json(dialog, messages)
        let _ = simplifile.write(filename, json_content)

        sleep_ms(500)  // Пауза между диалогами

        #(dialog.id, list.length(messages), filename)
      }
    }
  })

  let total_messages = list.fold(results, 0, fn(sum, r) {
    let #(_, count, _) = r
    sum + count
  })

  let files = list.filter_map(results, fn(r) {
    let #(_, _, f) = r
    case f {
      "" -> Error(Nil)
      _ -> Ok(f)
    }
  })

  logging.info("Export complete!")
  logging.info("  Dialogs: " <> int.to_string(list.length(files)))
  logging.info("  Messages: " <> int.to_string(total_messages))

  ExportResult(
    dialog_count: list.length(files),
    message_count: total_messages,
    files_created: files,
  )
}

/// Парсинг диалогов из JSON
fn parse_dialogs(json_str: String) -> List(Dialog) {
  // Простой парсинг - ищем объекты диалогов
  let parts = string.split(json_str, "{\"id\":")

  list.filter_map(list.drop(parts, 1), fn(part) {
    parse_dialog_object(part)
  })
}

fn parse_dialog_object(part: String) -> Result(Dialog, Nil) {
  // Извлекаем id
  let id = case string.split(part, ",") {
    [id_str, ..] -> string.trim(id_str)
    _ -> ""
  }

  // Извлекаем title
  let title = extract_json_string(part, "title")

  // Извлекаем type
  let dialog_type = extract_json_string(part, "type")

  // Извлекаем unread_count
  let unread = case string.split(part, "\"unread_count\":") {
    [_, rest, ..] -> {
      case string.split(rest, "}") {
        [num_str, ..] -> {
          case int.parse(string.trim(num_str)) {
            Ok(n) -> n
            Error(_) -> 0
          }
        }
        _ -> 0
      }
    }
    _ -> 0
  }

  case id, title {
    "", _ -> Error(Nil)
    _, "" -> Error(Nil)
    _, _ -> Ok(Dialog(id: id, title: title, dialog_type: dialog_type, unread_count: unread))
  }
}

/// Парсинг сообщений из JSON
fn parse_messages(json_str: String) -> List(Message) {
  let parts = string.split(json_str, "{\"id\":")

  list.filter_map(list.drop(parts, 1), fn(part) {
    parse_message_object(part)
  })
}

fn parse_message_object(part: String) -> Result(Message, Nil) {
  // Извлекаем id
  let id = case string.split(part, ",") {
    [id_str, ..] -> {
      case int.parse(string.trim(id_str)) {
        Ok(n) -> n
        Error(_) -> 0
      }
    }
    _ -> 0
  }

  let text = extract_json_string(part, "text")
  let from_name = case extract_json_string(part, "from_name") {
    "" -> "User"
    name -> name
  }
  let date = extract_json_string(part, "date")

  case id, text {
    0, _ -> Error(Nil)
    _, "" -> Error(Nil)
    _, _ -> Ok(Message(id: id, text: text, from_name: from_name, date: date))
  }
}

/// Извлечь строковое поле из JSON
fn extract_json_string(json: String, field: String) -> String {
  let pattern = "\"" <> field <> "\":\""
  case string.split(json, pattern) {
    [_, rest, ..] -> {
      // Ищем закрывающую кавычку, учитывая escaped quotes
      extract_until_quote(rest, "")
    }
    _ -> ""
  }
}

fn extract_until_quote(s: String, acc: String) -> String {
  case string.pop_grapheme(s) {
    Error(_) -> acc
    Ok(#("\"", _)) -> acc
    Ok(#("\\", rest)) -> {
      // Escaped character - берём следующий символ
      case string.pop_grapheme(rest) {
        Ok(#(c, remaining)) -> extract_until_quote(remaining, acc <> c)
        Error(_) -> acc
      }
    }
    Ok(#(c, rest)) -> extract_until_quote(rest, acc <> c)
  }
}

/// Конвертация диалога в JSON
fn dialog_to_json(dialog: Dialog, messages: List(Message)) -> String {
  let messages_json = list.map(messages, fn(msg) {
    json.object([
      #("id", json.int(msg.id)),
      #("text", json.string(msg.text)),
      #("from_name", json.string(msg.from_name)),
      #("date", json.string(msg.date)),
    ])
  })

  json.object([
    #("chat_id", json.string(dialog.id)),
    #("title", json.string(dialog.title)),
    #("type", json.string(dialog.dialog_type)),
    #("message_count", json.int(list.length(messages))),
    #("messages", json.array(messages_json, fn(x) { x })),
  ])
  |> json.to_string()
}

/// Sleep в миллисекундах
@external(erlang, "timer", "sleep")
fn sleep_ms(ms: Int) -> Nil
