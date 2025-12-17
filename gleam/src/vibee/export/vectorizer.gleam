// VIBEE Vectorizer - Векторизация диалогов через Ollama (100% Gleam)
// Создаёт эмбеддинги для семантического поиска

import gleam/dynamic
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import simplifile
import vibee/logging

/// Конфигурация векторизации
pub type VectorConfig {
  VectorConfig(
    ollama_url: String,
    model: String,
    input_dir: String,
    output_file: String,
  )
}

/// Вектор эмбеддинга
pub type Embedding {
  Embedding(
    chat_id: String,
    msg_id: Int,
    text: String,
    from_name: String,
    vector: List(Float),
  )
}

/// Результат векторизации
pub type VectorResult {
  VectorResult(
    messages_processed: Int,
    embeddings_created: Int,
    failed: Int,
  )
}

/// Конфиг по умолчанию
pub fn default_config() -> VectorConfig {
  VectorConfig(
    ollama_url: "http://localhost:11434",
    model: "nomic-embed-text",
    input_dir: "/Users/playra/vibee-eliza-999/vibee/gleam/data/dialogs",
    output_file: "/Users/playra/vibee-eliza-999/vibee/gleam/data/embeddings.json",
  )
}

/// Векторизовать все диалоги
pub fn vectorize_all_dialogs(config: VectorConfig) -> VectorResult {
  logging.info("Starting vectorization with Ollama...")
  logging.info("Model: " <> config.model)

  // Читаем все JSON файлы с диалогами
  let files = case simplifile.read_directory(config.input_dir) {
    Ok(f) -> list.filter(f, fn(name) { string.ends_with(name, ".json") })
    Error(_) -> []
  }

  logging.info("Found " <> int.to_string(list.length(files)) <> " dialog files")

  // Обрабатываем каждый файл
  let all_embeddings = list.flat_map(files, fn(filename) {
    let filepath = config.input_dir <> "/" <> filename
    process_dialog_file(config, filepath)
  })

  // Сохраняем все эмбеддинги
  let json_content = embeddings_to_json(all_embeddings)
  let _ = simplifile.write(config.output_file, json_content)

  logging.info("Vectorization complete!")
  logging.info("Total embeddings: " <> int.to_string(list.length(all_embeddings)))
  logging.info("Output: " <> config.output_file)

  VectorResult(
    messages_processed: list.length(all_embeddings),
    embeddings_created: list.length(all_embeddings),
    failed: 0,
  )
}

/// Обработать один файл диалога
fn process_dialog_file(config: VectorConfig, filepath: String) -> List(Embedding) {
  case simplifile.read(filepath) {
    Ok(content) -> {
      let messages = parse_messages_from_dialog(content)
      let chat_id = extract_chat_id(content)

      io.println("Processing " <> filepath <> " (" <> int.to_string(list.length(messages)) <> " msgs)")

      // Векторизуем каждое сообщение с паузой
      vectorize_messages_batch(config, chat_id, messages)
    }
    Error(_) -> []
  }
}

/// Векторизация батча сообщений с паузами
fn vectorize_messages_batch(config: VectorConfig, chat_id: String, messages: List(#(Int, String, String))) -> List(Embedding) {
  vectorize_messages_loop(config, chat_id, messages, [], 0)
}

fn vectorize_messages_loop(
  config: VectorConfig,
  chat_id: String,
  messages: List(#(Int, String, String)),
  acc: List(Embedding),
  count: Int
) -> List(Embedding) {
  case messages {
    [] -> acc
    [msg, ..rest] -> {
      let #(msg_id, text, from_name) = msg

      // Пропускаем короткие сообщения
      case string.length(text) < 10 {
        True -> vectorize_messages_loop(config, chat_id, rest, acc, count)
        False -> {
          // Пауза каждые 10 сообщений чтобы не перегружать Ollama
          let _ = case count > 0 && count % 10 == 0 {
            True -> {
              io.print(".")
              sleep_ms(100)
            }
            False -> Nil
          }

          case get_embedding(config, text) {
            Ok(vector) -> {
              let emb = Embedding(
                chat_id: chat_id,
                msg_id: msg_id,
                text: text,
                from_name: from_name,
                vector: vector,
              )
              vectorize_messages_loop(config, chat_id, rest, [emb, ..acc], count + 1)
            }
            Error(_) -> {
              // Пропускаем с ошибкой, но продолжаем
              vectorize_messages_loop(config, chat_id, rest, acc, count + 1)
            }
          }
        }
      }
    }
  }
}

/// Получить эмбеддинг от Ollama с retry
fn get_embedding(config: VectorConfig, text: String) -> Result(List(Float), String) {
  get_embedding_with_retry(config, text, 3)
}

fn get_embedding_with_retry(config: VectorConfig, text: String, retries: Int) -> Result(List(Float), String) {
  let body = json.object([
    #("model", json.string(config.model)),
    #("prompt", json.string(text)),
  ])
  |> json.to_string()

  let req = request.new()
    |> request.set_scheme(http.Http)
    |> request.set_method(http.Post)
    |> request.set_host("localhost")
    |> request.set_port(11434)
    |> request.set_path("/api/embeddings")
    |> request.set_header("Content-Type", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) -> {
      parse_embedding_response(response.body)
    }
    Error(_) -> {
      case retries > 0 {
        True -> {
          // Пауза перед retry
          sleep_ms(500)
          get_embedding_with_retry(config, text, retries - 1)
        }
        False -> Error("Failed to get embedding after retries")
      }
    }
  }
}

@external(erlang, "timer", "sleep")
fn sleep_ms(ms: Int) -> Nil

/// Парсинг ответа от Ollama
fn parse_embedding_response(body: String) -> Result(List(Float), String) {
  // Ищем "embedding":[...]
  case string.split(body, "\"embedding\":[") {
    [_, rest, ..] -> {
      case string.split(rest, "]") {
        [numbers_str, ..] -> {
          let floats = string.split(numbers_str, ",")
            |> list.filter_map(fn(s) {
              parse_float(string.trim(s))
            })
          Ok(floats)
        }
        _ -> Error("No closing bracket")
      }
    }
    _ -> Error("No embedding in response")
  }
}

/// Парсинг float из строки
fn parse_float(s: String) -> Result(Float, Nil) {
  case string.contains(s, ".") {
    True -> {
      // Уже float
      case string.split(s, ".") {
        [int_part, dec_part, ..] -> {
          case int.parse(int_part), int.parse(string.slice(dec_part, 0, 8)) {
            Ok(i), Ok(d) -> {
              let divisor = pow10(string.length(string.slice(dec_part, 0, 8)))
              let sign = case i < 0 || string.starts_with(s, "-") {
                True -> -1.0
                False -> 1.0
              }
              let abs_i = case i < 0 {
                True -> -i
                False -> i
              }
              Ok(sign *. { int.to_float(abs_i) +. int.to_float(d) /. int.to_float(divisor) })
            }
            _, _ -> Error(Nil)
          }
        }
        _ -> Error(Nil)
      }
    }
    False -> {
      // Integer
      case int.parse(s) {
        Ok(i) -> Ok(int.to_float(i))
        Error(_) -> Error(Nil)
      }
    }
  }
}

fn pow10(n: Int) -> Int {
  case n {
    0 -> 1
    1 -> 10
    2 -> 100
    3 -> 1000
    4 -> 10000
    5 -> 100000
    6 -> 1000000
    7 -> 10000000
    8 -> 100000000
    _ -> 1000000000
  }
}

/// Извлечь chat_id из JSON диалога
fn extract_chat_id(content: String) -> String {
  case string.split(content, "\"chat_id\":\"") {
    [_, rest, ..] -> {
      case string.split(rest, "\"") {
        [id, ..] -> id
        _ -> "unknown"
      }
    }
    _ -> "unknown"
  }
}

/// Парсинг сообщений из JSON диалога
fn parse_messages_from_dialog(content: String) -> List(#(Int, String, String)) {
  // Ищем каждое сообщение
  let parts = string.split(content, "{\"id\":")

  list.filter_map(list.drop(parts, 1), fn(part) {
    parse_message_tuple(part)
  })
}

fn parse_message_tuple(part: String) -> Result(#(Int, String, String), Nil) {
  // id
  let id = case string.split(part, ",") {
    [id_str, ..] -> {
      case int.parse(string.trim(id_str)) {
        Ok(n) -> n
        Error(_) -> 0
      }
    }
    _ -> 0
  }

  // text
  let text = extract_json_field(part, "text")

  // from_name
  let from_name = case extract_json_field(part, "from_name") {
    "" -> "User"
    name -> name
  }

  case id, text {
    0, _ -> Error(Nil)
    _, "" -> Error(Nil)
    _, _ -> Ok(#(id, text, from_name))
  }
}

fn extract_json_field(json: String, field: String) -> String {
  let pattern = "\"" <> field <> "\":\""
  case string.split(json, pattern) {
    [_, rest, ..] -> {
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
      case string.pop_grapheme(rest) {
        Ok(#(c, remaining)) -> extract_until_quote(remaining, acc <> c)
        Error(_) -> acc
      }
    }
    Ok(#(c, rest)) -> extract_until_quote(rest, acc <> c)
  }
}

/// Конвертация эмбеддингов в JSON
fn embeddings_to_json(embeddings: List(Embedding)) -> String {
  let items = list.map(embeddings, fn(emb) {
    json.object([
      #("chat_id", json.string(emb.chat_id)),
      #("msg_id", json.int(emb.msg_id)),
      #("text", json.string(emb.text)),
      #("from_name", json.string(emb.from_name)),
      #("vector", json.array(emb.vector, json.float)),
    ])
  })

  json.object([
    #("count", json.int(list.length(embeddings))),
    #("embeddings", json.array(items, fn(x) { x })),
  ])
  |> json.to_string()
}
