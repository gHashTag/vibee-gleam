// Trigger Handlers - API для проверки и тестирования триггеров
// ВАЖНО: Данные хранятся в БД (trigger_words table), НЕ хардкод!

import gleam/bytes_tree
import gleam/http/response.{type Response}
import gleam/json
import gleam/list
import gleam/string
import mist.{type ResponseData}
import vibee/config/trigger_store
import vibee/config/trigger_chats

// =============================================================================
// PUBLIC API
// =============================================================================

/// GET /api/triggers/list - Список всех триггеров из БД
pub fn list_handler() -> Response(ResponseData) {
  // Получаем триггеры из БД
  let all_triggers = trigger_store.list_all_triggers()
  let counts = trigger_store.count_by_category()

  let body = json.object([
    #("status", json.string("ok")),
    #("source", json.string("database")),  // Не хардкод!
    #("total_triggers", json.int(list.length(all_triggers))),
    #("categories", json.array(counts, fn(c) {
      json.object([
        #("category", json.string(c.0)),
        #("count", json.int(c.1)),
      ])
    })),
    #("triggers", json.array(all_triggers, fn(t) {
      json.object([
        #("id", json.int(t.id)),
        #("word", json.string(t.word)),
        #("category", json.string(t.category)),
        #("is_active", json.bool(t.is_active)),
      ])
    })),
    #("chats", json.array(
      trigger_chats.get_trigger_chats(),
      fn(c) {
        json.object([
          #("chat_id", json.string(c.chat_id)),
          #("chat_name", json.string(c.chat_name)),
          #("is_active", json.bool(c.is_active)),
          #("observe_only", json.bool(c.observe_only)),
          #("forward_chat_id", json.string(c.forward_chat_id)),
        ])
      }
    )),
  ])
  |> json.to_string

  response.new(200)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

/// POST /api/triggers/test - Проверить текст на триггеры
/// Body: {"text": "Хочу создать ai аватар"}
pub fn test_handler(body: String) -> Response(ResponseData) {
  case parse_text_from_json(body) {
    Error(_) -> json_error(400, "Invalid JSON. Expected: {\"text\": \"...\"}")
    Ok(text) -> test_text(text)
  }
}

/// POST /api/triggers/add - Добавить триггер в БД
/// Body: {"word": "новый триггер", "category": "ai_custom"}
pub fn add_handler(body: String) -> Response(ResponseData) {
  case parse_add_request(body) {
    Error(_) -> json_error(400, "Invalid JSON. Expected: {\"word\": \"...\", \"category\": \"...\"}")
    Ok(#(word, category)) -> {
      case trigger_store.add_trigger(word, category) {
        Ok(id) -> {
          let response_body = json.object([
            #("status", json.string("ok")),
            #("message", json.string("Trigger added")),
            #("id", json.int(id)),
            #("word", json.string(word)),
            #("category", json.string(category)),
          ])
          |> json.to_string

          response.new(201)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string(response_body)))
        }
        Error(e) -> json_error(500, "Failed to add trigger: " <> e)
      }
    }
  }
}

/// DELETE /api/triggers/remove - Удалить триггер
/// Body: {"word": "триггер"}
pub fn remove_handler(body: String) -> Response(ResponseData) {
  case parse_word_from_json(body) {
    Error(_) -> json_error(400, "Invalid JSON. Expected: {\"word\": \"...\"}")
    Ok(word) -> {
      case trigger_store.remove_trigger(word) {
        Ok(_) -> {
          let response_body = json.object([
            #("status", json.string("ok")),
            #("message", json.string("Trigger removed")),
            #("word", json.string(word)),
          ])
          |> json.to_string

          response.new(200)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string(response_body)))
        }
        Error(e) -> json_error(500, "Failed to remove trigger: " <> e)
      }
    }
  }
}

/// GET /api/triggers/self-check - Полная самопроверка системы
pub fn self_check_handler() -> Response(ResponseData) {
  let test_cases = [
    // AI тесты (должны срабатывать)
    #("как создать ai аватар", True, "ai"),
    #("нейросеть для видео контента", True, "ai"),
    #("устал снимать рилсы каждый день", True, "ai"),
    #("нужен цифровой клон для блога", True, "ai"),
    #("voice clone для озвучки", True, "ai"),
    #("lip sync технология", True, "ai"),
    #("хочу использовать midjourney", True, "ai"),
    #("как работает heygen", True, "ai"),
    #("нужен генератор видео", True, "ai"),
    #("автоматизация контента для инстаграма", True, "ai"),
    // Негативные тесты (НЕ должны срабатывать)
    #("привет как дела", False, "none"),
    #("погода сегодня хорошая", False, "none"),
    #("что посмотреть вечером", False, "none"),
    #("купить продукты в магазине", False, "none"),
  ]

  let results = list.map(test_cases, fn(tc) {
    let #(text, expected, category) = tc
    let matched = trigger_store.find_matching_trigger(text)
    let has_trigger = case matched {
      Ok(_) -> True
      Error(_) -> False
    }
    let passed = has_trigger == expected

    json.object([
      #("text", json.string(text)),
      #("expected_trigger", json.bool(expected)),
      #("actual_trigger", json.bool(has_trigger)),
      #("matched_word", json.string(case matched {
        Ok(w) -> w
        Error(_) -> ""
      })),
      #("category", json.string(category)),
      #("passed", json.bool(passed)),
    ])
  })

  let passed_count = list.count(test_cases, fn(tc) {
    let #(text, expected, _) = tc
    let has_trigger = trigger_store.contains_trigger(text)
    has_trigger == expected
  })

  let total = list.length(test_cases)
  let all_passed = passed_count == total

  let body = json.object([
    #("status", json.string(case all_passed { True -> "PASSED" False -> "FAILED" })),
    #("source", json.string("database")),
    #("total", json.int(total)),
    #("passed", json.int(passed_count)),
    #("failed", json.int(total - passed_count)),
    #("success_rate", json.string(int_to_string(passed_count * 100 / total) <> "%")),
    #("tests", json.array(results, fn(x) { x })),
  ])
  |> json.to_string

  response.new(case all_passed { True -> 200 False -> 500 })
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

// =============================================================================
// INTERNAL
// =============================================================================

fn test_text(text: String) -> Response(ResponseData) {
  let matched_triggers = trigger_store.find_all_matching_triggers(text)
  let has_any = list.length(matched_triggers) > 0

  let body = json.object([
    #("text", json.string(text)),
    #("has_trigger", json.bool(has_any)),
    #("matched_triggers", json.array(matched_triggers, json.string)),
    #("trigger_count", json.int(list.length(matched_triggers))),
    #("would_generate_lead", json.bool(has_any)),
    #("source", json.string("database")),
  ])
  |> json.to_string

  response.new(200)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

fn parse_text_from_json(body: String) -> Result(String, Nil) {
  case string.contains(body, "\"text\"") {
    False -> Error(Nil)
    True -> {
      case string.split(body, "\"text\"") {
        [_, rest] -> {
          case string.split(rest, "\"") {
            [_, value, ..] -> Ok(value)
            _ -> Error(Nil)
          }
        }
        _ -> Error(Nil)
      }
    }
  }
}

fn parse_word_from_json(body: String) -> Result(String, Nil) {
  case string.contains(body, "\"word\"") {
    False -> Error(Nil)
    True -> {
      case string.split(body, "\"word\"") {
        [_, rest] -> {
          case string.split(rest, "\"") {
            [_, value, ..] -> Ok(value)
            _ -> Error(Nil)
          }
        }
        _ -> Error(Nil)
      }
    }
  }
}

fn parse_add_request(body: String) -> Result(#(String, String), Nil) {
  case parse_word_from_json(body) {
    Error(_) -> Error(Nil)
    Ok(word) -> {
      case string.contains(body, "\"category\"") {
        False -> Ok(#(word, "ai_custom"))
        True -> {
          case string.split(body, "\"category\"") {
            [_, rest] -> {
              case string.split(rest, "\"") {
                [_, category, ..] -> Ok(#(word, category))
                _ -> Ok(#(word, "ai_custom"))
              }
            }
            _ -> Ok(#(word, "ai_custom"))
          }
        }
      }
    }
  }
}

fn json_error(code: Int, message: String) -> Response(ResponseData) {
  let body = json.object([
    #("error", json.string(message)),
  ])
  |> json.to_string

  response.new(code)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ -> do_int_to_string(n, "")
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        _ -> "9"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}
