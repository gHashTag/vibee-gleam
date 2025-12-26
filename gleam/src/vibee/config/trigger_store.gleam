// Trigger Store - работа с триггерами в БД
// NO HARDCODE! Все триггеры хранятся в trigger_words table

import gleam/dynamic/decode
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import pog
import vibee/db/postgres

// =============================================================================
// Types
// =============================================================================

pub type TriggerWord {
  TriggerWord(
    id: Int,
    word: String,
    category: String,
    is_active: Bool,
  )
}

// =============================================================================
// Public API
// =============================================================================

/// Получить все активные триггеры из БД
pub fn get_triggers(category_prefix: String) -> List(String) {
  case postgres.get_global_pool() {
    None -> {
      io.println("[TRIGGER_STORE] No database pool, using fallback")
      []  // Пустой список, если нет БД
    }
    Some(pool) -> get_triggers_from_db(pool, category_prefix)
  }
}

/// Получить все активные AI триггеры
pub fn get_ai_triggers() -> List(String) {
  get_triggers("ai_")
}

/// Получить все активные крипто триггеры
pub fn get_crypto_triggers() -> List(String) {
  get_triggers("crypto_")
}

/// Найти первый совпавший триггер в тексте
pub fn find_matching_trigger(text: String) -> Result(String, Nil) {
  let triggers = get_ai_triggers()
  let lower_text = string.lowercase(text)

  list.find(triggers, fn(trigger) {
    let lower_trigger = string.lowercase(trigger)
    string.contains(lower_text, lower_trigger)
  })
}

/// Найти все совпавшие триггеры
pub fn find_all_matching_triggers(text: String) -> List(String) {
  let triggers = get_ai_triggers()
  let lower_text = string.lowercase(text)

  list.filter(triggers, fn(trigger) {
    let lower_trigger = string.lowercase(trigger)
    string.contains(lower_text, lower_trigger)
  })
}

/// Проверить содержит ли текст триггер
pub fn contains_trigger(text: String) -> Bool {
  case find_matching_trigger(text) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Добавить новый триггер
pub fn add_trigger(word: String, category: String) -> Result(Int, String) {
  case postgres.get_global_pool() {
    None -> Error("No database connection")
    Some(pool) -> {
      let sql = "INSERT INTO trigger_words (word, category, is_active) VALUES ($1, $2, true) RETURNING id"

      let decoder = {
        use id <- decode.field(0, decode.int)
        decode.success(id)
      }

      case pog.query(sql)
        |> pog.parameter(pog.text(word))
        |> pog.parameter(pog.text(category))
        |> pog.returning(decoder)
        |> pog.execute(pool)
      {
        Ok(pog.Returned(_, [id])) -> {
          io.println("[TRIGGER_STORE] Added trigger: " <> word <> " (" <> category <> ")")
          Ok(id)
        }
        Ok(_) -> Error("Failed to insert trigger")
        Error(e) -> Error("Database error: " <> string.inspect(e))
      }
    }
  }
}

/// Удалить триггер (soft delete - установить is_active = false)
pub fn remove_trigger(word: String) -> Result(Nil, String) {
  case postgres.get_global_pool() {
    None -> Error("No database connection")
    Some(pool) -> {
      let sql = "UPDATE trigger_words SET is_active = false WHERE word = $1"

      case pog.query(sql)
        |> pog.parameter(pog.text(word))
        |> pog.execute(pool)
      {
        Ok(_) -> {
          io.println("[TRIGGER_STORE] Removed trigger: " <> word)
          Ok(Nil)
        }
        Error(e) -> Error("Database error: " <> string.inspect(e))
      }
    }
  }
}

/// Получить все триггеры с их категориями
pub fn list_all_triggers() -> List(TriggerWord) {
  case postgres.get_global_pool() {
    None -> []
    Some(pool) -> {
      let sql = "SELECT id, word, category, is_active FROM trigger_words WHERE is_active = true ORDER BY category, word"

      let decoder = {
        use id <- decode.field(0, decode.int)
        use word <- decode.field(1, decode.string)
        use category <- decode.field(2, decode.string)
        use is_active <- decode.field(3, decode.bool)
        decode.success(TriggerWord(id, word, category, is_active))
      }

      case pog.query(sql)
        |> pog.returning(decoder)
        |> pog.execute(pool)
      {
        Ok(pog.Returned(_, triggers)) -> triggers
        Error(_) -> []
      }
    }
  }
}

/// Получить количество триггеров по категориям
pub fn count_by_category() -> List(#(String, Int)) {
  case postgres.get_global_pool() {
    None -> []
    Some(pool) -> {
      let sql = "SELECT category, COUNT(*) FROM trigger_words WHERE is_active = true GROUP BY category ORDER BY category"

      let decoder = {
        use category <- decode.field(0, decode.string)
        use count <- decode.field(1, decode.int)
        decode.success(#(category, count))
      }

      case pog.query(sql)
        |> pog.returning(decoder)
        |> pog.execute(pool)
      {
        Ok(pog.Returned(_, counts)) -> counts
        Error(_) -> []
      }
    }
  }
}

// =============================================================================
// Internal
// =============================================================================

fn get_triggers_from_db(pool: pog.Connection, category_prefix: String) -> List(String) {
  let sql = "SELECT word FROM trigger_words WHERE category LIKE $1 AND is_active = true"
  let pattern = category_prefix <> "%"

  let decoder = {
    use word <- decode.field(0, decode.string)
    decode.success(word)
  }

  case pog.query(sql)
    |> pog.parameter(pog.text(pattern))
    |> pog.returning(decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, words)) -> {
      io.println("[TRIGGER_STORE] Loaded " <> string.inspect(list.length(words)) <> " triggers for " <> category_prefix)
      words
    }
    Error(e) -> {
      io.println("[TRIGGER_STORE] Error loading triggers: " <> string.inspect(e))
      []
    }
  }
}
