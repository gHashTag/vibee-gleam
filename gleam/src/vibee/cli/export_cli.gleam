// VIBEE Export CLI - Консольный интерфейс для экспорта диалогов
// Запуск: gleam run -m vibee/cli/export_cli

import gleam/io
import vibee/export/dialog_exporter
import vibee/logging

pub fn main() {
  io.println("=" <> string_repeat("=", 59))
  io.println("VIBEE - Telegram Dialog Exporter (Gleam)")
  io.println("=" <> string_repeat("=", 59))
  io.println("")

  let config = dialog_exporter.default_config()

  io.println("Config:")
  io.println("  Bridge: " <> config.bridge_url)
  io.println("  Session: " <> config.session_id)
  io.println("  Output: " <> config.output_dir)
  io.println("  Batch size: " <> int_to_string(config.batch_size))
  io.println("")

  // Экспортируем только user диалоги
  let result = dialog_exporter.export_all_user_dialogs(config)

  io.println("")
  io.println("=" <> string_repeat("=", 59))
  io.println("EXPORT COMPLETE!")
  io.println("  Dialogs exported: " <> int_to_string(result.dialog_count))
  io.println("  Messages downloaded: " <> int_to_string(result.message_count))
  io.println("=" <> string_repeat("=", 59))
}

fn string_repeat(s: String, n: Int) -> String {
  case n <= 0 {
    True -> ""
    False -> s <> string_repeat(s, n - 1)
  }
}

@external(erlang, "erlang", "integer_to_list")
fn int_to_list(n: Int) -> List(Int)

fn int_to_string(n: Int) -> String {
  n
  |> int_to_list()
  |> list_to_string()
}

@external(erlang, "erlang", "list_to_binary")
fn list_to_string(chars: List(Int)) -> String
