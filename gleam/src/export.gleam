// VIBEE Export Tool - Скачивание всех диалогов из Telegram
// Запуск: gleam run -m export

import gleam/io
import gleam/int
import vibee/export/dialog_exporter
import vibee/logging

pub fn main() {
  io.println("============================================================")
  io.println("VIBEE - Telegram Dialog Exporter (100% Gleam)")
  io.println("============================================================")
  io.println("")

  let config = dialog_exporter.default_config()

  io.println("Configuration:")
  io.println("  Bridge URL: " <> config.bridge_url)
  io.println("  Session ID: " <> config.session_id)
  io.println("  Output dir: " <> config.output_dir)
  io.println("  Batch size: " <> int.to_string(config.batch_size))
  io.println("")
  io.println("Starting export of PERSONAL dialogs (users only)...")
  io.println("")

  // Экспортируем только личные диалоги для контекста общения
  let result = dialog_exporter.export_all_user_dialogs(config)

  io.println("")
  io.println("============================================================")
  io.println("EXPORT COMPLETE!")
  io.println("  Dialogs exported: " <> int.to_string(result.dialog_count))
  io.println("  Messages downloaded: " <> int.to_string(result.message_count))
  io.println("  Output directory: " <> config.output_dir)
  io.println("============================================================")
}
