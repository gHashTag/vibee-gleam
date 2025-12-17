// VIBEE Vectorize - Запуск векторизации диалогов
// Запуск: gleam run -m vectorize

import gleam/int
import gleam/io
import vibee/export/vectorizer

pub fn main() {
  io.println("============================================================")
  io.println("VIBEE - Dialog Vectorizer (100% Gleam)")
  io.println("============================================================")
  io.println("")

  let config = vectorizer.default_config()

  io.println("Configuration:")
  io.println("  Ollama URL: " <> config.ollama_url)
  io.println("  Model: " <> config.model)
  io.println("  Input dir: " <> config.input_dir)
  io.println("  Output: " <> config.output_file)
  io.println("")
  io.println("Starting vectorization...")
  io.println("")

  let result = vectorizer.vectorize_all_dialogs(config)

  io.println("")
  io.println("============================================================")
  io.println("VECTORIZATION COMPLETE!")
  io.println("  Messages processed: " <> int.to_string(result.messages_processed))
  io.println("  Embeddings created: " <> int.to_string(result.embeddings_created))
  io.println("============================================================")
}
