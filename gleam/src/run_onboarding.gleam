// Runner for manual onboarding trigger
import gleam/io
import gleam/int
import gleam/result
import vibee/onboarding/auto_tasks

pub fn main() {
  let session_id = "session_144022504"
  let user_telegram_id = 144_022_504

  io.println("Starting onboarding for user " <> int.to_string(user_telegram_id))
  io.println("Session: " <> session_id)
  io.println("")

  case auto_tasks.run_onboarding(session_id, user_telegram_id) {
    Ok(status) -> {
      io.println("=== Onboarding Completed ===")
      io.println("Status: " <> status.status)
      io.println("Dialogs parsed: " <> int.to_string(status.dialogs_parsed))
      io.println("Tasks extracted: " <> int.to_string(status.tasks_extracted))
      io.println("Input tokens: " <> int.to_string(status.total_input_tokens))
      io.println("Output tokens: " <> int.to_string(status.total_output_tokens))
    }
    Error(e) -> {
      io.println("=== Onboarding Failed ===")
      io.println("Error: " <> e)
    }
  }
}
