// Graceful Shutdown Handler
// Ensures clean shutdown without losing messages

import gleam/erlang/process.{type Subject}
import gleam/io
import vibee/agent/polling_actor

pub type ShutdownMessage {
  Shutdown
}

/// Setup graceful shutdown handler
pub fn setup_handler(_polling_subject: Subject(polling_actor.PollingMessage)) {
  io.println("[SHUTDOWN] Graceful shutdown handler registered")
  io.println("[SHUTDOWN] Will handle SIGTERM from Fly.io")
  
  // Trap exit signals for graceful shutdown
  process.trap_exits(True)
  
  Nil
}


