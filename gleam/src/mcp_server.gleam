// VIBEE MCP Server - Model Context Protocol для AI агентов
// 100% Gleam implementation

import gleam/erlang/process
import gleam/list
import vibee/logging
import vibee/mcp/server
import vibee/mcp/session_manager
import vibee/mcp/tools

pub fn main() {
  logging.info("============================================================")
  logging.info("VIBEE MCP Server v1.0")
  logging.info("Model Context Protocol for AI Agents")
  logging.info("============================================================")
  logging.info("")

  // Initialize session manager (ETS table for multi-account support)
  session_manager.init()
  logging.info("Session manager initialized (multi-account support enabled)")

  // Инициализируем инструменты
  let tool_registry = tools.init_registry()

  logging.info("Registered tools:")
  tools.list_tools(tool_registry)
  |> list.each(fn(t) { logging.info("  - " <> t) })

  logging.info("")
  logging.info("Starting MCP server on stdio...")
  logging.info("")

  // Запускаем MCP сервер
  case server.start(tool_registry) {
    Ok(_) -> {
      logging.info("MCP Server running. Waiting for requests...")
      // Держим процесс живым
      process.sleep_forever()
    }
    Error(err) -> {
      logging.error("Failed to start MCP server: " <> err)
    }
  }
}
