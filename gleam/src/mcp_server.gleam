// VIBEE MCP Server - Model Context Protocol для AI агентов
// 100% Gleam implementation

import gleam/erlang/process
import gleam/list
import vibee/logging
import vibee/mcp/server
import vibee/mcp/session_manager
import vibee/mcp/tools

pub fn main() {
  logging.quick_info("============================================================")
  logging.quick_info("VIBEE MCP Server v1.0")
  logging.quick_info("Model Context Protocol for AI Agents")
  logging.quick_info("============================================================")
  logging.quick_info("")

  // Initialize session manager (ETS table for multi-account support)
  session_manager.init()
  logging.quick_info("Session manager initialized (multi-account support enabled)")

  // Инициализируем инструменты
  let tool_registry = tools.init_registry()

  logging.quick_info("Registered tools:")
  tools.list_tools(tool_registry)
  |> list.each(fn(t) { logging.quick_info("  - " <> t) })

  logging.quick_info("")
  logging.quick_info("Starting MCP server on stdio...")
  logging.quick_info("")

  // Запускаем MCP сервер
  case server.start(tool_registry) {
    Ok(_) -> {
      logging.quick_info("MCP Server running. Waiting for requests...")
      // Держим процесс живым
      process.sleep_forever()
    }
    Error(err) -> {
      logging.quick_error("Failed to start MCP server: " <> err)
    }
  }
}
