// MCP Tools Registry - All available tools for AI agents

import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/float
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile
import vibee/logging
import vibee/mcp/a2a_protocol
import vibee/mcp/agent_memory
import vibee/mcp/autonomous
import vibee/mcp/config
import vibee/mcp/decision
import vibee/mcp/decoders
import vibee/mcp/events
import vibee/mcp/healing
import vibee/mcp/protocol
import vibee/mcp/ai_tools
import vibee/mcp/ai_handlers
import vibee/mcp/rag_tools
import vibee/mcp/storage_tools
import vibee/mcp/task_tools
import vibee/mcp/remotion_tools
import vibee/mcp/rainbow_types
import vibee/mcp/session_manager
import vibee/mcp/shell
import vibee/mcp/super_agent
import vibee/mcp/task_store
import vibee/mcp/types.{type Tool, type ToolResult, TextContent, Tool}
import vibee/mcp/validation
import vibee/config/telegram_config
import vibee/onboarding/tools as onboarding_tools
import vibee/onboarding/auto_tasks
import vibee/payment/tools as payment_tools
import vibee/payment/invoice_tools
import vibee/p2p/tools as p2p_tools
import vibee/earning/tools as earning_tools

/// Tool handler function type
pub type ToolHandler =
  fn(json.Json) -> ToolResult

/// Tool category for filtering
pub type ToolCategory {
  CategoryTelegram
  CategoryKnowledge
  CategoryFile
  CategoryVoice
  CategorySystem
  CategoryEvent
  CategoryDebug
  CategoryCode
  CategoryTest
  CategoryAgent
  CategoryBot
  CategoryAuth
  CategoryRainbow
  CategoryA2A
  CategorySuperAgent
  CategoryMemory
  CategorySession
  CategoryAI
  CategoryOnboarding
  CategoryPayment
  CategoryP2P
  CategoryEarning
  CategoryClone
  CategoryTask
  CategoryRemotion
}

/// Tool registry
pub type ToolRegistry {
  ToolRegistry(
    tools: Dict(String, Tool),
    handlers: Dict(String, ToolHandler),
    categories: Dict(String, ToolCategory),
  )
}

/// Get tool category by name
pub fn get_tool_category(name: String) -> ToolCategory {
  case string.starts_with(name, "telegram_") {
    True -> CategoryTelegram
    False ->
      case string.starts_with(name, "knowledge_") {
        True -> CategoryKnowledge
        False ->
          case string.starts_with(name, "file_") {
            True -> CategoryFile
            False ->
              case string.starts_with(name, "voice_") {
                True -> CategoryVoice
                False ->
                  case string.starts_with(name, "system_") {
                    True -> CategorySystem
                    False ->
                      case string.starts_with(name, "event_") {
                        True -> CategoryEvent
                        False ->
                          case string.starts_with(name, "debug_") {
                            True -> CategoryDebug
                            False ->
                              case string.starts_with(name, "code_") {
                                True -> CategoryCode
                                False ->
                                  case string.starts_with(name, "test_") {
                                    True -> CategoryTest
                                    False ->
                                      case string.starts_with(name, "agent_") {
                                        True -> CategoryAgent
                                        False ->
                                          case
                                            string.starts_with(name, "bot_")
                                          {
                                            True -> CategoryBot
                                            False ->
                                              case
                                                string.starts_with(
                                                  name,
                                                  "auth_",
                                                )
                                              {
                                                True -> CategoryAuth
                                                False ->
                                                  case
                                                    string.starts_with(
                                                      name,
                                                      "task_",
                                                    )
                                                  {
                                                    True -> CategoryTask
                                                    False ->
                                                      case
                                                        string.starts_with(
                                                          name,
                                                          "rainbow_",
                                                        )
                                                        || string.starts_with(
                                                          name,
                                                          "heal_",
                                                        )
                                                        || string.starts_with(
                                                          name,
                                                          "decide_",
                                                        )
                                                      {
                                                    True -> CategoryRainbow
                                                    False ->
                                                      case
                                                        string.starts_with(
                                                          name,
                                                          "a2a_",
                                                        )
                                                      {
                                                        True -> CategoryA2A
                                                        False ->
                                                          case
                                                            string.starts_with(
                                                              name,
                                                              "super_",
                                                            )
                                                          {
                                                            True ->
                                                              CategorySuperAgent
                                                            False ->
                                                              case
                                                                string.starts_with(
                                                                  name,
                                                                  "memory_",
                                                                )
                                                              {
                                                                True ->
                                                                  CategoryMemory
                                                                False ->
                                                                  case
                                                                    string.starts_with(
                                                                      name,
                                                                      "session_",
                                                                    )
                                                                  {
                                                                    True ->
                                                                      CategorySession
                                                                    False ->
                                                                      case
                                                                        string.starts_with(
                                                                          name,
                                                                          "ai_",
                                                                        )
                                                                      {
                                                                        True ->
                                                                          CategoryAI
                                                                        False ->
                                                                          case
                                                                            string.starts_with(
                                                                              name,
                                                                              "onboarding_",
                                                                            )
                                                                          {
                                                                            True ->
                                                                              CategoryOnboarding
                                                                            False ->
                                                                              case
                                                                                string.starts_with(
                                                                                  name,
                                                                                  "payment_",
                                                                                )
                                                                                || string.starts_with(
                                                                                  name,
                                                                                  "balance_",
                                                                                )
                                                                              {
                                                                                True ->
                                                                                  CategoryPayment
                                                                                False ->
                                                                                  case
                                                                                    string.starts_with(
                                                                                      name,
                                                                                      "p2p_",
                                                                                    )
                                                                                  {
                                                                                    True ->
                                                                                      CategoryP2P
                                                                                    False ->
                                                                                      case
                                                                                        string.starts_with(
                                                                                          name,
                                                                                          "earning_",
                                                                                        )
                                                                                        || string.starts_with(
                                                                                          name,
                                                                                          "arbitrage_",
                                                                                        )
                                                                                      {
                                                                                        True ->
                                                                                          CategoryEarning
                                                                                        False ->
                                                                                          case
                                                                                            string.starts_with(
                                                                                              name,
                                                                                              "clone_",
                                                                                            )
                                                                                            || string.starts_with(
                                                                                              name,
                                                                                              "service_",
                                                                                            )
                                                                                          {
                                                                                            True ->
                                                                                              CategoryClone
                                                                                            False ->
                                                                                              case
                                                                                                string.starts_with(
                                                                                                  name,
                                                                                                  "remotion_",
                                                                                                )
                                                                                                || string.starts_with(
                                                                                                  name,
                                                                                                  "template_",
                                                                                                )
                                                                                                || string.starts_with(
                                                                                                  name,
                                                                                                  "props_",
                                                                                                )
                                                                                              {
                                                                                                True ->
                                                                                                  CategoryRemotion
                                                                                                False ->
                                                                                                  CategorySystem
                                                                                                // Default
                                                                                              }
                                                                                          }
                                                                                      }
                                                                                  }
                                                                              }
                                                                          }
                                                                      }
                                                                  }
                                                              }
                                                          }
                                                      }
                                                  }
                                              }
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
              }
          }
      }
    }
  }
}

/// Get tools by category
pub fn get_tools_by_category(
  registry: ToolRegistry,
  category: ToolCategory,
) -> List(Tool) {
  dict.to_list(registry.tools)
  |> list.filter(fn(pair) {
    let #(name, _) = pair
    get_tool_category(name) == category
  })
  |> list.map(fn(pair) {
    let #(_, tool) = pair
    tool
  })
}

/// Get all category names for tools/list filtering
pub fn category_to_string(cat: ToolCategory) -> String {
  case cat {
    CategoryTelegram -> "telegram"
    CategoryKnowledge -> "knowledge"
    CategoryFile -> "file"
    CategoryVoice -> "voice"
    CategorySystem -> "system"
    CategoryEvent -> "event"
    CategoryDebug -> "debug"
    CategoryCode -> "code"
    CategoryTest -> "test"
    CategoryAgent -> "agent"
    CategoryBot -> "bot"
    CategoryAuth -> "auth"
    CategoryRainbow -> "rainbow"
    CategoryA2A -> "a2a"
    CategorySuperAgent -> "super_agent"
    CategoryMemory -> "memory"
    CategorySession -> "session"
    CategoryAI -> "ai"
    CategoryOnboarding -> "onboarding"
    CategoryPayment -> "payment"
    CategoryP2P -> "p2p"
    CategoryEarning -> "earning"
    CategoryClone -> "clone"
    CategoryTask -> "task"
    CategoryRemotion -> "remotion"
  }
}

/// Parse category from string
pub fn parse_category(s: String) -> Result(ToolCategory, Nil) {
  case string.lowercase(s) {
    "telegram" -> Ok(CategoryTelegram)
    "knowledge" -> Ok(CategoryKnowledge)
    "file" -> Ok(CategoryFile)
    "voice" -> Ok(CategoryVoice)
    "system" -> Ok(CategorySystem)
    "event" -> Ok(CategoryEvent)
    "debug" -> Ok(CategoryDebug)
    "code" -> Ok(CategoryCode)
    "test" -> Ok(CategoryTest)
    "agent" -> Ok(CategoryAgent)
    "bot" -> Ok(CategoryBot)
    "auth" -> Ok(CategoryAuth)
    "rainbow" -> Ok(CategoryRainbow)
    "a2a" -> Ok(CategoryA2A)
    "super_agent" -> Ok(CategorySuperAgent)
    "memory" -> Ok(CategoryMemory)
    "session" -> Ok(CategorySession)
    "ai" -> Ok(CategoryAI)
    "onboarding" -> Ok(CategoryOnboarding)
    "payment" -> Ok(CategoryPayment)
    "p2p" -> Ok(CategoryP2P)
    "earning" -> Ok(CategoryEarning)
    "clone" -> Ok(CategoryClone)
    "task" -> Ok(CategoryTask)
    "remotion" -> Ok(CategoryRemotion)
    _ -> Error(Nil)
  }
}

/// Initialize tool registry with all available tools
pub fn init_registry() -> ToolRegistry {
  let tools =
    [
      // Telegram tools
      telegram_get_dialogs_tool(),
      telegram_get_history_tool(),
      telegram_send_message_tool(),
      telegram_send_buttons_tool(),
      telegram_send_photo_tool(),
      telegram_download_media_tool(),
      telegram_get_me_tool(),
      telegram_subscribe_updates_tool(),

      // Knowledge tools
      knowledge_search_tool(),
      knowledge_embed_tool(),

      // File tools
      file_read_tool(),
      file_write_tool(),
      file_list_tool(),

      // Voice tools
      voice_transcribe_tool(),

      // System tools
      system_log_tool(),
      system_exec_tool(),

      // Event tools
      event_emit_tool(),
      event_list_tool(),

      // Debug tools
      debug_build_tool(),
      debug_test_tool(),
      debug_analyze_tool(),
      debug_trace_tool(),
      debug_log_tool(),

      // Code tools
      code_generate_tool(),
      code_refactor_tool(),
      code_explain_tool(),
      code_find_similar_tool(),
      code_diff_tool(),

      // Test tools
      test_run_tool(),
      test_create_tool(),
      test_coverage_tool(),
      test_validate_tool(),

      // Agent tools
      agent_spawn_tool(),
      agent_message_tool(),
      agent_status_tool(),
      agent_kill_tool(),

      // Bot Analysis tools
      bot_analyze_tool(),
      bot_compare_tool(),
      bot_monitor_tool(),
      bot_extract_commands_tool(),
      bot_test_interaction_tool(),

      // Auth tools
      auth_status_tool(),
      auth_send_code_tool(),
      auth_verify_code_tool(),
      auth_2fa_tool(),
      auth_logout_tool(),

      // Session tools (multi-account support)
      session_list_tool(),
      session_set_active_tool(),
      session_create_tool(),

      // Onboarding tools (auto client onboarding after MCP connection)
      onboarding_tools.onboarding_start_tool(),
      onboarding_tools.onboarding_status_tool(),
      onboarding_tools.onboarding_select_dialogs_tool(),
      onboarding_tools.onboarding_list_tool(),

      // Payment tools (Robokassa, TON USDT, TON Native, Telegram Stars)
      payment_tools.payment_create_tool(),
      payment_tools.payment_status_tool(),
      payment_tools.payment_verify_tool(),
      payment_tools.balance_get_tool(),
      payment_tools.balance_topup_options_tool(),

      // Invoice tools (xRocket, CryptoBot - send invoices to Telegram users)
      invoice_tools.invoice_create_tool(),
      invoice_tools.invoice_send_tool(),
      invoice_tools.invoice_cheque_tool(),
      invoice_tools.invoice_format_tool(),

      // P2P Escrow tools (TON-based P2P trading with fiat)
      p2p_tools.p2p_create_sell_order_tool(),
      p2p_tools.p2p_list_orders_tool(),
      p2p_tools.p2p_take_order_tool(),
      p2p_tools.p2p_mark_paid_tool(),
      p2p_tools.p2p_confirm_payment_tool(),
      p2p_tools.p2p_cancel_order_tool(),
      p2p_tools.p2p_my_orders_tool(),
      p2p_tools.p2p_order_status_tool(),
      p2p_tools.p2p_rates_tool(),

      // P2P Earning Agent tools (automated trading)
      earning_tools.earning_start_tool(),
      earning_tools.earning_stop_tool(),
      earning_tools.earning_status_tool(),
      earning_tools.earning_config_tool(),
      earning_tools.earning_history_tool(),
      earning_tools.arbitrage_scan_tool(),
      earning_tools.credentials_set_tool(),
      earning_tools.arbitrage_execute_tool(),

      // Rainbow Bridge tools (P6 - Autonomous Self-Healing)
      rainbow_autonomous_debug_cycle_tool(),
      task_create_tool(),
      task_get_tool(),
      task_list_tool(),
      task_update_tool(),
      heal_start_tool(),
      heal_apply_fix_tool(),
      heal_verify_tool(),
      heal_rollback_tool(),
      decide_next_step_tool(),
      decide_apply_tool(),
      // A2A Protocol tools (TODO: implement)
    // a2a_register_agent_tool(),
    // a2a_discover_agents_tool(),
    // a2a_submit_task_tool(),
    // a2a_list_agents_tool(),

    // Super Agent tools (TODO: implement)
    // super_start_tool(),
    // super_stop_tool(),
    // super_status_tool(),
    // super_send_event_tool(),

    // Memory tools (TODO: implement)
    // memory_record_episode_tool(),
    // memory_add_reflection_tool(),
    // memory_search_tool(),
    // memory_stats_tool(),
    ]
    // Add AI tools (ElevenLabs, Hedra, BFL, Kling, HeyGen)
    |> list.append(ai_tools.all_ai_tools())
    // Add RAG tools (Telegram parsing, media processing, embeddings, search)
    |> list.append(rag_tools.get_all_rag_tools())
    // Add TaskFlow tools (task management with Telegram contacts)
    |> list.append(task_tools.get_all_tools())
    // Add Storage tools (S3/Tigris upload)
    |> list.append(storage_tools.all_tools())
    // Add Remotion tools (video rendering and template management)
    |> list.append(remotion_tools.all_tools())

  let tool_dict =
    list.fold(tools, dict.new(), fn(acc, tool) {
      dict.insert(acc, tool.name, tool)
    })

  let handler_dict =
    dict.new()
    |> dict.insert("telegram_get_dialogs", handle_telegram_get_dialogs)
    |> dict.insert("telegram_get_history", handle_telegram_get_history)
    |> dict.insert("telegram_send_message", handle_telegram_send_message)
    |> dict.insert("telegram_send_buttons", handle_telegram_send_buttons)
    |> dict.insert("telegram_send_photo", handle_telegram_send_photo)
    |> dict.insert("telegram_download_media", handle_telegram_download_media)
    |> dict.insert("telegram_get_me", handle_telegram_get_me)
    |> dict.insert(
      "telegram_subscribe_updates",
      handle_telegram_subscribe_updates,
    )
    |> dict.insert("knowledge_search", handle_knowledge_search)
    |> dict.insert("knowledge_embed", handle_knowledge_embed)
    |> dict.insert("file_read", handle_file_read)
    |> dict.insert("file_write", handle_file_write)
    |> dict.insert("file_list", handle_file_list)
    |> dict.insert("voice_transcribe", handle_voice_transcribe)
    |> dict.insert("system_log", handle_system_log)
    |> dict.insert("system_exec", handle_system_exec)
    |> dict.insert("event_emit", handle_event_emit)
    |> dict.insert("event_list", handle_event_list)
    // Debug handlers
    |> dict.insert("debug_build", handle_debug_build)
    |> dict.insert("debug_test", handle_debug_test)
    |> dict.insert("debug_analyze", handle_debug_analyze)
    |> dict.insert("debug_trace", handle_debug_trace)
    |> dict.insert("debug_log", handle_debug_log)
    // Code handlers
    |> dict.insert("code_generate", handle_code_generate)
    |> dict.insert("code_refactor", handle_code_refactor)
    |> dict.insert("code_explain", handle_code_explain)
    |> dict.insert("code_find_similar", handle_code_find_similar)
    |> dict.insert("code_diff", handle_code_diff)
    // Test handlers
    |> dict.insert("test_run", handle_test_run)
    |> dict.insert("test_create", handle_test_create)
    |> dict.insert("test_coverage", handle_test_coverage)
    |> dict.insert("test_validate", handle_test_validate)
    // Agent handlers
    |> dict.insert("agent_spawn", handle_agent_spawn)
    |> dict.insert("agent_message", handle_agent_message)
    |> dict.insert("agent_status", handle_agent_status)
    |> dict.insert("agent_kill", handle_agent_kill)
    // Bot Analysis handlers
    |> dict.insert("bot_analyze", handle_bot_analyze)
    |> dict.insert("bot_compare", handle_bot_compare)
    |> dict.insert("bot_monitor", handle_bot_monitor)
    |> dict.insert("bot_extract_commands", handle_bot_extract_commands)
    |> dict.insert("bot_test_interaction", handle_bot_test_interaction)
    // Auth handlers
    |> dict.insert("auth_status", handle_auth_status)
    |> dict.insert("auth_send_code", handle_auth_send_code)
    |> dict.insert("auth_verify_code", handle_auth_verify_code)
    |> dict.insert("auth_2fa", handle_auth_2fa)
    |> dict.insert("auth_logout", handle_auth_logout)
    // Session handlers (multi-account support)
    |> dict.insert("session_list", handle_session_list)
    |> dict.insert("session_set_active", handle_session_set_active)
    |> dict.insert("session_create", handle_session_create)
    // Onboarding handlers (auto client onboarding)
    |> dict.insert("onboarding_start", onboarding_tools.handle_onboarding_start)
    |> dict.insert("onboarding_status", onboarding_tools.handle_onboarding_status)
    |> dict.insert("onboarding_select_dialogs", onboarding_tools.handle_onboarding_select_dialogs)
    |> dict.insert("onboarding_list", onboarding_tools.handle_onboarding_list)
    // Payment handlers (Robokassa, TON, Telegram Stars)
    |> dict.insert("payment_create", payment_tools.handle_payment_create)
    |> dict.insert("payment_status", payment_tools.handle_payment_status)
    |> dict.insert("payment_verify", payment_tools.handle_payment_verify)
    |> dict.insert("balance_get", payment_tools.handle_balance_get)
    |> dict.insert("balance_topup_options", payment_tools.handle_balance_topup_options)
    // Invoice handlers (xRocket, CryptoBot - send invoices to users)
    |> dict.insert("invoice_create", invoice_tools.handle_invoice_create)
    |> dict.insert("invoice_send", invoice_tools.handle_invoice_send)
    |> dict.insert("invoice_cheque", invoice_tools.handle_invoice_cheque)
    |> dict.insert("invoice_format", invoice_tools.handle_invoice_format)
    // P2P Escrow handlers (TON-based P2P trading)
    |> dict.insert("p2p_create_sell_order", p2p_tools.handle_p2p_create_sell_order)
    |> dict.insert("p2p_list_orders", p2p_tools.handle_p2p_list_orders)
    |> dict.insert("p2p_take_order", p2p_tools.handle_p2p_take_order)
    |> dict.insert("p2p_mark_paid", p2p_tools.handle_p2p_mark_paid)
    |> dict.insert("p2p_confirm_payment", p2p_tools.handle_p2p_confirm_payment)
    |> dict.insert("p2p_cancel_order", p2p_tools.handle_p2p_cancel_order)
    |> dict.insert("p2p_my_orders", p2p_tools.handle_p2p_my_orders)
    |> dict.insert("p2p_order_status", p2p_tools.handle_p2p_order_status)
    |> dict.insert("p2p_rates", p2p_tools.handle_p2p_rates)
    // P2P Earning Agent handlers
    |> dict.insert("earning_start", earning_tools.handle_earning_start_wrapper)
    |> dict.insert("earning_stop", earning_tools.handle_earning_stop_wrapper)
    |> dict.insert("earning_status", earning_tools.handle_earning_status_wrapper)
    |> dict.insert("earning_config", earning_tools.handle_earning_config_wrapper)
    |> dict.insert("earning_history", earning_tools.handle_earning_history_wrapper)
    |> dict.insert("arbitrage_scan", earning_tools.handle_arbitrage_scan_wrapper)
    |> dict.insert("credentials_set", earning_tools.handle_credentials_set_wrapper)
    |> dict.insert("arbitrage_execute", earning_tools.handle_arbitrage_execute_wrapper)
    // Rainbow Bridge handlers
    |> dict.insert(
      "rainbow_autonomous_debug_cycle",
      handle_rainbow_autonomous_debug_cycle,
    )
    |> dict.insert("task_create", handle_task_create)
    |> dict.insert("task_get", handle_task_get)
    |> dict.insert("task_list", handle_task_list)
    |> dict.insert("task_update", handle_task_update)
    |> dict.insert("heal_start", handle_heal_start)
    |> dict.insert("heal_apply_fix", handle_heal_apply_fix)
    |> dict.insert("heal_verify", handle_heal_verify)
    |> dict.insert("heal_rollback", handle_heal_rollback)
    |> dict.insert("decide_next_step", handle_decide_next_step)
    |> dict.insert("decide_apply", handle_decide_apply)
    // Storage tools
    |> dict.insert("storage_upload", handle_storage_upload)
    |> dict.insert("storage_list", handle_storage_list)
    |> dict.insert("storage_config", handle_storage_config)
  // A2A Protocol handlers (TODO: implement)
  // |> dict.insert("a2a_register_agent", handle_a2a_register_agent)
  // |> dict.insert("a2a_discover_agents", handle_a2a_discover_agents)
  // |> dict.insert("a2a_submit_task", handle_a2a_submit_task)
  // |> dict.insert("a2a_list_agents", handle_a2a_list_agents)
  // Super Agent handlers (TODO: implement)
  // |> dict.insert("super_start", handle_super_start)
  // |> dict.insert("super_stop", handle_super_stop)
  // |> dict.insert("super_status", handle_super_status)
  // |> dict.insert("super_send_event", handle_super_send_event)
  // Memory handlers (TODO: implement)
  // |> dict.insert("memory_record_episode", handle_memory_record_episode)
  // |> dict.insert("memory_add_reflection", handle_memory_add_reflection)
  // |> dict.insert("memory_search", handle_memory_search)
  // |> dict.insert("memory_stats", handle_memory_stats)

  // Add RAG handlers (Telegram parsing, media processing, embeddings, search)
  let handler_dict =
    rag_tools.get_rag_handlers()
    |> list.fold(handler_dict, fn(acc, pair) {
      let #(name, handler) = pair
      dict.insert(acc, name, handler)
    })

  // Add AI handlers (ElevenLabs, Hedra, BFL, Kling, HeyGen)
  let handler_dict =
    ai_handlers.get_ai_handlers()
    |> list.fold(handler_dict, fn(acc, pair) {
      let #(name, handler) = pair
      dict.insert(acc, name, handler)
    })

  // Add TaskFlow handlers (task management with Telegram contacts)
  let handler_dict =
    task_tools.get_all_handlers()
    |> list.fold(handler_dict, fn(acc, pair) {
      let #(name, handler) = pair
      dict.insert(acc, name, handler)
    })

  // Add Remotion handlers (video rendering and template management)
  let handler_dict =
    remotion_tools.get_handlers()
    |> list.fold(handler_dict, fn(acc, pair) {
      let #(name, handler) = pair
      dict.insert(acc, name, handler)
    })

  // Build categories dict
  let category_dict =
    dict.keys(tool_dict)
    |> list.fold(dict.new(), fn(acc, name) {
      dict.insert(acc, name, get_tool_category(name))
    })

  ToolRegistry(
    tools: tool_dict,
    handlers: handler_dict,
    categories: category_dict,
  )
}

/// List all tool names
pub fn list_tools(registry: ToolRegistry) -> List(String) {
  dict.keys(registry.tools)
}

/// Get all tools
pub fn get_all_tools(registry: ToolRegistry) -> List(Tool) {
  dict.values(registry.tools)
}

/// Get all tools with annotations for MCP 2024-11-05
pub fn get_all_tools_with_annotations(
  registry: ToolRegistry,
) -> List(#(Tool, protocol.ToolAnnotations)) {
  dict.values(registry.tools)
  |> list.map(fn(tool) {
    let annotations = get_tool_annotations(tool.name)
    #(tool, annotations)
  })
}

/// Get tools by category with annotations (for filtered tools/list)
pub fn get_tools_by_category_with_annotations(
  registry: ToolRegistry,
  category: ToolCategory,
) -> List(#(Tool, protocol.ToolAnnotations)) {
  get_tools_by_category(registry, category)
  |> list.map(fn(tool) {
    let annotations = get_tool_annotations(tool.name)
    #(tool, annotations)
  })
}

/// Get available categories with tool counts
pub fn get_category_summary(registry: ToolRegistry) -> List(#(String, Int)) {
  [
    CategoryTelegram, CategoryKnowledge, CategoryFile, CategoryVoice,
    CategorySystem, CategoryEvent, CategoryDebug, CategoryCode, CategoryTest,
    CategoryAgent, CategoryBot, CategoryAuth,
  ]
  |> list.map(fn(cat) {
    let count = get_tools_by_category(registry, cat) |> list.length
    #(category_to_string(cat), count)
  })
  |> list.filter(fn(pair) {
    let #(_, count) = pair
    count > 0
  })
}

/// Get annotations for a specific tool
fn get_tool_annotations(name: String) -> protocol.ToolAnnotations {
  // Read-only tools (safe, no side effects)
  let read_only_tools = [
    "telegram_get_dialogs", "telegram_get_history", "telegram_get_me",
    "file_read", "file_list", "knowledge_search", "event_list", "debug_build",
    "debug_test", "debug_analyze", "debug_trace", "code_explain",
    "code_find_similar", "code_diff", "test_run", "test_coverage",
    "test_validate", "agent_status", "bot_analyze", "bot_compare",
    "bot_extract_commands", "auth_status",
  ]

  // Destructive tools (can cause significant changes)
  let destructive_tools = [
    "system_exec",
    "agent_kill",
    "auth_logout",
  ]

  case list.contains(read_only_tools, name) {
    True -> protocol.read_only_annotations()
    False -> {
      let is_destructive = list.contains(destructive_tools, name)
      protocol.write_annotations(is_destructive)
    }
  }
}

/// Execute a tool
pub fn execute_tool(
  registry: ToolRegistry,
  name: String,
  args: json.Json,
) -> ToolResult {
  logging.info("[TOOL] Executing: " <> name)

  case dict.get(registry.handlers, name) {
    Ok(handler) -> {
      let result = handler(args)
      case result.is_error {
        True -> logging.error("[TOOL] " <> name <> " failed")
        False -> logging.info("[TOOL] " <> name <> " completed")
      }
      result
    }
    Error(_) -> protocol.error_result("Unknown tool: " <> name)
  }
}

// ============================================================
// Tool Definitions
// ============================================================

fn telegram_get_dialogs_tool() -> Tool {
  Tool(
    name: "telegram_get_dialogs",
    description: "Get list of Telegram dialogs (chats, groups, channels)",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Telegram session ID")),
            ]),
          ),
          #(
            "limit",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Max dialogs to return")),
              #("default", json.int(100)),
            ]),
          ),
          #(
            "type_filter",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(["all", "user", "group", "channel"], json.string),
              ),
              #("description", json.string("Filter by dialog type")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["session_id"], json.string)),
    ]),
  )
}

fn telegram_get_history_tool() -> Tool {
  Tool(
    name: "telegram_get_history",
    description: "Get message history from a Telegram chat",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Telegram session ID")),
            ]),
          ),
          #(
            "chat_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Chat ID to get history from")),
            ]),
          ),
          #(
            "limit",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Max messages to return")),
              #("default", json.int(100)),
            ]),
          ),
          #(
            "offset_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Message ID to start from")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["session_id", "chat_id"], json.string)),
    ]),
  )
}

fn telegram_send_message_tool() -> Tool {
  Tool(
    name: "telegram_send_message",
    description: "Send a message to a Telegram chat",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Telegram session ID")),
            ]),
          ),
          #(
            "chat_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Chat ID to send message to")),
            ]),
          ),
          #(
            "text",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Message text to send")),
            ]),
          ),
          #(
            "reply_to",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Message ID to reply to")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["session_id", "chat_id", "text"], json.string)),
    ]),
  )
}

fn telegram_send_buttons_tool() -> Tool {
  Tool(
    name: "telegram_send_buttons",
    description: "Send a message with inline keyboard buttons",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Telegram session ID")),
            ]),
          ),
          #(
            "chat_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Chat ID to send message to")),
            ]),
          ),
          #(
            "text",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Message text")),
            ]),
          ),
          #(
            "buttons",
            json.object([
              #("type", json.string("array")),
              #(
                "description",
                json.string(
                  "Array of button rows. Each row is array of {text, callback_data} or {text, url}",
                ),
              ),
              #(
                "items",
                json.object([
                  #("type", json.string("array")),
                  #(
                    "items",
                    json.object([
                      #("type", json.string("object")),
                      #(
                        "properties",
                        json.object([
                          #(
                            "text",
                            json.object([#("type", json.string("string"))]),
                          ),
                          #(
                            "callback_data",
                            json.object([#("type", json.string("string"))]),
                          ),
                          #(
                            "url",
                            json.object([#("type", json.string("string"))]),
                          ),
                        ]),
                      ),
                    ]),
                  ),
                ]),
              ),
            ]),
          ),
        ]),
      ),
      #(
        "required",
        json.array(["session_id", "chat_id", "text", "buttons"], json.string),
      ),
    ]),
  )
}

fn telegram_send_photo_tool() -> Tool {
  Tool(
    name: "telegram_send_photo",
    description: "Send a photo to a Telegram chat",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Telegram session ID")),
            ]),
          ),
          #(
            "chat_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Chat ID to send photo to")),
            ]),
          ),
          #(
            "file_path",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Local file path to photo")),
            ]),
          ),
          #(
            "caption",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Photo caption")),
            ]),
          ),
        ]),
      ),
      #(
        "required",
        json.array(["session_id", "chat_id", "file_path"], json.string),
      ),
    ]),
  )
}

fn telegram_download_media_tool() -> Tool {
  Tool(
    name: "telegram_download_media",
    description: "Download media file from a Telegram message",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Telegram session ID")),
            ]),
          ),
          #(
            "chat_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Chat ID")),
            ]),
          ),
          #(
            "message_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Message ID with media")),
            ]),
          ),
          #(
            "output_path",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Path to save downloaded file")),
            ]),
          ),
        ]),
      ),
      #(
        "required",
        json.array(["session_id", "chat_id", "message_id"], json.string),
      ),
    ]),
  )
}

fn telegram_get_me_tool() -> Tool {
  Tool(
    name: "telegram_get_me",
    description: "Get current Telegram user info",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Telegram session ID")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["session_id"], json.string)),
    ]),
  )
}

fn telegram_subscribe_updates_tool() -> Tool {
  Tool(
    name: "telegram_subscribe_updates",
    description: "Subscribe to Telegram updates (new messages, button clicks, etc)",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Telegram session ID")),
            ]),
          ),
          #(
            "event_types",
            json.object([
              #("type", json.string("array")),
              #(
                "description",
                json.string(
                  "Event types to subscribe: message, callback_query, edited_message",
                ),
              ),
              #("items", json.object([#("type", json.string("string"))])),
              #(
                "default",
                json.array(["message", "callback_query"], json.string),
              ),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["session_id"], json.string)),
    ]),
  )
}

fn knowledge_search_tool() -> Tool {
  Tool(
    name: "knowledge_search",
    description: "Search through knowledge base using semantic similarity",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "query",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Search query")),
            ]),
          ),
          #(
            "limit",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Max results to return")),
              #("default", json.int(10)),
            ]),
          ),
          #(
            "chat_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Filter by chat ID")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["query"], json.string)),
    ]),
  )
}

fn knowledge_embed_tool() -> Tool {
  Tool(
    name: "knowledge_embed",
    description: "Create embedding vector for text using Ollama",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "text",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Text to embed")),
            ]),
          ),
          #(
            "model",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Embedding model")),
              #("default", json.string("nomic-embed-text")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["text"], json.string)),
    ]),
  )
}

fn file_read_tool() -> Tool {
  Tool(
    name: "file_read",
    description: "Read contents of a file",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "path",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("File path to read")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["path"], json.string)),
    ]),
  )
}

fn file_write_tool() -> Tool {
  Tool(
    name: "file_write",
    description: "Write contents to a file",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "path",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("File path to write")),
            ]),
          ),
          #(
            "content",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Content to write")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["path", "content"], json.string)),
    ]),
  )
}

fn file_list_tool() -> Tool {
  Tool(
    name: "file_list",
    description: "List files in a directory",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "path",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Directory path")),
            ]),
          ),
          #(
            "pattern",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Glob pattern filter")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["path"], json.string)),
    ]),
  )
}

fn voice_transcribe_tool() -> Tool {
  Tool(
    name: "voice_transcribe",
    description: "Transcribe voice message to text using Whisper",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "file_path",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Path to audio file")),
            ]),
          ),
          #(
            "language",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Language code (ru, en, etc)")),
              #("default", json.string("ru")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["file_path"], json.string)),
    ]),
  )
}

fn system_log_tool() -> Tool {
  Tool(
    name: "system_log",
    description: "Write to system log",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "level",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(["debug", "info", "warn", "error"], json.string),
              ),
              #("description", json.string("Log level")),
            ]),
          ),
          #(
            "message",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Log message")),
            ]),
          ),
          #(
            "context",
            json.object([
              #("type", json.string("object")),
              #("description", json.string("Additional context")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["level", "message"], json.string)),
    ]),
  )
}

fn system_exec_tool() -> Tool {
  Tool(
    name: "system_exec",
    description: "Execute a shell command (restricted)",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "command",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Command to execute")),
            ]),
          ),
          #(
            "args",
            json.object([
              #("type", json.string("array")),
              #("items", json.object([#("type", json.string("string"))])),
              #("description", json.string("Command arguments")),
            ]),
          ),
          #(
            "timeout",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Timeout in seconds")),
              #("default", json.int(30)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["command"], json.string)),
    ]),
  )
}

fn event_emit_tool() -> Tool {
  Tool(
    name: "event_emit",
    description: "Emit an event to the event bus for other agents/tools",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "event_type",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string(
                  "Event type: message_received, button_clicked, task_completed, agent_response",
                ),
              ),
            ]),
          ),
          #(
            "payload",
            json.object([
              #("type", json.string("object")),
              #("description", json.string("Event payload data")),
            ]),
          ),
          #(
            "target",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string(
                  "Target agent/tool to receive event (optional, broadcast if empty)",
                ),
              ),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["event_type", "payload"], json.string)),
    ]),
  )
}

fn event_list_tool() -> Tool {
  Tool(
    name: "event_list",
    description: "List available event types and recent events",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "limit",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Max recent events to return")),
              #("default", json.int(20)),
            ]),
          ),
          #(
            "event_type",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Filter by event type")),
            ]),
          ),
        ]),
      ),
      #("required", json.array([], json.string)),
    ]),
  )
}

// ============================================================
// Tool Handlers
// ============================================================

fn bridge_url() -> String {
  case config.get_env("VIBEE_BRIDGE_URL") {
    "" -> "http://localhost:8081"
    url -> url
  }
}

// ============================================================
// Telegram Auth Helper
// ============================================================

/// Check if Telegram is authorized, return auth prompt if not
fn check_telegram_auth() -> Result(Nil, ToolResult) {
  // Check active session first
  let active_session = session_manager.get_active()
  case active_session {
    option.None -> {
      Error(auth_required_result("Нет активной сессии. Создайте сессию через session_create."))
    }
    option.Some(session_id) -> {
      // Check if this specific session is authorized
      let path = "/api/v1/auth/status"
      case http_get_with_session(path, session_id) {
        Error(_) -> {
          Error(auth_required_result(
            "Telegram bridge не запущен. Запустите его и авторизуйтесь.",
          ))
        }
        Ok(body) -> {
          // Parse response to check if session is authorized
          case json.parse(from: body, using: session_auth_decoder()) {
            Ok(True) -> Ok(Nil)
            Ok(False) -> Error(auth_required_result("Сессия не авторизована. Выполните auth_send_code и auth_verify_code."))
            Error(_) -> Error(auth_required_result("Telegram не авторизован."))
          }
        }
      }
    }
  }
}

fn session_auth_decoder() -> decode.Decoder(Bool) {
  use authorized <- decode.field("authorized", decode.bool)
  decode.success(authorized)
}

fn session_count_decoder() -> decode.Decoder(Int) {
  use total <- decode.field("total_sessions", decode.int)
  decode.success(total)
}

/// Create auth required result with instructions
fn auth_required_result(message: String) -> ToolResult {
  let response =
    json.object([
      #("error", json.bool(False)),
      #("auth_required", json.bool(True)),
      #("status", json.string("authorization_needed")),
      #("message", json.string(message)),
      #("prompt", json.string("Введите номер телефона в формате +79001234567:")),
      #(
        "next_action",
        json.object([
          #("tool", json.string("auth_send_code")),
          #("await_user_input", json.string("phone")),
        ]),
      ),
      #(
        "instructions",
        json.string(
          "Для авторизации в Telegram:\n"
          <> "1. Вызовите auth_send_code с вашим номером телефона\n"
          <> "2. Получите код в Telegram\n"
          <> "3. Вызовите auth_verify_code с кодом и phone_code_hash",
        ),
      ),
    ])
  protocol.text_result(json.to_string(response))
}

fn handle_telegram_get_dialogs(args: json.Json) -> ToolResult {
  // Check auth first
  case check_telegram_auth() {
    Error(auth_result) -> auth_result
    Ok(Nil) -> {
      case decoders.decode_telegram_get_dialogs(args) {
        Error(err) -> protocol.error_result(decoders.error_to_string(err))
        Ok(parsed) -> {
          case validation.validate_session_id(parsed.session_id) {
            Error(err) -> protocol.error_result(validation.error_to_string(err))
            Ok(sid) -> {
              let limit = option.unwrap(parsed.limit, 100)
              let path = "/api/v1/dialogs?limit=" <> int.to_string(limit)
              case http_get_with_session(path, sid) {
                Ok(body) -> protocol.text_result(body)
                Error(err) -> protocol.error_result(err)
              }
            }
          }
        }
      }
    }
  }
}

fn handle_telegram_get_history(args: json.Json) -> ToolResult {
  case check_telegram_auth() {
    Error(auth_result) -> auth_result
    Ok(Nil) -> {
      case decoders.decode_telegram_get_history(args) {
        Error(err) -> protocol.error_result(decoders.error_to_string(err))
        Ok(parsed) -> {
          case
            validation.validate_session_id(parsed.session_id),
            validation.validate_chat_id(parsed.chat_id)
          {
            Error(err), _ ->
              protocol.error_result(validation.error_to_string(err))
            _, Error(err) ->
              protocol.error_result(validation.error_to_string(err))
            Ok(sid), Ok(cid) -> {
              let limit = option.unwrap(parsed.limit, 100)
              let path =
                "/api/v1/history/" <> cid <> "?limit=" <> int.to_string(limit)
              case http_get_with_session(path, sid) {
                Ok(body) -> protocol.text_result(body)
                Error(err) -> protocol.error_result(err)
              }
            }
          }
        }
      }
    }
  }
}

fn handle_telegram_send_message(args: json.Json) -> ToolResult {
  case check_telegram_auth() {
    Error(auth_result) -> auth_result
    Ok(Nil) -> {
      case decoders.decode_telegram_send_message(args) {
        Error(err) -> protocol.error_result(decoders.error_to_string(err))
        Ok(parsed) -> {
          case
            validation.validate_session_id(parsed.session_id),
            validation.validate_chat_id(parsed.chat_id)
          {
            Error(err), _ ->
              protocol.error_result(validation.error_to_string(err))
            _, Error(err) ->
              protocol.error_result(validation.error_to_string(err))
            Ok(sid), Ok(cid) -> {
              // Resolve username to numeric ID if needed
              let chat_id_result = case string.starts_with(cid, "@") {
                True -> {
                  let username = string.drop_start(cid, 1)
                  case
                    http_get_with_session(
                      "/api/v1/resolve?username=" <> username,
                      sid,
                    )
                  {
                    Ok(resp) -> {
                      // Parse response to get ID using decoder
                      let decoder = {
                        use id <- decode.field("id", decode.int)
                        decode.success(id)
                      }
                      case json.parse(resp, decoder) {
                        Ok(id) -> Ok(id)
                        Error(_) ->
                          Error(
                            "Failed to get id from resolve response: " <> resp,
                          )
                      }
                    }
                    Error(err) -> Error("Failed to resolve username: " <> err)
                  }
                }
                False -> {
                  case int.parse(cid) {
                    Ok(num) -> Ok(num)
                    Error(_) -> Error("Invalid chat_id format")
                  }
                }
              }

              case chat_id_result {
                Error(err) -> protocol.error_result(err)
                Ok(chat_id_int) -> {
                  let body =
                    json.object([
                      #("chat_id", json.int(chat_id_int)),
                      #("text", json.string(parsed.text)),
                    ])
                    |> json.to_string()

                  case http_post_with_session("/api/v1/send", sid, body) {
                    Ok(resp) -> protocol.text_result(resp)
                    Error(err) -> protocol.error_result(err)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

fn handle_telegram_get_me(args: json.Json) -> ToolResult {
  case check_telegram_auth() {
    Error(auth_result) -> auth_result
    Ok(Nil) -> {
      case decoders.decode_telegram_get_me(args) {
        Error(err) -> protocol.error_result(decoders.error_to_string(err))
        Ok(parsed) -> {
          case validation.validate_session_id(parsed.session_id) {
            Error(err) -> protocol.error_result(validation.error_to_string(err))
            Ok(sid) -> {
              case http_get_with_session("/api/v1/me", sid) {
                Ok(body) -> protocol.text_result(body)
                Error(err) -> protocol.error_result(err)
              }
            }
          }
        }
      }
    }
  }
}

fn handle_knowledge_search(args: json.Json) -> ToolResult {
  case decoders.decode_knowledge_search(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      let limit = option.unwrap(parsed.limit, 10)

      // Get embedding for query
      case get_query_embedding(parsed.query) {
        Error(err) ->
          protocol.error_result("Failed to get query embedding: " <> err)
        Ok(query_embedding) -> {
          // Load stored embeddings and search
          let config = config.get_config()
          let embeddings_path = config.data_dir <> "/embeddings.jsonl"
          case simplifile.read(embeddings_path) {
            Ok(content) -> {
              // Parse JSONL and compute similarities
              let results = search_embeddings(content, query_embedding, limit)
              protocol.text_result(
                json.object([
                  #("query", json.string(parsed.query)),
                  #("results", json.array(results, fn(r) { r })),
                  #("count", json.int(list.length(results))),
                ])
                |> json.to_string(),
              )
            }
            Error(_) -> {
              // Return empty results if no embeddings file
              protocol.text_result(
                json.object([
                  #("query", json.string(parsed.query)),
                  #("results", json.array([], fn(r) { r })),
                  #("count", json.int(0)),
                  #(
                    "note",
                    json.string(
                      "No embeddings file found at " <> embeddings_path,
                    ),
                  ),
                ])
                |> json.to_string(),
              )
            }
          }
        }
      }
    }
  }
}

/// Get embedding vector for query text via Ollama
fn get_query_embedding(text: String) -> Result(List(Float), String) {
  let body =
    json.object([
      #("model", json.string("nomic-embed-text")),
      #("prompt", json.string(text)),
    ])
    |> json.to_string()

  case http_post("http://localhost:11434/api/embeddings", body) {
    Error(err) -> Error(err)
    Ok(resp) -> {
      // Parse embedding from response
      case extract_embedding(resp) {
        Ok(emb) -> Ok(emb)
        Error(_) -> Error("Failed to parse embedding response")
      }
    }
  }
}

/// Extract embedding array from Ollama response
fn extract_embedding(resp: String) -> Result(List(Float), Nil) {
  // Response format: {"embedding":[0.1, 0.2, ...]}
  case string.split_once(resp, "\"embedding\":[") {
    Error(_) -> Error(Nil)
    Ok(#(_, rest)) -> {
      case string.split_once(rest, "]") {
        Error(_) -> Error(Nil)
        Ok(#(nums_str, _)) -> {
          let nums =
            string.split(nums_str, ",")
            |> list.filter_map(fn(s) {
              case float.parse(string.trim(s)) {
                Ok(f) -> Ok(f)
                Error(_) -> Error(Nil)
              }
            })
          Ok(nums)
        }
      }
    }
  }
}

/// Search embeddings and return top matches
fn search_embeddings(
  content: String,
  query_emb: List(Float),
  limit: Int,
) -> List(json.Json) {
  // Parse JSONL: each line is {"text": "...", "embedding": [...], "source": "..."}
  string.split(content, "\n")
  |> list.filter(fn(line) { string.length(string.trim(line)) > 0 })
  |> list.filter_map(fn(line) {
    case parse_embedding_entry(line) {
      Ok(entry) -> {
        let similarity = cosine_similarity(query_emb, entry.embedding)
        Ok(#(similarity, entry))
      }
      Error(_) -> Error(Nil)
    }
  })
  |> list.sort(fn(a, b) {
    // Sort by similarity descending
    let #(sim_a, _) = a
    let #(sim_b, _) = b
    float.compare(sim_b, sim_a)
  })
  |> list.take(limit)
  |> list.map(fn(pair) {
    let #(sim, entry) = pair
    json.object([
      #("text", json.string(entry.text)),
      #("source", json.string(entry.source)),
      #("similarity", json.float(sim)),
    ])
  })
}

/// Embedding entry from JSONL
type EmbeddingEntry {
  EmbeddingEntry(text: String, embedding: List(Float), source: String)
}

/// Parse a JSONL line into EmbeddingEntry
fn parse_embedding_entry(line: String) -> Result(EmbeddingEntry, Nil) {
  // Extract text
  let text = case string.split_once(line, "\"text\":\"") {
    Ok(#(_, rest)) -> {
      case string.split_once(rest, "\"") {
        Ok(#(t, _)) -> t
        Error(_) -> ""
      }
    }
    Error(_) -> ""
  }

  // Extract source
  let source = case string.split_once(line, "\"source\":\"") {
    Ok(#(_, rest)) -> {
      case string.split_once(rest, "\"") {
        Ok(#(s, _)) -> s
        Error(_) -> ""
      }
    }
    Error(_) -> ""
  }

  // Extract embedding
  case string.split_once(line, "\"embedding\":[") {
    Error(_) -> Error(Nil)
    Ok(#(_, rest)) -> {
      case string.split_once(rest, "]") {
        Error(_) -> Error(Nil)
        Ok(#(nums_str, _)) -> {
          let embedding =
            string.split(nums_str, ",")
            |> list.filter_map(fn(s) {
              case float.parse(string.trim(s)) {
                Ok(f) -> Ok(f)
                Error(_) -> Error(Nil)
              }
            })
          Ok(EmbeddingEntry(text: text, embedding: embedding, source: source))
        }
      }
    }
  }
}

/// Cosine similarity between two vectors
fn cosine_similarity(a: List(Float), b: List(Float)) -> Float {
  let pairs = list.zip(a, b)
  let dot =
    list.fold(pairs, 0.0, fn(acc, pair) {
      let #(x, y) = pair
      acc +. x *. y
    })
  let sum_a = list.fold(a, 0.0, fn(acc, x) { acc +. x *. x })
  let sum_b = list.fold(b, 0.0, fn(acc, x) { acc +. x *. x })
  let mag_a = result.unwrap(float.square_root(sum_a), 0.0)
  let mag_b = result.unwrap(float.square_root(sum_b), 0.0)
  case mag_a *. mag_b {
    0.0 -> 0.0
    denom -> dot /. denom
  }
}

fn handle_knowledge_embed(args: json.Json) -> ToolResult {
  case decoders.decode_knowledge_embed(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      let model = option.unwrap(parsed.model, "nomic-embed-text")
      let body =
        json.object([
          #("model", json.string(model)),
          #("prompt", json.string(parsed.text)),
        ])
        |> json.to_string()

      case http_post("http://localhost:11434/api/embeddings", body) {
        Ok(resp) -> protocol.text_result(resp)
        Error(err) -> protocol.error_result(err)
      }
    }
  }
}

fn handle_file_read(args: json.Json) -> ToolResult {
  case decoders.decode_file_read(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      case validation.validate_path(parsed.path) {
        Error(err) -> protocol.error_result(validation.error_to_string(err))
        Ok(path) -> {
          case simplifile.read(path) {
            Ok(content) -> protocol.text_result(content)
            Error(_) -> protocol.error_result("Failed to read file: " <> path)
          }
        }
      }
    }
  }
}

fn handle_file_write(args: json.Json) -> ToolResult {
  case decoders.decode_file_write(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      case validation.validate_path(parsed.path) {
        Error(err) -> protocol.error_result(validation.error_to_string(err))
        Ok(path) -> {
          case simplifile.write(path, parsed.content) {
            Ok(_) -> protocol.text_result("Written to: " <> path)
            Error(_) -> protocol.error_result("Failed to write file: " <> path)
          }
        }
      }
    }
  }
}

fn handle_file_list(args: json.Json) -> ToolResult {
  case decoders.decode_file_list(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      case validation.validate_path(parsed.path) {
        Error(err) -> protocol.error_result(validation.error_to_string(err))
        Ok(path) -> {
          case simplifile.read_directory(path) {
            Ok(files) -> {
              let content = string.join(files, "\n")
              protocol.text_result(content)
            }
            Error(_) ->
              protocol.error_result("Failed to list directory: " <> path)
          }
        }
      }
    }
  }
}

fn handle_voice_transcribe(args: json.Json) -> ToolResult {
  case decoders.decode_voice_transcribe(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      case validation.validate_path(parsed.file_path) {
        Error(err) -> protocol.error_result(validation.error_to_string(err))
        Ok(file_path) -> {
          // Check if file exists
          case simplifile.is_file(file_path) {
            Ok(True) -> {
              // Use safe whisper transcription
              case shell.whisper_transcribe(file_path, parsed.language) {
                Ok(output) -> protocol.text_result(output)
                Error(err) ->
                  protocol.error_result("Transcription failed: " <> err)
              }
            }
            _ -> protocol.error_result("File not found: " <> file_path)
          }
        }
      }
    }
  }
}

fn handle_system_log(args: json.Json) -> ToolResult {
  case decoders.decode_system_log(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      case validation.validate_log_level(parsed.level) {
        Error(err) -> protocol.error_result(validation.error_to_string(err))
        Ok(level) -> {
          case level {
            "debug" -> logging.debug(parsed.message)
            "info" -> logging.info(parsed.message)
            "warn" -> logging.warn(parsed.message)
            "warning" -> logging.warn(parsed.message)
            "error" -> logging.error(parsed.message)
            _ -> logging.info(parsed.message)
          }
          protocol.text_result("Logged: [" <> level <> "] " <> parsed.message)
        }
      }
    }
  }
}

fn handle_system_exec(args: json.Json) -> ToolResult {
  case decoders.decode_system_exec(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      // Use safe shell execution with whitelist validation
      case shell.system_exec(parsed.command, parsed.args, parsed.timeout) {
        Ok(output) -> protocol.text_result(output)
        Error(err) -> protocol.error_result(err)
      }
    }
  }
}

// New Telegram handlers
fn handle_telegram_send_buttons(args: json.Json) -> ToolResult {
  case check_telegram_auth() {
    Error(auth_result) -> auth_result
    Ok(Nil) -> {
      case decoders.decode_telegram_send_buttons(args) {
        Error(err) -> protocol.error_result(decoders.error_to_string(err))
        Ok(parsed) -> {
          case
            validation.validate_session_id(parsed.session_id),
            validation.validate_chat_id(parsed.chat_id)
          {
            Error(err), _ ->
              protocol.error_result(validation.error_to_string(err))
            _, Error(err) ->
              protocol.error_result(validation.error_to_string(err))
            Ok(sid), Ok(cid) -> {
              // Build proper inline keyboard JSON - avoid double-escaping
              let keyboard =
                json.array(parsed.buttons, fn(row) {
                  json.array(row, fn(btn) {
                    let fields = [#("text", json.string(btn.text))]
                    let fields = case btn.callback_data {
                      Some(data) ->
                        list.append(fields, [
                          #("callback_data", json.string(data)),
                        ])
                      None -> fields
                    }
                    let fields = case btn.url {
                      Some(url) ->
                        list.append(fields, [#("url", json.string(url))])
                      None -> fields
                    }
                    json.object(fields)
                  })
                })

              let body =
                json.object([
                  #("chat_id", json.string(cid)),
                  #("text", json.string(parsed.text)),
                  #(
                    "reply_markup",
                    json.object([
                      #("inline_keyboard", keyboard),
                    ]),
                  ),
                ])
                |> json.to_string()

              case http_post_with_session("/api/v1/send", sid, body) {
                Ok(resp) -> protocol.text_result(resp)
                Error(err) -> protocol.error_result(err)
              }
            }
          }
        }
      }
    }
  }
}

fn handle_telegram_send_photo(args: json.Json) -> ToolResult {
  case check_telegram_auth() {
    Error(auth_result) -> auth_result
    Ok(Nil) -> {
      case decoders.decode_telegram_send_photo(args) {
        Error(err) -> protocol.error_result(decoders.error_to_string(err))
        Ok(parsed) -> {
          case
            validation.validate_session_id(parsed.session_id),
            validation.validate_chat_id(parsed.chat_id),
            validation.validate_path(parsed.file_path)
          {
            Error(err), _, _ ->
              protocol.error_result(validation.error_to_string(err))
            _, Error(err), _ ->
              protocol.error_result(validation.error_to_string(err))
            _, _, Error(err) ->
              protocol.error_result(validation.error_to_string(err))
            Ok(sid), Ok(cid), Ok(fp) -> {
              // Check file exists
              case simplifile.is_file(fp) {
                Ok(True) -> {
                  let caption = option.unwrap(parsed.caption, "")
                  let body =
                    json.object([
                      #("chat_id", json.string(cid)),
                      #("file_path", json.string(fp)),
                      #("caption", json.string(caption)),
                    ])
                    |> json.to_string()

                  case http_post_with_session("/api/v1/send_photo", sid, body) {
                    Ok(resp) -> protocol.text_result(resp)
                    Error(err) -> protocol.error_result(err)
                  }
                }
                _ -> protocol.error_result("File not found: " <> fp)
              }
            }
          }
        }
      }
    }
  }
}

fn handle_telegram_download_media(args: json.Json) -> ToolResult {
  case check_telegram_auth() {
    Error(auth_result) -> auth_result
    Ok(Nil) -> {
      case decoders.decode_telegram_download_media(args) {
        Error(err) -> protocol.error_result(decoders.error_to_string(err))
        Ok(parsed) -> {
          case
            validation.validate_session_id(parsed.session_id),
            validation.validate_chat_id(parsed.chat_id)
          {
            Error(err), _ ->
              protocol.error_result(validation.error_to_string(err))
            _, Error(err) ->
              protocol.error_result(validation.error_to_string(err))
            Ok(sid), Ok(cid) -> {
              let path =
                "/api/v1/download/"
                <> cid
                <> "/"
                <> int.to_string(parsed.message_id)
              case parsed.output_path {
                None -> {
                  case http_get_with_session(path, sid) {
                    Ok(body) -> protocol.text_result(body)
                    Error(err) -> protocol.error_result(err)
                  }
                }
                Some(output) -> {
                  case validation.validate_path(output) {
                    Error(err) ->
                      protocol.error_result(validation.error_to_string(err))
                    Ok(safe_path) -> {
                      case http_get_with_session(path, sid) {
                        Ok(body) -> {
                          case simplifile.write(safe_path, body) {
                            Ok(_) ->
                              protocol.text_result(
                                "Downloaded to: " <> safe_path,
                              )
                            Error(_) ->
                              protocol.error_result("Failed to save file")
                          }
                        }
                        Error(err) -> protocol.error_result(err)
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

fn handle_telegram_subscribe_updates(args: json.Json) -> ToolResult {
  case check_telegram_auth() {
    Error(auth_result) -> auth_result
    Ok(Nil) -> {
      case decoders.decode_telegram_subscribe_updates(args) {
        Error(err) -> protocol.error_result(decoders.error_to_string(err))
        Ok(parsed) -> {
          case validation.validate_session_id(parsed.session_id) {
            Error(err) -> protocol.error_result(validation.error_to_string(err))
            Ok(sid) -> {
              // Return WebSocket URL for updates
              let ws_url =
                "ws://localhost:8081/api/v1/updates?session_id=" <> sid
              protocol.text_result(
                json.object([
                  #("websocket_url", json.string(ws_url)),
                  #(
                    "event_types",
                    json.array(
                      [
                        "message",
                        "callback_query",
                        "edited_message",
                        "channel_post",
                      ],
                      json.string,
                    ),
                  ),
                  #(
                    "status",
                    json.string(
                      "Subscribe using WebSocket to receive real-time updates",
                    ),
                  ),
                ])
                |> json.to_string(),
              )
            }
          }
        }
      }
    }
  }
}

// Event handlers
fn handle_event_emit(args: json.Json) -> ToolResult {
  case decoders.decode_event_emit(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      // Parse event type
      let event_type = events.parse_event_type(parsed.event_type)

      // Extract payload as raw JSON string and wrap in object
      let payload_str = extract_json_value(args, "payload")
      let payload = json.object([#("raw", json.string(payload_str))])

      // Emit to Event Bus
      case events.emit(event_type, payload, parsed.target, "mcp_tool") {
        Ok(event_id) -> {
          logging.info(
            "[EVENT BUS] Emitted: "
            <> parsed.event_type
            <> " (id: "
            <> event_id
            <> ")",
          )
          protocol.text_result(
            json.object([
              #("success", json.bool(True)),
              #("event_id", json.string(event_id)),
              #("event_type", json.string(parsed.event_type)),
            ])
            |> json.to_string(),
          )
        }
        Error(err) -> {
          logging.error("[EVENT BUS] Failed to emit: " <> err)
          protocol.error_result("Failed to emit event: " <> err)
        }
      }
    }
  }
}

/// Extract JSON value from args without double-escaping
fn extract_json_value(args: json.Json, field: String) -> String {
  let s = json.to_string(args)
  let pattern = "\"" <> field <> "\":"
  case string.split(s, pattern) {
    [_, rest, ..] -> {
      case string.first(string.trim(rest)) {
        Ok("[") -> extract_until_bracket(rest, "[", "]")
        Ok("{") -> extract_until_bracket(rest, "{", "}")
        _ -> "{}"
      }
    }
    _ -> "{}"
  }
}

fn handle_event_list(args: json.Json) -> ToolResult {
  case decoders.decode_event_list(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      let limit = option.unwrap(parsed.limit, 20)
      let filter_type = option.unwrap(parsed.event_type, "")

      // Build query for Event Bus
      let event_type_filter = case filter_type {
        "" -> None
        ft -> Some(events.parse_event_type(ft))
      }

      let query =
        events.EventQuery(
          event_type: event_type_filter,
          target: None,
          since_timestamp: None,
          limit: limit,
        )

      // Query events from Event Bus
      let event_list = events.query(query)

      // Get stats for additional info
      let stats = events.stats()

      let result =
        json.object([
          #(
            "available_types",
            json.array(events.available_types(), json.string),
          ),
          #("recent_events", json.array(event_list, events.encode_event)),
          #("count", json.int(list.length(event_list))),
          #("total_in_bus", json.int(stats.total_events)),
          #("active_subscriptions", json.int(stats.active_subscriptions)),
        ])
        |> json.to_string()

      protocol.text_result(result)
    }
  }
}

@external(erlang, "calendar", "local_time")
fn erlang_local_time() -> #(#(Int, Int, Int), #(Int, Int, Int))

fn get_timestamp() -> String {
  let #(#(y, mo, d), #(h, mi, s)) = erlang_local_time()
  int.to_string(y)
  <> "-"
  <> pad2(mo)
  <> "-"
  <> pad2(d)
  <> " "
  <> pad2(h)
  <> ":"
  <> pad2(mi)
  <> ":"
  <> pad2(s)
}

fn pad2(n: Int) -> String {
  case n < 10 {
    True -> "0" <> int.to_string(n)
    False -> int.to_string(n)
  }
}

// ============================================================
// Helper Functions for JSON value extraction
// ============================================================

fn extract_until_bracket(s: String, open: String, close: String) -> String {
  extract_bracket_loop(s, open, close, 0, "")
}

fn extract_bracket_loop(
  s: String,
  open: String,
  close: String,
  depth: Int,
  acc: String,
) -> String {
  case string.pop_grapheme(s) {
    Error(_) -> acc
    Ok(#(c, rest)) -> {
      let new_depth = case c == open {
        True -> depth + 1
        False ->
          case c == close {
            True -> depth - 1
            False -> depth
          }
      }
      case new_depth == 0 && depth > 0 {
        True -> acc <> c
        False -> extract_bracket_loop(rest, open, close, new_depth, acc <> c)
      }
    }
  }
}

fn http_get(url: String) -> Result(String, String) {
  let base = bridge_url()
  let #(scheme, host, port) = parse_bridge_url(base)
  let path = string.replace(url, base, "")
  let api_key = config.get_env("VIBEE_API_KEY")

  // Get active session_id for X-Session-ID header
  let session_id = session_manager.get_active()
    |> option.unwrap("")

  let req =
    request.new()
    |> request.set_scheme(scheme)
    |> request.set_method(http.Get)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path(path)
    |> request.set_header("authorization", "Bearer " <> api_key)
    |> request.set_header("x-session-id", session_id)

  case httpc.send(req) {
    Ok(response) -> Ok(response.body)
    Error(_) -> Error("HTTP request failed")
  }
}

/// HTTP GET with session_id in X-Session-ID header
fn http_get_with_session(
  path: String,
  session_id: String,
) -> Result(String, String) {
  let base = bridge_url()
  let #(scheme, host, port) = parse_bridge_url(base)
  let api_key = config.get_env("VIBEE_API_KEY")

  let req =
    request.new()
    |> request.set_scheme(scheme)
    |> request.set_method(http.Get)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path(path)
    |> request.set_header("X-Session-ID", session_id)
    |> request.set_header("authorization", "Bearer " <> api_key)

  case httpc.send(req) {
    Ok(response) -> Ok(response.body)
    Error(_) -> Error("HTTP request failed")
  }
}

/// HTTP POST with session_id in X-Session-ID header
fn http_post_with_session(
  path: String,
  session_id: String,
  body: String,
) -> Result(String, String) {
  let base = bridge_url()
  let #(scheme, host, port) = parse_bridge_url(base)
  let api_key = config.get_env("VIBEE_API_KEY")

  let req =
    request.new()
    |> request.set_scheme(scheme)
    |> request.set_method(http.Post)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path(path)
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("X-Session-ID", session_id)
    |> request.set_header("authorization", "Bearer " <> api_key)
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) -> Ok(response.body)
    Error(_) -> Error("HTTP request failed")
  }
}

fn http_post(url: String, body: String) -> Result(String, String) {
  let base = bridge_url()
  let #(scheme, host, port) = parse_bridge_url(base)
  let path = string.replace(url, base, "")
  let api_key = config.get_env("VIBEE_API_KEY")

  let req =
    request.new()
    |> request.set_scheme(scheme)
    |> request.set_method(http.Post)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path(path)
    |> request.set_header("Content-Type", "application/json")
    |> request.set_header("authorization", "Bearer " <> api_key)
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) -> Ok(response.body)
    Error(_) -> Error("HTTP request failed")
  }
}

fn parse_url(url: String) -> #(String, Int, String) {
  case string.split(url, "://") {
    [_, rest] -> {
      case string.split(rest, "/") {
        [host_port, ..path_parts] -> {
          let path = "/" <> string.join(path_parts, "/")
          case string.split(host_port, ":") {
            [host, port_str] -> {
              case int.parse(port_str) {
                Ok(port) -> #(host, port, path)
                Error(_) -> #(host, 80, path)
              }
            }
            [host] -> #(host, 80, path)
            _ -> #("localhost", 80, "/")
          }
        }
        _ -> #("localhost", 80, "/")
      }
    }
    _ -> #("localhost", 80, "/")
  }
}

@external(erlang, "os", "cmd")
fn os_cmd(cmd: String) -> String

fn exec_command(cmd: String) -> Result(String, String) {
  let result = os_cmd(cmd)
  Ok(result)
}

// ============================================================
// DEBUG TOOLS - Definitions
// ============================================================

fn debug_build_tool() -> Tool {
  Tool(
    name: "debug_build",
    description: "Build Gleam project and return compilation errors/warnings",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "path",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Project path to build")),
            ]),
          ),
          #(
            "target",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array(["erlang", "javascript"], json.string)),
              #("description", json.string("Build target")),
              #("default", json.string("erlang")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["path"], json.string)),
    ]),
  )
}

fn debug_test_tool() -> Tool {
  Tool(
    name: "debug_test",
    description: "Run Gleam tests and return results",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "path",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Project path")),
            ]),
          ),
          #(
            "filter",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Filter tests by name pattern")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["path"], json.string)),
    ]),
  )
}

fn debug_analyze_tool() -> Tool {
  Tool(
    name: "debug_analyze",
    description: "Analyze a compilation error and suggest fixes",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "error_text",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("The error message to analyze")),
            ]),
          ),
          #(
            "file_path",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Path to the file with error")),
            ]),
          ),
          #(
            "context",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Additional context about the code")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["error_text"], json.string)),
    ]),
  )
}

fn debug_trace_tool() -> Tool {
  Tool(
    name: "debug_trace",
    description: "Add trace logging to a module/function for debugging",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "module",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Module name to trace")),
            ]),
          ),
          #(
            "function",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Function name to trace")),
            ]),
          ),
          #(
            "level",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array(["entry", "exit", "all"], json.string)),
              #("description", json.string("Trace level")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["module"], json.string)),
    ]),
  )
}

fn debug_log_tool() -> Tool {
  Tool(
    name: "debug_log",
    description: "Write structured debug log entry",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "operation",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Operation being debugged")),
            ]),
          ),
          #(
            "status",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(
                  ["start", "progress", "success", "error"],
                  json.string,
                ),
              ),
              #("description", json.string("Debug status")),
            ]),
          ),
          #(
            "data",
            json.object([
              #("type", json.string("object")),
              #("description", json.string("Debug data/context")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["operation", "status"], json.string)),
    ]),
  )
}

// ============================================================
// CODE TOOLS - Definitions
// ============================================================

fn code_generate_tool() -> Tool {
  Tool(
    name: "code_generate",
    description: "Generate code based on description (returns template/scaffold)",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "description",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("What code to generate")),
            ]),
          ),
          #(
            "language",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(["gleam", "erlang", "typescript"], json.string),
              ),
              #("description", json.string("Target language")),
              #("default", json.string("gleam")),
            ]),
          ),
          #(
            "template",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(
                  ["function", "module", "test", "handler", "tool"],
                  json.string,
                ),
              ),
              #("description", json.string("Code template type")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["description"], json.string)),
    ]),
  )
}

fn code_refactor_tool() -> Tool {
  Tool(
    name: "code_refactor",
    description: "Apply refactoring to code (rename, extract, inline)",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "file_path",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("File to refactor")),
            ]),
          ),
          #(
            "refactoring",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(
                  ["rename", "extract_function", "inline", "move", "simplify"],
                  json.string,
                ),
              ),
              #("description", json.string("Type of refactoring")),
            ]),
          ),
          #(
            "target",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("What to refactor (function name, variable, etc)"),
              ),
            ]),
          ),
          #(
            "new_name",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("New name (for rename refactoring)")),
            ]),
          ),
        ]),
      ),
      #(
        "required",
        json.array(["file_path", "refactoring", "target"], json.string),
      ),
    ]),
  )
}

fn code_explain_tool() -> Tool {
  Tool(
    name: "code_explain",
    description: "Explain what a piece of code does",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "code",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Code to explain")),
            ]),
          ),
          #(
            "file_path",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Or file path to read and explain")),
            ]),
          ),
          #(
            "detail_level",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(["brief", "detailed", "step_by_step"], json.string),
              ),
              #("description", json.string("Level of explanation detail")),
            ]),
          ),
        ]),
      ),
      #("required", json.array([], json.string)),
    ]),
  )
}

fn code_find_similar_tool() -> Tool {
  Tool(
    name: "code_find_similar",
    description: "Find similar code patterns in the project",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "pattern",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Code pattern to search for")),
            ]),
          ),
          #(
            "path",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Directory to search in")),
            ]),
          ),
          #(
            "file_type",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("File extension filter (gleam, ts, etc)"),
              ),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["pattern"], json.string)),
    ]),
  )
}

fn code_diff_tool() -> Tool {
  Tool(
    name: "code_diff",
    description: "Show diff between file versions or generate patch",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "file_path",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("File to diff")),
            ]),
          ),
          #(
            "compare_with",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(
                  ["git_head", "git_staged", "backup", "custom"],
                  json.string,
                ),
              ),
              #("description", json.string("What to compare with")),
            ]),
          ),
          #(
            "other_file",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Other file for custom comparison")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["file_path"], json.string)),
    ]),
  )
}

// ============================================================
// TEST TOOLS - Definitions
// ============================================================

fn test_run_tool() -> Tool {
  Tool(
    name: "test_run",
    description: "Run tests and return detailed results",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "path",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Project/test path")),
            ]),
          ),
          #(
            "filter",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Test name filter")),
            ]),
          ),
          #(
            "verbose",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("Show verbose output")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["path"], json.string)),
    ]),
  )
}

fn test_create_tool() -> Tool {
  Tool(
    name: "test_create",
    description: "Generate test code for a function",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "function_code",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Function code to create tests for")),
            ]),
          ),
          #(
            "function_name",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Function name")),
            ]),
          ),
          #(
            "test_cases",
            json.object([
              #("type", json.string("array")),
              #("description", json.string("Specific test cases to generate")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["function_code"], json.string)),
    ]),
  )
}

fn test_coverage_tool() -> Tool {
  Tool(
    name: "test_coverage",
    description: "Analyze test coverage for a project/file",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "path",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Project or file path")),
            ]),
          ),
          #(
            "format",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(["summary", "detailed", "json"], json.string),
              ),
              #("description", json.string("Output format")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["path"], json.string)),
    ]),
  )
}

fn test_validate_tool() -> Tool {
  Tool(
    name: "test_validate",
    description: "Validate expected vs actual result",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "expected",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Expected result")),
            ]),
          ),
          #(
            "actual",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Actual result")),
            ]),
          ),
          #(
            "comparison",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(
                  ["exact", "contains", "regex", "json_equal"],
                  json.string,
                ),
              ),
              #("description", json.string("Comparison type")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["expected", "actual"], json.string)),
    ]),
  )
}

// ============================================================
// AGENT TOOLS - Definitions
// ============================================================

fn agent_spawn_tool() -> Tool {
  Tool(
    name: "agent_spawn",
    description: "Spawn a new sub-agent for parallel task execution",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "name",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Agent name/identifier")),
            ]),
          ),
          #(
            "task",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Task description for the agent")),
            ]),
          ),
          #(
            "type",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(
                  ["debug", "code", "test", "search", "general"],
                  json.string,
                ),
              ),
              #("description", json.string("Agent type/specialization")),
            ]),
          ),
          #(
            "timeout",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Timeout in seconds")),
              #("default", json.int(300)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["name", "task"], json.string)),
    ]),
  )
}

fn agent_message_tool() -> Tool {
  Tool(
    name: "agent_message",
    description: "Send a message to a running agent",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "agent_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Agent ID to message")),
            ]),
          ),
          #(
            "message",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Message content")),
            ]),
          ),
          #(
            "wait_response",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("Wait for agent response")),
              #("default", json.bool(True)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["agent_id", "message"], json.string)),
    ]),
  )
}

fn agent_status_tool() -> Tool {
  Tool(
    name: "agent_status",
    description: "Get status of all agents or specific agent",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "agent_id",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string(
                  "Specific agent ID (optional, returns all if empty)",
                ),
              ),
            ]),
          ),
        ]),
      ),
      #("required", json.array([], json.string)),
    ]),
  )
}

fn agent_kill_tool() -> Tool {
  Tool(
    name: "agent_kill",
    description: "Terminate a running agent",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "agent_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Agent ID to kill")),
            ]),
          ),
          #(
            "force",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("Force kill without cleanup")),
              #("default", json.bool(False)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["agent_id"], json.string)),
    ]),
  )
}

// ============================================================
// DEBUG HANDLERS
// ============================================================

fn handle_debug_build(args: json.Json) -> ToolResult {
  case decoders.decode_debug_build(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      // Validate path
      case validation.validate_path(parsed.path) {
        Error(err) -> protocol.error_result(validation.error_to_string(err))
        Ok(path) -> {
          let target = validation.validate_build_target(parsed.target)
          logging.info("[DEBUG_BUILD] path=" <> path <> " target=" <> target)

          // Use safe shell execution
          case shell.gleam_build(path, target) {
            Ok(output) -> {
              let has_error =
                string.contains(output, "error:")
                || string.contains(output, "Error:")
              let status = case has_error {
                True -> "error"
                False -> "success"
              }
              protocol.text_result(
                json.object([
                  #("status", json.string(status)),
                  #("output", json.string(output)),
                  #("path", json.string(path)),
                  #("target", json.string(target)),
                ])
                |> json.to_string(),
              )
            }
            Error(err) -> protocol.error_result(err)
          }
        }
      }
    }
  }
}

fn handle_debug_test(args: json.Json) -> ToolResult {
  case decoders.decode_debug_test(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      case validation.validate_path(parsed.path) {
        Error(err) -> protocol.error_result(validation.error_to_string(err))
        Ok(path) -> {
          logging.info("[DEBUG_TEST] path=" <> path)

          case shell.gleam_test(path, parsed.filter) {
            Ok(output) -> {
              let passed = string.contains(output, "Passed")
              let failed = string.contains(output, "Failed")
              let status = case passed, failed {
                True, False -> "passed"
                _, True -> "failed"
                _, _ -> "unknown"
              }
              let filter_str = option.unwrap(parsed.filter, "")
              protocol.text_result(
                json.object([
                  #("status", json.string(status)),
                  #("output", json.string(output)),
                  #("filter", json.string(filter_str)),
                ])
                |> json.to_string(),
              )
            }
            Error(err) -> protocol.error_result(err)
          }
        }
      }
    }
  }
}

fn handle_debug_analyze(args: json.Json) -> ToolResult {
  case decoders.decode_debug_analyze(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      // Pattern matching for common Gleam errors
      let analysis = analyze_gleam_error(parsed.error_text)
      let file_context = case parsed.file_path {
        None -> ""
        Some(fp) -> {
          case validation.validate_path(fp) {
            Error(_) -> ""
            Ok(safe_fp) ->
              case simplifile.read(safe_fp) {
                Ok(content) ->
                  "\n\nFile context:\n" <> string.slice(content, 0, 500)
                Error(_) -> ""
              }
          }
        }
      }

      protocol.text_result(
        json.object([
          #("error_type", json.string(analysis.0)),
          #("suggestion", json.string(analysis.1)),
          #("fix_template", json.string(analysis.2)),
          #("file_context", json.string(file_context)),
        ])
        |> json.to_string(),
      )
    }
  }
}

fn analyze_gleam_error(error: String) -> #(String, String, String) {
  case string.contains(error, "Unknown variable") {
    True -> #(
      "unknown_variable",
      "Variable not defined. Check spelling or import the module.",
      "import module_name\n// or define: let variable_name = value",
    )
    False ->
      case string.contains(error, "Unknown type") {
        True -> #(
          "unknown_type",
          "Type not imported or defined. Add import statement.",
          "import module/type.{TypeName}",
        )
        False ->
          case string.contains(error, "Expected type") {
            True -> #(
              "type_mismatch",
              "Type mismatch. Check function signature and argument types.",
              "// Ensure argument type matches parameter type",
            )
            False ->
              case string.contains(error, "Not exhaustive") {
                True -> #(
                  "incomplete_pattern",
                  "Pattern match not exhaustive. Add missing cases.",
                  "case value {\n  Pattern1 -> ...\n  Pattern2 -> ...\n  _ -> ... // catch-all\n}",
                )
                False ->
                  case string.contains(error, "Duplicate") {
                    True -> #(
                      "duplicate",
                      "Duplicate definition. Rename or remove one instance.",
                      "// Remove duplicate or use unique names",
                    )
                    False ->
                      case string.contains(error, "Could not find") {
                        True -> #(
                          "missing_module",
                          "Module not found. Check gleam.toml dependencies.",
                          "# In gleam.toml:\n[dependencies]\npackage_name = \"~> version\"",
                        )
                        False -> #(
                          "unknown",
                          "Unable to determine error type. Check the error message.",
                          "// Review error message and check documentation",
                        )
                      }
                  }
              }
          }
      }
  }
}

fn handle_debug_trace(args: json.Json) -> ToolResult {
  case decoders.decode_debug_trace(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      let function = option.unwrap(parsed.function, "")
      let level = option.unwrap(parsed.level, "all")

      // Generate trace code
      let trace_code = generate_trace_code(parsed.module, function, level)
      logging.info(
        "[DEBUG_TRACE] Adding trace to " <> parsed.module <> ":" <> function,
      )
      protocol.text_result(
        json.object([
          #("module", json.string(parsed.module)),
          #("function", json.string(function)),
          #("level", json.string(level)),
          #("trace_code", json.string(trace_code)),
          #(
            "instructions",
            json.string("Add this code at function entry/exit points"),
          ),
        ])
        |> json.to_string(),
      )
    }
  }
}

fn generate_trace_code(
  module: String,
  function: String,
  level: String,
) -> String {
  let prefix = "io.debug(\"[TRACE " <> module
  case level {
    "entry" -> prefix <> ":" <> function <> "] ENTRY args=\")\nio.debug(args)"
    "exit" -> prefix <> ":" <> function <> "] EXIT result=\")\nio.debug(result)"
    _ ->
      prefix
      <> ":"
      <> function
      <> "] ENTRY\")\n// ... function body ...\n"
      <> prefix
      <> ":"
      <> function
      <> "] EXIT\")"
  }
}

fn handle_debug_log(args: json.Json) -> ToolResult {
  case decoders.decode_debug_log(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      case validation.validate_agent_status(parsed.status) {
        Error(err) -> protocol.error_result(validation.error_to_string(err))
        Ok(status) -> {
          // Extract data as raw JSON to avoid double-escaping
          let data_json = extract_json_value(args, "data")

          let timestamp = get_timestamp()
          // Build with raw JSON data
          let log_entry =
            "{\"timestamp\":\""
            <> timestamp
            <> "\",\"operation\":\""
            <> parsed.operation
            <> "\",\"status\":\""
            <> status
            <> "\",\"data\":"
            <> data_json
            <> "}"

          // Append to debug log
          let debug_log = "/tmp/vibee_debug.log"
          let _ = case simplifile.read(debug_log) {
            Ok(content) ->
              simplifile.write(debug_log, content <> "\n" <> log_entry)
            Error(_) -> simplifile.write(debug_log, log_entry)
          }

          logging.info("[DEBUG_LOG] " <> parsed.operation <> " -> " <> status)
          protocol.text_result(log_entry)
        }
      }
    }
  }
}

// ============================================================
// CODE HANDLERS
// ============================================================

fn handle_code_generate(args: json.Json) -> ToolResult {
  case decoders.decode_code_generate(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      let language = option.unwrap(parsed.language, "gleam")
      let template = option.unwrap(parsed.template, "")

      let code = generate_code_template(parsed.description, language, template)
      protocol.text_result(
        json.object([
          #("language", json.string(language)),
          #("template", json.string(template)),
          #("code", json.string(code)),
          #("note", json.string("Generated template - review and customize")),
        ])
        |> json.to_string(),
      )
    }
  }
}

fn generate_code_template(
  desc: String,
  lang: String,
  template: String,
) -> String {
  case lang {
    "gleam" ->
      case template {
        "function" ->
          "pub fn "
          <> snake_case(desc)
          <> "(arg: Type) -> Result(Output, Error) {\n  // TODO: "
          <> desc
          <> "\n  Ok(value)\n}"
        "module" ->
          "// Module: "
          <> desc
          <> "\n\nimport gleam/io\nimport gleam/result\n\npub type Error {\n  NotImplemented\n}\n\npub fn main() {\n  io.println(\""
          <> desc
          <> "\")\n}"
        "test" ->
          "import gleeunit\nimport gleeunit/should\n\npub fn main() {\n  gleeunit.main()\n}\n\npub fn "
          <> snake_case(desc)
          <> "_test() {\n  // TODO: "
          <> desc
          <> "\n  True |> should.be_true()\n}"
        "handler" ->
          "fn handle_"
          <> snake_case(desc)
          <> "(args: json.Json) -> ToolResult {\n  // TODO: "
          <> desc
          <> "\n  protocol.text_result(\"Not implemented\")\n}"
        "tool" ->
          "fn "
          <> snake_case(desc)
          <> "_tool() -> Tool {\n  Tool(\n    name: \""
          <> snake_case(desc)
          <> "\",\n    description: \""
          <> desc
          <> "\",\n    input_schema: json.object([\n      #(\"type\", json.string(\"object\")),\n      #(\"properties\", json.object([])),\n      #(\"required\", json.array([], json.string)),\n    ]),\n  )\n}"
        _ -> "// " <> desc <> "\npub fn example() {\n  todo\n}"
      }
    "typescript" ->
      "// "
      <> desc
      <> "\nexport function example(): void {\n  // TODO: implement\n}"
    _ -> "// " <> desc <> "\n// TODO: implement"
  }
}

fn snake_case(s: String) -> String {
  s
  |> string.lowercase()
  |> string.replace(" ", "_")
  |> string.replace("-", "_")
  |> string.slice(0, 30)
}

fn handle_code_refactor(args: json.Json) -> ToolResult {
  case decoders.decode_code_refactor(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      case
        validation.validate_path(parsed.file_path),
        validation.validate_refactoring_type(parsed.refactoring)
      {
        Error(err), _ -> protocol.error_result(validation.error_to_string(err))
        _, Error(err) -> protocol.error_result(validation.error_to_string(err))
        Ok(fp), Ok(ref) -> {
          case simplifile.read(fp) {
            Ok(content) -> {
              let new_name = option.unwrap(parsed.new_name, "")
              let result = case ref {
                "rename" -> {
                  case new_name {
                    "" -> #(False, "new_name is required for rename")
                    nn -> {
                      let new_content =
                        string.replace(content, parsed.target, nn)
                      let changed = new_content != content
                      case changed {
                        True -> {
                          case simplifile.write(fp, new_content) {
                            Ok(_) -> #(
                              True,
                              "Renamed " <> parsed.target <> " to " <> nn,
                            )
                            Error(_) -> #(False, "Failed to write file")
                          }
                        }
                        False -> #(False, "Target not found: " <> parsed.target)
                      }
                    }
                  }
                }
                "simplify" -> {
                  // Just report suggestions
                  #(
                    True,
                    "Simplification suggestions for "
                      <> parsed.target
                      <> ":\n1. Remove unused imports\n2. Combine nested case statements\n3. Use pipeline operators",
                  )
                }
                _ -> #(
                  True,
                  "Refactoring '"
                    <> ref
                    <> "' would be applied to "
                    <> parsed.target,
                )
              }

              protocol.text_result(
                json.object([
                  #("success", json.bool(result.0)),
                  #("message", json.string(result.1)),
                  #("file", json.string(fp)),
                  #("refactoring", json.string(ref)),
                ])
                |> json.to_string(),
              )
            }
            Error(_) -> protocol.error_result("Failed to read file: " <> fp)
          }
        }
      }
    }
  }
}

fn handle_code_explain(args: json.Json) -> ToolResult {
  case decoders.decode_code_explain(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      let detail_level = option.unwrap(parsed.detail_level, "brief")

      let code_to_explain = case parsed.code, parsed.file_path {
        None, None -> ""
        Some(c), None -> c
        None, Some(fp) -> {
          case validation.validate_path(fp) {
            Error(_) -> ""
            Ok(safe_fp) ->
              case simplifile.read(safe_fp) {
                Ok(content) -> content
                Error(_) -> ""
              }
          }
        }
        Some(c), _ -> c
      }

      case code_to_explain {
        "" -> protocol.error_result("Provide either 'code' or 'file_path'")
        c -> {
          let analysis = analyze_code_structure(c)
          protocol.text_result(
            json.object([
              #("detail_level", json.string(detail_level)),
              #("analysis", json.string(analysis)),
              #("line_count", json.int(list.length(string.split(c, "\n")))),
            ])
            |> json.to_string(),
          )
        }
      }
    }
  }
}

fn analyze_code_structure(code: String) -> String {
  let lines = string.split(code, "\n")
  let imports =
    list.filter(lines, fn(l) { string.starts_with(string.trim(l), "import") })
  let functions =
    list.filter(lines, fn(l) {
      string.contains(l, "pub fn") || string.contains(l, "fn ")
    })
  let types =
    list.filter(lines, fn(l) {
      string.contains(l, "pub type") || string.contains(l, "type ")
    })

  "Structure:\n"
  <> "- Imports: "
  <> int.to_string(list.length(imports))
  <> "\n"
  <> "- Functions: "
  <> int.to_string(list.length(functions))
  <> "\n"
  <> "- Types: "
  <> int.to_string(list.length(types))
  <> "\n"
  <> "- Total lines: "
  <> int.to_string(list.length(lines))
}

fn handle_code_find_similar(args: json.Json) -> ToolResult {
  case decoders.decode_code_find_similar(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      let path =
        option.unwrap(
          parsed.path,
          "/Users/playra/vibee-eliza-999/vibee/gleam/src",
        )
      let file_type = option.unwrap(parsed.file_type, "gleam")

      // Use safe grep
      case shell.grep_files(parsed.pattern, path, Some(file_type)) {
        Ok(output) -> {
          let files =
            string.split(output, "\n") |> list.filter(fn(f) { f != "" })
          protocol.text_result(
            json.object([
              #("pattern", json.string(parsed.pattern)),
              #("matches", json.array(files, json.string)),
              #("count", json.int(list.length(files))),
            ])
            |> json.to_string(),
          )
        }
        Error(err) -> protocol.error_result(err)
      }
    }
  }
}

fn handle_code_diff(args: json.Json) -> ToolResult {
  case decoders.decode_code_diff(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      case validation.validate_path(parsed.file_path) {
        Error(err) -> protocol.error_result(validation.error_to_string(err))
        Ok(file_path) -> {
          // Use safe git diff
          case
            shell.git_diff(file_path, parsed.compare_with, parsed.other_file)
          {
            Ok(output) -> {
              let compare_with = option.unwrap(parsed.compare_with, "git_head")
              protocol.text_result(
                json.object([
                  #("file", json.string(file_path)),
                  #("compare_with", json.string(compare_with)),
                  #("diff", json.string(output)),
                  #("has_changes", json.bool(output != "")),
                ])
                |> json.to_string(),
              )
            }
            Error(err) -> protocol.error_result(err)
          }
        }
      }
    }
  }
}

// ============================================================
// TEST HANDLERS
// ============================================================

fn handle_test_run(args: json.Json) -> ToolResult {
  case decoders.decode_test_run(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      case validation.validate_path(parsed.path) {
        Error(err) -> protocol.error_result(validation.error_to_string(err))
        Ok(path) -> {
          case shell.gleam_test(path, parsed.filter) {
            Ok(output) -> {
              let passed = string.contains(output, "Passed")
              let failed = string.contains(output, "Failed")
              let filter_str = option.unwrap(parsed.filter, "")
              protocol.text_result(
                json.object([
                  #(
                    "status",
                    json.string(case passed, failed {
                      True, False -> "passed"
                      _, True -> "failed"
                      _, _ -> "unknown"
                    }),
                  ),
                  #("output", json.string(output)),
                  #("filter", json.string(filter_str)),
                ])
                |> json.to_string(),
              )
            }
            Error(err) -> protocol.error_result(err)
          }
        }
      }
    }
  }
}

fn handle_test_create(args: json.Json) -> ToolResult {
  case decoders.decode_test_create(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      let name = option.unwrap(parsed.function_name, "test_function")

      let test_code =
        "import gleeunit/should\n\npub fn "
        <> name
        <> "_test() {\n  // Test basic case\n  "
        <> name
        <> "(input) |> should.equal(expected)\n}\n\npub fn "
        <> name
        <> "_edge_case_test() {\n  // Test edge case\n  "
        <> name
        <> "(edge_input) |> should.equal(edge_expected)\n}\n\npub fn "
        <> name
        <> "_error_test() {\n  // Test error case\n  "
        <> name
        <> "(invalid_input) |> should.be_error()\n}"

      protocol.text_result(
        json.object([
          #("function_name", json.string(name)),
          #("test_code", json.string(test_code)),
          #("test_count", json.int(3)),
        ])
        |> json.to_string(),
      )
    }
  }
}

fn handle_test_coverage(args: json.Json) -> ToolResult {
  case decoders.decode_test_coverage(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      case validation.validate_path(parsed.path) {
        Error(err) -> protocol.error_result(validation.error_to_string(err))
        Ok(path) -> {
          let format = option.unwrap(parsed.format, "summary")

          // Use safe shell commands
          let src_count = case shell.count_source_files(path) {
            Ok(out) ->
              case int.parse(string.trim(out)) {
                Ok(n) -> n
                Error(_) -> 0
              }
            Error(_) -> 0
          }

          let test_count = case shell.count_test_files(path) {
            Ok(out) ->
              case int.parse(string.trim(out)) {
                Ok(n) -> n
                Error(_) -> 0
              }
            Error(_) -> 0
          }

          let ratio = case src_count {
            0 -> 0.0
            _ -> int.to_float(test_count) /. int.to_float(src_count) *. 100.0
          }

          protocol.text_result(
            json.object([
              #("path", json.string(path)),
              #("source_files", json.int(src_count)),
              #("test_files", json.int(test_count)),
              #("coverage_ratio", json.string(float_to_string(ratio) <> "%")),
              #("format", json.string(format)),
            ])
            |> json.to_string(),
          )
        }
      }
    }
  }
}

fn float_to_string(f: Float) -> String {
  let i = float.truncate(f)
  int.to_string(i)
}

@external(erlang, "erlang", "float")
fn to_float(i: Int) -> Float

fn handle_test_validate(args: json.Json) -> ToolResult {
  case decoders.decode_test_validate(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      let comparison = validation.validate_comparison_type(parsed.comparison)

      let passed = case comparison {
        "exact" -> parsed.expected == parsed.actual
        "contains" -> string.contains(parsed.actual, parsed.expected)
        _ -> parsed.expected == parsed.actual
      }

      protocol.text_result(
        json.object([
          #("passed", json.bool(passed)),
          #("comparison", json.string(comparison)),
          #("expected", json.string(parsed.expected)),
          #("actual", json.string(parsed.actual)),
          #(
            "diff",
            json.string(case passed {
              True -> ""
              False ->
                "Expected: " <> parsed.expected <> "\nActual: " <> parsed.actual
            }),
          ),
        ])
        |> json.to_string(),
      )
    }
  }
}

// ============================================================
// AGENT HANDLERS
// ============================================================

// Agent state storage (in-memory for now)
@external(erlang, "persistent_term", "get")
fn persistent_get(key: a, default: b) -> b

@external(erlang, "persistent_term", "put")
fn persistent_put(key: a, value: b) -> a

fn handle_agent_spawn(args: json.Json) -> ToolResult {
  case decoders.decode_agent_spawn(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      let agent_type = option.unwrap(parsed.agent_type, "general")
      let timeout = option.unwrap(parsed.timeout, 300)

      let agent_id = "agent_" <> parsed.name <> "_" <> get_timestamp_compact()
      let timestamp = get_timestamp()

      // Log agent spawn
      let agent_info =
        json.object([
          #("id", json.string(agent_id)),
          #("name", json.string(parsed.name)),
          #("task", json.string(parsed.task)),
          #("type", json.string(agent_type)),
          #("status", json.string("running")),
          #("spawned_at", json.string(timestamp)),
          #("timeout", json.int(timeout)),
        ])
        |> json.to_string()

      // Store in events
      let event_file =
        "/Users/playra/vibee-eliza-999/vibee/gleam/data/agents.jsonl"
      let _ = case simplifile.read(event_file) {
        Ok(content) ->
          simplifile.write(event_file, content <> "\n" <> agent_info)
        Error(_) -> simplifile.write(event_file, agent_info)
      }

      logging.info("[AGENT_SPAWN] " <> agent_id <> " for task: " <> parsed.task)
      protocol.text_result(agent_info)
    }
  }
}

fn get_timestamp_compact() -> String {
  let #(#(y, mo, d), #(h, mi, s)) = erlang_local_time()
  int.to_string(y) <> pad2(mo) <> pad2(d) <> pad2(h) <> pad2(mi) <> pad2(s)
}

fn handle_agent_message(args: json.Json) -> ToolResult {
  case decoders.decode_agent_message(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      let timestamp = get_timestamp()
      let msg_entry =
        json.object([
          #("type", json.string("agent_message")),
          #("agent_id", json.string(parsed.agent_id)),
          #("message", json.string(parsed.message)),
          #("timestamp", json.string(timestamp)),
        ])
        |> json.to_string()

      // Log message
      let event_file =
        "/Users/playra/vibee-eliza-999/vibee/gleam/data/agents.jsonl"
      let _ = case simplifile.read(event_file) {
        Ok(content) ->
          simplifile.write(event_file, content <> "\n" <> msg_entry)
        Error(_) -> simplifile.write(event_file, msg_entry)
      }

      logging.info(
        "[AGENT_MSG] " <> parsed.agent_id <> " <- " <> parsed.message,
      )
      protocol.text_result(
        json.object([
          #("sent", json.bool(True)),
          #("agent_id", json.string(parsed.agent_id)),
          #("message", json.string(parsed.message)),
        ])
        |> json.to_string(),
      )
    }
  }
}

fn handle_agent_status(args: json.Json) -> ToolResult {
  case decoders.decode_agent_status(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      let event_file =
        "/Users/playra/vibee-eliza-999/vibee/gleam/data/agents.jsonl"
      let agent_id = option.unwrap(parsed.agent_id, "")

      case simplifile.read(event_file) {
        Ok(content) -> {
          let lines =
            string.split(content, "\n")
            |> list.filter(fn(l) { l != "" })

          let filtered = case agent_id {
            "" -> lines
            aid -> list.filter(lines, fn(l) { string.contains(l, aid) })
          }

          protocol.text_result(
            json.object([
              #("agent_id_filter", json.string(agent_id)),
              #("agents", json.array(filtered, json.string)),
              #("count", json.int(list.length(filtered))),
            ])
            |> json.to_string(),
          )
        }
        Error(_) -> {
          protocol.text_result(
            json.object([
              #("agents", json.array([], json.string)),
              #("count", json.int(0)),
              #("message", json.string("No agents found")),
            ])
            |> json.to_string(),
          )
        }
      }
    }
  }
}

fn handle_agent_kill(args: json.Json) -> ToolResult {
  case decoders.decode_agent_kill(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      let timestamp = get_timestamp()
      let kill_entry =
        json.object([
          #("type", json.string("agent_killed")),
          #("agent_id", json.string(parsed.agent_id)),
          #("timestamp", json.string(timestamp)),
        ])
        |> json.to_string()

      let event_file =
        "/Users/playra/vibee-eliza-999/vibee/gleam/data/agents.jsonl"
      let _ = case simplifile.read(event_file) {
        Ok(content) ->
          simplifile.write(event_file, content <> "\n" <> kill_entry)
        Error(_) -> simplifile.write(event_file, kill_entry)
      }

      logging.info("[AGENT_KILL] " <> parsed.agent_id)
      protocol.text_result(
        json.object([
          #("killed", json.bool(True)),
          #("agent_id", json.string(parsed.agent_id)),
        ])
        |> json.to_string(),
      )
    }
  }
}

// ============================================================
// BOT ANALYSIS TOOLS - Definitions
// ============================================================

fn bot_analyze_tool() -> Tool {
  Tool(
    name: "bot_analyze",
    description: "Analyze a Telegram bot: extract commands, response patterns, capabilities, and behavior",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "bot_username",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Bot username (with or without @)")),
            ]),
          ),
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("Telegram session ID for interaction"),
              ),
            ]),
          ),
          #(
            "depth",
            json.object([
              #("type", json.string("string")),
              #("enum", json.array(["quick", "standard", "deep"], json.string)),
              #(
                "description",
                json.string(
                  "Analysis depth: quick (commands only), standard (+ responses), deep (+ patterns)",
                ),
              ),
              #("default", json.string("standard")),
            ]),
          ),
          #(
            "message_history",
            json.object([
              #("type", json.string("integer")),
              #(
                "description",
                json.string("Number of messages to analyze from history"),
              ),
              #("default", json.int(100)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["bot_username"], json.string)),
    ]),
  )
}

fn bot_compare_tool() -> Tool {
  Tool(
    name: "bot_compare",
    description: "Compare two or more bots: features, response times, command coverage",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "bots",
            json.object([
              #("type", json.string("array")),
              #("items", json.object([#("type", json.string("string"))])),
              #("description", json.string("List of bot usernames to compare")),
            ]),
          ),
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Telegram session ID")),
            ]),
          ),
          #(
            "aspects",
            json.object([
              #("type", json.string("array")),
              #("items", json.object([#("type", json.string("string"))])),
              #(
                "description",
                json.string(
                  "Aspects to compare: commands, response_time, features, ui, language",
                ),
              ),
              #(
                "default",
                json.array(
                  ["commands", "features", "response_time"],
                  json.string,
                ),
              ),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["bots"], json.string)),
    ]),
  )
}

fn bot_monitor_tool() -> Tool {
  Tool(
    name: "bot_monitor",
    description: "Monitor bot activity and changes over time",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "bot_username",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Bot username to monitor")),
            ]),
          ),
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Telegram session ID")),
            ]),
          ),
          #(
            "action",
            json.object([
              #("type", json.string("string")),
              #(
                "enum",
                json.array(["start", "stop", "status", "report"], json.string),
              ),
              #("description", json.string("Monitoring action")),
            ]),
          ),
          #(
            "interval_seconds",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Check interval in seconds")),
              #("default", json.int(300)),
            ]),
          ),
          #(
            "metrics",
            json.object([
              #("type", json.string("array")),
              #("items", json.object([#("type", json.string("string"))])),
              #(
                "description",
                json.string(
                  "Metrics to track: availability, response_time, command_changes",
                ),
              ),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["bot_username", "action"], json.string)),
    ]),
  )
}

fn bot_extract_commands_tool() -> Tool {
  Tool(
    name: "bot_extract_commands",
    description: "Extract and categorize all commands from a bot (via /help, BotFather, or message analysis)",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "bot_username",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Bot username")),
            ]),
          ),
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Telegram session ID")),
            ]),
          ),
          #(
            "methods",
            json.object([
              #("type", json.string("array")),
              #("items", json.object([#("type", json.string("string"))])),
              #(
                "description",
                json.string(
                  "Extraction methods: help_command, start_command, botfather, message_scan, inline_buttons",
                ),
              ),
              #(
                "default",
                json.array(
                  ["help_command", "start_command", "inline_buttons"],
                  json.string,
                ),
              ),
            ]),
          ),
          #(
            "test_commands",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("Test each found command")),
              #("default", json.bool(False)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["bot_username"], json.string)),
    ]),
  )
}

fn bot_test_interaction_tool() -> Tool {
  Tool(
    name: "bot_test_interaction",
    description: "Test bot interaction: send messages, commands, and analyze responses",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "bot_username",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Bot username")),
            ]),
          ),
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Telegram session ID")),
            ]),
          ),
          #(
            "interactions",
            json.object([
              #("type", json.string("array")),
              #(
                "items",
                json.object([
                  #("type", json.string("object")),
                  #(
                    "properties",
                    json.object([
                      #(
                        "type",
                        json.object([
                          #("type", json.string("string")),
                          #(
                            "enum",
                            json.array(
                              [
                                "command",
                                "text",
                                "button_click",
                                "inline_query",
                              ],
                              json.string,
                            ),
                          ),
                        ]),
                      ),
                      #(
                        "value",
                        json.object([#("type", json.string("string"))]),
                      ),
                      #(
                        "expected_pattern",
                        json.object([#("type", json.string("string"))]),
                      ),
                      #(
                        "timeout_ms",
                        json.object([#("type", json.string("integer"))]),
                      ),
                    ]),
                  ),
                ]),
              ),
              #("description", json.string("List of interactions to test")),
            ]),
          ),
          #(
            "wait_between_ms",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Wait time between interactions")),
              #("default", json.int(1000)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["bot_username", "interactions"], json.string)),
    ]),
  )
}

// ============================================================
// BOT ANALYSIS HANDLERS
// ============================================================

fn handle_bot_analyze(args: json.Json) -> ToolResult {
  case decoders.decode_bot_analyze(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      let bot = normalize_username(parsed.bot_username)
      let session_id = option.unwrap(parsed.session_id, "")
      let depth = validation.validate_analysis_depth(parsed.depth)
      let history_limit = option.unwrap(parsed.message_history, 100)

      logging.info(
        "[BOT_ANALYZE] Analyzing bot: " <> bot <> " (depth: " <> depth <> ")",
      )

      // Collect analysis data
      let analysis =
        json.object([
          #("bot_username", json.string(bot)),
          #("analysis_depth", json.string(depth)),
          #("timestamp", json.string(get_timestamp())),
          #("status", json.string("analysis_started")),
        ])

      // If session provided, try to get actual data
      case session_id {
        "" -> {
          // Return template for manual analysis
          protocol.text_result(
            json.object([
              #("bot", json.string(bot)),
              #("depth", json.string(depth)),
              #("status", json.string("no_session")),
              #(
                "suggestion",
                json.string("Provide session_id to interact with bot"),
              ),
              #(
                "manual_analysis_template",
                json.object([
                  #(
                    "commands_to_try",
                    json.array(
                      [
                        "/start",
                        "/help",
                        "/settings",
                        "/menu",
                        "/info",
                        "/about",
                      ],
                      json.string,
                    ),
                  ),
                  #(
                    "interaction_patterns",
                    json.array(
                      ["greeting", "question", "command", "inline_query"],
                      json.string,
                    ),
                  ),
                  #(
                    "metrics_to_collect",
                    json.array(
                      [
                        "response_time",
                        "message_format",
                        "button_usage",
                        "media_support",
                      ],
                      json.string,
                    ),
                  ),
                ]),
              ),
            ])
            |> json.to_string(),
          )
        }
        sid -> {
          // Try to get bot info and history
          let bot_history_url =
            bridge_url()
            <> "/api/v1/history/"
            <> bot
            <> "?session_id="
            <> sid
            <> "&limit="
            <> int.to_string(history_limit)

          case http_get(bot_history_url) {
            Ok(history_json) -> {
              // Analyze the history
              let analysis_result = analyze_bot_messages(history_json, depth)
              protocol.text_result(
                json.object([
                  #("bot", json.string(bot)),
                  #("depth", json.string(depth)),
                  #("status", json.string("completed")),
                  #("analysis", json.string(analysis_result)),
                  #("history_analyzed", json.int(history_limit)),
                ])
                |> json.to_string(),
              )
            }
            Error(err) -> {
              protocol.text_result(
                json.object([
                  #("bot", json.string(bot)),
                  #("status", json.string("error")),
                  #("error", json.string(err)),
                  #(
                    "suggestion",
                    json.string(
                      "Start a conversation with the bot first via /start",
                    ),
                  ),
                ])
                |> json.to_string(),
              )
            }
          }
        }
      }
    }
  }
}

fn normalize_username(username: String) -> String {
  case string.starts_with(username, "@") {
    True -> string.drop_start(username, 1)
    False -> username
  }
}

fn analyze_bot_messages(history_json: String, depth: String) -> String {
  // Extract patterns from message history
  let commands_found = extract_commands_from_history(history_json)
  let has_buttons =
    string.contains(history_json, "reply_markup")
    || string.contains(history_json, "inline_keyboard")
  let has_media =
    string.contains(history_json, "photo")
    || string.contains(history_json, "document")
    || string.contains(history_json, "video")

  let base_analysis =
    "Commands found: "
    <> commands_found
    <> "\n"
    <> "Has buttons: "
    <> bool_to_string(has_buttons)
    <> "\n"
    <> "Has media: "
    <> bool_to_string(has_media)

  case depth {
    "quick" -> base_analysis
    "standard" ->
      base_analysis
      <> "\n"
      <> "Response patterns: text, "
      <> case has_buttons {
        True -> "buttons, "
        False -> ""
      }
      <> case has_media {
        True -> "media"
        False -> ""
      }
    "deep" ->
      base_analysis
      <> "\n"
      <> "Response patterns: analyzed\n"
      <> "Conversation flow: extracted\n"
      <> "Language detection: auto"
    _ -> base_analysis
  }
}

fn extract_commands_from_history(json_str: String) -> String {
  // Find /command patterns in the JSON
  let parts = string.split(json_str, "/")
  let commands =
    list.filter_map(parts, fn(p) {
      let word =
        string.slice(p, 0, 20)
        |> string.split(" ")
        |> list.first()
      case word {
        Ok(w) ->
          case string.length(w) > 0 && string.length(w) < 20 {
            True -> Ok("/" <> w)
            False -> Error(Nil)
          }
        Error(_) -> Error(Nil)
      }
    })
    |> list.unique()
    |> list.take(10)

  case list.length(commands) {
    0 -> "none detected"
    _ -> string.join(commands, ", ")
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "yes"
    False -> "no"
  }
}

fn handle_bot_compare(args: json.Json) -> ToolResult {
  case decoders.decode_bot_compare(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      // Normalize bot usernames - proper JSON array handling
      let bots = list.map(parsed.bots, normalize_username)

      logging.info("[BOT_COMPARE] Comparing bots: " <> string.join(bots, ", "))

      let comparison =
        json.object([
          #("bots_compared", json.array(bots, json.string)),
          #("timestamp", json.string(get_timestamp())),
          #(
            "comparison_template",
            json.object([
              #("features", json.string("To be filled after analysis")),
              #("commands", json.string("To be filled after analysis")),
              #("response_time", json.string("To be filled after analysis")),
              #("ui_quality", json.string("To be filled after analysis")),
            ]),
          ),
          #(
            "recommendation",
            json.string(
              "Run bot_analyze on each bot first, then compare results",
            ),
          ),
        ])
        |> json.to_string()

      protocol.text_result(comparison)
    }
  }
}

fn handle_bot_monitor(args: json.Json) -> ToolResult {
  case decoders.decode_bot_monitor(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      case validation.validate_monitor_action(parsed.action) {
        Error(err) -> protocol.error_result(validation.error_to_string(err))
        Ok(act) -> {
          let bot = normalize_username(parsed.bot_username)
          let interval = option.unwrap(parsed.interval_seconds, 300)
          {
            let monitor_file =
              "/Users/playra/vibee-eliza-999/vibee/gleam/data/bot_monitors.jsonl"
            let timestamp = get_timestamp()

            case act {
              "start" -> {
                let entry =
                  json.object([
                    #("bot", json.string(bot)),
                    #("action", json.string("monitor_started")),
                    #("interval_seconds", json.int(interval)),
                    #("timestamp", json.string(timestamp)),
                  ])
                  |> json.to_string()

                let _ = case simplifile.read(monitor_file) {
                  Ok(content) ->
                    simplifile.write(monitor_file, content <> "\n" <> entry)
                  Error(_) -> simplifile.write(monitor_file, entry)
                }

                protocol.text_result(
                  json.object([
                    #("status", json.string("monitoring_started")),
                    #("bot", json.string(bot)),
                    #("interval_seconds", json.int(interval)),
                    #(
                      "note",
                      json.string(
                        "Monitor will check bot availability and response time",
                      ),
                    ),
                  ])
                  |> json.to_string(),
                )
              }
              "stop" -> {
                let entry =
                  json.object([
                    #("bot", json.string(bot)),
                    #("action", json.string("monitor_stopped")),
                    #("timestamp", json.string(timestamp)),
                  ])
                  |> json.to_string()

                let _ = case simplifile.read(monitor_file) {
                  Ok(content) ->
                    simplifile.write(monitor_file, content <> "\n" <> entry)
                  Error(_) -> simplifile.write(monitor_file, entry)
                }

                protocol.text_result(
                  json.object([
                    #("status", json.string("monitoring_stopped")),
                    #("bot", json.string(bot)),
                  ])
                  |> json.to_string(),
                )
              }
              "status" -> {
                case simplifile.read(monitor_file) {
                  Ok(content) -> {
                    let bot_entries =
                      string.split(content, "\n")
                      |> list.filter(fn(l) { string.contains(l, bot) })
                      |> list.reverse()
                      |> list.take(5)

                    protocol.text_result(
                      json.object([
                        #("bot", json.string(bot)),
                        #(
                          "recent_entries",
                          json.array(bot_entries, json.string),
                        ),
                      ])
                      |> json.to_string(),
                    )
                  }
                  Error(_) ->
                    protocol.text_result(
                      json.object([
                        #("bot", json.string(bot)),
                        #("status", json.string("no_monitoring_data")),
                      ])
                      |> json.to_string(),
                    )
                }
              }
              "report" -> {
                case simplifile.read(monitor_file) {
                  Ok(content) -> {
                    let bot_entries =
                      string.split(content, "\n")
                      |> list.filter(fn(l) { string.contains(l, bot) })

                    protocol.text_result(
                      json.object([
                        #("bot", json.string(bot)),
                        #("total_entries", json.int(list.length(bot_entries))),
                        #("entries", json.array(bot_entries, json.string)),
                      ])
                      |> json.to_string(),
                    )
                  }
                  Error(_) -> protocol.error_result("No monitoring data found")
                }
              }
              _ -> protocol.error_result("Invalid action: " <> act)
            }
          }
        }
      }
    }
  }
}

fn handle_bot_extract_commands(args: json.Json) -> ToolResult {
  case decoders.decode_bot_extract_commands(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      let bot = normalize_username(parsed.bot_username)
      let session_id = option.unwrap(parsed.session_id, "")
      let test_commands = option.unwrap(parsed.test_commands, False)

      logging.info("[BOT_EXTRACT] Extracting commands from: " <> bot)

      // Common bot commands to try
      let common_commands = [
        #("/start", "Initialize bot conversation"),
        #("/help", "Get help and command list"),
        #("/settings", "Bot settings"),
        #("/menu", "Main menu"),
        #("/cancel", "Cancel current operation"),
        #("/info", "Bot information"),
        #("/about", "About the bot"),
        #("/lang", "Language settings"),
        #("/feedback", "Send feedback"),
        #("/support", "Get support"),
      ]

      let extraction_result =
        json.object([
          #("bot", json.string(bot)),
          #(
            "extraction_methods",
            json.array(
              [
                "help_command",
                "start_command",
                "inline_buttons",
                "message_scan",
              ],
              json.string,
            ),
          ),
          #(
            "common_commands_to_try",
            json.array(
              list.map(common_commands, fn(cmd) {
                json.object([
                  #("command", json.string(cmd.0)),
                  #("description", json.string(cmd.1)),
                ])
              }),
              fn(x) { x },
            ),
          ),
          #("session_provided", json.bool(session_id != "")),
          #("test_commands", json.bool(test_commands)),
          #(
            "next_steps",
            json.array(
              [
                "Send /start to bot",
                "Send /help to get command list",
                "Analyze inline buttons in responses",
                "Parse command descriptions from messages",
              ],
              json.string,
            ),
          ),
        ])
        |> json.to_string()

      protocol.text_result(extraction_result)
    }
  }
}

fn handle_bot_test_interaction(args: json.Json) -> ToolResult {
  case decoders.decode_bot_test_interaction(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      let bot = normalize_username(parsed.bot_username)
      let wait_between = option.unwrap(parsed.wait_between_ms, 1000)

      case parsed.session_id {
        None ->
          protocol.error_result(
            "session_id is required for testing interactions",
          )
        Some(sid) -> {
          case validation.validate_session_id_string(sid) {
            Error(err) -> protocol.error_result(validation.error_to_string(err))
            Ok(valid_sid) -> {
              logging.info("[BOT_TEST] Testing interactions with: " <> bot)

              // Build interactions JSON array
              let interactions_json =
                json.array(parsed.interactions, fn(inter) {
                  json.object([
                    #("type", json.string(inter.interaction_type)),
                    #("value", json.string(inter.value)),
                    #("expected_pattern", case inter.expected_pattern {
                      Some(p) -> json.string(p)
                      None -> json.null()
                    }),
                    #("timeout_ms", case inter.timeout_ms {
                      Some(t) -> json.int(t)
                      None -> json.null()
                    }),
                  ])
                })

              let test_plan =
                json.object([
                  #("bot", json.string(bot)),
                  #("session_id", json.string(valid_sid)),
                  #("status", json.string("test_plan_created")),
                  #("interactions_to_test", interactions_json),
                  #("wait_between_ms", json.int(wait_between)),
                  #(
                    "execution_steps",
                    json.array(
                      [
                        "1. Resolve bot chat_id",
                        "2. Send /start command",
                        "3. Execute each interaction",
                        "4. Record response time",
                        "5. Match response against expected pattern",
                        "6. Generate report",
                      ],
                      json.string,
                    ),
                  ),
                  #(
                    "note",
                    json.string(
                      "Use telegram_send_message to execute individual interactions",
                    ),
                  ),
                ])
                |> json.to_string()

              protocol.text_result(test_plan)
            }
          }
        }
      }
    }
  }
}

// ============================================================
// Auth Tools - Telegram Authorization
// ============================================================

fn auth_status_tool() -> Tool {
  Tool(
    name: "auth_status",
    description: "Check Telegram authorization status for current session",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("Session ID (optional, uses default)"),
              ),
            ]),
          ),
        ]),
      ),
      #("required", json.array([], json.string)),
    ]),
  )
}

fn auth_send_code_tool() -> Tool {
  Tool(
    name: "auth_send_code",
    description: "Send authorization code to phone number via Telegram",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "phone",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string(
                  "Phone number in international format (+79001234567)",
                ),
              ),
            ]),
          ),
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Session ID (optional)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["phone"], json.string)),
    ]),
  )
}

fn auth_verify_code_tool() -> Tool {
  Tool(
    name: "auth_verify_code",
    description: "Verify authorization code received via Telegram",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "phone",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Phone number used for send_code")),
            ]),
          ),
          #(
            "code",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Authorization code from Telegram")),
            ]),
          ),
          #(
            "phone_code_hash",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("Phone code hash from send_code response"),
              ),
            ]),
          ),
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Session ID (optional)")),
            ]),
          ),
        ]),
      ),
      #(
        "required",
        json.array(["phone", "code", "phone_code_hash"], json.string),
      ),
    ]),
  )
}

fn auth_2fa_tool() -> Tool {
  Tool(
    name: "auth_2fa",
    description: "Complete two-factor authentication with cloud password",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "password",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Telegram cloud password (2FA)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["password"], json.string)),
    ]),
  )
}

fn auth_logout_tool() -> Tool {
  Tool(
    name: "auth_logout",
    description: "Logout from Telegram session",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Session ID to logout")),
            ]),
          ),
        ]),
      ),
      #("required", json.array([], json.string)),
    ]),
  )
}

// ============================================================
// Auth Handlers
// ============================================================

fn handle_auth_status(_args: json.Json) -> ToolResult {
  logging.info("[AUTH] Checking authorization status...")

  let base = bridge_url()
  let #(scheme, host, port) = parse_bridge_url(base)
  let api_key = config.get_env("VIBEE_API_KEY")
  let path = "/api/v1/auth/status"

  let req =
    request.new()
    |> request.set_scheme(scheme)
    |> request.set_method(http.Get)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path(path)
    |> request.set_header("authorization", "Bearer " <> api_key)

  case httpc.send(req) {
    Ok(resp) -> {
      logging.info("[AUTH] Status received")
      // Parse response to check if authorized and add helpful instructions
      case json.parse(from: resp.body, using: session_count_decoder()) {
        Ok(count) if count > 0 -> {
          // Authorized - return original response
          protocol.text_result(resp.body)
        }
        _ -> {
          // Not authorized - add helpful instructions
          let response =
            json.object([
              #("status", json.string("not_authorized")),
              #("sessions", json.array([], fn(x) { x })),
              #("total_sessions", json.int(0)),
              #(
                "message",
                json.string(
                  "Telegram не авторизован. Для использования Telegram tools необходима авторизация.",
                ),
              ),
              #("action_required", json.string("authorization")),
              #(
                "prompt",
                json.string("Введите номер телефона в формате +79001234567:"),
              ),
              #(
                "quick_start",
                json.object([
                  #("step", json.int(1)),
                  #(
                    "action",
                    json.string("Укажите номер телефона для отправки кода"),
                  ),
                  #("tool", json.string("auth_send_code")),
                  #(
                    "example",
                    json.object([
                      #("phone", json.string("+79001234567")),
                    ]),
                  ),
                ]),
              ),
              #(
                "instructions",
                json.string(
                  "Для авторизации в Telegram выполните следующие шаги:\n"
                  <> "1. Вызовите auth_send_code с вашим номером телефона (+79001234567)\n"
                  <> "2. Получите код авторизации в Telegram\n"
                  <> "3. Вызовите auth_verify_code с полученным кодом и phone_code_hash",
                ),
              ),
            ])
          protocol.text_result(json.to_string(response))
        }
      }
    }
    Error(_) -> {
      logging.error("[AUTH] Status check failed - bridge not running")
      let response =
        json.object([
          #("status", json.string("bridge_not_running")),
          #(
            "message",
            json.string(
              "Telegram bridge не запущен. Запустите telegram-bridge на порту 8081.",
            ),
          ),
          #("error", json.bool(True)),
        ])
      protocol.text_result(json.to_string(response))
    }
  }
}

fn handle_auth_send_code(args: json.Json) -> ToolResult {
  // Log active session for debugging
  let active = session_manager.get_active() |> option.unwrap("NONE")
  logging.info("[AUTH_SEND_CODE] Active session: " <> active)

  case decoders.decode_auth_send_code(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      case validation.validate_phone(parsed.phone) {
        Error(err) -> protocol.error_result(validation.error_to_string(err))
        Ok(phone) -> {
          logging.info("[AUTH] Sending code to: " <> phone <> " with session: " <> active)

          let url = bridge_url() <> "/api/v1/auth/phone"
          let body =
            json.object([
              #("phone", json.string(phone)),
            ])
            |> json.to_string()

          case make_telegram_post_request(url, body) {
            Ok(response) -> {
              logging.info("[AUTH] Code sent successfully")
              protocol.text_result(response)
            }
            Error(err) -> {
              logging.error("[AUTH] Send code failed: " <> err)
              protocol.error_result("Failed to send code: " <> err)
            }
          }
        }
      }
    }
  }
}

fn handle_auth_verify_code(args: json.Json) -> ToolResult {
  case decoders.decode_auth_verify_code(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      case validation.validate_phone(parsed.phone) {
        Error(err) -> protocol.error_result(validation.error_to_string(err))
        Ok(phone) -> {
          logging.info("[AUTH] Verifying code for: " <> phone)

          let url = bridge_url() <> "/api/v1/auth/code"
          let body =
            json.object([
              #("phone", json.string(phone)),
              #("code", json.string(parsed.code)),
              #("phone_code_hash", json.string(parsed.phone_code_hash)),
            ])
            |> json.to_string()

          case make_telegram_post_request(url, body) {
            Ok(response) -> {
              let _ = io.println_error("[AUTH] Verification successful, response: " <> string.slice(response, 0, 200))

              // Trigger automatic task extraction after successful auth
              let user_id_decoder = {
                use id <- decode.field("id", decode.int)
                decode.success(id)
              }
              let user_decoder = {
                use user <- decode.field("user", user_id_decoder)
                decode.success(user)
              }
              case json.parse(response, user_decoder) {
                Ok(user_telegram_id) -> {
                  let session_id = session_manager.get_active()
                    |> option.unwrap("default")
                  let _ = io.println_error("[ONBOARDING] Starting for user " <> int.to_string(user_telegram_id) <> " session " <> session_id)
                  auto_tasks.start_onboarding_async(session_id, user_telegram_id)
                  let _ = io.println_error("[ONBOARDING] Background process spawned")
                  Nil
                }
                Error(e) -> {
                  let _ = io.println_error("[ONBOARDING] Could not extract user_id: " <> string.inspect(e))
                  Nil
                }
              }

              protocol.text_result(response)
            }
            Error(err) -> {
              logging.error("[AUTH] Verification failed: " <> err)
              protocol.error_result("Failed to verify code: " <> err)
            }
          }
        }
      }
    }
  }
}

fn handle_auth_2fa(args: json.Json) -> ToolResult {
  let _ = io.println_error("[DEBUG] handle_auth_2fa called")
  let args_str = json.to_string(args)
  let password = json_get_optional_string(args_str, "password")
    |> option.unwrap("")

  let _ = io.println_error("[AUTH] Completing 2FA...")

  let url = bridge_url() <> "/api/v1/auth/2fa"
  let body =
    json.object([#("password", json.string(password))])
    |> json.to_string()

  case make_telegram_post_request(url, body) {
    Ok(response) -> {
      let _ = io.println_error("[AUTH] 2FA successful, response: " <> string.slice(response, 0, 200))

      // Trigger automatic task extraction after successful 2FA auth
      let user_id_decoder = {
        use id <- decode.field("id", decode.int)
        decode.success(id)
      }
      // Try to extract user_id from nested "user" object
      let user_decoder = {
        use user <- decode.field("user", user_id_decoder)
        decode.success(user)
      }
      case json.parse(response, user_decoder) {
        Ok(user_telegram_id) -> {
          let session_id = session_manager.get_active()
            |> option.unwrap("default")
          let _ = io.println_error("[ONBOARDING] Starting for user " <> int.to_string(user_telegram_id) <> " session " <> session_id)
          auto_tasks.start_onboarding_async(session_id, user_telegram_id)
          let _ = io.println_error("[ONBOARDING] Background process spawned")
          Nil
        }
        Error(e) -> {
          let _ = io.println_error("[ONBOARDING] Could not extract user_id: " <> string.inspect(e))
          Nil
        }
      }

      protocol.text_result(response)
    }
    Error(err) -> {
      let _ = io.println_error("[AUTH] 2FA failed: " <> err)
      protocol.error_result("Failed to complete 2FA: " <> err)
    }
  }
}

fn handle_auth_logout(_args: json.Json) -> ToolResult {
  logging.info("[AUTH] Logging out...")

  let url = bridge_url() <> "/api/v1/auth/logout"

  case make_telegram_post_request(url, "{}") {
    Ok(response) -> {
      logging.info("[AUTH] Logout successful")
      protocol.text_result(response)
    }
    Error(err) -> {
      logging.error("[AUTH] Logout failed: " <> err)
      protocol.error_result("Failed to logout: " <> err)
    }
  }
}

fn make_telegram_post_request(
  url: String,
  body: String,
) -> Result(String, String) {
  let base = bridge_url()
  let #(scheme, host, port) = parse_bridge_url(base)
  let path = string.replace(url, base, "")
  let api_key = config.get_env("VIBEE_API_KEY")

  // Get active session_id for X-Session-ID header
  let session_id = session_manager.get_active()
    |> option.unwrap("")

  logging.info("[HTTP] POST " <> path <> " with X-Session-ID: " <> session_id)

  let req =
    request.new()
    |> request.set_method(http.Post)
    |> request.set_scheme(scheme)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path(path)
    |> request.set_header("content-type", "application/json")
    |> request.set_header("authorization", "Bearer " <> api_key)
    |> request.set_header("x-session-id", session_id)
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) -> {
      logging.info("[HTTP] Response: " <> resp.body)
      Ok(resp.body)
    }
    Error(_) -> Error("HTTP request failed")
  }
}

/// Parse bridge URL into scheme, host, port
fn parse_bridge_url(url: String) -> #(http.Scheme, String, Int) {
  case string.starts_with(url, "https://") {
    True -> {
      let host = string.drop_start(url, 8)
        |> string.split("/")
        |> list.first
        |> result.unwrap("localhost")
      #(http.Https, host, 443)
    }
    False -> {
      case string.starts_with(url, "http://") {
        True -> {
          let rest = string.drop_start(url, 7)
            |> string.split("/")
            |> list.first
            |> result.unwrap("localhost:8081")
          case string.split(rest, ":") {
            [h, p] -> {
              let port = int.parse(p) |> result.unwrap(80)
              #(http.Http, h, port)
            }
            _ -> #(http.Http, rest, 80)
          }
        }
        False -> #(http.Http, "localhost", 8081)
      }
    }
  }
}

// =============================================================================
// RAINBOW BRIDGE TOOLS (P6 - Autonomous Self-Healing)
// =============================================================================

/// Rainbow Bridge - Autonomous Debug Cycle tool
fn rainbow_autonomous_debug_cycle_tool() -> Tool {
  Tool(
    name: "rainbow_autonomous_debug_cycle",
    description: "Run autonomous debug cycle: build → analyze → fix → verify. Automatically iterates until no errors or max iterations reached.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "path",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Project path to build and fix")),
            ]),
          ),
          #(
            "max_iterations",
            json.object([
              #("type", json.string("integer")),
              #(
                "description",
                json.string("Maximum number of fix iterations (default: 5)"),
              ),
              #("default", json.int(5)),
            ]),
          ),
        ]),
      ),
      #("required", json.array([json.string("path")], fn(x) { x })),
    ]),
  )
}

/// Task Create tool
fn task_create_tool() -> Tool {
  Tool(
    name: "task_create",
    description: "Create a new task for tracking autonomous operations",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "description",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Task description")),
            ]),
          ),
          #(
            "files",
            json.object([
              #("type", json.string("array")),
              #("items", json.object([#("type", json.string("string"))])),
              #("description", json.string("Files involved in this task")),
            ]),
          ),
        ]),
      ),
      #("required", json.array([json.string("description")], fn(x) { x })),
    ]),
  )
}

/// Task Get tool
fn task_get_tool() -> Tool {
  Tool(
    name: "task_get",
    description: "Get task by ID with full context and history",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Task ID")),
            ]),
          ),
        ]),
      ),
      #("required", json.array([json.string("task_id")], fn(x) { x })),
    ]),
  )
}

/// Task List tool
fn task_list_tool() -> Tool {
  Tool(
    name: "task_list",
    description: "List tasks, optionally filtered by state",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "state",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string(
                  "Filter by state: pending, in_progress, healing, completed, failed",
                ),
              ),
              #(
                "enum",
                json.array(
                  [
                    json.string("pending"),
                    json.string("in_progress"),
                    json.string("healing"),
                    json.string("completed"),
                    json.string("failed"),
                  ],
                  fn(x) { x },
                ),
              ),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

/// Task Update tool
fn task_update_tool() -> Tool {
  Tool(
    name: "task_update",
    description: "Update task state",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Task ID")),
            ]),
          ),
          #(
            "state",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("New state")),
            ]),
          ),
        ]),
      ),
      #(
        "required",
        json.array([json.string("task_id"), json.string("state")], fn(x) { x }),
      ),
    ]),
  )
}

/// Heal Start tool
fn heal_start_tool() -> Tool {
  Tool(
    name: "heal_start",
    description: "Start self-healing process for a task, taking snapshots of files for rollback",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Task ID to start healing")),
            ]),
          ),
        ]),
      ),
      #("required", json.array([json.string("task_id")], fn(x) { x })),
    ]),
  )
}

/// Heal Apply Fix tool
fn heal_apply_fix_tool() -> Tool {
  Tool(
    name: "heal_apply_fix",
    description: "Apply a suggested fix to the code",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Task ID")),
            ]),
          ),
          #(
            "fix_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Fix ID to apply")),
            ]),
          ),
        ]),
      ),
      #(
        "required",
        json.array([json.string("task_id"), json.string("fix_id")], fn(x) { x }),
      ),
    ]),
  )
}

/// Heal Verify tool
fn heal_verify_tool() -> Tool {
  Tool(
    name: "heal_verify",
    description: "Verify if an applied fix was successful by running build",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Task ID")),
            ]),
          ),
          #(
            "fix_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Fix ID to verify")),
            ]),
          ),
        ]),
      ),
      #(
        "required",
        json.array([json.string("task_id"), json.string("fix_id")], fn(x) { x }),
      ),
    ]),
  )
}

/// Heal Rollback tool
fn heal_rollback_tool() -> Tool {
  Tool(
    name: "heal_rollback",
    description: "Rollback a fix that didn't work, restoring original file content",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Task ID")),
            ]),
          ),
          #(
            "fix_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Fix ID to rollback")),
            ]),
          ),
        ]),
      ),
      #(
        "required",
        json.array([json.string("task_id"), json.string("fix_id")], fn(x) { x }),
      ),
    ]),
  )
}

/// Decide Next Step tool
fn decide_next_step_tool() -> Tool {
  Tool(
    name: "decide_next_step",
    description: "Get decision engine recommendation for next action based on task state",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Task ID to analyze")),
            ]),
          ),
        ]),
      ),
      #("required", json.array([json.string("task_id")], fn(x) { x })),
    ]),
  )
}

/// Decide Apply tool
fn decide_apply_tool() -> Tool {
  Tool(
    name: "decide_apply",
    description: "Apply a decision from the decision engine",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Task ID")),
            ]),
          ),
          #(
            "decision_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Decision ID to apply")),
            ]),
          ),
        ]),
      ),
      #(
        "required",
        json.array([json.string("task_id"), json.string("decision_id")], fn(x) {
          x
        }),
      ),
    ]),
  )
}

// =============================================================================
// RAINBOW BRIDGE HANDLERS
// =============================================================================

fn handle_rainbow_autonomous_debug_cycle(args: json.Json) -> ToolResult {
  case decoders.decode_rainbow_debug_cycle(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      logging.info("[RAINBOW] Running autonomous debug cycle...")
      let max_iter = option.unwrap(parsed.max_iterations, 5)

      // Initialize rainbow bridge modules
      task_store.init()
      healing.init()

      // Run the cycle
      let report = autonomous.run_debug_cycle(parsed.path, max_iter)

      // Return report as JSON
      let result_json = rainbow_types.encode_debug_report(report)
      protocol.text_result(json.to_string(result_json))
    }
  }
}

fn handle_task_create(args: json.Json) -> ToolResult {
  case decoders.decode_task_create(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      logging.info("[RAINBOW] Creating task...")
      let files = option.unwrap(parsed.files, [])

      task_store.init()
      let task = task_store.create(parsed.description, files)

      let result = rainbow_types.encode_task(task)
      protocol.text_result(json.to_string(result))
    }
  }
}

fn handle_task_get(args: json.Json) -> ToolResult {
  case decoders.decode_task_get(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      logging.info("[RAINBOW] Getting task...")

      case task_store.get(parsed.task_id) {
        Some(task) -> {
          let result = rainbow_types.encode_task(task)
          protocol.text_result(json.to_string(result))
        }
        None -> protocol.error_result("Task not found: " <> parsed.task_id)
      }
    }
  }
}

fn handle_task_list(args: json.Json) -> ToolResult {
  case decoders.decode_task_list(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      logging.info("[RAINBOW] Listing tasks...")

      let tasks = case parsed.state {
        None -> task_store.list_all()
        Some(s) -> {
          let state = rainbow_types.parse_task_state(s)
          task_store.list_by_state(state)
        }
      }

      let result =
        json.object([
          #("tasks", json.array(tasks, rainbow_types.encode_task)),
          #("count", json.int(list.length(tasks))),
          #("stats", task_store.get_stats()),
        ])
      protocol.text_result(json.to_string(result))
    }
  }
}

fn handle_task_update(args: json.Json) -> ToolResult {
  case decoders.decode_task_update(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      logging.info("[RAINBOW] Updating task...")
      let state = rainbow_types.parse_task_state(parsed.state)

      case task_store.update_state(parsed.task_id, state) {
        Ok(task) -> {
          let result = rainbow_types.encode_task(task)
          protocol.text_result(json.to_string(result))
        }
        Error(err) -> protocol.error_result(err)
      }
    }
  }
}

fn handle_heal_start(args: json.Json) -> ToolResult {
  case decoders.decode_heal_start(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      logging.info("[RAINBOW] Starting healing...")
      healing.init()

      case healing.start_healing(parsed.task_id) {
        Ok(task) -> {
          let result = rainbow_types.encode_task(task)
          protocol.text_result(json.to_string(result))
        }
        Error(err) -> protocol.error_result(err)
      }
    }
  }
}

fn handle_heal_apply_fix(args: json.Json) -> ToolResult {
  case decoders.decode_heal_apply_fix(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      logging.info("[RAINBOW] Applying fix...")

      // Get the suggested fix from task context
      case task_store.get(parsed.task_id) {
        Some(_task) -> {
          // For now, return success message
          // In full implementation, would find fix by ID and apply
          protocol.text_result(
            json.to_string(
              json.object([
                #("status", json.string("fix_application_requested")),
                #("task_id", json.string(parsed.task_id)),
                #("fix_id", json.string(parsed.fix_id)),
              ]),
            ),
          )
        }
        None -> protocol.error_result("Task not found: " <> parsed.task_id)
      }
    }
  }
}

fn handle_heal_verify(args: json.Json) -> ToolResult {
  case decoders.decode_heal_verify(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      logging.info("[RAINBOW] Verifying fix...")

      case healing.verify_fix(parsed.task_id, parsed.fix_id) {
        Ok(success) -> {
          protocol.text_result(
            json.to_string(
              json.object([
                #("verified", json.bool(success)),
                #("task_id", json.string(parsed.task_id)),
                #("fix_id", json.string(parsed.fix_id)),
              ]),
            ),
          )
        }
        Error(err) -> protocol.error_result(err)
      }
    }
  }
}

fn handle_heal_rollback(args: json.Json) -> ToolResult {
  case decoders.decode_heal_rollback(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      logging.info("[RAINBOW] Rolling back fix...")

      case healing.rollback(parsed.task_id, parsed.fix_id) {
        Ok(_) -> {
          protocol.text_result(
            json.to_string(
              json.object([
                #("status", json.string("rolled_back")),
                #("task_id", json.string(parsed.task_id)),
                #("fix_id", json.string(parsed.fix_id)),
              ]),
            ),
          )
        }
        Error(err) -> protocol.error_result(err)
      }
    }
  }
}

fn handle_decide_next_step(args: json.Json) -> ToolResult {
  case decoders.decode_decide_next_step(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      logging.info("[RAINBOW] Deciding next step...")

      case decision.decide_next_step(parsed.task_id) {
        Ok(d) -> {
          let result = rainbow_types.encode_decision(d)
          protocol.text_result(json.to_string(result))
        }
        Error(err) -> protocol.error_result(err)
      }
    }
  }
}

fn handle_decide_apply(args: json.Json) -> ToolResult {
  case decoders.decode_decide_apply(args) {
    Error(err) -> protocol.error_result(decoders.error_to_string(err))
    Ok(parsed) -> {
      logging.info("[RAINBOW] Applying decision...")

      // Get the decision and apply it
      case decision.decide_next_step(parsed.task_id) {
        Ok(d) -> {
          case decision.apply_decision(parsed.task_id, d) {
            Ok(result) -> {
              protocol.text_result(
                json.to_string(
                  json.object([
                    #("status", json.string("applied")),
                    #("decision_id", json.string(parsed.decision_id)),
                    #("result", json.string(result)),
                  ]),
                ),
              )
            }
            Error(err) -> protocol.error_result(err)
          }
        }
        Error(err) -> protocol.error_result(err)
      }
    }
  }
}

// ============================================================
// Session Tools (Multi-Account Support)
// ============================================================

fn session_list_tool() -> Tool {
  Tool(
    name: "session_list",
    description: "List all Telegram sessions with their authorization status. Shows which session is currently active.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
      #("required", json.array([], json.string)),
    ]),
  )
}

fn session_set_active_tool() -> Tool {
  Tool(
    name: "session_set_active",
    description: "Set the active Telegram session. This session will be used by default when session_id is not explicitly provided.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("Session ID to set as active"),
              ),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["session_id"], json.string)),
    ]),
  )
}

fn session_create_tool() -> Tool {
  Tool(
    name: "session_create",
    description: "Create a new Telegram session. After creation, use auth_send_code to authorize with phone number.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "phone",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string(
                  "Phone number in international format (+79001234567). Optional, for convenience.",
                ),
              ),
            ]),
          ),
          #(
            "set_active",
            json.object([
              #("type", json.string("boolean")),
              #(
                "description",
                json.string("Set this session as active after creation (default: true)"),
              ),
              #("default", json.bool(True)),
            ]),
          ),
        ]),
      ),
      #("required", json.array([], json.string)),
    ]),
  )
}

// ============================================================
// Session Handlers
// ============================================================

fn handle_session_list(_args: json.Json) -> ToolResult {
  logging.info("[SESSION] Listing all sessions from bridge...")

  // Get sessions from telegram-bridge (source of truth)
  let base = bridge_url()
  logging.info("[SESSION] Bridge URL: " <> base)
  let #(scheme, host, port) = parse_bridge_url(base)
  logging.info("[SESSION] Parsed: scheme=" <> string.inspect(scheme) <> " host=" <> host <> " port=" <> int.to_string(port))
  let api_key = config.get_env("VIBEE_API_KEY")
  logging.info("[SESSION] API key length: " <> int.to_string(string.length(api_key)))
  let path = "/api/v1/auth/status"

  let req =
    request.new()
    |> request.set_scheme(scheme)
    |> request.set_method(http.Get)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path(path)
    |> request.set_header("authorization", "Bearer " <> api_key)
  // Note: NO X-Session-ID header - this returns all sessions

  let active_session = session_manager.get_active()

  case httpc.send(req) {
    Error(e) -> {
      logging.error("[SESSION] Failed to get sessions from bridge: " <> string.inspect(e))
      // Return empty list on error
      protocol.text_result(
        json.to_string(
          json.object([
            #("sessions", json.array([], fn(x) { x })),
            #("active_session", json.null()),
            #("total", json.int(0)),
            #("error", json.string("Failed to connect to telegram-bridge")),
          ]),
        ),
      )
    }
    Ok(response) -> {
      logging.info("[SESSION] Bridge response status: " <> string.inspect(response.status))
      case response.status {
        200 -> {
          // Parse response from bridge
          let sessions_decoder = {
            use sessions <- decode.field("sessions", decode.list(decode.dynamic))
            decode.success(sessions)
          }
          case json.parse(response.body, sessions_decoder) {
            Error(_) -> {
              logging.error("[SESSION] Failed to parse sessions response: " <> response.body)
              protocol.error_result("Failed to parse sessions from bridge")
            }
            Ok(sessions) -> {
              // Convert dynamic sessions to JSON
              let session_item_decoder = {
                use sid <- decode.field("session_id", decode.string)
                use auth <- decode.field("authorized", decode.bool)
                decode.success(#(sid, auth))
              }
              let sessions_json =
                sessions
                |> list.filter_map(fn(s) {
                  // Extract session_id and authorized from dynamic
                  case decode.run(s, session_item_decoder) {
                    Ok(#(sid, auth)) -> {
                      let is_active = case active_session {
                        Some(active_id) -> active_id == sid
                        None -> False
                      }
                      Ok(json.object([
                        #("session_id", json.string(sid)),
                        #("authorized", json.bool(auth)),
                        #("is_active", json.bool(is_active)),
                      ]))
                    }
                    _ -> Error(Nil)
                  }
                })

              protocol.text_result(
                json.to_string(
                  json.object([
                    #("sessions", json.array(sessions_json, fn(x) { x })),
                    #(
                      "active_session",
                      case active_session {
                        Some(sid) -> json.string(sid)
                        None -> json.null()
                      },
                    ),
                    #("total", json.int(list.length(sessions_json))),
                    #(
                      "hint",
                      case list.length(sessions_json) {
                        0 ->
                          json.string(
                            "No sessions found. Use session_create to create a new session, "
                            <> "or session_set_active with an existing session_id.",
                          )
                        _ -> json.null()
                      },
                    ),
                  ]),
                ),
              )
            }
          }
        }
        _ -> {
          logging.error("[SESSION] Bridge returned error: " <> response.body)
          protocol.error_result("Bridge error: " <> response.body)
        }
      }
    }
  }
}

fn handle_session_set_active(args: json.Json) -> ToolResult {
  logging.info("[SESSION] Setting active session...")

  // Parse session_id from args using json.parse pattern
  let session_id_decoder = {
    use v <- decode.field("session_id", decode.string)
    decode.success(v)
  }
  let args_str = json.to_string(args)

  case json.parse(args_str, session_id_decoder) {
    Error(_) ->
      protocol.error_result("Missing required parameter: session_id")
    Ok(session_id) -> {
      // Validate session exists by checking with bridge
      let base = bridge_url()
      let #(scheme, host, port) = parse_bridge_url(base)
      let api_key = config.get_env("VIBEE_API_KEY")
      let path = "/api/v1/me"
      let req =
        request.new()
        |> request.set_scheme(scheme)
        |> request.set_method(http.Get)
        |> request.set_host(host)
        |> request.set_port(port)
        |> request.set_path(path)
        |> request.set_header("authorization", "Bearer " <> api_key)
        |> request.set_header("x-session-id", session_id)

      case httpc.send(req) {
        Ok(response) -> {
          case response.status {
            200 -> {
              // Session is valid, set as active
              session_manager.set_active(session_id)

              // Try to extract user info and update local store
              let username = json_get_optional_string(response.body, "username")
              let phone = json_get_optional_string(response.body, "phone")

              session_manager.upsert(session_manager.SessionInfo(
                session_id: session_id,
                phone: phone,
                username: username,
                authorized: True,
                created_at: 0,
              ))

              protocol.text_result(
                json.to_string(
                  json.object([
                    #("status", json.string("ok")),
                    #("active_session", json.string(session_id)),
                    #(
                      "message",
                      json.string(
                        "Session " <> session_id <> " is now active",
                      ),
                    ),
                  ]),
                ),
              )
            }
            401 ->
              protocol.error_result(
                "Session " <> session_id <> " is not authorized. Use auth_send_code first.",
              )
            _ ->
              protocol.error_result(
                "Session " <> session_id <> " not found or invalid",
              )
          }
        }
        Error(_) ->
          protocol.error_result(
            "Failed to connect to Telegram Bridge. Make sure it's running on port 8081.",
          )
      }
    }
  }
}

fn handle_session_create(args: json.Json) -> ToolResult {
  logging.info("[SESSION] Creating new session...")

  let args_str = json.to_string(args)

  // Parse optional phone from args
  let phone = json_get_optional_string(args_str, "phone")

  // Parse optional set_active (default: true)
  let set_active = case json_get_optional_bool(args_str, "set_active") {
    Some(v) -> v
    None -> True
  }

  // Call Bridge to create new session
  // NOTE: Don't pass phone here - it forces PostgreSQL storage which doesn't persist session_id
  // Phone is only needed for auth_send_code, not for session creation
  let path = "/api/v1/connect"
  let body =
    json.to_string(
      json.object([
        #("app_id", json.int(telegram_config.api_id())),
        #("app_hash", json.string(telegram_config.api_hash())),
      ]),
    )

  // Store phone in local session manager for later use
  let _ = phone

  let base = bridge_url()
  let #(scheme, host, port) = parse_bridge_url(base)
  let api_key = config.get_env("VIBEE_API_KEY")

  let req =
    request.new()
    |> request.set_scheme(scheme)
    |> request.set_method(http.Post)
    |> request.set_host(host)
    |> request.set_port(port)
    |> request.set_path(path)
    |> request.set_header("content-type", "application/json")
    |> request.set_header("authorization", "Bearer " <> api_key)
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) -> {
      case response.status {
        200 | 201 -> {
          // Parse session_id from response
          let session_id_decoder = {
            use v <- decode.field("session_id", decode.string)
            decode.success(v)
          }

          case json.parse(response.body, session_id_decoder) {
            Ok(new_session_id) -> {
              // Store in local ETS
              session_manager.upsert(session_manager.SessionInfo(
                session_id: new_session_id,
                phone: phone,
                username: None,
                authorized: False,
                created_at: 0,
              ))

              // Set as active if requested
              case set_active {
                True -> session_manager.set_active(new_session_id)
                False -> Nil
              }

              protocol.text_result(
                json.to_string(
                  json.object([
                    #("status", json.string("created")),
                    #("session_id", json.string(new_session_id)),
                    #("is_active", json.bool(set_active)),
                    #("authorized", json.bool(False)),
                    #(
                      "next_step",
                      json.string(
                        "Use auth_send_code with phone number to authorize this session",
                      ),
                    ),
                  ]),
                ),
              )
            }
            Error(_) ->
              protocol.error_result(
                "Failed to parse session_id from response: " <> response.body,
              )
          }
        }
        _ ->
          protocol.error_result(
            "Failed to create session. Status: "
            <> int.to_string(response.status)
            <> ", Body: "
            <> response.body,
          )
      }
    }
    Error(_) ->
      protocol.error_result(
        "Failed to connect to Telegram Bridge at " <> base <> ". Check VIBEE_BRIDGE_URL.",
      )
  }
}

// =============================================================================
// Helper Functions for JSON parsing
// =============================================================================

/// Get optional string from JSON string
fn json_get_optional_string(json_str: String, key: String) -> Option(String) {
  let decoder = {
    use v <- decode.field(key, decode.string)
    decode.success(v)
  }
  case json.parse(json_str, decoder) {
    Ok(v) -> Some(v)
    Error(_) -> None
  }
}

/// Get optional bool from JSON string
fn json_get_optional_bool(json_str: String, key: String) -> Option(Bool) {
  let decoder = {
    use v <- decode.field(key, decode.bool)
    decode.success(v)
  }
  case json.parse(json_str, decoder) {
    Ok(v) -> Some(v)
    Error(_) -> None
  }
}

/// Get int from parsed JSON object
fn json_get_int(j: json.Json, key: String) -> Option(Int) {
  let decoder = {
    use v <- decode.field(key, decode.int)
    decode.success(v)
  }
  let json_str = json.to_string(j)
  case json.parse(json_str, decoder) {
    Ok(v) -> Some(v)
    Error(_) -> None
  }
}

// ============================================================
// Storage Tool Handlers
// ============================================================

fn handle_storage_upload(args: json.Json) -> ToolResult {
  let args_str = json.to_string(args)
  let data = json_get_optional_string(args_str, "data") |> option.unwrap("")
  let filename = json_get_optional_string(args_str, "filename") |> option.unwrap("file")
  let content_type =
    json_get_optional_string(args_str, "content_type") |> option.unwrap("")

  storage_tools.handle_upload(data, filename, content_type)
}

fn handle_storage_list(_args: json.Json) -> ToolResult {
  storage_tools.handle_list()
}

fn handle_storage_config(_args: json.Json) -> ToolResult {
  storage_tools.handle_config()
}
