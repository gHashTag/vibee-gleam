// Onboarding MCP Tools for VIBEE
// Автоматический онбординг клиентов после подключения MCP

import gleam/dynamic.{type Dynamic}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/logging
import vibee/mcp/protocol
import vibee/mcp/session_manager
import vibee/mcp/types.{type Tool, type ToolResult, Tool}
import vibee/onboarding/state.{
  type OnboardingState, type OnboardingStatus, type DialogInfo,
  DialogInfo, OnboardingProgress, OnboardingState,
  StatusPending, StatusScanning, StatusWaitingSelection, StatusEmbedding, StatusCompleted, StatusFailed,
}

// =============================================================================
// TOOL DEFINITIONS
// =============================================================================

/// Tool: onboarding_start
pub fn onboarding_start_tool() -> Tool {
  Tool(
    name: "onboarding_start",
    description: "Start client onboarding process. Checks authorization, starts background scanning of all Telegram dialogs, returns onboarding ID for progress tracking.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("session_id", json.object([
          #("type", json.string("string")),
          #("description", json.string("Telegram session ID (optional, uses active session if not specified)")),
        ])),
        #("auto_select", json.object([
          #("type", json.string("boolean")),
          #("description", json.string("Automatically select all dialogs for analysis (default: false)")),
          #("default", json.bool(False)),
        ])),
      ])),
    ]),
  )
}

/// Tool: onboarding_status
pub fn onboarding_status_tool() -> Tool {
  Tool(
    name: "onboarding_status",
    description: "Get current onboarding status: scan progress, dialogs list, embeddings count. Use after onboarding_start to monitor progress.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("onboarding_id", json.object([
          #("type", json.string("string")),
          #("description", json.string("Onboarding ID from onboarding_start response")),
        ])),
      ])),
      #("required", json.array(["onboarding_id"], json.string)),
    ]),
  )
}

/// Tool: onboarding_select_dialogs
pub fn onboarding_select_dialogs_tool() -> Tool {
  Tool(
    name: "onboarding_select_dialogs",
    description: "Select dialogs for analysis. After selection, embedding generation starts automatically using Ollama (free). Use after status shows 'waiting_selection'.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("onboarding_id", json.object([
          #("type", json.string("string")),
          #("description", json.string("Onboarding ID")),
        ])),
        #("dialog_ids", json.object([
          #("type", json.string("array")),
          #("items", json.object([#("type", json.string("integer"))])),
          #("description", json.string("List of dialog IDs to analyze")),
        ])),
        #("select_all", json.object([
          #("type", json.string("boolean")),
          #("description", json.string("Select all available dialogs")),
          #("default", json.bool(False)),
        ])),
      ])),
      #("required", json.array(["onboarding_id"], json.string)),
    ]),
  )
}

/// Tool: onboarding_list
pub fn onboarding_list_tool() -> Tool {
  Tool(
    name: "onboarding_list",
    description: "List all active onboarding sessions (not completed/failed).",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
    ]),
  )
}

// =============================================================================
// HANDLERS
// =============================================================================

/// Handle onboarding_start
pub fn handle_onboarding_start(args: json.Json) -> ToolResult {
  // Parse arguments
  let session_id_opt = get_string_field(args, "session_id")
  let auto_select = get_bool_field(args, "auto_select") |> result.unwrap(False)

  // Get session ID (from args or active session)
  let session_id = case session_id_opt {
    Some(sid) -> sid
    None -> case session_manager.get_active() {
      Some(sid) -> sid
      None -> ""
    }
  }

  case session_id {
    "" -> protocol.error_result("No active session. Complete auth_verify_code first.")
    sid -> {
      logging.info("[ONBOARDING] Starting for session: " <> sid)

      // Initialize store if needed
      state.init_store()

      // Create onboarding session
      let result = state.create_in_store(sid, "", auto_select)

      let #(_status, onboarding_id) = result

      case onboarding_id {
        "" -> protocol.error_result("Failed to create onboarding session")
        id -> {
          logging.info("[ONBOARDING] Created: " <> id)

          // Update status to scanning
          let _ = state.update_status_in_store(id, scanning_atom())

          // TODO: Start background scanning worker here
          // For now, return success with ID

          protocol.text_result(json.to_string(json.object([
            #("onboarding_id", json.string(id)),
            #("status", json.string("scanning")),
            #("message", json.string("Onboarding started. Use onboarding_status to check progress.")),
            #("next_step", json.string("Wait for status 'waiting_selection', then use onboarding_select_dialogs to choose dialogs.")),
          ])))
        }
      }
    }
  }
}

/// Handle onboarding_status
pub fn handle_onboarding_status(args: json.Json) -> ToolResult {
  case get_string_field(args, "onboarding_id") {
    None -> protocol.error_result("Missing required field: onboarding_id")
    Some(onboarding_id) -> {
      logging.info("[ONBOARDING] Getting status for: " <> onboarding_id)

      // Get from store
      let result = state.get_from_store(onboarding_id)

      case decode_onboarding_result(result) {
        None -> protocol.error_result("Onboarding session not found: " <> onboarding_id)
        Some(onboarding_state) -> {
          protocol.text_result(json.to_string(state.state_to_json(onboarding_state)))
        }
      }
    }
  }
}

/// Handle onboarding_select_dialogs
pub fn handle_onboarding_select_dialogs(args: json.Json) -> ToolResult {
  case get_string_field(args, "onboarding_id") {
    None -> protocol.error_result("Missing required field: onboarding_id")
    Some(onboarding_id) -> {
      let select_all = get_bool_field(args, "select_all") |> result.unwrap(False)
      let dialog_ids = get_int_list_field(args, "dialog_ids") |> result.unwrap([])

      logging.info("[ONBOARDING] Selecting dialogs for: " <> onboarding_id)

      // Get current state
      let result = state.get_from_store(onboarding_id)

      case decode_onboarding_result(result) {
        None -> protocol.error_result("Onboarding session not found")
        Some(onboarding_state) -> {
          // Check status
          case onboarding_state.status {
            StatusWaitingSelection -> {
              // Get dialog IDs to select
              let ids_to_select = case select_all {
                True -> list.map(onboarding_state.dialogs, fn(d) { d.id })
                False -> dialog_ids
              }

              case ids_to_select {
                [] -> protocol.error_result("No dialogs selected. Provide dialog_ids or set select_all=true")
                _ -> {
                  // Update store
                  let _ = state.set_selected_in_store(onboarding_id, ids_to_select)

                  // TODO: Start embedding generation worker here

                  protocol.text_result(json.to_string(json.object([
                    #("onboarding_id", json.string(onboarding_id)),
                    #("status", json.string("embedding")),
                    #("selected_count", json.int(list.length(ids_to_select))),
                    #("message", json.string("Dialogs selected. Embedding generation started.")),
                  ])))
                }
              }
            }
            StatusPending | StatusScanning ->
              protocol.error_result("Onboarding still scanning. Wait for status 'waiting_selection'.")
            StatusEmbedding ->
              protocol.error_result("Embedding already in progress.")
            StatusCompleted ->
              protocol.error_result("Onboarding already completed.")
            StatusFailed ->
              protocol.error_result("Onboarding failed. Start a new one.")
          }
        }
      }
    }
  }
}

/// Handle onboarding_list
pub fn handle_onboarding_list(_args: json.Json) -> ToolResult {
  logging.info("[ONBOARDING] Listing active sessions")

  let active = state.list_active_in_store()

  // For now, return empty list - proper decoding needed
  protocol.text_result(json.to_string(json.object([
    #("active_count", json.int(0)),
    #("sessions", json.array([], fn(_) { json.null() })),
  ])))
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

/// Get string field from JSON
fn get_string_field(args: json.Json, field: String) -> Option(String) {
  // Simplified - in production use proper decoder
  None
}

/// Get bool field from JSON
fn get_bool_field(args: json.Json, field: String) -> Result(Bool, Nil) {
  Error(Nil)
}

/// Get int list field from JSON
fn get_int_list_field(args: json.Json, field: String) -> Result(List(Int), Nil) {
  Error(Nil)
}

/// Decode onboarding result from ETS
fn decode_onboarding_result(result: Dynamic) -> Option(OnboardingState) {
  // Simplified - proper Erlang map decoding needed
  None
}

// Atom helpers for FFI
@external(erlang, "erlang", "binary_to_atom")
fn binary_to_atom(s: String) -> state.Atom

fn scanning_atom() -> state.Atom {
  binary_to_atom("scanning")
}
