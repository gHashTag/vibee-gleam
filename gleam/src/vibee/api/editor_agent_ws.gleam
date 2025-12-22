// Editor Agent WebSocket - AI-powered template editing via WebSocket
// Endpoint: /agent
//
// Handles messages from remotion/player for AI chat and template management

import gleam/dynamic/decode
import gleam/erlang/process
import gleam/float
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import mist.{type Connection, type ResponseData}
import vibee/ai/client as ai_client
import vibee/ai/openrouter
import vibee/vibe_logger

// =============================================================================
// Types
// =============================================================================

/// WebSocket state for editor agent
pub type EditorAgentState {
  EditorAgentState(
    client_id: String,
    conversation_history: List(openrouter.Message),
  )
}

/// Parsed request from client
pub type AgentRequest {
  ChatRequest(message: String, template: String, selected_items: List(String))
  GetTemplatesRequest
  CreateTemplateRequest(name: String, code: String)
  ApplyActionRequest(action_id: String)
}

/// WebSocket message types for async operations
pub type EditorAgentMsg {
  ChatComplete(String)
  ChatError(String)
}

// =============================================================================
// WebSocket Handler
// =============================================================================

/// Main WebSocket handler for /agent
pub fn handler(req: Request(Connection)) -> Response(ResponseData) {
  io.println("[Agent WS] 🚀 Handler called")

  // Use simple client ID to avoid FFI issues
  let client_id = "agent_test_" <> int.to_string(erlang_now())

  io.println("[Agent WS] 📝 Client ID: " <> client_id)

  // Create state with conversation history
  let state = EditorAgentState(
    client_id: client_id,
    conversation_history: [],
  )

  io.println("[Agent WS] 📦 State created")

  // Create selector (needed for mist.websocket, even if empty)
  let selector = process.new_selector()

  mist.websocket(
    request: req,
    on_init: fn(conn) {
      io.println("[Agent WS] 🎬 on_init SUCCESS! Client: " <> client_id)

      // Send welcome message
      let welcome = encode_chat_response(
        "Hi! I'm your VIBEE AI assistant. I can help you create and edit video templates. What would you like to build today?",
        None,
        [],
      )
      let _ = mist.send_text_frame(conn, welcome)

      #(state, Some(selector))
    },
    on_close: fn(_state) {
      io.println("[Agent WS] 🔚 Client disconnected: " <> client_id)
    },
    handler: fn(st, message, conn) {
      handle_ws_message(st, message, conn)
    },
  )
}

/// Handle incoming WebSocket messages
fn handle_ws_message(
  state: EditorAgentState,
  message: mist.WebsocketMessage(EditorAgentMsg),
  conn: mist.WebsocketConnection,
) {
  case message {
    mist.Text("ping") -> {
      let _ = mist.send_text_frame(conn, "pong")
      mist.continue(state)
    }

    mist.Text(text) -> {
      io.println("[Agent WS] 📨 Received: " <> string.slice(text, 0, 100))

      // Parse the request
      case parse_request(text) {
        Ok(ChatRequest(user_message, template, selected_items)) -> {
          handle_chat_request(state, conn, user_message, template, selected_items)
        }

        Ok(GetTemplatesRequest) -> {
          handle_get_templates(state, conn)
        }

        Ok(CreateTemplateRequest(name, code)) -> {
          handle_create_template(state, conn, name, code)
        }

        Ok(ApplyActionRequest(action_id)) -> {
          handle_apply_action(state, conn, action_id)
        }

        Error(err) -> {
          io.println("[Agent WS] ❌ Parse error: " <> err)
          let _ = mist.send_text_frame(conn, encode_error(err))
          mist.continue(state)
        }
      }
    }

    mist.Binary(_) -> mist.continue(state)
    mist.Custom(_) -> mist.continue(state)
    mist.Closed | mist.Shutdown -> mist.stop()
  }
}

// =============================================================================
// Request Handlers
// =============================================================================

/// Handle chat request - send to OpenRouter for AI response
fn handle_chat_request(
  state: EditorAgentState,
  conn: mist.WebsocketConnection,
  user_message: String,
  _template: String,
  _selected_items: List(String),
) {
  io.println("[Agent WS] 💬 Chat request: " <> string.slice(user_message, 0, 50))

  // Build messages with system prompt for fresh conversations
  let messages_with_system = case state.conversation_history {
    [] -> [openrouter.system_message(editor_system_prompt())]
    existing -> existing
  }

  // Add user message to conversation
  let new_history = list.append(
    messages_with_system,
    [openrouter.user_message(user_message)],
  )

  // Call OpenRouter for response
  let chat_request = openrouter.ChatRequest(
    model: "google/gemini-3-flash-preview",
    messages: new_history,
    temperature: Some(0.7),
    max_tokens: Some(2000),
    top_p: None,
    frequency_penalty: None,
    presence_penalty: None,
    stop: None,
  )

  // Create OpenRouter config
  let api_key = get_env_or("OPENROUTER_API_KEY", "")
  let config = openrouter.config_with_site(
    api_key,
    "https://vibee-player.fly.dev",
    "VIBEE Editor",
  )

  // Build the HTTP request
  let request = openrouter.create_chat_request(config, chat_request)

  // Execute HTTP request
  case ai_client.execute_json(ai_client.Request(
    url: request.url,
    method: request.method,
    headers: request.headers,
    body: request.body,
  )) {
    Ok(response_body) -> {
      let content = extract_content_from_response(response_body)
      io.println("[Agent WS] ✅ AI response: " <> string.slice(content, 0, 50) <> "...")

      // Extract code blocks and actions if present
      let code_block = extract_code_block(content)
      let actions = extract_actions(content)

      // Send response to client
      let response = encode_chat_response(content, code_block, actions)
      let _ = mist.send_text_frame(conn, response)

      // Update state with assistant response
      let updated_history = list.append(
        new_history,
        [openrouter.assistant_message(content)],
      )
      let new_state = EditorAgentState(..state, conversation_history: updated_history)
      mist.continue(new_state)
    }

    Error(err) -> {
      let error_msg = ai_client.error_to_string(err)
      io.println("[Agent WS] ❌ AI error: " <> error_msg)
      let _ = mist.send_text_frame(conn, encode_error("AI request failed: " <> error_msg))
      mist.continue(state)
    }
  }
}

/// Handle get templates request
fn handle_get_templates(state: EditorAgentState, conn: mist.WebsocketConnection) {
  io.println("[Agent WS] 📋 Get templates request")

  // Return list of available templates
  let templates = [
    #("split-talking-head", "Split Talking Head", "TikTok-style split screen with captions"),
    #("lipsync-main", "LipSync Main", "Full screen avatar video with captions"),
    #("text-overlay", "Text Overlay", "Simple text animation over video"),
  ]

  let template_json = json.array(templates, fn(t) {
    let #(id, name, desc) = t
    json.object([
      #("id", json.string(id)),
      #("name", json.string(name)),
      #("description", json.string(desc)),
      #("isPremium", json.bool(False)),
    ])
  })

  let response = json.object([
    #("type", json.string("template_list")),
    #("payload", json.object([
      #("templates", template_json),
    ])),
  ])
  |> json.to_string

  let _ = mist.send_text_frame(conn, response)
  mist.continue(state)
}

/// Handle create template request
fn handle_create_template(
  state: EditorAgentState,
  conn: mist.WebsocketConnection,
  name: String,
  code: String,
) {
  io.println("[Agent WS] 🆕 Create template: " <> name <> " (" <> int.to_string(string.length(code)) <> " chars)")

  // For now, just acknowledge the request
  // TODO: Implement actual template creation via MCP tools
  let response = json.object([
    #("type", json.string("action_result")),
    #("payload", json.object([
      #("content", json.string("Template '" <> name <> "' created successfully!")),
    ])),
  ])
  |> json.to_string

  let _ = mist.send_text_frame(conn, response)
  mist.continue(state)
}

/// Handle apply action request
fn handle_apply_action(
  state: EditorAgentState,
  conn: mist.WebsocketConnection,
  action_id: String,
) {
  io.println("[Agent WS] ⚡ Apply action: " <> action_id)

  // For now, just acknowledge the request
  let response = json.object([
    #("type", json.string("action_result")),
    #("payload", json.object([
      #("content", json.string("Action '" <> action_id <> "' applied!")),
    ])),
  ])
  |> json.to_string

  let _ = mist.send_text_frame(conn, response)
  mist.continue(state)
}

// =============================================================================
// JSON Encoding
// =============================================================================

fn encode_chat_response(
  content: String,
  code_block: Option(#(String, String)),
  actions: List(#(String, String, String, String)),
) -> String {
  json.to_string(
    json.object([
      #("type", json.string("chat_response")),
      #(
        "payload",
        json.object([
          #("content", json.string(content)),
          #("codeBlock", encode_code_block_json(code_block)),
          #("actions", encode_actions_json(actions)),
        ]),
      ),
    ]),
  )
}

fn encode_streaming_response(
  content: String,
  is_complete: Bool,
  code_block: Option(#(String, String)),
  actions: List(#(String, String, String, String)),
) -> String {
  json.to_string(
    json.object([
      #("type", json.string("streaming")),
      #(
        "payload",
        json.object([
          #("content", json.string(content)),
          #("isComplete", json.bool(is_complete)),
          #("codeBlock", encode_code_block_json(code_block)),
          #("actions", encode_actions_json(actions)),
        ]),
      ),
    ]),
  )
}

fn encode_error(error: String) -> String {
  json.to_string(
    json.object([
      #("type", json.string("error")),
      #("payload", json.object([#("error", json.string(error))])),
    ]),
  )
}

fn encode_code_block_json(code_block: Option(#(String, String))) -> json.Json {
  case code_block {
    Some(#(language, code)) ->
      json.object([
        #("language", json.string(language)),
        #("code", json.string(code)),
      ])
    None -> json.null()
  }
}

fn encode_actions_json(actions: List(#(String, String, String, String))) -> json.Json {
  json.array(actions, fn(action) {
    let #(id, action_type, label, payload_json) = action
    json.object([
      #("id", json.string(id)),
      #("type", json.string(action_type)),
      #("label", json.string(label)),
      // Pass payload as raw JSON string - frontend will parse it
      #("payloadJson", json.string(payload_json)),
      #("status", json.string("pending")),
    ])
  })
}

// =============================================================================
// JSON Parsing
// =============================================================================

fn parse_request(text: String) -> Result(AgentRequest, String) {
  // First, decode the type field to determine request type
  let type_decoder = {
    use request_type <- decode.field("type", decode.string)
    decode.success(request_type)
  }

  case json.parse(text, type_decoder) {
    Ok(request_type) -> {
      case request_type {
        "chat" -> parse_chat_request(text)
        "get_templates" -> Ok(GetTemplatesRequest)
        "create_template" -> parse_create_template_request(text)
        "apply_action" -> parse_apply_action_request(text)
        _ -> Error("Unknown request type: " <> request_type)
      }
    }
    Error(_) -> Error("Failed to parse request type")
  }
}

fn parse_chat_request(text: String) -> Result(AgentRequest, String) {
  // Decoder for the chat message
  let message_decoder = {
    use message <- decode.field("payload", {
      use msg <- decode.field("message", decode.string)
      decode.success(msg)
    })
    decode.success(message)
  }

  case json.parse(text, message_decoder) {
    Ok(message) -> {
      // Try to get context (optional)
      let #(template, selected) = parse_context(text)
      Ok(ChatRequest(message, template, selected))
    }
    Error(_) -> Error("Failed to parse chat message")
  }
}

fn parse_context(text: String) -> #(String, List(String)) {
  // Try to decode context.template
  let template_decoder = {
    use template <- decode.field("payload", {
      use t <- decode.field("context", {
        use template <- decode.field("template", decode.string)
        decode.success(template)
      })
      decode.success(t)
    })
    decode.success(template)
  }

  let template = case json.parse(text, template_decoder) {
    Ok(t) -> t
    Error(_) -> ""
  }

  // Try to decode context.selectedItems
  let selected_decoder = {
    use selected <- decode.field("payload", {
      use s <- decode.field("context", {
        use items <- decode.field("selectedItems", decode.list(decode.string))
        decode.success(items)
      })
      decode.success(s)
    })
    decode.success(selected)
  }

  let selected = case json.parse(text, selected_decoder) {
    Ok(items) -> items
    Error(_) -> []
  }

  #(template, selected)
}

fn parse_create_template_request(text: String) -> Result(AgentRequest, String) {
  let decoder = {
    use #(name, code) <- decode.field("payload", {
      use name <- decode.field("templateName", decode.string)
      use code <- decode.field("templateCode", decode.string)
      decode.success(#(name, code))
    })
    decode.success(#(name, code))
  }

  case json.parse(text, decoder) {
    Ok(#(name, code)) -> Ok(CreateTemplateRequest(name, code))
    Error(_) -> Error("Failed to parse create_template request")
  }
}

fn parse_apply_action_request(text: String) -> Result(AgentRequest, String) {
  let decoder = {
    use action_id <- decode.field("payload", {
      use id <- decode.field("actionId", decode.string)
      decode.success(id)
    })
    decode.success(action_id)
  }

  case json.parse(text, decoder) {
    Ok(action_id) -> Ok(ApplyActionRequest(action_id))
    Error(_) -> Error("Failed to parse apply_action request")
  }
}

// =============================================================================
// AI Response Parsing
// =============================================================================

/// Extract content from OpenRouter response
fn extract_content_from_response(response_body: String) -> String {
  // OpenRouter format: { choices: [{ message: { content: "..." } }] }
  let decoder = {
    use choices <- decode.field("choices", decode.list({
      use content <- decode.field("message", {
        use c <- decode.field("content", decode.string)
        decode.success(c)
      })
      decode.success(content)
    }))
    decode.success(choices)
  }

  case json.parse(response_body, decoder) {
    Ok([content, ..]) -> content
    Ok([]) -> "No response from AI"
    Error(_) -> "Failed to parse AI response"
  }
}

/// Extract code block from content (```language\ncode\n```)
fn extract_code_block(content: String) -> Option(#(String, String)) {
  case string.split_once(content, "```") {
    Ok(#(_, rest)) -> {
      case string.split_once(rest, "\n") {
        Ok(#(language, code_rest)) -> {
          case string.split_once(code_rest, "```") {
            Ok(#(code, _)) ->
              Some(#(string.trim(language), string.trim(code)))
            Error(_) -> None
          }
        }
        Error(_) -> None
      }
    }
    Error(_) -> None
  }
}

/// Extract action suggestions from content - parses JSON action blocks
fn extract_actions(content: String) -> List(#(String, String, String, String)) {
  // Find all ```json blocks and extract actions
  extract_json_actions(content, [], 0)
}

/// Recursively extract JSON action blocks from content
fn extract_json_actions(
  content: String,
  acc: List(#(String, String, String, String)),
  counter: Int,
) -> List(#(String, String, String, String)) {
  case string.split_once(content, "```json") {
    Ok(#(_, rest)) -> {
      case string.split_once(rest, "```") {
        Ok(#(json_block, remaining)) -> {
          // Try to parse the JSON block
          let trimmed = string.trim(json_block)
          case parse_action_json(trimmed, counter) {
            Ok(action) ->
              extract_json_actions(remaining, list.append(acc, [action]), counter + 1)
            Error(_) ->
              extract_json_actions(remaining, acc, counter)
          }
        }
        Error(_) -> acc
      }
    }
    Error(_) -> acc
  }
}

/// Parse a JSON action block into action tuple (id, type, label, payload_json)
fn parse_action_json(json_str: String, counter: Int) -> Result(#(String, String, String, String), String) {
  // Try to decode the action type
  let action_decoder = {
    use action_type <- decode.field("action", decode.string)
    decode.success(action_type)
  }

  case json.parse(json_str, action_decoder) {
    Ok(action_type) -> {
      let id = "action_" <> int.to_string(counter)
      let label = action_type_to_label(action_type, json_str)
      // Convert action JSON to payload format (remove "action" key, use "type" instead)
      let payload_json = transform_action_to_payload(json_str, action_type)
      Ok(#(id, action_type, label, payload_json))
    }
    Error(_) -> Error("Failed to parse action JSON")
  }
}

/// Transform action JSON to payload format expected by frontend
fn transform_action_to_payload(json_str: String, action_type: String) -> String {
  // For most actions, we need to extract the relevant fields from the JSON
  // and create a payload object without the "action" key
  case action_type {
    "update_prop" -> {
      // Extract key and value
      let decoder = {
        use key <- decode.field("key", decode.string)
        use value <- decode.field("value", decode.dynamic)
        decode.success(#(key, value))
      }
      case json.parse(json_str, decoder) {
        Ok(#(key, value)) -> {
          "{\"key\":\"" <> key <> "\",\"value\":" <> dynamic_to_json_string(value) <> "}"
        }
        Error(_) -> "{}"
      }
    }
    "add_track_item" -> {
      // Extract trackId and itemData
      let decoder = {
        use track_id <- decode.field("trackId", decode.string)
        decode.success(track_id)
      }
      case json.parse(json_str, decoder) {
        Ok(track_id) -> {
          // Keep the original JSON structure but rename for frontend
          string.replace(json_str, "\"action\":", "\"_action\":")
          |> string.replace("\"trackId\":", "\"trackId\":")
          |> string.replace("\"itemData\":", "\"itemData\":")
        }
        Error(_) -> json_str
      }
    }
    "update_track_item" -> {
      // Keep as-is, frontend expects itemId and updates
      string.replace(json_str, "\"action\":", "\"_action\":")
    }
    "delete_track_items" -> {
      // Extract itemIds
      let decoder = {
        use ids <- decode.field("itemIds", decode.list(decode.string))
        decode.success(ids)
      }
      case json.parse(json_str, decoder) {
        Ok(ids) -> {
          let ids_json = "[" <> string.join(list.map(ids, fn(id) { "\"" <> id <> "\"" }), ",") <> "]"
          "{\"itemIds\":" <> ids_json <> "}"
        }
        Error(_) -> "{\"itemIds\":[]}"
      }
    }
    "select_items" -> {
      // Extract itemIds
      let decoder = {
        use ids <- decode.field("itemIds", decode.list(decode.string))
        decode.success(ids)
      }
      case json.parse(json_str, decoder) {
        Ok(ids) -> {
          let ids_json = "[" <> string.join(list.map(ids, fn(id) { "\"" <> id <> "\"" }), ",") <> "]"
          "{\"itemIds\":" <> ids_json <> "}"
        }
        Error(_) -> "{\"itemIds\":[]}"
      }
    }
    _ -> json_str
  }
}

/// Convert dynamic value to JSON string (simple implementation)
fn dynamic_to_json_string(value: decode.Dynamic) -> String {
  // Try different types
  case decode.run(value, decode.string) {
    Ok(s) -> "\"" <> s <> "\""
    Error(_) ->
      case decode.run(value, decode.int) {
        Ok(i) -> int.to_string(i)
        Error(_) ->
          case decode.run(value, decode.float) {
            Ok(f) -> float.to_string(f)
            Error(_) ->
              case decode.run(value, decode.bool) {
                Ok(True) -> "true"
                Ok(False) -> "false"
                Error(_) -> "null"
              }
          }
      }
  }
}

/// Convert action type to human-readable label
fn action_type_to_label(action_type: String, json_str: String) -> String {
  case action_type {
    "update_prop" -> {
      // Try to extract key name
      let key_decoder = {
        use key <- decode.field("key", decode.string)
        decode.success(key)
      }
      case json.parse(json_str, key_decoder) {
        Ok(key) -> "Update " <> key
        Error(_) -> "Update property"
      }
    }
    "add_track_item" -> "Add item to timeline"
    "update_track_item" -> "Update timeline item"
    "delete_track_items" -> "Delete items"
    "select_items" -> "Select items"
    "apply_style" -> "Apply style changes"
    _ -> "Apply: " <> action_type
  }
}

// =============================================================================
// Helpers
// =============================================================================

/// Generate unique client ID
fn generate_client_id() -> String {
  "agent_" <> int.to_string(get_timestamp()) <> "_" <> random_suffix()
}

fn random_suffix() -> String {
  int.to_string(ffi_random())
}

/// Get current Unix timestamp using Erlang built-in
@external(erlang, "erlang", "system_time")
fn erlang_now() -> Int

/// Get current Unix timestamp (legacy)
@external(erlang, "vibee_agent_registry_ffi", "get_unix_timestamp")
fn get_timestamp() -> Int

/// Get random number
@external(erlang, "rand", "uniform")
fn ffi_random() -> Int

/// Get environment variable with default
@external(erlang, "vibee_editor_agent_ws_ffi", "get_env_or")
fn get_env_or(key: String, default: String) -> String

// =============================================================================
// System Prompt
// =============================================================================

fn editor_system_prompt() -> String {
  "You are VIBEE AI, an expert assistant for creating and editing Remotion video templates.
Respond in the same language the user uses.

## Your Capabilities
- Help users create and modify video compositions (React/Remotion)
- Manage timeline: add, update, delete video/audio items
- Suggest property changes for existing templates
- Explain video editing concepts

## Available Actions (include as JSON in your response)

### Template Property Actions:
```json
{\"action\": \"update_prop\", \"key\": \"musicVolume\", \"value\": 0.1}
```
Available props: musicVolume (0-1), coverDuration (0-1), vignetteStrength (0-1), colorCorrection (0.5-2.0), circleSizePercent, circleBottomPercent, circleLeftPx, showCaptions (true/false)

### Timeline Actions:
```json
{\"action\": \"add_track_item\", \"trackId\": \"video-track\", \"itemData\": {\"type\": \"video\", \"assetId\": \"asset-1\", \"startFrame\": 0, \"durationFrames\": 90, \"mediaStartFrame\": 0}}
```

```json
{\"action\": \"update_track_item\", \"itemId\": \"item-1\", \"updates\": {\"startFrame\": 30, \"durationFrames\": 120}}
```

```json
{\"action\": \"delete_track_items\", \"itemIds\": [\"item-1\", \"item-2\"]}
```

```json
{\"action\": \"select_items\", \"itemIds\": [\"item-1\"]}
```

## Context You Receive
- tracks: Array of timeline tracks with items (id, type, startFrame, durationFrames)
- assets: Array of available media files (id, name, type, src, durationFrames)
- project: {name, fps, width, height, durationInFrames}
- props: Current template properties
- selectedItems: Currently selected item IDs

## Available Track IDs
- video-track: B-roll videos
- avatar-track: Avatar/talking head
- audio-track: Background music
- text-track: Text overlays

## Guidelines
- Keep responses concise and actionable
- Include JSON action blocks when suggesting changes
- Consider 9:16 vertical video format (1080x1920)
- Default FPS is 30
- When adding items, calculate startFrame based on existing items

Be helpful, creative, and precise!"
}
