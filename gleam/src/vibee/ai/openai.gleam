// OpenAI API Integration
// API Documentation: https://platform.openai.com/docs/api-reference

import gleam/json
import gleam/option.{type Option, None, Some}

// ============================================================
// Types
// ============================================================

pub type Config {
  Config(api_key: String, organization: Option(String))
}

pub type ChatMessage {
  ChatMessage(role: String, content: String)
}

pub type ChatRequest {
  ChatRequest(
    model: String,
    messages: List(ChatMessage),
    temperature: Option(Float),
    max_tokens: Option(Int),
  )
}

pub type EmbeddingRequest {
  EmbeddingRequest(model: String, input: String)
}

pub type Request {
  Request(
    url: String,
    method: String,
    headers: List(#(String, String)),
    body: String,
  )
}

// ============================================================
// Request Builders
// ============================================================

/// Create a chat completion request
pub fn create_chat_request(config: Config, req: ChatRequest) -> Request {
  let messages_json =
    json.array(req.messages, fn(m) {
      json.object([
        #("role", json.string(m.role)),
        #("content", json.string(m.content)),
      ])
    })

  let base_parts = [
    #("model", json.string(req.model)),
    #("messages", messages_json),
  ]

  let with_temp = case req.temperature {
    Some(t) -> [#("temperature", json.float(t)), ..base_parts]
    None -> base_parts
  }

  let with_tokens = case req.max_tokens {
    Some(t) -> [#("max_tokens", json.int(t)), ..with_temp]
    None -> with_temp
  }

  let body = json.to_string(json.object(with_tokens))

  let headers = [
    #("Authorization", "Bearer " <> config.api_key),
    #("Content-Type", "application/json"),
  ]

  let headers_with_org = case config.organization {
    Some(org) -> [#("OpenAI-Organization", org), ..headers]
    None -> headers
  }

  Request(
    url: "https://api.openai.com/v1/chat/completions",
    method: "POST",
    headers: headers_with_org,
    body: body,
  )
}

/// Create an embedding request
pub fn create_embedding_request(config: Config, req: EmbeddingRequest) -> Request {
  let body =
    json.to_string(json.object([
      #("model", json.string(req.model)),
      #("input", json.string(req.input)),
    ]))

  Request(
    url: "https://api.openai.com/v1/embeddings",
    method: "POST",
    headers: [
      #("Authorization", "Bearer " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create a request to list models
pub fn list_models_request(config: Config) -> Request {
  Request(
    url: "https://api.openai.com/v1/models",
    method: "GET",
    headers: [
      #("Authorization", "Bearer " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

// ============================================================
// Available Models
// ============================================================

pub fn chat_models() -> List(#(String, String)) {
  [
    #("gpt-4o", "GPT-4o - Most capable, multimodal"),
    #("gpt-4o-mini", "GPT-4o Mini - Faster, cheaper"),
    #("gpt-4-turbo", "GPT-4 Turbo - Fast, capable"),
    #("gpt-4", "GPT-4 - Original"),
    #("gpt-3.5-turbo", "GPT-3.5 Turbo - Fast, cheap"),
  ]
}

pub fn embedding_models() -> List(#(String, String)) {
  [
    #("text-embedding-3-large", "1536 dimensions, best quality"),
    #("text-embedding-3-small", "1536 dimensions, fast"),
    #("text-embedding-ada-002", "Legacy, 1536 dimensions"),
  ]
}

// ============================================================
// Helper Functions
// ============================================================

/// Create a simple chat request with user message
pub fn simple_chat(prompt: String) -> ChatRequest {
  ChatRequest(
    model: "gpt-4o-mini",
    messages: [ChatMessage(role: "user", content: prompt)],
    temperature: None,
    max_tokens: None,
  )
}

/// Create a chat request with system prompt
pub fn chat_with_system(system: String, user: String) -> ChatRequest {
  ChatRequest(
    model: "gpt-4o-mini",
    messages: [
      ChatMessage(role: "system", content: system),
      ChatMessage(role: "user", content: user),
    ],
    temperature: None,
    max_tokens: None,
  )
}

/// Create a simple embedding request
pub fn simple_embedding(text: String) -> EmbeddingRequest {
  EmbeddingRequest(model: "text-embedding-3-small", input: text)
}

/// Create default config with API key
pub fn default_config(api_key: String) -> Config {
  Config(api_key: api_key, organization: None)
}

// ============================================================
// Function Calling Types
// ============================================================

/// A function definition for tool use
pub type FunctionDef {
  FunctionDef(
    name: String,
    description: String,
    parameters: json.Json,
  )
}

/// A tool definition (function or other)
pub type Tool {
  FunctionTool(function: FunctionDef)
}

/// Tool choice options
pub type ToolChoice {
  ToolChoiceAuto
  ToolChoiceNone
  ToolChoiceRequired
  ToolChoiceSpecific(name: String)
}

/// Chat message with optional tool calls support
pub type ChatMessageFull {
  UserMessage(content: String)
  SystemMessage(content: String)
  AssistantMessage(content: String, tool_calls: Option(List(ToolCall)))
  ToolMessage(tool_call_id: String, content: String)
}

/// A tool call from the assistant
pub type ToolCall {
  ToolCall(id: String, function_name: String, arguments: String)
}

/// Chat request with tools support
pub type ChatRequestWithTools {
  ChatRequestWithTools(
    model: String,
    messages: List(ChatMessageFull),
    tools: Option(List(Tool)),
    tool_choice: Option(ToolChoice),
    temperature: Option(Float),
    max_tokens: Option(Int),
    response_format: Option(ResponseFormat),
  )
}

/// Response format options
pub type ResponseFormat {
  ResponseFormatText
  ResponseFormatJsonObject
  ResponseFormatJsonSchema(name: String, schema: json.Json)
}

// ============================================================
// Function Calling Request Builders
// ============================================================

/// Create a chat request with function calling support
pub fn create_chat_with_tools_request(
  config: Config,
  req: ChatRequestWithTools,
) -> Request {
  let messages_json = json.array(req.messages, encode_message_full)

  let base_parts = [
    #("model", json.string(req.model)),
    #("messages", messages_json),
  ]

  // Add tools if present
  let with_tools = case req.tools {
    Some(tools) -> [
      #("tools", json.array(tools, encode_tool)),
      ..base_parts
    ]
    None -> base_parts
  }

  // Add tool_choice if present
  let with_tool_choice = case req.tool_choice {
    Some(choice) -> [
      #("tool_choice", encode_tool_choice(choice)),
      ..with_tools
    ]
    None -> with_tools
  }

  // Add temperature if present
  let with_temp = case req.temperature {
    Some(t) -> [#("temperature", json.float(t)), ..with_tool_choice]
    None -> with_tool_choice
  }

  // Add max_tokens if present
  let with_tokens = case req.max_tokens {
    Some(t) -> [#("max_tokens", json.int(t)), ..with_temp]
    None -> with_temp
  }

  // Add response_format if present
  let final_parts = case req.response_format {
    Some(fmt) -> [#("response_format", encode_response_format(fmt)), ..with_tokens]
    None -> with_tokens
  }

  let body = json.to_string(json.object(final_parts))

  let headers = [
    #("Authorization", "Bearer " <> config.api_key),
    #("Content-Type", "application/json"),
  ]

  let headers_with_org = case config.organization {
    Some(org) -> [#("OpenAI-Organization", org), ..headers]
    None -> headers
  }

  Request(
    url: "https://api.openai.com/v1/chat/completions",
    method: "POST",
    headers: headers_with_org,
    body: body,
  )
}

/// Encode a full message to JSON
fn encode_message_full(msg: ChatMessageFull) -> json.Json {
  case msg {
    UserMessage(content) ->
      json.object([
        #("role", json.string("user")),
        #("content", json.string(content)),
      ])
    SystemMessage(content) ->
      json.object([
        #("role", json.string("system")),
        #("content", json.string(content)),
      ])
    AssistantMessage(content, tool_calls) -> {
      let base = [
        #("role", json.string("assistant")),
        #("content", json.string(content)),
      ]
      case tool_calls {
        Some(calls) -> json.object([
          #("tool_calls", json.array(calls, encode_tool_call)),
          ..base
        ])
        None -> json.object(base)
      }
    }
    ToolMessage(tool_call_id, content) ->
      json.object([
        #("role", json.string("tool")),
        #("tool_call_id", json.string(tool_call_id)),
        #("content", json.string(content)),
      ])
  }
}

/// Encode a tool call to JSON
fn encode_tool_call(call: ToolCall) -> json.Json {
  json.object([
    #("id", json.string(call.id)),
    #("type", json.string("function")),
    #("function", json.object([
      #("name", json.string(call.function_name)),
      #("arguments", json.string(call.arguments)),
    ])),
  ])
}

/// Encode a tool to JSON
fn encode_tool(tool: Tool) -> json.Json {
  case tool {
    FunctionTool(func) ->
      json.object([
        #("type", json.string("function")),
        #("function", json.object([
          #("name", json.string(func.name)),
          #("description", json.string(func.description)),
          #("parameters", func.parameters),
        ])),
      ])
  }
}

/// Encode tool choice to JSON
fn encode_tool_choice(choice: ToolChoice) -> json.Json {
  case choice {
    ToolChoiceAuto -> json.string("auto")
    ToolChoiceNone -> json.string("none")
    ToolChoiceRequired -> json.string("required")
    ToolChoiceSpecific(name) ->
      json.object([
        #("type", json.string("function")),
        #("function", json.object([
          #("name", json.string(name)),
        ])),
      ])
  }
}

/// Encode response format to JSON
fn encode_response_format(fmt: ResponseFormat) -> json.Json {
  case fmt {
    ResponseFormatText ->
      json.object([#("type", json.string("text"))])
    ResponseFormatJsonObject ->
      json.object([#("type", json.string("json_object"))])
    ResponseFormatJsonSchema(name, schema) ->
      json.object([
        #("type", json.string("json_schema")),
        #("json_schema", json.object([
          #("name", json.string(name)),
          #("schema", schema),
        ])),
      ])
  }
}

// ============================================================
// Function Calling Helpers
// ============================================================

/// Create a simple function tool
pub fn function_tool(
  name: String,
  description: String,
  parameters: json.Json,
) -> Tool {
  FunctionTool(FunctionDef(
    name: name,
    description: description,
    parameters: parameters,
  ))
}

/// Create a chat request with a single function
pub fn chat_with_function(
  model: String,
  messages: List(ChatMessageFull),
  function: Tool,
) -> ChatRequestWithTools {
  ChatRequestWithTools(
    model: model,
    messages: messages,
    tools: Some([function]),
    tool_choice: Some(ToolChoiceAuto),
    temperature: None,
    max_tokens: None,
    response_format: None,
  )
}

/// Create a simple user message
pub fn user_msg(content: String) -> ChatMessageFull {
  UserMessage(content)
}

/// Create a simple system message
pub fn system_msg(content: String) -> ChatMessageFull {
  SystemMessage(content)
}

/// Create a simple assistant message
pub fn assistant_msg(content: String) -> ChatMessageFull {
  AssistantMessage(content, None)
}

/// Create a tool response message
pub fn tool_msg(tool_call_id: String, content: String) -> ChatMessageFull {
  ToolMessage(tool_call_id, content)
}

/// Create JSON schema for function parameters
pub fn params_schema(properties: List(#(String, json.Json)), required: List(String)) -> json.Json {
  json.object([
    #("type", json.string("object")),
    #("properties", json.object(properties)),
    #("required", json.array(required, json.string)),
  ])
}

/// Create a string property for schema
pub fn string_prop(description: String) -> json.Json {
  json.object([
    #("type", json.string("string")),
    #("description", json.string(description)),
  ])
}

/// Create an integer property for schema
pub fn int_prop(description: String) -> json.Json {
  json.object([
    #("type", json.string("integer")),
    #("description", json.string(description)),
  ])
}

/// Create a number property for schema
pub fn number_prop(description: String) -> json.Json {
  json.object([
    #("type", json.string("number")),
    #("description", json.string(description)),
  ])
}

/// Create a boolean property for schema
pub fn bool_prop(description: String) -> json.Json {
  json.object([
    #("type", json.string("boolean")),
    #("description", json.string(description)),
  ])
}

/// Create an enum property for schema
pub fn enum_prop(description: String, values: List(String)) -> json.Json {
  json.object([
    #("type", json.string("string")),
    #("description", json.string(description)),
    #("enum", json.array(values, json.string)),
  ])
}

/// Create an array property for schema
pub fn array_prop(description: String, item_type: json.Json) -> json.Json {
  json.object([
    #("type", json.string("array")),
    #("description", json.string(description)),
    #("items", item_type),
  ])
}

// ============================================================
// Vision Support
// ============================================================

/// Image content part for vision messages
pub type ImageContent {
  ImageUrl(url: String, detail: Option(String))
  ImageBase64(media_type: String, data: String, detail: Option(String))
}

/// Content part for multimodal messages
pub type ContentPart {
  TextPart(text: String)
  ImagePart(image: ImageContent)
}

/// Vision message
pub type VisionMessage {
  VisionUserMessage(content: List(ContentPart))
  VisionSystemMessage(content: String)
  VisionAssistantMessage(content: String)
}

/// Create a vision chat request
pub fn create_vision_request(
  config: Config,
  model: String,
  messages: List(VisionMessage),
  max_tokens: Option(Int),
) -> Request {
  let messages_json = json.array(messages, encode_vision_message)

  let base_parts = [
    #("model", json.string(model)),
    #("messages", messages_json),
  ]

  let final_parts = case max_tokens {
    Some(t) -> [#("max_tokens", json.int(t)), ..base_parts]
    None -> base_parts
  }

  let body = json.to_string(json.object(final_parts))

  Request(
    url: "https://api.openai.com/v1/chat/completions",
    method: "POST",
    headers: [
      #("Authorization", "Bearer " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Encode vision message to JSON
fn encode_vision_message(msg: VisionMessage) -> json.Json {
  case msg {
    VisionUserMessage(content) ->
      json.object([
        #("role", json.string("user")),
        #("content", json.array(content, encode_content_part)),
      ])
    VisionSystemMessage(content) ->
      json.object([
        #("role", json.string("system")),
        #("content", json.string(content)),
      ])
    VisionAssistantMessage(content) ->
      json.object([
        #("role", json.string("assistant")),
        #("content", json.string(content)),
      ])
  }
}

/// Encode content part to JSON
fn encode_content_part(part: ContentPart) -> json.Json {
  case part {
    TextPart(text) ->
      json.object([
        #("type", json.string("text")),
        #("text", json.string(text)),
      ])
    ImagePart(ImageUrl(url, detail)) -> {
      let url_obj = case detail {
        Some(d) -> json.object([
          #("url", json.string(url)),
          #("detail", json.string(d)),
        ])
        None -> json.object([#("url", json.string(url))])
      }
      json.object([
        #("type", json.string("image_url")),
        #("image_url", url_obj),
      ])
    }
    ImagePart(ImageBase64(media_type, data, detail)) -> {
      let data_url = "data:" <> media_type <> ";base64," <> data
      let url_obj = case detail {
        Some(d) -> json.object([
          #("url", json.string(data_url)),
          #("detail", json.string(d)),
        ])
        None -> json.object([#("url", json.string(data_url))])
      }
      json.object([
        #("type", json.string("image_url")),
        #("image_url", url_obj),
      ])
    }
  }
}

/// Create text content part
pub fn text_part(text: String) -> ContentPart {
  TextPart(text)
}

/// Create image URL content part
pub fn image_url_part(url: String) -> ContentPart {
  ImagePart(ImageUrl(url, None))
}

/// Create image URL content part with detail level
pub fn image_url_part_detailed(url: String, detail: String) -> ContentPart {
  ImagePart(ImageUrl(url, Some(detail)))
}

/// Create base64 image content part
pub fn image_base64_part(media_type: String, data: String) -> ContentPart {
  ImagePart(ImageBase64(media_type, data, None))
}

// ============================================================
// Audio Support (Whisper)
// ============================================================

/// Transcription request for Whisper API
pub type TranscriptionRequest {
  TranscriptionRequest(
    model: String,
    language: Option(String),
    prompt: Option(String),
    response_format: Option(String),
    temperature: Option(Float),
    timestamp_granularities: Option(List(String)),
  )
}

/// Create a transcription request (returns metadata, actual audio upload needs form-data)
pub fn create_transcription_request(
  config: Config,
  req: TranscriptionRequest,
) -> Request {
  let base_parts = [#("model", json.string(req.model))]

  let with_language = case req.language {
    Some(l) -> [#("language", json.string(l)), ..base_parts]
    None -> base_parts
  }

  let with_prompt = case req.prompt {
    Some(p) -> [#("prompt", json.string(p)), ..with_language]
    None -> with_language
  }

  let with_format = case req.response_format {
    Some(f) -> [#("response_format", json.string(f)), ..with_prompt]
    None -> with_prompt
  }

  let with_temp = case req.temperature {
    Some(t) -> [#("temperature", json.float(t)), ..with_format]
    None -> with_format
  }

  let final_parts = case req.timestamp_granularities {
    Some(granularities) -> [
      #("timestamp_granularities", json.array(granularities, json.string)),
      ..with_temp
    ]
    None -> with_temp
  }

  let body = json.to_string(json.object(final_parts))

  Request(
    url: "https://api.openai.com/v1/audio/transcriptions",
    method: "POST",
    headers: [
      #("Authorization", "Bearer " <> config.api_key),
      // Note: Actual API requires multipart/form-data with file upload
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create a simple transcription request
pub fn simple_transcription() -> TranscriptionRequest {
  TranscriptionRequest(
    model: "whisper-1",
    language: None,
    prompt: None,
    response_format: None,
    temperature: None,
    timestamp_granularities: None,
  )
}

/// Whisper models
pub fn whisper_models() -> List(#(String, String)) {
  [#("whisper-1", "Whisper v2 - Speech to text")]
}

/// Supported response formats for transcription
pub fn transcription_formats() -> List(String) {
  ["json", "text", "srt", "verbose_json", "vtt"]
}

// ============================================================
// Improve Prompt Support
// ============================================================

/// Create a request to improve/enhance a prompt for AI generation
pub fn create_improve_prompt_request(
  config: Config,
  original_prompt: String,
  style: Option(String),
) -> Request {
  let style_context = case style {
    Some("image") ->
      "You are an expert at writing prompts for AI image generation (Midjourney, DALL-E, Stable Diffusion, FLUX). "
    Some("video") ->
      "You are an expert at writing prompts for AI video generation (Sora, Runway, Kling, Veo). "
    Some("text") -> "You are an expert at writing clear, detailed prompts for LLMs. "
    _ ->
      "You are an expert at writing prompts for AI image and video generation. "
  }

  let system_content =
    style_context
    <> "Improve the given prompt to be more detailed, vivid, and effective. "
    <> "Add artistic style, lighting, mood, composition details where appropriate. "
    <> "Keep the core idea but make it more professional and specific. "
    <> "Return ONLY the improved prompt text, no explanations or formatting."

  let messages_json =
    json.array(
      [
        json.object([
          #("role", json.string("system")),
          #("content", json.string(system_content)),
        ]),
        json.object([
          #("role", json.string("user")),
          #("content", json.string("Improve this prompt: " <> original_prompt)),
        ]),
      ],
      fn(x) { x },
    )

  let body_parts = [
    #("model", json.string("gpt-4o-mini")),
    #("messages", messages_json),
    #("max_tokens", json.int(500)),
    #("temperature", json.float(0.7)),
  ]

  let body = json.to_string(json.object(body_parts))

  Request(
    url: "https://api.openai.com/v1/chat/completions",
    method: "POST",
    headers: [
      #("Authorization", "Bearer " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}

/// Create a request to describe an image (image-to-prompt)
pub fn create_image_to_prompt_request(
  config: Config,
  image_url: String,
) -> Request {
  let prompt_text =
    "Analyze this image and write a detailed prompt that could be used to recreate it with an AI image generator. "
    <> "Include: subject description, art style, lighting, colors, mood, composition, camera angle. "
    <> "Format: A single paragraph prompt suitable for Midjourney, DALL-E, or FLUX."

  let user_content =
    json.array(
      [
        json.object([
          #("type", json.string("text")),
          #("text", json.string(prompt_text)),
        ]),
        json.object([
          #("type", json.string("image_url")),
          #(
            "image_url",
            json.object([#("url", json.string(image_url))]),
          ),
        ]),
      ],
      fn(x) { x },
    )

  let messages_json =
    json.array(
      [
        json.object([
          #("role", json.string("user")),
          #("content", user_content),
        ]),
      ],
      fn(x) { x },
    )

  let body_parts = [
    #("model", json.string("gpt-4o")),
    #("messages", messages_json),
    #("max_tokens", json.int(500)),
  ]

  let body = json.to_string(json.object(body_parts))

  Request(
    url: "https://api.openai.com/v1/chat/completions",
    method: "POST",
    headers: [
      #("Authorization", "Bearer " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
  )
}
