// OpenRouter Chat API Integration
// Universal gateway for ALL chat models: GPT, Claude, DeepSeek, Gemini, etc.
// API Documentation: https://openrouter.ai/docs

import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}

// ============================================================
// Types
// ============================================================

pub type Config {
  Config(api_key: String, site_url: Option(String), site_name: Option(String))
}

pub type Message {
  Message(role: String, content: String)
}

pub type ChatRequest {
  ChatRequest(
    model: String,
    messages: List(Message),
    temperature: Option(Float),
    max_tokens: Option(Int),
    top_p: Option(Float),
    frequency_penalty: Option(Float),
    presence_penalty: Option(Float),
    stop: Option(List(String)),
  )
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
// Available Models
// ============================================================

/// OpenRouter-compatible models
pub fn available_models() -> List(#(String, String)) {
  [
    // DeepSeek models
    #("deepseek/deepseek-chat", "DeepSeek Chat - Fast, cost-effective"),
    #("deepseek/deepseek-reasoner", "DeepSeek Reasoner - Advanced reasoning"),
    // OpenAI models
    #("openai/gpt-4o", "GPT-4o - Latest OpenAI flagship"),
    #("openai/gpt-4o-mini", "GPT-4o Mini - Fast, affordable"),
    #("openai/gpt-4-turbo", "GPT-4 Turbo - Previous gen flagship"),
    #("openai/gpt-3.5-turbo", "GPT-3.5 Turbo - Legacy, cheap"),
    #("openai/o1-mini", "O1 Mini - Reasoning model"),
    #("openai/o1-preview", "O1 Preview - Advanced reasoning"),
    // Anthropic Claude models
    #("anthropic/claude-3.5-sonnet", "Claude 3.5 Sonnet - Latest flagship"),
    #(
      "anthropic/claude-3.5-sonnet:beta",
      "Claude 3.5 Sonnet Beta - Extended thinking",
    ),
    #("anthropic/claude-3.5-haiku", "Claude 3.5 Haiku - Fast, affordable"),
    #("anthropic/claude-3-opus", "Claude 3 Opus - Most capable (legacy)"),
    #("anthropic/claude-3-sonnet", "Claude 3 Sonnet - Balanced (legacy)"),
    #("anthropic/claude-3-haiku", "Claude 3 Haiku - Fast (legacy)"),
    // Google Gemini models
    #("google/gemini-2.0-flash-exp:free", "Gemini 2.0 Flash FREE - Fast"),
    #("google/gemini-pro", "Gemini Pro - Balanced performance"),
    #("google/gemini-pro-1.5", "Gemini Pro 1.5 - Enhanced"),
    #(
      "google/gemini-pro-1.5-exp",
      "Gemini Pro 1.5 Exp - Latest experimental",
    ),
    // Meta Llama models
    #("meta-llama/llama-3.3-70b-instruct", "Llama 3.3 70B - Latest open"),
    #("meta-llama/llama-3.1-405b-instruct", "Llama 3.1 405B - Largest open"),
    #("meta-llama/llama-3.1-70b-instruct", "Llama 3.1 70B - Balanced open"),
    #("meta-llama/llama-3.1-8b-instruct", "Llama 3.1 8B - Fast open"),
    // Mistral models
    #("mistralai/mistral-large", "Mistral Large - Flagship"),
    #("mistralai/mistral-medium", "Mistral Medium - Balanced"),
    #("mistralai/mistral-small", "Mistral Small - Fast"),
    // Qwen models
    #("qwen/qwen-2.5-72b-instruct", "Qwen 2.5 72B - Chinese+English"),
    #("qwen/qwen-2-72b-instruct", "Qwen 2 72B - Multilingual"),
  ]
}

/// Model categories for easy selection
pub fn model_categories() -> List(#(String, List(String))) {
  [
    #("flagship", [
      "anthropic/claude-3.5-sonnet",
      "openai/gpt-4o",
      "deepseek/deepseek-reasoner",
      "google/gemini-pro-1.5-exp",
    ]),
    #("affordable", [
      "openai/gpt-4o-mini",
      "anthropic/claude-3.5-haiku",
      "deepseek/deepseek-chat",
      "google/gemini-2.0-flash-exp:free",
    ]),
    #("reasoning", [
      "deepseek/deepseek-reasoner",
      "openai/o1-preview",
      "openai/o1-mini",
      "anthropic/claude-3.5-sonnet:beta",
    ]),
    #("open-source", [
      "meta-llama/llama-3.3-70b-instruct",
      "meta-llama/llama-3.1-405b-instruct",
      "qwen/qwen-2.5-72b-instruct",
      "mistralai/mistral-large",
    ]),
  ]
}

// ============================================================
// Config Functions
// ============================================================

/// Create default config
pub fn default_config(api_key: String) -> Config {
  Config(api_key: api_key, site_url: None, site_name: None)
}

/// Create config with site info (for better rate limits)
pub fn config_with_site(
  api_key: String,
  site_url: String,
  site_name: String,
) -> Config {
  Config(
    api_key: api_key,
    site_url: Some(site_url),
    site_name: Some(site_name),
  )
}

// ============================================================
// Request Builders
// ============================================================

/// Create a chat completion request
pub fn create_chat_request(config: Config, req: ChatRequest) -> Request {
  let messages_json =
    json.array(req.messages, fn(msg) {
      json.object([
        #("role", json.string(msg.role)),
        #("content", json.string(msg.content)),
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

  let with_max_tokens = case req.max_tokens {
    Some(m) -> [#("max_tokens", json.int(m)), ..with_temp]
    None -> with_temp
  }

  let with_top_p = case req.top_p {
    Some(p) -> [#("top_p", json.float(p)), ..with_max_tokens]
    None -> with_max_tokens
  }

  let with_freq_penalty = case req.frequency_penalty {
    Some(f) -> [#("frequency_penalty", json.float(f)), ..with_top_p]
    None -> with_top_p
  }

  let with_pres_penalty = case req.presence_penalty {
    Some(p) -> [#("presence_penalty", json.float(p)), ..with_freq_penalty]
    None -> with_freq_penalty
  }

  let final_parts = case req.stop {
    Some(stop_sequences) -> {
      let stop_json = json.array(stop_sequences, fn(s) { json.string(s) })
      [#("stop", stop_json), ..with_pres_penalty]
    }
    None -> with_pres_penalty
  }

  let body = json.to_string(json.object(final_parts))

  let base_headers = [
    #("Authorization", "Bearer " <> config.api_key),
    #("Content-Type", "application/json"),
  ]

  let with_site_url = case config.site_url {
    Some(url) -> [#("HTTP-Referer", url), ..base_headers]
    None -> base_headers
  }

  let final_headers = case config.site_name {
    Some(name) -> [#("X-Title", name), ..with_site_url]
    None -> with_site_url
  }

  Request(
    url: "https://openrouter.ai/api/v1/chat/completions",
    method: "POST",
    headers: final_headers,
    body: body,
  )
}

/// Get list of available models from OpenRouter
pub fn list_models_request(config: Config) -> Request {
  Request(
    url: "https://openrouter.ai/api/v1/models",
    method: "GET",
    headers: [
      #("Authorization", "Bearer " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

/// Get user's generation stats
pub fn get_generation_stats_request(config: Config) -> Request {
  Request(
    url: "https://openrouter.ai/api/v1/generation",
    method: "GET",
    headers: [
      #("Authorization", "Bearer " <> config.api_key),
      #("Content-Type", "application/json"),
    ],
    body: "",
  )
}

// ============================================================
// Helper Functions
// ============================================================

/// Create simple chat request with minimal params
pub fn simple_chat(model: String, user_message: String) -> ChatRequest {
  ChatRequest(
    model: model,
    messages: [Message(role: "user", content: user_message)],
    temperature: None,
    max_tokens: None,
    top_p: None,
    frequency_penalty: None,
    presence_penalty: None,
    stop: None,
  )
}

/// Create chat request with system prompt
pub fn chat_with_system(
  model: String,
  system_prompt: String,
  user_message: String,
) -> ChatRequest {
  ChatRequest(
    model: model,
    messages: [
      Message(role: "system", content: system_prompt),
      Message(role: "user", content: user_message),
    ],
    temperature: None,
    max_tokens: None,
    top_p: None,
    frequency_penalty: None,
    presence_penalty: None,
    stop: None,
  )
}

/// Create chat request with conversation history
pub fn chat_with_history(
  model: String,
  messages: List(Message),
) -> ChatRequest {
  ChatRequest(
    model: model,
    messages: messages,
    temperature: None,
    max_tokens: None,
    top_p: None,
    frequency_penalty: None,
    presence_penalty: None,
    stop: None,
  )
}

/// Create chat with temperature control
pub fn chat_with_temperature(
  model: String,
  user_message: String,
  temperature: Float,
) -> ChatRequest {
  ChatRequest(
    model: model,
    messages: [Message(role: "user", content: user_message)],
    temperature: Some(temperature),
    max_tokens: None,
    top_p: None,
    frequency_penalty: None,
    presence_penalty: None,
    stop: None,
  )
}

/// Create full chat request with all parameters
pub fn chat_full(
  model: String,
  messages: List(Message),
  temperature: Float,
  max_tokens: Int,
  top_p: Float,
  frequency_penalty: Float,
  presence_penalty: Float,
  stop_sequences: List(String),
) -> ChatRequest {
  ChatRequest(
    model: model,
    messages: messages,
    temperature: Some(temperature),
    max_tokens: Some(max_tokens),
    top_p: Some(top_p),
    frequency_penalty: Some(frequency_penalty),
    presence_penalty: Some(presence_penalty),
    stop: Some(stop_sequences),
  )
}

/// Create message (helper)
pub fn message(role: String, content: String) -> Message {
  Message(role: role, content: content)
}

/// Create user message
pub fn user_message(content: String) -> Message {
  Message(role: "user", content: content)
}

/// Create assistant message
pub fn assistant_message(content: String) -> Message {
  Message(role: "assistant", content: content)
}

/// Create system message
pub fn system_message(content: String) -> Message {
  Message(role: "system", content: content)
}

// ============================================================
// Pricing Information (per 1M tokens)
// ============================================================

pub fn pricing() -> List(#(String, String, String)) {
  [
    // Model, Input price, Output price
    #("deepseek/deepseek-chat", "$0.14", "$0.28"),
    #("deepseek/deepseek-reasoner", "$0.55", "$2.19"),
    #("openai/gpt-4o", "$2.50", "$10.00"),
    #("openai/gpt-4o-mini", "$0.15", "$0.60"),
    #("anthropic/claude-3.5-sonnet", "$3.00", "$15.00"),
    #("anthropic/claude-3.5-haiku", "$0.80", "$4.00"),
    #("google/gemini-2.0-flash-exp:free", "$0.00", "$0.00"),
    #("meta-llama/llama-3.3-70b-instruct", "$0.35", "$0.40"),
    #("google/gemini-3-pro-image-preview", "$2.00", "$12.00"),
  ]
}

// ============================================================
// Multimodal Types (Vision/Image Analysis)
// ============================================================

/// Content part for multimodal messages
pub type ContentPart {
  /// Plain text content
  TextPart(text: String)
  /// Image from URL
  ImageUrlPart(url: String, detail: String)
  /// Image from base64 data
  ImageBase64Part(base64_data: String, media_type: String)
}

/// Multimodal message with mixed content (text + images)
pub type MultimodalMessage {
  MultimodalMessage(role: String, content: List(ContentPart))
}

/// Request for vision/image analysis
pub type VisionRequest {
  VisionRequest(
    model: String,
    messages: List(MultimodalMessage),
    max_tokens: Option(Int),
    temperature: Option(Float),
  )
}

// ============================================================
// Multimodal Request Builders
// ============================================================

/// Create a vision/image analysis request
pub fn create_vision_request(config: Config, req: VisionRequest) -> Request {
  let messages_json =
    json.array(req.messages, fn(msg) {
      let content_parts = json.array(msg.content, fn(part) {
        case part {
          TextPart(text) ->
            json.object([
              #("type", json.string("text")),
              #("text", json.string(text)),
            ])
          ImageUrlPart(url, detail) ->
            json.object([
              #("type", json.string("image_url")),
              #("image_url", json.object([
                #("url", json.string(url)),
                #("detail", json.string(detail)),
              ])),
            ])
          ImageBase64Part(data, media_type) ->
            json.object([
              #("type", json.string("image_url")),
              #("image_url", json.object([
                #("url", json.string("data:" <> media_type <> ";base64," <> data)),
              ])),
            ])
        }
      })
      json.object([
        #("role", json.string(msg.role)),
        #("content", content_parts),
      ])
    })

  let base_parts = [
    #("model", json.string(req.model)),
    #("messages", messages_json),
  ]

  let with_max_tokens = case req.max_tokens {
    Some(m) -> [#("max_tokens", json.int(m)), ..base_parts]
    None -> base_parts
  }

  let final_parts = case req.temperature {
    Some(t) -> [#("temperature", json.float(t)), ..with_max_tokens]
    None -> with_max_tokens
  }

  let body = json.to_string(json.object(final_parts))

  let base_headers = [
    #("Authorization", "Bearer " <> config.api_key),
    #("Content-Type", "application/json"),
  ]

  let with_site_url = case config.site_url {
    Some(url) -> [#("HTTP-Referer", url), ..base_headers]
    None -> base_headers
  }

  let final_headers = case config.site_name {
    Some(name) -> [#("X-Title", name), ..with_site_url]
    None -> with_site_url
  }

  Request(
    url: "https://openrouter.ai/api/v1/chat/completions",
    method: "POST",
    headers: final_headers,
    body: body,
  )
}

// ============================================================
// Multimodal Helper Functions
// ============================================================

/// Analyze image from URL with Gemini 3 Pro Image Preview
pub fn analyze_image_url(
  image_url: String,
  prompt: String,
) -> VisionRequest {
  VisionRequest(
    model: "google/gemini-3-pro-image-preview",
    messages: [
      MultimodalMessage(
        role: "user",
        content: [
          ImageUrlPart(url: image_url, detail: "high"),
          TextPart(text: prompt),
        ],
      ),
    ],
    max_tokens: Some(4096),
    temperature: Some(0.3),
  )
}

/// Analyze image from base64 data
pub fn analyze_image_base64(
  base64_data: String,
  media_type: String,
  prompt: String,
) -> VisionRequest {
  VisionRequest(
    model: "google/gemini-3-pro-image-preview",
    messages: [
      MultimodalMessage(
        role: "user",
        content: [
          ImageBase64Part(base64_data: base64_data, media_type: media_type),
          TextPart(text: prompt),
        ],
      ),
    ],
    max_tokens: Some(4096),
    temperature: Some(0.3),
  )
}

/// Analyze multiple images at once
pub fn analyze_multiple_images(
  image_urls: List(String),
  prompt: String,
) -> VisionRequest {
  let image_parts = list.map(image_urls, fn(url) {
    ImageUrlPart(url: url, detail: "high")
  })
  let all_parts = list.append(image_parts, [TextPart(text: prompt)])

  VisionRequest(
    model: "google/gemini-3-pro-image-preview",
    messages: [
      MultimodalMessage(role: "user", content: all_parts),
    ],
    max_tokens: Some(4096),
    temperature: Some(0.3),
  )
}

/// Create text content part
pub fn text_part(text: String) -> ContentPart {
  TextPart(text: text)
}

/// Create image URL content part
pub fn image_url_part(url: String) -> ContentPart {
  ImageUrlPart(url: url, detail: "high")
}

/// Create image base64 content part
pub fn image_base64_part(data: String, media_type: String) -> ContentPart {
  ImageBase64Part(base64_data: data, media_type: media_type)
}

/// Vision models that support image input
pub fn vision_models() -> List(#(String, String)) {
  [
    #("google/gemini-3-pro-image-preview", "Gemini 3 Pro - Best multimodal, $2/$12 per 1M"),
    #("google/gemini-2.5-pro-preview", "Gemini 2.5 Pro - Video + Image understanding"),
    #("openai/gpt-4o", "GPT-4o - Vision + Text, $2.50/$10 per 1M"),
    #("openai/gpt-4o-mini", "GPT-4o Mini - Fast vision, $0.15/$0.60 per 1M"),
    #("anthropic/claude-3.5-sonnet", "Claude 3.5 Sonnet - Vision, $3/$15 per 1M"),
    #("anthropic/claude-3-opus", "Claude 3 Opus - Best quality vision"),
  ]
}
