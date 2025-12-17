// MCP Protocol Types - JSON-RPC 2.0 based
// https://modelcontextprotocol.io/specification

import gleam/json
import gleam/option.{type Option, None, Some}

/// JSON-RPC Request
pub type JsonRpcRequest {
  JsonRpcRequest(
    jsonrpc: String,
    id: Option(Int),
    method: String,
    params: Option(json.Json),
  )
}

/// JSON-RPC Response
pub type JsonRpcResponse {
  JsonRpcResponse(
    jsonrpc: String,
    id: Option(Int),
    result: Option(json.Json),
    error: Option(JsonRpcError),
  )
}

/// JSON-RPC Error
pub type JsonRpcError {
  JsonRpcError(
    code: Int,
    message: String,
    data: Option(json.Json),
  )
}

/// MCP Tool definition
pub type Tool {
  Tool(
    name: String,
    description: String,
    input_schema: json.Json,
  )
}

/// MCP Tool call
pub type ToolCall {
  ToolCall(
    name: String,
    arguments: json.Json,
  )
}

/// MCP Tool result
pub type ToolResult {
  ToolResult(
    content: List(ContentBlock),
    is_error: Bool,
  )
}

/// Content block types
pub type ContentBlock {
  TextContent(text: String)
  ImageContent(data: String, mime_type: String)
  ResourceContent(uri: String, mime_type: String, text: Option(String))
}

/// MCP Resource
pub type Resource {
  Resource(
    uri: String,
    name: String,
    description: Option(String),
    mime_type: Option(String),
  )
}

/// MCP Prompt
pub type Prompt {
  Prompt(
    name: String,
    description: Option(String),
    arguments: List(PromptArgument),
  )
}

pub type PromptArgument {
  PromptArgument(
    name: String,
    description: Option(String),
    required: Bool,
  )
}

/// Server capabilities
pub type ServerCapabilities {
  ServerCapabilities(
    tools: Bool,
    resources: Bool,
    prompts: Bool,
    logging: Bool,
  )
}

/// Server info
pub type ServerInfo {
  ServerInfo(
    name: String,
    version: String,
    capabilities: ServerCapabilities,
  )
}

// Error codes
pub const parse_error = -32700
pub const invalid_request = -32600
pub const method_not_found = -32601
pub const invalid_params = -32602
pub const internal_error = -32603
