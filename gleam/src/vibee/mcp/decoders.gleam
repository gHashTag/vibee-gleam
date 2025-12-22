// Type-safe JSON decoders for MCP tool arguments
// Uses gleam/dynamic/decode for proper JSON parsing

import gleam/dynamic/decode.{type Decoder}
import gleam/dynamic
import gleam/io
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/mcp/tool_args.{
  type TelegramGetDialogsArgs, TelegramGetDialogsArgs,
  type TelegramGetHistoryArgs, TelegramGetHistoryArgs,
  type TelegramSendMessageArgs, TelegramSendMessageArgs,
  type TelegramSendButtonsArgs, TelegramSendButtonsArgs,
  type TelegramSendPhotoArgs, TelegramSendPhotoArgs,
  type TelegramDownloadMediaArgs, TelegramDownloadMediaArgs,
  type TelegramGetMeArgs, TelegramGetMeArgs,
  type TelegramGetUserPhotoArgs, TelegramGetUserPhotoArgs,
  type TelegramSubscribeUpdatesArgs, TelegramSubscribeUpdatesArgs,
  type ButtonDef, ButtonDef,
  type AuthStatusArgs, AuthStatusArgs,
  type AuthSendCodeArgs, AuthSendCodeArgs,
  type AuthVerifyCodeArgs, AuthVerifyCodeArgs,
  type AuthLogoutArgs, AuthLogoutArgs,
  type KnowledgeSearchArgs, KnowledgeSearchArgs,
  type KnowledgeEmbedArgs, KnowledgeEmbedArgs,
  type FileReadArgs, FileReadArgs,
  type FileWriteArgs, FileWriteArgs,
  type FileListArgs, FileListArgs,
  type VoiceTranscribeArgs, VoiceTranscribeArgs,
  type SystemLogArgs, SystemLogArgs,
  type SystemExecArgs, SystemExecArgs,
  type EventEmitArgs, EventEmitArgs,
  type EventListArgs, EventListArgs,
  type DebugBuildArgs, DebugBuildArgs,
  type DebugTestArgs, DebugTestArgs,
  type DebugAnalyzeArgs, DebugAnalyzeArgs,
  type DebugTraceArgs, DebugTraceArgs,
  type DebugLogArgs, DebugLogArgs,
  type CodeGenerateArgs, CodeGenerateArgs,
  type CodeRefactorArgs, CodeRefactorArgs,
  type CodeExplainArgs, CodeExplainArgs,
  type CodeFindSimilarArgs, CodeFindSimilarArgs,
  type CodeDiffArgs, CodeDiffArgs,
  type TestRunArgs, TestRunArgs,
  type TestCreateArgs, TestCreateArgs,
  type TestCoverageArgs, TestCoverageArgs,
  type TestValidateArgs, TestValidateArgs,
  type AgentSpawnArgs, AgentSpawnArgs,
  type AgentMessageArgs, AgentMessageArgs,
  type AgentStatusArgs, AgentStatusArgs,
  type AgentKillArgs, AgentKillArgs,
  type BotAnalyzeArgs, BotAnalyzeArgs,
  type BotCompareArgs, BotCompareArgs,
  type BotMonitorArgs, BotMonitorArgs,
  type BotExtractCommandsArgs, BotExtractCommandsArgs,
  type BotTestInteractionArgs, BotTestInteractionArgs,
  type InteractionDef, InteractionDef,
  type RainbowDebugCycleArgs, RainbowDebugCycleArgs,
  type TaskCreateArgs, TaskCreateArgs,
  type TaskGetArgs, TaskGetArgs,
  type TaskListArgs, TaskListArgs,
  type TaskUpdateArgs, TaskUpdateArgs,
  type HealStartArgs, HealStartArgs,
  type HealApplyFixArgs, HealApplyFixArgs,
  type HealVerifyArgs, HealVerifyArgs,
  type HealRollbackArgs, HealRollbackArgs,
  type DecideNextStepArgs, DecideNextStepArgs,
  type DecideApplyArgs, DecideApplyArgs,
}

/// Decode error type
pub type DecodeError {
  MissingField(field: String)
  InvalidType(field: String, expected: String)
  ParseError(message: String)
}

/// Convert decode error to string
pub fn error_to_string(err: DecodeError) -> String {
  case err {
    MissingField(f) -> "Missing required field: " <> f
    InvalidType(f, e) -> "Invalid type for field '" <> f <> "', expected: " <> e
    ParseError(m) -> "Parse error: " <> m
  }
}

// ============================================================
// Telegram Decoders
// ============================================================

fn telegram_get_dialogs_decoder() -> Decoder(TelegramGetDialogsArgs) {
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  use limit <- decode.optional_field("limit", None, decode.optional(decode.int))
  use type_filter <- decode.optional_field("type_filter", None, decode.optional(decode.string))
  decode.success(TelegramGetDialogsArgs(
    session_id: session_id,
    limit: limit,
    type_filter: type_filter,
  ))
}

pub fn decode_telegram_get_dialogs(args: json.Json) -> Result(TelegramGetDialogsArgs, DecodeError) {
  run_decoder(args, telegram_get_dialogs_decoder(), "telegram_get_dialogs")
}

fn telegram_get_history_decoder() -> Decoder(TelegramGetHistoryArgs) {
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  use chat_id <- decode.field("chat_id", decode.string)
  use limit <- decode.optional_field("limit", None, decode.optional(decode.int))
  use offset_id <- decode.optional_field("offset_id", None, decode.optional(decode.int))
  decode.success(TelegramGetHistoryArgs(
    session_id: session_id,
    chat_id: chat_id,
    limit: limit,
    offset_id: offset_id,
  ))
}

pub fn decode_telegram_get_history(args: json.Json) -> Result(TelegramGetHistoryArgs, DecodeError) {
  run_decoder(args, telegram_get_history_decoder(), "telegram_get_history")
}

fn telegram_send_message_decoder() -> Decoder(TelegramSendMessageArgs) {
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  use chat_id <- decode.field("chat_id", decode.string)
  use text <- decode.field("text", decode.string)
  use reply_to <- decode.optional_field("reply_to", None, decode.optional(decode.int))
  decode.success(TelegramSendMessageArgs(
    session_id: session_id,
    chat_id: chat_id,
    text: text,
    reply_to: reply_to,
  ))
}

pub fn decode_telegram_send_message(args: json.Json) -> Result(TelegramSendMessageArgs, DecodeError) {
  run_decoder(args, telegram_send_message_decoder(), "telegram_send_message")
}

fn button_decoder() -> Decoder(ButtonDef) {
  use text <- decode.field("text", decode.string)
  use callback_data <- decode.optional_field("callback_data", None, decode.optional(decode.string))
  use url <- decode.optional_field("url", None, decode.optional(decode.string))
  decode.success(ButtonDef(
    text: text,
    callback_data: callback_data,
    url: url,
  ))
}

fn telegram_send_buttons_decoder() -> Decoder(TelegramSendButtonsArgs) {
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  use chat_id <- decode.field("chat_id", decode.string)
  use text <- decode.field("text", decode.string)
  use buttons <- decode.field("buttons", decode.list(decode.list(button_decoder())))
  decode.success(TelegramSendButtonsArgs(
    session_id: session_id,
    chat_id: chat_id,
    text: text,
    buttons: buttons,
  ))
}

pub fn decode_telegram_send_buttons(args: json.Json) -> Result(TelegramSendButtonsArgs, DecodeError) {
  run_decoder(args, telegram_send_buttons_decoder(), "telegram_send_buttons")
}

fn telegram_send_photo_decoder() -> Decoder(TelegramSendPhotoArgs) {
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  use chat_id <- decode.field("chat_id", decode.string)
  use file_path <- decode.field("file_path", decode.string)
  use caption <- decode.optional_field("caption", None, decode.optional(decode.string))
  decode.success(TelegramSendPhotoArgs(
    session_id: session_id,
    chat_id: chat_id,
    file_path: file_path,
    caption: caption,
  ))
}

pub fn decode_telegram_send_photo(args: json.Json) -> Result(TelegramSendPhotoArgs, DecodeError) {
  run_decoder(args, telegram_send_photo_decoder(), "telegram_send_photo")
}

fn telegram_download_media_decoder() -> Decoder(TelegramDownloadMediaArgs) {
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  use chat_id <- decode.field("chat_id", decode.string)
  use message_id <- decode.field("message_id", decode.int)
  use output_path <- decode.optional_field("output_path", None, decode.optional(decode.string))
  decode.success(TelegramDownloadMediaArgs(
    session_id: session_id,
    chat_id: chat_id,
    message_id: message_id,
    output_path: output_path,
  ))
}

pub fn decode_telegram_download_media(args: json.Json) -> Result(TelegramDownloadMediaArgs, DecodeError) {
  run_decoder(args, telegram_download_media_decoder(), "telegram_download_media")
}

fn telegram_get_me_decoder() -> Decoder(TelegramGetMeArgs) {
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  decode.success(TelegramGetMeArgs(session_id: session_id))
}

pub fn decode_telegram_get_me(args: json.Json) -> Result(TelegramGetMeArgs, DecodeError) {
  run_decoder(args, telegram_get_me_decoder(), "telegram_get_me")
}

fn telegram_get_user_photo_decoder() -> Decoder(TelegramGetUserPhotoArgs) {
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  use user_id <- decode.field("user_id", decode.int)
  decode.success(TelegramGetUserPhotoArgs(session_id: session_id, user_id: user_id))
}

pub fn decode_telegram_get_user_photo(args: json.Json) -> Result(TelegramGetUserPhotoArgs, DecodeError) {
  run_decoder(args, telegram_get_user_photo_decoder(), "telegram_get_user_photo")
}

fn telegram_subscribe_updates_decoder() -> Decoder(TelegramSubscribeUpdatesArgs) {
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  use event_types <- decode.optional_field("event_types", None, decode.optional(decode.list(decode.string)))
  decode.success(TelegramSubscribeUpdatesArgs(
    session_id: session_id,
    event_types: event_types,
  ))
}

pub fn decode_telegram_subscribe_updates(args: json.Json) -> Result(TelegramSubscribeUpdatesArgs, DecodeError) {
  run_decoder(args, telegram_subscribe_updates_decoder(), "telegram_subscribe_updates")
}

// ============================================================
// Auth Decoders
// ============================================================

fn auth_status_decoder() -> Decoder(AuthStatusArgs) {
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  decode.success(AuthStatusArgs(session_id: session_id))
}

pub fn decode_auth_status(args: json.Json) -> Result(AuthStatusArgs, DecodeError) {
  run_decoder(args, auth_status_decoder(), "auth_status")
}

fn auth_send_code_decoder() -> Decoder(AuthSendCodeArgs) {
  use phone <- decode.field("phone", decode.string)
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  decode.success(AuthSendCodeArgs(
    phone: phone,
    session_id: session_id,
  ))
}

pub fn decode_auth_send_code(args: json.Json) -> Result(AuthSendCodeArgs, DecodeError) {
  run_decoder(args, auth_send_code_decoder(), "auth_send_code")
}

fn auth_verify_code_decoder() -> Decoder(AuthVerifyCodeArgs) {
  use phone <- decode.field("phone", decode.string)
  use code <- decode.field("code", decode.string)
  use phone_code_hash <- decode.field("phone_code_hash", decode.string)
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  decode.success(AuthVerifyCodeArgs(
    phone: phone,
    code: code,
    phone_code_hash: phone_code_hash,
    session_id: session_id,
  ))
}

pub fn decode_auth_verify_code(args: json.Json) -> Result(AuthVerifyCodeArgs, DecodeError) {
  run_decoder(args, auth_verify_code_decoder(), "auth_verify_code")
}

fn auth_logout_decoder() -> Decoder(AuthLogoutArgs) {
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  decode.success(AuthLogoutArgs(session_id: session_id))
}

pub fn decode_auth_logout(args: json.Json) -> Result(AuthLogoutArgs, DecodeError) {
  run_decoder(args, auth_logout_decoder(), "auth_logout")
}

// ============================================================
// Knowledge Decoders
// ============================================================

fn knowledge_search_decoder() -> Decoder(KnowledgeSearchArgs) {
  use query <- decode.field("query", decode.string)
  use limit <- decode.optional_field("limit", None, decode.optional(decode.int))
  use chat_id <- decode.optional_field("chat_id", None, decode.optional(decode.string))
  decode.success(KnowledgeSearchArgs(
    query: query,
    limit: limit,
    chat_id: chat_id,
  ))
}

pub fn decode_knowledge_search(args: json.Json) -> Result(KnowledgeSearchArgs, DecodeError) {
  run_decoder(args, knowledge_search_decoder(), "knowledge_search")
}

fn knowledge_embed_decoder() -> Decoder(KnowledgeEmbedArgs) {
  use text <- decode.field("text", decode.string)
  use model <- decode.optional_field("model", None, decode.optional(decode.string))
  decode.success(KnowledgeEmbedArgs(
    text: text,
    model: model,
  ))
}

pub fn decode_knowledge_embed(args: json.Json) -> Result(KnowledgeEmbedArgs, DecodeError) {
  run_decoder(args, knowledge_embed_decoder(), "knowledge_embed")
}

// ============================================================
// File Decoders
// ============================================================

fn file_read_decoder() -> Decoder(FileReadArgs) {
  use path <- decode.field("path", decode.string)
  decode.success(FileReadArgs(path: path))
}

pub fn decode_file_read(args: json.Json) -> Result(FileReadArgs, DecodeError) {
  run_decoder(args, file_read_decoder(), "file_read")
}

fn file_write_decoder() -> Decoder(FileWriteArgs) {
  use path <- decode.field("path", decode.string)
  use content <- decode.field("content", decode.string)
  decode.success(FileWriteArgs(
    path: path,
    content: content,
  ))
}

pub fn decode_file_write(args: json.Json) -> Result(FileWriteArgs, DecodeError) {
  run_decoder(args, file_write_decoder(), "file_write")
}

fn file_list_decoder() -> Decoder(FileListArgs) {
  use path <- decode.field("path", decode.string)
  use pattern <- decode.optional_field("pattern", None, decode.optional(decode.string))
  decode.success(FileListArgs(
    path: path,
    pattern: pattern,
  ))
}

pub fn decode_file_list(args: json.Json) -> Result(FileListArgs, DecodeError) {
  run_decoder(args, file_list_decoder(), "file_list")
}

// ============================================================
// Voice Decoders
// ============================================================

fn voice_transcribe_decoder() -> Decoder(VoiceTranscribeArgs) {
  use file_path <- decode.field("file_path", decode.string)
  use language <- decode.optional_field("language", None, decode.optional(decode.string))
  decode.success(VoiceTranscribeArgs(
    file_path: file_path,
    language: language,
  ))
}

pub fn decode_voice_transcribe(args: json.Json) -> Result(VoiceTranscribeArgs, DecodeError) {
  run_decoder(args, voice_transcribe_decoder(), "voice_transcribe")
}

// ============================================================
// System Decoders
// ============================================================

fn system_log_decoder() -> Decoder(SystemLogArgs) {
  use level <- decode.field("level", decode.string)
  use message <- decode.field("message", decode.string)
  // context is handled specially - we just ignore it in decoder and pass raw json
  decode.success(SystemLogArgs(
    level: level,
    message: message,
    context: None,
  ))
}

pub fn decode_system_log(args: json.Json) -> Result(SystemLogArgs, DecodeError) {
  run_decoder(args, system_log_decoder(), "system_log")
}

fn system_exec_decoder() -> Decoder(SystemExecArgs) {
  use command <- decode.field("command", decode.string)
  use args <- decode.optional_field("args", None, decode.optional(decode.list(decode.string)))
  use timeout <- decode.optional_field("timeout", None, decode.optional(decode.int))
  decode.success(SystemExecArgs(
    command: command,
    args: args,
    timeout: timeout,
  ))
}

pub fn decode_system_exec(args: json.Json) -> Result(SystemExecArgs, DecodeError) {
  run_decoder(args, system_exec_decoder(), "system_exec")
}

// ============================================================
// Event Decoders
// ============================================================

fn event_emit_decoder() -> Decoder(EventEmitArgs) {
  use event_type <- decode.field("event_type", decode.string)
  use target <- decode.optional_field("target", None, decode.optional(decode.string))
  // payload is handled specially - we use raw json
  decode.success(EventEmitArgs(
    event_type: event_type,
    payload: json.object([]),
    target: target,
  ))
}

pub fn decode_event_emit(args: json.Json) -> Result(EventEmitArgs, DecodeError) {
  run_decoder(args, event_emit_decoder(), "event_emit")
}

fn event_list_decoder() -> Decoder(EventListArgs) {
  use event_type <- decode.optional_field("event_type", None, decode.optional(decode.string))
  use limit <- decode.optional_field("limit", None, decode.optional(decode.int))
  decode.success(EventListArgs(
    event_type: event_type,
    limit: limit,
  ))
}

pub fn decode_event_list(args: json.Json) -> Result(EventListArgs, DecodeError) {
  run_decoder(args, event_list_decoder(), "event_list")
}

// ============================================================
// Debug Decoders
// ============================================================

fn debug_build_decoder() -> Decoder(DebugBuildArgs) {
  use path <- decode.field("path", decode.string)
  use target <- decode.optional_field("target", None, decode.optional(decode.string))
  decode.success(DebugBuildArgs(
    path: path,
    target: target,
  ))
}

pub fn decode_debug_build(args: json.Json) -> Result(DebugBuildArgs, DecodeError) {
  run_decoder(args, debug_build_decoder(), "debug_build")
}

fn debug_test_decoder() -> Decoder(DebugTestArgs) {
  use path <- decode.field("path", decode.string)
  use filter <- decode.optional_field("filter", None, decode.optional(decode.string))
  decode.success(DebugTestArgs(
    path: path,
    filter: filter,
  ))
}

pub fn decode_debug_test(args: json.Json) -> Result(DebugTestArgs, DecodeError) {
  run_decoder(args, debug_test_decoder(), "debug_test")
}

fn debug_analyze_decoder() -> Decoder(DebugAnalyzeArgs) {
  use error_text <- decode.field("error_text", decode.string)
  use file_path <- decode.optional_field("file_path", None, decode.optional(decode.string))
  use context <- decode.optional_field("context", None, decode.optional(decode.string))
  decode.success(DebugAnalyzeArgs(
    error_text: error_text,
    file_path: file_path,
    context: context,
  ))
}

pub fn decode_debug_analyze(args: json.Json) -> Result(DebugAnalyzeArgs, DecodeError) {
  run_decoder(args, debug_analyze_decoder(), "debug_analyze")
}

fn debug_trace_decoder() -> Decoder(DebugTraceArgs) {
  use module <- decode.field("module", decode.string)
  use function <- decode.optional_field("function", None, decode.optional(decode.string))
  use level <- decode.optional_field("level", None, decode.optional(decode.string))
  decode.success(DebugTraceArgs(
    module: module,
    function: function,
    level: level,
  ))
}

pub fn decode_debug_trace(args: json.Json) -> Result(DebugTraceArgs, DecodeError) {
  run_decoder(args, debug_trace_decoder(), "debug_trace")
}

fn debug_log_decoder() -> Decoder(DebugLogArgs) {
  use operation <- decode.field("operation", decode.string)
  use status <- decode.field("status", decode.string)
  // data is handled specially
  decode.success(DebugLogArgs(
    operation: operation,
    status: status,
    data: None,
  ))
}

pub fn decode_debug_log(args: json.Json) -> Result(DebugLogArgs, DecodeError) {
  run_decoder(args, debug_log_decoder(), "debug_log")
}

// ============================================================
// Code Decoders
// ============================================================

fn code_generate_decoder() -> Decoder(CodeGenerateArgs) {
  use description <- decode.field("description", decode.string)
  use language <- decode.optional_field("language", None, decode.optional(decode.string))
  use template <- decode.optional_field("template", None, decode.optional(decode.string))
  decode.success(CodeGenerateArgs(
    description: description,
    language: language,
    template: template,
  ))
}

pub fn decode_code_generate(args: json.Json) -> Result(CodeGenerateArgs, DecodeError) {
  run_decoder(args, code_generate_decoder(), "code_generate")
}

fn code_refactor_decoder() -> Decoder(CodeRefactorArgs) {
  use file_path <- decode.field("file_path", decode.string)
  use refactoring <- decode.field("refactoring", decode.string)
  use target <- decode.field("target", decode.string)
  use new_name <- decode.optional_field("new_name", None, decode.optional(decode.string))
  decode.success(CodeRefactorArgs(
    file_path: file_path,
    refactoring: refactoring,
    target: target,
    new_name: new_name,
  ))
}

pub fn decode_code_refactor(args: json.Json) -> Result(CodeRefactorArgs, DecodeError) {
  run_decoder(args, code_refactor_decoder(), "code_refactor")
}

fn code_explain_decoder() -> Decoder(CodeExplainArgs) {
  use code <- decode.optional_field("code", None, decode.optional(decode.string))
  use file_path <- decode.optional_field("file_path", None, decode.optional(decode.string))
  use detail_level <- decode.optional_field("detail_level", None, decode.optional(decode.string))
  decode.success(CodeExplainArgs(
    code: code,
    file_path: file_path,
    detail_level: detail_level,
  ))
}

pub fn decode_code_explain(args: json.Json) -> Result(CodeExplainArgs, DecodeError) {
  run_decoder(args, code_explain_decoder(), "code_explain")
}

fn code_find_similar_decoder() -> Decoder(CodeFindSimilarArgs) {
  use pattern <- decode.field("pattern", decode.string)
  use path <- decode.optional_field("path", None, decode.optional(decode.string))
  use file_type <- decode.optional_field("file_type", None, decode.optional(decode.string))
  decode.success(CodeFindSimilarArgs(
    pattern: pattern,
    path: path,
    file_type: file_type,
  ))
}

pub fn decode_code_find_similar(args: json.Json) -> Result(CodeFindSimilarArgs, DecodeError) {
  run_decoder(args, code_find_similar_decoder(), "code_find_similar")
}

fn code_diff_decoder() -> Decoder(CodeDiffArgs) {
  use file_path <- decode.field("file_path", decode.string)
  use compare_with <- decode.optional_field("compare_with", None, decode.optional(decode.string))
  use other_file <- decode.optional_field("other_file", None, decode.optional(decode.string))
  decode.success(CodeDiffArgs(
    file_path: file_path,
    compare_with: compare_with,
    other_file: other_file,
  ))
}

pub fn decode_code_diff(args: json.Json) -> Result(CodeDiffArgs, DecodeError) {
  run_decoder(args, code_diff_decoder(), "code_diff")
}

// ============================================================
// Test Decoders
// ============================================================

fn test_run_decoder() -> Decoder(TestRunArgs) {
  use path <- decode.field("path", decode.string)
  use filter <- decode.optional_field("filter", None, decode.optional(decode.string))
  use verbose <- decode.optional_field("verbose", None, decode.optional(decode.bool))
  decode.success(TestRunArgs(
    path: path,
    filter: filter,
    verbose: verbose,
  ))
}

pub fn decode_test_run(args: json.Json) -> Result(TestRunArgs, DecodeError) {
  run_decoder(args, test_run_decoder(), "test_run")
}

fn test_create_decoder() -> Decoder(TestCreateArgs) {
  use function_code <- decode.field("function_code", decode.string)
  use function_name <- decode.optional_field("function_name", None, decode.optional(decode.string))
  use test_cases <- decode.optional_field("test_cases", None, decode.optional(decode.list(decode.string)))
  decode.success(TestCreateArgs(
    function_code: function_code,
    function_name: function_name,
    test_cases: test_cases,
  ))
}

pub fn decode_test_create(args: json.Json) -> Result(TestCreateArgs, DecodeError) {
  run_decoder(args, test_create_decoder(), "test_create")
}

fn test_coverage_decoder() -> Decoder(TestCoverageArgs) {
  use path <- decode.field("path", decode.string)
  use format <- decode.optional_field("format", None, decode.optional(decode.string))
  decode.success(TestCoverageArgs(
    path: path,
    format: format,
  ))
}

pub fn decode_test_coverage(args: json.Json) -> Result(TestCoverageArgs, DecodeError) {
  run_decoder(args, test_coverage_decoder(), "test_coverage")
}

fn test_validate_decoder() -> Decoder(TestValidateArgs) {
  use expected <- decode.field("expected", decode.string)
  use actual <- decode.field("actual", decode.string)
  use comparison <- decode.optional_field("comparison", None, decode.optional(decode.string))
  decode.success(TestValidateArgs(
    expected: expected,
    actual: actual,
    comparison: comparison,
  ))
}

pub fn decode_test_validate(args: json.Json) -> Result(TestValidateArgs, DecodeError) {
  run_decoder(args, test_validate_decoder(), "test_validate")
}

// ============================================================
// Agent Decoders
// ============================================================

fn agent_spawn_decoder() -> Decoder(AgentSpawnArgs) {
  use name <- decode.field("name", decode.string)
  use task <- decode.field("task", decode.string)
  use agent_type <- decode.optional_field("type", None, decode.optional(decode.string))
  use timeout <- decode.optional_field("timeout", None, decode.optional(decode.int))
  decode.success(AgentSpawnArgs(
    name: name,
    task: task,
    agent_type: agent_type,
    timeout: timeout,
  ))
}

pub fn decode_agent_spawn(args: json.Json) -> Result(AgentSpawnArgs, DecodeError) {
  run_decoder(args, agent_spawn_decoder(), "agent_spawn")
}

fn agent_message_decoder() -> Decoder(AgentMessageArgs) {
  use agent_id <- decode.field("agent_id", decode.string)
  use message <- decode.field("message", decode.string)
  use wait_response <- decode.optional_field("wait_response", None, decode.optional(decode.bool))
  decode.success(AgentMessageArgs(
    agent_id: agent_id,
    message: message,
    wait_response: wait_response,
  ))
}

pub fn decode_agent_message(args: json.Json) -> Result(AgentMessageArgs, DecodeError) {
  run_decoder(args, agent_message_decoder(), "agent_message")
}

fn agent_status_decoder() -> Decoder(AgentStatusArgs) {
  use agent_id <- decode.optional_field("agent_id", None, decode.optional(decode.string))
  decode.success(AgentStatusArgs(agent_id: agent_id))
}

pub fn decode_agent_status(args: json.Json) -> Result(AgentStatusArgs, DecodeError) {
  run_decoder(args, agent_status_decoder(), "agent_status")
}

fn agent_kill_decoder() -> Decoder(AgentKillArgs) {
  use agent_id <- decode.field("agent_id", decode.string)
  use force <- decode.optional_field("force", None, decode.optional(decode.bool))
  decode.success(AgentKillArgs(
    agent_id: agent_id,
    force: force,
  ))
}

pub fn decode_agent_kill(args: json.Json) -> Result(AgentKillArgs, DecodeError) {
  run_decoder(args, agent_kill_decoder(), "agent_kill")
}

// ============================================================
// Bot Analysis Decoders
// ============================================================

fn bot_analyze_decoder() -> Decoder(BotAnalyzeArgs) {
  use bot_username <- decode.field("bot_username", decode.string)
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  use depth <- decode.optional_field("depth", None, decode.optional(decode.string))
  use message_history <- decode.optional_field("message_history", None, decode.optional(decode.int))
  decode.success(BotAnalyzeArgs(
    bot_username: bot_username,
    session_id: session_id,
    depth: depth,
    message_history: message_history,
  ))
}

pub fn decode_bot_analyze(args: json.Json) -> Result(BotAnalyzeArgs, DecodeError) {
  run_decoder(args, bot_analyze_decoder(), "bot_analyze")
}

fn bot_compare_decoder() -> Decoder(BotCompareArgs) {
  use bots <- decode.field("bots", decode.list(decode.string))
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  use aspects <- decode.optional_field("aspects", None, decode.optional(decode.list(decode.string)))
  decode.success(BotCompareArgs(
    bots: bots,
    session_id: session_id,
    aspects: aspects,
  ))
}

pub fn decode_bot_compare(args: json.Json) -> Result(BotCompareArgs, DecodeError) {
  run_decoder(args, bot_compare_decoder(), "bot_compare")
}

fn bot_monitor_decoder() -> Decoder(BotMonitorArgs) {
  use bot_username <- decode.field("bot_username", decode.string)
  use action <- decode.field("action", decode.string)
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  use interval_seconds <- decode.optional_field("interval_seconds", None, decode.optional(decode.int))
  use metrics <- decode.optional_field("metrics", None, decode.optional(decode.list(decode.string)))
  decode.success(BotMonitorArgs(
    bot_username: bot_username,
    action: action,
    session_id: session_id,
    interval_seconds: interval_seconds,
    metrics: metrics,
  ))
}

pub fn decode_bot_monitor(args: json.Json) -> Result(BotMonitorArgs, DecodeError) {
  run_decoder(args, bot_monitor_decoder(), "bot_monitor")
}

fn bot_extract_commands_decoder() -> Decoder(BotExtractCommandsArgs) {
  use bot_username <- decode.field("bot_username", decode.string)
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  use methods <- decode.optional_field("methods", None, decode.optional(decode.list(decode.string)))
  use test_commands <- decode.optional_field("test_commands", None, decode.optional(decode.bool))
  decode.success(BotExtractCommandsArgs(
    bot_username: bot_username,
    session_id: session_id,
    methods: methods,
    test_commands: test_commands,
  ))
}

pub fn decode_bot_extract_commands(args: json.Json) -> Result(BotExtractCommandsArgs, DecodeError) {
  run_decoder(args, bot_extract_commands_decoder(), "bot_extract_commands")
}

fn interaction_decoder() -> Decoder(InteractionDef) {
  use interaction_type <- decode.field("type", decode.string)
  use value <- decode.field("value", decode.string)
  use expected_pattern <- decode.optional_field("expected_pattern", None, decode.optional(decode.string))
  use timeout_ms <- decode.optional_field("timeout_ms", None, decode.optional(decode.int))
  decode.success(InteractionDef(
    interaction_type: interaction_type,
    value: value,
    expected_pattern: expected_pattern,
    timeout_ms: timeout_ms,
  ))
}

fn bot_test_interaction_decoder() -> Decoder(BotTestInteractionArgs) {
  use bot_username <- decode.field("bot_username", decode.string)
  use session_id <- decode.optional_field("session_id", None, decode.optional(decode.string))
  use interactions <- decode.field("interactions", decode.list(interaction_decoder()))
  use wait_between_ms <- decode.optional_field("wait_between_ms", None, decode.optional(decode.int))
  decode.success(BotTestInteractionArgs(
    bot_username: bot_username,
    session_id: session_id,
    interactions: interactions,
    wait_between_ms: wait_between_ms,
  ))
}

pub fn decode_bot_test_interaction(args: json.Json) -> Result(BotTestInteractionArgs, DecodeError) {
  run_decoder(args, bot_test_interaction_decoder(), "bot_test_interaction")
}

// ============================================================
// Helper Functions
// ============================================================

/// Run decoder on json.Json and convert result to our error type
fn run_decoder(
  args: json.Json,
  decoder: Decoder(a),
  tool_name: String,
) -> Result(a, DecodeError) {
  // Convert json.Json to string then parse with decoder
  let json_str = json.to_string(args)
  // Log for debugging
  let _ = io.println_error("[DECODE] " <> tool_name <> ": " <> json_str)
  case json.parse(from: json_str, using: decoder) {
    Ok(parsed) -> Ok(parsed)
    Error(_) -> {
      let _ = io.println_error("[DECODE ERROR] Failed to parse " <> tool_name)
      Error(ParseError("Failed to parse " <> tool_name <> " arguments"))
    }
  }
}

// ============================================================
// Rainbow Bridge Decoders
// ============================================================

fn rainbow_debug_cycle_decoder() -> Decoder(RainbowDebugCycleArgs) {
  use path <- decode.field("path", decode.string)
  use max_iterations <- decode.optional_field("max_iterations", None, decode.optional(decode.int))
  decode.success(RainbowDebugCycleArgs(
    path: path,
    max_iterations: max_iterations,
  ))
}

pub fn decode_rainbow_debug_cycle(args: json.Json) -> Result(RainbowDebugCycleArgs, DecodeError) {
  run_decoder(args, rainbow_debug_cycle_decoder(), "rainbow_autonomous_debug_cycle")
}

fn task_create_decoder() -> Decoder(TaskCreateArgs) {
  use description <- decode.field("description", decode.string)
  use files <- decode.optional_field("files", None, decode.optional(decode.list(decode.string)))
  decode.success(TaskCreateArgs(
    description: description,
    files: files,
  ))
}

pub fn decode_task_create(args: json.Json) -> Result(TaskCreateArgs, DecodeError) {
  run_decoder(args, task_create_decoder(), "task_create")
}

fn task_get_decoder() -> Decoder(TaskGetArgs) {
  use task_id <- decode.field("task_id", decode.string)
  decode.success(TaskGetArgs(task_id: task_id))
}

pub fn decode_task_get(args: json.Json) -> Result(TaskGetArgs, DecodeError) {
  run_decoder(args, task_get_decoder(), "task_get")
}

fn task_list_decoder() -> Decoder(TaskListArgs) {
  use state <- decode.optional_field("state", None, decode.optional(decode.string))
  decode.success(TaskListArgs(state: state))
}

pub fn decode_task_list(args: json.Json) -> Result(TaskListArgs, DecodeError) {
  run_decoder(args, task_list_decoder(), "task_list")
}

fn task_update_decoder() -> Decoder(TaskUpdateArgs) {
  use task_id <- decode.field("task_id", decode.string)
  use state <- decode.field("state", decode.string)
  decode.success(TaskUpdateArgs(
    task_id: task_id,
    state: state,
  ))
}

pub fn decode_task_update(args: json.Json) -> Result(TaskUpdateArgs, DecodeError) {
  run_decoder(args, task_update_decoder(), "task_update")
}

fn heal_start_decoder() -> Decoder(HealStartArgs) {
  use task_id <- decode.field("task_id", decode.string)
  decode.success(HealStartArgs(task_id: task_id))
}

pub fn decode_heal_start(args: json.Json) -> Result(HealStartArgs, DecodeError) {
  run_decoder(args, heal_start_decoder(), "heal_start")
}

fn heal_apply_fix_decoder() -> Decoder(HealApplyFixArgs) {
  use task_id <- decode.field("task_id", decode.string)
  use fix_id <- decode.field("fix_id", decode.string)
  decode.success(HealApplyFixArgs(
    task_id: task_id,
    fix_id: fix_id,
  ))
}

pub fn decode_heal_apply_fix(args: json.Json) -> Result(HealApplyFixArgs, DecodeError) {
  run_decoder(args, heal_apply_fix_decoder(), "heal_apply_fix")
}

fn heal_verify_decoder() -> Decoder(HealVerifyArgs) {
  use task_id <- decode.field("task_id", decode.string)
  use fix_id <- decode.field("fix_id", decode.string)
  decode.success(HealVerifyArgs(
    task_id: task_id,
    fix_id: fix_id,
  ))
}

pub fn decode_heal_verify(args: json.Json) -> Result(HealVerifyArgs, DecodeError) {
  run_decoder(args, heal_verify_decoder(), "heal_verify")
}

fn heal_rollback_decoder() -> Decoder(HealRollbackArgs) {
  use task_id <- decode.field("task_id", decode.string)
  use fix_id <- decode.field("fix_id", decode.string)
  decode.success(HealRollbackArgs(
    task_id: task_id,
    fix_id: fix_id,
  ))
}

pub fn decode_heal_rollback(args: json.Json) -> Result(HealRollbackArgs, DecodeError) {
  run_decoder(args, heal_rollback_decoder(), "heal_rollback")
}

fn decide_next_step_decoder() -> Decoder(DecideNextStepArgs) {
  use task_id <- decode.field("task_id", decode.string)
  decode.success(DecideNextStepArgs(task_id: task_id))
}

pub fn decode_decide_next_step(args: json.Json) -> Result(DecideNextStepArgs, DecodeError) {
  run_decoder(args, decide_next_step_decoder(), "decide_next_step")
}

fn decide_apply_decoder() -> Decoder(DecideApplyArgs) {
  use task_id <- decode.field("task_id", decode.string)
  use decision_id <- decode.field("decision_id", decode.string)
  decode.success(DecideApplyArgs(
    task_id: task_id,
    decision_id: decision_id,
  ))
}

pub fn decode_decide_apply(args: json.Json) -> Result(DecideApplyArgs, DecodeError) {
  run_decoder(args, decide_apply_decoder(), "decide_apply")
}
