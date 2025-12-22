// Type definitions for all MCP tool arguments
// Provides type safety for JSON parsing

import gleam/option.{type Option}
import gleam/json

// ============================================================
// Telegram Tool Arguments
// ============================================================

pub type TelegramGetDialogsArgs {
  TelegramGetDialogsArgs(
    session_id: Option(String),
    limit: Option(Int),
    type_filter: Option(String),
  )
}

pub type TelegramGetHistoryArgs {
  TelegramGetHistoryArgs(
    session_id: Option(String),
    chat_id: String,
    limit: Option(Int),
    offset_id: Option(Int),
  )
}

pub type TelegramSendMessageArgs {
  TelegramSendMessageArgs(
    session_id: Option(String),
    chat_id: String,
    text: String,
    reply_to: Option(Int),
  )
}

pub type ButtonDef {
  ButtonDef(
    text: String,
    callback_data: Option(String),
    url: Option(String),
  )
}

pub type TelegramSendButtonsArgs {
  TelegramSendButtonsArgs(
    session_id: Option(String),
    chat_id: String,
    text: String,
    buttons: List(List(ButtonDef)),
  )
}

pub type TelegramSendPhotoArgs {
  TelegramSendPhotoArgs(
    session_id: Option(String),
    chat_id: String,
    file_path: String,
    caption: Option(String),
  )
}

pub type TelegramDownloadMediaArgs {
  TelegramDownloadMediaArgs(
    session_id: Option(String),
    chat_id: String,
    message_id: Int,
    output_path: Option(String),
  )
}

pub type TelegramGetMeArgs {
  TelegramGetMeArgs(session_id: Option(String))
}

pub type TelegramGetUserPhotoArgs {
  TelegramGetUserPhotoArgs(
    session_id: Option(String),
    user_id: Int,
  )
}

pub type TelegramSubscribeUpdatesArgs {
  TelegramSubscribeUpdatesArgs(
    session_id: Option(String),
    event_types: Option(List(String)),
  )
}

// ============================================================
// Auth Tool Arguments
// ============================================================

pub type AuthStatusArgs {
  AuthStatusArgs(session_id: Option(String))
}

pub type AuthSendCodeArgs {
  AuthSendCodeArgs(
    phone: String,
    session_id: Option(String),
  )
}

pub type AuthVerifyCodeArgs {
  AuthVerifyCodeArgs(
    phone: String,
    code: String,
    phone_code_hash: String,
    session_id: Option(String),
  )
}

pub type AuthLogoutArgs {
  AuthLogoutArgs(session_id: Option(String))
}

// ============================================================
// Knowledge Tool Arguments
// ============================================================

pub type KnowledgeSearchArgs {
  KnowledgeSearchArgs(
    query: String,
    limit: Option(Int),
    chat_id: Option(String),
  )
}

pub type KnowledgeEmbedArgs {
  KnowledgeEmbedArgs(
    text: String,
    model: Option(String),
  )
}

// ============================================================
// File Tool Arguments
// ============================================================

pub type FileReadArgs {
  FileReadArgs(path: String)
}

pub type FileWriteArgs {
  FileWriteArgs(
    path: String,
    content: String,
  )
}

pub type FileListArgs {
  FileListArgs(
    path: String,
    pattern: Option(String),
  )
}

// ============================================================
// Voice Tool Arguments
// ============================================================

pub type VoiceTranscribeArgs {
  VoiceTranscribeArgs(
    file_path: String,
    language: Option(String),
  )
}

// ============================================================
// System Tool Arguments
// ============================================================

pub type SystemLogArgs {
  SystemLogArgs(
    level: String,
    message: String,
    context: Option(json.Json),
  )
}

pub type SystemExecArgs {
  SystemExecArgs(
    command: String,
    args: Option(List(String)),
    timeout: Option(Int),
  )
}

// ============================================================
// Event Tool Arguments
// ============================================================

pub type EventEmitArgs {
  EventEmitArgs(
    event_type: String,
    payload: json.Json,
    target: Option(String),
  )
}

pub type EventListArgs {
  EventListArgs(
    event_type: Option(String),
    limit: Option(Int),
  )
}

// ============================================================
// Debug Tool Arguments
// ============================================================

pub type DebugBuildArgs {
  DebugBuildArgs(
    path: String,
    target: Option(String),
  )
}

pub type DebugTestArgs {
  DebugTestArgs(
    path: String,
    filter: Option(String),
  )
}

pub type DebugAnalyzeArgs {
  DebugAnalyzeArgs(
    error_text: String,
    file_path: Option(String),
    context: Option(String),
  )
}

pub type DebugTraceArgs {
  DebugTraceArgs(
    module: String,
    function: Option(String),
    level: Option(String),
  )
}

pub type DebugLogArgs {
  DebugLogArgs(
    operation: String,
    status: String,
    data: Option(json.Json),
  )
}

// ============================================================
// Code Tool Arguments
// ============================================================

pub type CodeGenerateArgs {
  CodeGenerateArgs(
    description: String,
    language: Option(String),
    template: Option(String),
  )
}

pub type CodeRefactorArgs {
  CodeRefactorArgs(
    file_path: String,
    refactoring: String,
    target: String,
    new_name: Option(String),
  )
}

pub type CodeExplainArgs {
  CodeExplainArgs(
    code: Option(String),
    file_path: Option(String),
    detail_level: Option(String),
  )
}

pub type CodeFindSimilarArgs {
  CodeFindSimilarArgs(
    pattern: String,
    path: Option(String),
    file_type: Option(String),
  )
}

pub type CodeDiffArgs {
  CodeDiffArgs(
    file_path: String,
    compare_with: Option(String),
    other_file: Option(String),
  )
}

// ============================================================
// Test Tool Arguments
// ============================================================

pub type TestRunArgs {
  TestRunArgs(
    path: String,
    filter: Option(String),
    verbose: Option(Bool),
  )
}

pub type TestCreateArgs {
  TestCreateArgs(
    function_code: String,
    function_name: Option(String),
    test_cases: Option(List(String)),
  )
}

pub type TestCoverageArgs {
  TestCoverageArgs(
    path: String,
    format: Option(String),
  )
}

pub type TestValidateArgs {
  TestValidateArgs(
    expected: String,
    actual: String,
    comparison: Option(String),
  )
}

// ============================================================
// Agent Tool Arguments
// ============================================================

pub type AgentSpawnArgs {
  AgentSpawnArgs(
    name: String,
    task: String,
    agent_type: Option(String),
    timeout: Option(Int),
  )
}

pub type AgentMessageArgs {
  AgentMessageArgs(
    agent_id: String,
    message: String,
    wait_response: Option(Bool),
  )
}

pub type AgentStatusArgs {
  AgentStatusArgs(agent_id: Option(String))
}

pub type AgentKillArgs {
  AgentKillArgs(
    agent_id: String,
    force: Option(Bool),
  )
}

// ============================================================
// Bot Analysis Tool Arguments
// ============================================================

pub type BotAnalyzeArgs {
  BotAnalyzeArgs(
    bot_username: String,
    session_id: Option(String),
    depth: Option(String),
    message_history: Option(Int),
  )
}

pub type BotCompareArgs {
  BotCompareArgs(
    bots: List(String),
    session_id: Option(String),
    aspects: Option(List(String)),
  )
}

pub type BotMonitorArgs {
  BotMonitorArgs(
    bot_username: String,
    action: String,
    session_id: Option(String),
    interval_seconds: Option(Int),
    metrics: Option(List(String)),
  )
}

pub type BotExtractCommandsArgs {
  BotExtractCommandsArgs(
    bot_username: String,
    session_id: Option(String),
    methods: Option(List(String)),
    test_commands: Option(Bool),
  )
}

pub type BotTestInteractionArgs {
  BotTestInteractionArgs(
    bot_username: String,
    session_id: Option(String),
    interactions: List(InteractionDef),
    wait_between_ms: Option(Int),
  )
}

pub type InteractionDef {
  InteractionDef(
    interaction_type: String,
    value: String,
    expected_pattern: Option(String),
    timeout_ms: Option(Int),
  )
}

// ============================================================
// Rainbow Bridge Args
// ============================================================

pub type RainbowDebugCycleArgs {
  RainbowDebugCycleArgs(
    path: String,
    max_iterations: Option(Int),
  )
}

pub type TaskCreateArgs {
  TaskCreateArgs(
    description: String,
    files: Option(List(String)),
  )
}

pub type TaskGetArgs {
  TaskGetArgs(
    task_id: String,
  )
}

pub type TaskListArgs {
  TaskListArgs(
    state: Option(String),
  )
}

pub type TaskUpdateArgs {
  TaskUpdateArgs(
    task_id: String,
    state: String,
  )
}

pub type HealStartArgs {
  HealStartArgs(
    task_id: String,
  )
}

pub type HealApplyFixArgs {
  HealApplyFixArgs(
    task_id: String,
    fix_id: String,
  )
}

pub type HealVerifyArgs {
  HealVerifyArgs(
    task_id: String,
    fix_id: String,
  )
}

pub type HealRollbackArgs {
  HealRollbackArgs(
    task_id: String,
    fix_id: String,
  )
}

pub type DecideNextStepArgs {
  DecideNextStepArgs(
    task_id: String,
  )
}

pub type DecideApplyArgs {
  DecideApplyArgs(
    task_id: String,
    decision_id: String,
  )
}
