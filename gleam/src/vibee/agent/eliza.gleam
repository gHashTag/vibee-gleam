// ElizaOS-inspired Architecture for User-Bots
// Based on https://docs.elizaos.ai/plugins/architecture
//
// KEY PRINCIPLE: NO COMMANDS! User-bots work through conversation understanding.
// Actions trigger based on context and NLP patterns, not explicit /commands.
//
// Components:
// - Actions: discrete tasks with validate() + handler()
// - Providers: aggregate context for decision-making
// - Evaluators: assess situations and determine responses

import gleam/dict.{type Dict}
import gleam/option.{type Option}

// ============================================================
// Core Types
// ============================================================

/// Action Result - standardized return from action handlers
pub type ActionResult {
  ActionResult(
    success: Bool,
    text: String,           // Response text for user
    values: Dict(String, String),  // Key-value pairs
    data: Dict(String, String),    // Structured data as strings
  )
}

/// Action Context - passed to validate() and handler()
pub type ActionContext {
  ActionContext(
    user_id: Int,
    chat_id: String,
    message: String,        // Current message text
    history: List(String),  // Recent message history
    user_name: Option(String),
    photo_url: Option(String),
    // Previous action results (for chaining)
    previous_results: List(ActionResult),
  )
}

/// Action - discrete task an agent can perform
/// Similar to ElizaOS Action interface
pub type Action {
  Action(
    name: String,
    description: String,
    // Similes for fuzzy NLP matching (synonyms, triggers)
    similes: List(String),
    // Examples of messages that should trigger this action
    examples: List(String),
    // Validate - determines if action should run
    // Returns True if context matches action requirements
    validate: fn(ActionContext) -> Bool,
    // Handler - executes the action
    handler: fn(ActionContext) -> ActionResult,
  )
}

/// Provider - supplies contextual information before decisions
pub type Provider {
  Provider(
    name: String,
    description: String,
    // Dynamic - refreshes each call (True) or cached (False)
    dynamic: Bool,
    // Position - execution priority (higher = runs first)
    position: Int,
    // Get context - returns structured data for action decisions
    get: fn(ActionContext) -> ProviderResult,
  )
}

/// Provider Result - context data from provider
pub type ProviderResult {
  ProviderResult(
    text: String,              // Human-readable summary
    values: Dict(String, String),
    data: Dict(String, String),
  )
}

/// Evaluator - post-processes responses, extracts insights
pub type Evaluator {
  Evaluator(
    name: String,
    description: String,
    // Handler - analyze response and extract info
    handler: fn(ActionContext, ActionResult) -> EvaluatorResult,
  )
}

/// Evaluator Result
pub type EvaluatorResult {
  EvaluatorResult(
    // Should we store this in memory?
    store_memory: Bool,
    // Extracted facts/insights
    facts: List(String),
    // Suggested follow-up actions
    suggested_actions: List(String),
  )
}

// ============================================================
// Action Registry
// ============================================================

/// Registry of all available actions
pub type ActionRegistry {
  ActionRegistry(
    actions: List(Action),
    providers: List(Provider),
    evaluators: List(Evaluator),
  )
}

/// Create empty registry
pub fn new_registry() -> ActionRegistry {
  ActionRegistry(
    actions: [],
    providers: [],
    evaluators: [],
  )
}

/// Register an action
pub fn register_action(
  registry: ActionRegistry,
  action: Action,
) -> ActionRegistry {
  ActionRegistry(
    ..registry,
    actions: [action, ..registry.actions],
  )
}

/// Register a provider
pub fn register_provider(
  registry: ActionRegistry,
  provider: Provider,
) -> ActionRegistry {
  ActionRegistry(
    ..registry,
    providers: [provider, ..registry.providers],
  )
}

/// Register an evaluator
pub fn register_evaluator(
  registry: ActionRegistry,
  evaluator: Evaluator,
) -> ActionRegistry {
  ActionRegistry(
    ..registry,
    evaluators: [evaluator, ..registry.evaluators],
  )
}

// ============================================================
// Action Execution Flow
// ============================================================

/// Find matching action for context
/// Uses validate() and simile matching
pub fn find_matching_action(
  registry: ActionRegistry,
  context: ActionContext,
) -> Option(Action) {
  find_first_valid(registry.actions, context)
}

fn find_first_valid(
  actions: List(Action),
  context: ActionContext,
) -> Option(Action) {
  case actions {
    [] -> option.None
    [action, ..rest] -> {
      case action.validate(context) {
        True -> option.Some(action)
        False -> find_first_valid(rest, context)
      }
    }
  }
}

/// Execute action with full flow:
/// 1. Gather context from providers
/// 2. Execute action handler
/// 3. Process with evaluators
pub fn execute_action(
  registry: ActionRegistry,
  action: Action,
  context: ActionContext,
) -> ActionResult {
  // Step 1: Gather provider context
  let _provider_data = gather_provider_context(registry.providers, context)

  // Step 2: Execute action handler
  let result = action.handler(context)

  // Step 3: Run evaluators (for memory/analytics)
  let _evaluator_results = run_evaluators(registry.evaluators, context, result)

  // Return action result
  result
}

fn gather_provider_context(
  providers: List(Provider),
  context: ActionContext,
) -> List(ProviderResult) {
  // Sort by position (higher first)
  // For now, just run all providers
  case providers {
    [] -> []
    [provider, ..rest] -> {
      let result = provider.get(context)
      [result, ..gather_provider_context(rest, context)]
    }
  }
}

fn run_evaluators(
  evaluators: List(Evaluator),
  context: ActionContext,
  result: ActionResult,
) -> List(EvaluatorResult) {
  case evaluators {
    [] -> []
    [evaluator, ..rest] -> {
      let eval_result = evaluator.handler(context, result)
      [eval_result, ..run_evaluators(rest, context, result)]
    }
  }
}

// ============================================================
// Helper Functions
// ============================================================

/// Create a successful action result
pub fn success_result(text: String) -> ActionResult {
  ActionResult(
    success: True,
    text: text,
    values: dict.new(),
    data: dict.new(),
  )
}

/// Create a successful action result with data
pub fn success_result_with_data(
  text: String,
  data: Dict(String, String),
) -> ActionResult {
  ActionResult(
    success: True,
    text: text,
    values: dict.new(),
    data: data,
  )
}

/// Create a failed action result
pub fn error_result(error: String) -> ActionResult {
  ActionResult(
    success: False,
    text: error,
    values: dict.new(),
    data: dict.new(),
  )
}

/// Create empty context for testing
pub fn empty_context() -> ActionContext {
  ActionContext(
    user_id: 0,
    chat_id: "",
    message: "",
    history: [],
    user_name: option.None,
    photo_url: option.None,
    previous_results: [],
  )
}
