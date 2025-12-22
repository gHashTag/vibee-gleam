// Reels Plugin (ElizaOS Architecture)
//
// Registers all reels-related actions, providers, and evaluators.
// This is the main entry point for the reels functionality.
//
// NO COMMANDS! This plugin works through conversation understanding.
// The user doesn't type /reels - they say "создай рилс про бизнес"
// and the action validates and triggers automatically.

import gleam/io
import vibee/agent/eliza.{
  type ActionContext, type ActionRegistry, type ActionResult,
  ActionContext,
  new_registry, register_action, register_provider, register_evaluator,
  find_matching_action, execute_action, empty_context,
}
import vibee/agent/actions/reels_action
import vibee/agent/providers/reels_context_provider
import vibee/agent/evaluators/reels_evaluator

// ============================================================
// Plugin Initialization
// ============================================================

/// Initialize the Reels Plugin with all components
pub fn init() -> ActionRegistry {
  io.println("[REELS_PLUGIN] Initializing...")

  let registry = new_registry()

  // Register actions
  let registry = register_action(registry, reels_action.create_reels_action())

  // Register providers
  let registry = register_provider(registry, reels_context_provider.create_reels_context_provider())

  // Register evaluators
  let registry = register_evaluator(registry, reels_evaluator.create_reels_evaluator())
  let registry = register_evaluator(registry, reels_evaluator.create_analytics_evaluator())

  io.println("[REELS_PLUGIN] Initialized with 1 action, 1 provider, 2 evaluators")

  registry
}

// ============================================================
// Plugin Entry Points
// ============================================================

/// Process a message and execute matching action if any
pub fn process_message(
  registry: ActionRegistry,
  context: ActionContext,
) -> Result(ActionResult, String) {
  io.println("[REELS_PLUGIN] Processing message: " <> context.message)

  // Find matching action
  case find_matching_action(registry, context) {
    option.Some(action) -> {
      io.println("[REELS_PLUGIN] Found matching action: " <> action.name)

      // Execute action with full flow (providers + handler + evaluators)
      let result = execute_action(registry, action, context)

      Ok(result)
    }
    option.None -> {
      io.println("[REELS_PLUGIN] No matching action found")
      Error("No matching action for this message")
    }
  }
}

/// Check if message should trigger reels action (for integration with existing bot)
pub fn should_handle_message(message: String) -> Bool {
  let context = ActionContext(
    ..empty_context(),
    message: message,
  )

  let registry = init()

  case find_matching_action(registry, context) {
    option.Some(_) -> True
    option.None -> False
  }
}

// ============================================================
// Testing Helpers
// ============================================================

/// Test the plugin with a sample message
pub fn test_plugin() -> Result(ActionResult, String) {
  io.println("\n=== REELS PLUGIN TEST ===\n")

  let registry = init()

  let context = ActionContext(
    user_id: 12345,
    chat_id: "test_chat_123",
    message: "Создай рилс про продуктивность и тайм-менеджмент",
    history: [
      "Привет!",
      "Хочу создавать контент для инстаграма",
      "Интересуют темы бизнеса и продуктивности",
    ],
    user_name: option.Some("TestUser"),
    photo_url: option.None,
    previous_results: [],
  )

  process_message(registry, context)
}

/// Test validation only
pub fn test_validation() -> Nil {
  io.println("\n=== VALIDATION TEST ===\n")

  let test_messages = [
    #("Создай рилс про бизнес", True),
    #("Хочу рилс на тему здоровья", True),
    #("Make a reel about technology", True),
    #("Привет, как дела?", False),
    #("Расскажи о погоде", False),
    #("Сделай видео для инстаграма", True),
    #("Генерируй reels контент", True),
  ]

  list.each(test_messages, fn(test_case) {
    let #(message, expected) = test_case
    let result = should_handle_message(message)
    let status = case result == expected {
      True -> "PASS"
      False -> "FAIL"
    }
    io.println(status <> ": \"" <> message <> "\" -> " <> string.inspect(result))
  })

  Nil
}

import gleam/list
import gleam/option
import gleam/string
