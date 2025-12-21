// Scene Types - Convenience types for scene handlers
// Re-exports and helpers for scene implementations

import gleam/option.{type Option, None, Some}
import vibee/bot/scene.{
  type InlineButton, type OutgoingMessage, type UserSession,
  TextReply, TextWithKeyboard, InlineButton,
}
import vibee/bot/scene_handler.{NoAction}

// Re-export types from scene_handler
pub type SceneResult = scene_handler.SceneResult
pub type NextAction = scene_handler.NextAction

// =============================================================================
// Response Types - Simplified constructors
// =============================================================================

/// Response type for scene results
pub type SceneResponse {
  Reply(text: String)
  SendButtons(text: String, buttons: List(List(#(String, String))))
  SendPhoto(url: String, caption: Option(String))
  NoReply
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Create SceneResult with text reply
pub fn reply(session: UserSession, text: String) -> SceneResult {
  scene_handler.SceneResult(
    session: session,
    response: Some(TextReply(text)),
    next_action: NoAction,
  )
}

/// Create SceneResult with buttons
pub fn send_buttons(
  session: UserSession,
  text: String,
  buttons: List(List(#(String, String))),
) -> SceneResult {
  let keyboard = buttons
    |> convert_buttons

  scene_handler.SceneResult(
    session: session,
    response: Some(TextWithKeyboard(text, keyboard)),
    next_action: NoAction,
  )
}

/// Create SceneResult without response
pub fn no_reply(session: UserSession) -> SceneResult {
  scene_handler.SceneResult(
    session: session,
    response: None,
    next_action: NoAction,
  )
}

/// Convert button tuples to InlineButton format
fn convert_buttons(
  buttons: List(List(#(String, String))),
) -> List(List(InlineButton)) {
  buttons
  |> list.map(fn(row) {
    row
    |> list.map(fn(btn) {
      let #(text, data) = btn
      InlineButton(text: text, callback_data: data)
    })
  })
}

import gleam/list
