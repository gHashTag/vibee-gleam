// Централизованная конфигурация Telegram
// Все настройки в одном месте
//
// SECURITY: All credentials MUST come from ENV variables
// Never hardcode API keys, phone numbers, or secrets
//
// Required ENV variables:
// - TELEGRAM_API_ID: Telegram API ID from my.telegram.org
// - TELEGRAM_API_HASH: Telegram API Hash from my.telegram.org
// - TELEGRAM_DEFAULT_PHONE: Default phone for auth (optional)
//
// NOTE: session_id больше не хардкодится здесь.
// Используйте session_manager для управления сессиями:
// - session_list - показать все сессии
// - session_set_active - установить активную сессию
// - session_create - создать новую сессию

import gleam/int
import gleam/result
import vibee/mcp/config

/// Bridge URL для Go MTProto bridge (из ENV, обязателен для production)
/// Возвращает пустую строку если не установлен - caller должен обрабатывать
pub fn bridge_url() -> String {
  config.get_env("VIBEE_BRIDGE_URL")
}

/// API ID from ENV (required for Telegram MTProto)
pub fn api_id() -> Int {
  config.get_env("TELEGRAM_API_ID")
  |> int.parse
  |> result.unwrap(0)
}

/// API Hash from ENV (required for Telegram MTProto)
pub fn api_hash() -> String {
  config.get_env("TELEGRAM_API_HASH")
}

/// Default phone from ENV (optional, for auto-connect)
pub fn default_phone() -> String {
  config.get_env("TELEGRAM_DEFAULT_PHONE")
}

/// Bridge API Key from ENV (required for Go bridge auth)
pub fn bridge_api_key() -> String {
  config.get_env("VIBEE_API_KEY")
}

/// Check if Telegram credentials are configured
pub fn is_configured() -> Bool {
  api_id() > 0 && api_hash() != ""
}
