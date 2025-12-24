-- Migration: Populate global_config with default values
-- This provides a central configuration store that can be managed via MCP tools
-- All values default to 0/empty - must be configured for each deployment

-- Ensure global_config table exists (should be created by migration 007)
-- CREATE TABLE IF NOT EXISTS already handled by 007_dynamic_config.sql

-- Populate default configuration values
INSERT INTO global_config (key, value, value_type, description) VALUES
  -- User IDs (must be configured per deployment)
  ('owner_id', '0', 'int', 'Primary owner Telegram ID'),
  ('agent_user_id', '0', 'int', 'Bot/Agent Telegram user ID'),

  -- Chat IDs (must be configured per deployment)
  ('leads_group_id', '0', 'int', 'Leads forwarding group ID'),
  ('test_channel_id', '0', 'int', 'Test/monitoring channel ID'),
  ('trigger_chat_id', '0', 'int', 'Primary trigger chat ID'),
  ('trigger_chat_2_id', '0', 'int', 'Secondary trigger chat ID'),
  ('alert_chat_id', '0', 'int', 'Alert notifications chat ID'),

  -- Service URLs (production defaults)
  ('bridge_url', 'https://vibee-telegram-bridge.fly.dev', 'string', 'Telegram Bridge URL'),
  ('embedding_url', 'https://api.openai.com/v1/embeddings', 'string', 'Embeddings API URL'),
  ('remotion_player_url', 'https://vibee-remotion.fly.dev', 'string', 'Remotion render URL'),

  -- AI Configuration
  ('llm_model', 'anthropic/claude-sonnet-4-20250514', 'string', 'Default LLM model'),
  ('embedding_model', 'text-embedding-3-small', 'string', 'Default embedding model'),

  -- Feature flags
  ('digital_twin_enabled', 'true', 'bool', 'Enable Digital Twin mode'),
  ('p2p_enabled', 'true', 'bool', 'Enable P2P trading features'),
  ('earning_enabled', 'false', 'bool', 'Enable earning/arbitrage features'),

  -- Lists (JSON format)
  ('target_chats', '[]', 'json', 'List of monitored chat IDs')

ON CONFLICT (key) DO NOTHING;

-- Add comment for documentation
COMMENT ON TABLE global_config IS 'Centralized configuration store for VIBEE. Values can be updated via MCP tools (config_set, config_get) without redeployment.';
