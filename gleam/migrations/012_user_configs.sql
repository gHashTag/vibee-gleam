-- Migration: Create user_configs table for multi-tenant support
-- Allows per-user configuration with different settings, chats, and features

CREATE TABLE IF NOT EXISTS user_configs (
    telegram_id BIGINT PRIMARY KEY,
    owner_id BIGINT,                    -- Who manages this user (for multi-tenant)
    session_id VARCHAR(100),            -- Associated Telegram session from bridge

    -- Feature flags (per-user overrides)
    digital_twin_enabled BOOLEAN DEFAULT false,
    p2p_enabled BOOLEAN DEFAULT false,
    earning_enabled BOOLEAN DEFAULT false,

    -- Chat configuration (per-user)
    target_chats JSONB DEFAULT '[]',    -- Monitored chats for this user
    trigger_chats JSONB DEFAULT '[]',   -- Trigger response chats for this user

    -- Personalization
    response_template TEXT,             -- Custom response template
    llm_model VARCHAR(100),             -- Custom LLM model override
    voice_id VARCHAR(100),              -- ElevenLabs voice ID for TTS
    avatar_url TEXT,                    -- Custom avatar URL

    -- Wallet configuration
    ton_wallet VARCHAR(100),            -- TON wallet address
    preferred_currency VARCHAR(10) DEFAULT 'USDT',

    -- Metadata
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Indexes for efficient lookups
CREATE INDEX IF NOT EXISTS idx_user_configs_owner ON user_configs(owner_id);
CREATE INDEX IF NOT EXISTS idx_user_configs_session ON user_configs(session_id);
CREATE INDEX IF NOT EXISTS idx_user_configs_updated ON user_configs(updated_at DESC);

-- Auto-update updated_at
CREATE OR REPLACE FUNCTION update_user_configs_updated_at()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trigger_user_configs_updated_at ON user_configs;
CREATE TRIGGER trigger_user_configs_updated_at
    BEFORE UPDATE ON user_configs
    FOR EACH ROW
    EXECUTE FUNCTION update_user_configs_updated_at();

-- Add comments for documentation
COMMENT ON TABLE user_configs IS 'Per-user configuration for multi-tenant VIBEE deployments. Allows different users to have different settings, chats, and features enabled.';
COMMENT ON COLUMN user_configs.owner_id IS 'Telegram ID of the owner who manages this user config';
COMMENT ON COLUMN user_configs.session_id IS 'Session ID from Telegram Bridge for this user';
COMMENT ON COLUMN user_configs.target_chats IS 'JSON array of chat IDs to monitor for this user';
COMMENT ON COLUMN user_configs.trigger_chats IS 'JSON array of chat IDs with trigger-based responses';
