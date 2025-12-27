-- Migration: User-Bot Setup Wizard Progress
-- Stores wizard state for multi-step onboarding
-- Users can connect their own Telegram account as user-bot

CREATE TABLE IF NOT EXISTS userbot_setup_progress (
    user_id BIGINT PRIMARY KEY,  -- Telegram user_id who is setting up

    -- Wizard step tracking
    current_step VARCHAR(50) NOT NULL DEFAULT 'welcome',

    -- Step 1: Auth data
    phone VARCHAR(20),
    bridge_session_id VARCHAR(100),  -- Session ID from Bridge
    connected_telegram_id BIGINT,    -- Telegram ID of connected account
    connected_username VARCHAR(100), -- Username of connected account

    -- Step 2: Selected chats (JSON array of chat_ids)
    selected_chats JSONB DEFAULT '[]',

    -- Step 3: Mode configuration (JSON object {chat_id: "digital_twin"|"observer"})
    chat_modes JSONB DEFAULT '{}',

    -- Step 4: Trigger words (JSON array)
    trigger_words JSONB DEFAULT '[]',
    trigger_preset VARCHAR(50),  -- 'crypto', 'realty', 'it', 'custom'

    -- Step 5: Character config (for Digital Twin mode)
    character_name VARCHAR(100),
    character_style TEXT,
    character_bio TEXT,
    character_examples JSONB DEFAULT '[]',

    -- Status
    is_completed BOOLEAN DEFAULT false,
    is_active BOOLEAN DEFAULT false,  -- Is the user-bot running?
    completed_at TIMESTAMPTZ,

    -- Metadata
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Indexes
CREATE INDEX IF NOT EXISTS idx_userbot_setup_step
    ON userbot_setup_progress(current_step);
CREATE INDEX IF NOT EXISTS idx_userbot_setup_completed
    ON userbot_setup_progress(is_completed);
CREATE INDEX IF NOT EXISTS idx_userbot_setup_active
    ON userbot_setup_progress(is_active) WHERE is_active = true;
CREATE INDEX IF NOT EXISTS idx_userbot_setup_session
    ON userbot_setup_progress(bridge_session_id) WHERE bridge_session_id IS NOT NULL;

-- Auto-update trigger for updated_at
CREATE OR REPLACE FUNCTION update_userbot_setup_updated_at()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trigger_userbot_setup_updated ON userbot_setup_progress;
CREATE TRIGGER trigger_userbot_setup_updated
    BEFORE UPDATE ON userbot_setup_progress
    FOR EACH ROW EXECUTE FUNCTION update_userbot_setup_updated_at();

-- Comment
COMMENT ON TABLE userbot_setup_progress IS 'Stores wizard progress for User-Bot Setup. Each user can have one setup in progress.';
