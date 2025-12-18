-- User Sessions Table
-- Stores Telegram session credentials per user/client

CREATE TABLE IF NOT EXISTS user_sessions (
    id SERIAL PRIMARY KEY,
    
    -- User identification
    user_id VARCHAR(255) NOT NULL UNIQUE,
    username VARCHAR(255),
    phone VARCHAR(50),
    
    -- Telegram credentials (encrypted)
    session_id VARCHAR(255) NOT NULL UNIQUE,
    api_id INTEGER NOT NULL,
    api_hash_encrypted TEXT NOT NULL,
    
    -- Session status
    is_active BOOLEAN DEFAULT true,
    is_authorized BOOLEAN DEFAULT false,
    last_seen_at TIMESTAMP,
    
    -- Configuration
    digital_twin_enabled BOOLEAN DEFAULT false,
    auto_reply_enabled BOOLEAN DEFAULT true,
    target_chats JSONB DEFAULT '[]'::jsonb,
    
    -- Metadata
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW()
);

-- Index for fast lookups
CREATE INDEX idx_user_sessions_user_id ON user_sessions(user_id);
CREATE INDEX idx_user_sessions_session_id ON user_sessions(session_id);
CREATE INDEX idx_user_sessions_active ON user_sessions(is_active) WHERE is_active = true;

-- Encryption key storage (single row table)
CREATE TABLE IF NOT EXISTS encryption_keys (
    id INTEGER PRIMARY KEY DEFAULT 1,
    master_key TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT NOW(),
    CONSTRAINT single_row CHECK (id = 1)
);

-- Session activity log
CREATE TABLE IF NOT EXISTS session_activity (
    id SERIAL PRIMARY KEY,
    session_id VARCHAR(255) NOT NULL REFERENCES user_sessions(session_id),
    activity_type VARCHAR(50) NOT NULL, -- 'login', 'logout', 'message_sent', 'error'
    details JSONB,
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX idx_session_activity_session_id ON session_activity(session_id);
CREATE INDEX idx_session_activity_created_at ON session_activity(created_at DESC);

-- Update timestamp trigger
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language 'plpgsql';

CREATE TRIGGER update_user_sessions_updated_at BEFORE UPDATE ON user_sessions
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- Insert default encryption key (should be replaced in production)
INSERT INTO encryption_keys (master_key) 
VALUES ('CHANGE_ME_IN_PRODUCTION_USE_32_BYTE_KEY')
ON CONFLICT (id) DO NOTHING;

COMMENT ON TABLE user_sessions IS 'Stores encrypted Telegram session credentials per user';
COMMENT ON COLUMN user_sessions.api_hash_encrypted IS 'AES-256 encrypted API hash';
COMMENT ON TABLE encryption_keys IS 'Master encryption key for session credentials';
