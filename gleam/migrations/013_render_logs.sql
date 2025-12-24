-- Migration: 013_render_logs.sql
-- Purpose: Track video renders for freemium model (3 free renders)

-- Render logs table
CREATE TABLE IF NOT EXISTS render_logs (
  id SERIAL PRIMARY KEY,
  telegram_id BIGINT NOT NULL,
  composition_id VARCHAR(100) DEFAULT 'SplitTalkingHead',
  duration_seconds INT,
  status VARCHAR(20) DEFAULT 'completed', -- pending, completed, failed
  created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Index for fast user lookups
CREATE INDEX IF NOT EXISTS idx_render_logs_telegram_id ON render_logs(telegram_id);
CREATE INDEX IF NOT EXISTS idx_render_logs_created_at ON render_logs(created_at);

-- Function to count renders for a user
CREATE OR REPLACE FUNCTION get_render_count(p_telegram_id BIGINT)
RETURNS INT AS $$
  SELECT COUNT(*)::INT FROM render_logs WHERE telegram_id = p_telegram_id;
$$ LANGUAGE SQL STABLE;

-- Function to check if user has free renders remaining
CREATE OR REPLACE FUNCTION has_free_renders(p_telegram_id BIGINT, p_free_limit INT DEFAULT 3)
RETURNS BOOLEAN AS $$
  SELECT COUNT(*) < p_free_limit FROM render_logs WHERE telegram_id = p_telegram_id;
$$ LANGUAGE SQL STABLE;

COMMENT ON TABLE render_logs IS 'Tracks video renders per user for freemium quota';
COMMENT ON COLUMN render_logs.telegram_id IS 'Telegram user ID';
COMMENT ON COLUMN render_logs.composition_id IS 'Remotion composition used';
COMMENT ON COLUMN render_logs.duration_seconds IS 'Video duration in seconds';
COMMENT ON COLUMN render_logs.status IS 'Render status: pending, completed, failed';
