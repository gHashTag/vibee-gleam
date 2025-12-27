-- Migration: 021_users
-- Description: User profiles with Telegram integration

CREATE TABLE IF NOT EXISTS users (
  id SERIAL PRIMARY KEY,
  telegram_id BIGINT UNIQUE NOT NULL,

  -- Profile info
  username VARCHAR(50) UNIQUE,
  display_name VARCHAR(100),
  bio TEXT,
  avatar_url VARCHAR(500),

  -- Social links (JSON array)
  social_links JSONB DEFAULT '[]',

  -- Profile settings
  is_public BOOLEAN DEFAULT true,

  -- Stats (denormalized for performance, updated via triggers)
  followers_count INT DEFAULT 0,
  following_count INT DEFAULT 0,
  templates_count INT DEFAULT 0,
  total_views INT DEFAULT 0,
  total_likes INT DEFAULT 0,

  -- Timestamps
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Index for username lookup (case-insensitive)
CREATE UNIQUE INDEX IF NOT EXISTS idx_users_username_lower
  ON users(LOWER(username));

-- Index for Telegram ID lookup
CREATE INDEX IF NOT EXISTS idx_users_telegram_id
  ON users(telegram_id);

-- Index for public users (discovery feed)
CREATE INDEX IF NOT EXISTS idx_users_public
  ON users(is_public, followers_count DESC)
  WHERE is_public = true;

-- Function to auto-generate username from telegram_id if null
CREATE OR REPLACE FUNCTION generate_username_from_telegram_id()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.username IS NULL THEN
    NEW.username := 'user_' || NEW.telegram_id::text;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Trigger to auto-generate username
DROP TRIGGER IF EXISTS trigger_generate_username ON users;
CREATE TRIGGER trigger_generate_username
  BEFORE INSERT ON users
  FOR EACH ROW EXECUTE FUNCTION generate_username_from_telegram_id();

-- Function to update updated_at timestamp
CREATE OR REPLACE FUNCTION update_users_updated_at()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Trigger to update updated_at on update
DROP TRIGGER IF EXISTS trigger_users_updated_at ON users;
CREATE TRIGGER trigger_users_updated_at
  BEFORE UPDATE ON users
  FOR EACH ROW EXECUTE FUNCTION update_users_updated_at();

COMMENT ON TABLE users IS 'User profiles with Telegram integration';
COMMENT ON COLUMN users.username IS 'URL-safe username for profile page (from Telegram or auto-generated)';
COMMENT ON COLUMN users.social_links IS 'Array of {platform, url, label} objects';
