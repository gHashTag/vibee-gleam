-- Migration: 019_public_templates
-- Description: Public templates feed for social sharing

CREATE TABLE IF NOT EXISTS public_templates (
  id SERIAL PRIMARY KEY,
  telegram_id BIGINT NOT NULL,
  creator_name VARCHAR(255),
  creator_avatar VARCHAR(500),

  -- Template info
  name VARCHAR(255) NOT NULL,
  description TEXT,
  thumbnail_url TEXT,
  video_url TEXT NOT NULL,

  -- Settings for "Use Template" (all editor settings as JSON)
  template_settings JSONB NOT NULL DEFAULT '{}',
  -- Assets used in template (videos, images, audio)
  assets JSONB DEFAULT '[]',
  -- Timeline tracks
  tracks JSONB DEFAULT '[]',

  -- Stats
  likes_count INT DEFAULT 0,
  views_count INT DEFAULT 0,
  uses_count INT DEFAULT 0,

  -- Flags
  is_featured BOOLEAN DEFAULT false,
  is_public BOOLEAN DEFAULT true,

  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Index for feed (newest first)
CREATE INDEX IF NOT EXISTS idx_public_templates_feed
  ON public_templates(is_public, created_at DESC);

-- Index for popular sorting
CREATE INDEX IF NOT EXISTS idx_public_templates_popular
  ON public_templates(is_public, likes_count DESC);

-- Index for user's templates
CREATE INDEX IF NOT EXISTS idx_public_templates_user
  ON public_templates(telegram_id);

-- Comments
COMMENT ON TABLE public_templates IS 'Public templates shared by users for reuse';
COMMENT ON COLUMN public_templates.template_settings IS 'Editor settings JSON (effects, avatar position, captions style, etc)';
COMMENT ON COLUMN public_templates.assets IS 'Array of assets used in template [{id, url, type, name}]';
COMMENT ON COLUMN public_templates.tracks IS 'Timeline tracks with items [{id, type, items}]';
