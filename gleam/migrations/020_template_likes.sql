-- Migration: 020_template_likes
-- Description: Likes for public templates

CREATE TABLE IF NOT EXISTS template_likes (
  id SERIAL PRIMARY KEY,
  template_id INT NOT NULL REFERENCES public_templates(id) ON DELETE CASCADE,
  user_id BIGINT NOT NULL,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  UNIQUE(template_id, user_id)
);

-- Index for counting likes per template
CREATE INDEX IF NOT EXISTS idx_template_likes_template
  ON template_likes(template_id);

-- Index for user's likes
CREATE INDEX IF NOT EXISTS idx_template_likes_user
  ON template_likes(user_id);

-- Function to update likes_count on public_templates
CREATE OR REPLACE FUNCTION update_template_likes_count()
RETURNS TRIGGER AS $$
BEGIN
  IF TG_OP = 'INSERT' THEN
    UPDATE public_templates SET likes_count = likes_count + 1 WHERE id = NEW.template_id;
  ELSIF TG_OP = 'DELETE' THEN
    UPDATE public_templates SET likes_count = likes_count - 1 WHERE id = OLD.template_id;
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- Trigger to auto-update likes_count
DROP TRIGGER IF EXISTS trigger_update_likes_count ON template_likes;
CREATE TRIGGER trigger_update_likes_count
AFTER INSERT OR DELETE ON template_likes
FOR EACH ROW EXECUTE FUNCTION update_template_likes_count();

COMMENT ON TABLE template_likes IS 'User likes on public templates';
