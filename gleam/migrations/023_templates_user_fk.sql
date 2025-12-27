-- Migration: 023_templates_user_fk
-- Description: Add foreign key from public_templates to users

-- Add user_id column (nullable for existing records without user)
ALTER TABLE public_templates
  ADD COLUMN IF NOT EXISTS user_id INT REFERENCES users(id) ON DELETE SET NULL;

-- Backfill user_id from telegram_id where users exist
UPDATE public_templates pt
SET user_id = u.id
FROM users u
WHERE pt.telegram_id = u.telegram_id
  AND pt.user_id IS NULL;

-- Index for user's templates
CREATE INDEX IF NOT EXISTS idx_public_templates_user_id
  ON public_templates(user_id, created_at DESC)
  WHERE user_id IS NOT NULL;

-- Function to update user template count
CREATE OR REPLACE FUNCTION update_user_template_count()
RETURNS TRIGGER AS $$
BEGIN
  IF TG_OP = 'INSERT' AND NEW.user_id IS NOT NULL THEN
    UPDATE users SET templates_count = templates_count + 1 WHERE id = NEW.user_id;
  ELSIF TG_OP = 'DELETE' AND OLD.user_id IS NOT NULL THEN
    UPDATE users SET templates_count = GREATEST(0, templates_count - 1) WHERE id = OLD.user_id;
  ELSIF TG_OP = 'UPDATE' THEN
    -- Handle user_id change
    IF OLD.user_id IS NOT NULL AND (NEW.user_id IS NULL OR OLD.user_id != NEW.user_id) THEN
      UPDATE users SET templates_count = GREATEST(0, templates_count - 1) WHERE id = OLD.user_id;
    END IF;
    IF NEW.user_id IS NOT NULL AND (OLD.user_id IS NULL OR OLD.user_id != NEW.user_id) THEN
      UPDATE users SET templates_count = templates_count + 1 WHERE id = NEW.user_id;
    END IF;
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- Trigger for template count updates
DROP TRIGGER IF EXISTS trigger_update_user_template_count ON public_templates;
CREATE TRIGGER trigger_update_user_template_count
  AFTER INSERT OR UPDATE OR DELETE ON public_templates
  FOR EACH ROW EXECUTE FUNCTION update_user_template_count();

-- Function to update user total views/likes when template stats change
CREATE OR REPLACE FUNCTION update_user_stats_from_template()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.user_id IS NOT NULL THEN
    -- Recalculate total views and likes for the user
    UPDATE users SET
      total_views = COALESCE((
        SELECT SUM(views_count) FROM public_templates WHERE user_id = NEW.user_id
      ), 0),
      total_likes = COALESCE((
        SELECT SUM(likes_count) FROM public_templates WHERE user_id = NEW.user_id
      ), 0)
    WHERE id = NEW.user_id;
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- Trigger for user stats updates
DROP TRIGGER IF EXISTS trigger_update_user_stats ON public_templates;
CREATE TRIGGER trigger_update_user_stats
  AFTER UPDATE OF views_count, likes_count ON public_templates
  FOR EACH ROW EXECUTE FUNCTION update_user_stats_from_template();

COMMENT ON COLUMN public_templates.user_id IS 'Reference to users table (nullable for legacy templates)';
