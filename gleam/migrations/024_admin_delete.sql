-- Migration: Add admin role and soft delete for templates
-- Created: 2025-12-27

-- Add admin flag to users
ALTER TABLE users ADD COLUMN IF NOT EXISTS is_admin BOOLEAN DEFAULT false;

-- Set initial admin (vibee owner)
UPDATE users SET is_admin = true WHERE telegram_id = 144022504;

-- Add soft delete to public_templates
ALTER TABLE public_templates ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ;

-- Create index for filtering non-deleted templates
CREATE INDEX IF NOT EXISTS idx_public_templates_deleted_at
ON public_templates (deleted_at)
WHERE deleted_at IS NULL;

-- Create deletion audit log table
CREATE TABLE IF NOT EXISTS template_deletions (
  id SERIAL PRIMARY KEY,
  template_id INT NOT NULL,
  deleted_by_telegram_id BIGINT NOT NULL,
  reason VARCHAR(255),
  deleted_at TIMESTAMPTZ DEFAULT NOW()
);

-- Index for audit lookups
CREATE INDEX IF NOT EXISTS idx_template_deletions_template_id ON template_deletions (template_id);
CREATE INDEX IF NOT EXISTS idx_template_deletions_deleted_by ON template_deletions (deleted_by_telegram_id);
