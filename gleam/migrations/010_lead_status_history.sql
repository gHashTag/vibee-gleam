-- 010: Lead Status History
-- Tracks all status changes for leads analytics and lifecycle visualization

CREATE TABLE IF NOT EXISTS lead_status_history (
  id SERIAL PRIMARY KEY,
  lead_id INTEGER REFERENCES leads(id) ON DELETE CASCADE,
  telegram_user_id BIGINT NOT NULL,
  old_status TEXT,
  new_status TEXT NOT NULL,
  old_funnel_stage TEXT,
  new_funnel_stage TEXT,
  changed_by TEXT,  -- 'system', 'agent', or user identifier
  change_reason TEXT,  -- optional note
  created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Indexes for efficient querying
CREATE INDEX IF NOT EXISTS idx_lead_status_history_lead ON lead_status_history(lead_id);
CREATE INDEX IF NOT EXISTS idx_lead_status_history_user ON lead_status_history(telegram_user_id);
CREATE INDEX IF NOT EXISTS idx_lead_status_history_status ON lead_status_history(new_status);
CREATE INDEX IF NOT EXISTS idx_lead_status_history_date ON lead_status_history(created_at);

-- Index for lifecycle analytics
CREATE INDEX IF NOT EXISTS idx_lead_status_history_transition ON lead_status_history(old_status, new_status);

-- Comment
COMMENT ON TABLE lead_status_history IS 'Tracks all lead status transitions for lifecycle analytics';

-- Trigger to auto-log status changes
CREATE OR REPLACE FUNCTION log_lead_status_change()
RETURNS TRIGGER AS $$
BEGIN
  -- Only log if status or funnel_stage changed
  IF (OLD.status IS DISTINCT FROM NEW.status) OR (OLD.funnel_stage IS DISTINCT FROM NEW.funnel_stage) THEN
    INSERT INTO lead_status_history (
      lead_id,
      telegram_user_id,
      old_status,
      new_status,
      old_funnel_stage,
      new_funnel_stage,
      changed_by
    ) VALUES (
      NEW.id,
      NEW.telegram_user_id,
      OLD.status,
      NEW.status,
      OLD.funnel_stage,
      NEW.funnel_stage,
      'system'
    );
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create trigger on leads table
DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = 'lead_status_change_trigger') THEN
    CREATE TRIGGER lead_status_change_trigger
    AFTER UPDATE ON leads
    FOR EACH ROW
    EXECUTE FUNCTION log_lead_status_change();
  END IF;
END $$;
