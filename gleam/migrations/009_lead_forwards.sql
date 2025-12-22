-- 009: Lead Forwards Tracking
-- Tracks all lead forwarding events for metrics and observability

CREATE TABLE IF NOT EXISTS lead_forwards (
  id SERIAL PRIMARY KEY,
  lead_id INTEGER REFERENCES leads(id) ON DELETE SET NULL,
  source_chat_id BIGINT NOT NULL,
  target_chat_id BIGINT NOT NULL,
  message_id INTEGER,
  user_id BIGINT NOT NULL,
  user_name TEXT,
  username TEXT,
  status TEXT NOT NULL DEFAULT 'pending', -- pending, forwarded, failed, deduplicated, rate_limited
  quality_score INTEGER DEFAULT 0,
  intent TEXT,
  urgency TEXT,
  forwarded_at TIMESTAMPTZ DEFAULT NOW(),
  latency_ms INTEGER,
  error_message TEXT,
  created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Indexes for efficient querying
CREATE INDEX IF NOT EXISTS idx_lead_forwards_source ON lead_forwards(source_chat_id);
CREATE INDEX IF NOT EXISTS idx_lead_forwards_target ON lead_forwards(target_chat_id);
CREATE INDEX IF NOT EXISTS idx_lead_forwards_user ON lead_forwards(user_id);
CREATE INDEX IF NOT EXISTS idx_lead_forwards_status ON lead_forwards(status);
CREATE INDEX IF NOT EXISTS idx_lead_forwards_date ON lead_forwards(forwarded_at);

-- Index for deduplication queries
CREATE INDEX IF NOT EXISTS idx_lead_forwards_user_recent ON lead_forwards(user_id, forwarded_at DESC);

-- Comment
COMMENT ON TABLE lead_forwards IS 'Tracks all lead forwarding events for metrics and observability';
