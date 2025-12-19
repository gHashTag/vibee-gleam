-- Agent States Table
-- Stores persistent state for each agent

CREATE TABLE IF NOT EXISTS agent_states (
    agent_id VARCHAR(255) PRIMARY KEY,
    state JSONB NOT NULL DEFAULT '{}',
    config JSONB NOT NULL,
    history JSONB NOT NULL DEFAULT '[]',
    last_updated TIMESTAMP DEFAULT NOW()
);

-- Index for faster lookups
CREATE INDEX IF NOT EXISTS idx_agent_states_last_updated ON agent_states(last_updated);

-- Agent metrics table for monitoring
CREATE TABLE IF NOT EXISTS agent_metrics (
    id SERIAL PRIMARY KEY,
    agent_id VARCHAR(255) NOT NULL,
    metric_type VARCHAR(50) NOT NULL,
    value NUMERIC NOT NULL,
    metadata JSONB DEFAULT '{}',
    recorded_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_agent_metrics_agent_id ON agent_metrics(agent_id);
CREATE INDEX IF NOT EXISTS idx_agent_metrics_recorded_at ON agent_metrics(recorded_at);

-- Agent errors table for debugging
CREATE TABLE IF NOT EXISTS agent_errors (
    id SERIAL PRIMARY KEY,
    agent_id VARCHAR(255) NOT NULL,
    error_type VARCHAR(100) NOT NULL,
    error_message TEXT NOT NULL,
    stack_trace TEXT,
    context JSONB DEFAULT '{}',
    occurred_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_agent_errors_agent_id ON agent_errors(agent_id);
CREATE INDEX IF NOT EXISTS idx_agent_errors_occurred_at ON agent_errors(occurred_at);
