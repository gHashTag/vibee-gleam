-- Migration: 006_sales_funnel.sql
-- Sales Funnel System - Products, Subscriptions, Leads, Quiz
-- VIBEE Digital Clone pricing tiers

-- =============================================================================
-- Products (VIBEE tiers: Junior, Middle, Senior)
-- =============================================================================

CREATE TABLE IF NOT EXISTS products (
  id SERIAL PRIMARY KEY,
  code VARCHAR(50) UNIQUE NOT NULL,
  name_ru VARCHAR(200) NOT NULL,
  name_en VARCHAR(200),
  description_ru TEXT,
  description_en TEXT,
  price_usd_cents INT NOT NULL,
  price_rub_cents INT,
  billing_period VARCHAR(20) DEFAULT 'monthly',  -- monthly, yearly, one_time
  generation_limit INT,  -- NULL = unlimited
  features JSONB DEFAULT '[]',
  is_active BOOLEAN DEFAULT TRUE,
  sort_order INT DEFAULT 0,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Insert default VIBEE tiers
INSERT INTO products (code, name_ru, name_en, price_usd_cents, price_rub_cents, billing_period, generation_limit, features, sort_order) VALUES
('junior', 'VIBEE Junior', 'VIBEE Junior', 9900, 990000, 'monthly', 100,
 '["basic_responses", "telegram_bot", "email_support", "1_persona"]', 1),
('middle', 'VIBEE Middle', 'VIBEE Middle', 29900, 2990000, 'monthly', 500,
 '["custom_persona", "crm_integration", "analytics", "priority_support", "3_personas", "voice_clone"]', 2),
('senior', 'VIBEE Senior', 'VIBEE Senior', 99900, 9990000, 'monthly', NULL,
 '["unlimited_generations", "multichannel", "api_access", "dedicated_support", "unlimited_personas", "white_label", "custom_training"]', 3)
ON CONFLICT (code) DO NOTHING;

-- =============================================================================
-- Subscriptions
-- =============================================================================

CREATE TABLE IF NOT EXISTS subscriptions (
  id SERIAL PRIMARY KEY,
  telegram_id BIGINT NOT NULL,
  product_id INT REFERENCES products(id),
  status VARCHAR(20) DEFAULT 'pending',  -- pending, active, cancelled, expired, paused
  payment_method VARCHAR(30),  -- robokassa, ton, stars, crypto
  payment_id VARCHAR(255),  -- External payment reference
  current_period_start TIMESTAMPTZ,
  current_period_end TIMESTAMPTZ,
  generations_used INT DEFAULT 0,
  cancel_at_period_end BOOLEAN DEFAULT FALSE,
  cancelled_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_subscriptions_telegram_id ON subscriptions(telegram_id);
CREATE INDEX IF NOT EXISTS idx_subscriptions_status ON subscriptions(status);

-- =============================================================================
-- Leads (persistent, replacing in-memory lead_logger)
-- =============================================================================

CREATE TABLE IF NOT EXISTS leads (
  id SERIAL PRIMARY KEY,
  telegram_user_id BIGINT NOT NULL UNIQUE,
  username VARCHAR(100),
  first_name VARCHAR(100),
  last_name VARCHAR(100),
  phone VARCHAR(50),
  email VARCHAR(255),
  first_message TEXT,
  intent VARCHAR(30),  -- purchase, question, support, partnership
  priority VARCHAR(20) DEFAULT 'medium',  -- low, medium, high, urgent
  status VARCHAR(30) DEFAULT 'new',  -- new, contacted, qualified, proposal_sent, negotiation, won, lost
  funnel_stage VARCHAR(50) DEFAULT 'awareness',  -- awareness, interest, consideration, intent, evaluation, purchase
  source VARCHAR(50),  -- telegram, instagram, website, referral
  utm_source VARCHAR(100),
  utm_medium VARCHAR(100),
  utm_campaign VARCHAR(100),
  quiz_score INT,
  recommended_product_id INT REFERENCES products(id),
  assigned_to VARCHAR(100),
  notes TEXT,
  last_contact_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_leads_telegram_user_id ON leads(telegram_user_id);
CREATE INDEX IF NOT EXISTS idx_leads_status ON leads(status);
CREATE INDEX IF NOT EXISTS idx_leads_funnel_stage ON leads(funnel_stage);
CREATE INDEX IF NOT EXISTS idx_leads_priority ON leads(priority);

-- =============================================================================
-- Quiz Responses
-- =============================================================================

CREATE TABLE IF NOT EXISTS quiz_responses (
  id SERIAL PRIMARY KEY,
  telegram_id BIGINT NOT NULL,
  lead_id INT REFERENCES leads(id),
  quiz_type VARCHAR(50) DEFAULT 'qualification',  -- qualification, onboarding, feedback
  responses JSONB NOT NULL,
  score INT,
  recommended_product_id INT REFERENCES products(id),
  completed_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_quiz_responses_telegram_id ON quiz_responses(telegram_id);
CREATE INDEX IF NOT EXISTS idx_quiz_responses_lead_id ON quiz_responses(lead_id);

-- =============================================================================
-- Usage Tracking (for paywall)
-- =============================================================================

CREATE TABLE IF NOT EXISTS usage_logs (
  id SERIAL PRIMARY KEY,
  telegram_id BIGINT NOT NULL,
  subscription_id INT REFERENCES subscriptions(id),
  action_type VARCHAR(50) NOT NULL,  -- generation, api_call, export, etc.
  action_details JSONB,
  tokens_used INT DEFAULT 0,
  cost_cents INT DEFAULT 0,
  created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_usage_logs_telegram_id ON usage_logs(telegram_id);
CREATE INDEX IF NOT EXISTS idx_usage_logs_subscription_id ON usage_logs(subscription_id);
CREATE INDEX IF NOT EXISTS idx_usage_logs_created_at ON usage_logs(created_at);

-- =============================================================================
-- Proposals (auto-generated КП)
-- =============================================================================

CREATE TABLE IF NOT EXISTS proposals (
  id SERIAL PRIMARY KEY,
  lead_id INT REFERENCES leads(id),
  product_id INT REFERENCES products(id),
  title VARCHAR(255) NOT NULL,
  content TEXT NOT NULL,
  price_usd_cents INT,
  discount_percent INT DEFAULT 0,
  valid_until TIMESTAMPTZ,
  status VARCHAR(20) DEFAULT 'draft',  -- draft, sent, viewed, accepted, rejected, expired
  sent_at TIMESTAMPTZ,
  viewed_at TIMESTAMPTZ,
  responded_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_proposals_lead_id ON proposals(lead_id);
CREATE INDEX IF NOT EXISTS idx_proposals_status ON proposals(status);

-- =============================================================================
-- Triggers for updated_at
-- =============================================================================

CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language 'plpgsql';

DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = 'update_products_updated_at') THEN
    CREATE TRIGGER update_products_updated_at BEFORE UPDATE ON products
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();
  END IF;

  IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = 'update_subscriptions_updated_at') THEN
    CREATE TRIGGER update_subscriptions_updated_at BEFORE UPDATE ON subscriptions
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();
  END IF;

  IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = 'update_leads_updated_at') THEN
    CREATE TRIGGER update_leads_updated_at BEFORE UPDATE ON leads
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();
  END IF;
END $$;
