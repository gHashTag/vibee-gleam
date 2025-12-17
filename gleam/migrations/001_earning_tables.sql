-- VIBEE P2P Earning Agent Tables Migration
-- Creates tables for automated P2P trading

-- Earning configurations
CREATE TABLE IF NOT EXISTS earning_configs (
  telegram_id BIGINT PRIMARY KEY,
  wallet TEXT NOT NULL,
  strategy VARCHAR(20) NOT NULL DEFAULT 'passive_fees',
  max_position_usdt DECIMAL(12,2) DEFAULT 1000,
  max_position_ton DECIMAL(12,4) DEFAULT 200,
  spread_percent DECIMAL(4,2) DEFAULT 2.0,
  min_order_size DECIMAL(12,2) DEFAULT 10,
  max_order_size DECIMAL(12,2) DEFAULT 500,
  daily_loss_limit DECIMAL(12,2) DEFAULT 50,
  stop_loss_percent DECIMAL(4,2) DEFAULT 5.0,
  enabled_crypto TEXT[] DEFAULT ARRAY['USDT', 'TON'],
  enabled_fiat TEXT[] DEFAULT ARRAY['THB', 'RUB'],
  enabled_methods TEXT[] DEFAULT ARRAY['promptpay', 'sberbank'],
  is_active BOOLEAN DEFAULT false,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW(),

  CONSTRAINT valid_strategy CHECK (
    strategy IN ('passive_fees', 'market_making', 'arbitrage', 'hybrid')
  )
);

-- Earning history (trade log)
CREATE TABLE IF NOT EXISTS earning_history (
  id SERIAL PRIMARY KEY,
  telegram_id BIGINT NOT NULL,
  trade_type VARCHAR(20) NOT NULL,
  order_id TEXT,
  volume_usdt DECIMAL(12,2) NOT NULL DEFAULT 0,
  profit_usdt DECIMAL(12,4) NOT NULL,
  fee_paid DECIMAL(12,4) DEFAULT 0,
  crypto VARCHAR(10) NOT NULL,
  fiat VARCHAR(10) NOT NULL,
  buy_price DECIMAL(16,6),
  sell_price DECIMAL(16,6),
  buy_source VARCHAR(30),
  sell_source VARCHAR(30),
  details JSONB DEFAULT '{}',
  executed_at TIMESTAMPTZ DEFAULT NOW(),

  CONSTRAINT valid_trade_type CHECK (
    trade_type IN ('fee', 'spread', 'arbitrage')
  )
);

CREATE INDEX idx_earning_history_telegram ON earning_history(telegram_id);
CREATE INDEX idx_earning_history_type ON earning_history(trade_type);
CREATE INDEX idx_earning_history_executed ON earning_history(executed_at DESC);

-- Earning stats (aggregated per user)
CREATE TABLE IF NOT EXISTS earning_stats (
  telegram_id BIGINT PRIMARY KEY,
  total_trades INT DEFAULT 0,
  successful_trades INT DEFAULT 0,
  failed_trades INT DEFAULT 0,
  total_volume_usdt DECIMAL(16,2) DEFAULT 0,
  total_fees_earned DECIMAL(16,4) DEFAULT 0,
  total_spread_profit DECIMAL(16,4) DEFAULT 0,
  total_arbitrage_profit DECIMAL(16,4) DEFAULT 0,
  today_profit DECIMAL(12,4) DEFAULT 0,
  today_date DATE DEFAULT CURRENT_DATE,
  this_week_profit DECIMAL(12,4) DEFAULT 0,
  week_start DATE,
  this_month_profit DECIMAL(12,4) DEFAULT 0,
  month_start DATE,
  best_trade_profit DECIMAL(12,4) DEFAULT 0,
  avg_trade_profit DECIMAL(12,4) DEFAULT 0,
  updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Arbitrage opportunities log
CREATE TABLE IF NOT EXISTS arbitrage_opportunities (
  id TEXT PRIMARY KEY,
  crypto VARCHAR(10) NOT NULL,
  fiat VARCHAR(10) NOT NULL,
  buy_source VARCHAR(30) NOT NULL,
  buy_price DECIMAL(16,6) NOT NULL,
  buy_volume DECIMAL(16,2),
  sell_source VARCHAR(30) NOT NULL,
  sell_price DECIMAL(16,6) NOT NULL,
  sell_volume DECIMAL(16,2),
  spread_percent DECIMAL(8,4) NOT NULL,
  potential_profit_percent DECIMAL(8,4),
  max_trade_size DECIMAL(16,2),
  estimated_profit DECIMAL(12,4),
  risk_score INT DEFAULT 50,
  was_executed BOOLEAN DEFAULT false,
  executed_by BIGINT,
  execution_profit DECIMAL(12,4),
  detected_at TIMESTAMPTZ DEFAULT NOW(),
  expires_at TIMESTAMPTZ,
  executed_at TIMESTAMPTZ
);

CREATE INDEX idx_arb_opp_crypto_fiat ON arbitrage_opportunities(crypto, fiat);
CREATE INDEX idx_arb_opp_spread ON arbitrage_opportunities(spread_percent DESC);
CREATE INDEX idx_arb_opp_detected ON arbitrage_opportunities(detected_at DESC);

-- Active orders (market making)
CREATE TABLE IF NOT EXISTS mm_active_orders (
  order_id TEXT PRIMARY KEY,
  telegram_id BIGINT NOT NULL,
  side VARCHAR(4) NOT NULL, -- 'buy' or 'sell'
  crypto VARCHAR(10) NOT NULL,
  crypto_amount DECIMAL(16,8) NOT NULL,
  fiat VARCHAR(10) NOT NULL,
  fiat_amount DECIMAL(16,2) NOT NULL,
  rate DECIMAL(16,6) NOT NULL,
  payment_method VARCHAR(30) NOT NULL,
  status VARCHAR(20) DEFAULT 'open',
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW(),
  expires_at TIMESTAMPTZ,

  CONSTRAINT valid_side CHECK (side IN ('buy', 'sell')),
  CONSTRAINT valid_status CHECK (
    status IN ('open', 'locked', 'completed', 'cancelled', 'expired')
  )
);

CREATE INDEX idx_mm_orders_telegram ON mm_active_orders(telegram_id);
CREATE INDEX idx_mm_orders_status ON mm_active_orders(status);
CREATE INDEX idx_mm_orders_expires ON mm_active_orders(expires_at);

-- Function to update earning stats
CREATE OR REPLACE FUNCTION update_earning_stats()
RETURNS TRIGGER AS $$
BEGIN
  INSERT INTO earning_stats (telegram_id, total_trades, successful_trades, total_volume_usdt)
  VALUES (NEW.telegram_id, 1, 1, NEW.volume_usdt)
  ON CONFLICT (telegram_id) DO UPDATE SET
    total_trades = earning_stats.total_trades + 1,
    successful_trades = earning_stats.successful_trades + 1,
    total_volume_usdt = earning_stats.total_volume_usdt + NEW.volume_usdt,
    total_fees_earned = CASE WHEN NEW.trade_type = 'fee'
                        THEN earning_stats.total_fees_earned + NEW.profit_usdt
                        ELSE earning_stats.total_fees_earned END,
    total_spread_profit = CASE WHEN NEW.trade_type = 'spread'
                          THEN earning_stats.total_spread_profit + NEW.profit_usdt
                          ELSE earning_stats.total_spread_profit END,
    total_arbitrage_profit = CASE WHEN NEW.trade_type = 'arbitrage'
                             THEN earning_stats.total_arbitrage_profit + NEW.profit_usdt
                             ELSE earning_stats.total_arbitrage_profit END,
    today_profit = CASE WHEN earning_stats.today_date = CURRENT_DATE
                   THEN earning_stats.today_profit + NEW.profit_usdt
                   ELSE NEW.profit_usdt END,
    today_date = CURRENT_DATE,
    best_trade_profit = GREATEST(earning_stats.best_trade_profit, NEW.profit_usdt),
    avg_trade_profit = (earning_stats.total_fees_earned + earning_stats.total_spread_profit +
                        earning_stats.total_arbitrage_profit + NEW.profit_usdt) /
                       (earning_stats.total_trades + 1),
    updated_at = NOW();

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Trigger to auto-update stats on new trade
DROP TRIGGER IF EXISTS earning_history_stats_trigger ON earning_history;
CREATE TRIGGER earning_history_stats_trigger
  AFTER INSERT ON earning_history
  FOR EACH ROW
  EXECUTE FUNCTION update_earning_stats();

-- Comments
COMMENT ON TABLE earning_configs IS 'User configurations for automated P2P earning';
COMMENT ON TABLE earning_history IS 'History of all earning trades (fees, spread, arbitrage)';
COMMENT ON TABLE earning_stats IS 'Aggregated earning statistics per user';
COMMENT ON TABLE arbitrage_opportunities IS 'Detected arbitrage opportunities log';
COMMENT ON TABLE mm_active_orders IS 'Active market making orders';
