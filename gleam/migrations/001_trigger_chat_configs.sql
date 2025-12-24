-- Migration: Create trigger_chat_configs and trigger_words tables
-- Run via Neon MCP or psql

-- =============================================================================
-- Table: trigger_chat_configs
-- Stores chat configuration for trigger-based responses
-- =============================================================================
CREATE TABLE IF NOT EXISTS trigger_chat_configs (
  id SERIAL PRIMARY KEY,
  chat_id VARCHAR(50) NOT NULL UNIQUE,
  chat_name VARCHAR(255) NOT NULL,
  chat_type VARCHAR(50) DEFAULT 'group',
  is_active BOOLEAN DEFAULT true,
  can_write BOOLEAN DEFAULT true,
  response_probability FLOAT DEFAULT 0.0,
  forward_chat_id VARCHAR(50),
  allow_images BOOLEAN DEFAULT false,
  response_template TEXT,
  expected_response_pattern TEXT,
  expected_forward_pattern TEXT,
  proactive_enabled BOOLEAN DEFAULT false,
  forward_only BOOLEAN DEFAULT false,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);

-- =============================================================================
-- Table: trigger_words
-- Stores trigger words/phrases for detection
-- =============================================================================
CREATE TABLE IF NOT EXISTS trigger_words (
  id SERIAL PRIMARY KEY,
  word VARCHAR(255) NOT NULL,
  category VARCHAR(100),
  is_active BOOLEAN DEFAULT true,
  created_at TIMESTAMP DEFAULT NOW()
);

-- =============================================================================
-- Table: chat_trigger_words (junction table)
-- Links chats to their trigger words
-- =============================================================================
CREATE TABLE IF NOT EXISTS chat_trigger_words (
  chat_config_id INTEGER REFERENCES trigger_chat_configs(id) ON DELETE CASCADE,
  trigger_word_id INTEGER REFERENCES trigger_words(id) ON DELETE CASCADE,
  PRIMARY KEY (chat_config_id, trigger_word_id)
);

-- =============================================================================
-- Indexes
-- =============================================================================
CREATE INDEX IF NOT EXISTS idx_trigger_chat_configs_chat_id ON trigger_chat_configs(chat_id);
CREATE INDEX IF NOT EXISTS idx_trigger_chat_configs_is_active ON trigger_chat_configs(is_active);
CREATE INDEX IF NOT EXISTS idx_trigger_words_category ON trigger_words(category);
CREATE INDEX IF NOT EXISTS idx_trigger_words_is_active ON trigger_words(is_active);

-- =============================================================================
-- Insert existing data
-- =============================================================================

-- Chat configs
INSERT INTO trigger_chat_configs
(chat_id, chat_name, chat_type, is_active, can_write, response_probability,
 forward_chat_id, allow_images, response_template, expected_response_pattern,
 expected_forward_pattern, proactive_enabled, forward_only)
VALUES
('-5082217642', 'Aimly.io dev', 'group', true, true, 0.0,
 '-1002737186844', false, 'Напиши мне в личку, помогу разобраться',
 'личку|напиши|помогу|разобраться', 'ЛИД|Клиент|крипт', false, true),
('-1002298297094', 'Crypto Group 2', 'group', true, true, 0.0,
 '-1002737186844', false, 'Напиши мне в личку, помогу разобраться',
 'личку|напиши|помогу|разобраться', 'ЛИД|Клиент|крипт', false, false)
ON CONFLICT (chat_id) DO UPDATE SET
  chat_name = EXCLUDED.chat_name,
  forward_only = EXCLUDED.forward_only,
  updated_at = NOW();

-- Trigger words (crypto category)
INSERT INTO trigger_words (word, category) VALUES
-- Покупка/продажа
('куплю крипту', 'crypto_buy'),
('купить крипту', 'crypto_buy'),
('куплю крипты', 'crypto_buy'),
('купить крипты', 'crypto_buy'),
('где купить', 'crypto_buy'),
('где куплю', 'crypto_buy'),
('подскажите где купить', 'crypto_buy'),
('как купить', 'crypto_buy'),
('хочу купить', 'crypto_buy'),
('хочу куплю', 'crypto_buy'),
('я бы купил', 'crypto_buy'),
('я бы крипты купил', 'crypto_buy'),
('крипту купить', 'crypto_buy'),
('крипты купить', 'crypto_buy'),
('куплю биткоин', 'crypto_buy'),
('купить биткоин', 'crypto_buy'),
-- Обмен
('обменять крипту', 'crypto_exchange'),
('обмен крипты', 'crypto_exchange'),
('обменять на', 'crypto_exchange'),
('обменник', 'crypto_exchange'),
('обмен', 'crypto_exchange'),
('п2п', 'crypto_exchange'),
('p2p', 'crypto_exchange'),
('обменять биткоин', 'crypto_exchange'),
-- Криптовалюты
('usdt', 'crypto_coins'),
('баты', 'crypto_coins'),
('купить usdt', 'crypto_coins'),
('куплю usdt', 'crypto_coins'),
('биткоин', 'crypto_coins'),
('эфир', 'crypto_coins'),
('токены', 'crypto_coins'),
('монеты', 'crypto_coins'),
('криптовалюту', 'crypto_coins'),
('валюту', 'crypto_coins'),
('биткоин на', 'crypto_coins'),
('на биткоин', 'crypto_coins'),
('крипта на', 'crypto_coins'),
('на крипту', 'crypto_coins'),
('крипты на', 'crypto_coins'),
('на крипты', 'crypto_coins'),
-- Вопросы
('где взять', 'crypto_question'),
('где достать', 'crypto_question'),
('пацаны где', 'crypto_question'),
('ребята где', 'crypto_question'),
('где можно купить', 'crypto_question'),
('где можно обменять', 'crypto_question')
ON CONFLICT DO NOTHING;

-- Link all crypto trigger words to both chats
INSERT INTO chat_trigger_words (chat_config_id, trigger_word_id)
SELECT c.id, w.id
FROM trigger_chat_configs c
CROSS JOIN trigger_words w
WHERE w.category LIKE 'crypto_%'
ON CONFLICT DO NOTHING;
