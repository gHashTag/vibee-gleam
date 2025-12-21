-- Migration: Dynamic Configuration System
-- Централизованное хранение конфигурации с hot-reload
-- Заменяет захардкоженные данные в trigger_chats.gleam и других файлах

-- ============================================
-- 1. Глобальная конфигурация
-- ============================================
CREATE TABLE IF NOT EXISTS global_config (
    key VARCHAR(100) PRIMARY KEY,
    value TEXT NOT NULL,
    value_type VARCHAR(20) DEFAULT 'string', -- string, int, float, bool, json
    description TEXT,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Начальные значения
INSERT INTO global_config (key, value, value_type, description) VALUES
    ('owner_id', '144022504', 'int', 'Telegram ID владельца бота'),
    ('bridge_url', 'https://vibee-telegram-bridge.fly.dev', 'string', 'URL Telegram bridge'),
    ('llm_model', 'anthropic/claude-sonnet-4-20250514', 'string', 'Модель LLM по умолчанию'),
    ('digital_twin_enabled', 'true', 'bool', 'Включен ли Digital Twin'),
    ('default_response_probability', '0.0', 'float', 'Вероятность случайного ответа по умолчанию')
ON CONFLICT (key) DO NOTHING;

-- ============================================
-- 2. Trigger Chat Configs (замена trigger_chats.gleam)
-- ============================================
CREATE TABLE IF NOT EXISTS trigger_chat_configs (
    chat_id BIGINT PRIMARY KEY,
    chat_name VARCHAR(255) NOT NULL,
    chat_type VARCHAR(20) DEFAULT 'group', -- group, supergroup, channel
    is_active BOOLEAN DEFAULT true,
    can_write BOOLEAN DEFAULT true,
    response_probability FLOAT DEFAULT 0.0, -- 0.0 = sniper mode
    custom_triggers JSONB DEFAULT '[]'::jsonb,
    forward_chat_id BIGINT, -- Куда пересылать диалоги (lead группа)
    allow_images BOOLEAN DEFAULT false,
    response_template TEXT,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Миграция существующих данных из trigger_chats.gleam
INSERT INTO trigger_chat_configs (
    chat_id, chat_name, chat_type, is_active, can_write,
    response_probability, custom_triggers, forward_chat_id,
    allow_images, response_template
) VALUES
    -- Aimly.io dev (Sniper Mode)
    (-1005082217642, 'Aimly.io dev', 'supergroup', true, true, 0.0,
     '["куплю крипту", "купить крипту", "куплю крипты", "купить крипты", "где купить", "где куплю", "подскажите где купить", "как купить", "хочу купить", "хочу куплю", "я бы купил", "я бы крипты купил", "крипту купить", "крипты купить", "куплю биткоин", "купить биткоин", "обменять крипту", "обмен крипты", "обменять на", "обменник", "обмен", "п2п", "p2p", "обменять биткоин", "usdt", "баты", "купить usdt", "куплю usdt", "биткоин", "эфир", "токены", "монеты", "криптовалюту", "валюту", "биткоин на", "на биткоин", "крипта на", "на крипту", "крипты на", "на крипты", "где взять", "где достать", "пацаны где", "ребята где", "где можно купить", "где можно обменять"]'::jsonb,
     2737186844, false,
     'Напиши мне в личку, помогу разобраться'),

    -- Crypto Group 2 (Sniper Mode)
    (-1002298297094, 'Crypto Group 2', 'supergroup', true, true, 0.0,
     '["куплю крипту", "купить крипту", "куплю крипты", "купить крипты", "где купить", "где куплю", "подскажите где купить", "как купить", "хочу купить", "хочу куплю", "я бы купил", "я бы крипты купил", "крипту купить", "крипты купить", "куплю биткоин", "купить биткоин", "обменять крипту", "обмен крипты", "обменять на", "обменник", "обмен", "п2п", "p2p", "обменять биткоин", "usdt", "баты", "купить usdt", "куплю usdt", "биткоин", "эфир", "токены", "монеты", "криптовалюту", "валюту", "биткоин на", "на биткоин", "крипта на", "на крипту", "крипты на", "на крипты", "где взять", "где достать", "пацаны где", "ребята где", "где можно купить", "где можно обменять"]'::jsonb,
     2737186844, false,
     'Напиши мне в личку, помогу разобраться')
ON CONFLICT (chat_id) DO NOTHING;

-- ============================================
-- 3. Предустановленные наборы триггеров
-- ============================================
CREATE TABLE IF NOT EXISTS trigger_presets (
    name VARCHAR(100) PRIMARY KEY,
    description TEXT,
    triggers JSONB NOT NULL DEFAULT '[]'::jsonb,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Crypto triggers preset
INSERT INTO trigger_presets (name, description, triggers) VALUES
    ('crypto_exchange', 'Триггеры для крипто-обменников',
     '["куплю крипту", "купить крипту", "куплю крипты", "купить крипты", "где купить", "где куплю", "подскажите где купить", "как купить", "хочу купить", "хочу куплю", "я бы купил", "я бы крипты купил", "крипту купить", "крипты купить", "куплю биткоин", "купить биткоин", "обменять крипту", "обмен крипты", "обменять на", "обменник", "обмен", "п2п", "p2p", "обменять биткоин", "usdt", "баты", "купить usdt", "куплю usdt", "биткоин", "эфир", "токены", "монеты", "криптовалюту", "валюту", "биткоин на", "на биткоин", "крипта на", "на крипту", "крипты на", "на крипты", "где взять", "где достать", "пацаны где", "ребята где", "где можно купить", "где можно обменять"]'::jsonb)
ON CONFLICT (name) DO NOTHING;

-- ============================================
-- 4. Индексы для производительности
-- ============================================
CREATE INDEX IF NOT EXISTS idx_trigger_chat_active
    ON trigger_chat_configs(is_active) WHERE is_active = true;

CREATE INDEX IF NOT EXISTS idx_global_config_updated
    ON global_config(updated_at);

-- ============================================
-- 5. Функция для обновления updated_at
-- ============================================
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Триггеры для auto-update
DROP TRIGGER IF EXISTS update_global_config_updated_at ON global_config;
CREATE TRIGGER update_global_config_updated_at
    BEFORE UPDATE ON global_config
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

DROP TRIGGER IF EXISTS update_trigger_chat_configs_updated_at ON trigger_chat_configs;
CREATE TRIGGER update_trigger_chat_configs_updated_at
    BEFORE UPDATE ON trigger_chat_configs
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();
