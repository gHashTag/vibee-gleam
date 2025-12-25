-- Migration: Owner Notification System
-- Real-time уведомления owner о событиях агента
-- Inline кнопки для быстрых действий

-- ============================================
-- 1. Настройки уведомлений owner
-- ============================================

CREATE TABLE IF NOT EXISTS owner_notification_settings (
    owner_id BIGINT PRIMARY KEY,
    enabled BOOLEAN DEFAULT true,

    -- Фильтры по типу события
    notify_new_messages BOOLEAN DEFAULT true,
    notify_agent_replies BOOLEAN DEFAULT false,
    notify_triggers BOOLEAN DEFAULT true,
    notify_new_chats BOOLEAN DEFAULT true,
    notify_leads BOOLEAN DEFAULT true,
    notify_errors BOOLEAN DEFAULT true,

    -- Фильтры по важности (low, medium, high, critical)
    min_importance VARCHAR(20) DEFAULT 'medium',

    -- Mute настройки
    mute_until TIMESTAMPTZ,
    muted_chats BIGINT[] DEFAULT '{}',

    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- ============================================
-- 2. Лог отправленных уведомлений
-- ============================================

CREATE TABLE IF NOT EXISTS owner_notification_log (
    id SERIAL PRIMARY KEY,
    owner_id BIGINT NOT NULL,
    event_type VARCHAR(50) NOT NULL,
    source_chat_id BIGINT,
    from_user_id BIGINT,
    message_text TEXT,
    telegram_message_id INT,
    sent_at TIMESTAMPTZ DEFAULT NOW(),
    callback_data TEXT
);

-- ============================================
-- 3. Индексы для производительности
-- ============================================

CREATE INDEX IF NOT EXISTS idx_notification_log_owner
    ON owner_notification_log(owner_id, sent_at DESC);

CREATE INDEX IF NOT EXISTS idx_notification_log_event_type
    ON owner_notification_log(event_type);

CREATE INDEX IF NOT EXISTS idx_notification_log_source_chat
    ON owner_notification_log(source_chat_id);

-- ============================================
-- 4. Триггер для auto-update updated_at
-- ============================================

DROP TRIGGER IF EXISTS update_owner_notification_settings_updated_at ON owner_notification_settings;
CREATE TRIGGER update_owner_notification_settings_updated_at
    BEFORE UPDATE ON owner_notification_settings
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- ============================================
-- 5. Дефолтные настройки для owner
-- ============================================

INSERT INTO owner_notification_settings (
    owner_id, enabled,
    notify_new_messages, notify_agent_replies, notify_triggers,
    notify_new_chats, notify_leads, notify_errors,
    min_importance
) VALUES (
    144022504, true,
    true, false, true,
    true, true, true,
    'medium'
) ON CONFLICT (owner_id) DO NOTHING;

-- ============================================
-- 6. Комментарии для документации
-- ============================================

COMMENT ON TABLE owner_notification_settings IS 'Настройки уведомлений для owner (фильтры по типу и важности)';
COMMENT ON COLUMN owner_notification_settings.min_importance IS 'Минимальный уровень важности: low, medium, high, critical';
COMMENT ON COLUMN owner_notification_settings.muted_chats IS 'Массив chat_id которые временно замьючены';
COMMENT ON TABLE owner_notification_log IS 'Лог отправленных уведомлений owner для аналитики и дедупликации';
