-- Migration: Centralized Chat Permissions System
-- Whitelist подход: по умолчанию агент молчит, отвечает только в разрешённых чатах
-- Уровни: owner, admin, user, observe_only, blocked

-- ============================================
-- 1. ENUM типы для уровней и типов чатов
-- ============================================

-- Создаём ENUM только если не существует
DO $$ BEGIN
    CREATE TYPE permission_level AS ENUM ('owner', 'admin', 'user', 'observe_only', 'blocked');
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

DO $$ BEGIN
    CREATE TYPE chat_type AS ENUM ('private', 'group', 'supergroup', 'channel');
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

-- ============================================
-- 2. Основная таблица прав чатов
-- ============================================

CREATE TABLE IF NOT EXISTS chat_permissions (
    chat_id BIGINT PRIMARY KEY,
    chat_type chat_type NOT NULL,
    permission_level permission_level NOT NULL DEFAULT 'blocked',
    chat_name VARCHAR(255),
    username VARCHAR(100),

    -- Настройки поведения
    can_respond BOOLEAN DEFAULT false,       -- Может ли агент отвечать
    can_initiate BOOLEAN DEFAULT false,      -- Может ли инициировать диалог
    use_triggers BOOLEAN DEFAULT true,       -- Проверять триггеры (для sniper mode)

    -- Связь с trigger_chat_configs (если нужны специфичные триггеры)
    trigger_config_id BIGINT REFERENCES trigger_chat_configs(chat_id),

    -- Пересылка лидов
    forward_to_chat_id BIGINT,

    -- Аудит
    granted_by BIGINT,                       -- Кто выдал права
    granted_at TIMESTAMPTZ DEFAULT NOW(),
    expires_at TIMESTAMPTZ,                  -- NULL = бессрочно
    notes TEXT,

    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- ============================================
-- 3. Таблица для логирования неизвестных чатов (pending review)
-- ============================================

CREATE TABLE IF NOT EXISTS pending_chat_reviews (
    chat_id BIGINT PRIMARY KEY,
    chat_type chat_type,
    first_contact_at TIMESTAMPTZ DEFAULT NOW(),
    last_message_at TIMESTAMPTZ DEFAULT NOW(),
    message_count INTEGER DEFAULT 1,
    sample_message TEXT,
    from_user_id BIGINT,
    from_username VARCHAR(100),
    from_name VARCHAR(255),
    reviewed BOOLEAN DEFAULT false,
    reviewed_at TIMESTAMPTZ,
    reviewed_by BIGINT,
    review_action VARCHAR(20),  -- 'approved', 'blocked', 'ignored'
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- ============================================
-- 4. Индексы для производительности
-- ============================================

CREATE INDEX IF NOT EXISTS idx_chat_permissions_level
    ON chat_permissions(permission_level);

CREATE INDEX IF NOT EXISTS idx_chat_permissions_can_respond
    ON chat_permissions(can_respond)
    WHERE can_respond = true;

CREATE INDEX IF NOT EXISTS idx_chat_permissions_expires
    ON chat_permissions(expires_at)
    WHERE expires_at IS NOT NULL;

CREATE INDEX IF NOT EXISTS idx_pending_reviews_unreviewed
    ON pending_chat_reviews(reviewed)
    WHERE reviewed = false;

CREATE INDEX IF NOT EXISTS idx_pending_reviews_last_message
    ON pending_chat_reviews(last_message_at DESC);

-- ============================================
-- 5. Триггер для auto-update updated_at
-- ============================================

DROP TRIGGER IF EXISTS update_chat_permissions_updated_at ON chat_permissions;
CREATE TRIGGER update_chat_permissions_updated_at
    BEFORE UPDATE ON chat_permissions
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- ============================================
-- 6. Миграция существующих данных
-- ============================================

-- Добавить owner (144022504) с полными правами
INSERT INTO chat_permissions (
    chat_id, chat_type, permission_level, chat_name,
    can_respond, can_initiate, use_triggers, granted_by, notes
) VALUES (
    144022504, 'private', 'owner', 'Owner',
    true, true, false, 144022504, 'Auto-migrated owner'
) ON CONFLICT (chat_id) DO NOTHING;

-- Мигрировать target_chats как User
INSERT INTO chat_permissions (
    chat_id, chat_type, permission_level, chat_name,
    can_respond, can_initiate, use_triggers, granted_by, notes
) VALUES
    (693774948, 'private', 'user', 'Test User', true, false, false, 144022504, 'Auto-migrated from target_chats'),
    (6579515876, 'private', 'user', 'vibee_agent bot', true, false, false, 144022504, 'Auto-migrated from target_chats'),
    (-1002737186844, 'supergroup', 'user', 'VIBEE AGENT Lead group', true, false, true, 144022504, 'Auto-migrated from target_chats'),
    (-1002298297094, 'supergroup', 'user', 'Test channel', true, false, true, 144022504, 'Auto-migrated from target_chats')
ON CONFLICT (chat_id) DO NOTHING;

-- Мигрировать trigger_chats с observe_only режимом
INSERT INTO chat_permissions (
    chat_id, chat_type, permission_level, chat_name,
    can_respond, can_initiate, use_triggers, trigger_config_id, forward_to_chat_id,
    granted_by, notes
)
SELECT
    tc.chat_id,
    'supergroup'::chat_type,
    CASE
        WHEN tc.response_probability = 0.0 THEN 'observe_only'::permission_level
        ELSE 'user'::permission_level
    END,
    tc.chat_name,
    tc.response_probability > 0.0,  -- can_respond только если probability > 0
    false,
    true,
    tc.chat_id,
    tc.forward_chat_id,
    144022504,
    'Auto-migrated from trigger_chat_configs'
FROM trigger_chat_configs tc
WHERE tc.is_active = true
ON CONFLICT (chat_id) DO NOTHING;

-- ============================================
-- 7. Комментарии для документации
-- ============================================

COMMENT ON TABLE chat_permissions IS 'Централизованное управление правами чатов (whitelist подход)';
COMMENT ON COLUMN chat_permissions.permission_level IS 'owner=полный контроль, admin=управление, user=стандарт, observe_only=только чтение, blocked=заблокирован';
COMMENT ON COLUMN chat_permissions.can_respond IS 'Может ли агент отвечать в этом чате';
COMMENT ON COLUMN chat_permissions.use_triggers IS 'Проверять ли триггер-слова (sniper mode)';
COMMENT ON TABLE pending_chat_reviews IS 'Неизвестные чаты ожидающие review от owner';
