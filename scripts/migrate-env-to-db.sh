#!/bin/bash
# Migrate session from environment variables to database

set -e

echo "📦 Migrating Telegram session to database..."
echo ""

# Load .env if exists
if [ -f ".env" ]; then
    source .env
fi

# Validate required variables
if [ -z "$DATABASE_URL" ]; then
    echo "❌ DATABASE_URL not set"
    exit 1
fi

if [ -z "$TELEGRAM_SESSION_ID" ]; then
    echo "❌ TELEGRAM_SESSION_ID not set"
    exit 1
fi

if [ -z "$TELEGRAM_API_ID" ]; then
    echo "❌ TELEGRAM_API_ID not set"
    exit 1
fi

if [ -z "$TELEGRAM_API_HASH" ]; then
    echo "❌ TELEGRAM_API_HASH not set"
    exit 1
fi

# Get user_id (default to owner_id if not set)
USER_ID=${USER_ID:-"144022504"}
PHONE=${TELEGRAM_SESSION_1_PHONE:-""}

echo "Configuration:"
echo "  User ID:    $USER_ID"
echo "  Session ID: $TELEGRAM_SESSION_ID"
echo "  API ID:     $TELEGRAM_API_ID"
echo "  Phone:      ${PHONE:-"(not set)"}"
echo ""

# Run migration SQL
echo "Inserting session into database..."

psql "$DATABASE_URL" <<EOF
INSERT INTO user_sessions (
    user_id, 
    session_id, 
    api_id, 
    api_hash_encrypted, 
    phone,
    is_active,
    is_authorized,
    digital_twin_enabled,
    auto_reply_enabled
) VALUES (
    '$USER_ID',
    '$TELEGRAM_SESSION_ID',
    $TELEGRAM_API_ID,
    pgp_sym_encrypt('$TELEGRAM_API_HASH', 
        (SELECT master_key FROM encryption_keys WHERE id = 1)),
    $([ -n "$PHONE" ] && echo "'$PHONE'" || echo "NULL"),
    true,
    true,
    true,
    true
)
ON CONFLICT (user_id) DO UPDATE SET
    session_id = EXCLUDED.session_id,
    api_id = EXCLUDED.api_id,
    api_hash_encrypted = EXCLUDED.api_hash_encrypted,
    phone = EXCLUDED.phone,
    is_active = true,
    updated_at = NOW();
EOF

if [ $? -eq 0 ]; then
    echo ""
    echo "✅ Session migrated successfully!"
    echo ""
    echo "Next steps:"
    echo "  1. Verify session in database:"
    echo "     psql \$DATABASE_URL -c \"SELECT user_id, session_id, is_active FROM user_sessions WHERE user_id = '$USER_ID';\""
    echo ""
    echo "  2. Test loading from database:"
    echo "     export USER_ID=$USER_ID"
    echo "     ./start.sh"
    echo ""
    echo "  3. (Optional) Remove env vars from .env after testing"
else
    echo ""
    echo "❌ Migration failed!"
    exit 1
fi
