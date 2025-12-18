#!/bin/bash
# Universal VIBEE startup script
# Supports: Single-user (env), Single-user (DB), Multi-tenant (DB)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "🚀 Starting VIBEE Telegram Agent..."
echo ""

# 1. Load environment files
if [ -f ".env" ]; then
    echo "✓ Loading environment from .env"
    set -a
    source .env
    set +a
fi

if [ -f ".envrc" ]; then
    echo "✓ Loading environment from .envrc"
    set -a
    source .envrc
    set +a
fi

# 2. Determine mode
MODE="unknown"

if [ -n "$MULTI_TENANT" ] && [ "$MULTI_TENANT" = "true" ]; then
    MODE="multi-tenant"
    echo "📦 Mode: Multi-tenant (all active sessions from DB)"
elif [ -n "$USER_ID" ]; then
    MODE="single-db"
    echo "👤 Mode: Single user from database (USER_ID=$USER_ID)"
elif [ -n "$TELEGRAM_SESSION_ID" ]; then
    MODE="single-env"
    echo "🔧 Mode: Single user from environment variables"
else
    MODE="auto"
    echo "🔍 Mode: Auto-detect (will try DB first, then env)"
fi

# 3. Validate based on mode
case $MODE in
    "multi-tenant")
        if [ -z "$DATABASE_URL" ]; then
            echo "❌ DATABASE_URL required for multi-tenant mode"
            exit 1
        fi
        ;;
    "single-db")
        if [ -z "$DATABASE_URL" ]; then
            echo "❌ DATABASE_URL required for database mode"
            exit 1
        fi
        ;;
    "single-env")
        REQUIRED_VARS=("TELEGRAM_SESSION_ID" "TELEGRAM_API_ID" "TELEGRAM_API_HASH")
        MISSING_VARS=()
        for var in "${REQUIRED_VARS[@]}"; do
            if [ -z "${!var}" ]; then
                MISSING_VARS+=("$var")
            fi
        done
        
        if [ ${#MISSING_VARS[@]} -gt 0 ]; then
            echo "❌ Missing required variables for env mode:"
            for var in "${MISSING_VARS[@]}"; do
                echo "   - $var"
            done
            exit 1
        fi
        ;;
esac

# 4. Show configuration
echo ""
echo "Configuration:"
if [ -n "$DATABASE_URL" ]; then
    echo "  Database:   ${DATABASE_URL:0:40}..."
fi
if [ -n "$TELEGRAM_SESSION_ID" ]; then
    echo "  Session ID: ${TELEGRAM_SESSION_ID:0:15}..."
fi
if [ -n "$USER_ID" ]; then
    echo "  User ID:    $USER_ID"
fi
echo ""

# 5. Start application
cd gleam
echo "Starting Gleam application..."
exec gleam run
