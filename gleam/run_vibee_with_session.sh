#!/bin/bash

echo "========================================="
echo "  VIBEE MCP Server with Telegram"
echo "========================================="
echo ""

# Load .env if exists
if [ -f .env ]; then
    echo "Loading .env..."
    set -a
    source .env
    set +a
fi

# Required: TELEGRAM_SESSION_ID from .env or environment
if [ -z "$TELEGRAM_SESSION_ID" ]; then
    echo "ERROR: TELEGRAM_SESSION_ID not set"
    echo "Copy .env.example to .env and configure"
    exit 1
fi

# Defaults
export VIBEE_MODE=${VIBEE_MODE:-mcp}
export VIBEE_BRIDGE_URL=${VIBEE_BRIDGE_URL:-http://localhost:8081}
export OPENROUTER_API_KEY="${OPENROUTER_API_KEY:-dummy}"

echo "Configuration:"
echo "   Session: $TELEGRAM_SESSION_ID"
echo "   Bridge: $VIBEE_BRIDGE_URL"
echo "   Mode: $VIBEE_MODE"
echo ""

# Test telegram-bridge first
echo "Testing telegram-bridge connection..."
BRIDGE_TEST=$(curl -s "$VIBEE_BRIDGE_URL/api/v1/me" -H "X-Session-ID: $TELEGRAM_SESSION_ID" -H "Authorization: Bearer ${VIBEE_API_KEY:-}")

if echo "$BRIDGE_TEST" | grep -q "username"; then
    echo "Connected: $(echo $BRIDGE_TEST | jq -r '.username // .first_name')"
    echo ""
else
    echo "Bridge not responding: $BRIDGE_TEST"
    exit 1
fi

echo "Starting VIBEE..."
gleam run 2>&1 | tee /tmp/vibee.log
