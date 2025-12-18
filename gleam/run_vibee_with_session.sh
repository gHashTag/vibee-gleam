#!/bin/bash

echo "========================================="
echo "  VIBEE MCP Server with Telegram"
echo "========================================="
echo ""

# Export all required variables
export VIBEE_MODE=mcp
export TELEGRAM_API_ID=94892
export TELEGRAM_API_HASH=REDACTED_API_HASH
export TELEGRAM_SESSION_ID=REDACTED_SESSION
export TELEGRAM_SESSION_1_PHONE="+79933420465"
export TELEGRAM_SESSION_1_USERNAME="neuro_sage"
export TELEGRAM_BRIDGE_URL=http://localhost:8081
export DATABASE_URL=""
export OPENROUTER_API_KEY="${OPENROUTER_API_KEY:-dummy}"
export GEMINI_API_KEY="${GEMINI_API_KEY:-dummy}"

echo "✅ Configuration loaded"
echo "   Session: REDACTED_SESSION"
echo "   User: @neuro_sage"
echo "   Bridge: http://localhost:8081"
echo ""

# Test telegram-bridge first
echo "Testing telegram-bridge connection..."
BRIDGE_TEST=$(curl -s http://localhost:8081/api/v1/me -H "X-Session-ID: REDACTED_SESSION")

if echo "$BRIDGE_TEST" | grep -q "neuro_sage"; then
    echo "✅ Telegram bridge connected"
    echo "   User: $(echo $BRIDGE_TEST | jq -r '.username')"
    echo ""
else
    echo "❌ Telegram bridge not responding"
    echo "   Response: $BRIDGE_TEST"
    exit 1
fi

echo "Starting VIBEE MCP Server..."
echo ""

# Run VIBEE
gleam run 2>&1 | tee /tmp/vibee.log
