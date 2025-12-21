#!/bin/bash

echo "========================================="
echo "  Starting VIBEE with OpenRouter"
echo "========================================="
echo ""

# Load .env if exists
if [ -f .env ]; then
    set -a; source .env; set +a
fi

# Required
if [ -z "$TELEGRAM_SESSION_ID" ]; then
    echo "ERROR: TELEGRAM_SESSION_ID not set. Copy .env.example to .env"
    exit 1
fi

if [ -z "$OPENROUTER_API_KEY" ]; then
    echo "ERROR: OPENROUTER_API_KEY not set in .env"
    exit 1
fi

export VIBEE_MODE=${VIBEE_MODE:-mcp}
export VIBEE_BRIDGE_URL=${VIBEE_BRIDGE_URL:-http://localhost:8081}
export DATABASE_URL="${DATABASE_URL:-}"
export GEMINI_API_KEY="${GEMINI_API_KEY:-dummy}"

echo "Configuration:"
echo "  Session: $TELEGRAM_SESSION_ID"
echo "  OpenRouter: ${OPENROUTER_API_KEY:0:20}..."
echo ""

# Test OpenRouter
echo "Testing OpenRouter API..."
TEST_RESPONSE=$(curl -s https://openrouter.ai/api/v1/auth/key \
  -H "Authorization: Bearer $OPENROUTER_API_KEY" | jq -r '.data.label')

if [ "$TEST_RESPONSE" != "null" ]; then
    echo "✅ OpenRouter API: $TEST_RESPONSE"
else
    echo "❌ OpenRouter API test failed"
    exit 1
fi

echo ""
echo "Starting VIBEE MCP Server..."
echo ""

gleam run 2>&1 | grep -E "^\[|^✅|^❌|VIBEE|MCP|TWIN|POLL|ERROR" | head -100 &
VIBEE_PID=$!

sleep 8

if ps -p $VIBEE_PID > /dev/null 2>&1; then
    echo ""
    echo "========================================="
    echo "VIBEE Agent is RUNNING!"
    echo "========================================="
    echo ""
    echo "  PID: $VIBEE_PID"
    echo "  MCP WebSocket: ws://localhost:8080/ws/mcp"
    echo "  Health: http://localhost:8080/health"
    echo ""
    echo "To stop: kill $VIBEE_PID"
    echo ""

    # Keep running
    wait $VIBEE_PID
else
    echo "Failed to start"
fi
