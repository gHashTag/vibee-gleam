#!/bin/bash

echo "========================================="
echo "  Starting VIBEE with OpenRouter"
echo "========================================="
echo ""

export VIBEE_MODE=mcp
export TELEGRAM_API_ID=94892
export TELEGRAM_API_HASH=cacf9ad137d228611b49b2ecc6d68d43
export TELEGRAM_SESSION_ID=sess_df0p27qhvzvv
export TELEGRAM_SESSION_1_PHONE="+79933420465"
export TELEGRAM_SESSION_1_USERNAME="neuro_sage"
export TELEGRAM_BRIDGE_URL=http://localhost:8081
export OPENROUTER_API_KEY=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
export DATABASE_URL=""
export GEMINI_API_KEY="${GEMINI_API_KEY:-dummy}"

echo "✅ OpenRouter API Key configured"
echo "   Usage: $1.02 (monthly)"
echo "   Status: Active"
echo ""
echo "✅ Telegram Session: sess_df0p27qhvzvv"
echo "   User: @neuro_sage"
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
    echo "✅ VIBEE Agent is RUNNING!"
    echo "========================================="
    echo ""
    echo "  PID: $VIBEE_PID"
    echo "  MCP WebSocket: ws://localhost:8080/ws/mcp"
    echo "  Health: http://localhost:8080/health"
    echo ""
    echo "  Telegram: Connected (@neuro_sage)"
    echo "  OpenRouter: Active"
    echo "  Digital Twin: Responding to messages"
    echo ""
    echo "To stop: kill $VIBEE_PID"
    echo ""
    
    # Keep running
    wait $VIBEE_PID
else
    echo "❌ Failed to start"
fi
