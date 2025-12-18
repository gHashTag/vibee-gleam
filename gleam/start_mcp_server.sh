#!/bin/bash

echo "========================================="
echo "  Starting VIBEE MCP Server"
echo "========================================="
echo ""

export VIBEE_MODE=mcp
export TELEGRAM_API_ID=94892
export TELEGRAM_API_HASH=REDACTED_API_HASH
export TELEGRAM_SESSION_ID=REDACTED_SESSION
export TELEGRAM_SESSION_1_PHONE="+79933420465"
export TELEGRAM_SESSION_1_USERNAME="neuro_sage"
export MCP_PORT=3000
export DATABASE_URL=""
export OPENROUTER_API_KEY="${OPENROUTER_API_KEY:-dummy}"
export GEMINI_API_KEY="${GEMINI_API_KEY:-dummy}"

echo "Configuration:"
echo "  Mode: MCP"
echo "  Port: 3000"
echo "  Session: REDACTED_SESSION"
echo "  User: @neuro_sage"
echo ""

echo "Starting VIBEE MCP Server..."
gleam run 2>&1 &
VIBEE_PID=$!

sleep 5

if ps -p $VIBEE_PID > /dev/null 2>&1; then
    echo ""
    echo "✅ VIBEE MCP Server started (PID: $VIBEE_PID)"
    echo "✅ Listening on port 3000"
    echo "✅ Telegram authenticated"
    echo "✅ RAG tools available"
    echo ""
    echo "Server is running in background"
    echo "To stop: kill $VIBEE_PID"
else
    echo ""
    echo "❌ Failed to start VIBEE"
    echo "Check logs for errors"
fi
