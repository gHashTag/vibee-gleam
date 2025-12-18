#!/bin/bash
export VIBEE_MODE=mcp
export MCP_PORT=3000
export DATABASE_URL=""
export OPENROUTER_API_KEY="${OPENROUTER_API_KEY:-dummy}"
export GEMINI_API_KEY="${GEMINI_API_KEY:-dummy}"
export TELEGRAM_API_ID="${TELEGRAM_API_ID:-12345}"
export TELEGRAM_API_HASH="${TELEGRAM_API_HASH:-dummy}"
export TELEGRAM_SESSION_1_PHONE="+79933420465"
export TELEGRAM_SESSION_1_USERNAME="neuro_sage"
export TELEGRAM_SESSION_2_PHONE="+6662401417"
export TELEGRAM_SESSION_2_USERNAME="vibee_agent"

echo "Starting VIBEE MCP Server..."
echo "Port: 3000"
echo "Sessions configured:"
echo "  1. +7 (993) 342-04-65 - @neuro_sage"
echo "  2. +66 6-2401-4170 - @vibee_agent"
echo ""

gleam run 2>&1 | grep -v "^warning:" | grep -v "^Hint:" | grep -v "┌─" | grep -v "│" &
MCP_PID=$!

sleep 5

if ps -p $MCP_PID > /dev/null 2>&1; then
    echo "✅ MCP Server started (PID: $MCP_PID)"
    echo ""
    # Keep running
    wait $MCP_PID
else
    echo "❌ MCP Server failed to start"
    exit 1
fi
