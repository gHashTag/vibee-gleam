#!/bin/bash

echo "========================================="
echo "  Starting VIBEE MCP Server"
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

export VIBEE_MODE=mcp
export MCP_PORT=${MCP_PORT:-3000}
export OPENROUTER_API_KEY="${OPENROUTER_API_KEY:-dummy}"

echo "Configuration:"
echo "  Mode: MCP"
echo "  Port: $MCP_PORT"
echo "  Session: $TELEGRAM_SESSION_ID"
echo ""

echo "Starting VIBEE MCP Server..."
gleam run 2>&1 &
VIBEE_PID=$!

sleep 5

if ps -p $VIBEE_PID > /dev/null 2>&1; then
    echo ""
    echo "VIBEE MCP Server started (PID: $VIBEE_PID)"
    echo "To stop: kill $VIBEE_PID"
else
    echo "Failed to start VIBEE. Check logs."
fi
