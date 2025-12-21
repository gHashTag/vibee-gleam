#!/bin/bash

echo "========================================="
echo "  Starting VIBEE with Authenticated Session"
echo "========================================="
echo ""

# Load .env if exists
if [ -f .env ]; then
    set -a; source .env; set +a
fi

if [ -z "$TELEGRAM_SESSION_ID" ]; then
    echo "ERROR: TELEGRAM_SESSION_ID not set. Copy .env.example to .env"
    exit 1
fi

export VIBEE_MODE=${VIBEE_MODE:-mcp}
export MCP_PORT=${MCP_PORT:-3000}

echo "Configuration:"
echo "  Mode: $VIBEE_MODE"
echo "  Session: $TELEGRAM_SESSION_ID"
echo "  Port: $MCP_PORT"
echo ""

echo "Starting VIBEE..."
gleam run 2>&1 | grep -v "^warning:" | grep -v "^Hint:" | head -100
