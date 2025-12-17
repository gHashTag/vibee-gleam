#!/bin/bash
# VIBEE Services Startup Script
# Запускает все необходимые сервисы для VIBEE MCP

VIBEE_DIR="/Users/playra/vibee-eliza-999/vibee"
LOG_DIR="$VIBEE_DIR/logs"

mkdir -p "$LOG_DIR"

echo "Starting VIBEE services..."

# 1. Start Telegram Bridge
BRIDGE_PID=$(pgrep -f "telegram-bridge" 2>/dev/null)
if [ -z "$BRIDGE_PID" ]; then
    echo "Starting Telegram Bridge..."
    cd "$VIBEE_DIR/telegram-bridge"
    nohup ./telegram-bridge > "$LOG_DIR/telegram-bridge.log" 2>&1 &
    sleep 2
    echo "Telegram Bridge started (PID: $!)"
else
    echo "Telegram Bridge already running (PID: $BRIDGE_PID)"
fi

# 2. Check Bridge health
HEALTH=$(curl -s http://localhost:8081/health 2>/dev/null)
if echo "$HEALTH" | grep -q "ok"; then
    echo "Telegram Bridge: OK"
else
    echo "Telegram Bridge: FAILED to start!"
    exit 1
fi

echo ""
echo "VIBEE services started successfully!"
echo ""
echo "Available MCP tools:"
echo "  - mcp__vibee__auth_status"
echo "  - mcp__vibee__telegram_get_dialogs"
echo "  - mcp__vibee__telegram_send_message"
echo "  - ... and 40+ more tools"
echo ""
echo "To use in Claude Code, restart the session."
