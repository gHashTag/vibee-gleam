#!/bin/bash
# Start VIBEE agents with proper configuration

export VIBEE_MODE=mcp
export DATABASE_URL=""
export OPENROUTER_API_KEY="${OPENROUTER_API_KEY:-dummy}"
export GEMINI_API_KEY="${GEMINI_API_KEY:-dummy}"

echo "========================================="
echo "  VIBEE Agent Framework - Starting"
echo "========================================="
echo ""
echo "Erlang/OTP: $(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell 2>&1)"
echo "Gleam: $(gleam --version)"
echo "Mode: MCP Server"
echo ""

# Run with timeout to show startup
timeout 10 gleam run 2>&1 | head -100 &
PID=$!

sleep 5

if ps -p $PID > /dev/null; then
    echo ""
    echo "✅ VIBEE agents are starting..."
    echo "Process PID: $PID"
    echo ""
    echo "Agents initialized with:"
    echo "  - RAG Tools (telegram_search_history, etc.)"
    echo "  - Embedding Worker (Gemini)"
    echo "  - Session Manager (multi-account)"
    echo "  - Payment System (Robokassa, TON, Stars)"
    echo "  - P2P Escrow"
    echo "  - Earning Worker (arbitrage scanner)"
    echo ""
    wait $PID
else
    echo "❌ Process failed to start"
fi
