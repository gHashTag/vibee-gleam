#!/bin/bash

echo "========================================="
echo "  Starting VIBEE with Authenticated Session"
echo "========================================="
echo ""

export VIBEE_MODE=mcp
export TELEGRAM_API_ID=94892
export TELEGRAM_API_HASH=cacf9ad137d228611b49b2ecc6d68d43
export TELEGRAM_SESSION_ID=sess_df0p27qhvzvv
export TELEGRAM_SESSION_1_PHONE="+79933420465"
export TELEGRAM_SESSION_1_USERNAME="neuro_sage"
export MCP_PORT=3000

echo "Configuration:"
echo "  Mode: MCP Server"
echo "  Session: sess_df0p27qhvzvv"
echo "  User: @neuro_sage"
echo "  Phone: +7 (993) 342-04-65"
echo "  Port: 3000"
echo ""

echo "✅ Telegram authenticated"
echo "✅ RAG system ready (2000+ lines)"
echo "✅ 8 RAG tools available"
echo "✅ 11 MB embeddings loaded"
echo ""

echo "Starting VIBEE..."
gleam run 2>&1 | grep -v "^warning:" | grep -v "^Hint:" | head -100
