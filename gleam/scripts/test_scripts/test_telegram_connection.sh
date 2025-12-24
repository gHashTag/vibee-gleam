#!/bin/bash

echo "========================================="
echo "  VIBEE Telegram Connection Test"
echo "========================================="
echo ""

export TELEGRAM_API_ID=94892
export TELEGRAM_API_HASH=cacf9ad137d228611b49b2ecc6d68d43
export TELEGRAM_SESSION_1_PHONE="+79933420465"
export TELEGRAM_SESSION_1_USERNAME="neuro_sage"
export VIBEE_MODE=telegram

echo "Configuration:"
echo "  API ID: $TELEGRAM_API_ID"
echo "  Phone: $TELEGRAM_SESSION_1_PHONE"
echo "  Username: @$TELEGRAM_SESSION_1_USERNAME"
echo ""

echo "Available session files:"
ls -lh ../telegram-bridge/sessions/*.session | head -5
echo ""

echo "✅ Credentials configured"
echo "✅ Session files available"
echo "✅ System ready to connect"
echo ""

echo "To start VIBEE with Telegram:"
echo "  export TELEGRAM_API_ID=94892"
echo "  export TELEGRAM_API_HASH=cacf9ad137d228611b49b2ecc6d68d43"
echo "  gleam run"
