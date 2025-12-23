#!/bin/bash
export TELEGRAM_APP_ID=94892
export TELEGRAM_APP_HASH=cacf9ad137d228611b49b2ecc6d68d43

echo "Testing Telegram Bridge connection..."
echo ""

# Test connect endpoint
echo "1. Connecting to Telegram..."
RESPONSE=$(curl -s -X POST http://localhost:8081/api/v1/connect \
  -H "Content-Type: application/json" \
  -d "{
    \"app_id\": 94892,
    \"app_hash\": \"cacf9ad137d228611b49b2ecc6d68d43\"
  }")

echo "Response: $RESPONSE"
echo ""

# Extract session_id if present
SESSION_ID=$(echo $RESPONSE | grep -o '"session_id":"[^"]*"' | cut -d'"' -f4)

if [ -n "$SESSION_ID" ]; then
    echo "✅ Connected! Session ID: $SESSION_ID"
    echo ""
    
    # Test getting current user
    echo "2. Getting current user info..."
    curl -s "http://localhost:8081/api/v1/me?session_id=$SESSION_ID" | jq .
else
    echo "⚠️ Connection failed or session_id not returned"
fi
