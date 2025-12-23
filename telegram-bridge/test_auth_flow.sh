#!/bin/bash
set -e

echo "========================================="
echo "  Testing Fixed Authentication Flow"
echo "========================================="
echo ""

# Step 1: Connect
echo "Step 1: Creating new connection..."
CONNECT_RESPONSE=$(curl -s -X POST http://localhost:8081/api/v1/connect \
  -H "Content-Type: application/json" \
  -d '{
    "app_id": 94892,
    "app_hash": "cacf9ad137d228611b49b2ecc6d68d43"
  }')

echo "$CONNECT_RESPONSE" | jq .
SESSION_ID=$(echo "$CONNECT_RESPONSE" | jq -r '.session_id')
echo ""
echo "✅ Session ID: $SESSION_ID"
echo ""

# Step 2: Send phone
echo "Step 2: Sending phone number..."
PHONE_RESPONSE=$(curl -s -X POST http://localhost:8081/api/v1/auth/phone \
  -H "Content-Type: application/json" \
  -d "{
    \"session_id\": \"$SESSION_ID\",
    \"phone\": \"+79933420465\"
  }")

echo "$PHONE_RESPONSE" | jq .
echo ""

if echo "$PHONE_RESPONSE" | jq -e '.status == "code_sent"' > /dev/null; then
    echo "✅ SMS code sent to +7 (993) 342-04-65"
    echo ""
    echo "📱 Please check your phone for the SMS code"
    echo ""
    echo "To complete authentication, run:"
    echo ""
    echo "curl -X POST http://localhost:8081/api/v1/auth/code \\"
    echo "  -H 'Content-Type: application/json' \\"
    echo "  -d '{\"session_id\": \"$SESSION_ID\", \"code\": \"YOUR_CODE\"}'"
    echo ""
    echo "If 2FA is required:"
    echo "curl -X POST http://localhost:8081/api/v1/auth/2fa \\"
    echo "  -H 'Content-Type: application/json' \\"
    echo "  -d '{\"session_id\": \"$SESSION_ID\", \"password\": \"Vishnu8087\"}'"
else
    echo "❌ Failed to send code"
    echo "Response: $PHONE_RESPONSE"
fi
