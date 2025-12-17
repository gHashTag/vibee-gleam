#!/bin/bash
BRIDGE="https://vibee-telegram-bridge.fly.dev"
SESSION="REDACTED_SESSION"

echo "Verifying 2FA..."
curl --max-time 30 -s -X POST "$BRIDGE/api/v1/auth/2fa" \
  -H "Content-Type: application/json" \
  -H "X-Session-ID: $SESSION" \
  -d '{"password": "REDACTED_2FA_PASSWORD"}'
