#!/bin/bash
BRIDGE="https://vibee-telegram-bridge.fly.dev"

echo "Connecting neuro_sage account..."
curl --max-time 30 -s -X POST "$BRIDGE/api/v1/connect" \
  -H "Content-Type: application/json" \
  -d '{"app_id": 94892, "app_hash": "cacf9ad137d228611b49b2ecc6d68d43", "phone": "+79933420465"}'
