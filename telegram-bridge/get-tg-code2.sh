#!/bin/bash
# Try with header instead of query param
echo "With X-Session-ID header:"
curl -s -H "X-Session-ID: REDACTED_SESSION" "http://localhost:8081/api/v1/history/777000?limit=5"
echo ""
echo "Try REDACTED_SESSION:"
curl -s -H "X-Session-ID: REDACTED_SESSION" "http://localhost:8081/api/v1/history/777000?limit=5"
