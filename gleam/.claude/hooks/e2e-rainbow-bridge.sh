#!/bin/bash
# Rainbow Bridge E2E Testing via MCP API
#
# Использует MCP сервис VIBEE для E2E тестирования
# Endpoint: GET /api/e2e/run
#
# Тестовые аккаунты:
#   ТЕСТЕР: @neuro_sage - отправляет команды
#   ЮЗЕР-БОТ: @vibee_agent - Digital Twin с автоматизацией

set -e

MCP_URL="${VIBEE_MCP_URL:-https://vibee-mcp.fly.dev}"
E2E_JSON_LOG="${E2E_JSON_LOG:-false}"

# Colors for human-readable output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() {
    if [ "$E2E_JSON_LOG" = "true" ]; then
        echo "{\"level\":\"INFO\",\"logger\":\"e2e\",\"message\":\"$1\"}" >&2
    else
        echo -e "${GREEN}[E2E]${NC} $1"
    fi
}

log_error() {
    if [ "$E2E_JSON_LOG" = "true" ]; then
        echo "{\"level\":\"ERROR\",\"logger\":\"e2e\",\"message\":\"$1\"}" >&2
    else
        echo -e "${RED}[E2E]${NC} $1"
    fi
}

# Wait for service to be ready after deploy
log_info "Waiting for service to start after deploy..."
sleep 5

# Call E2E endpoint
log_info "Running E2E tests via MCP API..."
log_info "URL: $MCP_URL/api/e2e/run"

response=$(curl -s --max-time 180 "$MCP_URL/api/e2e/run" 2>&1)
curl_status=$?

if [ $curl_status -ne 0 ]; then
    log_error "Failed to call E2E API (curl status $curl_status): $response"
    exit 1
fi

# Debug: Show first 200 chars of response
log_info "Raw response (first 200 chars): ${response:0:200}"

# Parse response
status=$(echo "$response" | jq -r '.status // "unknown"')
total=$(echo "$response" | jq -r '.total // 0')
passed=$(echo "$response" | jq -r '.passed // 0')
failed=$(echo "$response" | jq -r '.failed // 0')
error=$(echo "$response" | jq -r '.error // ""')

if [ -n "$error" ] && [ "$error" != "null" ]; then
    log_error "E2E API error: $error"
    exit 1
fi

echo ""
log_info "E2E Test Results:"
log_info "  Total:  $total"
log_info "  Passed: $passed"
log_info "  Failed: $failed"

# Show individual test results
echo ""
echo "$response" | jq -r '.tests[] | "  [\(.passed | if . then "PASS" else "FAIL" end)] \(.command) - \(.response | .[0:100])"' 2>/dev/null || true

echo ""
if [ "$status" = "passed" ]; then
    log_info "All E2E tests passed!"
    exit 0
else
    log_error "E2E tests failed!"
    exit 1
fi
