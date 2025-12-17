#!/bin/bash
# Test MCP WebSocket connection
# Usage: ./test_mcp_ws.sh [port]

PORT=${1:-8082}
WS_URL="ws://localhost:$PORT/ws/mcp"

echo "Testing MCP WebSocket at $WS_URL"

# Check if websocat is available
if ! command -v websocat &> /dev/null; then
    echo "websocat not found. Installing via brew..."
    brew install websocat 2>/dev/null || {
        echo "Please install websocat: brew install websocat"
        echo "Or use: cargo install websocat"
        exit 1
    }
fi

# Test 1: Initialize
echo ""
echo "=== Test 1: Initialize ==="
INIT_REQUEST='{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test-client","version":"1.0.0"}}}'
echo "Request: $INIT_REQUEST"
echo "$INIT_REQUEST" | timeout 5 websocat -n1 "$WS_URL" 2>/dev/null || echo "Connection failed or timed out"

echo ""
echo "=== Test 2: Ping ==="
PING_REQUEST='{"jsonrpc":"2.0","id":2,"method":"ping","params":{}}'
echo "Request: $PING_REQUEST"
echo "$PING_REQUEST" | timeout 5 websocat -n1 "$WS_URL" 2>/dev/null || echo "Connection failed or timed out"

echo ""
echo "=== Test 3: Tools List ==="
TOOLS_REQUEST='{"jsonrpc":"2.0","id":3,"method":"tools/list","params":{}}'
echo "Request: $TOOLS_REQUEST"
echo "$TOOLS_REQUEST" | timeout 5 websocat -n1 "$WS_URL" 2>/dev/null | head -c 500 || echo "Connection failed or timed out"
echo "..."

echo ""
echo "=== Test 4: File Read ==="
FILE_REQUEST='{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"file_read","arguments":{"path":"/tmp/test.txt"}}}'
echo "Request: $FILE_REQUEST"
echo "$FILE_REQUEST" | timeout 5 websocat -n1 "$WS_URL" 2>/dev/null || echo "Connection failed or timed out"

echo ""
echo "Tests completed!"
