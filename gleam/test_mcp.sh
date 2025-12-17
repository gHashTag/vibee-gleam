#!/bin/bash
# Test VIBEE MCP Server
cd /Users/playra/vibee-eliza-999/vibee/gleam

echo "Testing VIBEE MCP Server..."
echo ""

# Build first
echo "Building..."
gleam build 2>/dev/null

# Create test file
cat > /tmp/mcp_test.txt << 'EOF'
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}
{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}
EOF

# Run server with test input
echo "Sending test requests..."
cat /tmp/mcp_test.txt | gleam run -m mcp_server 2>/tmp/mcp_err.log

echo ""
echo "Log output:"
cat /tmp/mcp_err.log | head -20
