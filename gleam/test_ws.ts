// Simple WebSocket MCP test
const WS_URL = "ws://localhost:8080/ws/mcp";

async function test() {
  console.log("Connecting to:", WS_URL);

  const ws = new WebSocket(WS_URL);

  ws.onopen = () => {
    console.log("Connected!");

    // Send initialize request (MCP protocol)
    const initRequest = {
      jsonrpc: "2.0",
      id: 1,
      method: "initialize",
      params: {
        protocolVersion: "2024-11-05",
        capabilities: {},
        clientInfo: { name: "test", version: "1.0.0" }
      }
    };

    console.log("Sending:", JSON.stringify(initRequest));
    ws.send(JSON.stringify(initRequest));
  };

  ws.onmessage = (event) => {
    console.log("Received:", event.data);

    // After init response, request tools list
    try {
      const response = JSON.parse(event.data);
      if (response.id === 1) {
        // Send tools/list request
        const toolsRequest = {
          jsonrpc: "2.0",
          id: 2,
          method: "tools/list"
        };
        console.log("Sending tools/list:", JSON.stringify(toolsRequest));
        ws.send(JSON.stringify(toolsRequest));
      } else if (response.id === 2) {
        // Got tools list
        console.log("\n=== Tools List ===");
        const tools = response.result?.tools || [];
        console.log(`Found ${tools.length} tools`);

        const p2pTools = tools.filter((t: any) => t.name.startsWith("p2p_"));
        console.log(`P2P tools: ${p2pTools.length}`);
        p2pTools.forEach((t: any) => console.log(`  - ${t.name}`));

        // Test p2p_rates
        const ratesRequest = {
          jsonrpc: "2.0",
          id: 3,
          method: "tools/call",
          params: {
            name: "p2p_rates",
            arguments: {}
          }
        };
        console.log("\nCalling p2p_rates...");
        ws.send(JSON.stringify(ratesRequest));
      } else if (response.id === 3) {
        console.log("\n=== p2p_rates result ===");
        console.log(JSON.stringify(response.result, null, 2));

        console.log("\n=== TEST PASSED ===");
        ws.close();
        process.exit(0);
      }
    } catch (e) {
      console.log("Parse error:", e);
    }
  };

  ws.onerror = (err) => {
    console.error("WebSocket error:", err);
    process.exit(1);
  };

  ws.onclose = () => {
    console.log("Connection closed");
  };

  // Timeout after 15 seconds
  setTimeout(() => {
    console.log("TIMEOUT - no response received");
    process.exit(1);
  }, 15000);
}

test();
