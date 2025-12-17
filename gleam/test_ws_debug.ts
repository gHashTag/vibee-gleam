// Debug test - check exactly what's being sent/received
const WS_URL = "ws://localhost:8080/ws/mcp";

async function test() {
  console.log("Connecting to:", WS_URL);

  const ws = new WebSocket(WS_URL);

  ws.onopen = () => {
    console.log("Connected!");

    // Send initialize request - ensure id is numeric 1
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

    const msg = JSON.stringify(initRequest);
    console.log("Sending (raw):", msg);
    ws.send(msg);
  };

  ws.onmessage = (event) => {
    console.log("Received (raw):", event.data);

    try {
      const response = JSON.parse(event.data);
      console.log("Parsed response:");
      console.log("  id:", response.id, "(type:", typeof response.id, ")");
      console.log("  result:", response.result ? "present" : "absent");
      console.log("  error:", response.error ? JSON.stringify(response.error) : "absent");

      // Send tools/list after receiving any response
      if (response.result?.protocolVersion) {
        console.log("\nSending tools/list...");
        const toolsRequest = {
          jsonrpc: "2.0",
          id: 2,
          method: "tools/list"
        };
        console.log("Sending (raw):", JSON.stringify(toolsRequest));
        ws.send(JSON.stringify(toolsRequest));
      } else if (response.result?.tools !== undefined) {
        console.log("\n=== Tools found: ===");
        const tools = response.result?.tools || [];
        console.log("Total:", tools.length);

        tools.forEach((t: any) => console.log(" -", t.name));

        // Test p2p_rates
        console.log("\nSending p2p_rates call...");
        const ratesRequest = {
          jsonrpc: "2.0",
          id: 3,
          method: "tools/call",
          params: {
            name: "p2p_rates",
            arguments: {}
          }
        };
        ws.send(JSON.stringify(ratesRequest));
      } else if (response.id === 3) {
        console.log("\n=== p2p_rates result ===");
        console.log(JSON.stringify(response, null, 2));
        console.log("\n=== SUCCESS ===");
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

  // Timeout after 20 seconds
  setTimeout(() => {
    console.log("TIMEOUT - no response received");
    process.exit(1);
  }, 20000);
}

test();
