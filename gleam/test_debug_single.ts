// Test a single p2p_list_orders call with extended timeout
const WS_URL = "ws://localhost:8080/ws/mcp";

async function test() {
  console.log("Testing p2p_list_orders...");
  
  const ws = new WebSocket(WS_URL);
  
  await new Promise<void>((resolve, reject) => {
    ws.onopen = () => {
      console.log("Connected");
      resolve();
    };
    ws.onerror = reject;
    setTimeout(() => reject(new Error("Timeout")), 5000);
  });

  ws.onmessage = (event) => {
    console.log("Response:", event.data);
  };

  // Initialize
  console.log("Sending initialize...");
  ws.send(JSON.stringify({
    jsonrpc: "2.0",
    id: 1,
    method: "initialize",
    params: {
      protocolVersion: "2024-11-05",
      capabilities: {},
      clientInfo: { name: "test", version: "1.0.0" }
    }
  }));

  await new Promise(r => setTimeout(r, 1000));

  // Call p2p_list_orders
  console.log("\nSending p2p_list_orders...");
  const request = {
    jsonrpc: "2.0",
    id: 2,
    method: "tools/call",
    params: {
      name: "p2p_list_orders",
      arguments: {
        crypto: "USDT",
        fiat: "THB"
      }
    }
  };
  console.log("Request:", JSON.stringify(request, null, 2));
  ws.send(JSON.stringify(request));

  // Wait for response
  await new Promise(r => setTimeout(r, 10000));
  
  console.log("\nDone");
  ws.close();
  process.exit(0);
}

test().catch(err => {
  console.error("Error:", err);
  process.exit(1);
});
