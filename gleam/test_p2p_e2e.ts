// E2E Test for P2P MCP Tools
// Run with: bun test_p2p_e2e.ts

const WS_URL = "ws://localhost:8080/ws/mcp";

interface JsonRpcRequest {
  jsonrpc: "2.0";
  id: number;
  method: string;
  params?: any;
}

interface JsonRpcResponse {
  jsonrpc: "2.0";
  id: number;
  result?: any;
  error?: { code: number; message: string };
}

let requestId = 1;

async function callMcpTool(
  ws: WebSocket,
  toolName: string,
  args: Record<string, any> = {}
): Promise<any> {
  return new Promise((resolve, reject) => {
    const id = requestId++;
    const request: JsonRpcRequest = {
      jsonrpc: "2.0",
      id,
      method: "tools/call",
      params: {
        name: toolName,
        arguments: args,
      },
    };

    const timeout = setTimeout(() => {
      reject(new Error(`Timeout waiting for response to ${toolName}`));
    }, 10000);

    const handler = (event: MessageEvent) => {
      try {
        const response: JsonRpcResponse = JSON.parse(event.data);
        if (response.id === id) {
          clearTimeout(timeout);
          ws.removeEventListener("message", handler);
          if (response.error) {
            reject(new Error(response.error.message));
          } else {
            resolve(response.result);
          }
        }
      } catch (e) {
        // Ignore parse errors for other messages
      }
    };

    ws.addEventListener("message", handler);
    ws.send(JSON.stringify(request));
  });
}

async function listTools(ws: WebSocket): Promise<any[]> {
  return new Promise((resolve, reject) => {
    const id = requestId++;
    const request: JsonRpcRequest = {
      jsonrpc: "2.0",
      id,
      method: "tools/list",
    };

    const timeout = setTimeout(() => {
      reject(new Error("Timeout waiting for tools list"));
    }, 10000);

    const handler = (event: MessageEvent) => {
      try {
        const response: JsonRpcResponse = JSON.parse(event.data);
        if (response.id === id) {
          clearTimeout(timeout);
          ws.removeEventListener("message", handler);
          if (response.error) {
            reject(new Error(response.error.message));
          } else {
            resolve(response.result?.tools || []);
          }
        }
      } catch (e) {
        // Ignore parse errors
      }
    };

    ws.addEventListener("message", handler);
    ws.send(JSON.stringify(request));
  });
}

async function runTests() {
  console.log("🚀 Starting P2P E2E Tests\n");
  console.log("Connecting to:", WS_URL);

  const ws = new WebSocket(WS_URL);

  await new Promise<void>((resolve, reject) => {
    ws.onopen = () => {
      console.log("✅ WebSocket connected\n");
      resolve();
    };
    ws.onerror = (err) => {
      console.error("❌ WebSocket error:", err);
      reject(err);
    };
    setTimeout(() => reject(new Error("Connection timeout")), 5000);
  });

  let passed = 0;
  let failed = 0;

  async function test(name: string, fn: () => Promise<void>) {
    try {
      await fn();
      console.log(`✅ ${name}`);
      passed++;
    } catch (error: any) {
      console.log(`❌ ${name}: ${error.message}`);
      failed++;
    }
  }

  // ============ TEST: List Tools ============
  await test("List all MCP tools", async () => {
    const tools = await listTools(ws);
    console.log(`   Found ${tools.length} tools`);

    const p2pTools = tools.filter((t: any) => t.name.startsWith("p2p_"));
    console.log(`   P2P tools: ${p2pTools.map((t: any) => t.name).join(", ")}`);

    if (p2pTools.length < 9) {
      throw new Error(`Expected 9 P2P tools, found ${p2pTools.length}`);
    }
  });

  // ============ TEST: p2p_rates ============
  await test("p2p_rates - Get market rates", async () => {
    const result = await callMcpTool(ws, "p2p_rates", {});
    console.log(`   Rates:`, JSON.stringify(result).substring(0, 100));

    if (!result.content || result.is_error) {
      throw new Error("Failed to get rates");
    }
  });

  // ============ TEST: p2p_list_orders ============
  await test("p2p_list_orders - List orders", async () => {
    const result = await callMcpTool(ws, "p2p_list_orders", {
      crypto: "USDT",
      fiat: "THB",
    });
    console.log(`   Orders result:`, JSON.stringify(result).substring(0, 100));
  });

  // ============ TEST: p2p_create_sell_order ============
  const testSellerId = 123456789;
  let orderId: string | null = null;

  await test("p2p_create_sell_order - Create sell order", async () => {
    const result = await callMcpTool(ws, "p2p_create_sell_order", {
      seller_telegram_id: testSellerId,
      crypto: "USDT",
      crypto_amount: 100.0,
      fiat: "THB",
      fiat_amount: 3500.0,
      payment_method: "promptpay",
      payment_details: "PromptPay: 0812345678",
    });

    console.log(`   Created order:`, JSON.stringify(result).substring(0, 150));

    // Extract order ID from response
    const content = result.content?.[0]?.text || "";
    const match = content.match(/P2P-\d+-\d+/);
    if (match) {
      orderId = match[0];
      console.log(`   Order ID: ${orderId}`);
    }
  });

  // ============ TEST: p2p_order_status ============
  if (orderId) {
    await test("p2p_order_status - Get order status", async () => {
      const result = await callMcpTool(ws, "p2p_order_status", {
        order_id: orderId,
      });
      console.log(`   Order status:`, JSON.stringify(result).substring(0, 150));
    });
  }

  // ============ TEST: p2p_my_orders ============
  await test("p2p_my_orders - List user's orders", async () => {
    const result = await callMcpTool(ws, "p2p_my_orders", {
      telegram_id: testSellerId,
    });
    console.log(`   My orders:`, JSON.stringify(result).substring(0, 150));
  });

  // ============ TEST: p2p_take_order ============
  const testBuyerId = 987654321;

  if (orderId) {
    await test("p2p_take_order - Take order as buyer", async () => {
      const result = await callMcpTool(ws, "p2p_take_order", {
        order_id: orderId,
        buyer_telegram_id: testBuyerId,
      });
      console.log(`   Take order:`, JSON.stringify(result).substring(0, 150));
    });
  }

  // ============ TEST: p2p_mark_paid ============
  if (orderId) {
    await test("p2p_mark_paid - Mark fiat payment sent", async () => {
      const result = await callMcpTool(ws, "p2p_mark_paid", {
        order_id: orderId,
        buyer_telegram_id: testBuyerId,
      });
      console.log(`   Mark paid:`, JSON.stringify(result).substring(0, 150));
    });
  }

  // ============ TEST: p2p_confirm_payment ============
  if (orderId) {
    await test("p2p_confirm_payment - Confirm fiat received", async () => {
      const result = await callMcpTool(ws, "p2p_confirm_payment", {
        order_id: orderId,
        seller_telegram_id: testSellerId,
      });
      console.log(`   Confirm:`, JSON.stringify(result).substring(0, 150));
    });
  }

  // ============ TEST: Create another order to test cancel ============
  let cancelOrderId: string | null = null;

  await test("p2p_create_sell_order - Create order for cancel test", async () => {
    const result = await callMcpTool(ws, "p2p_create_sell_order", {
      seller_telegram_id: testSellerId,
      crypto: "TON",
      crypto_amount: 50.0,
      fiat: "RUB",
      fiat_amount: 5000.0,
      payment_method: "sberbank",
      payment_details: "Sberbank: 4276 1234 5678 9012",
    });

    const content = result.content?.[0]?.text || "";
    const match = content.match(/P2P-\d+-\d+/);
    if (match) {
      cancelOrderId = match[0];
      console.log(`   Order ID: ${cancelOrderId}`);
    }
  });

  // ============ TEST: p2p_cancel_order ============
  if (cancelOrderId) {
    await test("p2p_cancel_order - Cancel order", async () => {
      const result = await callMcpTool(ws, "p2p_cancel_order", {
        order_id: cancelOrderId,
        telegram_id: testSellerId,
      });
      console.log(`   Cancel:`, JSON.stringify(result).substring(0, 150));
    });
  }

  // Close WebSocket
  ws.close();

  // Summary
  console.log("\n" + "=".repeat(50));
  console.log(`📊 Test Results: ${passed} passed, ${failed} failed`);
  console.log("=".repeat(50));

  if (failed > 0) {
    process.exit(1);
  }
}

runTests().catch((err) => {
  console.error("Fatal error:", err);
  process.exit(1);
});
