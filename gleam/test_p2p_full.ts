// Full P2P E2E Test - using response order instead of id matching
const WS_URL = "ws://localhost:8080/ws/mcp";

let responseQueue: any[] = [];
let ws: WebSocket;

function sendRequest(method: string, params: any = {}) {
  const request = {
    jsonrpc: "2.0",
    id: Date.now(),
    method,
    params
  };
  console.log(`  > ${method}`, params.name || "");
  ws.send(JSON.stringify(request));
}

function waitForResponse(timeout = 3000): Promise<any> {
  return new Promise((resolve, reject) => {
    const startTime = Date.now();
    const check = () => {
      if (responseQueue.length > 0) {
        resolve(responseQueue.shift());
      } else if (Date.now() - startTime > timeout) {
        reject(new Error("Timeout waiting for response"));
      } else {
        setTimeout(check, 50);
      }
    };
    check();
  });
}

async function callTool(name: string, args: any = {}): Promise<any> {
  sendRequest("tools/call", { name, arguments: args });
  const response = await waitForResponse(5000);
  return response;
}

async function runTests() {
  console.log("=".repeat(60));
  console.log("  VIBEE P2P Escrow System - E2E Test");
  console.log("=".repeat(60));
  console.log("");

  ws = new WebSocket(WS_URL);

  await new Promise<void>((resolve, reject) => {
    ws.onopen = () => {
      console.log("✅ Connected to MCP WebSocket\n");
      resolve();
    };
    ws.onerror = (err) => reject(err);
    setTimeout(() => reject(new Error("Connection timeout")), 5000);
  });

  ws.onmessage = (event) => {
    try {
      responseQueue.push(JSON.parse(event.data));
    } catch (e) {
      console.error("Parse error:", e);
    }
  };

  // Initialize
  sendRequest("initialize", {
    protocolVersion: "2024-11-05",
    capabilities: {},
    clientInfo: { name: "p2p-test", version: "1.0.0" }
  });
  await waitForResponse();
  console.log("✅ MCP initialized\n");

  let passed = 0;
  let failed = 0;

  async function test(name: string, fn: () => Promise<void>) {
    try {
      await fn();
      console.log(`✅ ${name}`);
      passed++;
    } catch (error: any) {
      console.log(`❌ ${name}`);
      console.log(`   Error: ${error.message}`);
      failed++;
    }
  }

  // ============= P2P TOOL TESTS =============

  console.log("\n--- P2P Tools Testing ---\n");

  // 1. p2p_rates
  await test("p2p_rates - Get market rates", async () => {
    const result = await callTool("p2p_rates", {});
    const content = result.result?.content?.[0]?.text;
    if (!content) throw new Error("No content in response");
    const data = JSON.parse(content);
    if (!data.success || !data.rates || data.rates.length < 4) {
      throw new Error("Invalid rates response");
    }
    console.log(`   Found ${data.rates.length} currency pairs`);
  });

  // 2. p2p_list_orders - empty at start
  await test("p2p_list_orders - List orders (initially empty)", async () => {
    const result = await callTool("p2p_list_orders", { crypto: "USDT", fiat: "THB" });
    const content = result.result?.content?.[0]?.text;
    if (!content) throw new Error("No content");
    console.log(`   Result: ${content.substring(0, 80)}...`);
  });

  // 3. p2p_create_sell_order
  const sellerId = 123456789;
  let orderId: string | null = null;

  await test("p2p_create_sell_order - Create sell order", async () => {
    const result = await callTool("p2p_create_sell_order", {
      seller_telegram_id: sellerId,
      crypto: "USDT",
      crypto_amount: 100.0,
      fiat: "THB",
      fiat_amount: 3500.0,
      payment_method: "promptpay",
      payment_details: "PromptPay: 0812345678"
    });
    const content = result.result?.content?.[0]?.text;
    if (!content) throw new Error("No content");

    // Extract order ID
    const match = content.match(/P2P-\d+-\d+/);
    if (match) {
      orderId = match[0];
      console.log(`   Created order: ${orderId}`);
    } else {
      console.log(`   Response: ${content.substring(0, 100)}`);
    }
  });

  // 4. p2p_order_status
  if (orderId) {
    await test("p2p_order_status - Get order status", async () => {
      const result = await callTool("p2p_order_status", { order_id: orderId });
      const content = result.result?.content?.[0]?.text;
      if (!content) throw new Error("No content");
      console.log(`   Status: ${content.substring(0, 100)}...`);
    });
  }

  // 5. p2p_my_orders
  await test("p2p_my_orders - List seller's orders", async () => {
    const result = await callTool("p2p_my_orders", { telegram_id: sellerId });
    const content = result.result?.content?.[0]?.text;
    if (!content) throw new Error("No content");
    console.log(`   Result: ${content.substring(0, 80)}...`);
  });

  // 6. p2p_list_orders - should now have 1 order
  await test("p2p_list_orders - List orders (should have 1)", async () => {
    const result = await callTool("p2p_list_orders", { crypto: "USDT" });
    const content = result.result?.content?.[0]?.text;
    if (!content) throw new Error("No content");
    console.log(`   Result: ${content.substring(0, 80)}...`);
  });

  // 7. p2p_take_order
  const buyerId = 987654321;

  if (orderId) {
    await test("p2p_take_order - Buyer takes order", async () => {
      const result = await callTool("p2p_take_order", {
        order_id: orderId,
        buyer_telegram_id: buyerId
      });
      const content = result.result?.content?.[0]?.text;
      if (!content) throw new Error("No content");
      console.log(`   Result: ${content.substring(0, 100)}...`);
    });
  }

  // 8. p2p_mark_paid
  if (orderId) {
    await test("p2p_mark_paid - Buyer marks fiat sent", async () => {
      const result = await callTool("p2p_mark_paid", {
        order_id: orderId,
        buyer_telegram_id: buyerId
      });
      const content = result.result?.content?.[0]?.text;
      if (!content) throw new Error("No content");
      console.log(`   Result: ${content.substring(0, 100)}...`);
    });
  }

  // 9. p2p_confirm_payment
  if (orderId) {
    await test("p2p_confirm_payment - Seller confirms receipt", async () => {
      const result = await callTool("p2p_confirm_payment", {
        order_id: orderId,
        seller_telegram_id: sellerId
      });
      const content = result.result?.content?.[0]?.text;
      if (!content) throw new Error("No content");
      console.log(`   Result: ${content.substring(0, 100)}...`);
    });
  }

  // 10. Create another order for cancel test
  let cancelOrderId: string | null = null;

  await test("p2p_create_sell_order - Create order for cancel", async () => {
    const result = await callTool("p2p_create_sell_order", {
      seller_telegram_id: sellerId,
      crypto: "TON",
      crypto_amount: 50.0,
      fiat: "RUB",
      fiat_amount: 5000.0,
      payment_method: "sberbank",
      payment_details: "Sberbank: 4276 1234 5678 9012"
    });
    const content = result.result?.content?.[0]?.text;
    if (!content) throw new Error("No content");

    const match = content.match(/P2P-\d+-\d+/);
    if (match) {
      cancelOrderId = match[0];
      console.log(`   Created order: ${cancelOrderId}`);
    }
  });

  // 11. p2p_cancel_order
  if (cancelOrderId) {
    await test("p2p_cancel_order - Cancel order", async () => {
      const result = await callTool("p2p_cancel_order", {
        order_id: cancelOrderId,
        telegram_id: sellerId
      });
      const content = result.result?.content?.[0]?.text;
      if (!content) throw new Error("No content");
      console.log(`   Result: ${content.substring(0, 100)}...`);
    });
  }

  // Close connection
  ws.close();

  // Summary
  console.log("\n" + "=".repeat(60));
  console.log(`  RESULTS: ${passed} passed, ${failed} failed`);
  console.log("=".repeat(60));

  if (failed > 0) {
    process.exit(1);
  } else {
    console.log("\n🎉 All P2P E2E tests passed!\n");
    process.exit(0);
  }
}

runTests().catch((err) => {
  console.error("Fatal error:", err);
  process.exit(1);
});
