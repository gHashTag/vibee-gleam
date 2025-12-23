// Test telegram-bridge API directly
const API_KEY = "vibee-mcp-secret-2024";
const BRIDGE_URL = "https://vibee-telegram-bridge.fly.dev";

async function testBridge() {
  console.log("Testing telegram-bridge API...\n");

  // Test 1: Auth status (list all sessions)
  console.log("1. Getting all sessions from bridge...");
  try {
    const resp = await fetch(`${BRIDGE_URL}/api/v1/auth/status`, {
      headers: {
        "Authorization": `Bearer ${API_KEY}`
      }
    });
    const data = await resp.json();
    console.log(`   Status: ${resp.status}`);
    console.log(`   Sessions: ${JSON.stringify(data, null, 2)}`);
  } catch (e) {
    console.log(`   Error: ${e}`);
  }

  console.log("\n2. Getting dialogs with session...");
  const SESSION_ID = "sess_deyznjf1vzn6"; // NEURO_SAGE
  try {
    const resp = await fetch(`${BRIDGE_URL}/api/v1/dialogs?limit=5`, {
      headers: {
        "Authorization": `Bearer ${API_KEY}`,
        "X-Session-ID": SESSION_ID
      }
    });
    const data = await resp.json();
    console.log(`   Status: ${resp.status}`);
    console.log(`   Dialogs count: ${data.dialogs?.length || 0}`);
    if (data.dialogs?.[0]) {
      console.log(`   First dialog: ${JSON.stringify(data.dialogs[0], null, 2)}`);
    }
  } catch (e) {
    console.log(`   Error: ${e}`);
  }
}

testBridge();
