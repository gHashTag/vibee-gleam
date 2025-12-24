#!/usr/bin/env node
// Check all sessions on bridge to find different accounts

import https from 'https';

const BRIDGE_URL = 'vibee-telegram-bridge.fly.dev';
const API_KEY = 'vibee-secret-key-2024';

const SESSIONS = [
  'sess_deysqvuushz9',
  'sess_deyt2m0f63rq',
  'sess_deytwy41y4oi',
  'sess_deyy8tm312z6',
  'sess_deyyfxjyao6q',
  'sess_deyyqx1brjkz',
  'sess_deyyxfssk6o8',
  'sess_deyz1a1y6ab5',
];

function makeRequest(path, method, sessionId) {
  return new Promise((resolve, reject) => {
    const options = {
      hostname: BRIDGE_URL,
      port: 443,
      path: path,
      method: method,
      headers: {
        'Content-Type': 'application/json',
        'Authorization': 'Bearer ' + API_KEY,
        'X-Session-ID': sessionId,
      },
    };

    const req = https.request(options, (res) => {
      let data = '';
      res.on('data', (chunk) => data += chunk);
      res.on('end', () => {
        try {
          resolve(JSON.parse(data));
        } catch (e) {
          resolve({ error: data });
        }
      });
    });

    req.on('error', reject);
    req.end();
  });
}

async function main() {
  console.log('Checking all sessions on bridge...\n');

  for (const sessionId of SESSIONS) {
    try {
      const result = await makeRequest('/api/v1/me', 'GET', sessionId);
      if (result.error) {
        console.log(`${sessionId}: ERROR - ${result.error}`);
      } else if (result.id) {
        console.log(`${sessionId}: @${result.username || 'no_username'} (ID: ${result.id})`);
      } else {
        console.log(`${sessionId}: ${JSON.stringify(result)}`);
      }
    } catch (err) {
      console.log(`${sessionId}: FAILED - ${err.message}`);
    }
  }
}

main().catch(console.error);
