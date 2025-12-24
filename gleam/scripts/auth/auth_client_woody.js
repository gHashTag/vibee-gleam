#!/usr/bin/env node
// Client @Best_WoodyWeed Telegram Authorization Script
// Phone: +66 6 4843 5742

import https from 'https';

const BRIDGE_URL = 'vibee-telegram-bridge.fly.dev';
const API_KEY = process.env.VIBEE_API_KEY || 'vibee-secret-2024-prod';
const PHONE = '+66648435742';  // Thailand format: +66 64 843 5742
const APP_ID = 94892;
const APP_HASH = 'cacf9ad137d228611b49b2ecc6d68d43';

function makeRequest(path, method, body) {
  return new Promise((resolve, reject) => {
    const headers = {
      'Content-Type': 'application/json',
      'Authorization': 'Bearer ' + API_KEY,
      'X-API-Key': API_KEY,
    };
    if (SESSION_ID) {
      headers['X-Session-ID'] = SESSION_ID;
    }
    const options = {
      hostname: BRIDGE_URL,
      port: 443,
      path: path,
      method: method,
      headers: headers,
    };

    const req = https.request(options, (res) => {
      let data = '';
      res.on('data', (chunk) => data += chunk);
      res.on('end', () => {
        try {
          resolve(JSON.parse(data));
        } catch (e) {
          resolve(data);
        }
      });
    });

    req.on('error', reject);

    if (body) {
      req.write(JSON.stringify(body));
    }
    req.end();
  });
}

let SESSION_ID = process.env.SESSION_ID || '';

async function main() {
  const action = process.argv[2];

  if (action === 'connect') {
    console.log('Creating new session for @Best_WoodyWeed');
    console.log('App ID:', APP_ID);
    const result = await makeRequest('/api/v1/connect', 'POST', {
      app_id: APP_ID,
      app_hash: APP_HASH
    });
    console.log(JSON.stringify(result, null, 2));
    if (result.session_id) {
      console.log('\n=== NEXT STEP ===');
      console.log('Run: SESSION_ID=' + result.session_id + ' node scripts/auth/auth_client_woody.js send');
    }
  }
  else if (action === 'send') {
    if (!SESSION_ID) {
      console.log('No SESSION_ID set. First run: node scripts/auth/auth_client_woody.js connect');
      return;
    }
    console.log('Sending auth code to', PHONE, 'with session', SESSION_ID);
    const result = await makeRequest('/api/v1/auth/phone', 'POST', { phone: PHONE });
    console.log(JSON.stringify(result, null, 2));
    if (result.phone_code_hash) {
      console.log('\n=== NEXT STEP ===');
      console.log('When you receive the code, run:');
      console.log('SESSION_ID=' + SESSION_ID + ' node scripts/auth/auth_client_woody.js verify <CODE> ' + result.phone_code_hash);
    }
  }
  else if (action === 'verify') {
    const code = process.argv[3];
    const hash = process.argv[4];
    if (!code || !hash) {
      console.log('Usage: node auth_client_woody.js verify <CODE> <HASH>');
      return;
    }
    console.log('Verifying code:', code);
    const result = await makeRequest('/api/v1/auth/code', 'POST', {
      phone: PHONE,
      code: code,
      phone_code_hash: hash
    });
    console.log(JSON.stringify(result, null, 2));
    if (result.status === 'authorized') {
      console.log('\n=== SUCCESS ===');
      console.log('Session authorized! Save this session_id:', SESSION_ID);
    } else if (result.requires_2fa) {
      console.log('\n=== 2FA REQUIRED ===');
      console.log('Run: SESSION_ID=' + SESSION_ID + ' node scripts/auth/auth_client_woody.js 2fa <PASSWORD>');
    }
  }
  else if (action === '2fa') {
    const password = process.argv[3];
    if (!password) {
      console.log('Usage: node auth_client_woody.js 2fa <PASSWORD>');
      return;
    }
    console.log('Submitting 2FA password');
    const result = await makeRequest('/api/v1/auth/2fa', 'POST', {
      password: password
    });
    console.log(JSON.stringify(result, null, 2));
  }
  else {
    console.log('=== @Best_WoodyWeed Authorization ===');
    console.log('Phone: ' + PHONE);
    console.log('');
    console.log('Usage:');
    console.log('  1. node scripts/auth/auth_client_woody.js connect');
    console.log('  2. SESSION_ID=xxx node scripts/auth/auth_client_woody.js send');
    console.log('  3. SESSION_ID=xxx node scripts/auth/auth_client_woody.js verify <code> <hash>');
    console.log('  4. SESSION_ID=xxx node scripts/auth/auth_client_woody.js 2fa <password>  (if needed)');
  }
}

main().catch(console.error);
