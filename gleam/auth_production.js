#!/usr/bin/env node
// Production Telegram Authorization Script

import https from 'https';

const BRIDGE_URL = 'vibee-telegram-bridge.fly.dev';
const API_KEY = 'vibee-secret-key-2024';
const PHONE = '+79933420465';
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
    console.log('Creating new session with app_id:', APP_ID);
    const result = await makeRequest('/api/v1/connect', 'POST', {
      app_id: APP_ID,
      app_hash: APP_HASH
    });
    console.log(JSON.stringify(result, null, 2));
    if (result.session_id) {
      console.log('\nRun: SESSION_ID=' + result.session_id + ' node auth_production.js send');
    }
  }
  else if (action === 'send') {
    if (!SESSION_ID) {
      console.log('No SESSION_ID set. First run: node auth_production.js connect');
      return;
    }
    console.log('Sending auth code to', PHONE, 'with session', SESSION_ID);
    const result = await makeRequest('/api/v1/auth/phone', 'POST', { phone: PHONE });
    console.log(JSON.stringify(result, null, 2));
  }
  else if (action === 'verify') {
    const code = process.argv[3];
    const hash = process.argv[4];
    console.log('Verifying code:', code);
    const result = await makeRequest('/api/v1/auth/code', 'POST', {
      phone: PHONE,
      code: code,
      phone_code_hash: hash
    });
    console.log(JSON.stringify(result, null, 2));
  }
  else if (action === '2fa') {
    const password = process.argv[3];
    console.log('Submitting 2FA password');
    const result = await makeRequest('/api/v1/auth/2fa', 'POST', {
      password: password
    });
    console.log(JSON.stringify(result, null, 2));
  }
  else {
    console.log('Usage:');
    console.log('  node auth_production.js send');
    console.log('  node auth_production.js verify <code> <hash>');
    console.log('  node auth_production.js 2fa <password>');
  }
}

main().catch(console.error);
