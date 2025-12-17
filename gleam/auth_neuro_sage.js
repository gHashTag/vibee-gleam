#!/usr/bin/env node
// Fresh authorization for @neuro_sage

import https from 'https';

const BRIDGE_URL = 'vibee-telegram-bridge.fly.dev';
const API_KEY = 'vibee-secret-key-2024';
const PHONE = '+79933420465';
const APP_ID = 94892;
const APP_HASH = 'cacf9ad137d228611b49b2ecc6d68d43';

let SESSION_ID = process.env.SESSION_ID || '';

function makeRequest(path, method, body) {
  return new Promise((resolve, reject) => {
    const headers = {
      'Content-Type': 'application/json',
      'Authorization': 'Bearer ' + API_KEY,
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
    if (body) req.write(JSON.stringify(body));
    req.end();
  });
}

async function main() {
  const action = process.argv[2];

  if (action === 'connect') {
    console.log('Creating fresh session for @neuro_sage');
    const result = await makeRequest('/api/v1/connect', 'POST', {
      app_id: APP_ID,
      app_hash: APP_HASH
    });
    console.log(JSON.stringify(result, null, 2));
  }
  else if (action === 'send') {
    console.log('Sending code to', PHONE);
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
    console.log('Submitting 2FA');
    const result = await makeRequest('/api/v1/auth/2fa', 'POST', { password: password });
    console.log(JSON.stringify(result, null, 2));
  }
  else if (action === 'dialogs') {
    console.log('Getting dialogs...');
    const result = await makeRequest('/api/v1/dialogs?limit=5', 'GET');
    console.log(JSON.stringify(result, null, 2));
  }
  else {
    console.log('Usage: SESSION_ID=xxx node auth_neuro_sage.js [connect|send|verify|2fa|dialogs]');
  }
}

main().catch(console.error);
