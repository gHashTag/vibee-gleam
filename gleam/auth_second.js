#!/usr/bin/env node
// Authorization for second account (Thai number)

import https from 'https';

const BRIDGE_URL = 'vibee-telegram-bridge.fly.dev';
const API_KEY = 'vibee-secret-key-2024';
const PHONE = '+66624014170';
const APP_ID = 94892;
const APP_HASH = 'REDACTED_API_HASH';

let SESSION_ID = process.env.SESSION_ID || '';

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

async function main() {
  const action = process.argv[2];

  if (action === 'connect') {
    console.log('Creating new session for', PHONE);
    const result = await makeRequest('/api/v1/connect', 'POST', {
      app_id: APP_ID,
      app_hash: APP_HASH
    });
    console.log(JSON.stringify(result, null, 2));
    if (result.session_id) {
      console.log('\nRun: SESSION_ID=' + result.session_id + ' node auth_second.js send');
    }
  }
  else if (action === 'send') {
    if (!SESSION_ID) {
      console.log('No SESSION_ID. First run: node auth_second.js connect');
      return;
    }
    console.log('Sending code to', PHONE);
    const result = await makeRequest('/api/v1/auth/phone', 'POST', { phone: PHONE });
    console.log(JSON.stringify(result, null, 2));
    if (result.phone_code_hash) {
      console.log('\nWait for SMS, then run:');
      console.log('SESSION_ID=' + SESSION_ID + ' node auth_second.js verify <CODE> ' + result.phone_code_hash);
    }
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
  else if (action === 'me') {
    console.log('Getting user info...');
    const result = await makeRequest('/api/v1/me', 'GET');
    console.log(JSON.stringify(result, null, 2));
  }
  else if (action === 'send-msg') {
    const text = process.argv[3] || 'Привет! Как дела с VIBEE?';
    console.log('Sending message to @neuro_sage:', text);
    const result = await makeRequest('/api/v1/send', 'POST', {
      chat_id: 144022504,  // @neuro_sage user_id
      text: text
    });
    console.log(JSON.stringify(result, null, 2));
  }
  else {
    console.log('Usage:');
    console.log('  node auth_second.js connect');
    console.log('  SESSION_ID=xxx node auth_second.js send');
    console.log('  SESSION_ID=xxx node auth_second.js verify <code> <hash>');
    console.log('  SESSION_ID=xxx node auth_second.js 2fa <password>');
    console.log('  SESSION_ID=xxx node auth_second.js me');
    console.log('  SESSION_ID=xxx node auth_second.js send-msg "текст"');
  }
}

main().catch(console.error);
