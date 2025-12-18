# Multi-Tenant Session Management

## Overview

VIBEE supports multi-tenant mode where each client has their own Telegram session stored securely in PostgreSQL.

## Architecture

```
┌─────────────┐
│   Client 1  │──┐
└─────────────┘  │
                 │    ┌──────────────┐      ┌─────────────┐
┌─────────────┐  │    │              │      │             │
│   Client 2  │──┼───▶│  VIBEE Agent │─────▶│  PostgreSQL │
└─────────────┘  │    │              │      │  (Sessions) │
                 │    └──────────────┘      └─────────────┘
┌─────────────┐  │
│   Client N  │──┘
└─────────────┘
```

## Database Schema

### `user_sessions` table

Stores encrypted Telegram credentials per user:

```sql
CREATE TABLE user_sessions (
    id SERIAL PRIMARY KEY,
    user_id VARCHAR(255) NOT NULL UNIQUE,
    session_id VARCHAR(255) NOT NULL UNIQUE,
    api_id INTEGER NOT NULL,
    api_hash_encrypted TEXT NOT NULL,  -- AES-256 encrypted
    is_active BOOLEAN DEFAULT true,
    digital_twin_enabled BOOLEAN DEFAULT false,
    ...
);
```

### Encryption

- API hash is encrypted using PostgreSQL's `pgp_sym_encrypt()`
- Master key stored in `encryption_keys` table
- **Production**: Replace default key with secure 32-byte key

## Setup

### 1. Run Migration

```bash
psql $DATABASE_URL < migrations/001_user_sessions.sql
```

### 2. Add User Session

```sql
-- Insert new user session
INSERT INTO user_sessions (
    user_id, session_id, api_id, api_hash_encrypted, phone
) VALUES (
    '144022504',  -- Telegram user ID
    'sess_df0p27qhvzvv',  -- From telegram-bridge
    94892,
    pgp_sym_encrypt('cacf9ad137d228611b49b2ecc6d68d43', 
        (SELECT master_key FROM encryption_keys WHERE id = 1)),
    '+79933420465'
);
```

### 3. Start Agent

**Option A: Single user mode (from DB)**
```bash
export DATABASE_URL=postgresql://...
export USER_ID=144022504
./start.sh
```

**Option B: Multi-tenant mode (all active sessions)**
```bash
export DATABASE_URL=postgresql://...
export MULTI_TENANT=true
./start.sh
```

**Option C: Legacy mode (from env vars)**
```bash
export TELEGRAM_SESSION_ID=sess_xxx
export TELEGRAM_API_ID=94892
export TELEGRAM_API_HASH=xxx
./start.sh
```

## API Endpoints

### Add/Update Session

```bash
POST /api/v1/sessions
Content-Type: application/json

{
  "user_id": "144022504",
  "session_id": "sess_df0p27qhvzvv",
  "api_id": 94892,
  "api_hash": "cacf9ad137d228611b49b2ecc6d68d43",
  "phone": "+79933420465"
}
```

### Get Session

```bash
GET /api/v1/sessions/:user_id
```

### List Active Sessions

```bash
GET /api/v1/sessions
```

### Deactivate Session

```bash
DELETE /api/v1/sessions/:session_id
```

## Security Best Practices

1. **Encryption Key**: Replace default master key in production
   ```sql
   UPDATE encryption_keys 
   SET master_key = 'YOUR_SECURE_32_BYTE_KEY_HERE'
   WHERE id = 1;
   ```

2. **Database Access**: Use SSL/TLS for PostgreSQL connections
   ```
   DATABASE_URL=postgresql://...?sslmode=require
   ```

3. **API Authentication**: Add JWT/API key auth to session endpoints

4. **Audit Logging**: Monitor `session_activity` table for suspicious activity

## Migration from Environment Variables

1. Export current session to database:
   ```bash
   ./scripts/migrate-env-to-db.sh
   ```

2. Verify session in database:
   ```sql
   SELECT user_id, session_id, is_active 
   FROM user_sessions 
   WHERE user_id = 'YOUR_USER_ID';
   ```

3. Remove environment variables from `.env`

4. Restart agent - it will auto-load from database

## Troubleshooting

### Session not loading

```bash
# Check database connection
psql $DATABASE_URL -c "SELECT version();"

# Check session exists
psql $DATABASE_URL -c "SELECT user_id, session_id, is_active FROM user_sessions;"

# Check encryption key
psql $DATABASE_URL -c "SELECT id FROM encryption_keys WHERE id = 1;"
```

### Decryption errors

```bash
# Test decryption manually
psql $DATABASE_URL -c "
SELECT pgp_sym_decrypt(api_hash_encrypted::bytea, 
    (SELECT master_key FROM encryption_keys WHERE id = 1))
FROM user_sessions WHERE user_id = 'YOUR_USER_ID';
"
```

## Performance

- **Connection Pooling**: Uses `pog` connection pool (default: 10 connections)
- **Caching**: Session configs cached in memory after first load
- **Lazy Loading**: Sessions loaded on-demand, not at startup

## Monitoring

```sql
-- Active sessions count
SELECT COUNT(*) FROM user_sessions WHERE is_active = true;

-- Recent activity
SELECT session_id, activity_type, created_at 
FROM session_activity 
ORDER BY created_at DESC 
LIMIT 10;

-- Sessions by authorization status
SELECT is_authorized, COUNT(*) 
FROM user_sessions 
WHERE is_active = true 
GROUP BY is_authorized;
```
