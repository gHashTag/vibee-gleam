# VIBEE Session Management - Quick Start

## Проблема

Раньше: каждый клиент должен был вручную устанавливать переменные окружения `TELEGRAM_SESSION_ID`, `TELEGRAM_API_ID`, `TELEGRAM_API_HASH`.

Теперь: все сессии хранятся в PostgreSQL с шифрованием, автоматически загружаются при старте.

## Решение

### 1. Запустите миграцию БД

```bash
# Установите psql если нужно
apt-get install postgresql-client

# Запустите миграцию
psql "$DATABASE_URL" < migrations/001_user_sessions.sql
```

### 2. Мигрируйте существующую сессию

```bash
# Если у вас уже есть .env с TELEGRAM_SESSION_ID
./scripts/migrate-env-to-db.sh
```

Или вручную:

```sql
INSERT INTO user_sessions (
    user_id, session_id, api_id, api_hash_encrypted, phone
) VALUES (
    '144022504',  -- Ваш Telegram user ID
    'sess_df0p27qhvzvv',  -- Из telegram-bridge
    94892,
    pgp_sym_encrypt('cacf9ad137d228611b49b2ecc6d68d43', 
        (SELECT master_key FROM encryption_keys WHERE id = 1)),
    '+79933420465'
);
```

### 3. Запустите агента

**Вариант A: Один пользователь из БД**
```bash
export DATABASE_URL=postgresql://...
export USER_ID=144022504
./start.sh
```

**Вариант B: Все активные сессии (multi-tenant)**
```bash
export DATABASE_URL=postgresql://...
export MULTI_TENANT=true
./start.sh
```

**Вариант C: Legacy (из env)**
```bash
export TELEGRAM_SESSION_ID=sess_xxx
export TELEGRAM_API_ID=94892
export TELEGRAM_API_HASH=xxx
./start.sh
```

## Для новых клиентов

### Через API

```bash
# Добавить нового клиента
curl -X POST http://localhost:8080/api/v1/sessions \
  -H "Content-Type: application/json" \
  -d '{
    "user_id": "123456789",
    "session_id": "sess_new_client",
    "api_id": 94892,
    "api_hash": "client_api_hash",
    "phone": "+1234567890"
  }'
```

### Через SQL

```sql
INSERT INTO user_sessions (
    user_id, session_id, api_id, api_hash_encrypted, phone
) VALUES (
    'NEW_USER_ID',
    'sess_new_session',
    94892,
    pgp_sym_encrypt('NEW_API_HASH', 
        (SELECT master_key FROM encryption_keys WHERE id = 1)),
    '+1234567890'
);
```

## Безопасность

### Смените ключ шифрования

```bash
# Сгенерируйте новый ключ
./scripts/generate-encryption-key.sh

# Обновите в БД
psql "$DATABASE_URL" -c "UPDATE encryption_keys SET master_key = 'YOUR_NEW_KEY' WHERE id = 1;"
```

### Используйте SSL для БД

```bash
export DATABASE_URL=postgresql://...?sslmode=require&channel_binding=require
```

## Мониторинг

```sql
-- Активные сессии
SELECT user_id, session_id, is_authorized, last_seen_at 
FROM user_sessions 
WHERE is_active = true;

-- Последняя активность
SELECT session_id, activity_type, created_at 
FROM session_activity 
ORDER BY created_at DESC 
LIMIT 10;
```

## Troubleshooting

### Сессия не загружается

```bash
# Проверьте подключение к БД
psql "$DATABASE_URL" -c "SELECT 1;"

# Проверьте сессию
psql "$DATABASE_URL" -c "SELECT user_id, session_id FROM user_sessions WHERE user_id = 'YOUR_ID';"
```

### Ошибка дешифрования

```bash
# Проверьте ключ шифрования
psql "$DATABASE_URL" -c "SELECT id FROM encryption_keys WHERE id = 1;"

# Тест дешифрования
psql "$DATABASE_URL" -c "
SELECT pgp_sym_decrypt(api_hash_encrypted::bytea, 
    (SELECT master_key FROM encryption_keys WHERE id = 1))
FROM user_sessions WHERE user_id = 'YOUR_ID';
"
```

## Архитектура

```
┌──────────────┐
│  Client 1    │──┐
└──────────────┘  │
                  │    ┌─────────────┐      ┌──────────────┐
┌──────────────┐  │    │             │      │              │
│  Client 2    │──┼───▶│ VIBEE Agent │─────▶│  PostgreSQL  │
└──────────────┘  │    │             │      │  (Sessions)  │
                  │    └─────────────┘      └──────────────┘
┌──────────────┐  │           │
│  Client N    │──┘           │
└──────────────┘              ▼
                      ┌─────────────────┐
                      │ Telegram Bridge │
                      └─────────────────┘
```

## Преимущества

✅ **Централизованное управление** - все сессии в одном месте  
✅ **Безопасность** - API hash зашифрован AES-256  
✅ **Multi-tenant** - поддержка множества клиентов  
✅ **API управления** - REST endpoints для CRUD операций  
✅ **Аудит** - логирование всех действий с сессиями  
✅ **Масштабируемость** - легко добавлять новых клиентов  

## Дополнительно

- Полная документация: `docs/MULTI_TENANT_SETUP.md`
- Схема БД: `migrations/001_user_sessions.sql`
- API endpoints: `gleam/src/vibee/api/session_handlers.gleam`
