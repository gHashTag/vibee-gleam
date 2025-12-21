---
name: db
description: Database operations for Neon PostgreSQL
---

# /db - Database Operations

Manage Neon PostgreSQL database: migrations, queries, embeddings.

## Connection

```
Host: ep-bitter-frog-a1bewei7.ap-southeast-1.aws.neon.tech
Database: neondb
User: neondb_owner
```

**IMPORTANT:** Use direct endpoint, NOT pooler:
- Correct: `ep-bitter-frog-a1bewei7.ap-southeast-1.aws.neon.tech`
- Wrong: `ep-bitter-frog-a1bewei7-pooler.ap-southeast-1.aws.neon.tech`

## Usage

```bash
/db status                # Show table statistics
/db migrate               # Run pending migrations
/db query "SELECT..."     # Execute SQL query
/db embeddings            # Check embedding coverage
/db tables                # List all tables
/db schema <table>        # Show table schema
```

## Tables

| Table | Description | Records |
|-------|-------------|---------|
| telegram_dialogs | Chats, groups, channels | ~500 |
| telegram_messages | Message history | ~50,000 |
| telegram_sessions | MTProto sessions | ~16 |
| products | Pricing tiers | 3 |
| subscriptions | User subscriptions | - |
| leads | Sales leads | - |
| github_documents | GitHub content (planned) | - |

## Migrations

Location: `/Users/playra/vibee/gleam/migrations/`

| Migration | Description |
|-----------|-------------|
| 001_initial.sql | Core tables |
| 002_messages.sql | Message history |
| 003_rag.sql | RAG embeddings |
| 004_agent.sql | Agent persistence |
| 005_github_documents.sql | GitHub content |
| 006_sales_funnel.sql | Products, subscriptions |

## Implementation

When user runs `/db`:

1. **Status**
```sql
SELECT
  schemaname,
  relname as table_name,
  n_live_tup as row_count
FROM pg_stat_user_tables
ORDER BY n_live_tup DESC;
```

2. **Embeddings Coverage**
```sql
SELECT
  COUNT(*) as total,
  COUNT(embedding) as with_embeddings,
  ROUND(COUNT(embedding)::numeric / COUNT(*)::numeric * 100, 2) as coverage_pct
FROM telegram_messages;
```

3. **Run Migration**
```bash
psql "$DATABASE_URL" -f migrations/006_sales_funnel.sql
```

## Connection String

```bash
export DATABASE_URL="postgresql://neondb_owner:npg_xxx@ep-bitter-frog-a1bewei7.ap-southeast-1.aws.neon.tech/neondb?sslmode=require"
```

## Common Queries

```sql
-- Recent messages
SELECT * FROM telegram_messages ORDER BY created_at DESC LIMIT 10;

-- Dialogs by type
SELECT type, COUNT(*) FROM telegram_dialogs GROUP BY type;

-- Products
SELECT * FROM products;

-- Active subscriptions
SELECT * FROM subscriptions WHERE status = 'active';
```
