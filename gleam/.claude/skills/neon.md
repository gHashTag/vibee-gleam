---
name: neon
description: Database operations via Neon MCP (SQL, branches, migrations)
---

# Neon Database Skill

Быстрый доступ к PostgreSQL через Neon MCP.

## Quick Reference

| Action | Command |
|--------|---------|
| Run SQL | `mcp__neon__run_sql` |
| List tables | `mcp__neon__run_sql` с query |
| Create branch | `mcp__neon__create_branch` |
| List projects | `mcp__neon__list_projects` |
| Get connection | `mcp__neon__get_connection_string` |

## Prerequisites

1. Neon MCP должен быть подключен: `/mcp` → проверь статус `neon`
2. `NEON_API_KEY` в `.env`

## Common Queries

### Проверить таблицы
```sql
SELECT table_name FROM information_schema.tables
WHERE table_schema = 'public' ORDER BY table_name;
```

### Проверить leads
```sql
SELECT id, user_id, status, created_at
FROM leads ORDER BY created_at DESC LIMIT 10;
```

### Проверить lead_forwards
```sql
SELECT lf.*, l.status as lead_status
FROM lead_forwards lf
JOIN leads l ON lf.lead_id = l.id
ORDER BY lf.created_at DESC LIMIT 10;
```

### Проверить users
```sql
SELECT id, telegram_id, username, created_at
FROM users ORDER BY created_at DESC LIMIT 10;
```

## Migration Workflow

### 1. Создать ветку для миграции
```
mcp__neon__create_branch
- branch_name: "migration-add-column"
- parent_branch: "main"
```

### 2. Применить миграцию на ветке
```sql
ALTER TABLE leads ADD COLUMN priority INTEGER DEFAULT 0;
```

### 3. Протестировать
```sql
SELECT * FROM leads WHERE priority IS NOT NULL LIMIT 5;
```

### 4. Применить на main или удалить ветку
```
mcp__neon__delete_branch (если тест не прошёл)
```

## Troubleshooting

### MCP не подключен
```
/mcp → neon → restart
```

### Permission denied
Проверь `NEON_API_KEY` в `.env`:
```bash
grep NEON_API_KEY gleam/.env
```

### Connection timeout
Используй direct endpoint (без `-pooler`):
```
ep-xxx.region.aws.neon.tech (NOT ep-xxx-pooler)
```

## Project Info

| Parameter | Value |
|-----------|-------|
| Project | vibee |
| Region | ap-southeast-1 |
| Database | neondb |
| Direct endpoint | Set in DATABASE_URL env var |
