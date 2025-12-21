---
name: dev
description: Manage local development environment with Docker Compose
---

# /dev - Local Development

Manage local development environment with Docker Compose.

## Services

| Service | Port | Description |
|---------|------|-------------|
| postgres | 5432 | PostgreSQL database |
| telegram-bridge | 8080 | Go MTProto bridge |
| vibee | 3000 | Gleam MCP server |
| remotion | 3333 | Video rendering |

## Usage

```bash
/dev up                   # Start all services
/dev down                 # Stop all services
/dev restart              # Restart all services
/dev logs                 # View all logs
/dev logs vibee           # View specific service logs
/dev db-shell             # PostgreSQL shell
/dev reset                # Reset volumes and restart
/dev status               # Show service status
```

## Implementation

When user runs `/dev`:

1. **Start Services**
```bash
cd /Users/playra/vibee
docker-compose up -d
```

2. **Stop Services**
```bash
docker-compose down
```

3. **Database Shell**
```bash
docker-compose exec postgres psql -U postgres vibee
```

4. **Reset**
```bash
docker-compose down -v
docker-compose up -d
docker-compose exec postgres psql -U postgres -f /docker-entrypoint-initdb.d/init.sql
```

## docker-compose.yml Services

```yaml
services:
  postgres:
    image: pgvector/pgvector:pg16
    ports:
      - "5432:5432"
    volumes:
      - ./init.sql:/docker-entrypoint-initdb.d/init.sql

  telegram-bridge:
    build: ./telegram-bridge
    ports:
      - "8080:8080"
    depends_on:
      - postgres

  vibee:
    build: ./gleam
    ports:
      - "3000:3000"
    depends_on:
      - postgres
      - telegram-bridge

  remotion:
    build: ./remotion
    ports:
      - "3333:3333"
```

## Environment Variables (Local)

```bash
DATABASE_URL=postgresql://postgres:postgres@localhost:5432/vibee
VIBEE_BRIDGE_URL=http://localhost:8080
VIBEE_API_KEY=dev-api-key
```

## Health Checks

```bash
# Check postgres
curl http://localhost:5432

# Check bridge
curl http://localhost:8080/health

# Check vibee
curl http://localhost:3000/health
```

## Common Issues

| Issue | Solution |
|-------|----------|
| Port already in use | `lsof -i :5432` and kill process |
| Volume permissions | `docker-compose down -v` and restart |
| Init script failed | Check init.sql syntax |
