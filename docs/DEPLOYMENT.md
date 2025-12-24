# VIBEE Agent System - Deployment Guide

## Prerequisites

- Gleam 1.0+
- Erlang/OTP 26+
- PostgreSQL 14+ (or Neon PostgreSQL)
- Git

## Environment Setup

### 1. Database Configuration

Create a `.env` file in the `gleam/` directory:

```bash
DATABASE_URL=postgresql://user:password@host:port/database?sslmode=require
```

For Neon PostgreSQL:
```bash
DATABASE_URL=postgresql://neondb_owner:password@ep-xxx.region.aws.neon.tech/neondb?sslmode=require
```

### 2. Apply Database Migrations

```bash
cd gleam
export $(cat .env | xargs)
psql "$DATABASE_URL" -f migrations/004_agent_states.sql
```

Verify tables were created:
```bash
psql "$DATABASE_URL" -c "\dt"
```

Expected output:
```
 agent_states  | table
 agent_metrics | table
 agent_errors  | table
```

## Build and Test

### 1. Install Dependencies

```bash
cd gleam
gleam deps download
```

### 2. Build Project

```bash
gleam build
```

### 3. Run Tests

```bash
gleam test
```

### 4. Run Load Test

```bash
gleam run -m load_test
```

Expected output:
```
✅ Started 5 agents in Xms
✅ Started 10 agents in Xms
✅ Started 20 agents in Xms
```

## Deployment Options

### Option 1: Docker Deployment

Create `Dockerfile`:

```dockerfile
FROM ghcr.io/gleam-lang/gleam:v1.0.0-erlang-alpine

WORKDIR /app

# Copy project files
COPY gleam.toml manifest.toml ./
COPY src ./src
COPY migrations ./migrations

# Download dependencies
RUN gleam deps download

# Build project
RUN gleam build

# Set environment
ENV DATABASE_URL=""

# Run application
CMD ["gleam", "run"]
```

Build and run:
```bash
docker build -t vibee-agent-system .
docker run -e DATABASE_URL="$DATABASE_URL" vibee-agent-system
```

### Option 2: Fly.io Deployment

Create `fly.toml`:

```toml
app = "vibee-agent-system"
primary_region = "sin"

[build]
  dockerfile = "Dockerfile"

[env]
  PORT = "8080"

[[services]]
  internal_port = 8080
  protocol = "tcp"

  [[services.ports]]
    port = 80
    handlers = ["http"]

  [[services.ports]]
    port = 443
    handlers = ["tls", "http"]
```

Deploy:
```bash
fly launch
fly secrets set DATABASE_URL="postgresql://..."
fly deploy
```

### Option 3: Heroku Deployment

Create `Procfile`:
```
web: gleam run
```

Create `heroku.yml`:
```yaml
build:
  docker:
    web: Dockerfile
```

Deploy:
```bash
heroku create vibee-agent-system
heroku addons:create heroku-postgresql:mini
heroku config:set DATABASE_URL="$(heroku config:get DATABASE_URL)"
git push heroku main
```

### Option 4: Kubernetes Deployment

Create `deployment.yaml`:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: vibee-agent-system
spec:
  replicas: 3
  selector:
    matchLabels:
      app: vibee-agent-system
  template:
    metadata:
      labels:
        app: vibee-agent-system
    spec:
      containers:
      - name: vibee-agent-system
        image: your-registry/vibee-agent-system:latest
        env:
        - name: DATABASE_URL
          valueFrom:
            secretKeyRef:
              name: database-secret
              key: url
        ports:
        - containerPort: 8080
        resources:
          requests:
            memory: "256Mi"
            cpu: "250m"
          limits:
            memory: "512Mi"
            cpu: "500m"
---
apiVersion: v1
kind: Service
metadata:
  name: vibee-agent-system
spec:
  selector:
    app: vibee-agent-system
  ports:
  - port: 80
    targetPort: 8080
  type: LoadBalancer
```

Deploy:
```bash
kubectl create secret generic database-secret --from-literal=url="$DATABASE_URL"
kubectl apply -f deployment.yaml
```

## Production Configuration

### 1. Database Connection Pooling

The system uses `pog` for PostgreSQL connections with automatic pooling.

Configure pool size in code:
```gleam
let pool_config = pog.Config(
  host: "host",
  port: 5432,
  database: "database",
  user: "user",
  password: Some("password"),
  ssl: True,
  connection_parameters: [],
  pool_size: 10,  // Adjust based on load
)
```

### 2. Agent Configuration

Adjust auto-save threshold in `agent.gleam`:
```gleam
// Auto-save every N messages
let auto_save_threshold = 5  // Increase for high-volume bots
```

Adjust history limit per agent:
```gleam
AgentConfig(
  // ...
  history_limit: 50,  // Balance context vs memory
)
```

### 3. Monitoring Setup

Query metrics regularly:
```sql
-- Active agents
SELECT COUNT(DISTINCT agent_id) FROM agent_states;

-- Messages per hour
SELECT 
  DATE_TRUNC('hour', recorded_at) as hour,
  COUNT(*) as message_count
FROM agent_metrics
WHERE metric_type = 'message_received'
GROUP BY hour
ORDER BY hour DESC;

-- Error rate
SELECT 
  error_type,
  COUNT(*) as count
FROM agent_errors
WHERE occurred_at > NOW() - INTERVAL '1 hour'
GROUP BY error_type;
```

### 4. Health Check Endpoint

Add HTTP endpoint for health checks:
```gleam
import mist
import gleam/http/response

pub fn health_check_handler(sup) {
  fn(_request) {
    let health = supervisor.health_check(sup)
    let alive_count = list.filter(health, fn(h) { h.is_alive }) |> list.length
    
    response.new(200)
    |> response.set_body(
      "{ \"status\": \"ok\", \"agents\": " <> int.to_string(alive_count) <> " }"
    )
  }
}
```

## Scaling Considerations

### Horizontal Scaling

The agent system is designed for horizontal scaling:

1. **Stateless Supervisor**: Each instance has its own supervisor
2. **Shared Database**: All instances share PostgreSQL for state
3. **Agent Affinity**: Route users to same instance for best performance

Load balancer configuration:
```nginx
upstream vibee_agents {
  ip_hash;  # Sticky sessions for agent affinity
  server instance1:8080;
  server instance2:8080;
  server instance3:8080;
}
```

### Vertical Scaling

Resource requirements per agent:
- **Memory**: ~10MB per agent
- **CPU**: Minimal (mostly I/O bound)
- **Database**: ~1KB per agent state

Example capacity:
- **1GB RAM**: ~100 concurrent agents
- **2GB RAM**: ~200 concurrent agents
- **4GB RAM**: ~400 concurrent agents

### Database Scaling

For high load:
1. **Read Replicas**: Use for metrics queries
2. **Connection Pooling**: Increase pool size
3. **Indexing**: Add indexes on frequently queried columns

```sql
-- Add index for metrics queries
CREATE INDEX idx_agent_metrics_agent_type 
ON agent_metrics(agent_id, metric_type);

-- Add index for error queries
CREATE INDEX idx_agent_errors_occurred 
ON agent_errors(occurred_at DESC);
```

## Monitoring and Alerting

### Metrics to Monitor

1. **Agent Count**: Number of active agents
2. **Message Rate**: Messages per second
3. **Auto-save Rate**: Saves per minute
4. **Error Rate**: Errors per hour
5. **Database Latency**: Query response time

### Alerting Rules

```yaml
# Prometheus alerting rules
groups:
- name: vibee_agents
  rules:
  - alert: HighErrorRate
    expr: rate(agent_errors_total[5m]) > 10
    annotations:
      summary: "High error rate detected"
  
  - alert: DatabaseLatency
    expr: agent_db_query_duration_seconds > 1
    annotations:
      summary: "Database queries are slow"
  
  - alert: AgentCrashes
    expr: rate(agent_crashes_total[5m]) > 5
    annotations:
      summary: "Multiple agents crashing"
```

## Backup and Recovery

### Database Backups

Automated backups (Neon PostgreSQL):
```bash
# Neon provides automatic backups
# Configure retention period in Neon dashboard
```

Manual backup:
```bash
pg_dump "$DATABASE_URL" > backup_$(date +%Y%m%d).sql
```

Restore:
```bash
psql "$DATABASE_URL" < backup_20240101.sql
```

### Agent State Recovery

Agents automatically load saved state on startup:
```gleam
// In agent.gleam
case persistence.load_state(config.id) {
  Ok(saved_state) -> {
    io.println("Loaded saved state")
    saved_state
  }
  Error(_) -> {
    io.println("Creating new state")
    create_new_state(config)
  }
}
```

## Troubleshooting

### Common Issues

**1. Database Connection Errors**
```
Error: Failed to get database connection
```
Solution: Check DATABASE_URL and network connectivity

**2. Agent Not Responding**
```
Error: Agent not found
```
Solution: Check if agent crashed, review agent_errors table

**3. High Memory Usage**
```
Warning: Memory usage > 80%
```
Solution: Reduce history_limit or increase instance size

**4. Slow Queries**
```
Warning: Query took > 1s
```
Solution: Add database indexes, check connection pool

### Debug Mode

Enable verbose logging:
```gleam
// In agent.gleam
io.println("[DEBUG] " <> message)
```

Check logs:
```bash
# Docker
docker logs vibee-agent-system

# Kubernetes
kubectl logs -f deployment/vibee-agent-system

# Fly.io
fly logs
```

## Security Considerations

### 1. Database Security

- Use SSL/TLS for database connections
- Rotate database credentials regularly
- Use read-only replicas for metrics queries
- Enable database audit logging

### 2. Environment Variables

- Never commit `.env` files
- Use secret management (Vault, AWS Secrets Manager)
- Rotate secrets regularly

### 3. Rate Limiting

Add rate limiting per user:
```gleam
// Check message rate before processing
if is_rate_limited(user_id) {
  send_error("Too many messages, please slow down")
} else {
  process_message(agent_ref, message)
}
```

### 4. Input Validation

Validate all user inputs:
```gleam
fn validate_message(text: String) -> Result(String, String) {
  case string.length(text) {
    0 -> Error("Empty message")
    n if n > 4000 -> Error("Message too long")
    _ -> Ok(text)
  }
}
```

## Performance Tuning

### 1. Auto-save Threshold

Adjust based on message volume:
- **Low volume** (< 10 msg/min): threshold = 3
- **Medium volume** (10-100 msg/min): threshold = 5
- **High volume** (> 100 msg/min): threshold = 10

### 2. History Limit

Balance context vs memory:
- **Short conversations**: limit = 20
- **Medium conversations**: limit = 50
- **Long conversations**: limit = 100

### 3. Database Queries

Optimize queries:
```sql
-- Use EXPLAIN ANALYZE
EXPLAIN ANALYZE
SELECT * FROM agent_states WHERE agent_id = 'xxx';

-- Add covering indexes
CREATE INDEX idx_agent_states_id_updated 
ON agent_states(agent_id, last_updated);
```

## Maintenance

### Regular Tasks

**Daily:**
- Check error logs
- Monitor agent count
- Review metrics

**Weekly:**
- Analyze slow queries
- Review database size
- Check backup status

**Monthly:**
- Update dependencies
- Review and optimize indexes
- Capacity planning

### Cleanup Scripts

Remove old metrics:
```sql
-- Delete metrics older than 30 days
DELETE FROM agent_metrics 
WHERE recorded_at < NOW() - INTERVAL '30 days';

-- Delete old errors
DELETE FROM agent_errors 
WHERE occurred_at < NOW() - INTERVAL '30 days';
```

## Support and Resources

- **Documentation**: See AGENT_SYSTEM_SUMMARY.md
- **Integration Guide**: See TELEGRAM_INTEGRATION.md
- **GitHub Issues**: Report bugs and feature requests
- **Community**: Join discussions

## Conclusion

The VIBEE Agent System is production-ready with:
- ✅ Horizontal and vertical scaling
- ✅ Automatic state persistence
- ✅ Built-in monitoring
- ✅ Graceful error handling
- ✅ Multiple deployment options

Ready to deploy!
