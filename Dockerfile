# Multi-stage build for VIBEE (Telegram Bridge + Gleam MCP Server)
# Stage 1: Build Go Telegram Bridge
FROM golang:1.24-alpine AS bridge-builder

WORKDIR /bridge

RUN apk add --no-cache git ca-certificates

COPY telegram-bridge/go.mod telegram-bridge/go.sum* ./
RUN go mod download

COPY telegram-bridge/ .
RUN CGO_ENABLED=0 GOOS=linux go build -ldflags="-w -s" -o /telegram-bridge ./cmd/server


# Stage 2: Build Gleam MCP Server
# Using v1.13.0-erlang-alpine which uses OTP 28
FROM ghcr.io/gleam-lang/gleam:v1.13.0-erlang-alpine AS gleam-builder

WORKDIR /app

# Cache bust to force rebuild of gleam - must be before COPY to bust cache
ARG CACHE_BUST=1
RUN echo "Cache bust: $CACHE_BUST"

# Copy project files
COPY gleam/gleam.toml gleam/manifest.toml ./
COPY gleam/src/ ./src/

# Clean build dir and rebuild fresh
RUN rm -rf build && gleam export erlang-shipment


# Stage 3: Runtime - combining both services
# MUST use OTP 28 to match Gleam builder (v1.13.0-erlang-alpine uses OTP 28)
FROM erlang:28-alpine

WORKDIR /app

# Install runtime dependencies
RUN apk add --no-cache curl bash ca-certificates tzdata supervisor

# Copy Telegram Bridge from builder
COPY --from=bridge-builder /telegram-bridge /app/telegram-bridge

# Copy Gleam application from builder
COPY --from=gleam-builder /app/build/erlang-shipment /app/gleam

# Create directories
RUN mkdir -p /app/sessions /app/logs /var/log/supervisor

# Copy supervisord config and make it readable
COPY supervisord.conf /etc/supervisord.conf
RUN chmod 644 /etc/supervisord.conf

# Non-root user
RUN adduser -D -u 1000 vibee
RUN chown -R vibee:vibee /app /var/log/supervisor /var/run
USER vibee

EXPOSE 8080

CMD ["/usr/bin/supervisord", "-c", "/etc/supervisord.conf"]
