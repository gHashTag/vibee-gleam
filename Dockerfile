# Single stage build for Fly.io
FROM ghcr.io/gleam-lang/gleam:v1.13.0-erlang-alpine

WORKDIR /build

# Copy entire Gleam project
COPY gleam/ ./

# Copy dashboard to gleam directory (where app runs from)
COPY dashboard/ /build/dashboard/

# Build
RUN gleam build

# Create directories
RUN mkdir -p /build/sessions /build/data

# Environment
ENV PORT=8080
ENV VIBEE_BRIDGE_URL=https://vibee-telegram-bridge.fly.dev
ENV VIBEE_LOG_LEVEL=info

EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD wget --no-verbose --tries=1 --spider http://localhost:8080/health || exit 1

# Run
CMD ["gleam", "run"]
