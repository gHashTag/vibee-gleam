# Single stage build - use Gleam image for both build and runtime
# This avoids potential COPY corruption issues between stages
FROM ghcr.io/gleam-lang/gleam:v1.13.0-erlang-alpine

# Cache bust - force rebuild with new FFI code
ARG BUILD_TIMESTAMP="2025-12-15T18:53:00"
RUN echo "Build timestamp: ${BUILD_TIMESTAMP}"

WORKDIR /build

# Copy manifest first for dependency caching
COPY gleam.toml manifest.toml ./

# Download dependencies
RUN gleam deps download

# Copy source code
COPY src/ ./src/

# Build and export
RUN set -ex && \
    rm -rf build && \
    gleam build && \
    gleam export erlang-shipment && \
    echo "=== Verifying vibee@@main.beam ===" && \
    ls -la build/erlang-shipment/vibee/ebin/vibee@@main.beam && \
    echo "=== Checking exports ===" && \
    erl -noshell -eval "io:format(\"~p~n\", [beam_lib:chunks(\"build/erlang-shipment/vibee/ebin/vibee@@main.beam\", [exports])]), halt()."

# Install runtime dependencies (including psql for RAG context queries)
RUN apk add --no-cache curl bash postgresql-client

# Move shipment to /app
RUN mv build/erlang-shipment /app

# Verify after move
RUN set -ex && \
    echo "=== After move: Verifying files ===" && \
    ls -la /app/vibee/ebin/vibee@@main.beam && \
    erl -noshell -eval "io:format(\"~p~n\", [beam_lib:chunks(\"/app/vibee/ebin/vibee@@main.beam\", [exports])]), halt()."

# Create logs directory
RUN mkdir -p /app/logs

# Make entrypoint executable
RUN chmod +x /app/entrypoint.sh

# Non-root user
RUN adduser -D -u 1000 vibee
RUN chown -R vibee:vibee /app
USER vibee

# Environment variables
ENV PORT=8080
ENV VIBEE_BRIDGE_URL=http://localhost:8081
ENV VIBEE_LOG_LEVEL=info

EXPOSE 8080

# Run the application
WORKDIR /app
CMD ["./entrypoint.sh", "run"]
