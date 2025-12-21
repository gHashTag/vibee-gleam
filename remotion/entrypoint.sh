#!/bin/bash
set -e

# Create directories in the persistent volume
mkdir -p /app/storage/renders
mkdir -p /app/storage/public

# Create symlinks (remove existing directories first if needed)
rm -rf /app/out
rm -rf /app/public
ln -sf /app/storage/renders /app/out
ln -sf /app/storage/public /app/public

# Run the main command with xvfb-run for virtual display
exec xvfb-run --server-args="-screen 0 1920x1080x24" npx tsx render-server.ts
