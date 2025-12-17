#!/bin/bash
cd /Users/playra/vibee-eliza-999/vibee/gleam

export PATH="/opt/homebrew/bin:$PATH"
export DATABASE_URL="postgresql://neondb_owner:REDACTED_DB_PASSWORD@ep-bitter-frog-a1bewei7-pooler.ap-southeast-1.aws.neon.tech/neondb?sslmode=require"
export VIBEE_BRIDGE_URL="https://vibee-telegram-bridge.fly.dev"
export VIBEE_API_KEY="vibee-secret-key-2024"
export OPENROUTER_API_KEY="REDACTED_OPENROUTER_KEY"

gleam run -m run_onboarding
