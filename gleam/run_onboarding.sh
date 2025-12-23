#!/bin/bash
cd /Users/playra/vibee-eliza-999/vibee/gleam

export PATH="/opt/homebrew/bin:$PATH"
export DATABASE_URL="postgresql://neondb_owner:npg_A9z2dErbkfhw@ep-bitter-frog-a1bewei7-pooler.ap-southeast-1.aws.neon.tech/neondb?sslmode=require"
export VIBEE_BRIDGE_URL="https://vibee-telegram-bridge.fly.dev"
export VIBEE_API_KEY="vibee-secret-key-2024"
export OPENROUTER_API_KEY="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"

gleam run -m run_onboarding
