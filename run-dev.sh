#!/bin/bash
# Development run script with environment variables

cd "$(dirname "$0")/gleam"

# Load environment variables
export TELEGRAM_SESSION_ID=sess_df0p27qhvzvv
export TELEGRAM_API_ID=94892
export TELEGRAM_API_HASH=cacf9ad137d228611b49b2ecc6d68d43
export OPENROUTER_API_KEY=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
export DATABASE_URL=postgresql://neondb_owner:npg_A9z2dErbkfhw@ep-bitter-frog-a1bewei7-pooler.ap-southeast-1.aws.neon.tech/neondb?sslmode=require&channel_binding=require

echo "Starting VIBEE with environment variables..."
gleam run
