#!/bin/bash
# Production Deployment Script
# Run this script locally (not in Gitpod) after: fly auth login

set -e

echo "🚀 VIBEE Production Deployment"
echo "================================"
echo ""

# Check if fly is installed
if ! command -v fly &> /dev/null; then
    echo "❌ Fly CLI not found. Install it:"
    echo "   curl -L https://fly.io/install.sh | sh"
    exit 1
fi

# Check if logged in
if ! fly auth whoami &> /dev/null; then
    echo "❌ Not logged in to Fly.io"
    echo "   Run: fly auth login"
    exit 1
fi

echo "✅ Fly CLI ready"
echo ""

# Set secrets
echo "📝 Setting secrets..."
fly secrets set \
  OPENROUTER_API_KEY=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 \
  TELEGRAM_API_ID=94892 \
  TELEGRAM_API_HASH=cacf9ad137d228611b49b2ecc6d68d43 \
  TELEGRAM_SESSION_ID=sess_df0p27qhvzvv \
  TELEGRAM_SESSION_1_PHONE=+79933420465 \
  TELEGRAM_SESSION_1_USERNAME=neuro_sage \
  --app vibee-mcp

echo "✅ Secrets set"
echo ""

# Deploy
echo "🚢 Deploying to Fly.io..."
fly deploy --config fly.toml

echo ""
echo "✅ Deployment complete!"
echo ""
echo "📊 Check status:"
echo "   fly status --app vibee-mcp"
echo ""
echo "🔍 View logs:"
echo "   fly logs --app vibee-mcp"
echo ""
echo "🏥 Health check:"
echo "   curl https://vibee-mcp.fly.dev/health"
echo ""
