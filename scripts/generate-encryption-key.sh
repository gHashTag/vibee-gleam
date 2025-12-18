#!/bin/bash
# Generate secure encryption key for session storage

set -e

echo "🔐 Generating secure encryption key..."
echo ""

# Generate 32-byte random key (base64 encoded)
KEY=$(openssl rand -base64 32)

echo "Generated key:"
echo "$KEY"
echo ""

echo "To update in database:"
echo ""
echo "psql \$DATABASE_URL -c \"UPDATE encryption_keys SET master_key = '$KEY' WHERE id = 1;\""
echo ""

echo "Or add to .env:"
echo ""
echo "ENCRYPTION_MASTER_KEY=$KEY"
echo ""

echo "⚠️  IMPORTANT: Store this key securely!"
echo "   - Do NOT commit to git"
echo "   - Use secrets manager in production"
echo "   - Backup the key - lost key = lost sessions"
