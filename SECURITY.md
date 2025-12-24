# Security Policy

## Reporting a Vulnerability

If you discover a security vulnerability in VIBEE, please report it responsibly:

1. **Do NOT** open a public GitHub issue
2. Email security concerns to: 999aigents@gmail.com
3. Include:
   - Description of the vulnerability
   - Steps to reproduce
   - Potential impact
   - Any suggested fixes

We aim to respond within 48 hours and will work with you to understand and address the issue.

## Security Considerations

### API Keys and Secrets

- Never commit API keys, passwords, or session tokens
- Use environment variables for all sensitive configuration
- See `.env.example` for required variables

### Telegram Sessions

- Session files contain authentication tokens
- Store session data securely
- Rotate sessions if compromised

### MCP Tools

VIBEE provides powerful MCP tools that can:

- Send messages as the authenticated Telegram user
- Access message history
- Create payments and invoices
- Execute arbitrary Telegram API calls

**Best Practices:**

- Use separate Telegram accounts for development
- Limit MCP tool access in production
- Monitor tool usage via audit logs
- Apply rate limiting (configured in `rate_limiter.gleam`)

### GraphQL API

The GraphQL endpoint includes:

- Query complexity limits (max depth: 10)
- Rate limiting (60 requests/minute)
- API key authentication
- Introspection can be disabled

### Database

- Use direct PostgreSQL connections (not pooler) with SSL
- Never expose DATABASE_URL publicly
- Apply row-level security for multi-tenant deployments

## Supported Versions

| Version | Supported |
|---------|-----------|
| main    | Yes       |
| < 1.0   | No        |

## Security Updates

Security patches are released as soon as possible after a vulnerability is confirmed.
