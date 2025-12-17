# Telegram Integration

tags: telegram, mtproto, integration

## Architecture

VIBEE uses a Go microservice (telegram-bridge) for MTProto communication.

```
┌─────────────┐      HTTP/WS      ┌─────────────┐     MTProto     ┌──────────┐
│  Gleam App  │ ←──────────────→ │  Go Bridge  │ ←─────────────→ │ Telegram │
└─────────────┘                   └─────────────┘                 └──────────┘
```

## Why Go for MTProto?

- **gotd/td**: Mature, well-tested MTProto library
- **Performance**: Low memory footprint (~150KB per client)
- **Reliability**: Handles reconnections automatically

## Authentication Flow

1. **Send Code**: Request verification code to phone
2. **Verify Code**: Enter the 5-digit code
3. **2FA (if enabled)**: Enter cloud password
4. **Session**: Store session for future connections

## API Endpoints

### Connect

```
POST /api/v1/connect
{
  "api_id": 12345,
  "api_hash": "your_hash",
  "phone": "+1234567890"
}
```

### Send Code

```
POST /api/v1/auth/phone
X-Session-ID: session_123
{}
```

### Verify Code

```
POST /api/v1/auth/code
X-Session-ID: session_123
{
  "code": "12345"
}
```

### Send Message

```
POST /api/v1/send
X-Session-ID: session_123
{
  "chat_id": -1001234567890,
  "text": "Hello from VIBEE!"
}
```

## WebSocket Updates

Connect to `/api/v1/updates` for real-time Telegram updates:

```
WS /api/v1/updates?session_id=session_123
```

Message format:
```json
{
  "type": "new_message",
  "chat_id": -1001234567890,
  "message_id": 123,
  "text": "User message",
  "sender_id": 987654321
}
```

## Session Persistence

Sessions are stored in PostgreSQL for reliability:

- Automatic reconnection after server restart
- Multiple concurrent sessions supported
- Session cleanup on explicit logout
