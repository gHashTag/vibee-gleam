---
name: deploy
description: Deploy VIBEE with MANDATORY E2E testing
---

# /deploy - Деплой с ОБЯЗАТЕЛЬНЫМ E2E

**E2E тесты ВСЕГДА запускаются после деплоя! Это обязательно!**

## Тестовые аккаунты (ПРОВЕРЕНО 20.12.2025)

| Роль | Username | ID | Телефон |
|------|----------|----|---------|
| **ТЕСТЕР** | @neuro_sage | User: 144022504 | +7 (993) 342-04-65 |
| **ЮЗЕР-БОТ** | @vibee_agent | Chat: 6579515876 | +66 6-2401-4170 |

**@vibee_agent** = Digital Twin (ЮЗЕР-БОТ с MTProto автоматизацией)
Session для тестов: `REDACTED_SESSION`

## Services

| Service | App Name | Description |
|---------|----------|-------------|
| vibee-mcp | vibee-mcp | Gleam MCP server (default) |
| bridge | vibee-telegram-bridge | Go Telegram bridge |
| remotion | vibee-remotion | Video rendering |

## Workflow

1. `gleam build && gleam format src test`
2. `fly deploy -a vibee-mcp`
3. Wait for health check
4. **ОБЯЗАТЕЛЬНО**: Rainbow Bridge E2E тесты
5. Report results

## Usage

```bash
/deploy              # Deploy vibee-mcp + E2E
/deploy bridge       # Deploy telegram-bridge
/deploy remotion     # Deploy remotion
/deploy all          # Deploy all services + E2E
```

## ВАЖНО

Деплой считается **УСПЕШНЫМ** только если:
- Сервис запустился
- Health check прошёл
- **ВСЕ E2E тесты прошли**

## Implementation

When user runs `/deploy`:

1. **Build Phase**
```bash
cd /Users/playra/vibee/gleam
gleam build
gleam format src test
```

2. **Deploy Phase**
```bash
fly deploy -a vibee-mcp
```

3. **E2E Test Phase** (ОБЯЗАТЕЛЬНО!)
```bash
VIBEE_API_KEY="vibee-secret-2024-prod" \
VIBEE_BRIDGE_URL="https://vibee-telegram-bridge.fly.dev" \
./.claude/hooks/e2e-rainbow-bridge.sh
```

4. **Verify Results**
- ALL tests must pass
- If any test fails - FIX and redeploy
- Report final status

## Правило

**НЕ СЧИТАТЬ ЗАДАЧУ ВЫПОЛНЕННОЙ** пока:
1. Код задеплоен
2. E2E тесты запущены
3. ВСЕ тесты прошли (/pricing, /quiz, /help)
