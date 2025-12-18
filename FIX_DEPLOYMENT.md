# 🔧 Исправление деплоя - Добавить FLY_API_TOKEN

## ❌ Проблема

```
Error: No access token available. Please login with 'flyctl auth login'
Error: Process completed with exit code 1.
```

GitHub Actions не может задеплоить, потому что отсутствует `FLY_API_TOKEN` в secrets.

## ✅ Решение

### Шаг 1: Получить токен Fly.io

```bash
# Установить Fly CLI (если еще не установлен)
curl -L https://fly.io/install.sh | sh

# Добавить в PATH
export FLYCTL_INSTALL="/home/vscode/.fly"
export PATH="$FLYCTL_INSTALL/bin:$PATH"

# Логин в Fly.io
flyctl auth login
# Откроется браузер, войти в аккаунт

# Получить токен
flyctl auth token
```

**Скопируйте токен!** Он выглядит примерно так:
```
fo1_aBcDeFgHiJkLmNoPqRsTuVwXyZ1234567890
```

### Шаг 2: Добавить токен в GitHub Secrets

1. **Открыть настройки репозитория:**
   ```
   https://github.com/gHashTag/vibee-gleam/settings/secrets/actions
   ```

2. **Нажать "New repository secret"**

3. **Заполнить:**
   - **Name:** `FLY_API_TOKEN`
   - **Value:** [вставить токен из шага 1]

4. **Нажать "Add secret"**

### Шаг 3: Запустить деплой снова

1. **Открыть Actions:**
   ```
   https://github.com/gHashTag/vibee-gleam/actions
   ```

2. **Выбрать "Deploy to Fly.io"**

3. **Нажать "Run workflow"**
   - Branch: `main`
   - Нажать зеленую кнопку "Run workflow"

4. **Подождать 3-5 минут**

5. **Проверить:**
   ```bash
   curl https://vibee-mcp.fly.dev/dashboard/agent
   ```

## 🎯 Альтернатива: Деплой через CLI

Если не хотите добавлять токен в GitHub, можно задеплоить вручную:

```bash
# 1. Логин
flyctl auth login

# 2. Деплой
cd /workspaces/vibee-gleam
flyctl deploy --config fly.toml --remote-only

# 3. Проверка
curl https://vibee-mcp.fly.dev/dashboard/agent
```

## 📋 Checklist

- [ ] Установить Fly CLI
- [ ] Выполнить `flyctl auth login`
- [ ] Получить токен: `flyctl auth token`
- [ ] Добавить токен в GitHub Secrets как `FLY_API_TOKEN`
- [ ] Запустить workflow "Deploy to Fly.io"
- [ ] Подождать 3-5 минут
- [ ] Проверить https://vibee-mcp.fly.dev/dashboard/agent

## 🔍 Проверка токена

После добавления токена в GitHub Secrets:

1. **Открыть:**
   ```
   https://github.com/gHashTag/vibee-gleam/settings/secrets/actions
   ```

2. **Должен быть виден:**
   - `FLY_API_TOKEN` (Updated X minutes ago)

3. **Запустить workflow снова**

## 🎉 После успешного деплоя

Dashboard будет доступен:
```
https://vibee-mcp.fly.dev/dashboard/agent
```

Проверить:
```bash
# Health check
curl https://vibee-mcp.fly.dev/health

# Dashboard
curl https://vibee-mcp.fly.dev/dashboard/agent | head -20

# Logs
curl https://vibee-mcp.fly.dev/logs | head -20
```

## 🐛 Troubleshooting

### Токен не работает

```bash
# Проверить что токен валидный
flyctl auth whoami

# Если ошибка - перелогиниться
flyctl auth logout
flyctl auth login
flyctl auth token
```

### Workflow все еще падает

1. Проверить логи workflow
2. Убедиться что токен добавлен правильно
3. Проверить что токен не истек
4. Попробовать получить новый токен

### Нет доступа к Fly.io

1. Зарегистрироваться: https://fly.io/app/sign-up
2. Подтвердить email
3. Добавить платежный метод (если требуется)
4. Выполнить `flyctl auth login`

## 📞 Нужна помощь?

1. **Fly.io документация:**
   - https://fly.io/docs/flyctl/auth-token/
   - https://fly.io/docs/reference/deploy/

2. **GitHub Secrets:**
   - https://docs.github.com/en/actions/security-guides/encrypted-secrets

3. **Проверить статус Fly.io:**
   - https://status.fly.io/

---

**Важно:** Токен `FLY_API_TOKEN` дает полный доступ к вашему Fly.io аккаунту. Никогда не публикуйте его в коде или логах!
