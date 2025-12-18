# 🚀 Auto Deploy Setup

## Текущий статус

✅ GitHub Actions workflow уже настроен (`.github/workflows/deploy.yml`)
✅ Деплой происходит автоматически при push в `main`
⏳ Нужно добавить FLY_API_TOKEN в GitHub Secrets

## Шаг 1: Получить Fly.io API Token

### Вариант A: Через веб-интерфейс
1. Откройте https://fly.io/dashboard
2. Перейдите в **Account** → **Access Tokens**
3. Нажмите **Create Token**
4. Скопируйте токен

### Вариант B: Через CLI (локально)
```bash
fly auth token
```

## Шаг 2: Добавить токен в GitHub Secrets

1. Откройте https://github.com/gHashTag/vibee-gleam/settings/secrets/actions
2. Нажмите **New repository secret**
3. Name: `FLY_API_TOKEN`
4. Value: вставьте токен из шага 1
5. Нажмите **Add secret**

## Шаг 3: Запустить деплой

### Автоматический деплой (при push)
```bash
git add .
git commit -m "feat: add unified design and leads panel"
git push origin main
```

### Ручной деплой (через GitHub UI)
1. Откройте https://github.com/gHashTag/vibee-gleam/actions
2. Выберите **Deploy to Fly.io**
3. Нажмите **Run workflow**
4. Выберите branch `main`
5. Нажмите **Run workflow**

## Проверка деплоя

### Через GitHub Actions
1. Откройте https://github.com/gHashTag/vibee-gleam/actions
2. Найдите последний workflow run
3. Дождитесь завершения (обычно 2-3 минуты)
4. Проверьте логи

### Через браузер
После успешного деплоя откройте:

- **Leads Panel**: https://vibee-mcp.fly.dev/leads
- **P2P Agent**: https://vibee-mcp.fly.dev/p2p
- **Factory**: https://vibee-mcp.fly.dev/factory
- **Health Check**: https://vibee-mcp.fly.dev/health

## Что происходит при деплое

1. ✅ Checkout code
2. ✅ Setup Fly CLI
3. ✅ Build Docker image
4. ✅ Deploy to Fly.io
5. ✅ Health check
6. ✅ Notify success

## Мониторинг

### GitHub Actions
- **Workflow runs**: https://github.com/gHashTag/vibee-gleam/actions
- **Deployment history**: https://github.com/gHashTag/vibee-gleam/deployments

### Fly.io Dashboard
- **App dashboard**: https://fly.io/apps/vibee-mcp
- **Metrics**: https://fly.io/apps/vibee-mcp/metrics
- **Logs**: https://fly.io/apps/vibee-mcp/logs

## Troubleshooting

### Деплой не запускается
```bash
# Проверьте что токен добавлен
gh secret list

# Проверьте workflow файл
cat .github/workflows/deploy.yml
```

### Деплой падает с ошибкой
```bash
# Проверьте логи в GitHub Actions
# https://github.com/gHashTag/vibee-gleam/actions

# Проверьте логи Fly.io
fly logs --app vibee-mcp
```

### Health check не проходит
```bash
# Проверьте статус приложения
fly status --app vibee-mcp

# Проверьте health endpoint
curl https://vibee-mcp.fly.dev/health

# Перезапустите
fly apps restart vibee-mcp
```

## Быстрый старт (если токен уже добавлен)

```bash
# 1. Сделать изменения
git add .
git commit -m "feat: your changes"

# 2. Push в main
git push origin main

# 3. Открыть Actions
open https://github.com/gHashTag/vibee-gleam/actions

# 4. Дождаться деплоя (2-3 минуты)

# 5. Проверить результат
open https://vibee-mcp.fly.dev/leads
```

## Альтернатива: Деплой из Gitpod

Если не хотите настраивать GitHub Actions, можно деплоить прямо из Gitpod:

```bash
# 1. Установить Fly CLI (уже сделано)
export PATH="/home/vscode/.fly/bin:$PATH"

# 2. Авторизоваться
flyctl auth login

# 3. Задеплоить
cd /workspaces/vibee-gleam
flyctl deploy --ha=false

# 4. Открыть
flyctl open
```

## Рекомендация

✅ **Используйте GitHub Actions** - автоматический деплой при каждом push
- Не нужно помнить команды
- История деплоев в GitHub
- Автоматические health checks
- Уведомления о статусе

## После настройки

Просто делайте:
```bash
git push origin main
```

И через 2-3 минуты изменения будут в проде:
**https://vibee-mcp.fly.dev/leads**
