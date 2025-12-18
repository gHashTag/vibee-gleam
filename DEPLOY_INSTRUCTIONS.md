# 🚀 Инструкция по деплою Dashboard

## ✅ Все готово к деплою!

Dashboard создан, протестирован локально и готов к production деплою.

## 🎯 Способ 1: Через GitHub Actions (Рекомендуется)

### Шаги:

1. **Открыть GitHub Actions**
   ```
   https://github.com/gHashTag/vibee-gleam/actions
   ```

2. **Найти workflow "Deploy to Fly.io"**
   - В левой панели выбрать "Deploy to Fly.io"

3. **Запустить вручную**
   - Нажать кнопку "Run workflow" (справа сверху)
   - Выбрать branch: `main`
   - Нажать зеленую кнопку "Run workflow"

4. **Подождать 3-5 минут**
   - Workflow запустится
   - Соберет Docker образ
   - Задеплоит на Fly.io
   - Проверит health check

5. **Проверить результат**
   ```bash
   curl https://vibee-mcp.fly.dev/dashboard/agent
   ```

### Если workflow не запускается:

**Проверить secrets:**
1. Открыть Settings → Secrets and variables → Actions
2. Должен быть secret: `FLY_API_TOKEN`
3. Если нет - создать (см. Способ 2)

## 🎯 Способ 2: Через Fly CLI (Альтернатива)

### Предварительные требования:

```bash
# Установить Fly CLI (если нет)
curl -L https://fly.io/install.sh | sh

# Добавить в PATH
export FLYCTL_INSTALL="/home/vscode/.fly"
export PATH="$FLYCTL_INSTALL/bin:$PATH"
```

### Шаги:

1. **Логин в Fly.io**
   ```bash
   fly auth login
   ```
   - Откроется браузер
   - Войти в аккаунт Fly.io
   - Подтвердить авторизацию

2. **Деплой**
   ```bash
   cd /workspaces/vibee-gleam
   fly deploy --config fly.toml --remote-only
   ```

3. **Подождать 3-5 минут**
   - Fly.io соберет образ
   - Задеплоит на сервер
   - Запустит health check

4. **Проверить результат**
   ```bash
   curl https://vibee-mcp.fly.dev/dashboard/agent
   ```

### Получить токен для GitHub Actions:

```bash
# Получить токен
fly auth token

# Скопировать токен
# Добавить в GitHub:
# Settings → Secrets → Actions → New repository secret
# Name: FLY_API_TOKEN
# Value: [вставить токен]
```

## 🎯 Способ 3: Автоматический деплой (Уже настроен)

GitHub Actions автоматически деплоит при push в `main`.

**Проверить:**
1. Последний коммит: `e89425d Fix dashboard path`
2. Открыть: https://github.com/gHashTag/vibee-gleam/actions
3. Найти workflow для коммита `e89425d`
4. Проверить статус

**Если failed:**
- Открыть логи
- Найти ошибку
- Исправить и запушить снова

## ✅ После успешного деплоя

### Проверить работу:

```bash
# Health check
curl https://vibee-mcp.fly.dev/health

# Dashboard
curl https://vibee-mcp.fly.dev/dashboard/agent | head -20

# Logs
curl https://vibee-mcp.fly.dev/logs | head -20
```

### Открыть в браузере:

- **Dashboard**: https://vibee-mcp.fly.dev/dashboard/agent
- **Logs**: https://vibee-mcp.fly.dev/logs
- **Health**: https://vibee-mcp.fly.dev/health

## 🐛 Troubleshooting

### Workflow не запускается

**Причина**: Нет permissions или secrets

**Решение:**
1. Проверить Settings → Actions → General → Workflow permissions
2. Должно быть: "Read and write permissions"
3. Проверить наличие `FLY_API_TOKEN` в secrets

### Build fails

**Причина**: Ошибка компиляции или Docker

**Решение:**
1. Проверить логи workflow
2. Найти ошибку
3. Исправить локально:
   ```bash
   cd /workspaces/vibee-gleam/gleam
   gleam build
   ```
4. Если билд успешен локально - проблема в Dockerfile
5. Проверить Dockerfile

### Deploy fails

**Причина**: Проблема с Fly.io

**Решение:**
1. Проверить статус Fly.io: https://status.fly.io
2. Проверить логи:
   ```bash
   fly logs
   ```
3. Проверить конфигурацию:
   ```bash
   fly config validate
   ```

### Dashboard возвращает 404

**Причина**: Файлы не скопировались

**Решение:**
1. Проверить Dockerfile:
   ```dockerfile
   COPY dashboard/ /build/dashboard/
   ```
2. Проверить что файлы есть в репозитории:
   ```bash
   ls -la gleam/dashboard/
   ```
3. Пересобрать:
   ```bash
   fly deploy --config fly.toml --remote-only
   ```

## 📊 Мониторинг деплоя

### Через GitHub Actions:

```
https://github.com/gHashTag/vibee-gleam/actions
```

### Через Fly.io:

```bash
# Статус приложения
fly status

# Логи в реальном времени
fly logs

# История деплоев
fly releases

# Информация о приложении
fly info
```

## 🎉 Успешный деплой

После успешного деплоя вы увидите:

```bash
$ curl https://vibee-mcp.fly.dev/dashboard/agent | head -5
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
```

**Dashboard доступен по адресу:**
```
https://vibee-mcp.fly.dev/dashboard/agent
```

## 📝 Checklist

- [ ] Открыть GitHub Actions
- [ ] Запустить workflow "Deploy to Fly.io"
- [ ] Подождать 3-5 минут
- [ ] Проверить https://vibee-mcp.fly.dev/dashboard/agent
- [ ] Если работает - обновить README
- [ ] Добавить скриншоты
- [ ] Поделиться с командой

## 🔗 Полезные ссылки

- **GitHub Repo**: https://github.com/gHashTag/vibee-gleam
- **GitHub Actions**: https://github.com/gHashTag/vibee-gleam/actions
- **Fly.io Dashboard**: https://fly.io/apps/vibee-mcp
- **Production URL**: https://vibee-mcp.fly.dev
- **Dashboard**: https://vibee-mcp.fly.dev/dashboard/agent
- **Logs**: https://vibee-mcp.fly.dev/logs
- **Health**: https://vibee-mcp.fly.dev/health

---

**Создано**: 2025-12-18 15:10 UTC
**Статус**: ✅ Готово к деплою
**Рекомендуемый способ**: GitHub Actions (workflow_dispatch)
