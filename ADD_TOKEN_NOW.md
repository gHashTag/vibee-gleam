# 🔑 Добавить FLY_API_TOKEN - Инструкция

## Проблема
Приложение не задеплоилось, потому что GitHub Actions не может авторизоваться в Fly.io.

## Решение (2 минуты)

### Шаг 1: Добавить токен в GitHub Secrets

1. **Откройте эту ссылку:**
   https://github.com/gHashTag/vibee-gleam/settings/secrets/actions

2. **Нажмите кнопку:** `New repository secret`

3. **Заполните форму:**
   - **Name:** `FLY_API_TOKEN`
   - **Secret:** Вставьте ваш токен:
   ```
   FlyV1 fm2_lJPECAAAAAAACz56xBAhnVKFRdWs3QqNKyVGJ8QDwrVodHRwczovL2FwaS5mbHkuaW8vdjGUAJLOABS7fR8Lk7lodHRwczovL2FwaS5mbHkuaW8vYWFhL3YxxDzBpSNUvx6IYJ40UwYZeJfd3goySQ6uL1fT0swhfQ5zPLNpYsUq5fftVjFKAs3SMzQR4axooN4RGNr6rSrETu5PNljmoGc9jeZBHI1bITxmiuNtnhV/nyUThdytcB6b5wtIG5wAAtexbplMtRvmdlTBrrsAMdnCgZh8eM2fFo011Ll8W2VjWIX6MYimvMQgOt8ajF1F7bynSYXTLgBBtmMg1oZVe+JorE35MwXULHs=,fm2_lJPETu5PNljmoGc9jeZBHI1bITxmiuNtnhV/nyUThdytcB6b5wtIG5wAAtexbplMtRvmdlTBrrsAMdnCgZh8eM2fFo011Ll8W2VjWIX6MYimvMQQ01iSTSIizO3w8yXHnyq958O5aHR0cHM6Ly9hcGkuZmx5LmlvL2FhYS92MZgEks5pQ6HCzwAAAAElO7/gF84AE+niCpHOABPp4gzEEOhJ+7OPCEzxa4dRNlIOkSPEIN/XO4u0WqIDPLOXANqtceCIqrxoPFkBYAyzUpNC28CR
   ```

4. **Нажмите:** `Add secret`

### Шаг 2: Запустить деплой вручную

1. **Откройте Actions:**
   https://github.com/gHashTag/vibee-gleam/actions

2. **Выберите workflow:** `Deploy to Fly.io`

3. **Нажмите:** `Run workflow` (справа)

4. **Выберите branch:** `main`

5. **Нажмите:** `Run workflow` (зелёная кнопка)

### Шаг 3: Дождаться деплоя (3 минуты)

Следите за прогрессом здесь:
https://github.com/gHashTag/vibee-gleam/actions

Когда появится зелёная галочка ✅ - всё готово!

### Шаг 4: Проверить результат

Откройте:
**https://vibee-mcp.fly.dev/leads**

Должны увидеть leads panel с dark theme! 🎯

---

## Альтернатива: Деплой из Gitpod (если не хотите ждать)

```bash
# 1. Авторизоваться в Fly.io
export PATH="/home/vscode/.fly/bin:$PATH"
flyctl auth token FlyV1 fm2_lJPECAAAAAAACz56xBAhnVKFRdWs3QqNKyVGJ8QDwrVodHRwczovL2FwaS5mbHkuaW8vdjGUAJLOABS7fR8Lk7lodHRwczovL2FwaS5mbHkuaW8vYWFhL3YxxDzBpSNUvx6IYJ40UwYZeJfd3goySQ6uL1fT0swhfQ5zPLNpYsUq5fftVjFKAs3SMzQR4axooN4RGNr6rSrETu5PNljmoGc9jeZBHI1bITxmiuNtnhV/nyUThdytcB6b5wtIG5wAAtexbplMtRvmdlTBrrsAMdnCgZh8eM2fFo011Ll8W2VjWIX6MYimvMQgOt8ajF1F7bynSYXTLgBBtmMg1oZVe+JorE35MwXULHs=,fm2_lJPETu5PNljmoGc9jeZBHI1bITxmiuNtnhV/nyUThdytcB6b5wtIG5wAAtexbplMtRvmdlTBrrsAMdnCgZh8eM2fFo011Ll8W2VjWIX6MYimvMQQ01iSTSIizO3w8yXHnyq958O5aHR0cHM6Ly9hcGkuZmx5LmlvL2FhYS92MZgEks5pQ6HCzwAAAAElO7/gF84AE+niCpHOABPp4gzEEOhJ+7OPCEzxa4dRNlIOkSPEIN/XO4u0WqIDPLOXANqtceCIqrxoPFkBYAyzUpNC28CR

# 2. Задеплоить
cd /workspaces/vibee-gleam
flyctl deploy --ha=false

# 3. Открыть
flyctl open
```

---

## Скриншоты для помощи

### 1. GitHub Secrets страница
![image](https://github.com/user-attachments/assets/secrets-page.png)

Должна выглядеть так:
- Кнопка "New repository secret" справа вверху
- Список существующих секретов (если есть)

### 2. Форма добавления секрета
- **Name:** FLY_API_TOKEN (точно так, заглавными буквами)
- **Secret:** Ваш токен (начинается с FlyV1)
- Кнопка "Add secret" внизу

### 3. GitHub Actions
После добавления токена:
- Перейдите в Actions
- Найдите "Deploy to Fly.io"
- Нажмите "Run workflow"
- Выберите "main"
- Нажмите зелёную кнопку "Run workflow"

---

## Проверка что всё работает

После успешного деплоя:

✅ **Health Check:**
```bash
curl https://vibee-mcp.fly.dev/health
# Должен вернуть: {"status":"ok"}
```

✅ **Leads Panel:**
Откройте в браузере: https://vibee-mcp.fly.dev/leads
- Должен загрузиться dark theme интерфейс
- Навигация: Dashboard, Leads, P2P, Factory, Events
- Статистика: Total, New, Contacted, Converted
- Таблица (может быть пустой)

✅ **P2P Agent:**
https://vibee-mcp.fly.dev/p2p

✅ **Factory:**
https://vibee-mcp.fly.dev/factory

---

## Если что-то не работает

### Ошибка "Not found"
- Приложение ещё не задеплоилось
- Проверьте статус в GitHub Actions
- Дождитесь завершения деплоя (3 минуты)

### Ошибка "Bad credentials"
- Токен неправильный или истёк
- Получите новый токен: https://fly.io/dashboard/personal/tokens
- Обновите секрет в GitHub

### Деплой падает с ошибкой
- Проверьте логи в GitHub Actions
- Откройте конкретный workflow run
- Посмотрите на шаг где произошла ошибка

---

## Быстрый чеклист

- [ ] Открыл https://github.com/gHashTag/vibee-gleam/settings/secrets/actions
- [ ] Нажал "New repository secret"
- [ ] Ввёл Name: `FLY_API_TOKEN`
- [ ] Вставил токен в Secret
- [ ] Нажал "Add secret"
- [ ] Открыл https://github.com/gHashTag/vibee-gleam/actions
- [ ] Нажал "Run workflow"
- [ ] Выбрал "main"
- [ ] Нажал зелёную кнопку "Run workflow"
- [ ] Жду 3 минуты
- [ ] Открываю https://vibee-mcp.fly.dev/leads
- [ ] Вижу leads panel! 🎉

---

## Контакты

Если нужна помощь:
- GitHub Issues: https://github.com/gHashTag/vibee-gleam/issues
- Fly.io Dashboard: https://fly.io/apps/vibee-mcp
