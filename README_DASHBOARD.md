# 🤖 VIBEE Digital Clone Dashboard

## ✅ Готово и работает!

Dashboard для управления цифровым клоном в Telegram создан и протестирован.

## 🎯 Быстрый старт

### Локально:
```bash
cd /workspaces/vibee-gleam/gleam
gleam run

# Открыть
http://localhost:8080/dashboard/agent
```

### Production (после деплоя):
```
https://vibee-mcp.fly.dev/dashboard/agent
```

## 📊 Возможности

### Control Panel (Левая панель)
- **Agent Status** - включить/выключить агента
- **Strategy Selector** - 4 готовых стратегии:
  - 🛑 Passive - только мониторинг
  - 🎯 Selective - ответы в целевых чатах
  - ⚡ Active - активная работа (рекомендуется)
  - 🔥 Aggressive - максимальная активность
- **Configuration**:
  - Digital Twin Mode (ON/OFF)
  - Auto Reply (ON/OFF)
  - Cooldown (10s - 300s)
  - Confidence (50% - 95%)
- **Quick Actions** - Start/Pause/Stop/Reset

### Analytics (Центральная панель)
- **Metrics Cards**:
  - Messages Processed
  - Messages Sent
  - Response Rate
  - Avg Confidence
- **Charts** (Chart.js):
  - Activity Timeline (24h)
  - Chat Distribution (Top-5)
  - Confidence by Capability

### Telegram Logs (Правая панель)
- Real-time логи через WebSocket
- Типы: INCOMING / OUTGOING / SYSTEM
- Поиск по логам
- Автоскролл

## 🚀 Деплой

### Способ 1: GitHub Actions (Рекомендуется)

1. Открыть https://github.com/gHashTag/vibee-gleam/actions
2. Выбрать "Deploy to Fly.io"
3. Нажать "Run workflow"
4. Подождать 3-5 минут
5. Проверить https://vibee-mcp.fly.dev/dashboard/agent

### Способ 2: Fly CLI

```bash
fly deploy --config fly.toml --remote-only
```

Подробнее: [DEPLOY_INSTRUCTIONS.md](DEPLOY_INSTRUCTIONS.md)

## 📚 Документация

- **[DEPLOY_INSTRUCTIONS.md](DEPLOY_INSTRUCTIONS.md)** - инструкция по деплою
- **[FINAL_STATUS.md](FINAL_STATUS.md)** - итоговый статус
- **[AGENT_DASHBOARD_GUIDE.md](AGENT_DASHBOARD_GUIDE.md)** - полное руководство
- **[DIGITAL_CLONE_STRATEGY.md](DIGITAL_CLONE_STRATEGY.md)** - стратегия работы
- **[VIBEE_CHARACTER_CONFIG.md](VIBEE_CHARACTER_CONFIG.md)** - конфигурация (ElizaOS)
- **[DASHBOARD_SUMMARY.md](DASHBOARD_SUMMARY.md)** - summary
- **[LOGS_UI_IMPROVEMENTS.md](LOGS_UI_IMPROVEMENTS.md)** - улучшения логов

## 🎨 Технологии

- **Frontend**: Vanilla JS, Chart.js 4.4.0, WebSocket
- **Backend**: Gleam, Mist HTTP server
- **Deployment**: Fly.io, Docker
- **Design**: Dark theme, CSS Grid, CSS Variables

## 🎯 Use Cases

### Lead Generation
```javascript
Strategy: Selective
Digital Twin: OFF
Cooldown: 60s
Confidence: 80%
Target Chats: ["VIBEE AGENT"]
```

### Personal Assistant
```javascript
Strategy: Active
Digital Twin: ON
Cooldown: 30s
Confidence: 70%
Target Chats: [] (все)
```

### Community Manager
```javascript
Strategy: Selective
Cooldown: 120s
Confidence: 75%
Target Chats: ["Group1", "Group2"]
```

## 🔗 URL

- **Dashboard**: https://vibee-mcp.fly.dev/dashboard/agent
- **Logs**: https://vibee-mcp.fly.dev/logs
- **Health**: https://vibee-mcp.fly.dev/health
- **API**: https://vibee-mcp.fly.dev/api/v1

## 📸 Screenshots

(Добавить после деплоя)

## 🐛 Troubleshooting

### Dashboard не открывается

```bash
# Проверить health
curl https://vibee-mcp.fly.dev/health

# Проверить логи
fly logs

# Пересобрать
fly deploy --config fly.toml --remote-only
```

### Логи не обновляются

1. Проверить WebSocket подключение (F12 Console)
2. Обновить страницу (F5)
3. Проверить что агент запущен

## 🎉 Features

- ✅ 3-panel layout (Control/Analytics/Logs)
- ✅ 4 preset стратегии
- ✅ Real-time WebSocket логи
- ✅ Chart.js графики
- ✅ Responsive design
- ✅ Dark theme
- ✅ API integration
- ✅ Best practices от ElizaOS

## 📝 Changelog

### 2025-12-18
- ✅ Создан dashboard с 3 панелями
- ✅ Добавлены 4 preset стратегии
- ✅ Интегрированы Chart.js графики
- ✅ Добавлены real-time логи
- ✅ Написана документация
- ✅ Протестировано локально
- ⏳ Ожидание деплоя на production

## 🤝 Contributing

1. Fork репозиторий
2. Создать feature branch
3. Commit изменения
4. Push в branch
5. Создать Pull Request

## 📄 License

MIT License - VIBEE 2025

## 📞 Support

- GitHub Issues: https://github.com/gHashTag/vibee-gleam/issues
- Telegram: @neuro_sage
- Documentation: /docs

---

**Статус**: ✅ Готово к использованию
**Версия**: 1.0.0
**Последнее обновление**: 2025-12-18
