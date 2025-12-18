# Unified Design System - Summary

## Что создано ✅

### 1. Unified Layout Component (`vibee/web/layout.gleam`)

**Компоненты:**
- ✅ `render_page()` - полная страница с header, sidebar, main content
- ✅ `render_card()` - карточка с иконкой и заголовком
- ✅ `render_stat_card()` - статистическая карточка
- ✅ `render_button()` - кнопка с разными стилями
- ✅ `render_table()` - таблица данных

**Unified CSS Theme:**
```css
--bg-primary: #0a0a0f
--bg-secondary: #12121a
--bg-card: #1a1a2e
--text-primary: #ffffff
--text-secondary: #888888
--accent: #00ffaa
--success: #00ff00
--warning: #ffaa00
--danger: #ff4444
--info: #0088cc
```

**Навигация:**
- 📊 Dashboard
- 🎯 Leads
- 💱 P2P Agent
- 🏭 Factory
- 📡 Events

### 2. Интеграция с Leads Panel

Обновлен `leads_panel.gleam` для использования унифицированного layout:
```gleam
let config = layout.PageConfig(
  title: "Leads Management",
  active_page: "leads",
  show_sidebar: False,
  show_language_toggle: True,
)

layout.render_page(config, "", main_content)
```

### 3. Общие компоненты

**Header:**
- Logo с иконкой
- Навигационные ссылки
- Language toggle
- Status indicator

**Cards:**
- Стандартный card с header/body
- Stat card для метрик
- Hover эффекты
- Consistent spacing

**Buttons:**
- Primary (accent color)
- Secondary (transparent)
- Success (green)
- Warning (yellow)
- Danger (red)

**Tables:**
- Responsive design
- Hover effects
- Consistent styling

## Структура проекта

```
vibee/web/
├── layout.gleam          # Unified layout system
├── leads_panel.gleam     # Leads management (uses layout)
├── p2p_panel.gleam       # P2P agent (to be updated)
├── factory_panel.gleam   # Factory gallery (to be updated)
├── tasks_ui.gleam        # Tasks UI (to be updated)
└── html.gleam            # HTML utilities
```

## Следующие шаги

### 1. Исправить ошибки компиляции

**Проблемы:**
- Logging API изменился (требует context)
- Некоторые функции отсутствуют

**Решение:**
```bash
cd gleam
# Заменить все logging.info() на logging.quick_info()
find src -name "*.gleam" -exec sed -i 's/logging\.info(/logging.quick_info(/g' {} \;
find src -name "*.gleam" -exec sed -i 's/logging\.warn(/logging.quick_warn(/g' {} \;
find src -name "*.gleam" -exec sed -i 's/logging\.error(/logging.quick_error(/g' {} \;

# Пересобрать
gleam build
```

### 2. Обновить остальные панели

**P2P Panel:**
```gleam
// Было:
pub fn render() -> String {
  "<!DOCTYPE html>..."
}

// Стало:
pub fn render() -> String {
  let config = layout.PageConfig(
    title: "P2P Agent",
    active_page: "p2p",
    show_sidebar: True,
    show_language_toggle: True,
  )
  
  let sidebar = render_p2p_sidebar()
  let main = render_p2p_main()
  
  layout.render_page(config, sidebar, main)
}
```

**Factory Panel:**
```gleam
pub fn render() -> String {
  let config = layout.PageConfig(
    title: "Template Factory",
    active_page: "factory",
    show_sidebar: False,
    show_language_toggle: False,
  )
  
  layout.render_page(config, "", render_factory_gallery())
}
```

### 3. Деплой

**Локальное тестирование:**
```bash
cd gleam
gleam build
gleam run

# Откройте в браузере:
# http://localhost:8080/leads
# http://localhost:8080/p2p
# http://localhost:8080/factory
```

**Production деплой:**
```bash
# 1. Убедитесь что все собирается
cd gleam && gleam build

# 2. Проверьте Dockerfile
cat ../Dockerfile.production

# 3. Деплой на Fly.io
cd ..
fly deploy

# 4. Проверьте статус
fly status

# 5. Откройте в браузере
fly open
```

**Проверка в проде:**
```bash
# Получить URL приложения
fly info

# Проверить endpoints
curl https://vibee-mcp.fly.dev/health
curl https://vibee-mcp.fly.dev/leads
curl https://vibee-mcp.fly.dev/p2p
curl https://vibee-mcp.fly.dev/factory

# Проверить логи
fly logs

# SSH в контейнер
fly ssh console
```

## Преимущества унифицированного дизайна

### 1. Consistency
- Одинаковые цвета, шрифты, spacing
- Единый стиль кнопок, карточек, таблиц
- Consistent navigation

### 2. Maintainability
- Изменения в одном месте (layout.gleam)
- Легко добавлять новые страницы
- Меньше дублирования кода

### 3. Performance
- Общий CSS загружается один раз
- Меньше размер HTML
- Faster page loads

### 4. User Experience
- Знакомый интерфейс на всех страницах
- Predictable navigation
- Smooth transitions

## Примеры использования

### Создать новую страницу

```gleam
// src/vibee/web/my_panel.gleam
import vibee/web/layout

pub fn render() -> String {
  let config = layout.PageConfig(
    title: "My Panel",
    active_page: "my_panel",
    show_sidebar: True,
    show_language_toggle: False,
  )
  
  let sidebar = "
    " <> layout.render_card("📊", "Stats", "
      <p>Some stats here</p>
    ") <> "
  "
  
  let main = "
    <h1>My Panel</h1>
    <div class=\"stats-grid\">
      " <> layout.render_stat_card("🎯", "42", "Total Items", "blue") <> "
      " <> layout.render_stat_card("✅", "38", "Completed", "green") <> "
    </div>
    
    " <> layout.render_table(
      ["ID", "Name", "Status"],
      [
        ["1", "Item 1", "Active"],
        ["2", "Item 2", "Pending"],
      ]
    ) <> "
  "
  
  layout.render_page(config, sidebar, main)
}
```

### Добавить в router

```gleam
// src/vibee/api/router.gleam
import vibee/web/my_panel

// В handle_request:
http.Get, ["my-panel"] -> {
  let html = my_panel.render()
  html_response(200, html)
}
```

## Responsive Design

**Desktop (>1024px):**
- Sidebar: 280px
- Main content: flex 1
- Full navigation

**Tablet (768px - 1024px):**
- Sidebar: 240px
- Collapsed navigation
- Adjusted spacing

**Mobile (<768px):**
- Sidebar: full width, stacked
- Main content: full width
- Mobile menu

## Цветовая палитра

```css
/* Primary */
--bg-primary: #0a0a0f     /* Main background */
--bg-secondary: #12121a   /* Header, sidebar */
--bg-card: #1a1a2e        /* Cards, modals */

/* Text */
--text-primary: #ffffff   /* Main text */
--text-secondary: #888888 /* Labels, hints */

/* Accent */
--accent: #00ffaa         /* Primary actions */
--accent-hover: #00dd99   /* Hover state */

/* Status */
--success: #00ff00        /* Success, active */
--warning: #ffaa00        /* Warning, pending */
--danger: #ff4444         /* Error, critical */
--info: #0088cc           /* Info, neutral */

/* Border */
--border: #2a2a3e         /* Borders, dividers */
```

## Spacing System

```css
--spacing-xs: 0.25rem   /* 4px */
--spacing-sm: 0.5rem    /* 8px */
--spacing-md: 1rem      /* 16px */
--spacing-lg: 1.5rem    /* 24px */
--spacing-xl: 2rem      /* 32px */
```

## Border Radius

```css
--radius-sm: 4px
--radius-md: 8px
--radius-lg: 12px
--radius-xl: 16px
```

## Typography

```css
font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;

/* Sizes */
h1: 2rem (32px)
h2: 1.5rem (24px)
h3: 1.25rem (20px)
body: 1rem (16px)
small: 0.85rem (13.6px)
```

## Заключение

Создана унифицированная система дизайна для всех веб-панелей VIBEE:
- ✅ Consistent UI/UX
- ✅ Reusable components
- ✅ Easy to maintain
- ✅ Responsive design
- ✅ Dark theme
- ⏳ Нужно исправить ошибки компиляции
- ⏳ Обновить остальные панели
- ⏳ Задеплоить в production
