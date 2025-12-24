# План: Реорганизация документации gleam/

## Текущее состояние

| Файл | Строк | Назначение | Проблемы |
|------|-------|------------|----------|
| `CLAUDE.md` | 738 | Гайд для Claude Code | Остается в корне (стандарт) |
| `README.md` | 199 | Quick Start | Остается в корне (стандарт) |
| `docs/SETUP.md` | 202 | MCP подключение | Дублирует README |
| `docs/AGENTS_RUNNING.md` | 148 | Статус RAG | Устарел (17.12.2025) |
| `docs/SESSION_STATUS.md` | 176 | Статус сессий | Устарел (17.12.2025) |
| `docs/GRAPHQL_API.md` | 590 | GraphQL API | Актуален |

## Проблемы

1. **Дублирование ~30%**: E2E testing, auth flow, MCP tools в 3+ файлах
2. **Устаревшие статусы**: AGENTS_RUNNING.md, SESSION_STATUS.md - статические отчеты
3. **Нет индекса**: Непонятно какой док читать первым
4. **Языковой микс**: 60% RU / 40% EN

---

## План действий

### 1. Удалить устаревшие статус-файлы

```
docs/AGENTS_RUNNING.md  → DELETE (статический отчет от 17.12)
docs/SESSION_STATUS.md  → DELETE (статический отчет от 17.12)
```

**Причина**: Это не документация, а одноразовые отчеты. Актуальный статус получаем через:
- `fly logs -a vibee-mcp`
- `/api/e2e/status`
- `session_list` MCP tool

### 2. Объединить SETUP.md в README.md

```
docs/SETUP.md → MERGE into README.md → DELETE
```

**Причина**: Дублируют друг друга на 70%

### 3. Финальная структура

```
gleam/
├── CLAUDE.md          # Claude Code guide (НЕ ТРОГАЕМ - стандарт)
├── README.md          # Quick Start + Setup (объединенный)
└── docs/
    ├── GRAPHQL_API.md # GraphQL Lead CRM API
    └── ARCHITECTURE.md # NEW: ElizaOS Actions, структура проекта
```

### 4. Обновить README.md

**Добавить из SETUP.md:**
- Troubleshooting секцию
- Детали auth flow

**Удалить дубликаты:**
- MCP tools список (уже есть в CLAUDE.md)

### 5. Создать docs/ARCHITECTURE.md

Новый файл с:
- ElizaOS Actions-based архитектура
- Структура проекта
- Diagram: Polling → Actions → Handlers

---

## Файлы для изменения

| Действие | Файл |
|----------|------|
| DELETE | `docs/AGENTS_RUNNING.md` |
| DELETE | `docs/SESSION_STATUS.md` |
| DELETE | `docs/SETUP.md` (после merge) |
| UPDATE | `README.md` (merge SETUP.md) |
| CREATE | `docs/ARCHITECTURE.md` |
| KEEP | `CLAUDE.md` (без изменений) |
| KEEP | `docs/GRAPHQL_API.md` (актуален) |

---

## Результат

**До**: 6 файлов, 1463 строки, 30% дублирования
**После**: 4 файла, ~900 строк, 0% дублирования

```
gleam/
├── CLAUDE.md          # 738 lines - Claude Code guide
├── README.md          # ~250 lines - Quick Start + Setup
└── docs/
    ├── GRAPHQL_API.md # 590 lines - API reference
    └── ARCHITECTURE.md # ~150 lines - System design
```
