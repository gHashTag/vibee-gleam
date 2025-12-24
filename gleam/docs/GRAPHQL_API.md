# GraphQL API - Lead CRM

> **Для начинающих:** Пошаговое руководство по работе с API управления лидами.

---

## Быстрый старт (5 минут)

### Шаг 1: Откройте Playground

Перейдите в браузере: **https://vibee-mcp.fly.dev/graphql/playground**

### Шаг 2: Выполните первый запрос

Скопируйте в левую панель и нажмите кнопку Play:

```graphql
query {
  leads(limit: 5) {
    id
    username
    firstName
    status
  }
}
```

### Шаг 3: Посмотрите результат

В правой панели появится ответ:

```json
{
  "data": {
    "leads": [
      {
        "id": 1,
        "username": "neuro_sage",
        "firstName": "Dmitrii",
        "status": "NEW"
      },
      {
        "id": 2,
        "username": "crypto_buyer",
        "firstName": "Alex",
        "status": "CONTACTED"
      }
    ]
  }
}
```

---

## Базовая информация

| Параметр | Значение |
|----------|----------|
| **API URL** | `https://vibee-mcp.fly.dev/graphql` |
| **Playground** | https://vibee-mcp.fly.dev/graphql/playground |
| **Метод** | POST |
| **Content-Type** | `application/json` |
| **Аутентификация** | Не требуется |

---

## QUERIES (Чтение данных)

### leads - Список лидов

```graphql
query GetLeads {
  leads(limit: 10) {
    id
    telegramUserId
    username
    firstName
    status
    funnelStage
    priority
    qualityScore
    source
    createdAt
  }
}
```

**Параметры:**

| Параметр | Тип | Описание |
|----------|-----|----------|
| `limit` | Int | Максимум записей (по умолчанию 50) |
| `offset` | Int | Пропустить N записей (для пагинации) |
| `status` | String | Фильтр: new, contacted, qualified, won, lost |

**Пример ответа:**

```json
{
  "data": {
    "leads": [
      {
        "id": 1,
        "telegramUserId": 144022504,
        "username": "neuro_sage",
        "firstName": "Dmitrii",
        "status": "NEW",
        "funnelStage": "AWARENESS",
        "priority": "MEDIUM",
        "qualityScore": 7,
        "source": "Aimly.io dev",
        "createdAt": "2024-12-24T10:00:00Z"
      }
    ]
  }
}
```

**curl:**

```bash
curl -X POST https://vibee-mcp.fly.dev/graphql \
  -H "Content-Type: application/json" \
  -d '{"query": "{ leads(limit: 5) { id username firstName status } }"}'
```

---

### lead - Один лид по ID

```graphql
query GetLead {
  lead(id: 1) {
    id
    telegramUserId
    username
    firstName
    lastName
    status
    funnelStage
    priority
    qualityScore
    source
    createdAt
  }
}
```

**Параметры:**

| Параметр | Тип | Обязательный | Описание |
|----------|-----|--------------|----------|
| `id` | Int | Да | ID лида в базе |

**curl:**

```bash
curl -X POST https://vibee-mcp.fly.dev/graphql \
  -H "Content-Type: application/json" \
  -d '{"query": "{ lead(id: 1) { id username status } }"}'
```

---

### funnelStats - Статистика воронки

```graphql
query GetFunnelStats {
  funnelStats {
    awareness
    interest
    consideration
    intent
    evaluation
    purchase
    total
  }
}
```

**Пример ответа:**

```json
{
  "data": {
    "funnelStats": {
      "awareness": 15,
      "interest": 8,
      "consideration": 5,
      "intent": 3,
      "evaluation": 2,
      "purchase": 1,
      "total": 34
    }
  }
}
```

---

### triggerConfigs - Конфигурация чатов

```graphql
query GetTriggerConfigs {
  triggerConfigs {
    chatId
    chatName
    isActive
    triggers
    forwardChatId
  }
}
```

**Пример ответа:**

```json
{
  "data": {
    "triggerConfigs": [
      {
        "chatId": "-5082217642",
        "chatName": "Aimly.io dev",
        "isActive": true,
        "triggers": ["купить", "продать", "крипту", "btc", "usdt"],
        "forwardChatId": "-1002737186844"
      }
    ]
  }
}
```

---

### leadForwards - История пересылок

```graphql
query GetLeadForwards {
  leadForwards(limit: 10) {
    id
    leadId
    sourceChatId
    sourceChatName
    targetChatId
    qualityScore
    intent
    urgency
    status
    forwardedAt
  }
}
```

---

## MUTATIONS (Изменение данных)

### 1. createLead - Создать лида

```graphql
mutation CreateLead {
  createLead(
    telegramUserId: 123456789
    username: "john_doe"
    firstName: "John"
    source: "Telegram Group"
    firstMessage: "Хочу купить крипту"
  ) {
    id
    telegramUserId
    username
    status
    createdAt
  }
}
```

**Параметры:**

| Параметр | Тип | Обязательный | Описание |
|----------|-----|--------------|----------|
| `telegramUserId` | Int | Да | Telegram User ID |
| `username` | String | Нет | @username |
| `firstName` | String | Нет | Имя |
| `source` | String | Нет | Источник |
| `firstMessage` | String | Нет | Первое сообщение |

**curl:**

```bash
curl -X POST https://vibee-mcp.fly.dev/graphql \
  -H "Content-Type: application/json" \
  -d '{"query": "mutation { createLead(telegramUserId: 123456789, username: \"test_user\") { id status } }"}'
```

---

### 2. updateLeadStatus - Обновить статус

```graphql
mutation UpdateStatus {
  updateLeadStatus(leadId: 1, status: "contacted") {
    id
    status
    funnelStage
  }
}
```

**Доступные статусы:**

| Значение | Описание |
|----------|----------|
| `new` | Новый лид |
| `contacted` | Связались |
| `qualified` | Квалифицирован |
| `proposal_sent` | Отправлено КП |
| `negotiation` | Переговоры |
| `won` | Сделка закрыта |
| `lost` | Потеряно |

**curl:**

```bash
curl -X POST https://vibee-mcp.fly.dev/graphql \
  -H "Content-Type: application/json" \
  -d '{"query": "mutation { updateLeadStatus(leadId: 1, status: \"contacted\") { id status } }"}'
```

---

### 3. updateFunnelStage - Этап воронки

```graphql
mutation UpdateFunnel {
  updateFunnelStage(leadId: 1, stage: "interest") {
    id
    funnelStage
    status
  }
}
```

**Этапы воронки:**

| Значение | Описание |
|----------|----------|
| `awareness` | Осведомленность |
| `interest` | Интерес |
| `consideration` | Рассмотрение |
| `intent` | Намерение |
| `evaluation` | Оценка |
| `purchase` | Покупка |

---

### 4. updateQuizResult - Результат квиза

```graphql
mutation UpdateQuiz {
  updateQuizResult(leadId: 1, score: 8, productId: 2) {
    id
    qualityScore
  }
}
```

**Параметры:**

| Параметр | Тип | Описание |
|----------|-----|----------|
| `leadId` | Int | ID лида |
| `score` | Int | Оценка 0-10 |
| `productId` | Int | ID продукта |

---

### 5. updateLeadPriority - Приоритет

```graphql
mutation UpdatePriority {
  updateLeadPriority(leadId: 1, priority: "high") {
    id
    priority
    status
  }
}
```

**Приоритеты:**

| Значение | Описание |
|----------|----------|
| `low` | Низкий |
| `medium` | Средний |
| `high` | Высокий |
| `urgent` | Срочный |

---

### 6. addLeadNote - Добавить заметку

```graphql
mutation AddNote {
  addLeadNote(leadId: 1, note: "Клиент заинтересован в USDT") {
    id
    firstName
    status
  }
}
```

---

### 7. assignLead - Назначить менеджера

```graphql
mutation AssignLead {
  assignLead(leadId: 1, agentId: "manager_alex") {
    id
    firstName
    status
  }
}
```

---

### 8. deleteLead - Удалить лида

```graphql
mutation DeleteLead {
  deleteLead(leadId: 1) {
    id
    deleted
    message
  }
}
```

**Пример ответа:**

```json
{
  "data": {
    "deleteLead": {
      "id": 1,
      "deleted": true,
      "message": "Lead deleted successfully"
    }
  }
}
```

---

## JavaScript / TypeScript

### Базовый клиент

```typescript
const GRAPHQL_URL = 'https://vibee-mcp.fly.dev/graphql';

async function graphql<T>(query: string, variables?: Record<string, any>): Promise<T> {
  const response = await fetch(GRAPHQL_URL, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ query, variables }),
  });

  const result = await response.json();
  if (result.errors) throw new Error(result.errors[0].message);
  return result.data;
}
```

### Примеры использования

```typescript
// Получить лидов
const { leads } = await graphql<{ leads: Lead[] }>(`
  query { leads(limit: 10) { id username status } }
`);

// Создать лида
const { createLead } = await graphql<{ createLead: Lead }>(`
  mutation {
    createLead(telegramUserId: 123, username: "test") {
      id status
    }
  }
`);

// Обновить статус
const { updateLeadStatus } = await graphql<{ updateLeadStatus: Lead }>(`
  mutation {
    updateLeadStatus(leadId: 1, status: "contacted") {
      id status
    }
  }
`);
```

### TypeScript типы

```typescript
interface Lead {
  id: number;
  telegramUserId: number;
  username?: string;
  firstName?: string;
  lastName?: string;
  status: 'NEW' | 'CONTACTED' | 'QUALIFIED' | 'PROPOSAL_SENT' | 'NEGOTIATION' | 'WON' | 'LOST';
  funnelStage: 'AWARENESS' | 'INTEREST' | 'CONSIDERATION' | 'INTENT' | 'EVALUATION' | 'PURCHASE';
  priority: 'LOW' | 'MEDIUM' | 'HIGH' | 'URGENT';
  qualityScore?: number;
  source?: string;
  createdAt: string;
}

interface FunnelStats {
  awareness: number;
  interest: number;
  consideration: number;
  intent: number;
  evaluation: number;
  purchase: number;
  total: number;
}

interface DeleteResult {
  id: number;
  deleted: boolean;
  message: string;
}
```

---

## React Query

```typescript
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';

// Получить лидов
export function useLeads(limit = 50) {
  return useQuery({
    queryKey: ['leads', limit],
    queryFn: () => graphql<{ leads: Lead[] }>(`
      query { leads(limit: ${limit}) { id username firstName status funnelStage } }
    `).then(r => r.leads),
  });
}

// Статистика воронки
export function useFunnelStats() {
  return useQuery({
    queryKey: ['funnelStats'],
    queryFn: () => graphql<{ funnelStats: FunnelStats }>(`
      query { funnelStats { awareness interest consideration intent evaluation purchase total } }
    `).then(r => r.funnelStats),
  });
}

// Обновить статус
export function useUpdateStatus() {
  const qc = useQueryClient();
  return useMutation({
    mutationFn: ({ leadId, status }: { leadId: number; status: string }) =>
      graphql(`mutation { updateLeadStatus(leadId: ${leadId}, status: "${status}") { id } }`),
    onSuccess: () => qc.invalidateQueries({ queryKey: ['leads'] }),
  });
}
```

---

## Структура ответов

### Успешный ответ

```json
{
  "data": {
    "leads": [...]
  },
  "errors": null
}
```

### Ошибка

```json
{
  "data": null,
  "errors": [
    {
      "message": "Missing required argument: leadId"
    }
  ]
}
```

---

## Статус API

> Обновлено: 24.12.2024

### Queries

| Операция | Статус |
|----------|--------|
| `leads` | Работает |
| `lead` | Работает |
| `funnelStats` | Работает |
| `triggerConfigs` | Работает |
| `leadForwards` | Работает |

### Mutations

| Операция | Статус |
|----------|--------|
| `createLead` | Работает |
| `updateLeadStatus` | Работает |
| `updateFunnelStage` | Работает |
| `updateQuizResult` | Работает |
| `updateLeadPriority` | Работает |
| `addLeadNote` | Работает |
| `assignLead` | Работает |
| `deleteLead` | Работает |

---

## FAQ

**Почему пустой ответ?**
Данных нет в БД. Сначала создайте лида через `createLead`.

**Как узнать ID лида?**
```graphql
query { leads { id username } }
```

**Ошибка "Missing required argument"?**
Не передан обязательный параметр. Проверьте таблицу параметров.

**Как тестировать без Playground?**
Используйте curl команды из примеров выше.

---

## Ссылки

| Ресурс | URL |
|--------|-----|
| API | https://vibee-mcp.fly.dev/graphql |
| Playground | https://vibee-mcp.fly.dev/graphql/playground |
| Логи | `fly logs -a vibee-mcp` |
