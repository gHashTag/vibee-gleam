# 📚 GraphQL API - Документация для разработчиков

> **Для начинающих:** Эта документация поможет вам работать с Lead CRM API через GraphQL.

---

## 🚀 Быстрый старт

### Что такое GraphQL?
GraphQL — это язык запросов к API. Вы указываете, какие данные хотите получить, и получаете именно их.

### Где тестировать?
Откройте в браузере: **https://vibee-mcp.fly.dev/graphql/playground**

Там вы можете:
- ✍️ Писать запросы в левой панели
- ▶️ Нажимать кнопку Play для выполнения
- 📖 Смотреть документацию (вкладка DOCS справа)

---

## 📋 Базовая информация

| Параметр | Значение |
|----------|----------|
| **URL API** | `https://vibee-mcp.fly.dev/graphql` |
| **Playground** | https://vibee-mcp.fly.dev/graphql/playground |
| **Метод** | POST |
| **Content-Type** | `application/json` |
| **Аутентификация** | Не требуется (API открыт) |

---

## 🔍 QUERIES (Чтение данных)

> **Query** — это запрос на получение данных. Данные НЕ изменяются.

### 1️⃣ leads — Получить список лидов

**Скопируйте в Playground:**
```graphql
query GetAllLeads {
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
| `offset` | Int | Смещение для пагинации |
| `status` | String | Фильтр по статусу |

---

### 2️⃣ lead — Получить одного лида по ID

**Скопируйте в Playground:**
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
| `id` | Int | ✅ Да | ID лида в базе |

---

### 3️⃣ funnelStats — Статистика воронки продаж

**Скопируйте в Playground:**
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

**Возвращает:** Количество лидов на каждом этапе воронки.

---

### 4️⃣ triggerConfigs — Конфигурация триггер-чатов

**Скопируйте в Playground:**
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

**Возвращает:** Список чатов, где бот отслеживает триггерные слова.

---

### 5️⃣ leadForwards — История пересылок лидов

**Скопируйте в Playground:**
```graphql
query GetLeadForwards {
  leadForwards(limit: 20) {
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

## ✏️ MUTATIONS (Изменение данных)

> **Mutation** — это запрос на изменение данных (создание, обновление, удаление).

### 1️⃣ createLead — Создать нового лида

**Скопируйте в Playground:**
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
| `telegramUserId` | Int | ✅ Да | Telegram User ID |
| `username` | String | Нет | Username в Telegram |
| `firstName` | String | Нет | Имя пользователя |
| `source` | String | Нет | Источник (название чата) |
| `firstMessage` | String | Нет | Первое сообщение |

---

### 2️⃣ updateLeadStatus — Обновить статус лида

**Скопируйте в Playground:**
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
| `new` | 🆕 Новый |
| `contacted` | 📞 Связались |
| `qualified` | ✅ Квалифицирован |
| `proposal_sent` | 📧 Отправлено КП |
| `negotiation` | 🤝 Переговоры |
| `won` | 🏆 Выиграно |
| `lost` | ❌ Потеряно |

---

### 3️⃣ updateFunnelStage — Обновить этап воронки

**Скопируйте в Playground:**
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
| `awareness` | 👀 Осведомленность |
| `interest` | 💡 Интерес |
| `consideration` | 🤔 Рассмотрение |
| `intent` | 🎯 Намерение |
| `evaluation` | 📊 Оценка |
| `purchase` | 💰 Покупка |

---

### 4️⃣ updateQuizResult — Обновить результат квиза

**Скопируйте в Playground:**
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
| `score` | Int | Оценка (0-10) |
| `productId` | Int | ID рекомендуемого продукта |

---

### 5️⃣ updateLeadPriority — Изменить приоритет

**Скопируйте в Playground:**
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
| `low` | 🟢 Низкий |
| `medium` | 🟡 Средний |
| `high` | 🟠 Высокий |
| `urgent` | 🔴 Срочный |

---

### 6️⃣ addLeadNote — Добавить заметку

**Скопируйте в Playground:**
```graphql
mutation AddNote {
  addLeadNote(leadId: 1, note: "Клиент заинтересован в покупке USDT") {
    id
    firstName
    status
  }
}
```

---

### 7️⃣ assignLead — Назначить менеджера

**Скопируйте в Playground:**
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

### 8️⃣ deleteLead — Удалить лида

**Скопируйте в Playground:**
```graphql
mutation DeleteLead {
  deleteLead(leadId: 1) {
    id
    deleted
    message
  }
}
```

⚠️ **Внимание:** Удаление необратимо!

---

## Шаг 5: Интеграция в JavaScript/TypeScript

### 5.1 Базовый fetch клиент

```typescript
const GRAPHQL_URL = 'https://vibee-mcp.fly.dev/graphql';

async function graphqlQuery<T>(query: string, variables?: Record<string, any>): Promise<T> {
  const response = await fetch(GRAPHQL_URL, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({
      query,
      variables,
    }),
  });

  const result = await response.json();

  if (result.errors) {
    throw new Error(result.errors[0].message);
  }

  return result.data;
}
```

### 5.2 Примеры использования

```typescript
// Получить всех лидов
const leads = await graphqlQuery<{ leads: Lead[] }>(`
  query GetLeads($limit: Int, $status: String) {
    leads(limit: $limit, status: $status) {
      id
      telegramUserId
      username
      firstName
      status
      funnelStage
      qualityScore
      createdAt
    }
  }
`, { limit: 20, status: 'NEW' });

// Создать лида
const newLead = await graphqlQuery<{ createLead: Lead }>(`
  mutation CreateLead($telegramUserId: Int!, $username: String, $source: String) {
    createLead(telegramUserId: $telegramUserId, username: $username, source: $source) {
      id
      status
      createdAt
    }
  }
`, { telegramUserId: 123456789, username: 'test_user', source: 'Aimly.io' });

// Обновить статус
const updated = await graphqlQuery<{ updateLeadStatus: Lead }>(`
  mutation UpdateStatus($leadId: Int!, $status: String!) {
    updateLeadStatus(leadId: $leadId, status: $status) {
      id
      status
    }
  }
`, { leadId: 1, status: 'contacted' });
```

### 5.3 TypeScript типы

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

interface LeadForward {
  id: number;
  leadId?: number;
  sourceChatId: string;
  sourceChatName: string;
  targetChatId: string;
  qualityScore: number;
  intent: 'purchase' | 'question' | 'support';
  urgency: 'low' | 'normal' | 'high' | 'urgent';
  status: 'PENDING' | 'FORWARDED' | 'FAILED' | 'DEDUPLICATED' | 'RATE_LIMITED';
  forwardedAt: string;
}
```

---

## Шаг 6: React Query интеграция

```typescript
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';

// Хук для получения лидов
export function useLeads(limit = 50, status?: string) {
  return useQuery({
    queryKey: ['leads', limit, status],
    queryFn: () => graphqlQuery<{ leads: Lead[] }>(`
      query { leads(limit: ${limit}${status ? `, status: "${status}"` : ''}) {
        id telegramUserId username firstName status funnelStage qualityScore createdAt
      }}
    `).then(r => r.leads),
  });
}

// Хук для статистики воронки
export function useFunnelStats() {
  return useQuery({
    queryKey: ['funnelStats'],
    queryFn: () => graphqlQuery<{ funnelStats: FunnelStats }>(`
      query { funnelStats { awareness interest consideration intent evaluation purchase total }}
    `).then(r => r.funnelStats),
  });
}

// Мутация для обновления статуса
export function useUpdateLeadStatus() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: ({ leadId, status }: { leadId: number; status: string }) =>
      graphqlQuery<{ updateLeadStatus: Lead }>(`
        mutation { updateLeadStatus(leadId: ${leadId}, status: "${status}") { id status }}
      `),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['leads'] });
      queryClient.invalidateQueries({ queryKey: ['funnelStats'] });
    },
  });
}
```

---

## Шаг 7: Получение схемы (Introspection)

```bash
curl -X POST https://vibee-mcp.fly.dev/graphql \
  -H "Content-Type: application/json" \
  -d '{
    "query": "{ __schema { queryType { name } mutationType { name } types { name kind description fields { name type { name } } } } }"
  }'
```

---

## Шаг 8: GraphQL Playground

Для интерактивного тестирования откройте в браузере:
**https://vibee-mcp.fly.dev/graphql/playground**

Там можно:
- Писать и выполнять queries/mutations
- Смотреть документацию схемы (вкладка DOCS)
- Автодополнение полей
- История запросов

---

## Структура ответов

### Успешный ответ
```json
{
  "data": { ... },
  "errors": null
}
```

### Ответ с ошибкой
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

## ✅ Статус реализации

> Обновлено: 24.12.2024 — Все методы работают с реальной БД!

### Queries (Чтение)
| Операция | Статус | Примечание |
|----------|--------|------------|
| `leads` | ✅ Работает | PostgreSQL |
| `lead` | ✅ Работает | PostgreSQL |
| `funnelStats` | ✅ Работает | PostgreSQL |
| `triggerConfigs` | ✅ Работает | Статическая конфигурация |
| `leadForwards` | ✅ Работает | PostgreSQL (fallback на mock) |

### Mutations (Изменение)
| Операция | Статус | Примечание |
|----------|--------|------------|
| `createLead` | ✅ Работает | Сохраняет в PostgreSQL |
| `updateLeadStatus` | ✅ Работает | Обновляет в PostgreSQL |
| `updateFunnelStage` | ✅ Работает | Обновляет в PostgreSQL |
| `updateQuizResult` | ✅ Работает | Обновляет в PostgreSQL |
| `updateLeadPriority` | ✅ Работает | Обновляет в PostgreSQL |
| `addLeadNote` | ✅ Работает | Добавляет к существующим |
| `assignLead` | ✅ Работает | Назначает менеджера |
| `deleteLead` | ✅ Работает | Удаляет из PostgreSQL |

---

## 🔗 Полезные ссылки

| Ресурс | URL |
|--------|-----|
| **API Endpoint** | https://vibee-mcp.fly.dev/graphql |
| **Playground** | https://vibee-mcp.fly.dev/graphql/playground |
| **Логи** | `fly logs -a vibee-mcp` |

---

## ❓ FAQ

### Почему запрос возвращает пустой объект?
Проверьте, что данные существуют в БД. Используйте `leads` query для просмотра всех лидов.

### Как узнать ID лида?
Выполните `query { leads { id username } }` чтобы увидеть все ID.

### Что означает ошибка "Missing required argument"?
Вы забыли передать обязательный параметр. Проверьте таблицу параметров для этого метода.
