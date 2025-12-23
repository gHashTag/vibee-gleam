// GraphQL API Handlers
// HTTP endpoints for GraphQL queries, mutations, and subscriptions

import gleam/bit_array
import gleam/bytes_tree
import gleam/dict
import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import mist.{type Connection, type ResponseData}
import vibee/graphql/executor
import vibee/graphql/parser
import vibee/graphql/resolvers/lead_resolvers
import vibee/graphql/types

// === HTTP HANDLERS ===

/// Handle GraphQL HTTP POST requests (queries and mutations)
pub fn query_handler(req: Request(Connection)) -> Response(ResponseData) {
  case mist.read_body(req, 1024 * 1024) {
    Error(_) -> error_response(400, "Failed to read request body")

    Ok(req_with_body) -> {
      case bit_array.to_string(req_with_body.body) {
        Error(_) -> error_response(400, "Invalid UTF-8 in request body")

        Ok(body_str) -> {
          case parse_graphql_request(body_str) {
            Error(msg) -> error_response(400, msg)

            Ok(gql_request) -> {
              // Check for introspection query
              case string.contains(gql_request.query, "__schema") || string.contains(gql_request.query, "__type") {
                True -> introspection_response()
                False -> {
                  case parser.parse(gql_request.query) {
                    Error(parse_error) ->
                      graphql_error_response(
                        "Parse error: " <> parser.error_to_string(parse_error),
                      )

                    Ok(document) -> {
                      let registry = lead_resolvers.build_registry()
                      let variables =
                        dict.map_values(gql_request.variables, fn(_k, _v) {
                          types.NullValue
                        })
                      let response =
                        executor.execute(
                          document,
                          variables,
                          gql_request.operation_name,
                          registry,
                        )
                      graphql_response(response)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Handle GraphQL GET requests (introspection, playground redirect)
pub fn get_handler(_req: Request(Connection)) -> Response(ResponseData) {
  response.new(302)
  |> response.set_header("Location", "/graphql/playground")
  |> response.set_body(mist.Bytes(bytes_tree.new()))
}

/// Serve GraphQL Playground HTML
pub fn playground_handler(_req: Request(Connection)) -> Response(ResponseData) {
  let html =
    "<!DOCTYPE html>
<html>
<head>
  <meta charset=utf-8/>
  <meta name=\"viewport\" content=\"user-scalable=no, initial-scale=1.0, minimum-scale=1.0, maximum-scale=1.0, minimal-ui\">
  <title>GraphQL Playground - VIBEE</title>
  <link rel=\"stylesheet\" href=\"//cdn.jsdelivr.net/npm/graphql-playground-react/build/static/css/index.css\" />
  <link rel=\"shortcut icon\" href=\"//cdn.jsdelivr.net/npm/graphql-playground-react/build/favicon.png\" />
  <script src=\"//cdn.jsdelivr.net/npm/graphql-playground-react/build/static/js/middleware.js\"></script>
</head>
<body>
  <div id=\"root\">
    <style>
      body { background-color: rgb(23, 42, 58); font-family: Open Sans, sans-serif; height: 90vh; }
      #root { height: 100%; width: 100%; display: flex; align-items: center; justify-content: center; }
      .loading { font-size: 32px; font-weight: 200; color: rgba(255, 255, 255, .6); margin-left: 28px; }
      img { width: 78px; height: 78px; }
      .title { font-weight: 400; }
    </style>
    <img src='//cdn.jsdelivr.net/npm/graphql-playground-react/build/logo.png' alt=''>
    <div class=\"loading\">Loading <span class=\"title\">GraphQL Playground</span></div>
  </div>
  <script>window.addEventListener('load', function (event) {
    GraphQLPlayground.init(document.getElementById('root'), {
      endpoint: '/graphql',
      subscriptionEndpoint: '/graphql/sse',
      settings: {
        'editor.theme': 'dark',
        'editor.cursorShape': 'line',
        'editor.reuseHeaders': true,
        'tracing.hideTracingResponse': true,
        'schema.polling.enable': false
      }
    })
  })</script>
</body>
</html>"

  response.new(200)
  |> response.set_header("Content-Type", "text/html; charset=utf-8")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(html)))
}

// === SSE HANDLER ===

/// Handle GraphQL SSE subscription requests
pub fn sse_handler(_req: Request(Connection)) -> Response(ResponseData) {
  let msg =
    json.object([
      #(
        "message",
        json.string(
          "SSE endpoint ready. Use WebSocket at /graphql/ws for real-time subscriptions.",
        ),
      ),
    ])
    |> json.to_string

  response.new(200)
  |> response.set_header("Content-Type", "application/json")
  |> response.set_header("Access-Control-Allow-Origin", "*")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(msg)))
}

// === HELPER FUNCTIONS ===

/// Parse GraphQL request from JSON body
fn parse_graphql_request(
  body: String,
) -> Result(types.GraphQLRequest, String) {
  let decoder = {
    use query <- decode.field("query", decode.string)
    decode.success(types.GraphQLRequest(
      query: query,
      operation_name: None,
      variables: dict.new(),
    ))
  }

  case json.parse(body, decoder) {
    Ok(req) -> Ok(req)
    Error(_) -> Error("Invalid GraphQL request format")
  }
}

/// Create GraphQL error response
fn graphql_error_response(message: String) -> Response(ResponseData) {
  let body =
    json.object([
      #("data", json.null()),
      #(
        "errors",
        json.array([json.object([#("message", json.string(message))])], fn(x) {
          x
        }),
      ),
    ])
    |> json.to_string

  response.new(200)
  |> response.set_header("Content-Type", "application/json")
  |> response.set_header("Access-Control-Allow-Origin", "*")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

/// Create GraphQL success response
fn graphql_response(resp: types.GraphQLResponse) -> Response(ResponseData) {
  let body =
    json.object([
      #(
        "data",
        case resp.data {
          Some(d) -> d
          None -> json.null()
        },
      ),
      #(
        "errors",
        case resp.errors {
          Some(errs) ->
            json.array(errs, fn(e) {
              json.object([#("message", json.string(e.message))])
            })
          None -> json.null()
        },
      ),
    ])
    |> json.to_string

  response.new(200)
  |> response.set_header("Content-Type", "application/json")
  |> response.set_header("Access-Control-Allow-Origin", "*")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

/// Create error response
fn error_response(status: Int, message: String) -> Response(ResponseData) {
  let body =
    json.object([#("error", json.string(message))])
    |> json.to_string

  response.new(status)
  |> response.set_header("Content-Type", "application/json")
  |> response.set_header("Access-Control-Allow-Origin", "*")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

/// GraphQL introspection response - returns schema for Lead Automation API
fn introspection_response() -> Response(ResponseData) {
  let schema = json.object([
    #("__schema", json.object([
      #("queryType", json.object([#("name", json.string("Query"))])),
      #("mutationType", json.object([#("name", json.string("Mutation"))])),
      #("subscriptionType", json.object([#("name", json.string("Subscription"))])),
      #("types", json.array([
        // Query type
        object_type_desc("Query", "Корневой тип для всех запросов GraphQL API", [
          field_def_desc("lead", "Lead", "Получить лида по ID", [
            arg_def_desc("id", "Int", "Уникальный идентификатор лида", True),
          ]),
          field_def_desc("leads", "[Lead!]!", "Список лидов с фильтрацией и пагинацией", [
            arg_def_desc("status", "LeadStatus", "Фильтр по статусу (NEW, CONTACTED, QUALIFIED...)", False),
            arg_def_desc("limit", "Int", "Максимальное количество записей (по умолчанию 50)", False),
            arg_def_desc("offset", "Int", "Смещение для пагинации", False),
          ]),
          field_def_desc("leadForwards", "[LeadForward!]!", "История пересылок лидов в CRM-группу", [
            arg_def_desc("leadId", "Int", "Фильтр по ID лида", False),
            arg_def_desc("status", "ForwardStatus", "Фильтр по статусу пересылки", False),
            arg_def_desc("limit", "Int", "Максимальное количество записей", False),
          ]),
          field_def_desc("triggerConfigs", "[TriggerConfig!]!", "Конфигурация триггер-чатов для автоматического обнаружения лидов", []),
          field_def_desc("funnelStats", "FunnelStats!", "Статистика воронки продаж по этапам", []),
        ]),
        // Mutation type
        object_type_desc("Mutation", "Корневой тип для изменения данных", [
          field_def_desc("updateLeadStatus", "Lead!", "Обновить статус лида в воронке продаж", [
            arg_def_desc("leadId", "Int", "ID лида для обновления", True),
            arg_def_desc("status", "LeadStatus", "Новый статус лида", True),
          ]),
        ]),
        // Subscription type
        object_type_desc("Subscription", "Подписки на события в реальном времени (WebSocket/SSE)", [
          field_def_desc("leadForwarded", "LeadForward!", "Новый лид переслан в CRM-группу", [
            arg_def_desc("targetChatId", "String", "Фильтр по целевому чату", False),
          ]),
          field_def_desc("leadStatusChanged", "LeadStatusChange!", "Статус лида изменился", [
            arg_def_desc("leadId", "Int", "Фильтр по ID лида", False),
          ]),
          field_def_desc("triggerDetected", "TriggerEvent!", "Обнаружено триггерное слово в чате", [
            arg_def_desc("chatId", "String", "Фильтр по ID чата", False),
          ]),
          field_def_desc("forwardFailed", "ForwardError!", "Ошибка при пересылке лида", [
            arg_def_desc("targetChatId", "String", "Фильтр по целевому чату", False),
          ]),
        ]),
        // Lead type
        object_type_desc("Lead", "Лид - потенциальный клиент из Telegram", [
          field_desc("id", "Int!", "Уникальный ID лида в базе"),
          field_desc("telegramUserId", "Int!", "Telegram User ID"),
          field_desc("username", "String", "Username в Telegram (@username)"),
          field_desc("firstName", "String", "Имя пользователя"),
          field_desc("lastName", "String", "Фамилия пользователя"),
          field_desc("status", "LeadStatus!", "Текущий статус в CRM (NEW → WON/LOST)"),
          field_desc("funnelStage", "FunnelStage!", "Этап воронки продаж"),
          field_desc("priority", "LeadPriority!", "Приоритет обработки"),
          field_desc("qualityScore", "Int", "Оценка качества лида (1-10)"),
          field_desc("source", "String", "Источник лида (название чата)"),
          field_desc("createdAt", "String!", "Дата создания (ISO 8601)"),
        ]),
        // LeadForward type
        object_type_desc("LeadForward", "Запись о пересылке лида в CRM-группу", [
          field_desc("id", "Int!", "ID записи пересылки"),
          field_desc("leadId", "Int", "ID связанного лида"),
          field_desc("sourceChatId", "String!", "ID исходного чата"),
          field_desc("sourceChatName", "String!", "Название исходного чата"),
          field_desc("targetChatId", "String!", "ID целевой CRM-группы"),
          field_desc("qualityScore", "Int!", "Оценка качества (1-10)"),
          field_desc("intent", "String!", "Намерение: purchase, question, support"),
          field_desc("urgency", "String!", "Срочность: low, normal, high, urgent"),
          field_desc("status", "ForwardStatus!", "Статус пересылки"),
          field_desc("forwardedAt", "String!", "Время пересылки (ISO 8601)"),
        ]),
        // TriggerConfig type
        object_type_desc("TriggerConfig", "Конфигурация триггер-чата для обнаружения лидов", [
          field_desc("chatId", "String!", "Telegram Chat ID"),
          field_desc("chatName", "String!", "Название чата"),
          field_desc("isActive", "Boolean!", "Активен ли мониторинг"),
          field_desc("triggers", "[String!]!", "Список триггерных слов (крипта, купить...)"),
          field_desc("forwardChatId", "String!", "ID группы для пересылки лидов"),
        ]),
        // FunnelStats type
        object_type_desc("FunnelStats", "Статистика воронки продаж", [
          field_desc("awareness", "Int!", "Этап: Осведомленность"),
          field_desc("interest", "Int!", "Этап: Интерес"),
          field_desc("consideration", "Int!", "Этап: Рассмотрение"),
          field_desc("intent", "Int!", "Этап: Намерение"),
          field_desc("evaluation", "Int!", "Этап: Оценка"),
          field_desc("purchase", "Int!", "Этап: Покупка"),
          field_desc("total", "Int!", "Всего лидов"),
        ]),
        // LeadStatusChange type
        object_type_desc("LeadStatusChange", "Событие изменения статуса лида", [
          field_desc("leadId", "Int!", "ID лида"),
          field_desc("oldStatus", "LeadStatus!", "Предыдущий статус"),
          field_desc("newStatus", "LeadStatus!", "Новый статус"),
          field_desc("timestamp", "String!", "Время изменения (ISO 8601)"),
        ]),
        // TriggerEvent type
        object_type_desc("TriggerEvent", "Событие обнаружения триггера в чате", [
          field_desc("chatId", "String!", "ID чата где обнаружен триггер"),
          field_desc("trigger", "String!", "Сработавшее триггерное слово"),
          field_desc("userId", "Int!", "Telegram ID пользователя"),
          field_desc("username", "String", "Username пользователя"),
          field_desc("messageText", "String!", "Текст сообщения"),
          field_desc("timestamp", "String!", "Время события (ISO 8601)"),
        ]),
        // ForwardError type
        object_type_desc("ForwardError", "Ошибка при пересылке лида", [
          field_desc("userId", "Int!", "Telegram ID пользователя"),
          field_desc("username", "String", "Username пользователя"),
          field_desc("sourceChatId", "String!", "ID исходного чата"),
          field_desc("reason", "String!", "Причина ошибки: rate_limit, permission, network"),
          field_desc("errorMessage", "String", "Детальное сообщение об ошибке"),
          field_desc("timestamp", "String!", "Время ошибки (ISO 8601)"),
        ]),
        // Enums
        enum_type_desc("LeadStatus", "Статус лида в CRM воронке", [
          #("NEW", "Новый лид, ожидает обработки"),
          #("CONTACTED", "Связались с лидом"),
          #("QUALIFIED", "Квалифицирован как потенциальный клиент"),
          #("PROPOSAL_SENT", "Отправлено коммерческое предложение"),
          #("NEGOTIATION", "В процессе переговоров"),
          #("WON", "Сделка закрыта успешно"),
          #("LOST", "Сделка потеряна"),
        ]),
        enum_type_desc("FunnelStage", "Этап воронки продаж", [
          #("AWARENESS", "Осведомленность - клиент узнал о продукте"),
          #("INTEREST", "Интерес - проявляет активный интерес"),
          #("CONSIDERATION", "Рассмотрение - сравнивает варианты"),
          #("INTENT", "Намерение - готов к покупке"),
          #("EVALUATION", "Оценка - финальная проверка"),
          #("PURCHASE", "Покупка - сделка завершена"),
        ]),
        enum_type_desc("LeadPriority", "Приоритет обработки лида", [
          #("LOW", "Низкий приоритет"),
          #("MEDIUM", "Средний приоритет"),
          #("HIGH", "Высокий приоритет"),
          #("URGENT", "Срочный - требует немедленной обработки"),
        ]),
        enum_type_desc("ForwardStatus", "Статус пересылки лида в CRM", [
          #("PENDING", "Ожидает отправки"),
          #("FORWARDED", "Успешно переслан"),
          #("FAILED", "Ошибка при пересылке"),
          #("DEDUPLICATED", "Дедупликация - лид уже существует"),
          #("RATE_LIMITED", "Превышен лимит отправки"),
        ]),
        // Scalar types
        scalar_type("Int"),
        scalar_type("String"),
        scalar_type("Boolean"),
        scalar_type("ID"),
      ], fn(x) { x })),
      #("directives", json.array([], fn(x) { x })),
    ])),
  ])

  let body =
    json.object([#("data", schema)])
    |> json.to_string

  response.new(200)
  |> response.set_header("Content-Type", "application/json")
  |> response.set_header("Access-Control-Allow-Origin", "*")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

fn object_type_desc(name: String, description: String, fields: List(json.Json)) -> json.Json {
  json.object([
    #("kind", json.string("OBJECT")),
    #("name", json.string(name)),
    #("description", json.string(description)),
    #("fields", json.array(fields, fn(x) { x })),
    #("interfaces", json.array([], fn(x) { x })),
  ])
}

fn object_type(name: String, fields: List(json.Json)) -> json.Json {
  object_type_desc(name, "", fields)
}

fn field_desc(name: String, type_name: String, description: String) -> json.Json {
  json.object([
    #("name", json.string(name)),
    #("description", json.string(description)),
    #("args", json.array([], fn(x) { x })),
    #("type", type_ref(type_name)),
  ])
}

fn simple_field(name: String, type_name: String) -> json.Json {
  field_desc(name, type_name, "")
}

fn field_def_desc(name: String, type_name: String, description: String, args: List(json.Json)) -> json.Json {
  json.object([
    #("name", json.string(name)),
    #("description", json.string(description)),
    #("args", json.array(args, fn(x) { x })),
    #("type", type_ref(type_name)),
  ])
}

fn field_def(name: String, type_name: String, args: List(json.Json)) -> json.Json {
  field_def_desc(name, type_name, "", args)
}

fn arg_def_desc(name: String, type_name: String, description: String, required: Bool) -> json.Json {
  json.object([
    #("name", json.string(name)),
    #("description", json.string(description)),
    #("type", case required {
      True -> json.object([
        #("kind", json.string("NON_NULL")),
        #("ofType", json.object([
          #("kind", json.string("SCALAR")),
          #("name", json.string(type_name)),
        ])),
      ])
      False -> json.object([
        #("kind", json.string("SCALAR")),
        #("name", json.string(type_name)),
      ])
    }),
  ])
}

fn arg_def(name: String, type_name: String, required: Bool) -> json.Json {
  arg_def_desc(name, type_name, "", required)
}

fn type_ref(type_name: String) -> json.Json {
  case string.ends_with(type_name, "!") {
    True -> {
      let inner = string.drop_end(type_name, 1)
      case string.starts_with(inner, "[") {
        True -> {
          let list_inner = string.slice(inner, 1, string.length(inner) - 2)
          json.object([
            #("kind", json.string("NON_NULL")),
            #("ofType", json.object([
              #("kind", json.string("LIST")),
              #("ofType", type_ref(list_inner)),
            ])),
          ])
        }
        False -> json.object([
          #("kind", json.string("NON_NULL")),
          #("ofType", json.object([
            #("kind", json.string("SCALAR")),
            #("name", json.string(inner)),
          ])),
        ])
      }
    }
    False -> case string.starts_with(type_name, "[") {
      True -> {
        let inner = string.slice(type_name, 1, string.length(type_name) - 2)
        json.object([
          #("kind", json.string("LIST")),
          #("ofType", type_ref(inner)),
        ])
      }
      False -> json.object([
        #("kind", json.string("SCALAR")),
        #("name", json.string(type_name)),
      ])
    }
  }
}

fn enum_type_desc(name: String, description: String, values: List(#(String, String))) -> json.Json {
  json.object([
    #("kind", json.string("ENUM")),
    #("name", json.string(name)),
    #("description", json.string(description)),
    #("enumValues", json.array(values, fn(v) {
      json.object([
        #("name", json.string(v.0)),
        #("description", json.string(v.1)),
        #("isDeprecated", json.bool(False)),
      ])
    })),
  ])
}

fn enum_type(name: String, values: List(String)) -> json.Json {
  json.object([
    #("kind", json.string("ENUM")),
    #("name", json.string(name)),
    #("enumValues", json.array(values, fn(v) {
      json.object([
        #("name", json.string(v)),
        #("isDeprecated", json.bool(False)),
      ])
    })),
  ])
}

fn scalar_type(name: String) -> json.Json {
  json.object([
    #("kind", json.string("SCALAR")),
    #("name", json.string(name)),
  ])
}
