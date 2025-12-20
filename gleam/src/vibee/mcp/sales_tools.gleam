// MCP Sales Tools - Инструменты для системы продаж VIBEE
// Продукты, подписки, лиды, квиз, paywall

import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import vibee/mcp/types.{type Tool, type ToolResult, TextContent, Tool, ToolResult}
import vibee/sales/lead_service
import vibee/sales/paywall
import vibee/sales/product_catalog
import vibee/sales/proposal_generator
import vibee/sales/quiz
import vibee/sales/subscription
import vibee/sales/types as sales_types

// =============================================================================
// Tool Definitions
// =============================================================================

/// Получить список продуктов
pub fn sales_product_list_tool() -> Tool {
  Tool(
    name: "sales_product_list",
    description: "Получить список тарифов VIBEE с ценами и функциями. Junior ($99), Middle ($299), Senior ($999).",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("lang", json.object([
          #("type", json.string("string")),
          #("description", json.string("Язык: ru или en")),
          #("default", json.string("ru")),
        ])),
      ])),
      #("required", json.array([], json.string)),
    ]),
  )
}

/// Получить статус подписки
pub fn sales_subscription_status_tool() -> Tool {
  Tool(
    name: "sales_subscription_status",
    description: "Проверить статус подписки пользователя: активная, использование, лимиты.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Telegram ID пользователя")),
        ])),
      ])),
      #("required", json.array(["telegram_id"], json.string)),
    ]),
  )
}

/// Проверить доступ к функции
pub fn sales_check_access_tool() -> Tool {
  Tool(
    name: "sales_check_access",
    description: "Проверить, есть ли у пользователя доступ к функции (generation, api, voice_clone и т.д.).",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Telegram ID пользователя")),
        ])),
        #("action", json.object([
          #("type", json.string("string")),
          #("description", json.string("Действие для проверки")),
          #("enum", json.array([
            "generation", "api_access", "voice_clone", "multichannel", "custom_training", "white_label"
          ], json.string)),
        ])),
      ])),
      #("required", json.array(["telegram_id", "action"], json.string)),
    ]),
  )
}

/// Начать квиз
pub fn sales_start_quiz_tool() -> Tool {
  Tool(
    name: "sales_start_quiz",
    description: "Начать квалификационный квиз для определения подходящего тарифа. Возвращает первый вопрос.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Telegram ID пользователя")),
        ])),
        #("lang", json.object([
          #("type", json.string("string")),
          #("description", json.string("Язык: ru или en")),
          #("default", json.string("ru")),
        ])),
      ])),
      #("required", json.array(["telegram_id"], json.string)),
    ]),
  )
}

/// Получить вопросы квиза
pub fn sales_quiz_questions_tool() -> Tool {
  Tool(
    name: "sales_quiz_questions",
    description: "Получить все вопросы квалификационного квиза.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("lang", json.object([
          #("type", json.string("string")),
          #("description", json.string("Язык: ru или en")),
          #("default", json.string("ru")),
        ])),
      ])),
      #("required", json.array([], json.string)),
    ]),
  )
}

/// Отправить результаты квиза
pub fn sales_submit_quiz_tool() -> Tool {
  Tool(
    name: "sales_submit_quiz",
    description: "Отправить ответы на квиз и получить рекомендацию по тарифу.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("telegram_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Telegram ID пользователя")),
        ])),
        #("answers", json.object([
          #("type", json.string("array")),
          #("description", json.string("Массив ответов: [{question_id, answer}]")),
          #("items", json.object([
            #("type", json.string("object")),
            #("properties", json.object([
              #("question_id", json.object([#("type", json.string("string"))])),
              #("answer", json.object([#("type", json.string("string"))])),
            ])),
          ])),
        ])),
        #("lang", json.object([
          #("type", json.string("string")),
          #("default", json.string("ru")),
        ])),
      ])),
      #("required", json.array(["telegram_id", "answers"], json.string)),
    ]),
  )
}

/// Создать/обновить лида
pub fn sales_lead_create_tool() -> Tool {
  Tool(
    name: "sales_lead_create",
    description: "Создать или обновить лида в CRM. Используется при первом контакте.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("telegram_user_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Telegram ID пользователя")),
        ])),
        #("username", json.object([
          #("type", json.string("string")),
          #("description", json.string("Username в Telegram")),
        ])),
        #("first_name", json.object([
          #("type", json.string("string")),
          #("description", json.string("Имя пользователя")),
        ])),
        #("first_message", json.object([
          #("type", json.string("string")),
          #("description", json.string("Первое сообщение пользователя")),
        ])),
        #("source", json.object([
          #("type", json.string("string")),
          #("description", json.string("Источник: telegram, instagram, website")),
          #("default", json.string("telegram")),
        ])),
      ])),
      #("required", json.array(["telegram_user_id"], json.string)),
    ]),
  )
}

/// Получить статистику воронки
pub fn sales_funnel_stats_tool() -> Tool {
  Tool(
    name: "sales_funnel_stats",
    description: "Получить статистику воронки продаж: количество лидов на каждой стадии.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
      #("required", json.array([], json.string)),
    ]),
  )
}

/// Сгенерировать коммерческое предложение
pub fn sales_generate_proposal_tool() -> Tool {
  Tool(
    name: "sales_generate_proposal",
    description: "Сгенерировать персонализированное коммерческое предложение для лида. Возвращает текст и HTML.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("telegram_user_id", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Telegram ID пользователя")),
        ])),
        #("product_code", json.object([
          #("type", json.string("string")),
          #("description", json.string("Код тарифа: junior, middle, senior")),
          #("enum", json.array(["junior", "middle", "senior"], json.string)),
        ])),
        #("discount_percent", json.object([
          #("type", json.string("integer")),
          #("description", json.string("Скидка в процентах (0-50)")),
          #("default", json.int(0)),
        ])),
        #("lang", json.object([
          #("type", json.string("string")),
          #("description", json.string("Язык: ru или en")),
          #("default", json.string("ru")),
        ])),
      ])),
      #("required", json.array(["telegram_user_id", "product_code"], json.string)),
    ]),
  )
}

/// Получить все инструменты продаж
pub fn get_sales_tools() -> List(Tool) {
  [
    sales_product_list_tool(),
    sales_subscription_status_tool(),
    sales_check_access_tool(),
    sales_start_quiz_tool(),
    sales_quiz_questions_tool(),
    sales_submit_quiz_tool(),
    sales_lead_create_tool(),
    sales_funnel_stats_tool(),
    sales_generate_proposal_tool(),
  ]
}

/// Получить все обработчики продаж
pub fn get_sales_handlers() -> List(#(String, fn(json.Json) -> ToolResult)) {
  [
    #("sales_product_list", handle_product_list),
    #("sales_subscription_status", handle_subscription_status),
    #("sales_check_access", handle_check_access),
    #("sales_start_quiz", handle_start_quiz),
    #("sales_quiz_questions", handle_quiz_questions),
    #("sales_submit_quiz", handle_submit_quiz),
    #("sales_lead_create", handle_lead_create),
    #("sales_funnel_stats", handle_funnel_stats),
    #("sales_generate_proposal", handle_generate_proposal),
  ]
}

// =============================================================================
// Handlers
// =============================================================================

fn handle_product_list(args: json.Json) -> ToolResult {
  case product_catalog.get_all_products() {
    Error(e) -> error_result(product_catalog.error_to_string(e))
    Ok(products) -> {
      let response = json.object([
        #("success", json.bool(True)),
        #("products", product_catalog.products_to_json(products)),
        #("comparison_url", json.string("/pricing")),
      ])
      success_result(json.to_string(response))
    }
  }
}

fn handle_subscription_status(args: json.Json) -> ToolResult {
  case json_get_int(args, "telegram_id") {
    Error(_) -> error_result("Missing required parameter: telegram_id")
    Ok(telegram_id) -> {
      case subscription.get_active_subscription(telegram_id) {
        Error(subscription.NotFoundError(_)) -> {
          let response = json.object([
            #("success", json.bool(True)),
            #("has_subscription", json.bool(False)),
            #("message", json.string("Нет активной подписки")),
          ])
          success_result(json.to_string(response))
        }
        Error(e) -> error_result(subscription.error_to_string(e))
        Ok(sub) -> {
          // Получить информацию о продукте
          let product_info = case product_catalog.get_product_by_id(sub.product_id) {
            Ok(p) -> Some(p)
            Error(_) -> None
          }

          let response = json.object([
            #("success", json.bool(True)),
            #("has_subscription", json.bool(True)),
            #("subscription", subscription.subscription_to_json(sub)),
            #("product", case product_info {
              Some(p) -> product_catalog.product_to_json(p)
              None -> json.null()
            }),
            #("generations_used", json.int(sub.generations_used)),
            #("limit", case product_info {
              Some(p) -> case p.generation_limit {
                Some(limit) -> json.int(limit)
                None -> json.string("unlimited")
              }
              None -> json.null()
            }),
          ])
          success_result(json.to_string(response))
        }
      }
    }
  }
}

fn handle_check_access(args: json.Json) -> ToolResult {
  case json_get_int(args, "telegram_id") {
    Error(_) -> error_result("Missing required parameter: telegram_id")
    Ok(telegram_id) -> {
      case json_get_string(args, "action") {
        Error(_) -> error_result("Missing required parameter: action")
        Ok(action_str) -> {
          let action = parse_action(action_str)
          let result = paywall.check_access(telegram_id, action)
          let response = json.object([
            #("success", json.bool(True)),
            #("access", paywall.access_result_to_json(result)),
            #("message", json.string(paywall.get_access_message(result, "ru"))),
          ])
          success_result(json.to_string(response))
        }
      }
    }
  }
}

fn handle_start_quiz(args: json.Json) -> ToolResult {
  case json_get_int(args, "telegram_id") {
    Error(_) -> error_result("Missing required parameter: telegram_id")
    Ok(telegram_id) -> {
      let lang = json_get_string(args, "lang") |> result.unwrap("ru")
      let quiz_session = quiz.start_quiz(telegram_id)

      case quiz.get_next_question(quiz_session) {
        None -> error_result("No questions available")
        Some(question) -> {
          let response = json.object([
            #("success", json.bool(True)),
            #("quiz_started", json.bool(True)),
            #("total_questions", json.int(5)),
            #("current_question", quiz.question_to_json(question, lang)),
          ])
          success_result(json.to_string(response))
        }
      }
    }
  }
}

fn handle_quiz_questions(args: json.Json) -> ToolResult {
  let lang = json_get_string(args, "lang") |> result.unwrap("ru")
  let response = json.object([
    #("success", json.bool(True)),
    #("questions", quiz.questions_to_json(lang)),
  ])
  success_result(json.to_string(response))
}

fn handle_submit_quiz(args: json.Json) -> ToolResult {
  case json_get_int(args, "telegram_id") {
    Error(_) -> error_result("Missing required parameter: telegram_id")
    Ok(telegram_id) -> {
      let lang = json_get_string(args, "lang") |> result.unwrap("ru")

      // Парсим ответы
      case json_get_answers(args, "answers") {
        Error(_) -> error_result("Missing or invalid: answers")
        Ok(answers) -> {
          // Создаём сессию квиза и добавляем ответы
          let quiz_session = list.fold(answers, quiz.start_quiz(telegram_id), fn(q, ans) {
            quiz.add_answer(q, ans.question_id, ans.answer)
          })

          // Завершаем квиз
          let completed = quiz.complete_quiz(quiz_session)

          // Обновляем лида с результатами
          case completed.recommended_product_id {
            Some(product_id) -> {
              case completed.score {
                Some(score) -> {
                  // Получить или создать лида
                  case lead_service.get_or_create_lead(telegram_id, None, None, None, Some("quiz")) {
                    Ok(lead) -> {
                      case lead.id {
                        Some(lead_id) -> {
                          let _ = lead_service.update_quiz_result(lead_id, score, product_id)
                          Nil
                        }
                        None -> Nil
                      }
                    }
                    Error(_) -> Nil
                  }
                }
                None -> Nil
              }
            }
            None -> Nil
          }

          let response = json.object([
            #("success", json.bool(True)),
            #("result", quiz.quiz_result_to_json(completed, lang)),
          ])
          success_result(json.to_string(response))
        }
      }
    }
  }
}

fn handle_lead_create(args: json.Json) -> ToolResult {
  case json_get_int(args, "telegram_user_id") {
    Error(_) -> error_result("Missing required parameter: telegram_user_id")
    Ok(telegram_user_id) -> {
      let username = json_get_string(args, "username") |> option.from_result()
      let first_name = json_get_string(args, "first_name") |> option.from_result()
      let first_message = json_get_string(args, "first_message") |> option.from_result()
      let source = json_get_string(args, "source") |> option.from_result()

      case lead_service.get_or_create_lead(
        telegram_user_id, username, first_name, first_message, source
      ) {
        Error(e) -> error_result(lead_service.error_to_string(e))
        Ok(lead) -> {
          let response = json.object([
            #("success", json.bool(True)),
            #("lead", lead_service.lead_to_json(lead)),
          ])
          success_result(json.to_string(response))
        }
      }
    }
  }
}

fn handle_funnel_stats(_args: json.Json) -> ToolResult {
  case lead_service.get_funnel_stats() {
    Error(e) -> error_result(lead_service.error_to_string(e))
    Ok(stats) -> {
      let response = json.object([
        #("success", json.bool(True)),
        #("funnel", lead_service.funnel_stats_to_json(stats)),
      ])
      success_result(json.to_string(response))
    }
  }
}

fn handle_generate_proposal(args: json.Json) -> ToolResult {
  case json_get_int(args, "telegram_user_id") {
    Error(_) -> error_result("Missing required parameter: telegram_user_id")
    Ok(telegram_user_id) -> {
      case json_get_string(args, "product_code") {
        Error(_) -> error_result("Missing required parameter: product_code")
        Ok(product_code) -> {
          let discount = json_get_int(args, "discount_percent") |> result.unwrap(0)
          let lang = json_get_string(args, "lang") |> result.unwrap("ru")

          // Получить или создать лида
          case lead_service.get_or_create_lead(telegram_user_id, None, None, None, Some("proposal")) {
            Error(e) -> error_result(lead_service.error_to_string(e))
            Ok(lead) -> {
              // Генерируем КП
              case proposal_generator.generate_for_lead(lead, product_code, discount, lang) {
                Error(proposal_generator.InvalidLeadError(msg)) ->
                  error_result("Invalid lead: " <> msg)
                Error(proposal_generator.InvalidProductError(msg)) ->
                  error_result("Invalid product: " <> msg)
                Error(proposal_generator.GenerationError(msg)) ->
                  error_result("Generation error: " <> msg)
                Ok(generated) -> {
                  let response = json.object([
                    #("success", json.bool(True)),
                    #("proposal", proposal_generator.generated_proposal_to_json(generated)),
                  ])
                  success_result(json.to_string(response))
                }
              }
            }
          }
        }
      }
    }
  }
}

// =============================================================================
// Helpers
// =============================================================================

fn success_result(content: String) -> ToolResult {
  ToolResult(content: [TextContent(content)], is_error: False)
}

fn error_result(message: String) -> ToolResult {
  let error_json = json.object([
    #("success", json.bool(False)),
    #("error", json.string(message)),
  ])
  ToolResult(content: [TextContent(json.to_string(error_json))], is_error: True)
}

fn json_get_string(j: json.Json, key: String) -> Result(String, Nil) {
  let decoder = {
    use v <- decode.field(key, decode.string)
    decode.success(v)
  }
  let json_str = json.to_string(j)
  case json.parse(json_str, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

fn json_get_int(j: json.Json, key: String) -> Result(Int, Nil) {
  let decoder = {
    use v <- decode.field(key, decode.int)
    decode.success(v)
  }
  let json_str = json.to_string(j)
  case json.parse(json_str, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

fn json_get_answers(j: json.Json, key: String) -> Result(List(sales_types.QuizAnswer), Nil) {
  let answer_decoder = {
    use qid <- decode.field("question_id", decode.string)
    use ans <- decode.field("answer", decode.string)
    decode.success(sales_types.QuizAnswer(question_id: qid, answer: ans, score: 0))
  }

  let decoder = {
    use v <- decode.field(key, decode.list(answer_decoder))
    decode.success(v)
  }

  let json_str = json.to_string(j)
  case json.parse(json_str, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

fn parse_action(s: String) -> paywall.ProtectedAction {
  case s {
    "generation" -> paywall.Generation
    "api_access" -> paywall.ApiAccess
    "voice_clone" -> paywall.VoiceClone
    "multichannel" -> paywall.Multichannel
    "custom_training" -> paywall.CustomTraining
    "white_label" -> paywall.WhiteLabel
    _ -> paywall.Generation
  }
}
