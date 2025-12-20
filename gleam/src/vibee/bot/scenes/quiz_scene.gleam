// Quiz Scene - Квалификационный квиз в Telegram боте
// /quiz команда для определения подходящего тарифа

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/bot/scene.{
  type UserSession, Quiz, QuizStart, QuizQuestion, QuizResult,
  Pricing, PricingList, Subscription, SubscriptionPayment, set_scene,
}
import vibee/bot/scene_types
import vibee/sales/quiz
import vibee/sales/lead_service
import vibee/sales/types as sales_types

// =============================================================================
// Scene Entry
// =============================================================================

/// Обработать команду /quiz
pub fn handle_quiz_command(session: UserSession) -> scene_types.SceneResult {
  let new_session = set_scene(session, Quiz(QuizStart))

  let questions = quiz.get_qualification_questions()
  case questions {
    [] -> scene_types.reply(new_session, "Квиз недоступен")
    [first, ..] -> {
      let message = format_quiz_intro() <> "\n\n" <> format_question(first, 1, list.length(questions))
      let buttons = question_buttons(first)
      let session_with_answers = set_scene(session, Quiz(QuizQuestion(0, [])))
      scene_types.send_buttons(session_with_answers, message, buttons)
    }
  }
}

// =============================================================================
// Callback Handlers
// =============================================================================

/// Обработать ответ на вопрос квиза
pub fn handle_callback(
  session: UserSession,
  callback_data: String,
) -> scene_types.SceneResult {
  case string.split(callback_data, ":") {
    ["quiz", "answer", question_id, answer_value] ->
      process_answer(session, question_id, answer_value)
    ["quiz", "restart"] ->
      handle_quiz_command(session)
    ["quiz", "subscribe", code] ->
      go_to_subscription(session, code)
    ["quiz", "pricing"] ->
      go_to_pricing(session)
    _ -> scene_types.reply(session, "Неизвестная команда")
  }
}

/// Обработать ответ на вопрос
fn process_answer(
  session: UserSession,
  question_id: String,
  answer_value: String,
) -> scene_types.SceneResult {
  let questions = quiz.get_qualification_questions()

  // Получить текущие ответы из сессии
  let #(current_index, current_answers) = case session.scene {
    Quiz(QuizQuestion(idx, answers)) -> #(idx, answers)
    _ -> #(0, [])
  }

  // Добавить новый ответ
  let new_answers = list.append(current_answers, [#(question_id, answer_value)])
  let new_index = current_index + 1

  // Проверить, есть ли ещё вопросы
  case list.drop(questions, new_index) {
    [] -> {
      // Квиз завершён - подсчитать результат
      calculate_and_show_result(session, new_answers)
    }
    [next_question, ..] -> {
      // Показать следующий вопрос
      let new_session = set_scene(session, Quiz(QuizQuestion(new_index, new_answers)))
      let message = format_question(next_question, new_index + 1, list.length(questions))
      let buttons = question_buttons(next_question)
      scene_types.send_buttons(new_session, message, buttons)
    }
  }
}

/// Подсчитать и показать результат
fn calculate_and_show_result(
  session: UserSession,
  answers: List(#(String, String)),
) -> scene_types.SceneResult {
  // Конвертировать ответы в формат квиза
  let _quiz_answers = list.map(answers, fn(pair) {
    let #(qid, ans) = pair
    sales_types.QuizAnswer(question_id: qid, answer: ans, score: 0)
  })

  // Создать сессию квиза и добавить ответы
  let quiz_session = list.fold(answers, quiz.start_quiz(session.user_id), fn(q, pair) {
    let #(qid, ans) = pair
    quiz.add_answer(q, qid, ans)
  })

  // Завершить квиз
  let completed = quiz.complete_quiz(quiz_session)

  let score = case completed.score {
    Some(s) -> s
    None -> 0
  }

  let product_code = quiz.recommend_product(score)

  // Обновить лида в базе
  case completed.recommended_product_id {
    Some(product_id) -> {
      case lead_service.get_or_create_lead(session.user_id, None, None, None, Some("quiz")) {
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

  let new_session = set_scene(session, Quiz(QuizResult(score, product_code)))

  let message = format_result(score, product_code)
  let buttons = result_buttons(product_code)
  scene_types.send_buttons(new_session, message, buttons)
}

/// Перейти к подписке
fn go_to_subscription(session: UserSession, code: String) -> scene_types.SceneResult {
  let new_session = set_scene(session, Subscription(SubscriptionPayment(code, "")))
  scene_types.reply(new_session, "Переходим к оформлению подписки " <> code <> "...")
}

/// Перейти к ценам
fn go_to_pricing(session: UserSession) -> scene_types.SceneResult {
  let new_session = set_scene(session, Pricing(PricingList))
  scene_types.reply(new_session, "Переходим к тарифам...")
}

// =============================================================================
// Formatters
// =============================================================================

fn format_quiz_intro() -> String {
  "🎯 *Квиз: Какой тариф VIBEE вам подходит?*\n\n"
  <> "Ответьте на 5 вопросов, и мы подберём оптимальный план для вас.\n"
  <> "Это займёт менее минуты!"
}

fn format_question(question: sales_types.QuizQuestion, num: Int, total: Int) -> String {
  "📝 *Вопрос " <> int.to_string(num) <> " из " <> int.to_string(total) <> "*\n\n"
  <> question.question_ru
}

fn format_result(score: Int, product_code: String) -> String {
  let product_name = case product_code {
    "junior" -> "VIBEE Junior"
    "middle" -> "VIBEE Middle"
    "senior" -> "VIBEE Senior"
    _ -> product_code
  }

  let emoji = case product_code {
    "junior" -> "🌱"
    "middle" -> "🚀"
    "senior" -> "💎"
    _ -> "✨"
  }

  let description = case product_code {
    "junior" -> "Отличный выбор для старта! Базовые функции AI-ассистента по доступной цене."
    "middle" -> "Оптимальный баланс! CRM, аналитика и расширенные возможности для растущего бизнеса."
    "senior" -> "Максимум возможностей! Безлимит, API и полная кастомизация для масштабных проектов."
    _ -> ""
  }

  "🎉 *Квиз завершён!*\n\n"
  <> "Ваш балл: *" <> int.to_string(score) <> "*\n\n"
  <> emoji <> " *Рекомендуем: " <> product_name <> "*\n\n"
  <> description
}

// =============================================================================
// Buttons
// =============================================================================

fn question_buttons(question: sales_types.QuizQuestion) -> List(List(#(String, String))) {
  question.options
  |> list.map(fn(opt) {
    [#(opt.label_ru, "quiz:answer:" <> question.id <> ":" <> opt.value)]
  })
}

fn result_buttons(product_code: String) -> List(List(#(String, String))) {
  [
    [#("🛒 Подписаться на " <> product_code, "quiz:subscribe:" <> product_code)],
    [#("📊 Все тарифы", "quiz:pricing"), #("🔄 Пройти заново", "quiz:restart")],
  ]
}
