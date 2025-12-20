// Quiz Service - Квалификационный квиз для определения тарифа
// Вопросы для оценки потребностей клиента

import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import vibee/sales/types.{
  type QuizAnswer, type QuizOption, type QuizQuestion, type QuizResponse,
  QuizAnswer, QuizOption, QuizQualification, QuizQuestion, QuizResponse,
}

// =============================================================================
// Quiz Questions
// =============================================================================

/// Получить вопросы квалификационного квиза
pub fn get_qualification_questions() -> List(QuizQuestion) {
  [
    QuizQuestion(
      id: "team_size",
      question_ru: "Сколько человек в вашей команде?",
      question_en: "How many people are on your team?",
      options: [
        QuizOption(value: "solo", label_ru: "Только я", label_en: "Just me", score: 10),
        QuizOption(value: "small", label_ru: "2-5 человек", label_en: "2-5 people", score: 20),
        QuizOption(value: "medium", label_ru: "6-20 человек", label_en: "6-20 people", score: 30),
        QuizOption(value: "large", label_ru: "20+ человек", label_en: "20+ people", score: 40),
      ],
      weight: 2,
    ),
    QuizQuestion(
      id: "monthly_interactions",
      question_ru: "Сколько взаимодействий с клиентами в месяц?",
      question_en: "How many customer interactions per month?",
      options: [
        QuizOption(value: "few", label_ru: "До 100", label_en: "Up to 100", score: 10),
        QuizOption(value: "moderate", label_ru: "100-500", label_en: "100-500", score: 25),
        QuizOption(value: "many", label_ru: "500-2000", label_en: "500-2000", score: 35),
        QuizOption(value: "massive", label_ru: "2000+", label_en: "2000+", score: 50),
      ],
      weight: 3,
    ),
    QuizQuestion(
      id: "channels",
      question_ru: "Какие каналы коммуникации используете?",
      question_en: "What communication channels do you use?",
      options: [
        QuizOption(value: "telegram_only", label_ru: "Только Telegram", label_en: "Telegram only", score: 10),
        QuizOption(value: "telegram_email", label_ru: "Telegram + Email", label_en: "Telegram + Email", score: 20),
        QuizOption(value: "multi", label_ru: "Telegram + соцсети", label_en: "Telegram + social", score: 30),
        QuizOption(value: "omni", label_ru: "Все каналы", label_en: "All channels", score: 40),
      ],
      weight: 2,
    ),
    QuizQuestion(
      id: "automation_level",
      question_ru: "Какой уровень автоматизации вам нужен?",
      question_en: "What level of automation do you need?",
      options: [
        QuizOption(value: "basic", label_ru: "Базовые ответы", label_en: "Basic responses", score: 10),
        QuizOption(value: "smart", label_ru: "Умные ответы + CRM", label_en: "Smart + CRM", score: 25),
        QuizOption(value: "advanced", label_ru: "Полная автоматизация", label_en: "Full automation", score: 35),
        QuizOption(value: "enterprise", label_ru: "Enterprise решение", label_en: "Enterprise", score: 50),
      ],
      weight: 3,
    ),
    QuizQuestion(
      id: "budget",
      question_ru: "Какой бюджет на AI-инструменты в месяц?",
      question_en: "What's your monthly AI tools budget?",
      options: [
        QuizOption(value: "starter", label_ru: "До $100", label_en: "Up to $100", score: 10),
        QuizOption(value: "growth", label_ru: "$100-300", label_en: "$100-300", score: 25),
        QuizOption(value: "scale", label_ru: "$300-1000", label_en: "$300-1000", score: 40),
        QuizOption(value: "enterprise", label_ru: "$1000+", label_en: "$1000+", score: 50),
      ],
      weight: 2,
    ),
  ]
}

// =============================================================================
// Quiz Scoring
// =============================================================================

/// Рассчитать итоговый балл квиза
pub fn calculate_score(answers: List(QuizAnswer)) -> Int {
  let questions = get_qualification_questions()

  list.fold(answers, 0, fn(acc, answer) {
    let weight = case list.find(questions, fn(q) { q.id == answer.question_id }) {
      Ok(q) -> q.weight
      Error(_) -> 1
    }
    acc + answer.score * weight
  })
}

/// Определить рекомендуемый продукт по баллам
pub fn recommend_product(score: Int) -> String {
  case score {
    s if s < 80 -> "junior"
    s if s < 180 -> "middle"
    _ -> "senior"
  }
}

/// Получить ID продукта по коду
pub fn get_product_id_by_code(code: String) -> Int {
  case code {
    "junior" -> 1
    "middle" -> 2
    "senior" -> 3
    _ -> 1
  }
}

/// Получить описание рекомендации
pub fn get_recommendation_description(product_code: String, score: Int, lang: String) -> String {
  let prefix = case lang {
    "en" -> "Based on your answers (score: " <> int.to_string(score) <> "), we recommend "
    _ -> "На основе ваших ответов (балл: " <> int.to_string(score) <> "), мы рекомендуем "
  }

  let product_name = case product_code {
    "junior" -> case lang {
      "en" -> "VIBEE Junior - perfect for starting with AI automation"
      _ -> "VIBEE Junior - идеален для начала работы с AI"
    }
    "middle" -> case lang {
      "en" -> "VIBEE Middle - for growing teams with CRM needs"
      _ -> "VIBEE Middle - для растущих команд с потребностью в CRM"
    }
    "senior" -> case lang {
      "en" -> "VIBEE Senior - enterprise solution for maximum scale"
      _ -> "VIBEE Senior - enterprise решение для максимального масштаба"
    }
    _ -> product_code
  }

  prefix <> product_name
}

// =============================================================================
// Quiz Flow
// =============================================================================

/// Создать новую сессию квиза
pub fn start_quiz(telegram_id: Int) -> QuizResponse {
  QuizResponse(
    id: None,
    telegram_id: telegram_id,
    lead_id: None,
    quiz_type: QuizQualification,
    responses: [],
    score: None,
    recommended_product_id: None,
    completed_at: None,
  )
}

/// Добавить ответ на вопрос
pub fn add_answer(
  quiz: QuizResponse,
  question_id: String,
  answer_value: String,
) -> QuizResponse {
  let questions = get_qualification_questions()

  // Найти вопрос и получить score
  let score = case list.find(questions, fn(q) { q.id == question_id }) {
    Error(_) -> 0
    Ok(question) -> {
      case list.find(question.options, fn(o) { o.value == answer_value }) {
        Error(_) -> 0
        Ok(option) -> option.score
      }
    }
  }

  let new_answer = QuizAnswer(
    question_id: question_id,
    answer: answer_value,
    score: score,
  )

  QuizResponse(..quiz, responses: list.append(quiz.responses, [new_answer]))
}

/// Завершить квиз и получить рекомендацию
pub fn complete_quiz(quiz: QuizResponse) -> QuizResponse {
  let score = calculate_score(quiz.responses)
  let product_code = recommend_product(score)
  let product_id = get_product_id_by_code(product_code)

  QuizResponse(
    ..quiz,
    score: Some(score),
    recommended_product_id: Some(product_id),
  )
}

/// Проверить, завершён ли квиз
pub fn is_complete(quiz: QuizResponse) -> Bool {
  let questions = get_qualification_questions()
  list.length(quiz.responses) >= list.length(questions)
}

/// Получить следующий вопрос
pub fn get_next_question(quiz: QuizResponse) -> Option(QuizQuestion) {
  let questions = get_qualification_questions()
  let answered_ids = list.map(quiz.responses, fn(a) { a.question_id })

  list.find(questions, fn(q) {
    !list.contains(answered_ids, q.id)
  })
  |> option.from_result()
}

// =============================================================================
// JSON Serialization
// =============================================================================

/// Сериализовать вопрос в JSON
pub fn question_to_json(q: QuizQuestion, lang: String) -> json.Json {
  json.object([
    #("id", json.string(q.id)),
    #("question", json.string(case lang {
      "en" -> q.question_en
      _ -> q.question_ru
    })),
    #("options", json.array(q.options, fn(opt) {
      json.object([
        #("value", json.string(opt.value)),
        #("label", json.string(case lang {
          "en" -> opt.label_en
          _ -> opt.label_ru
        })),
      ])
    })),
  ])
}

/// Сериализовать все вопросы
pub fn questions_to_json(lang: String) -> json.Json {
  json.array(get_qualification_questions(), fn(q) { question_to_json(q, lang) })
}

/// Сериализовать результат квиза
pub fn quiz_result_to_json(quiz: QuizResponse, lang: String) -> json.Json {
  let product_code = case quiz.recommended_product_id {
    Some(1) -> "junior"
    Some(2) -> "middle"
    Some(3) -> "senior"
    _ -> "unknown"
  }

  let score = case quiz.score {
    Some(s) -> s
    None -> 0
  }

  json.object([
    #("completed", json.bool(quiz.score != None)),
    #("score", json.int(score)),
    #("recommended_product", json.string(product_code)),
    #("recommendation", json.string(
      get_recommendation_description(product_code, score, lang)
    )),
    #("answers_count", json.int(list.length(quiz.responses))),
  ])
}
