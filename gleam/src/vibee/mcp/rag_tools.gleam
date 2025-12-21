// RAG Tools - MCP инструменты для Telegram RAG
// Парсинг, медиа обработка, эмбеддинги и поиск

import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import shellout
import vibee/db/postgres
import vibee/embedding/worker
import vibee/llm/gemini_client
import vibee/media/media_processor
import vibee/mcp/config
import vibee/mcp/protocol
import vibee/mcp/session_manager
import vibee/mcp/types.{type Tool, type ToolResult, TextContent, Tool}
import vibee/mcp/validation
import vibee/search/hybrid
import vibee/telegram/parser
import vibee/github/db as github_db
import vibee/github/loader as github_loader

// =============================================================================
// Tool Definitions
// =============================================================================

/// Tool: telegram_parse_all_dialogs
pub fn telegram_parse_all_dialogs_tool() -> Tool {
  Tool(
    name: "telegram_parse_all_dialogs",
    description: "Запустить парсинг всех Telegram диалогов и сохранить в базу данных. Поддерживает batch processing и rate limiting.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Telegram session ID. Если не указан, используется активная сессия.")),
            ]),
          ),
          #(
            "batch_size",
            json.object([
              #("type", json.string("integer")),
              #(
                "description",
                json.string(
                  "Количество сообщений за один запрос (default: 100)",
                ),
              ),
              #("default", json.int(100)),
            ]),
          ),
          #(
            "delay_ms",
            json.object([
              #("type", json.string("integer")),
              #(
                "description",
                json.string("Задержка между запросами в мс (default: 250)"),
              ),
              #("default", json.int(250)),
            ]),
          ),
        ]),
      ),
      #("required", json.array([], json.string)),
    ]),
  )
}

/// Tool: telegram_parse_chat
pub fn telegram_parse_chat_tool() -> Tool {
  Tool(
    name: "telegram_parse_chat",
    description: "Парсить конкретный чат/диалог и сохранить сообщения в базу данных.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "session_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Telegram session ID. Если не указан, используется активная сессия.")),
            ]),
          ),
          #(
            "chat_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("ID чата для парсинга")),
            ]),
          ),
          #(
            "from_message_id",
            json.object([
              #("type", json.string("integer")),
              #(
                "description",
                json.string("Начать с этого message ID (для продолжения)"),
              ),
            ]),
          ),
          #(
            "max_messages",
            json.object([
              #("type", json.string("integer")),
              #(
                "description",
                json.string(
                  "Максимальное количество сообщений (0 = без лимита)",
                ),
              ),
              #("default", json.int(0)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["chat_id"], json.string)),
    ]),
  )
}

/// Tool: telegram_get_parse_status
pub fn telegram_get_parse_status_tool() -> Tool {
  Tool(
    name: "telegram_get_parse_status",
    description: "Получить статус парсинга: количество диалогов, сообщений, медиа и pending задач.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
    ]),
  )
}

/// Tool: telegram_process_media
pub fn telegram_process_media_tool() -> Tool {
  Tool(
    name: "telegram_process_media",
    description: "Обработать pending медиа файлы через Gemini 2.5 Pro: транскрибация голосовых, анализ фото и видео.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "media_type",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("Тип медиа для обработки: voice, photo, all"),
              ),
              #("enum", json.array(["voice", "photo", "all"], json.string)),
              #("default", json.string("all")),
            ]),
          ),
          #(
            "batch_size",
            json.object([
              #("type", json.string("integer")),
              #(
                "description",
                json.string("Количество медиа за раз (default: 10)"),
              ),
              #("default", json.int(10)),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

/// Tool: telegram_generate_embeddings
pub fn telegram_generate_embeddings_tool() -> Tool {
  Tool(
    name: "telegram_generate_embeddings",
    description: "Сгенерировать embeddings для сообщений без них. Использует Ollama (dev) или OpenAI (prod).",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "provider",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Провайдер: ollama или openai")),
              #("enum", json.array(["ollama", "openai", "auto"], json.string)),
              #("default", json.string("auto")),
            ]),
          ),
          #(
            "batch_size",
            json.object([
              #("type", json.string("integer")),
              #(
                "description",
                json.string("Количество сообщений за раз (default: 100)"),
              ),
              #("default", json.int(100)),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

/// Tool: telegram_search_history
pub fn telegram_search_history_tool() -> Tool {
  Tool(
    name: "telegram_search_history",
    description: "Гибридный поиск по истории сообщений: vector + keyword с RRF (Reciprocal Rank Fusion).",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "query",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Поисковый запрос")),
            ]),
          ),
          #(
            "mode",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("Режим поиска: hybrid, vector, keyword"),
              ),
              #(
                "enum",
                json.array(["hybrid", "vector", "keyword"], json.string),
              ),
              #("default", json.string("hybrid")),
            ]),
          ),
          #(
            "dialog_ids",
            json.object([
              #("type", json.string("array")),
              #(
                "description",
                json.string("Фильтр по ID диалогов (опционально)"),
              ),
              #("items", json.object([#("type", json.string("integer"))])),
            ]),
          ),
          #(
            "limit",
            json.object([
              #("type", json.string("integer")),
              #(
                "description",
                json.string("Максимальное количество результатов (default: 20)"),
              ),
              #("default", json.int(20)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["query"], json.string)),
    ]),
  )
}

/// Tool: telegram_transcribe_voice
pub fn telegram_transcribe_voice_tool() -> Tool {
  Tool(
    name: "telegram_transcribe_voice",
    description: "Транскрибировать голосовое сообщение через Gemini 2.5 Pro.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "media_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("ID медиа записи в базе")),
            ]),
          ),
          #(
            "file_path",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("Путь к аудио файлу (альтернатива media_id)"),
              ),
            ]),
          ),
          #(
            "language",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Язык аудио (default: ru)")),
              #("default", json.string("ru")),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

/// Tool: telegram_analyze_image
pub fn telegram_analyze_image_tool() -> Tool {
  Tool(
    name: "telegram_analyze_image",
    description: "Анализировать изображение через Gemini Vision: описание, OCR, объекты, эмоции.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "media_id",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("ID медиа записи в базе")),
            ]),
          ),
          #(
            "file_path",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("Путь к изображению (альтернатива media_id)"),
              ),
            ]),
          ),
          #(
            "prompt",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Кастомный промпт для анализа")),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

// =============================================================================
// Get all RAG tools
// =============================================================================

/// Tool: conversation_get_context (Main API for AI Digital Clone)
pub fn conversation_get_context_tool() -> Tool {
  Tool(
    name: "conversation_get_context",
    description: "Получить контекст разговора для AI. Возвращает последние сообщения, семантически релевантные сообщения (если есть query), и метаданные диалога. Главный инструмент для цифрового клона.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "chat_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("ID диалога/чата")),
            ]),
          ),
          #(
            "query",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string(
                  "Опциональный поисковый запрос для семантического поиска релевантных сообщений",
                ),
              ),
            ]),
          ),
          #(
            "recent_messages",
            json.object([
              #("type", json.string("integer")),
              #(
                "description",
                json.string("Количество последних сообщений (default: 10)"),
              ),
              #("default", json.int(10)),
            ]),
          ),
          #(
            "semantic_results",
            json.object([
              #("type", json.string("integer")),
              #(
                "description",
                json.string(
                  "Количество семантически релевантных сообщений (default: 5)",
                ),
              ),
              #("default", json.int(5)),
            ]),
          ),
          #(
            "owner_id",
            json.object([
              #("type", json.string("integer")),
              #(
                "description",
                json.string(
                  "ID владельца аккаунта (для определения исходящих сообщений)",
                ),
              ),
            ]),
          ),
          #(
            "include_github",
            json.object([
              #("type", json.string("boolean")),
              #(
                "description",
                json.string(
                  "Включить контекст из GitHub профиля (default: true)",
                ),
              ),
              #("default", json.bool(True)),
            ]),
          ),
          #(
            "github_username",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string(
                  "GitHub username для поиска контекста (default: gHashTag)",
                ),
              ),
              #("default", json.string("gHashTag")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["chat_id"], json.string)),
    ]),
  )
}

pub fn get_all_rag_tools() -> List(Tool) {
  [
    telegram_parse_all_dialogs_tool(),
    telegram_parse_chat_tool(),
    telegram_get_parse_status_tool(),
    telegram_process_media_tool(),
    telegram_generate_embeddings_tool(),
    telegram_search_history_tool(),
    telegram_transcribe_voice_tool(),
    telegram_analyze_image_tool(),
    conversation_get_context_tool(),
  ]
}

// =============================================================================
// Tool Handlers
// =============================================================================

/// Handle telegram_parse_all_dialogs
pub fn handle_telegram_parse_all_dialogs(args: json.Json) -> ToolResult {
  // Parse arguments
  let session_id_opt =
    json_get_string(args, "session_id")
    |> option.from_result()
  let batch_size =
    json_get_int(args, "batch_size")
    |> result.unwrap(100)
  let delay_ms =
    json_get_int(args, "delay_ms")
    |> result.unwrap(250)

  // Resolve session_id using session_manager
  case validation.validate_session_id(session_id_opt) {
    Error(err) -> protocol.error_result(validation.error_to_string(err))
    Ok(sid) -> {
      // Get database URL from environment
      let db_url = config.get_env_or("DATABASE_URL", "")
      case db_url {
        "" ->
          protocol.error_result(
            "DATABASE_URL not set. Please configure Neon PostgreSQL connection.",
          )
        url -> {
          case postgres.connect(url) {
            Error(e) ->
              protocol.error_result(
                "Database connection failed: " <> db_error_to_string(e),
              )
            Ok(pool) -> {
              // Create parser config
              let cfg =
                parser.ParserConfig(
                  batch_size: batch_size,
                  delay_ms: delay_ms,
                  max_messages_per_dialog: 0,
                  include_media: True,
                )

              // Create parse job
              case
                postgres.create_parse_job_with_config(
                  pool,
                  "full_sync",
                  None,
                  parser.config_to_json(cfg),
                )
              {
                Error(e) -> {
                  postgres.disconnect(pool)
                  protocol.error_result(
                    "Failed to create job: " <> db_error_to_string(e),
                  )
                }
                Ok(job_id) -> {
                  // Start parsing (this would normally be async)
                  case parser.parse_all_dialogs(pool, sid, cfg, job_id) {
                    Error(e) -> {
                      postgres.disconnect(pool)
                      protocol.error_result(
                        "Parsing failed: " <> parser_error_to_string(e),
                      )
                    }
                    Ok(result) -> {
                      let _ =
                        postgres.complete_job(
                          pool,
                          job_id,
                          parser.result_to_json(result),
                        )
                      postgres.disconnect(pool)
                      protocol.text_result(parser.result_to_json(result))
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

/// Handle telegram_parse_chat
pub fn handle_telegram_parse_chat(args: json.Json) -> ToolResult {
  let session_id_opt =
    json_get_string(args, "session_id")
    |> option.from_result()
  let chat_id_str =
    json_get_string(args, "chat_id")
    |> result.unwrap("")
  let max_messages =
    json_get_int(args, "max_messages")
    |> result.unwrap(0)

  // Resolve session_id using session_manager
  case validation.validate_session_id(session_id_opt), chat_id_str {
    Error(err), _ -> protocol.error_result(validation.error_to_string(err))
    _, "" -> protocol.error_result("chat_id is required")
    Ok(sid), cid -> {
      case int.parse(cid) {
        Error(_) -> protocol.error_result("chat_id must be a valid integer")
        Ok(chat_id) -> {
          let db_url = config.get_env_or("DATABASE_URL", "")
          case db_url {
            "" -> protocol.error_result("DATABASE_URL not set")
            url -> {
              case postgres.connect(url) {
                Error(e) ->
                  protocol.error_result("DB error: " <> db_error_to_string(e))
                Ok(pool) -> {
                  let cfg =
                    parser.ParserConfig(
                      batch_size: 100,
                      delay_ms: 250,
                      max_messages_per_dialog: max_messages,
                      include_media: True,
                    )

                  case parser.parse_single_dialog(pool, sid, chat_id, cfg) {
                    Error(e) -> {
                      postgres.disconnect(pool)
                      protocol.error_result(
                        "Parsing failed: " <> parser_error_to_string(e),
                      )
                    }
                    Ok(result) -> {
                      postgres.disconnect(pool)
                      protocol.text_result(parser.dialog_result_to_json(result))
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

/// Handle telegram_get_parse_status
pub fn handle_telegram_get_parse_status(_args: json.Json) -> ToolResult {
  let db_url = config.get_env_or("DATABASE_URL", "")
  case db_url {
    "" -> protocol.error_result("DATABASE_URL not set")
    url -> {
      case postgres.connect(url) {
        Error(e) -> protocol.error_result("DB error: " <> db_error_to_string(e))
        Ok(pool) -> {
          let stats = postgres.get_parse_stats(pool)
          postgres.disconnect(pool)
          let result =
            json.object([
              #("total_dialogs", json.int(stats.total_dialogs)),
              #("parsed_dialogs", json.int(stats.parsed_dialogs)),
              #("total_messages", json.int(stats.total_messages)),
              #(
                "messages_with_embedding",
                json.int(stats.messages_with_embedding),
              ),
              #("total_media", json.int(stats.total_media)),
              #("processed_media", json.int(stats.processed_media)),
              #("pending_jobs", json.int(stats.pending_jobs)),
              #("running_jobs", json.int(stats.running_jobs)),
            ])
            |> json.to_string
          protocol.text_result(result)
        }
      }
    }
  }
}

/// Handle telegram_process_media
/// TODO: Implement media processor module
pub fn handle_telegram_process_media(args: json.Json) -> ToolResult {
  let media_type =
    json_get_string(args, "media_type")
    |> result.unwrap("all")
  let batch_size =
    json_get_int(args, "batch_size")
    |> result.unwrap(10)

  // Check if Gemini is configured
  let cfg = gemini_client.default_config()
  case gemini_client.is_configured(cfg) {
    False -> protocol.error_result(
      "GEMINI_API_KEY not set. Please configure Gemini API key to enable media processing."
    )
    True -> {
      // Get active session
      case session_manager.get_active() {
        None -> protocol.error_result("No active Telegram session. Use session_create first.")
        Some(session_id) -> {
          // Process pending media
          case media_processor.process_pending_by_type(media_type, batch_size, session_id) {
            Error(e) -> protocol.error_result(
              "Media processing failed: " <> media_processor.error_to_string(e)
            )
            Ok(result) -> {
              protocol.text_result(media_processor.batch_result_to_json(result))
            }
          }
        }
      }
    }
  }
}

/// Handle telegram_generate_embeddings
pub fn handle_telegram_generate_embeddings(args: json.Json) -> ToolResult {
  let provider =
    json_get_string(args, "provider")
    |> result.unwrap("auto")
  let batch_size =
    json_get_int(args, "batch_size")
    |> result.unwrap(100)

  let db_url = config.get_env_or("DATABASE_URL", "")
  case db_url {
    "" -> protocol.error_result("DATABASE_URL not set")
    url -> {
      case postgres.connect(url) {
        Error(e) -> protocol.error_result("DB error: " <> db_error_to_string(e))
        Ok(pool) -> {
          let cfg = case provider {
            "ollama" -> worker.ollama_config()
            "openai" -> worker.openai_config()
            _ -> worker.auto_config()
          }

          let cfg_with_batch =
            worker.EmbeddingConfig(..cfg, batch_size: batch_size)

          case worker.process_pending_embeddings(pool, cfg_with_batch) {
            Error(e) -> {
              postgres.disconnect(pool)
              protocol.error_result(
                "Embedding failed: " <> embedding_error_to_string(e),
              )
            }
            Ok(result) -> {
              postgres.disconnect(pool)
              protocol.text_result(worker.batch_result_to_json(result))
            }
          }
        }
      }
    }
  }
}

/// Handle telegram_search_history
pub fn handle_telegram_search_history(args: json.Json) -> ToolResult {
  let query =
    json_get_string(args, "query")
    |> result.unwrap("")
  let mode =
    json_get_string(args, "mode")
    |> result.unwrap("hybrid")
  let limit =
    json_get_int(args, "limit")
    |> result.unwrap(20)

  case query {
    "" -> protocol.error_result("query is required")
    q -> {
      let db_url = config.get_env_or("DATABASE_URL", "")
      case db_url {
        "" -> protocol.error_result("DATABASE_URL not set")
        url -> {
          case postgres.connect(url) {
            Error(e) ->
              protocol.error_result("DB error: " <> db_error_to_string(e))
            Ok(pool) -> {
              let embedding_cfg = worker.auto_config()
              let search_cfg =
                hybrid.SearchConfig(
                  ..hybrid.default_config(),
                  mode: case mode {
                    "vector" -> hybrid.ModeVector
                    "keyword" -> hybrid.ModeKeyword
                    _ -> hybrid.ModeHybrid
                  },
                  limit: limit,
                )

              case hybrid.search(pool, q, embedding_cfg, search_cfg) {
                Error(e) -> {
                  postgres.disconnect(pool)
                  protocol.error_result(hybrid.error_to_json(e))
                }
                Ok(response) -> {
                  postgres.disconnect(pool)
                  protocol.text_result(hybrid.response_to_json(response))
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Handle telegram_transcribe_voice
/// TODO: Implement media processor module
pub fn handle_telegram_transcribe_voice(args: json.Json) -> ToolResult {
  let _media_id = json_get_int(args, "media_id")
  let file_path_opt = json_get_string(args, "file_path")
  let language =
    json_get_string(args, "language")
    |> result.unwrap("ru")

  // Check if Gemini is configured
  let cfg = gemini_client.default_config()
  case gemini_client.is_configured(cfg) {
    False -> protocol.error_result(
      "GEMINI_API_KEY not set. Please configure Gemini API key to enable voice transcription."
    )
    True -> {
      case file_path_opt {
        Error(_) -> protocol.error_result(
          "Either media_id or file_path is required for transcription."
        )
        Ok(file_path) -> {
          // Transcribe audio using Gemini
          case gemini_client.analyze_audio(file_path, language, cfg) {
            Error(e) -> protocol.error_result(
              "Transcription failed: " <> gemini_client.error_to_string(e)
            )
            Ok(result) -> {
              protocol.text_result(gemini_client.result_to_json(result))
            }
          }
        }
      }
    }
  }
}

/// Handle telegram_analyze_image
/// Uses Gemini Vision for image analysis
pub fn handle_telegram_analyze_image(args: json.Json) -> ToolResult {
  let _media_id = json_get_int(args, "media_id")
  let file_path_opt = json_get_string(args, "file_path")
  let custom_prompt =
    json_get_string(args, "prompt")
    |> result.unwrap("")

  // Check if Gemini is configured
  let cfg = gemini_client.default_config()
  case gemini_client.is_configured(cfg) {
    False -> protocol.error_result(
      "GEMINI_API_KEY not set. Please configure Gemini API key to enable image analysis."
    )
    True -> {
      case file_path_opt {
        Error(_) -> protocol.error_result(
          "Either media_id or file_path is required for image analysis."
        )
        Ok(file_path) -> {
          // Analyze image using Gemini Vision
          case gemini_client.analyze_image(file_path, custom_prompt, cfg) {
            Error(e) -> protocol.error_result(
              "Image analysis failed: " <> gemini_client.error_to_string(e)
            )
            Ok(result) -> {
              protocol.text_result(gemini_client.result_to_json(result))
            }
          }
        }
      }
    }
  }
}

// =============================================================================
// Get all RAG handlers as dict entries
// =============================================================================

pub fn get_rag_handlers() -> List(#(String, fn(json.Json) -> ToolResult)) {
  [
    #("telegram_parse_all_dialogs", handle_telegram_parse_all_dialogs),
    #("telegram_parse_chat", handle_telegram_parse_chat),
    #("telegram_get_parse_status", handle_telegram_get_parse_status),
    #("telegram_process_media", handle_telegram_process_media),
    #("telegram_generate_embeddings", handle_telegram_generate_embeddings),
    #("telegram_search_history", handle_telegram_search_history),
    #("telegram_transcribe_voice", handle_telegram_transcribe_voice),
    #("telegram_analyze_image", handle_telegram_analyze_image),
    #("conversation_get_context", handle_conversation_get_context),
  ]
}

/// Handle conversation_get_context - Main API for AI Digital Clone
/// Uses shellout/psql for reliable SSL connection to Neon PostgreSQL
/// Now includes GitHub profile context for digital clone knowledge
pub fn handle_conversation_get_context(args: json.Json) -> ToolResult {
  let chat_id_str =
    json_get_string(args, "chat_id")
    |> result.unwrap("")
  let recent_count =
    json_get_int(args, "recent_messages")
    |> result.unwrap(10)
  let owner_id =
    json_get_int(args, "owner_id")
    |> result.unwrap(144_022_504)
  let query_opt =
    json_get_string(args, "query")
    |> option.from_result()
  let include_github =
    json_get_bool(args, "include_github")
    |> result.unwrap(True)
  let github_username =
    json_get_string(args, "github_username")
    |> result.unwrap("gHashTag")

  case chat_id_str {
    "" -> protocol.error_result("chat_id is required")
    _ -> {
      let db_url = config.get_env_or("DATABASE_URL", "")
      case db_url {
        "" -> protocol.error_result("DATABASE_URL not set")
        url -> {
          // Query recent messages via psql
          let recent_sql =
            "SELECT json_agg(row_to_json(t)) FROM (SELECT message_id, sender_name, LEFT(text_content, 500) as text_content, timestamp::text, CASE WHEN sender_id = "
            <> int.to_string(owner_id)
            <> " THEN true ELSE false END as is_outgoing FROM telegram_messages WHERE dialog_id = "
            <> chat_id_str
            <> " ORDER BY timestamp DESC LIMIT "
            <> int.to_string(recent_count)
            <> ") t"

          let recent_result =
            shellout.command(
              run: "psql",
              with: [url, "-t", "-c", recent_sql],
              in: ".",
              opt: [],
            )

          // Query metadata via psql
          let metadata_sql =
            "SELECT json_build_object('dialog_id', d.id, 'dialog_title', d.title, 'dialog_type', d.type, 'total_messages', COUNT(m.id), 'messages_with_embedding', COUNT(m.embedding), 'first_message_date', MIN(m.timestamp)::date::text, 'last_message_date', MAX(m.timestamp)::date::text) FROM telegram_dialogs d LEFT JOIN telegram_messages m ON m.dialog_id = d.id WHERE d.id = "
            <> chat_id_str
            <> " GROUP BY d.id, d.title, d.type"

          let metadata_result =
            shellout.command(
              run: "psql",
              with: [url, "-t", "-c", metadata_sql],
              in: ".",
              opt: [],
            )

          // Get GitHub context if enabled and there's a query
          let github_context = case include_github, query_opt {
            True, Some(query) -> get_github_context(url, github_username, query)
            True, None -> get_github_profile_summary(url, github_username)
            False, _ -> json.null()
          }

          case recent_result, metadata_result {
            Ok(recent_json), Ok(metadata_json) -> {
              // Parse and combine results
              let recent_trimmed = string.trim(recent_json)
              let metadata_trimmed = string.trim(metadata_json)

              let response =
                json.object([
                  #(
                    "recent",
                    case json.parse(recent_trimmed, decode.dynamic) {
                      Ok(_) -> json.string(recent_trimmed)
                      Error(_) -> json.array([], fn(x) { x })
                    },
                  ),
                  #("relevant", json.array([], fn(x) { x })),
                  #(
                    "metadata",
                    case json.parse(metadata_trimmed, decode.dynamic) {
                      Ok(_) -> json.string(metadata_trimmed)
                      Error(_) -> json.null()
                    },
                  ),
                  #("github_context", github_context),
                ])
                |> json.to_string

              protocol.text_result(response)
            }
            Error(#(_, err)), _ ->
              protocol.error_result("Failed to get recent messages: " <> err)
            _, Error(#(_, err)) ->
              protocol.error_result("Failed to get metadata: " <> err)
          }
        }
      }
    }
  }
}

/// Get GitHub context by searching documents
fn get_github_context(db_url: String, username: String, query: String) -> json.Json {
  case postgres.connect(db_url) {
    Error(_) -> json.null()
    Ok(pool) -> {
      case github_db.keyword_search(pool, query, Some(username), 5) {
        Error(_) -> {
          postgres.disconnect(pool)
          json.null()
        }
        Ok(results) -> {
          postgres.disconnect(pool)
          case results {
            [] -> json.null()
            _ -> {
              let formatted = github_loader.format_search_results_for_context(results)
              json.string(formatted)
            }
          }
        }
      }
    }
  }
}

/// Get GitHub profile summary (when no query)
fn get_github_profile_summary(db_url: String, username: String) -> json.Json {
  case postgres.connect(db_url) {
    Error(_) -> json.null()
    Ok(pool) -> {
      case github_db.get_stats(pool, username) {
        Error(_) -> {
          postgres.disconnect(pool)
          json.null()
        }
        Ok(#(total, with_embeddings, total_stars)) -> {
          postgres.disconnect(pool)
          json.object([
            #("username", json.string(username)),
            #("total_documents", json.int(total)),
            #("documents_with_embeddings", json.int(with_embeddings)),
            #("total_stars", json.int(total_stars)),
          ])
        }
      }
    }
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

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

fn json_get_bool(j: json.Json, key: String) -> Result(Bool, Nil) {
  let decoder = {
    use v <- decode.field(key, decode.bool)
    decode.success(v)
  }
  let json_str = json.to_string(j)
  case json.parse(json_str, decoder) {
    Ok(v) -> Ok(v)
    Error(_) -> Error(Nil)
  }
}

fn db_error_to_string(e: postgres.DbError) -> String {
  case e {
    postgres.DbConnectionError(msg) -> "Connection error: " <> msg
    postgres.DbQueryError(msg) -> "Query error: " <> msg
    postgres.DbNotFound -> "Not found"
  }
}

fn parser_error_to_string(e: parser.ParserError) -> String {
  case e {
    parser.ParserConnectionError(msg) -> "Connection error: " <> msg
    parser.ParserApiError(msg) -> "API error: " <> msg
    parser.ParserDbError(msg) -> "DB error: " <> msg
    parser.ParserRateLimited -> "Rate limited"
    parser.ParserCancelled -> "Cancelled"
  }
}

fn embedding_error_to_string(e: worker.EmbeddingError) -> String {
  case e {
    worker.EmbeddingApiError(msg) -> "API error: " <> msg
    worker.EmbeddingDbError(msg) -> "DB error: " <> msg
    worker.EmbeddingRateLimited -> "Rate limited"
    worker.EmbeddingInvalidResponse(msg) -> "Invalid response: " <> msg
  }
}
