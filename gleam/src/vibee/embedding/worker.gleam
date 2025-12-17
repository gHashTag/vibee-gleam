// Embedding Worker for Telegram RAG
// Batch embedding generation with Ollama (dev) and OpenAI (prod) support

import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/db/postgres.{type DbPool, type TelegramMessage}
import vibee/mcp/config

// =============================================================================
// Types
// =============================================================================

/// Embedding provider configuration
pub type EmbeddingConfig {
  EmbeddingConfig(
    provider: EmbeddingProvider,
    model: String,
    dimensions: Int,
    batch_size: Int,
    rate_limit_per_minute: Int,
  )
}

/// Embedding provider enum
pub type EmbeddingProvider {
  ProviderOllama
  ProviderOpenAI
}

/// Embedding result for a single text
pub type EmbeddingResult {
  EmbeddingResult(
    message_id: Int,
    embedding: List(Float),
    model: String,
    tokens_used: Int,
  )
}

/// Batch embedding result
pub type BatchEmbeddingResult {
  BatchEmbeddingResult(
    total: Int,
    embedded: Int,
    failed: Int,
    tokens_used: Int,
  )
}

/// Embedding error
pub type EmbeddingError {
  EmbeddingApiError(String)
  EmbeddingDbError(String)
  EmbeddingRateLimited
  EmbeddingInvalidResponse(String)
}

// =============================================================================
// Configuration
// =============================================================================

/// Default Ollama configuration (for development)
pub fn ollama_config() -> EmbeddingConfig {
  EmbeddingConfig(
    provider: ProviderOllama,
    model: "nomic-embed-text",
    dimensions: 768,
    batch_size: 100,
    rate_limit_per_minute: 1000,
  )
}

/// OpenAI configuration (for production)
pub fn openai_config() -> EmbeddingConfig {
  EmbeddingConfig(
    provider: ProviderOpenAI,
    model: "text-embedding-3-small",
    dimensions: 1536,
    batch_size: 100,
    rate_limit_per_minute: 3000,
  )
}

/// Get config from environment
pub fn auto_config() -> EmbeddingConfig {
  // Check if OpenAI API key is available
  case config.get_env("OPENAI_API_KEY") {
    "" -> ollama_config()
    _ -> openai_config()
  }
}

// =============================================================================
// Single Text Embedding
// =============================================================================

/// Generate embedding for a single text
pub fn embed_text(
  text: String,
  cfg: EmbeddingConfig,
) -> Result(List(Float), EmbeddingError) {
  case cfg.provider {
    ProviderOllama -> embed_ollama(text, cfg.model)
    ProviderOpenAI -> embed_openai(text, cfg.model)
  }
}

/// Generate embedding via Ollama
fn embed_ollama(text: String, model: String) -> Result(List(Float), EmbeddingError) {
  let ollama_url =
    config.get_env_or("OLLAMA_HOST", "http://localhost:11434")
    <> "/api/embeddings"

  let body =
    json.object([
      #("model", json.string(model)),
      #("prompt", json.string(text)),
    ])
    |> json.to_string

  case http_post(ollama_url, body, []) {
    Error(e) -> Error(EmbeddingApiError("Ollama request failed: " <> e))
    Ok(response) -> parse_ollama_response(response)
  }
}

/// Parse Ollama embedding response
fn parse_ollama_response(response: String) -> Result(List(Float), EmbeddingError) {
  let embedding_decoder = {
    use embedding <- decode.field("embedding", decode.list(decode.float))
    decode.success(embedding)
  }

  case json.parse(response, embedding_decoder) {
    Ok(embedding) -> Ok(embedding)
    Error(_) -> {
      // Try to parse error
      let error_decoder = {
        use err <- decode.field("error", decode.string)
        decode.success(err)
      }
      case json.parse(response, error_decoder) {
        Ok(err) -> Error(EmbeddingApiError("Ollama error: " <> err))
        Error(_) -> Error(EmbeddingInvalidResponse(response))
      }
    }
  }
}

/// Generate embedding via OpenAI
fn embed_openai(text: String, model: String) -> Result(List(Float), EmbeddingError) {
  let api_key = config.get_env("OPENAI_API_KEY")
  case api_key {
    "" -> Error(EmbeddingApiError("No OPENAI_API_KEY set"))
    key -> {
      let body =
        json.object([
          #("model", json.string(model)),
          #("input", json.string(text)),
        ])
        |> json.to_string

      let headers = [
        #("Authorization", "Bearer " <> key),
        #("Content-Type", "application/json"),
      ]

      case http_post("https://api.openai.com/v1/embeddings", body, headers) {
        Error(e) -> Error(EmbeddingApiError("OpenAI request failed: " <> e))
        Ok(response) -> parse_openai_response(response)
      }
    }
  }
}

/// Parse OpenAI embedding response
fn parse_openai_response(response: String) -> Result(List(Float), EmbeddingError) {
  // Decoder for single embedding item
  let embedding_item_decoder = {
    use embedding <- decode.field("embedding", decode.list(decode.float))
    decode.success(embedding)
  }

  // Decoder for the data array
  let data_decoder = {
    use data <- decode.field("data", decode.list(embedding_item_decoder))
    decode.success(data)
  }

  case json.parse(response, data_decoder) {
    Ok([embedding, ..]) -> Ok(embedding)
    Ok([]) -> Error(EmbeddingInvalidResponse("Empty data array"))
    Error(_) -> {
      // Try to parse error
      let error_msg_decoder = {
        use message <- decode.field("message", decode.string)
        decode.success(message)
      }
      let error_decoder = {
        use err <- decode.field("error", error_msg_decoder)
        decode.success(err)
      }
      case json.parse(response, error_decoder) {
        Ok(err) -> Error(EmbeddingApiError("OpenAI error: " <> err))
        Error(_) -> Error(EmbeddingInvalidResponse(response))
      }
    }
  }
}

// =============================================================================
// Batch Embedding
// =============================================================================

/// Generate embeddings for multiple texts (OpenAI supports batch)
pub fn embed_texts_batch(
  texts: List(#(Int, String)),
  cfg: EmbeddingConfig,
) -> Result(List(EmbeddingResult), EmbeddingError) {
  case cfg.provider {
    ProviderOpenAI -> embed_openai_batch(texts, cfg.model)
    ProviderOllama -> embed_ollama_sequential(texts, cfg.model)
  }
}

/// OpenAI batch embedding (single API call)
fn embed_openai_batch(
  texts: List(#(Int, String)),
  model: String,
) -> Result(List(EmbeddingResult), EmbeddingError) {
  let api_key = config.get_env("OPENAI_API_KEY")
  case api_key {
    "" -> Error(EmbeddingApiError("No OPENAI_API_KEY set"))
    key -> {
      let input_texts =
        texts
        |> list.map(fn(t) { t.1 })

      let body =
        json.object([
          #("model", json.string(model)),
          #("input", json.array(input_texts, json.string)),
        ])
        |> json.to_string

      let headers = [
        #("Authorization", "Bearer " <> key),
        #("Content-Type", "application/json"),
      ]

      case http_post("https://api.openai.com/v1/embeddings", body, headers) {
        Error(e) -> Error(EmbeddingApiError("OpenAI batch request failed: " <> e))
        Ok(response) -> parse_openai_batch_response(response, texts, model)
      }
    }
  }
}

/// Parse OpenAI batch embedding response
fn parse_openai_batch_response(
  response: String,
  texts: List(#(Int, String)),
  model: String,
) -> Result(List(EmbeddingResult), EmbeddingError) {
  // Decoder for embedding item with index
  let embedding_item_decoder = {
    use idx <- decode.field("index", decode.int)
    use emb <- decode.field("embedding", decode.list(decode.float))
    decode.success(#(idx, emb))
  }

  // Decoder for usage info
  let usage_decoder = {
    use total_tokens <- decode.field("total_tokens", decode.int)
    decode.success(total_tokens)
  }

  // Combined decoder
  let batch_decoder = {
    use data <- decode.field("data", decode.list(embedding_item_decoder))
    use tokens <- decode.field("usage", usage_decoder)
    decode.success(#(data, tokens))
  }

  case json.parse(response, batch_decoder) {
    Ok(#(embeddings, tokens)) -> {
      // Sort by index and match with original texts
      let sorted =
        embeddings
        |> list.sort(fn(a, b) { int.compare(a.0, b.0) })

      let results =
        list.zip(texts, sorted)
        |> list.map(fn(pair) {
          let #(#(msg_id, _), #(_, embedding)) = pair
          EmbeddingResult(
            message_id: msg_id,
            embedding: embedding,
            model: model,
            tokens_used: tokens / list.length(texts),
          )
        })

      Ok(results)
    }
    Error(_) -> Error(EmbeddingInvalidResponse("Failed to parse batch response"))
  }
}

/// Ollama sequential embedding (one at a time)
fn embed_ollama_sequential(
  texts: List(#(Int, String)),
  model: String,
) -> Result(List(EmbeddingResult), EmbeddingError) {
  let results =
    texts
    |> list.map(fn(pair) {
      let #(msg_id, text) = pair
      case embed_ollama(text, model) {
        Ok(embedding) ->
          Ok(EmbeddingResult(
            message_id: msg_id,
            embedding: embedding,
            model: model,
            tokens_used: 0,
          ))
        Error(e) -> Error(e)
      }
    })

  // Collect successful results
  let successes =
    results
    |> list.filter_map(fn(r) {
      case r {
        Ok(er) -> Ok(er)
        Error(_) -> Error(Nil)
      }
    })

  // Check if all succeeded
  case list.length(successes) == list.length(texts) {
    True -> Ok(successes)
    False -> {
      // Return partial results or first error
      case list.length(successes) > 0 {
        True -> Ok(successes)
        False -> {
          let first_error =
            results
            |> list.find(fn(r) {
              case r {
                Error(_) -> True
                Ok(_) -> False
              }
            })
          case first_error {
            Ok(Error(e)) -> Error(e)
            _ -> Error(EmbeddingApiError("Unknown error"))
          }
        }
      }
    }
  }
}

// =============================================================================
// Database Integration
// =============================================================================

/// Process messages without embeddings
pub fn process_pending_embeddings(
  pool: DbPool,
  cfg: EmbeddingConfig,
) -> Result(BatchEmbeddingResult, EmbeddingError) {
  // Get messages without embeddings
  case postgres.get_messages_without_embedding(pool, cfg.batch_size) {
    Error(e) -> Error(EmbeddingDbError(db_error_to_string(e)))
    Ok([]) ->
      Ok(BatchEmbeddingResult(total: 0, embedded: 0, failed: 0, tokens_used: 0))
    Ok(messages) -> {
      let total = list.length(messages)

      // Prepare texts for embedding
      let texts =
        messages
        |> list.filter_map(fn(msg) {
          case msg.text_content {
            Some(text) -> {
              case string.length(text) > 10 {
                True -> Ok(#(msg.id, text))
                False -> Error(Nil)
              }
            }
            _ -> Error(Nil)
          }
        })

      // Generate embeddings
      case embed_texts_batch(texts, cfg) {
        Error(e) -> Error(e)
        Ok(results) -> {
          // Save embeddings to database
          let save_results =
            results
            |> list.map(fn(er) {
              postgres.update_message_embedding(
                pool,
                er.message_id,
                er.embedding,
                er.model,
              )
            })

          let embedded =
            save_results
            |> list.filter(fn(r) {
              case r {
                Ok(_) -> True
                Error(_) -> False
              }
            })
            |> list.length

          let failed = total - embedded

          let tokens_used =
            results
            |> list.map(fn(er) { er.tokens_used })
            |> list.fold(0, fn(acc, t) { acc + t })

          Ok(BatchEmbeddingResult(
            total: total,
            embedded: embedded,
            failed: failed,
            tokens_used: tokens_used,
          ))
        }
      }
    }
  }
}

/// Get embedding statistics
pub fn get_embedding_stats(pool: DbPool) -> Result(EmbeddingStats, EmbeddingError) {
  let stats = postgres.get_parse_stats(pool)
  Ok(EmbeddingStats(
    total_messages: stats.total_messages,
    messages_with_embedding: stats.messages_with_embedding,
    pending: stats.total_messages - stats.messages_with_embedding,
    percentage: case stats.total_messages {
      0 -> 0.0
      n ->
        int.to_float(stats.messages_with_embedding)
        /. int.to_float(n)
        *. 100.0
    },
  ))
}

/// Embedding statistics
pub type EmbeddingStats {
  EmbeddingStats(
    total_messages: Int,
    messages_with_embedding: Int,
    pending: Int,
    percentage: Float,
  )
}

// =============================================================================
// Helper Functions
// =============================================================================

fn db_error_to_string(e: postgres.DbError) -> String {
  case e {
    postgres.DbConnectionError(msg) -> "Connection error: " <> msg
    postgres.DbQueryError(msg) -> "Query error: " <> msg
    postgres.DbNotFound -> "Not found"
  }
}

// =============================================================================
// FFI Functions
// =============================================================================

/// HTTP POST request with headers
@external(erlang, "vibee_http_ffi", "post")
fn http_post(
  url: String,
  body: String,
  headers: List(#(String, String)),
) -> Result(String, String)

// =============================================================================
// Result Serialization
// =============================================================================

/// Convert batch result to JSON
pub fn batch_result_to_json(result: BatchEmbeddingResult) -> String {
  json.object([
    #("total", json.int(result.total)),
    #("embedded", json.int(result.embedded)),
    #("failed", json.int(result.failed)),
    #("tokens_used", json.int(result.tokens_used)),
  ])
  |> json.to_string
}

/// Convert stats to JSON
pub fn stats_to_json(stats: EmbeddingStats) -> String {
  json.object([
    #("total_messages", json.int(stats.total_messages)),
    #("messages_with_embedding", json.int(stats.messages_with_embedding)),
    #("pending", json.int(stats.pending)),
    #("percentage", json.float(stats.percentage)),
  ])
  |> json.to_string
}
