// Hybrid Search for Telegram RAG
// Combines vector similarity search with keyword search using RRF (Reciprocal Rank Fusion)

import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/db/postgres.{type DbPool, type SearchResult, SearchResult}
import vibee/embedding/worker.{type EmbeddingConfig}

// =============================================================================
// Types
// =============================================================================

/// Search configuration
pub type SearchConfig {
  SearchConfig(
    mode: SearchMode,
    limit: Int,
    rrf_k: Int,
    vector_weight: Float,
    keyword_weight: Float,
    min_score: Float,
    dialog_ids: Option(List(Int)),
    date_from: Option(String),
    date_to: Option(String),
    content_types: Option(List(String)),
  )
}

/// Search mode enum
pub type SearchMode {
  ModeHybrid
  ModeVector
  ModeKeyword
}

/// Enhanced search result with additional context
pub type EnhancedSearchResult {
  EnhancedSearchResult(
    message_id: Int,
    dialog_id: Int,
    dialog_title: Option(String),
    text_content: String,
    timestamp: String,
    sender_name: Option(String),
    content_type: String,
    score: Float,
    vector_score: Option(Float),
    keyword_score: Option(Float),
    highlights: List(String),
  )
}

/// Search response
pub type SearchResponse {
  SearchResponse(
    query: String,
    mode: String,
    total_results: Int,
    results: List(EnhancedSearchResult),
    took_ms: Int,
  )
}

/// Search error
pub type SearchError {
  SearchEmbeddingError(String)
  SearchDbError(String)
  SearchInvalidQuery(String)
}

// =============================================================================
// Configuration
// =============================================================================

/// Default search configuration
pub fn default_config() -> SearchConfig {
  SearchConfig(
    mode: ModeHybrid,
    limit: 20,
    rrf_k: 60,
    vector_weight: 0.7,
    keyword_weight: 0.3,
    min_score: 0.0,
    dialog_ids: None,
    date_from: None,
    date_to: None,
    content_types: None,
  )
}

/// Create config from JSON
pub fn config_from_json(json_str: String) -> SearchConfig {
  case json.parse(json_str, decode_config()) {
    Ok(cfg) -> cfg
    Error(_) -> default_config()
  }
}

fn decode_config() -> Decoder(SearchConfig) {
  use mode <- decode.field(
    "mode",
    decode.optional(decode.string),
  )
  use limit <- decode.field(
    "limit",
    decode.optional(decode.int),
  )
  use rrf_k <- decode.field(
    "rrf_k",
    decode.optional(decode.int),
  )
  use vector_weight <- decode.field(
    "vector_weight",
    decode.optional(decode.float),
  )
  use keyword_weight <- decode.field(
    "keyword_weight",
    decode.optional(decode.float),
  )
  use min_score <- decode.field(
    "min_score",
    decode.optional(decode.float),
  )
  use dialog_ids <- decode.field(
    "dialog_ids",
    decode.optional(decode.list(decode.int)),
  )
  use date_from <- decode.field(
    "date_from",
    decode.optional(decode.string),
  )
  use date_to <- decode.field(
    "date_to",
    decode.optional(decode.string),
  )
  use content_types <- decode.field(
    "content_types",
    decode.optional(decode.list(decode.string)),
  )
  decode.success(SearchConfig(
    mode: parse_mode(option.unwrap(mode, "hybrid")),
    limit: option.unwrap(limit, 20),
    rrf_k: option.unwrap(rrf_k, 60),
    vector_weight: option.unwrap(vector_weight, 0.7),
    keyword_weight: option.unwrap(keyword_weight, 0.3),
    min_score: option.unwrap(min_score, 0.0),
    dialog_ids: dialog_ids,
    date_from: date_from,
    date_to: date_to,
    content_types: content_types,
  ))
}

fn parse_mode(s: String) -> SearchMode {
  case string.lowercase(s) {
    "vector" | "semantic" -> ModeVector
    "keyword" | "fulltext" | "fts" -> ModeKeyword
    _ -> ModeHybrid
  }
}

fn mode_to_string(mode: SearchMode) -> String {
  case mode {
    ModeHybrid -> "hybrid"
    ModeVector -> "vector"
    ModeKeyword -> "keyword"
  }
}

// =============================================================================
// Main Search Function
// =============================================================================

/// Perform search based on configuration
pub fn search(
  pool: DbPool,
  query: String,
  embedding_cfg: EmbeddingConfig,
  search_cfg: SearchConfig,
) -> Result(SearchResponse, SearchError) {
  // Validate query
  case string.length(string.trim(query)) < 2 {
    True -> Error(SearchInvalidQuery("Query too short"))
    False -> {
      let start_time = now_ms()

      // Execute search based on mode
      let results = case search_cfg.mode {
        ModeHybrid -> hybrid_search(pool, query, embedding_cfg, search_cfg)
        ModeVector -> vector_search(pool, query, embedding_cfg, search_cfg)
        ModeKeyword -> keyword_search(pool, query, search_cfg)
      }

      case results {
        Error(e) -> Error(e)
        Ok(raw_results) -> {
          // Enhance results with highlights and dialog info
          let enhanced =
            raw_results
            |> list.map(fn(r) { enhance_result(r, query) })
            |> list.filter(fn(r) { r.score >=. search_cfg.min_score })
            |> list.take(search_cfg.limit)

          let took = now_ms() - start_time

          Ok(SearchResponse(
            query: query,
            mode: mode_to_string(search_cfg.mode),
            total_results: list.length(enhanced),
            results: enhanced,
            took_ms: took,
          ))
        }
      }
    }
  }
}

// =============================================================================
// Search Implementations
// =============================================================================

/// Hybrid search combining vector and keyword
fn hybrid_search(
  pool: DbPool,
  query: String,
  embedding_cfg: EmbeddingConfig,
  search_cfg: SearchConfig,
) -> Result(List(SearchResult), SearchError) {
  // Generate query embedding
  case worker.embed_text(query, embedding_cfg) {
    Error(e) -> Error(SearchEmbeddingError(embedding_error_to_string(e)))
    Ok(embedding) -> {
      case
        postgres.hybrid_search(pool, embedding, query, search_cfg.limit * 2)
      {
        Error(e) -> Error(SearchDbError(db_error_to_string(e)))
        Ok(results) -> Ok(results)
      }
    }
  }
}

/// Vector-only semantic search
fn vector_search(
  pool: DbPool,
  query: String,
  embedding_cfg: EmbeddingConfig,
  search_cfg: SearchConfig,
) -> Result(List(SearchResult), SearchError) {
  // Generate query embedding
  case worker.embed_text(query, embedding_cfg) {
    Error(e) -> Error(SearchEmbeddingError(embedding_error_to_string(e)))
    Ok(embedding) -> {
      case
        postgres.semantic_search(
          pool,
          embedding,
          search_cfg.limit * 2,
          search_cfg.min_score,
        )
      {
        Error(e) -> Error(SearchDbError(db_error_to_string(e)))
        Ok(results) -> Ok(results)
      }
    }
  }
}

/// Keyword-only full-text search
fn keyword_search(
  pool: DbPool,
  query: String,
  search_cfg: SearchConfig,
) -> Result(List(SearchResult), SearchError) {
  case postgres.keyword_search(pool, query, search_cfg.limit * 2) {
    Error(e) -> Error(SearchDbError(db_error_to_string(e)))
    Ok(results) -> Ok(results)
  }
}

// =============================================================================
// Result Enhancement
// =============================================================================

/// Enhance search result with highlights and additional info
fn enhance_result(result: SearchResult, query: String) -> EnhancedSearchResult {
  // Extract highlights (snippets containing query terms)
  let highlights = extract_highlights(result.text_content, query, 3)

  // Calculate individual scores
  let vector_score = case result.vector_rank {
    9999 -> None
    rank -> Some(1.0 /. int.to_float(rank + 60))
  }

  let keyword_score = case result.keyword_rank {
    9999 -> None
    rank -> Some(1.0 /. int.to_float(rank + 60))
  }

  EnhancedSearchResult(
    message_id: result.message_id,
    dialog_id: result.dialog_id,
    dialog_title: None,
    // TODO: fetch from dialog table
    text_content: result.text_content,
    timestamp: result.timestamp,
    sender_name: result.sender_name,
    content_type: "text",
    // TODO: fetch actual content type
    score: result.rrf_score,
    vector_score: vector_score,
    keyword_score: keyword_score,
    highlights: highlights,
  )
}

/// Extract text snippets containing query terms
fn extract_highlights(
  text: String,
  query: String,
  max_highlights: Int,
) -> List(String) {
  let query_terms =
    query
    |> string.lowercase
    |> string.split(" ")
    |> list.filter(fn(t) { string.length(t) > 2 })

  let sentences =
    text
    |> string.split(".")
    |> list.map(string.trim)
    |> list.filter(fn(s) { string.length(s) > 10 })

  // Find sentences containing query terms
  sentences
  |> list.filter(fn(sentence) {
    let lower = string.lowercase(sentence)
    list.any(query_terms, fn(term) { string.contains(lower, term) })
  })
  |> list.take(max_highlights)
  |> list.map(fn(s) {
    // Truncate long sentences
    case string.length(s) > 200 {
      True -> string.slice(s, 0, 197) <> "..."
      False -> s
    }
  })
}

// =============================================================================
// Specialized Search Functions
// =============================================================================

/// Search within specific dialogs
pub fn search_in_dialogs(
  pool: DbPool,
  query: String,
  dialog_ids: List(Int),
  embedding_cfg: EmbeddingConfig,
  limit: Int,
) -> Result(SearchResponse, SearchError) {
  let cfg =
    SearchConfig(..default_config(), dialog_ids: Some(dialog_ids), limit: limit)

  search(pool, query, embedding_cfg, cfg)
}

/// Search by date range
pub fn search_by_date(
  pool: DbPool,
  query: String,
  date_from: String,
  date_to: String,
  embedding_cfg: EmbeddingConfig,
  limit: Int,
) -> Result(SearchResponse, SearchError) {
  let cfg =
    SearchConfig(
      ..default_config(),
      date_from: Some(date_from),
      date_to: Some(date_to),
      limit: limit,
    )

  search(pool, query, embedding_cfg, cfg)
}

/// Semantic similarity search (find similar messages)
pub fn find_similar(
  pool: DbPool,
  message_id: Int,
  embedding_cfg: EmbeddingConfig,
  limit: Int,
) -> Result(List(EnhancedSearchResult), SearchError) {
  // TODO: Get message embedding from DB and search for similar
  // For now, return empty list
  Ok([])
}

/// Search media content (transcriptions and image descriptions)
pub fn search_media(
  pool: DbPool,
  query: String,
  media_types: List(String),
  embedding_cfg: EmbeddingConfig,
  limit: Int,
) -> Result(SearchResponse, SearchError) {
  let cfg =
    SearchConfig(
      ..default_config(),
      content_types: Some(media_types),
      limit: limit,
    )

  search(pool, query, embedding_cfg, cfg)
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

fn embedding_error_to_string(e: worker.EmbeddingError) -> String {
  case e {
    worker.EmbeddingApiError(msg) -> "API error: " <> msg
    worker.EmbeddingDbError(msg) -> "DB error: " <> msg
    worker.EmbeddingRateLimited -> "Rate limited"
    worker.EmbeddingInvalidResponse(msg) -> "Invalid response: " <> msg
  }
}

// =============================================================================
// FFI Functions
// =============================================================================

/// Get current timestamp in milliseconds
@external(erlang, "vibee_time_ffi", "now_ms")
fn now_ms() -> Int

// =============================================================================
// Result Serialization
// =============================================================================

/// Convert search response to JSON
pub fn response_to_json(response: SearchResponse) -> String {
  json.object([
    #("query", json.string(response.query)),
    #("mode", json.string(response.mode)),
    #("total_results", json.int(response.total_results)),
    #("took_ms", json.int(response.took_ms)),
    #("results", json.array(response.results, enhanced_result_to_json)),
  ])
  |> json.to_string
}

fn enhanced_result_to_json(result: EnhancedSearchResult) -> json.Json {
  json.object([
    #("message_id", json.int(result.message_id)),
    #("dialog_id", json.int(result.dialog_id)),
    #("dialog_title", case result.dialog_title {
      Some(t) -> json.string(t)
      None -> json.null()
    }),
    #("text_content", json.string(result.text_content)),
    #("timestamp", json.string(result.timestamp)),
    #("sender_name", case result.sender_name {
      Some(n) -> json.string(n)
      None -> json.null()
    }),
    #("content_type", json.string(result.content_type)),
    #("score", json.float(result.score)),
    #("vector_score", case result.vector_score {
      Some(s) -> json.float(s)
      None -> json.null()
    }),
    #("keyword_score", case result.keyword_score {
      Some(s) -> json.float(s)
      None -> json.null()
    }),
    #("highlights", json.array(result.highlights, json.string)),
  ])
}

/// Convert error to JSON
pub fn error_to_json(error: SearchError) -> String {
  let msg = case error {
    SearchEmbeddingError(m) -> "Embedding error: " <> m
    SearchDbError(m) -> "Database error: " <> m
    SearchInvalidQuery(m) -> "Invalid query: " <> m
  }

  json.object([
    #("error", json.bool(True)),
    #("message", json.string(msg)),
  ])
  |> json.to_string
}
