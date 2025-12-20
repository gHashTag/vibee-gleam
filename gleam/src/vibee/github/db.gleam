// GitHub Documents Database Operations
// CRUD and search operations for github_documents table

import gleam/dynamic/decode.{type Decoder}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import pog
import vibee/db/postgres.{type DbPool}
import vibee/github/types.{
  type DocumentType, type GitHubDocument, type GitHubError, type GitHubSearchResult,
  DatabaseError, GitHubDocument, GitHubSearchResult, LanguagesDoc, ProfileDoc,
  ReadmeDoc, RepoDoc, doc_type_from_string, doc_type_to_string,
}

// ============================================================
// Helper Functions
// ============================================================

/// Helper to add multiple parameters to a query
fn add_parameters(query: pog.Query(t), params: List(pog.Value)) -> pog.Query(t) {
  list.fold(params, query, fn(q, p) { pog.parameter(q, p) })
}

/// Convert pog error to string
fn pog_error_to_string(err: pog.QueryError) -> String {
  case err {
    pog.ConstraintViolated(msg, constraint, _) ->
      "Constraint violated: " <> constraint <> " - " <> msg
    pog.PostgresqlError(code, name, msg) ->
      "PostgreSQL error " <> code <> " (" <> name <> "): " <> msg
    pog.UnexpectedArgumentCount(expected, got) ->
      "Unexpected argument count: expected "
      <> int.to_string(expected)
      <> ", got "
      <> int.to_string(got)
    pog.UnexpectedArgumentType(expected, got) ->
      "Unexpected argument type: expected " <> expected <> ", got " <> got
    pog.UnexpectedResultType(_) -> "Unexpected result type"
    pog.ConnectionUnavailable -> "Database connection unavailable"
    pog.QueryTimeout -> "Query timeout"
  }
}

// ============================================================
// Insert/Update Operations
// ============================================================

/// Insert or update a GitHub document
pub fn upsert_document(
  pool: DbPool,
  doc: GitHubDocument,
) -> Result(Int, GitHubError) {
  let sql =
    "INSERT INTO github_documents
      (username, doc_type, repo_name, title, content, source_url, tags, stars, language, topics, fetched_at)
     VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, NOW())
     ON CONFLICT (username, doc_type, COALESCE(repo_name, '__profile__'))
     DO UPDATE SET
       title = EXCLUDED.title,
       content = EXCLUDED.content,
       source_url = EXCLUDED.source_url,
       tags = EXCLUDED.tags,
       stars = EXCLUDED.stars,
       language = EXCLUDED.language,
       topics = EXCLUDED.topics,
       fetched_at = NOW(),
       embedding = NULL  -- Reset embedding on content change
     RETURNING id"

  let params = [
    pog.text(doc.username),
    pog.text(doc_type_to_string(doc.doc_type)),
    pog.nullable(pog.text, doc.repo_name),
    pog.text(doc.title),
    pog.text(doc.content),
    pog.text(doc.source_url),
    pog.array(pog.text, doc.tags),
    pog.int(doc.stars),
    pog.nullable(pog.text, doc.language),
    pog.array(pog.text, doc.topics),
  ]

  let id_decoder = decode.at([0], decode.int)

  case pog.query(sql)
    |> add_parameters(params)
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(result) -> {
      case result.rows {
        [id] -> Ok(id)
        _ -> Error(DatabaseError("No ID returned from insert"))
      }
    }
    Error(e) -> Error(DatabaseError(pog_error_to_string(e)))
  }
}

/// Insert multiple documents (batch)
pub fn upsert_documents(
  pool: DbPool,
  docs: List(GitHubDocument),
) -> Result(List(Int), GitHubError) {
  list.try_map(docs, fn(doc) { upsert_document(pool, doc) })
}

/// Update document embedding
pub fn update_embedding(
  pool: DbPool,
  doc_id: Int,
  embedding: List(Float),
) -> Result(Nil, GitHubError) {
  // Convert embedding list to PostgreSQL vector format
  let embedding_str =
    "["
    <> string.join(list.map(embedding, float.to_string), ",")
    <> "]"

  let sql =
    "UPDATE github_documents SET embedding = $1::vector WHERE id = $2"

  let params = [pog.text(embedding_str), pog.int(doc_id)]

  case pog.query(sql) |> add_parameters(params) |> pog.execute(pool) {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DatabaseError(pog_error_to_string(e)))
  }
}

// ============================================================
// Read Operations
// ============================================================

/// Get all documents for a username
pub fn get_documents_by_username(
  pool: DbPool,
  username: String,
) -> Result(List(GitHubDocument), GitHubError) {
  let sql =
    "SELECT id, username, doc_type, repo_name, title, content, source_url,
            tags, stars, language, topics, fetched_at::text
     FROM github_documents
     WHERE username = $1
     ORDER BY stars DESC, fetched_at DESC"

  let params = [pog.text(username)]

  case pog.query(sql)
    |> add_parameters(params)
    |> pog.returning(document_decoder())
    |> pog.execute(pool)
  {
    Ok(result) -> Ok(result.rows)
    Error(e) -> Error(DatabaseError(pog_error_to_string(e)))
  }
}

/// Get documents by type
pub fn get_documents_by_type(
  pool: DbPool,
  username: String,
  doc_type: DocumentType,
) -> Result(List(GitHubDocument), GitHubError) {
  let sql =
    "SELECT id, username, doc_type, repo_name, title, content, source_url,
            tags, stars, language, topics, fetched_at::text
     FROM github_documents
     WHERE username = $1 AND doc_type = $2
     ORDER BY stars DESC, fetched_at DESC"

  let params = [pog.text(username), pog.text(doc_type_to_string(doc_type))]

  case pog.query(sql)
    |> add_parameters(params)
    |> pog.returning(document_decoder())
    |> pog.execute(pool)
  {
    Ok(result) -> Ok(result.rows)
    Error(e) -> Error(DatabaseError(pog_error_to_string(e)))
  }
}

/// Get documents without embeddings
pub fn get_documents_without_embeddings(
  pool: DbPool,
  limit: Int,
) -> Result(List(GitHubDocument), GitHubError) {
  let sql =
    "SELECT id, username, doc_type, repo_name, title, content, source_url,
            tags, stars, language, topics, fetched_at::text
     FROM github_documents
     WHERE embedding IS NULL
     ORDER BY stars DESC, fetched_at DESC
     LIMIT $1"

  let params = [pog.int(limit)]

  case pog.query(sql)
    |> add_parameters(params)
    |> pog.returning(document_decoder())
    |> pog.execute(pool)
  {
    Ok(result) -> Ok(result.rows)
    Error(e) -> Error(DatabaseError(pog_error_to_string(e)))
  }
}

// ============================================================
// Search Operations
// ============================================================

/// Semantic search using vector similarity
pub fn semantic_search(
  pool: DbPool,
  query_embedding: List(Float),
  username: Option(String),
  doc_type: Option(DocumentType),
  limit: Int,
) -> Result(List(GitHubSearchResult), GitHubError) {
  let embedding_str =
    "["
    <> string.join(list.map(query_embedding, float.to_string), ",")
    <> "]"

  let #(sql, params) = case username, doc_type {
    Some(u), Some(dt) -> {
      #(
        "SELECT id, username, doc_type, repo_name, title, content, source_url,
                tags, stars, language, topics, fetched_at::text,
                1 - (embedding <=> $1::vector) as score
         FROM github_documents
         WHERE embedding IS NOT NULL AND username = $2 AND doc_type = $3
         ORDER BY embedding <=> $1::vector
         LIMIT $4",
        [
          pog.text(embedding_str),
          pog.text(u),
          pog.text(doc_type_to_string(dt)),
          pog.int(limit),
        ],
      )
    }
    Some(u), None -> {
      #(
        "SELECT id, username, doc_type, repo_name, title, content, source_url,
                tags, stars, language, topics, fetched_at::text,
                1 - (embedding <=> $1::vector) as score
         FROM github_documents
         WHERE embedding IS NOT NULL AND username = $2
         ORDER BY embedding <=> $1::vector
         LIMIT $3",
        [pog.text(embedding_str), pog.text(u), pog.int(limit)],
      )
    }
    None, Some(dt) -> {
      #(
        "SELECT id, username, doc_type, repo_name, title, content, source_url,
                tags, stars, language, topics, fetched_at::text,
                1 - (embedding <=> $1::vector) as score
         FROM github_documents
         WHERE embedding IS NOT NULL AND doc_type = $2
         ORDER BY embedding <=> $1::vector
         LIMIT $3",
        [pog.text(embedding_str), pog.text(doc_type_to_string(dt)), pog.int(limit)],
      )
    }
    None, None -> {
      #(
        "SELECT id, username, doc_type, repo_name, title, content, source_url,
                tags, stars, language, topics, fetched_at::text,
                1 - (embedding <=> $1::vector) as score
         FROM github_documents
         WHERE embedding IS NOT NULL
         ORDER BY embedding <=> $1::vector
         LIMIT $2",
        [pog.text(embedding_str), pog.int(limit)],
      )
    }
  }

  case pog.query(sql)
    |> add_parameters(params)
    |> pog.returning(search_result_decoder())
    |> pog.execute(pool)
  {
    Ok(result) -> Ok(result.rows)
    Error(e) -> Error(DatabaseError(pog_error_to_string(e)))
  }
}

/// Full-text keyword search
pub fn keyword_search(
  pool: DbPool,
  query: String,
  username: Option(String),
  limit: Int,
) -> Result(List(GitHubSearchResult), GitHubError) {
  let #(sql, params) = case username {
    Some(u) -> {
      #(
        "SELECT id, username, doc_type, repo_name, title, content, source_url,
                tags, stars, language, topics, fetched_at::text,
                ts_rank(to_tsvector('english', title || ' ' || content), plainto_tsquery('english', $1)) as score
         FROM github_documents
         WHERE username = $2
           AND to_tsvector('english', title || ' ' || content) @@ plainto_tsquery('english', $1)
         ORDER BY score DESC
         LIMIT $3",
        [pog.text(query), pog.text(u), pog.int(limit)],
      )
    }
    None -> {
      #(
        "SELECT id, username, doc_type, repo_name, title, content, source_url,
                tags, stars, language, topics, fetched_at::text,
                ts_rank(to_tsvector('english', title || ' ' || content), plainto_tsquery('english', $1)) as score
         FROM github_documents
         WHERE to_tsvector('english', title || ' ' || content) @@ plainto_tsquery('english', $1)
         ORDER BY score DESC
         LIMIT $2",
        [pog.text(query), pog.int(limit)],
      )
    }
  }

  case pog.query(sql)
    |> add_parameters(params)
    |> pog.returning(search_result_decoder())
    |> pog.execute(pool)
  {
    Ok(result) -> Ok(result.rows)
    Error(e) -> Error(DatabaseError(pog_error_to_string(e)))
  }
}

/// Hybrid search combining vector and keyword
pub fn hybrid_search(
  pool: DbPool,
  query: String,
  query_embedding: List(Float),
  username: Option(String),
  limit: Int,
) -> Result(List(GitHubSearchResult), GitHubError) {
  // Get results from both methods
  use vector_results <- try_result(semantic_search(
    pool,
    query_embedding,
    username,
    None,
    limit * 2,
  ))
  use keyword_results <- try_result(keyword_search(pool, query, username, limit * 2))

  // Combine using RRF (Reciprocal Rank Fusion)
  let combined = rrf_combine(vector_results, keyword_results, 60.0)

  Ok(list.take(combined, limit))
}

// ============================================================
// Delete Operations
// ============================================================

/// Delete all documents for a username
pub fn delete_by_username(
  pool: DbPool,
  username: String,
) -> Result(Int, GitHubError) {
  let sql = "DELETE FROM github_documents WHERE username = $1"
  let params = [pog.text(username)]

  case pog.query(sql) |> add_parameters(params) |> pog.execute(pool) {
    Ok(result) -> Ok(result.count)
    Error(e) -> Error(DatabaseError(pog_error_to_string(e)))
  }
}

// ============================================================
// Statistics
// ============================================================

/// Get document counts by username
pub fn get_stats(
  pool: DbPool,
  username: String,
) -> Result(#(Int, Int, Int), GitHubError) {
  let sql =
    "SELECT
       COUNT(*) as total,
       COUNT(embedding) as with_embeddings,
       COALESCE(SUM(stars), 0) as total_stars
     FROM github_documents
     WHERE username = $1"

  let params = [pog.text(username)]

  let stats_decoder = {
    use total <- decode.field(0, decode.int)
    use with_emb <- decode.field(1, decode.int)
    use stars <- decode.field(2, decode.int)
    decode.success(#(total, with_emb, stars))
  }

  case pog.query(sql)
    |> add_parameters(params)
    |> pog.returning(stats_decoder)
    |> pog.execute(pool)
  {
    Ok(result) -> {
      case result.rows {
        [stats] -> Ok(stats)
        _ -> Ok(#(0, 0, 0))
      }
    }
    Error(e) -> Error(DatabaseError(pog_error_to_string(e)))
  }
}

// ============================================================
// Decoders
// ============================================================

fn document_decoder() -> decode.Decoder(GitHubDocument) {
  use id <- decode.field(0, decode.int)
  use username <- decode.field(1, decode.string)
  use doc_type_str <- decode.field(2, decode.string)
  use repo_name <- decode.field(3, decode.optional(decode.string))
  use title <- decode.field(4, decode.string)
  use content <- decode.field(5, decode.string)
  use source_url <- decode.field(6, decode.string)
  use tags <- decode.field(7, decode.list(decode.string))
  use stars <- decode.field(8, decode.int)
  use language <- decode.field(9, decode.optional(decode.string))
  use topics <- decode.field(10, decode.list(decode.string))
  use fetched_at <- decode.field(11, decode.string)

  let doc_type = case doc_type_from_string(doc_type_str) {
    Ok(dt) -> dt
    Error(_) -> ProfileDoc
  }

  decode.success(GitHubDocument(
    id: Some(id),
    username: username,
    doc_type: doc_type,
    repo_name: repo_name,
    title: title,
    content: content,
    source_url: source_url,
    tags: tags,
    stars: stars,
    language: language,
    topics: topics,
    fetched_at: fetched_at,
  ))
}

fn search_result_decoder() -> decode.Decoder(GitHubSearchResult) {
  use id <- decode.field(0, decode.int)
  use username <- decode.field(1, decode.string)
  use doc_type_str <- decode.field(2, decode.string)
  use repo_name <- decode.field(3, decode.optional(decode.string))
  use title <- decode.field(4, decode.string)
  use content <- decode.field(5, decode.string)
  use source_url <- decode.field(6, decode.string)
  use tags <- decode.field(7, decode.list(decode.string))
  use stars <- decode.field(8, decode.int)
  use language <- decode.field(9, decode.optional(decode.string))
  use topics <- decode.field(10, decode.list(decode.string))
  use fetched_at <- decode.field(11, decode.string)
  use score <- decode.field(12, decode.float)

  let doc_type = case doc_type_from_string(doc_type_str) {
    Ok(dt) -> dt
    Error(_) -> ProfileDoc
  }

  decode.success(GitHubSearchResult(
    document: GitHubDocument(
      id: Some(id),
      username: username,
      doc_type: doc_type,
      repo_name: repo_name,
      title: title,
      content: content,
      source_url: source_url,
      tags: tags,
      stars: stars,
      language: language,
      topics: topics,
      fetched_at: fetched_at,
    ),
    score: score,
    matched_terms: [],
  ))
}

// ============================================================
// RRF (Reciprocal Rank Fusion)
// ============================================================

fn rrf_combine(
  vector_results: List(GitHubSearchResult),
  keyword_results: List(GitHubSearchResult),
  k: Float,
) -> List(GitHubSearchResult) {
  // Calculate RRF scores
  let vector_scores =
    list.index_map(vector_results, fn(result, idx) {
      let doc_id = case result.document.id {
        Some(id) -> id
        None -> 0
      }
      #(doc_id, 1.0 /. { k +. int.to_float(idx + 1) }, result)
    })

  let keyword_scores =
    list.index_map(keyword_results, fn(result, idx) {
      let doc_id = case result.document.id {
        Some(id) -> id
        None -> 0
      }
      #(doc_id, 1.0 /. { k +. int.to_float(idx + 1) }, result)
    })

  // Merge scores
  let all_ids =
    list.append(
      list.map(vector_scores, fn(x) { x.0 }),
      list.map(keyword_scores, fn(x) { x.0 }),
    )
    |> list.unique

  list.map(all_ids, fn(id) {
    let v_score =
      list.find(vector_scores, fn(x) { x.0 == id })
      |> option_to_float(fn(x) { x.1 })

    let k_score =
      list.find(keyword_scores, fn(x) { x.0 == id })
      |> option_to_float(fn(x) { x.1 })

    let combined_score = v_score +. k_score

    let result =
      list.find(vector_scores, fn(x) { x.0 == id })
      |> fn(r) {
        case r {
          Ok(#(_, _, res)) -> res
          Error(_) -> {
            case list.find(keyword_scores, fn(x) { x.0 == id }) {
              Ok(#(_, _, res)) -> res
              Error(_) ->
                GitHubSearchResult(
                  document: GitHubDocument(
                    id: None,
                    username: "",
                    doc_type: ProfileDoc,
                    repo_name: None,
                    title: "",
                    content: "",
                    source_url: "",
                    tags: [],
                    stars: 0,
                    language: None,
                    topics: [],
                    fetched_at: "",
                  ),
                  score: 0.0,
                  matched_terms: [],
                )
            }
          }
        }
      }

    GitHubSearchResult(..result, score: combined_score)
  })
  |> list.sort(fn(a, b) { float.compare(b.score, a.score) })
}

fn option_to_float(
  result: Result(a, Nil),
  extractor: fn(a) -> Float,
) -> Float {
  case result {
    Ok(a) -> extractor(a)
    Error(_) -> 0.0
  }
}

fn try_result(
  result: Result(a, GitHubError),
  next: fn(a) -> Result(b, GitHubError),
) -> Result(b, GitHubError) {
  case result {
    Ok(value) -> next(value)
    Error(e) -> Error(e)
  }
}
