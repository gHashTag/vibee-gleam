// Knowledge Provider - Main interface for RAG system
// Provides context-aware search for agent responses

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/knowledge/types.{
  type Document, type KnowledgeConfig, type KnowledgeError, type SearchQuery,
  type SearchResult, Document, KnowledgeConfig, SearchQuery, SearchResult,
}

/// Knowledge provider state
pub type KnowledgeProvider {
  KnowledgeProvider(
    config: KnowledgeConfig,
    documents: List(Document),
  )
}

/// Create a new knowledge provider with default config
pub fn new(base_path: String) -> KnowledgeProvider {
  let config =
    KnowledgeConfig(
      base_path: base_path,
      file_extensions: [".md", ".txt"],
      max_results: 5,
    )

  KnowledgeProvider(config: config, documents: [])
}

/// Create provider with custom config
pub fn with_config(config: KnowledgeConfig) -> KnowledgeProvider {
  KnowledgeProvider(config: config, documents: [])
}

/// Add a document to the knowledge base
pub fn add_document(
  provider: KnowledgeProvider,
  document: Document,
) -> KnowledgeProvider {
  KnowledgeProvider(..provider, documents: [document, ..provider.documents])
}

/// Add multiple documents
pub fn add_documents(
  provider: KnowledgeProvider,
  documents: List(Document),
) -> KnowledgeProvider {
  let new_docs = list.append(documents, provider.documents)
  KnowledgeProvider(..provider, documents: new_docs)
}

/// Search for relevant documents
pub fn search(
  provider: KnowledgeProvider,
  query: String,
) -> List(SearchResult) {
  let search_query =
    SearchQuery(query: query, tags: [], limit: provider.config.max_results)

  search_with_query(provider, search_query)
}

/// Search with full query options
pub fn search_with_query(
  provider: KnowledgeProvider,
  query: SearchQuery,
) -> List(SearchResult) {
  let query_terms = tokenize(query.query)

  provider.documents
  |> list.filter_map(fn(doc) {
    let score = calculate_score(doc, query_terms)
    case score >. 0.0 {
      True -> Ok(SearchResult(document: doc, score: score, matched_terms: query_terms))
      False -> Error(Nil)
    }
  })
  |> list.sort(fn(a, b) { compare_floats(b.score, a.score) })
  |> list.take(query.limit)
}

/// Search by tags
pub fn search_by_tags(
  provider: KnowledgeProvider,
  tags: List(String),
) -> List(Document) {
  provider.documents
  |> list.filter(fn(doc) {
    list.any(tags, fn(tag) { list.contains(doc.tags, tag) })
  })
}

/// Get document by ID
pub fn get_document(
  provider: KnowledgeProvider,
  id: String,
) -> Option(Document) {
  list.find(provider.documents, fn(doc) { doc.id == id })
  |> option.from_result()
}

/// Get all documents
pub fn all_documents(provider: KnowledgeProvider) -> List(Document) {
  provider.documents
}

/// Get document count
pub fn document_count(provider: KnowledgeProvider) -> Int {
  list.length(provider.documents)
}

/// Build context string from search results for LLM
pub fn build_context(results: List(SearchResult)) -> String {
  case results {
    [] -> ""
    _ -> {
      let context_parts =
        results
        |> list.map(fn(result) {
          "### " <> result.document.title <> "\n" <> result.document.content
        })

      "## Relevant Knowledge\n\n" <> string.join(context_parts, "\n\n---\n\n")
    }
  }
}

// Private helpers

/// Tokenize a query string into search terms
fn tokenize(text: String) -> List(String) {
  text
  |> string.lowercase()
  |> string.split(" ")
  |> list.filter(fn(s) { string.length(s) > 2 })
  |> list.unique()
}

/// Calculate relevance score for a document
fn calculate_score(doc: Document, terms: List(String)) -> Float {
  let content_lower = string.lowercase(doc.content)
  let title_lower = string.lowercase(doc.title)

  let content_matches =
    list.count(terms, fn(term) { string.contains(content_lower, term) })

  let title_matches =
    list.count(terms, fn(term) { string.contains(title_lower, term) })

  // Title matches are worth more
  let title_score = int_to_float(title_matches) *. 2.0
  let content_score = int_to_float(content_matches)

  // Normalize by term count
  let term_count = int_to_float(list.length(terms))
  case term_count >. 0.0 {
    True -> { title_score +. content_score } /. term_count
    False -> 0.0
  }
}

/// Compare two floats for sorting (returns -1, 0, or 1)
fn compare_floats(a: Float, b: Float) -> order.Order {
  case a >. b {
    True -> order.Gt
    False ->
      case a <. b {
        True -> order.Lt
        False -> order.Eq
      }
  }
}

/// Convert int to float
fn int_to_float(n: Int) -> Float {
  case n {
    0 -> 0.0
    1 -> 1.0
    2 -> 2.0
    3 -> 3.0
    4 -> 4.0
    5 -> 5.0
    6 -> 6.0
    7 -> 7.0
    8 -> 8.0
    9 -> 9.0
    10 -> 10.0
    _ -> int_to_float_helper(n, 0.0)
  }
}

fn int_to_float_helper(n: Int, acc: Float) -> Float {
  case n {
    0 -> acc
    _ -> int_to_float_helper(n - 1, acc +. 1.0)
  }
}

import gleam/order
