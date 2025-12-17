// Text Search utilities for Knowledge Base
// Simple TF-IDF style search without external dependencies

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/knowledge/types.{type Document, type SearchResult, SearchResult}

/// Search index for faster lookups
pub type SearchIndex {
  SearchIndex(
    documents: List(Document),
    term_frequencies: Dict(String, Dict(String, Int)),
    document_frequencies: Dict(String, Int),
  )
}

/// Create a new search index from documents
pub fn build_index(documents: List(Document)) -> SearchIndex {
  let tf = build_term_frequencies(documents)
  let df = build_document_frequencies(documents)

  SearchIndex(documents: documents, term_frequencies: tf, document_frequencies: df)
}

/// Search using the index
pub fn search(index: SearchIndex, query: String, limit: Int) -> List(SearchResult) {
  let query_terms = tokenize(query)

  index.documents
  |> list.filter_map(fn(doc) {
    let score = calculate_tfidf_score(index, doc, query_terms)
    case score >. 0.0 {
      True -> {
        let matched = find_matched_terms(doc, query_terms)
        Ok(SearchResult(document: doc, score: score, matched_terms: matched))
      }
      False -> Error(Nil)
    }
  })
  |> list.sort(fn(a, b) { compare_floats(b.score, a.score) })
  |> list.take(limit)
}

/// Simple keyword search without index
pub fn simple_search(
  documents: List(Document),
  query: String,
  limit: Int,
) -> List(SearchResult) {
  let query_terms = tokenize(query)

  documents
  |> list.filter_map(fn(doc) {
    let score = simple_score(doc, query_terms)
    case score >. 0.0 {
      True -> {
        let matched = find_matched_terms(doc, query_terms)
        Ok(SearchResult(document: doc, score: score, matched_terms: matched))
      }
      False -> Error(Nil)
    }
  })
  |> list.sort(fn(a, b) { compare_floats(b.score, a.score) })
  |> list.take(limit)
}

/// Full-text search checking both title and content
pub fn full_text_search(
  documents: List(Document),
  query: String,
) -> List(Document) {
  let query_lower = string.lowercase(query)

  documents
  |> list.filter(fn(doc) {
    let title_match = string.contains(string.lowercase(doc.title), query_lower)
    let content_match =
      string.contains(string.lowercase(doc.content), query_lower)
    title_match || content_match
  })
}

/// Phrase search - find exact phrase in content
pub fn phrase_search(
  documents: List(Document),
  phrase: String,
) -> List(Document) {
  let phrase_lower = string.lowercase(phrase)

  documents
  |> list.filter(fn(doc) {
    string.contains(string.lowercase(doc.content), phrase_lower)
  })
}

// Private helpers

/// Tokenize text into search terms
fn tokenize(text: String) -> List(String) {
  text
  |> string.lowercase()
  |> string.replace(",", " ")
  |> string.replace(".", " ")
  |> string.replace("?", " ")
  |> string.replace("!", " ")
  |> string.split(" ")
  |> list.filter(fn(s) { string.length(s) > 2 })
  |> list.filter(fn(s) { !is_stop_word(s) })
  |> list.unique()
}

/// Check if word is a stop word
fn is_stop_word(word: String) -> Bool {
  let stop_words = [
    "the", "a", "an", "is", "are", "was", "were", "be", "been", "being",
    "have", "has", "had", "do", "does", "did", "will", "would", "could",
    "should", "may", "might", "must", "shall", "can", "and", "or", "but",
    "not", "no", "nor", "so", "as", "if", "then", "than", "that", "this",
    "these", "those", "of", "for", "in", "on", "at", "to", "from", "with",
    "by", "about", "into", "through", "during", "before", "after", "above",
    "below", "up", "down", "out", "off", "over", "under", "again", "further",
    "once", "here", "there", "when", "where", "why", "how", "all", "each",
    "few", "more", "most", "other", "some", "such", "only", "own", "same",
    "too", "very", "just", "also",
  ]
  list.contains(stop_words, word)
}

/// Build term frequency map for each document
fn build_term_frequencies(
  documents: List(Document),
) -> Dict(String, Dict(String, Int)) {
  documents
  |> list.fold(dict.new(), fn(acc, doc) {
    let terms = tokenize(doc.content <> " " <> doc.title)
    let tf =
      terms
      |> list.fold(dict.new(), fn(term_acc, term) {
        let current = dict.get(term_acc, term) |> option_unwrap(0)
        dict.insert(term_acc, term, current + 1)
      })
    dict.insert(acc, doc.id, tf)
  })
}

/// Build document frequency map
fn build_document_frequencies(documents: List(Document)) -> Dict(String, Int) {
  documents
  |> list.fold(dict.new(), fn(acc, doc) {
    let terms = tokenize(doc.content <> " " <> doc.title) |> list.unique()
    terms
    |> list.fold(acc, fn(df_acc, term) {
      let current = dict.get(df_acc, term) |> option_unwrap(0)
      dict.insert(df_acc, term, current + 1)
    })
  })
}

/// Calculate TF-IDF score
fn calculate_tfidf_score(
  index: SearchIndex,
  doc: Document,
  query_terms: List(String),
) -> Float {
  let doc_count = list.length(index.documents)
  let doc_tf = dict.get(index.term_frequencies, doc.id) |> option_unwrap(dict.new())

  query_terms
  |> list.fold(0.0, fn(score, term) {
    let tf = dict.get(doc_tf, term) |> option_unwrap(0)
    let df = dict.get(index.document_frequencies, term) |> option_unwrap(1)

    // TF-IDF formula: tf * log(N / df)
    let tf_float = int_to_float(tf)
    let idf = log_approx(int_to_float(doc_count) /. int_to_float(df))

    score +. tf_float *. idf
  })
}

/// Simple score without TF-IDF
fn simple_score(doc: Document, query_terms: List(String)) -> Float {
  let content_lower = string.lowercase(doc.content)
  let title_lower = string.lowercase(doc.title)

  let content_matches =
    list.count(query_terms, fn(term) { string.contains(content_lower, term) })

  let title_matches =
    list.count(query_terms, fn(term) { string.contains(title_lower, term) })

  // Title matches are worth 3x
  int_to_float(title_matches * 3 + content_matches)
}

/// Find which query terms matched in the document
fn find_matched_terms(doc: Document, query_terms: List(String)) -> List(String) {
  let content_lower = string.lowercase(doc.content <> " " <> doc.title)

  query_terms
  |> list.filter(fn(term) { string.contains(content_lower, term) })
}

/// Compare floats for sorting
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

/// Unwrap Result with default
fn option_unwrap(result: Result(a, b), default: a) -> a {
  case result {
    Ok(value) -> value
    Error(_) -> default
  }
}

/// Approximate natural log using series
fn log_approx(x: Float) -> Float {
  // Simple approximation: ln(x) ≈ 2 * (x-1)/(x+1) for small x
  case x <=. 0.0 {
    True -> 0.0
    False ->
      case x >. 10.0 {
        True -> 2.3 // ~ln(10)
        False -> {
          let ratio = { x -. 1.0 } /. { x +. 1.0 }
          2.0 *. ratio
        }
      }
  }
}

/// Convert int to float
fn int_to_float(n: Int) -> Float {
  case n {
    0 -> 0.0
    _ -> int_to_float_helper(n, 0.0)
  }
}

fn int_to_float_helper(n: Int, acc: Float) -> Float {
  case n <= 0 {
    True -> acc
    False -> int_to_float_helper(n - 1, acc +. 1.0)
  }
}

import gleam/order
