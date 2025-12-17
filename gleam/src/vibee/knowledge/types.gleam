// Knowledge Base Types for RAG System

import gleam/option.{type Option}

/// A knowledge document with content and metadata
pub type Document {
  Document(
    id: String,
    title: String,
    content: String,
    source: String,
    tags: List(String),
  )
}

/// Search result with relevance score
pub type SearchResult {
  SearchResult(
    document: Document,
    score: Float,
    matched_terms: List(String),
  )
}

/// Knowledge base configuration
pub type KnowledgeConfig {
  KnowledgeConfig(
    base_path: String,
    file_extensions: List(String),
    max_results: Int,
  )
}

/// Search query with optional filters
pub type SearchQuery {
  SearchQuery(
    query: String,
    tags: List(String),
    limit: Int,
  )
}

/// Error types for knowledge operations
pub type KnowledgeError {
  FileNotFound(path: String)
  ParseError(path: String, message: String)
  SearchError(message: String)
}
