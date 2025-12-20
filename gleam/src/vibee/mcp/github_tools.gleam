// MCP GitHub Tools - GitHub profile integration for digital clone
// Provides tools for loading and searching GitHub profile data

import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/db/postgres
import vibee/embedding/worker as embedding
import vibee/github/client as github_client
import vibee/github/db as github_db
import vibee/github/loader as github_loader
import vibee/github/types as github_types
import vibee/mcp/types.{type Tool, type ToolResult, TextContent, Tool, ToolResult}

// ============================================================
// Tool Definitions
// ============================================================

/// Tool to load GitHub profile into knowledge base
pub fn github_load_profile_tool() -> Tool {
  Tool(
    name: "github_load_profile",
    description: "Load GitHub profile into knowledge base for digital clone. Fetches user info, repositories, READMEs, and languages. Generates embeddings for semantic search. Use this to index your GitHub profile as part of VIBEE's knowledge.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "username",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("GitHub username to load (e.g., 'gHashTag')")),
            ]),
          ),
          #(
            "include_readmes",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("Fetch README files from repositories (default: true)")),
              #("default", json.bool(True)),
            ]),
          ),
          #(
            "max_repos",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Maximum repositories to process (default: 50)")),
              #("default", json.int(50)),
            ]),
          ),
          #(
            "min_stars",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Only include repos with at least this many stars (default: 0)")),
              #("default", json.int(0)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["username"], json.string)),
    ]),
  )
}

/// Tool to search indexed GitHub documents
pub fn github_search_tool() -> Tool {
  Tool(
    name: "github_search",
    description: "Search indexed GitHub documents using hybrid search (vector + keyword). Returns relevant profile info, repositories, and README content. Use this to find information about skills, projects, and experience from the indexed GitHub profile.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "query",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Search query (e.g., 'React Native experience', 'TypeScript projects')")),
            ]),
          ),
          #(
            "username",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Filter by GitHub username (optional)")),
            ]),
          ),
          #(
            "doc_type",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Filter by document type")),
              #("enum", json.array(["profile", "repo", "readme", "all"], json.string)),
              #("default", json.string("all")),
            ]),
          ),
          #(
            "limit",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Maximum results (default: 10)")),
              #("default", json.int(10)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["query"], json.string)),
    ]),
  )
}

/// Tool to get GitHub profile summary
pub fn github_profile_summary_tool() -> Tool {
  Tool(
    name: "github_profile_summary",
    description: "Get a summary of indexed GitHub profile data. Returns statistics about repos, languages, and indexed documents.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "username",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("GitHub username")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["username"], json.string)),
    ]),
  )
}

/// Tool to generate embeddings for GitHub documents
pub fn github_generate_embeddings_tool() -> Tool {
  Tool(
    name: "github_generate_embeddings",
    description: "Generate vector embeddings for GitHub documents that don't have them yet. Required for semantic search to work.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "limit",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Maximum documents to process (default: 100)")),
              #("default", json.int(100)),
            ]),
          ),
        ]),
      ),
      #("required", json.array([], json.string)),
    ]),
  )
}

/// Get all GitHub tools
pub fn get_github_tools() -> List(Tool) {
  [
    github_load_profile_tool(),
    github_search_tool(),
    github_profile_summary_tool(),
    github_generate_embeddings_tool(),
  ]
}

/// Get all GitHub handlers
pub fn get_github_handlers() -> List(#(String, fn(json.Json) -> ToolResult)) {
  [
    #("github_load_profile", handle_github_load_profile),
    #("github_search", handle_github_search),
    #("github_profile_summary", handle_github_profile_summary),
    #("github_generate_embeddings", handle_github_generate_embeddings),
  ]
}

// ============================================================
// Tool Handlers
// ============================================================

/// Handle github_load_profile tool
pub fn handle_github_load_profile(args: json.Json) -> ToolResult {
  // Parse arguments
  case json_get_string(args, "username") {
    Error(_) -> error_result("Missing required parameter: username")
    Ok(username) -> {
      let include_readmes = json_get_bool(args, "include_readmes")
        |> option.unwrap(True)

      let max_repos = json_get_int(args, "max_repos")
        |> option.unwrap(50)

      let min_stars = json_get_int(args, "min_stars")
        |> option.unwrap(0)

      // Get database connection
      case postgres.get_global_pool() {
        None -> error_result("Database not connected")
        Some(pool) -> {
          // Build request
          let request = github_types.LoadProfileRequest(
            username: username,
            include_readmes: include_readmes,
            max_repos: max_repos,
            min_stars: min_stars,
          )

          // Load profile from GitHub
          case github_client.load_profile(request) {
            Error(e) -> error_result("Failed to load GitHub profile: " <> github_types.error_to_string(e))
            Ok(profile) -> {
              // Convert to documents
              let docs = github_loader.profile_to_documents(profile)

              // Add languages summary document
              let langs_doc = github_loader.languages_to_document(username, profile.repos)
              let all_docs = [langs_doc, ..docs]

              // Save to database
              case github_db.upsert_documents(pool, all_docs) {
                Error(e) -> error_result("Failed to save documents: " <> github_types.error_to_string(e))
                Ok(ids) -> {
                  let result = json.object([
                    #("success", json.bool(True)),
                    #("username", json.string(username)),
                    #("profile", json.object([
                      #("name", json.string(option.unwrap(profile.user.name, profile.user.login))),
                      #("bio", json.string(option.unwrap(profile.user.bio, ""))),
                      #("followers", json.int(profile.user.followers)),
                      #("public_repos", json.int(profile.user.public_repos)),
                    ])),
                    #("documents_created", json.int(list.length(ids))),
                    #("repos_indexed", json.int(list.length(profile.repos))),
                    #("total_stars", json.int(profile.total_stars)),
                    #("top_languages", json.array(profile.top_languages, json.string)),
                    #("message", json.string(
                      "Profile loaded. Run github_generate_embeddings to enable semantic search."
                    )),
                  ])

                  success_result(json.to_string(result))
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Handle github_search tool
pub fn handle_github_search(args: json.Json) -> ToolResult {
  // Parse arguments
  case json_get_string(args, "query") {
    Error(_) -> error_result("Missing required parameter: query")
    Ok(query) -> {
      let username = json_get_optional_string(args, "username")

      let doc_type_str = json_get_string(args, "doc_type")
        |> option.from_result
        |> option.unwrap("all")

      let _doc_type = case doc_type_str {
        "all" -> None
        other -> {
          case github_types.doc_type_from_string(other) {
            Ok(dt) -> Some(dt)
            Error(_) -> None
          }
        }
      }

      let limit = json_get_int(args, "limit")
        |> option.unwrap(10)

      // Get database connection
      case postgres.get_global_pool() {
        None -> error_result("Database not connected")
        Some(pool) -> {
          // First try keyword search (faster, no embedding needed)
          case github_db.keyword_search(pool, query, username, limit) {
            Error(e) -> error_result("Search failed: " <> github_types.error_to_string(e))
            Ok(results) -> {
              case results {
                [] -> {
                  // No results, return empty
                  let result = json.object([
                    #("success", json.bool(True)),
                    #("query", json.string(query)),
                    #("results", json.array([], fn(_) { json.null() })),
                    #("count", json.int(0)),
                    #("message", json.string(
                      "No results found. Make sure the profile is indexed using github_load_profile."
                    )),
                  ])
                  success_result(json.to_string(result))
                }
                _ -> {
                  // Format results
                  let formatted = format_search_results(results)
                  success_result(json.to_string(formatted))
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Handle github_profile_summary tool
pub fn handle_github_profile_summary(args: json.Json) -> ToolResult {
  case json_get_string(args, "username") {
    Error(_) -> error_result("Missing required parameter: username")
    Ok(username) -> {
      case postgres.get_global_pool() {
        None -> error_result("Database not connected")
        Some(pool) -> {
          case github_db.get_stats(pool, username) {
            Error(e) -> error_result("Failed to get stats: " <> github_types.error_to_string(e))
            Ok(#(total, with_embeddings, total_stars)) -> {
              // Get profile document for additional info
              case github_db.get_documents_by_type(pool, username, github_types.ProfileDoc) {
                Error(e) -> error_result("Failed to get profile: " <> github_types.error_to_string(e))
                Ok(docs) -> {
                  let profile_content = case docs {
                    [doc, ..] -> doc.content
                    [] -> "Profile not indexed yet."
                  }

                  let result = json.object([
                    #("success", json.bool(True)),
                    #("username", json.string(username)),
                    #("stats", json.object([
                      #("total_documents", json.int(total)),
                      #("documents_with_embeddings", json.int(with_embeddings)),
                      #("total_stars", json.int(total_stars)),
                      #("semantic_search_ready", json.bool(with_embeddings > 0)),
                    ])),
                    #("profile_summary", json.string(profile_content)),
                  ])

                  success_result(json.to_string(result))
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Handle github_generate_embeddings tool
pub fn handle_github_generate_embeddings(args: json.Json) -> ToolResult {
  let limit = json_get_int(args, "limit")
    |> option.unwrap(100)

  case postgres.get_global_pool() {
    None -> error_result("Database not connected")
    Some(pool) -> {
      // Get documents without embeddings
      case github_db.get_documents_without_embeddings(pool, limit) {
        Error(e) -> error_result("Failed to get documents: " <> github_types.error_to_string(e))
        Ok(docs) -> {
          case docs {
            [] -> {
              let result = json.object([
                #("success", json.bool(True)),
                #("processed", json.int(0)),
                #("message", json.string("All documents already have embeddings.")),
              ])
              success_result(json.to_string(result))
            }
            _ -> {
              // Generate embeddings for each document
              let processed = generate_embeddings_batch(pool, docs)

              let result = json.object([
                #("success", json.bool(True)),
                #("processed", json.int(processed)),
                #("total_pending", json.int(list.length(docs))),
                #("message", json.string(
                  "Generated embeddings for " <> int.to_string(processed) <> " documents."
                )),
              ])

              success_result(json.to_string(result))
            }
          }
        }
      }
    }
  }
}

// ============================================================
// Helper Functions
// ============================================================

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

fn json_get_optional_string(j: json.Json, key: String) -> Option(String) {
  case json_get_string(j, key) {
    Ok(v) -> Some(v)
    Error(_) -> None
  }
}

fn json_get_int(j: json.Json, key: String) -> Option(Int) {
  let decoder = {
    use v <- decode.field(key, decode.int)
    decode.success(v)
  }
  let json_str = json.to_string(j)
  case json.parse(json_str, decoder) {
    Ok(v) -> Some(v)
    Error(_) -> None
  }
}

fn json_get_bool(j: json.Json, key: String) -> Option(Bool) {
  let decoder = {
    use v <- decode.field(key, decode.bool)
    decode.success(v)
  }
  let json_str = json.to_string(j)
  case json.parse(json_str, decoder) {
    Ok(v) -> Some(v)
    Error(_) -> None
  }
}

fn format_search_results(results: List(github_types.GitHubSearchResult)) -> json.Json {
  json.object([
    #("success", json.bool(True)),
    #("count", json.int(list.length(results))),
    #(
      "results",
      json.array(results, fn(r) {
        json.object([
          #("title", json.string(r.document.title)),
          #("type", json.string(github_types.doc_type_to_string(r.document.doc_type))),
          #("content", json.string(truncate_content(r.document.content, 500))),
          #("source_url", json.string(r.document.source_url)),
          #("stars", json.int(r.document.stars)),
          #("score", json.float(r.score)),
          #(
            "language",
            case r.document.language {
              Some(l) -> json.string(l)
              None -> json.null()
            },
          ),
          #("topics", json.array(r.document.topics, json.string)),
        ])
      }),
    ),
  ])
}

fn truncate_content(content: String, max_length: Int) -> String {
  case string.length(content) > max_length {
    True -> string.slice(content, 0, max_length) <> "..."
    False -> content
  }
}

fn generate_embeddings_batch(
  pool: postgres.DbPool,
  docs: List(github_types.GitHubDocument),
) -> Int {
  list.fold(docs, 0, fn(count, doc) {
    case doc.id {
      Some(id) -> {
        // Create text for embedding
        let text = doc.title <> "\n" <> doc.content

        // Generate embedding using OpenAI
        case embedding.embed_text(text, embedding.openai_config()) {
          Ok(emb) -> {
            case github_db.update_embedding(pool, id, emb) {
              Ok(_) -> count + 1
              Error(_) -> count
            }
          }
          Error(_) -> count
        }
      }
      None -> count
    }
  })
}
