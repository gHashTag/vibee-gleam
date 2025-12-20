// GitHub Profile to Documents Loader
// Converts GitHub profile data to indexed documents for RAG

import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/github/types.{
  type DocumentType, type GitHubDocument, type GitHubProfile, type GitHubRepo,
  type GitHubUser, GitHubDocument, LanguagesDoc, ProfileDoc, ReadmeDoc, RepoDoc,
}

// ============================================================
// Profile to Documents
// ============================================================

/// Convert a complete GitHub profile to a list of documents
pub fn profile_to_documents(profile: GitHubProfile) -> List(GitHubDocument) {
  let user_doc = user_to_document(profile.user, profile.top_languages)

  let repo_docs = list.flat_map(profile.repos, fn(repo) {
    repo_to_documents(profile.user.login, repo)
  })

  [user_doc, ..repo_docs]
}

/// Convert GitHub user to profile document
fn user_to_document(
  user: GitHubUser,
  top_languages: List(String),
) -> GitHubDocument {
  let content = build_profile_content(user, top_languages)

  GitHubDocument(
    id: None,
    username: user.login,
    doc_type: ProfileDoc,
    repo_name: None,
    title: user_title(user),
    content: content,
    source_url: user.html_url,
    tags: build_profile_tags(user, top_languages),
    stars: 0,
    language: None,
    topics: [],
    fetched_at: "",
  )
}

/// Build profile content for embedding
fn build_profile_content(
  user: GitHubUser,
  top_languages: List(String),
) -> String {
  let parts = [
    "GitHub Profile: " <> user.login,
    option_to_line("Name: ", user.name),
    option_to_line("Bio: ", user.bio),
    option_to_line("Company: ", user.company),
    option_to_line("Location: ", user.location),
    option_to_line("Website: ", user.blog),
    option_to_line("Twitter: @", user.twitter_username),
    "Public repositories: " <> int.to_string(user.public_repos),
    "Followers: " <> int.to_string(user.followers),
    "Following: " <> int.to_string(user.following),
    "GitHub since: " <> string.slice(user.created_at, 0, 10),
    case top_languages {
      [] -> ""
      langs -> "Top languages: " <> string.join(langs, ", ")
    },
  ]

  parts
  |> list.filter(fn(s) { s != "" })
  |> string.join("\n")
}

/// Build profile tags
fn build_profile_tags(
  user: GitHubUser,
  top_languages: List(String),
) -> List(String) {
  let base_tags = ["github", "profile", user.login]

  let lang_tags = list.map(top_languages, string.lowercase)

  let company_tag = case user.company {
    Some(c) -> [string.lowercase(clean_company_name(c))]
    None -> []
  }

  list.flatten([base_tags, lang_tags, company_tag])
}

/// Clean company name (remove @ prefix)
fn clean_company_name(name: String) -> String {
  case string.starts_with(name, "@") {
    True -> string.drop_start(name, 1)
    False -> name
  }
}

/// User title for document
fn user_title(user: GitHubUser) -> String {
  case user.name {
    Some(name) -> name <> " (@" <> user.login <> ")"
    None -> "@" <> user.login
  }
}

// ============================================================
// Repository to Documents
// ============================================================

/// Convert repository to documents (repo + readme)
fn repo_to_documents(
  username: String,
  repo: GitHubRepo,
) -> List(GitHubDocument) {
  let repo_doc = repo_to_document(username, repo)

  let readme_doc = case repo.readme_content {
    Some(content) -> [readme_to_document(username, repo, content)]
    None -> []
  }

  [repo_doc, ..readme_doc]
}

/// Convert repository to document
fn repo_to_document(username: String, repo: GitHubRepo) -> GitHubDocument {
  let content = build_repo_content(repo)

  GitHubDocument(
    id: None,
    username: username,
    doc_type: RepoDoc,
    repo_name: Some(repo.name),
    title: repo.name,
    content: content,
    source_url: repo.html_url,
    tags: build_repo_tags(username, repo),
    stars: repo.stargazers_count,
    language: repo.language,
    topics: repo.topics,
    fetched_at: "",
  )
}

/// Build repository content
fn build_repo_content(repo: GitHubRepo) -> String {
  let parts = [
    "Repository: " <> repo.full_name,
    option_to_line("Description: ", repo.description),
    option_to_line("Language: ", repo.language),
    "Stars: " <> int.to_string(repo.stargazers_count),
    "Forks: " <> int.to_string(repo.forks_count),
    "Open issues: " <> int.to_string(repo.open_issues_count),
    case repo.topics {
      [] -> ""
      topics -> "Topics: " <> string.join(topics, ", ")
    },
    option_to_line("License: ", repo.license_name),
    "Created: " <> string.slice(repo.created_at, 0, 10),
    "Last updated: " <> string.slice(repo.updated_at, 0, 10),
    case repo.pushed_at {
      Some(p) -> "Last push: " <> string.slice(p, 0, 10)
      None -> ""
    },
    case repo.is_archived {
      True -> "Status: Archived"
      False -> ""
    },
  ]

  parts
  |> list.filter(fn(s) { s != "" })
  |> string.join("\n")
}

/// Build repository tags
fn build_repo_tags(username: String, repo: GitHubRepo) -> List(String) {
  let base_tags = ["github", "repo", username, repo.name]

  let lang_tag = case repo.language {
    Some(l) -> [string.lowercase(l)]
    None -> []
  }

  let topic_tags = list.map(repo.topics, string.lowercase)

  list.flatten([base_tags, lang_tag, topic_tags])
}

// ============================================================
// README to Document
// ============================================================

/// Convert README content to document
fn readme_to_document(
  username: String,
  repo: GitHubRepo,
  content: String,
) -> GitHubDocument {
  let cleaned_content = clean_readme(content)

  GitHubDocument(
    id: None,
    username: username,
    doc_type: ReadmeDoc,
    repo_name: Some(repo.name),
    title: repo.name <> " - README",
    content: cleaned_content,
    source_url: repo.html_url <> "#readme",
    tags: ["github", "readme", username, repo.name],
    stars: repo.stargazers_count,
    language: repo.language,
    topics: repo.topics,
    fetched_at: "",
  )
}

/// Clean README content for better embedding
fn clean_readme(content: String) -> String {
  content
  |> remove_badges
  |> remove_html_comments
  |> normalize_whitespace
}

/// Remove badge images from README
fn remove_badges(content: String) -> String {
  // Remove common badge patterns like [![...](https://...)]
  // This is a simplified version - in production would use regex
  content
  |> string.replace("[![", "[")
}

/// Remove HTML comments
fn remove_html_comments(content: String) -> String {
  // Simplified - in production would use regex
  content
}

/// Normalize whitespace
fn normalize_whitespace(content: String) -> String {
  content
  |> string.replace("\r\n", "\n")
  |> string.replace("\t", "  ")
  |> fn(s) {
    // Remove multiple consecutive blank lines
    case string.contains(s, "\n\n\n") {
      True -> normalize_whitespace(string.replace(s, "\n\n\n", "\n\n"))
      False -> s
    }
  }
  |> string.trim
}

// ============================================================
// Languages Document
// ============================================================

/// Create a languages summary document
pub fn languages_to_document(
  username: String,
  repos: List(GitHubRepo),
) -> GitHubDocument {
  let lang_counts = aggregate_languages(repos)

  let content =
    "Programming Languages used by @"
    <> username
    <> ":\n\n"
    <> {
      lang_counts
      |> list.map(fn(pair) {
        pair.0 <> ": " <> int.to_string(pair.1) <> " repositories"
      })
      |> string.join("\n")
    }

  GitHubDocument(
    id: None,
    username: username,
    doc_type: LanguagesDoc,
    repo_name: None,
    title: username <> " - Programming Languages",
    content: content,
    source_url: "https://github.com/" <> username,
    tags: [
      "github",
      "languages",
      username,
      ..list.map(lang_counts, fn(p) { string.lowercase(p.0) })
    ],
    stars: 0,
    language: None,
    topics: [],
    fetched_at: "",
  )
}

/// Aggregate language usage across repos
fn aggregate_languages(repos: List(GitHubRepo)) -> List(#(String, Int)) {
  repos
  |> list.filter_map(fn(r) { option.to_result(r.language, Nil) })
  |> list.group(fn(lang) { lang })
  |> dict.to_list
  |> list.map(fn(pair: #(String, List(String))) { #(pair.0, list.length(pair.1)) })
  |> list.sort(fn(a, b) { int.compare(b.1, a.1) })
}

// ============================================================
// Helpers
// ============================================================

fn option_to_line(prefix: String, opt: Option(String)) -> String {
  case opt {
    Some(value) ->
      case string.is_empty(string.trim(value)) {
        True -> ""
        False -> prefix <> value
      }
    None -> ""
  }
}

// ============================================================
// Document Formatting for Context
// ============================================================

/// Format documents as context for LLM
pub fn format_documents_for_context(
  docs: List(GitHubDocument),
) -> String {
  docs
  |> list.map(format_document)
  |> string.join("\n\n---\n\n")
}

/// Format single document for context
fn format_document(doc: GitHubDocument) -> String {
  let type_label = case doc.doc_type {
    ProfileDoc -> "GitHub Profile"
    RepoDoc -> "Repository"
    ReadmeDoc -> "README"
    LanguagesDoc -> "Languages Summary"
  }

  "[" <> type_label <> "] " <> doc.title <> "\n" <> doc.content
}

/// Format search results for context
pub fn format_search_results_for_context(
  results: List(types.GitHubSearchResult),
) -> String {
  results
  |> list.map(fn(r) { format_document(r.document) })
  |> string.join("\n\n---\n\n")
}
