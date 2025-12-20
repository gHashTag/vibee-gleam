// GitHub Types for VIBEE Digital Clone Knowledge Base
// Used for loading and indexing GitHub profile data

import gleam/option.{type Option}

/// GitHub user profile information
pub type GitHubUser {
  GitHubUser(
    login: String,
    id: Int,
    name: Option(String),
    bio: Option(String),
    company: Option(String),
    location: Option(String),
    blog: Option(String),
    email: Option(String),
    twitter_username: Option(String),
    public_repos: Int,
    public_gists: Int,
    followers: Int,
    following: Int,
    created_at: String,
    updated_at: String,
    avatar_url: String,
    html_url: String,
  )
}

/// GitHub repository information
pub type GitHubRepo {
  GitHubRepo(
    id: Int,
    name: String,
    full_name: String,
    description: Option(String),
    language: Option(String),
    stargazers_count: Int,
    forks_count: Int,
    watchers_count: Int,
    open_issues_count: Int,
    topics: List(String),
    is_fork: Bool,
    is_private: Bool,
    is_archived: Bool,
    html_url: String,
    clone_url: String,
    created_at: String,
    updated_at: String,
    pushed_at: Option(String),
    default_branch: String,
    license_name: Option(String),
    readme_content: Option(String),
    languages: List(#(String, Int)),
  )
}

/// Complete GitHub profile with all data
pub type GitHubProfile {
  GitHubProfile(
    user: GitHubUser,
    repos: List(GitHubRepo),
    fetched_at: String,
    total_stars: Int,
    top_languages: List(String),
  )
}

/// Document type for indexing (matches github_documents table)
pub type GitHubDocument {
  GitHubDocument(
    id: Option(Int),
    username: String,
    doc_type: DocumentType,
    repo_name: Option(String),
    title: String,
    content: String,
    source_url: String,
    tags: List(String),
    stars: Int,
    language: Option(String),
    topics: List(String),
    fetched_at: String,
  )
}

/// Type of GitHub document
pub type DocumentType {
  ProfileDoc
  RepoDoc
  ReadmeDoc
  LanguagesDoc
}

/// Convert DocumentType to string for database
pub fn doc_type_to_string(doc_type: DocumentType) -> String {
  case doc_type {
    ProfileDoc -> "profile"
    RepoDoc -> "repo"
    ReadmeDoc -> "readme"
    LanguagesDoc -> "languages"
  }
}

/// Parse DocumentType from string
pub fn doc_type_from_string(s: String) -> Result(DocumentType, Nil) {
  case s {
    "profile" -> Ok(ProfileDoc)
    "repo" -> Ok(RepoDoc)
    "readme" -> Ok(ReadmeDoc)
    "languages" -> Ok(LanguagesDoc)
    _ -> Error(Nil)
  }
}

/// Request for loading GitHub profile
pub type LoadProfileRequest {
  LoadProfileRequest(
    username: String,
    include_readmes: Bool,
    max_repos: Int,
    min_stars: Int,
  )
}

/// Default load profile request
pub fn default_load_request(username: String) -> LoadProfileRequest {
  LoadProfileRequest(
    username: username,
    include_readmes: True,
    max_repos: 50,
    min_stars: 0,
  )
}

/// GitHub search result
pub type GitHubSearchResult {
  GitHubSearchResult(
    document: GitHubDocument,
    score: Float,
    matched_terms: List(String),
  )
}

/// Error types for GitHub operations
pub type GitHubError {
  ApiError(status: Int, message: String)
  NetworkError(message: String)
  ParseError(message: String)
  RateLimitError(reset_at: String)
  NotFoundError(resource: String)
  DatabaseError(message: String)
}

/// Convert error to string for display
pub fn error_to_string(error: GitHubError) -> String {
  case error {
    ApiError(status, msg) ->
      "GitHub API error (" <> int_to_string(status) <> "): " <> msg
    NetworkError(msg) -> "Network error: " <> msg
    ParseError(msg) -> "Parse error: " <> msg
    RateLimitError(reset) -> "Rate limited. Resets at: " <> reset
    NotFoundError(resource) -> "Not found: " <> resource
    DatabaseError(msg) -> "Database error: " <> msg
  }
}

fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ -> do_int_to_string(n, "")
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        _ -> "9"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}
