// GitHub API Client for VIBEE
// Fetches user profiles, repositories, READMEs, and languages

import gleam/dict
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/ai/client as http_client
import vibee/github/types.{
  type GitHubError, type GitHubProfile, type GitHubRepo, type GitHubUser,
  type LoadProfileRequest, ApiError, GitHubProfile, GitHubRepo, GitHubUser,
  NetworkError, NotFoundError, ParseError, RateLimitError,
}
import vibee/mcp/config

// ============================================================
// Configuration
// ============================================================

const github_api_base = "https://api.github.com"

fn get_github_token() -> Option(String) {
  case config.get_env_or("GITHUB_TOKEN", "") {
    "" -> None
    token -> Some(token)
  }
}

/// Build headers for GitHub API
fn github_headers() -> List(#(String, String)) {
  let base_headers = [
    #("Accept", "application/vnd.github.v3+json"),
    #("User-Agent", "VIBEE-Digital-Clone"),
  ]

  case get_github_token() {
    Some(token) -> [#("Authorization", "Bearer " <> token), ..base_headers]
    None -> base_headers
  }
}

// ============================================================
// API Methods
// ============================================================

/// Fetch GitHub user profile
pub fn fetch_user(username: String) -> Result(GitHubUser, GitHubError) {
  let url = github_api_base <> "/users/" <> username

  let req =
    http_client.Request(
      url: url,
      method: "GET",
      headers: github_headers(),
      body: "",
    )

  case http_client.execute_json(req) {
    Ok(body) -> parse_user(body)
    Error(http_client.HttpError(404, _)) ->
      Error(NotFoundError("User: " <> username))
    Error(http_client.HttpError(403, body)) -> {
      case string.contains(body, "rate limit") {
        True -> Error(RateLimitError("Rate limit exceeded"))
        False -> Error(ApiError(403, body))
      }
    }
    Error(http_client.HttpError(status, body)) -> Error(ApiError(status, body))
    Error(http_client.NetworkError(msg)) -> Error(NetworkError(msg))
    Error(_) -> Error(NetworkError("Unknown error"))
  }
}

/// Fetch user repositories
pub fn fetch_repos(
  username: String,
  page: Int,
  per_page: Int,
) -> Result(List(GitHubRepo), GitHubError) {
  let url =
    github_api_base
    <> "/users/"
    <> username
    <> "/repos?sort=pushed&direction=desc&per_page="
    <> int.to_string(per_page)
    <> "&page="
    <> int.to_string(page)

  let req =
    http_client.Request(
      url: url,
      method: "GET",
      headers: github_headers(),
      body: "",
    )

  case http_client.execute_json(req) {
    Ok(body) -> parse_repos(body)
    Error(http_client.HttpError(status, body)) -> Error(ApiError(status, body))
    Error(http_client.NetworkError(msg)) -> Error(NetworkError(msg))
    Error(_) -> Error(NetworkError("Unknown error"))
  }
}

/// Fetch repository README content
pub fn fetch_readme(
  owner: String,
  repo: String,
) -> Result(String, GitHubError) {
  let url =
    github_api_base <> "/repos/" <> owner <> "/" <> repo <> "/readme"

  let req =
    http_client.Request(
      url: url,
      method: "GET",
      headers: [
        #("Accept", "application/vnd.github.v3.raw"),
        #("User-Agent", "VIBEE-Digital-Clone"),
        ..case get_github_token() {
          Some(token) -> [#("Authorization", "Bearer " <> token)]
          None -> []
        }
      ],
      body: "",
    )

  case http_client.execute_json(req) {
    Ok(content) -> Ok(content)
    Error(http_client.HttpError(404, _)) -> Error(NotFoundError("README"))
    Error(http_client.HttpError(status, body)) -> Error(ApiError(status, body))
    Error(http_client.NetworkError(msg)) -> Error(NetworkError(msg))
    Error(_) -> Error(NetworkError("Unknown error"))
  }
}

/// Fetch repository languages
pub fn fetch_languages(
  owner: String,
  repo: String,
) -> Result(List(#(String, Int)), GitHubError) {
  let url =
    github_api_base <> "/repos/" <> owner <> "/" <> repo <> "/languages"

  let req =
    http_client.Request(
      url: url,
      method: "GET",
      headers: github_headers(),
      body: "",
    )

  case http_client.execute_json(req) {
    Ok(body) -> parse_languages(body)
    Error(http_client.HttpError(status, body)) -> Error(ApiError(status, body))
    Error(http_client.NetworkError(msg)) -> Error(NetworkError(msg))
    Error(_) -> Error(NetworkError("Unknown error"))
  }
}

/// Load complete GitHub profile with all data
pub fn load_profile(
  request: LoadProfileRequest,
) -> Result(GitHubProfile, GitHubError) {
  // 1. Fetch user
  use user <- result.try(fetch_user(request.username))

  // 2. Fetch repos (paginated)
  use repos <- result.try(fetch_all_repos(
    request.username,
    request.max_repos,
    request.min_stars,
  ))

  // 3. Optionally fetch READMEs for repos
  let repos_with_readmes = case request.include_readmes {
    True -> enrich_repos_with_readmes(request.username, repos)
    False -> repos
  }

  // 4. Calculate aggregates
  let total_stars =
    list.fold(repos_with_readmes, 0, fn(acc, repo) {
      acc + repo.stargazers_count
    })

  let top_languages = extract_top_languages(repos_with_readmes, 10)

  Ok(GitHubProfile(
    user: user,
    repos: repos_with_readmes,
    fetched_at: get_current_timestamp(),
    total_stars: total_stars,
    top_languages: top_languages,
  ))
}

// ============================================================
// Helper Functions
// ============================================================

/// Fetch all repos with pagination
fn fetch_all_repos(
  username: String,
  max_repos: Int,
  min_stars: Int,
) -> Result(List(GitHubRepo), GitHubError) {
  fetch_repos_paginated(username, 1, 100, max_repos, min_stars, [])
}

fn fetch_repos_paginated(
  username: String,
  page: Int,
  per_page: Int,
  max_repos: Int,
  min_stars: Int,
  acc: List(GitHubRepo),
) -> Result(List(GitHubRepo), GitHubError) {
  case list.length(acc) >= max_repos {
    True -> Ok(list.take(acc, max_repos))
    False -> {
      case fetch_repos(username, page, per_page) {
        Ok(repos) -> {
          case repos {
            [] -> Ok(acc)
            _ -> {
              let filtered =
                repos
                |> list.filter(fn(r) { r.stargazers_count >= min_stars })
                |> list.filter(fn(r) { !r.is_fork })

              let new_acc = list.append(acc, filtered)

              case list.length(repos) < per_page {
                True -> Ok(list.take(new_acc, max_repos))
                False ->
                  fetch_repos_paginated(
                    username,
                    page + 1,
                    per_page,
                    max_repos,
                    min_stars,
                    new_acc,
                  )
              }
            }
          }
        }
        Error(e) -> Error(e)
      }
    }
  }
}

/// Enrich repos with README content
fn enrich_repos_with_readmes(
  username: String,
  repos: List(GitHubRepo),
) -> List(GitHubRepo) {
  list.map(repos, fn(repo) {
    case fetch_readme(username, repo.name) {
      Ok(content) ->
        GitHubRepo(..repo, readme_content: Some(truncate_readme(content)))
      Error(_) -> repo
    }
  })
}

/// Truncate README to reasonable size
fn truncate_readme(content: String) -> String {
  let max_length = 10_000
  case string.length(content) > max_length {
    True -> string.slice(content, 0, max_length) <> "\n...[truncated]"
    False -> content
  }
}

/// Extract top languages from repos
fn extract_top_languages(repos: List(GitHubRepo), limit: Int) -> List(String) {
  repos
  |> list.filter_map(fn(r) { option.to_result(r.language, Nil) })
  |> list.group(fn(lang) { lang })
  |> dict.to_list
  |> list.map(fn(pair: #(String, List(String))) { #(pair.0, list.length(pair.1)) })
  |> list.sort(fn(a, b) { int.compare(b.1, a.1) })
  |> list.take(limit)
  |> list.map(fn(pair) { pair.0 })
}

// ============================================================
// JSON Parsing with new decode API
// ============================================================

fn parse_user(body: String) -> Result(GitHubUser, GitHubError) {
  let decoder = user_decoder()
  case json.parse(body, decoder) {
    Ok(user) -> Ok(user)
    Error(_) -> Error(ParseError("Failed to parse user JSON"))
  }
}

fn user_decoder() -> decode.Decoder(GitHubUser) {
  use login <- decode.field("login", decode.string)
  use id <- decode.field("id", decode.int)
  use name <- decode.optional_field("name", None, decode.optional(decode.string))
  use bio <- decode.optional_field("bio", None, decode.optional(decode.string))
  use company <- decode.optional_field("company", None, decode.optional(decode.string))
  use location <- decode.optional_field("location", None, decode.optional(decode.string))
  use blog <- decode.optional_field("blog", None, decode.optional(decode.string))
  use email <- decode.optional_field("email", None, decode.optional(decode.string))
  use twitter_username <- decode.optional_field("twitter_username", None, decode.optional(decode.string))
  use public_repos <- decode.field("public_repos", decode.int)
  use public_gists <- decode.field("public_gists", decode.int)
  use followers <- decode.field("followers", decode.int)
  use following <- decode.field("following", decode.int)
  use created_at <- decode.field("created_at", decode.string)
  use updated_at <- decode.field("updated_at", decode.string)
  use avatar_url <- decode.field("avatar_url", decode.string)
  use html_url <- decode.field("html_url", decode.string)

  decode.success(GitHubUser(
    login: login,
    id: id,
    name: name,
    bio: bio,
    company: company,
    location: location,
    blog: blog,
    email: email,
    twitter_username: twitter_username,
    public_repos: public_repos,
    public_gists: public_gists,
    followers: followers,
    following: following,
    created_at: created_at,
    updated_at: updated_at,
    avatar_url: avatar_url,
    html_url: html_url,
  ))
}

fn parse_repos(body: String) -> Result(List(GitHubRepo), GitHubError) {
  let decoder = decode.list(repo_decoder())
  case json.parse(body, decoder) {
    Ok(repos) -> Ok(repos)
    Error(_) -> Error(ParseError("Failed to parse repos JSON"))
  }
}

fn repo_decoder() -> decode.Decoder(GitHubRepo) {
  use id <- decode.field("id", decode.int)
  use name <- decode.field("name", decode.string)
  use full_name <- decode.field("full_name", decode.string)
  use description <- decode.optional_field("description", None, decode.optional(decode.string))
  use language <- decode.optional_field("language", None, decode.optional(decode.string))
  use stars <- decode.field("stargazers_count", decode.int)
  use forks <- decode.field("forks_count", decode.int)
  use watchers <- decode.field("watchers_count", decode.int)
  use issues <- decode.field("open_issues_count", decode.int)
  use topics <- decode.optional_field("topics", [], decode.list(decode.string))
  use is_fork <- decode.field("fork", decode.bool)
  use is_private <- decode.field("private", decode.bool)
  use is_archived <- decode.field("archived", decode.bool)
  use html_url <- decode.field("html_url", decode.string)
  use clone_url <- decode.field("clone_url", decode.string)
  use created_at <- decode.field("created_at", decode.string)
  use updated_at <- decode.field("updated_at", decode.string)
  use pushed_at <- decode.optional_field("pushed_at", None, decode.optional(decode.string))
  use default_branch <- decode.field("default_branch", decode.string)
  use license <- decode.optional_field("license", None, decode.optional(license_name_decoder()))

  decode.success(GitHubRepo(
    id: id,
    name: name,
    full_name: full_name,
    description: description,
    language: language,
    stargazers_count: stars,
    forks_count: forks,
    watchers_count: watchers,
    open_issues_count: issues,
    topics: topics,
    is_fork: is_fork,
    is_private: is_private,
    is_archived: is_archived,
    html_url: html_url,
    clone_url: clone_url,
    created_at: created_at,
    updated_at: updated_at,
    pushed_at: pushed_at,
    default_branch: default_branch,
    license_name: license,
    readme_content: None,
    languages: [],
  ))
}

fn license_name_decoder() -> decode.Decoder(String) {
  use name <- decode.field("name", decode.string)
  decode.success(name)
}

fn parse_languages(body: String) -> Result(List(#(String, Int)), GitHubError) {
  // Parse as dict using decode.dict
  let decoder = decode.dict(decode.string, decode.int)
  case json.parse(body, decoder) {
    Ok(language_dict) -> {
      Ok(dict.to_list(language_dict))
    }
    Error(_) -> Error(ParseError("Failed to parse languages JSON"))
  }
}

// ============================================================
// Utilities
// ============================================================

@external(erlang, "calendar", "universal_time")
fn erlang_universal_time() -> #(#(Int, Int, Int), #(Int, Int, Int))

fn get_current_timestamp() -> String {
  let #(#(year, month, day), #(hour, min, sec)) = erlang_universal_time()
  int.to_string(year)
  <> "-"
  <> pad_zero(month)
  <> "-"
  <> pad_zero(day)
  <> "T"
  <> pad_zero(hour)
  <> ":"
  <> pad_zero(min)
  <> ":"
  <> pad_zero(sec)
  <> "Z"
}

fn pad_zero(n: Int) -> String {
  case n < 10 {
    True -> "0" <> int.to_string(n)
    False -> int.to_string(n)
  }
}
