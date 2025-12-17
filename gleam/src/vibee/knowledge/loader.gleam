// Markdown/Text file loader for Knowledge Base
// Parses markdown files into Document structures

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import gleam/result
import simplifile
import vibee/knowledge/types.{
  type Document, type KnowledgeError, Document, FileNotFound, ParseError,
}

/// Load a single markdown file as a Document
pub fn load_markdown(path: String) -> Result(Document, KnowledgeError) {
  case simplifile.read(path) {
    Ok(content) -> {
      let doc = parse_markdown(path, content)
      Ok(doc)
    }
    Error(_) -> Error(FileNotFound(path))
  }
}

/// Load multiple files from a list of paths
pub fn load_files(paths: List(String)) -> List(Document) {
  paths
  |> list.filter_map(fn(path) {
    case load_markdown(path) {
      Ok(doc) -> Ok(doc)
      Error(_) -> Error(Nil)
    }
  })
}

/// Parse markdown content into a Document
fn parse_markdown(path: String, content: String) -> Document {
  let title = extract_title(content)
  let tags = extract_tags(content)
  let clean_content = clean_markdown(content)
  let id = path_to_id(path)

  Document(
    id: id,
    title: title,
    content: clean_content,
    source: path,
    tags: tags,
  )
}

/// Extract title from markdown (first # heading)
fn extract_title(content: String) -> String {
  content
  |> string.split("\n")
  |> list.find(fn(line) { string.starts_with(line, "# ") })
  |> result.map(fn(line) { string.drop_start(line, 2) })
  |> result.map(string.trim)
  |> result.unwrap("Untitled")
}

/// Extract tags from markdown (lines starting with tags: or #tag)
fn extract_tags(content: String) -> List(String) {
  let lines = string.split(content, "\n")

  // Look for tags: line in frontmatter style
  let yaml_tags =
    lines
    |> list.find(fn(line) {
      string.starts_with(string.trim(line), "tags:")
    })
    |> result.map(fn(line) {
      line
      |> string.drop_start(string.length("tags:"))
      |> string.trim()
      |> string.split(",")
      |> list.map(string.trim)
      |> list.filter(fn(s) { string.length(s) > 0 })
    })
    |> result.unwrap([])

  // Look for hashtags in content
  let hashtags =
    lines
    |> list.flat_map(fn(line) { extract_hashtags(line) })

  list.append(yaml_tags, hashtags)
  |> list.unique()
}

/// Extract #hashtags from a line
fn extract_hashtags(line: String) -> List(String) {
  line
  |> string.split(" ")
  |> list.filter_map(fn(word) {
    case string.starts_with(word, "#") && string.length(word) > 1 {
      True -> {
        let tag = string.drop_start(word, 1)
        // Skip markdown headers
        case string.starts_with(tag, "#") {
          True -> Error(Nil)
          False -> Ok(string.lowercase(tag))
        }
      }
      False -> Error(Nil)
    }
  })
}

/// Clean markdown for plain text search
fn clean_markdown(content: String) -> String {
  content
  |> remove_code_blocks()
  |> remove_markdown_syntax()
  |> normalize_whitespace()
}

/// Remove code blocks
fn remove_code_blocks(content: String) -> String {
  // Simple approach: remove everything between ``` marks
  content
  |> string.split("```")
  |> list.index_map(fn(part, idx) {
    // Keep odd-indexed parts (outside code blocks)
    case idx % 2 == 0 {
      True -> part
      False -> ""
    }
  })
  |> string.join("")
}

/// Remove common markdown syntax
fn remove_markdown_syntax(content: String) -> String {
  content
  |> string.replace("**", "")
  |> string.replace("__", "")
  |> string.replace("*", "")
  |> string.replace("_", " ")
  |> string.replace("`", "")
  |> string.replace("[", "")
  |> string.replace("]", "")
  |> string.replace("(", "")
  |> string.replace(")", "")
}

/// Normalize whitespace
fn normalize_whitespace(content: String) -> String {
  content
  |> string.split("\n")
  |> list.map(string.trim)
  |> list.filter(fn(s) { string.length(s) > 0 })
  |> string.join("\n")
}

/// Convert file path to document ID
fn path_to_id(path: String) -> String {
  path
  |> string.split("/")
  |> list.last()
  |> result.unwrap("unknown")
  |> string.replace(".md", "")
  |> string.replace(".txt", "")
  |> string.lowercase()
  |> string.replace(" ", "-")
}

/// List all markdown files in a directory (recursive)
pub fn list_markdown_files(dir: String) -> List(String) {
  case simplifile.read_directory(dir) {
    Ok(entries) -> {
      entries
      |> list.flat_map(fn(entry) {
        let full_path = dir <> "/" <> entry
        case simplifile.is_directory(full_path) {
          Ok(True) -> list_markdown_files(full_path)
          _ -> {
            case is_markdown_file(entry) {
              True -> [full_path]
              False -> []
            }
          }
        }
      })
    }
    Error(_) -> []
  }
}

/// Check if file is a markdown file
fn is_markdown_file(filename: String) -> Bool {
  string.ends_with(filename, ".md") || string.ends_with(filename, ".txt")
}

/// Load all markdown files from a directory
pub fn load_directory(dir: String) -> List(Document) {
  dir
  |> list_markdown_files()
  |> load_files()
}
