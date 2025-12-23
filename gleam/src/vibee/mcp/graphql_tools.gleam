// GraphQL MCP Tools - Test GraphQL API via MCP
// Created: 22.12.2025

import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/list
import gleam/string
import vibee/mcp/types.{type Tool, type ToolResult, TextContent, Tool, ToolResult}

/// GraphQL API base URL
fn graphql_url() -> String {
  "https://vibee-mcp.fly.dev/graphql"
}

// =============================================================================
// Tool Definitions
// =============================================================================

pub fn graphql_query_tool() -> Tool {
  Tool(
    name: "graphql_query",
    description: "Execute GraphQL query. Use for reading data: leads, triggerConfigs, funnelStats, forwardStats, leadForwards.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("query", json.object([
          #("type", json.string("string")),
          #("description", json.string("GraphQL query string")),
        ])),
        #("variables", json.object([
          #("type", json.string("object")),
          #("description", json.string("Optional query variables")),
        ])),
      ])),
      #("required", json.array([json.string("query")], fn(x) { x })),
    ]),
  )
}

pub fn graphql_mutation_tool() -> Tool {
  Tool(
    name: "graphql_mutation",
    description: "Execute GraphQL mutation. Use for modifying data: updateLeadStatus, updateLeadFunnelStage, assignLead, addLeadNote.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([
        #("mutation", json.object([
          #("type", json.string("string")),
          #("description", json.string("GraphQL mutation string")),
        ])),
        #("variables", json.object([
          #("type", json.string("object")),
          #("description", json.string("Mutation variables")),
        ])),
      ])),
      #("required", json.array([json.string("mutation")], fn(x) { x })),
    ]),
  )
}

pub fn graphql_introspection_tool() -> Tool {
  Tool(
    name: "graphql_introspection",
    description: "Introspect GraphQL schema. Returns all types, queries, mutations, and subscriptions available in the API.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
    ]),
  )
}

pub fn graphql_test_all_tool() -> Tool {
  Tool(
    name: "graphql_test_all",
    description: "Run all GraphQL API tests: queries, mutations, introspection. Returns comprehensive test results.",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
    ]),
  )
}

/// Get all GraphQL tools
pub fn get_all_tools() -> List(Tool) {
  [
    graphql_query_tool(),
    graphql_mutation_tool(),
    graphql_introspection_tool(),
    graphql_test_all_tool(),
  ]
}

// =============================================================================
// Tool Handlers
// =============================================================================

pub fn handle_graphql_query(args: json.Json) -> ToolResult {
  case json_get_string(args, "query") {
    Ok(query) -> {
      execute_graphql(query)
    }
    Error(_) -> error_result("Missing required parameter: query")
  }
}

pub fn handle_graphql_mutation(args: json.Json) -> ToolResult {
  case json_get_string(args, "mutation") {
    Ok(mutation) -> {
      execute_graphql(mutation)
    }
    Error(_) -> error_result("Missing required parameter: mutation")
  }
}

pub fn handle_graphql_introspection(_args: json.Json) -> ToolResult {
  let introspection_query = "
    query IntrospectionQuery {
      __schema {
        queryType { name }
        mutationType { name }
        subscriptionType { name }
        types {
          kind
          name
          description
          fields(includeDeprecated: true) {
            name
            description
            args {
              name
              description
              type {
                kind
                name
                ofType { kind name ofType { kind name } }
              }
            }
            type {
              kind
              name
              ofType { kind name ofType { kind name } }
            }
          }
          enumValues(includeDeprecated: true) {
            name
            description
          }
        }
      }
    }
  "
  execute_graphql(introspection_query)
}

pub fn handle_graphql_test_all(_args: json.Json) -> ToolResult {
  let tests = [
    // Test 1: Get leads
    #("leads_query", "query { leads(limit: 3) { id telegramUserId username status funnelStage } }"),

    // Test 2: Get funnel stats
    #("funnel_stats", "query { funnelStats { awareness interest consideration intent evaluation purchase total } }"),

    // Test 3: Get trigger configs
    #("trigger_configs", "query { triggerConfigs { chatId chatName isActive triggers } }"),

    // Test 4: Get lead forwards
    #("lead_forwards", "query { leadForwards(limit: 3) { id qualityScore intent urgency status } }"),

    // Test 5: Schema introspection
    #("introspection", "query { __schema { queryType { name } mutationType { name } subscriptionType { name } } }"),
  ]

  let results = run_tests(tests, [])
  let summary = build_test_summary(results)

  success_result(summary)
}

// =============================================================================
// Helpers
// =============================================================================

fn execute_graphql(query: String) -> ToolResult {
  let body = json.to_string(json.object([
    #("query", json.string(query)),
  ]))

  let url = graphql_url()

  case request.to(url) {
    Ok(req) -> {
      let req = request.set_method(req, http.Post)
        |> request.set_header("Content-Type", "application/json")
        |> request.set_body(body)

      case httpc.send(req) {
        Ok(response) -> {
          success_result(response.body)
        }
        Error(err) -> {
          error_result("HTTP error: " <> string.inspect(err))
        }
      }
    }
    Error(_) -> error_result("Invalid GraphQL URL: " <> url)
  }
}

fn run_tests(tests: List(#(String, String)), results: List(#(String, Bool, String))) -> List(#(String, Bool, String)) {
  case tests {
    [] -> list.reverse(results)
    [#(name, query), ..rest] -> {
      let result = execute_graphql(query)
      let passed = !result.is_error
      let response = case result.content {
        [TextContent(text)] -> text
        _ -> "No response"
      }
      run_tests(rest, [#(name, passed, response), ..results])
    }
  }
}

fn build_test_summary(results: List(#(String, Bool, String))) -> String {
  let passed_count = count_passed(results, 0)
  let total = list.length(results)

  let header = "GraphQL API Test Results\n" <>
    "========================\n" <>
    "Passed: " <> int.to_string(passed_count) <> "/" <> int.to_string(total) <> "\n\n"

  let details = format_results(results, "")

  header <> details
}

fn count_passed(results: List(#(String, Bool, String)), acc: Int) -> Int {
  case results {
    [] -> acc
    [#(_, passed, _), ..rest] -> count_passed(rest, case passed { True -> acc + 1 False -> acc })
  }
}

fn format_results(results: List(#(String, Bool, String)), acc: String) -> String {
  case results {
    [] -> acc
    [#(name, passed, response), ..rest] -> {
      let status = case passed { True -> "PASS" False -> "FAIL" }
      let preview = string.slice(response, 0, 200)
      let line = "[" <> status <> "] " <> name <> "\n" <>
        "Response: " <> preview <> "...\n\n"
      format_results(rest, acc <> line)
    }
  }
}

// =============================================================================
// JSON Helpers
// =============================================================================

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
