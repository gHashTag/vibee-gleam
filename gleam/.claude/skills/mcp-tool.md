---
name: mcp-tool
description: Generate and manage MCP tools with handlers and tests
---

# /mcp-tool - MCP Tool Generator

Scaffold new MCP tools with handlers, tests, and documentation.

## Usage

```bash
/mcp-tool create github_search --category GitHub
/mcp-tool list                  # List all tools by category
/mcp-tool list --category AI    # List AI tools only
/mcp-tool show ai_bfl           # Show tool definition
/mcp-tool test ai_*             # Test tools matching pattern
```

## Tool Categories

| Category | Description | Count |
|----------|-------------|-------|
| Telegram | Messaging, dialogs, history | ~20 |
| Auth | Sessions, authorization | ~10 |
| RAG | Search, embeddings, context | ~8 |
| AI | Image, video, TTS, STT | ~15 |
| Payments | Invoices, subscriptions | ~8 |
| P2P | Escrow, orders | ~6 |
| Tasks | Task management | ~8 |
| Bot | Bot testing, analysis | ~5 |
| GitHub | Profile, repos (planned) | ~3 |

## Create New Tool

When user runs `/mcp-tool create <name> --category <cat>`:

1. **Add to Registry** (`src/vibee/mcp/tools.gleam`)
```gleam
Tool(
  name: "github_search",
  description: "Search GitHub documents with hybrid search",
  category: CategoryGitHub,
  inputs: [
    ToolInput(name: "query", type_: "string", required: True, description: "Search query"),
    ToolInput(name: "limit", type_: "number", required: False, description: "Max results"),
  ],
),
```

2. **Add Handler**
```gleam
fn handle_github_search(args: Dynamic, session_id: Option(String)) -> ToolResult {
  // Parse args
  let query = dynamic.field("query", dynamic.string)(args)

  // Implement logic
  case query {
    Ok(q) -> {
      // Search implementation
      protocol.success_result(json.object([
        #("results", json.array([], json.string)),
      ]))
    }
    Error(_) -> protocol.error_result("query is required")
  }
}
```

3. **Add Test** (`test/mcp_tools_test.gleam`)
```gleam
pub fn github_search_test() {
  let args = dynamic.from([#("query", "gleam")])
  let result = tools.handle_tool("github_search", args, None)
  should.be_ok(result)
}
```

4. **Update CLAUDE.md**
```markdown
**GitHub:**
- `github_search` - Search GitHub documents
```

## Key Files

| File | Purpose |
|------|---------|
| `src/vibee/mcp/tools.gleam` | Tool registry (6,940 lines) |
| `src/vibee/mcp/protocol.gleam` | Result types |
| `test/mcp_tools_test.gleam` | Tool tests |
| `CLAUDE.md` | Documentation |

## Handler Pattern

```gleam
fn handle_tool(name: String, args: Dynamic, session_id: Option(String)) -> ToolResult {
  case name {
    "my_tool" -> handle_my_tool(args, session_id)
    _ -> protocol.error_result("Unknown tool")
  }
}
```
