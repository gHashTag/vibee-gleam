---
name: test
description: Run Gleam tests with rich output and suggestions
---

# /test - Gleam Test Runner

Run Gleam tests with formatted output and failure analysis.

## Usage

```bash
/test                    # Run all tests
/test mcp_tools          # Run specific module
/test --module agent     # Run tests matching "agent"
/test --verbose          # Show full output
```

## Test Modules

| Module | Description |
|--------|-------------|
| mcp_tools_test | MCP tool handlers |
| agent_persistence_test | Agent state persistence |
| p2p_test | P2P escrow system |
| workflows_test | Workflow orchestration |
| session_test | Session management |
| earning_test | Token economics |

## Implementation

When user runs `/test`:

1. **Run Tests**
```bash
cd /Users/playra/vibee/gleam

# All tests
gleam test

# Specific module
gleam test --module mcp_tools_test
```

2. **Parse Output**
- Count passed/failed tests
- Extract failure messages
- Identify error patterns

3. **Format Report**
```
## Test Results

Passed: 42
Failed: 2
Skipped: 0

### Failures

1. `test_session_create` in session_test.gleam:45
   Error: Expected Some(...) but got None

2. `test_p2p_order` in p2p_test.gleam:112
   Error: Assertion failed
```

4. **Suggest Fixes**
- Common error patterns
- Related documentation
- Similar passing tests

## Common Errors

| Error | Suggestion |
|-------|------------|
| `init() not called` | Add init() to FFI function |
| `Connection unavailable` | Check DATABASE_URL (use direct, not pooler) |
| `Session not found` | Run auth flow first |
