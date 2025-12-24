# Contributing to VIBEE

Thank you for your interest in contributing to VIBEE!

## Development Setup

### Prerequisites

- Gleam 1.0+ ([installation guide](https://gleam.run/getting-started/installing/))
- Erlang/OTP 26+
- Go 1.21+
- PostgreSQL 15+ with pgvector extension

### Getting Started

```bash
# Clone the repository
git clone https://github.com/gHashTag/vibee
cd vibee

# Setup Gleam project
cd gleam
cp .env.example .env
# Edit .env with your development credentials

gleam deps download
gleam build
gleam test

# Setup Go bridge (optional)
cd ../telegram-bridge
go mod download
go build ./cmd/server
```

## Code Style

### Gleam

- Run `gleam format src test` before committing
- Follow existing naming conventions (snake_case for functions, PascalCase for types)
- Keep modules focused and under 500 lines when possible
- Add doc comments for public functions

### Go

- Run `go fmt ./...` before committing
- Follow standard Go conventions
- Use descriptive error messages

## Pull Request Process

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/your-feature`)
3. Make your changes
4. Run tests (`gleam test` and/or `go test ./...`)
5. Format code (`gleam format src test`)
6. Commit with a descriptive message
7. Push to your fork
8. Open a Pull Request

### PR Guidelines

- Keep PRs focused on a single feature or fix
- Include tests for new functionality
- Update documentation if needed
- Reference any related issues

## Testing

```bash
# Run all Gleam tests
cd gleam && gleam test

# Run specific test module
gleam test --module mcp_tools_test

# Run Go tests
cd telegram-bridge && go test -v ./...
```

## Project Structure

- `gleam/src/vibee/mcp/` - MCP protocol implementation
- `gleam/src/vibee/agent/` - OTP actors and supervision
- `gleam/src/vibee/api/` - HTTP handlers
- `telegram-bridge/` - Go MTProto service

## Questions?

Open an issue or start a discussion on GitHub.
