# VIBEE - Gleam/BEAM Agent Framework
# Makefile для управления проектом

.PHONY: all build test clean run dev docker-up docker-down gleam-build go-build help

# Default target
all: build

# === BUILD ===

build: gleam-build go-build
	@echo "✅ Build complete"

gleam-build:
	@echo "🔨 Building Gleam project..."
	cd gleam && gleam build

go-build:
	@echo "🔨 Building Go telegram-bridge..."
	cd telegram-bridge && go build -o bin/telegram-bridge ./cmd/server

# === TEST ===

test: gleam-test go-test
	@echo "✅ All tests passed"

gleam-test:
	@echo "🧪 Running Gleam tests..."
	cd gleam && gleam test

go-test:
	@echo "🧪 Running Go tests..."
	cd telegram-bridge && go test -v ./...

# === RUN ===

run: run-gleam

run-gleam:
	@echo "🚀 Running Gleam application..."
	cd gleam && gleam run

run-bridge:
	@echo "🚀 Running Telegram Bridge..."
	cd telegram-bridge && go run ./cmd/server

# === DEVELOPMENT ===

dev:
	@echo "🔄 Starting development environment..."
	$(MAKE) docker-up
	@echo "📡 PostgreSQL running on localhost:5432"
	@echo "🔧 Run 'make run-bridge' in one terminal"
	@echo "🔧 Run 'make run-gleam' in another terminal"

# === DOCKER ===

docker-up:
	@echo "🐳 Starting Docker services..."
	docker-compose up -d postgres
	@echo "⏳ Waiting for PostgreSQL..."
	@sleep 3
	@echo "✅ Services ready"

docker-down:
	@echo "🛑 Stopping Docker services..."
	docker-compose down

docker-build:
	@echo "🐳 Building Docker images..."
	docker-compose build

docker-logs:
	docker-compose logs -f

# === DATABASE ===

db-shell:
	@echo "📊 Connecting to PostgreSQL..."
	docker-compose exec postgres psql -U vibee -d vibee

db-reset:
	@echo "⚠️  Resetting database..."
	docker-compose down -v
	docker-compose up -d postgres
	@sleep 3
	@echo "✅ Database reset complete"

# === CLEAN ===

clean: clean-gleam clean-go
	@echo "🧹 Clean complete"

clean-gleam:
	@echo "🧹 Cleaning Gleam build..."
	cd gleam && rm -rf build

clean-go:
	@echo "🧹 Cleaning Go build..."
	cd telegram-bridge && rm -rf bin

clean-all: clean
	@echo "🧹 Cleaning Docker volumes..."
	docker-compose down -v

# === FORMAT & LINT ===

fmt: fmt-gleam fmt-go
	@echo "✨ Format complete"

fmt-gleam:
	@echo "✨ Formatting Gleam code..."
	cd gleam && gleam format src test

fmt-go:
	@echo "✨ Formatting Go code..."
	cd telegram-bridge && go fmt ./...

lint-go:
	@echo "🔍 Linting Go code..."
	cd telegram-bridge && go vet ./...

# === DEPS ===

deps: deps-gleam deps-go
	@echo "📦 Dependencies installed"

deps-gleam:
	@echo "📦 Installing Gleam dependencies..."
	cd gleam && gleam deps download

deps-go:
	@echo "📦 Installing Go dependencies..."
	cd telegram-bridge && go mod tidy

# === HELP ===

help:
	@echo "VIBEE - Gleam/BEAM Agent Framework"
	@echo ""
	@echo "Usage: make [target]"
	@echo ""
	@echo "Build:"
	@echo "  build         Build all projects"
	@echo "  gleam-build   Build Gleam project only"
	@echo "  go-build      Build Go telegram-bridge only"
	@echo ""
	@echo "Test:"
	@echo "  test          Run all tests"
	@echo "  gleam-test    Run Gleam tests only"
	@echo "  go-test       Run Go tests only"
	@echo ""
	@echo "Run:"
	@echo "  run           Run Gleam application"
	@echo "  run-gleam     Run Gleam application"
	@echo "  run-bridge    Run Telegram Bridge"
	@echo ""
	@echo "Development:"
	@echo "  dev           Start dev environment (PostgreSQL)"
	@echo "  docker-up     Start Docker services"
	@echo "  docker-down   Stop Docker services"
	@echo "  docker-build  Build Docker images"
	@echo "  docker-logs   Follow Docker logs"
	@echo ""
	@echo "Database:"
	@echo "  db-shell      Connect to PostgreSQL shell"
	@echo "  db-reset      Reset database (drops all data)"
	@echo ""
	@echo "Clean:"
	@echo "  clean         Clean build artifacts"
	@echo "  clean-all     Clean including Docker volumes"
	@echo ""
	@echo "Code Quality:"
	@echo "  fmt           Format all code"
	@echo "  lint-go       Lint Go code"
	@echo ""
	@echo "Dependencies:"
	@echo "  deps          Install all dependencies"
