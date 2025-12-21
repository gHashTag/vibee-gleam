package config

import (
	"fmt"
	"os"
	"strings"
)

// Config holds all configuration for the telegram bridge
type Config struct {
	// Server config
	Port string

	// Telegram MTProto config
	AppID   int
	AppHash string

	// Telegram Bot API config (for inline buttons)
	BotToken      string
	BotWebhookURL string
	GleamURL      string

	// Session storage
	SessionDir string

	// PostgreSQL (optional, for session persistence)
	DatabaseURL string

	// CORS - allowed origins (comma-separated in env, or "*" for dev mode)
	AllowedOrigins []string
}

// Load loads configuration from environment variables
func Load() (*Config, error) {
	cfg := &Config{
		Port:       getEnv("PORT", "8081"), // Default to 8081 to not conflict with Gleam on 8080
		SessionDir: getEnv("SESSION_DIR", "./sessions"),
	}

	// Telegram credentials are optional at startup
	// They can be provided via API when connecting
	appIDStr := os.Getenv("TELEGRAM_APP_ID")
	if appIDStr != "" {
		var appID int
		_, err := fmt.Sscanf(appIDStr, "%d", &appID)
		if err != nil {
			return nil, fmt.Errorf("invalid TELEGRAM_APP_ID: %w", err)
		}
		cfg.AppID = appID
	}

	cfg.AppHash = os.Getenv("TELEGRAM_APP_HASH")
	cfg.DatabaseURL = os.Getenv("DATABASE_URL")

	// Bot API config (optional - for inline buttons support)
	cfg.BotToken = os.Getenv("TELEGRAM_BOT_TOKEN")
	cfg.BotWebhookURL = os.Getenv("TELEGRAM_BOT_WEBHOOK_URL") // e.g., https://vibee-telegram-bridge.fly.dev/api/v1/bot/webhook
	cfg.GleamURL = getEnv("GLEAM_CALLBACK_URL", "https://vibee-mcp.fly.dev/api/v1/bot/callback")

	// Parse allowed origins
	// CORS_ALLOWED_ORIGINS can be comma-separated list or "*" for dev mode
	// Default includes Fly.io domains for production
	originsStr := getEnv("CORS_ALLOWED_ORIGINS", "http://localhost:8080,http://localhost:3000,https://vibee-mcp.fly.dev,https://vibee-eliza-999-prod-v2.fly.dev,https://vibee-telegram-bridge.fly.dev")
	if originsStr == "*" {
		cfg.AllowedOrigins = []string{"*"}
	} else {
		origins := strings.Split(originsStr, ",")
		for i, o := range origins {
			origins[i] = strings.TrimSpace(o)
		}
		cfg.AllowedOrigins = origins
	}

	return cfg, nil
}

// IsOriginAllowed checks if the given origin is in the allowed list
func (c *Config) IsOriginAllowed(origin string) bool {
	if len(c.AllowedOrigins) == 0 {
		return false
	}
	// If "*" is in allowed origins, allow all
	for _, allowed := range c.AllowedOrigins {
		if allowed == "*" {
			return true
		}
		if allowed == origin {
			return true
		}
	}
	return false
}

func getEnv(key, defaultValue string) string {
	if value := os.Getenv(key); value != "" {
		return value
	}
	return defaultValue
}
