// Package botapi provides Telegram Bot API client for handling callbacks
// This works alongside the MTProto client (gotd/td) for hybrid user-bot mode
package botapi

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"sync"

	tgbotapi "github.com/go-telegram-bot-api/telegram-bot-api/v5"
)

// BotClient wraps the Telegram Bot API for callback handling
type BotClient struct {
	api           *tgbotapi.BotAPI
	token         string
	webhookURL    string
	gleamURL      string // URL to forward callbacks to Gleam
	callbackQueue chan *tgbotapi.CallbackQuery
	mu            sync.RWMutex
}

// Config holds bot configuration
type Config struct {
	Token      string
	WebhookURL string // e.g., https://vibee-telegram-bridge.fly.dev/api/v1/bot/webhook
	GleamURL   string // e.g., https://vibee-mcp.fly.dev/api/v1/bot/callback
}

// NewBotClient creates a new Bot API client
func NewBotClient(cfg Config) (*BotClient, error) {
	if cfg.Token == "" {
		return nil, fmt.Errorf("bot token is required")
	}

	bot, err := tgbotapi.NewBotAPI(cfg.Token)
	if err != nil {
		return nil, fmt.Errorf("failed to create bot: %w", err)
	}

	log.Printf("[BotAPI] Authorized as @%s", bot.Self.UserName)

	return &BotClient{
		api:           bot,
		token:         cfg.Token,
		webhookURL:    cfg.WebhookURL,
		gleamURL:      cfg.GleamURL,
		callbackQueue: make(chan *tgbotapi.CallbackQuery, 100),
	}, nil
}

// SetWebhook configures the webhook for receiving updates
func (c *BotClient) SetWebhook(webhookURL string) error {
	wh, err := tgbotapi.NewWebhook(webhookURL)
	if err != nil {
		return fmt.Errorf("failed to create webhook: %w", err)
	}

	_, err = c.api.Request(wh)
	if err != nil {
		return fmt.Errorf("failed to set webhook: %w", err)
	}

	log.Printf("[BotAPI] Webhook set to: %s", webhookURL)
	return nil
}

// GetWebhookInfo returns current webhook configuration
func (c *BotClient) GetWebhookInfo() (tgbotapi.WebhookInfo, error) {
	return c.api.GetWebhookInfo()
}

// SendMessage sends a plain text message via Bot API
func (c *BotClient) SendMessage(chatID int64, text string) (int, error) {
	msg := tgbotapi.NewMessage(chatID, text)
	msg.ParseMode = "Markdown"

	sent, err := c.api.Send(msg)
	if err != nil {
		return 0, fmt.Errorf("send message: %w", err)
	}

	log.Printf("[BotAPI] Sent message to %d, msg_id=%d", chatID, sent.MessageID)
	return sent.MessageID, nil
}

// InlineButton represents a single inline keyboard button
type InlineButton struct {
	Text         string `json:"text"`
	CallbackData string `json:"callback_data"`
}

// SendWithButtons sends a message with inline keyboard
func (c *BotClient) SendWithButtons(chatID int64, text string, buttons [][]InlineButton) (int, error) {
	msg := tgbotapi.NewMessage(chatID, text)
	msg.ParseMode = "Markdown"

	// Convert to tgbotapi keyboard format
	var keyboard [][]tgbotapi.InlineKeyboardButton
	for _, row := range buttons {
		var keyboardRow []tgbotapi.InlineKeyboardButton
		for _, btn := range row {
			keyboardRow = append(keyboardRow, tgbotapi.NewInlineKeyboardButtonData(btn.Text, btn.CallbackData))
		}
		keyboard = append(keyboard, keyboardRow)
	}

	msg.ReplyMarkup = tgbotapi.NewInlineKeyboardMarkup(keyboard...)

	sent, err := c.api.Send(msg)
	if err != nil {
		return 0, fmt.Errorf("send with buttons: %w", err)
	}

	log.Printf("[BotAPI] Sent message with buttons to %d, msg_id=%d", chatID, sent.MessageID)
	return sent.MessageID, nil
}

// AnswerCallback answers a callback query
func (c *BotClient) AnswerCallback(queryID string, text string, showAlert bool) error {
	callback := tgbotapi.NewCallback(queryID, text)
	callback.ShowAlert = showAlert

	_, err := c.api.Request(callback)
	if err != nil {
		return fmt.Errorf("answer callback: %w", err)
	}

	log.Printf("[BotAPI] Answered callback %s", queryID)
	return nil
}

// EditMessageText edits a message text
func (c *BotClient) EditMessageText(chatID int64, messageID int, text string) error {
	edit := tgbotapi.NewEditMessageText(chatID, messageID, text)
	edit.ParseMode = "Markdown"

	_, err := c.api.Send(edit)
	if err != nil {
		return fmt.Errorf("edit message: %w", err)
	}

	return nil
}

// EditMessageButtons edits message with new buttons
func (c *BotClient) EditMessageButtons(chatID int64, messageID int, text string, buttons [][]InlineButton) error {
	edit := tgbotapi.NewEditMessageText(chatID, messageID, text)
	edit.ParseMode = "Markdown"

	var keyboard [][]tgbotapi.InlineKeyboardButton
	for _, row := range buttons {
		var keyboardRow []tgbotapi.InlineKeyboardButton
		for _, btn := range row {
			keyboardRow = append(keyboardRow, tgbotapi.NewInlineKeyboardButtonData(btn.Text, btn.CallbackData))
		}
		keyboard = append(keyboard, keyboardRow)
	}

	markup := tgbotapi.NewInlineKeyboardMarkup(keyboard...)
	edit.ReplyMarkup = &markup

	_, err := c.api.Send(edit)
	if err != nil {
		return fmt.Errorf("edit message buttons: %w", err)
	}

	return nil
}

// CallbackData represents callback query data for forwarding
type CallbackData struct {
	QueryID   string `json:"query_id"`
	ChatID    int64  `json:"chat_id"`
	UserID    int64  `json:"user_id"`
	Username  string `json:"username"`
	Data      string `json:"data"`
	MessageID int    `json:"message_id"`
}

// HandleWebhook processes incoming webhook updates
// Returns the callback data if it's a callback query, nil otherwise
func (c *BotClient) HandleWebhook(r *http.Request) (*CallbackData, error) {
	var update tgbotapi.Update
	if err := json.NewDecoder(r.Body).Decode(&update); err != nil {
		return nil, fmt.Errorf("decode update: %w", err)
	}

	// Only handle callback queries
	if update.CallbackQuery == nil {
		log.Printf("[BotAPI] Ignoring non-callback update")
		return nil, nil
	}

	cq := update.CallbackQuery
	log.Printf("[BotAPI] Received callback: query_id=%s, data=%s, from=%d", cq.ID, cq.Data, cq.From.ID)

	// Extract callback data
	data := &CallbackData{
		QueryID:  cq.ID,
		UserID:   cq.From.ID,
		Username: cq.From.UserName,
		Data:     cq.Data,
	}

	if cq.Message != nil {
		data.ChatID = cq.Message.Chat.ID
		data.MessageID = cq.Message.MessageID
	}

	return data, nil
}

// ForwardCallbackToGleam forwards callback data to Gleam service
func (c *BotClient) ForwardCallbackToGleam(ctx context.Context, data *CallbackData) error {
	if c.gleamURL == "" {
		return fmt.Errorf("gleam URL not configured")
	}

	// TODO: Implement HTTP POST to Gleam
	// For now, log the callback
	log.Printf("[BotAPI] Would forward callback to Gleam: %+v", data)

	return nil
}

// GetBotUsername returns the bot's username
func (c *BotClient) GetBotUsername() string {
	return c.api.Self.UserName
}

// GetBotID returns the bot's user ID
func (c *BotClient) GetBotID() int64 {
	return c.api.Self.ID
}
