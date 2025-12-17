package api

import (
	"context"
	"database/sql"
	"encoding/json"
	"log"
	"net/http"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/gorilla/websocket"
	"github.com/gotd/td/session"
	"github.com/vibee/telegram-bridge/internal/config"
	"github.com/vibee/telegram-bridge/internal/telegram"
)

// Router handles HTTP requests
type Router struct {
	cfg      *config.Config
	mux      *http.ServeMux
	db       *sql.DB
	clients  map[string]*telegram.Client
	upgrader websocket.Upgrader
	wsHub    *WSHub
	mu       sync.RWMutex
}

// NewRouter creates a new HTTP router
func NewRouter(cfg *config.Config, db *sql.DB) *Router {
	r := &Router{
		cfg:     cfg,
		mux:     http.NewServeMux(),
		db:      db,
		clients: make(map[string]*telegram.Client),
		upgrader: websocket.Upgrader{
			CheckOrigin: func(r *http.Request) bool {
				origin := r.Header.Get("Origin")
				return cfg.IsOriginAllowed(origin)
			},
		},
		wsHub: NewWSHub(),
	}

	// Start WebSocket hub
	go r.wsHub.Run()

	// Setup routes first so server can start immediately
	r.setupRoutes()

	// Restore sessions in background (don't block server startup)
	go r.restoreSessions()

	return r
}

// restoreSessions loads existing session files and reconnects to Telegram
func (r *Router) restoreSessions() {
	if r.cfg.AppID == 0 || r.cfg.AppHash == "" {
		log.Println("⚠️ TELEGRAM_APP_ID or TELEGRAM_APP_HASH not set, skipping session restoration")
		return
	}

	// Read session directory
	entries, err := os.ReadDir(r.cfg.SessionDir)
	if err != nil {
		log.Printf("⚠️ Failed to read session directory %s: %v", r.cfg.SessionDir, err)
		return
	}

	restored := 0
	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}

		name := entry.Name()
		if !strings.HasSuffix(name, ".session") {
			continue
		}

		// Extract session ID from filename (e.g., "sess_xxx.session" -> "sess_xxx")
		sessionID := strings.TrimSuffix(name, ".session")
		sessionPath := r.cfg.SessionDir + "/" + name

		log.Printf("🔄 Restoring session: %s", sessionID)

		// Create session storage - use gotd's FileStorage directly
		sessionStorage := &session.FileStorage{Path: sessionPath}

		// Create Telegram client
		client, err := telegram.NewClient(r.cfg.AppID, r.cfg.AppHash, sessionStorage)
		if err != nil {
			log.Printf("❌ Failed to create client for session %s: %v", sessionID, err)
			continue
		}

		// Connect to Telegram - use background context for long-running connection
		// Only timeout the initial connection establishment, not the ongoing connection
		ctx := context.Background()
		if err := client.Connect(ctx); err != nil {
			log.Printf("❌ Failed to connect session %s: %v", sessionID, err)
			continue
		}
		// Note: do NOT cancel the context - it keeps the MTProto connection alive

		// Store client
		r.mu.Lock()
		r.clients[sessionID] = client
		r.mu.Unlock()

		// Start forwarding updates
		go r.forwardUpdates(sessionID, client)

		if client.IsAuthorized() {
			log.Printf("✅ Restored session %s (authorized)", sessionID)
		} else {
			log.Printf("⚠️ Restored session %s (not authorized)", sessionID)
		}
		restored++
	}

	log.Printf("📊 Session restoration complete: %d sessions restored", restored)
}

func (r *Router) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	// Check and set CORS headers based on allowed origins
	origin := req.Header.Get("Origin")
	if r.cfg.IsOriginAllowed(origin) {
		w.Header().Set("Access-Control-Allow-Origin", origin)
		w.Header().Set("Access-Control-Allow-Credentials", "true")
	}
	w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization, X-Session-ID")

	if req.Method == "OPTIONS" {
		w.WriteHeader(http.StatusOK)
		return
	}

	r.mux.ServeHTTP(w, req)
}

func (r *Router) setupRoutes() {
	// Health check
	r.mux.HandleFunc("/health", r.handleHealth)

	// API v1 routes
	r.mux.HandleFunc("/api/v1/connect", r.handleConnect)
	r.mux.HandleFunc("/api/v1/auth/status", r.handleAuthStatus)
	r.mux.HandleFunc("/api/v1/auth/phone", r.handleAuthPhone)
	r.mux.HandleFunc("/api/v1/auth/code", r.handleAuthCode)
	r.mux.HandleFunc("/api/v1/auth/2fa", r.handleAuth2FA)
	r.mux.HandleFunc("/api/v1/me", r.handleGetMe)
	r.mux.HandleFunc("/api/v1/dialogs", r.handleGetDialogs)
	r.mux.HandleFunc("/api/v1/history/", r.handleGetHistory)
	r.mux.HandleFunc("/api/v1/send", r.handleSendMessage)
	r.mux.HandleFunc("/api/v1/send/photo", r.handleSendPhoto)
	r.mux.HandleFunc("/api/v1/resolve", r.handleResolveUsername)

	// Media endpoints
	r.mux.HandleFunc("/api/v1/download", r.handleDownloadMedia)
	r.mux.HandleFunc("/api/v1/media/", r.handleGetMediaInfo)

	// WebSocket for updates
	r.mux.HandleFunc("/api/v1/updates", r.handleWebSocket)
}

// Response helpers
func respondJSON(w http.ResponseWriter, status int, data interface{}) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	if err := json.NewEncoder(w).Encode(data); err != nil {
		log.Printf("Error encoding JSON: %v", err)
	}
}

func respondError(w http.ResponseWriter, status int, message string) {
	respondJSON(w, status, map[string]string{"error": message})
}

// getSessionID extracts session ID from X-Session-ID header (preferred) or query parameter (deprecated)
// SECURITY: Using header is more secure as it's not logged in access logs or browser history
func getSessionID(req *http.Request) string {
	// Prefer header (secure)
	sessionID := req.Header.Get("X-Session-ID")
	if sessionID != "" {
		return sessionID
	}
	// Fallback to query parameter (deprecated, for backwards compatibility)
	return req.URL.Query().Get("session_id")
}

// Handlers

func (r *Router) handleHealth(w http.ResponseWriter, req *http.Request) {
	respondJSON(w, http.StatusOK, map[string]interface{}{
		"status":  "ok",
		"service": "telegram-bridge",
		"version": "0.1.0",
	})
}

func (r *Router) handleAuthStatus(w http.ResponseWriter, req *http.Request) {
	if req.Method != http.MethodGet {
		respondError(w, http.StatusMethodNotAllowed, "Method not allowed")
		return
	}

	sessionID := req.Header.Get("X-Session-ID")

	// If no session provided, return list of active sessions
	if sessionID == "" {
		r.mu.RLock()
		sessions := make([]map[string]interface{}, 0, len(r.clients))
		for id, client := range r.clients {
			sessions = append(sessions, map[string]interface{}{
				"session_id": id,
				"authorized": client.IsAuthorized(),
			})
		}
		r.mu.RUnlock()

		respondJSON(w, http.StatusOK, map[string]interface{}{
			"status":        "ok",
			"sessions":      sessions,
			"total_sessions": len(sessions),
		})
		return
	}

	// Check specific session
	client := r.getClient(sessionID)
	if client == nil {
		respondJSON(w, http.StatusOK, map[string]interface{}{
			"status":     "not_found",
			"session_id": sessionID,
			"authorized": false,
			"message":    "Session not found. Use /api/v1/connect to create a new session.",
		})
		return
	}

	respondJSON(w, http.StatusOK, map[string]interface{}{
		"status":     "ok",
		"session_id": sessionID,
		"authorized": client.IsAuthorized(),
	})
}

// ConnectRequest is the request body for /connect
type ConnectRequest struct {
	AppID   int    `json:"app_id"`
	AppHash string `json:"app_hash"`
	Phone   string `json:"phone,omitempty"`
}

func (r *Router) handleConnect(w http.ResponseWriter, req *http.Request) {
	if req.Method != http.MethodPost {
		respondError(w, http.StatusMethodNotAllowed, "Method not allowed")
		return
	}

	var body ConnectRequest
	if err := json.NewDecoder(req.Body).Decode(&body); err != nil {
		respondError(w, http.StatusBadRequest, "Invalid JSON body")
		return
	}

	if body.AppID == 0 || body.AppHash == "" {
		respondError(w, http.StatusBadRequest, "app_id and app_hash are required")
		return
	}

	// Generate session ID
	sessionID := generateSessionID()

	// Create session storage - use gotd's FileStorage directly for proper persistence
	sessionStorage := &session.FileStorage{Path: r.cfg.SessionDir + "/" + sessionID + ".session"}

	// Create Telegram client
	client, err := telegram.NewClient(body.AppID, body.AppHash, sessionStorage)
	if err != nil {
		respondError(w, http.StatusInternalServerError, "Failed to create client: "+err.Error())
		return
	}

	// Connect to Telegram - use Background context so connection persists after handler returns
	// The connection goroutine needs to stay alive for subsequent API calls
	if err := client.Connect(context.Background()); err != nil {
		respondError(w, http.StatusInternalServerError, "Failed to connect: "+err.Error())
		return
	}

	// Store client
	r.mu.Lock()
	r.clients[sessionID] = client
	r.mu.Unlock()

	// Start forwarding updates to WebSocket
	go r.forwardUpdates(sessionID, client)

	log.Printf("Connect request: app_id=%d, session=%s", body.AppID, sessionID)

	respondJSON(w, http.StatusOK, map[string]interface{}{
		"status":      "connected",
		"session_id":  sessionID,
		"authorized":  client.IsAuthorized(),
		"message":     "Connection established. Use /auth/phone to start authentication if not authorized.",
	})
}

// AuthPhoneRequest is the request for /auth/phone
type AuthPhoneRequest struct {
	Phone string `json:"phone"`
}

func (r *Router) handleAuthPhone(w http.ResponseWriter, req *http.Request) {
	if req.Method != http.MethodPost {
		respondError(w, http.StatusMethodNotAllowed, "Method not allowed")
		return
	}

	sessionID := req.Header.Get("X-Session-ID")
	client := r.getClient(sessionID)
	if client == nil {
		respondError(w, http.StatusUnauthorized, "Invalid or missing session")
		return
	}

	var body AuthPhoneRequest
	if err := json.NewDecoder(req.Body).Decode(&body); err != nil {
		respondError(w, http.StatusBadRequest, "Invalid JSON body")
		return
	}

	if body.Phone == "" {
		respondError(w, http.StatusBadRequest, "phone is required")
		return
	}

	log.Printf("Sending code to phone: %s", body.Phone)

	// Retry logic for DC migration - Telegram may require reconnection to different data center
	var codeHash string
	var err error
	maxRetries := 2
	for attempt := 1; attempt <= maxRetries; attempt++ {
		// Use longer timeout per attempt to allow DC migration to complete
		ctx, cancel := context.WithTimeout(context.Background(), 90*time.Second)

		codeHash, err = client.SendCode(ctx, body.Phone)
		cancel()

		if err == nil {
			break
		}

		log.Printf("SendCode attempt %d/%d error for phone %s: %v", attempt, maxRetries, body.Phone, err)

		// Check if it's a DC migration error - if so, wait and retry
		if strings.Contains(err.Error(), "migrate") && attempt < maxRetries {
			log.Printf("DC migration detected, waiting 5 seconds before retry...")
			time.Sleep(5 * time.Second)
			continue
		}

		// For other errors, don't retry
		break
	}

	if err != nil {
		log.Printf("SendCode final error for phone %s: %v", body.Phone, err)
		respondError(w, http.StatusInternalServerError, "Failed to send code: "+err.Error())
		return
	}

	log.Printf("Auth phone request success: phone=%s", body.Phone)

	respondJSON(w, http.StatusOK, map[string]interface{}{
		"status":    "code_sent",
		"code_hash": codeHash,
		"message":   "Verification code sent to phone",
	})
}

// AuthCodeRequest is the request for /auth/code
type AuthCodeRequest struct {
	Code string `json:"code"`
}

func (r *Router) handleAuthCode(w http.ResponseWriter, req *http.Request) {
	if req.Method != http.MethodPost {
		respondError(w, http.StatusMethodNotAllowed, "Method not allowed")
		return
	}

	sessionID := req.Header.Get("X-Session-ID")
	client := r.getClient(sessionID)
	if client == nil {
		respondError(w, http.StatusUnauthorized, "Invalid or missing session")
		return
	}

	var body AuthCodeRequest
	if err := json.NewDecoder(req.Body).Decode(&body); err != nil {
		respondError(w, http.StatusBadRequest, "Invalid JSON body")
		return
	}

	if body.Code == "" {
		respondError(w, http.StatusBadRequest, "code is required")
		return
	}

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	user, err := client.SignIn(ctx, body.Code)
	if err != nil {
		// Check if 2FA required
		if strings.Contains(err.Error(), "SESSION_PASSWORD_NEEDED") {
			respondJSON(w, http.StatusOK, map[string]interface{}{
				"status":  "2fa_required",
				"message": "Two-factor authentication required",
			})
			return
		}
		respondError(w, http.StatusUnauthorized, "Failed to sign in: "+err.Error())
		return
	}

	log.Printf("Auth code request: user=%s", user.Username)

	// Explicitly save session to disk
	if err := client.SaveSession(); err != nil {
		log.Printf("⚠️ Failed to save session after SignIn: %v", err)
	}

	respondJSON(w, http.StatusOK, map[string]interface{}{
		"status":  "authenticated",
		"message": "Successfully authenticated",
		"user":    user,
	})
}

// Auth2FARequest is the request for /auth/2fa
type Auth2FARequest struct {
	Password string `json:"password"`
}

func (r *Router) handleAuth2FA(w http.ResponseWriter, req *http.Request) {
	if req.Method != http.MethodPost {
		respondError(w, http.StatusMethodNotAllowed, "Method not allowed")
		return
	}

	sessionID := req.Header.Get("X-Session-ID")
	client := r.getClient(sessionID)
	if client == nil {
		respondError(w, http.StatusUnauthorized, "Invalid or missing session")
		return
	}

	var body Auth2FARequest
	if err := json.NewDecoder(req.Body).Decode(&body); err != nil {
		respondError(w, http.StatusBadRequest, "Invalid JSON body")
		return
	}

	if body.Password == "" {
		respondError(w, http.StatusBadRequest, "password is required")
		return
	}

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	user, err := client.Check2FA(ctx, body.Password)
	if err != nil {
		respondError(w, http.StatusUnauthorized, "2FA verification failed: "+err.Error())
		return
	}

	log.Printf("Auth 2FA request: user=%s", user.Username)

	// Explicitly save session to disk
	if err := client.SaveSession(); err != nil {
		log.Printf("⚠️ Failed to save session after 2FA: %v", err)
	}

	respondJSON(w, http.StatusOK, map[string]interface{}{
		"status":  "authenticated",
		"message": "2FA verification successful",
		"user":    user,
	})
}

func (r *Router) handleGetMe(w http.ResponseWriter, req *http.Request) {
	if req.Method != http.MethodGet {
		respondError(w, http.StatusMethodNotAllowed, "Method not allowed")
		return
	}

	sessionID := req.Header.Get("X-Session-ID")
	client := r.getClient(sessionID)
	if client == nil {
		respondError(w, http.StatusUnauthorized, "Invalid or missing session")
		return
	}

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	user, err := client.GetMe(ctx)
	if err != nil {
		respondError(w, http.StatusInternalServerError, "Failed to get user: "+err.Error())
		return
	}

	respondJSON(w, http.StatusOK, user)
}

func (r *Router) handleGetDialogs(w http.ResponseWriter, req *http.Request) {
	if req.Method != http.MethodGet {
		respondError(w, http.StatusMethodNotAllowed, "Method not allowed")
		return
	}

	sessionID := req.Header.Get("X-Session-ID")
	client := r.getClient(sessionID)
	if client == nil {
		respondError(w, http.StatusUnauthorized, "Invalid or missing session")
		return
	}

	limit := 100
	if l := req.URL.Query().Get("limit"); l != "" {
		if parsed, err := strconv.Atoi(l); err == nil && parsed > 0 {
			limit = parsed
		}
	}

	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()

	dialogs, err := client.GetDialogs(ctx, limit)
	if err != nil {
		respondError(w, http.StatusInternalServerError, "Failed to get dialogs: "+err.Error())
		return
	}

	respondJSON(w, http.StatusOK, map[string]interface{}{
		"dialogs": dialogs,
	})
}

func (r *Router) handleGetHistory(w http.ResponseWriter, req *http.Request) {
	if req.Method != http.MethodGet {
		respondError(w, http.StatusMethodNotAllowed, "Method not allowed")
		return
	}

	sessionID := req.Header.Get("X-Session-ID")
	client := r.getClient(sessionID)
	if client == nil {
		respondError(w, http.StatusUnauthorized, "Invalid or missing session")
		return
	}

	// Extract chat ID from path: /api/v1/history/{chat_id}
	path := strings.TrimPrefix(req.URL.Path, "/api/v1/history/")
	chatID, err := strconv.ParseInt(path, 10, 64)
	if err != nil {
		respondError(w, http.StatusBadRequest, "Invalid chat_id")
		return
	}

	limit := 100
	if l := req.URL.Query().Get("limit"); l != "" {
		if parsed, err := strconv.Atoi(l); err == nil && parsed > 0 {
			limit = parsed
		}
	}

	offsetID := 0
	if o := req.URL.Query().Get("offset_id"); o != "" {
		if parsed, err := strconv.Atoi(o); err == nil && parsed > 0 {
			offsetID = parsed
		}
	}

	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()

	messages, err := client.GetHistoryWithOffset(ctx, chatID, limit, offsetID)
	if err != nil {
		respondError(w, http.StatusInternalServerError, "Failed to get history: "+err.Error())
		return
	}

	respondJSON(w, http.StatusOK, map[string]interface{}{
		"messages": messages,
	})
}

// SendMessageRequest is the request for /send
type SendMessageRequest struct {
	ChatID  int64  `json:"chat_id"`
	Text    string `json:"text"`
	ReplyTo int    `json:"reply_to,omitempty"`
}

func (r *Router) handleSendMessage(w http.ResponseWriter, req *http.Request) {
	if req.Method != http.MethodPost {
		respondError(w, http.StatusMethodNotAllowed, "Method not allowed")
		return
	}

	sessionID := req.Header.Get("X-Session-ID")
	client := r.getClient(sessionID)
	if client == nil {
		respondError(w, http.StatusUnauthorized, "Invalid or missing session")
		return
	}

	var body SendMessageRequest
	if err := json.NewDecoder(req.Body).Decode(&body); err != nil {
		respondError(w, http.StatusBadRequest, "Invalid JSON body")
		return
	}

	if body.ChatID == 0 || body.Text == "" {
		respondError(w, http.StatusBadRequest, "chat_id and text are required")
		return
	}

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	messageID, err := client.SendMessage(ctx, body.ChatID, body.Text, body.ReplyTo)
	if err != nil {
		respondError(w, http.StatusInternalServerError, "Failed to send message: "+err.Error())
		return
	}

	log.Printf("Send message: chat_id=%d, message_id=%d", body.ChatID, messageID)

	respondJSON(w, http.StatusOK, map[string]interface{}{
		"success":    true,
		"message_id": messageID,
	})
}

// SendPhotoRequest is the request for /send/photo
type SendPhotoRequest struct {
	ChatID   int64  `json:"chat_id"`
	PhotoURL string `json:"photo_url"`
	Caption  string `json:"caption,omitempty"`
}

func (r *Router) handleSendPhoto(w http.ResponseWriter, req *http.Request) {
	if req.Method != http.MethodPost {
		respondError(w, http.StatusMethodNotAllowed, "Method not allowed")
		return
	}

	sessionID := req.Header.Get("X-Session-ID")
	client := r.getClient(sessionID)
	if client == nil {
		respondError(w, http.StatusUnauthorized, "Invalid or missing session")
		return
	}

	var body SendPhotoRequest
	if err := json.NewDecoder(req.Body).Decode(&body); err != nil {
		respondError(w, http.StatusBadRequest, "Invalid JSON body")
		return
	}

	if body.ChatID == 0 || body.PhotoURL == "" {
		respondError(w, http.StatusBadRequest, "chat_id and photo_url are required")
		return
	}

	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()

	messageID, err := client.SendPhoto(ctx, body.ChatID, body.PhotoURL, body.Caption)
	if err != nil {
		respondError(w, http.StatusInternalServerError, "Failed to send photo: "+err.Error())
		return
	}

	log.Printf("Send photo: chat_id=%d, message_id=%d, url=%s", body.ChatID, messageID, body.PhotoURL[:min(50, len(body.PhotoURL))])

	respondJSON(w, http.StatusOK, map[string]interface{}{
		"success":    true,
		"message_id": messageID,
	})
}

// handleResolveUsername resolves a username to a peer ID
func (r *Router) handleResolveUsername(w http.ResponseWriter, req *http.Request) {
	if req.Method != http.MethodGet {
		respondError(w, http.StatusMethodNotAllowed, "Method not allowed")
		return
	}

	sessionID := req.Header.Get("X-Session-ID")
	client := r.getClient(sessionID)
	if client == nil {
		respondError(w, http.StatusUnauthorized, "Invalid or missing session")
		return
	}

	username := req.URL.Query().Get("username")
	if username == "" {
		respondError(w, http.StatusBadRequest, "username parameter required")
		return
	}

	// Remove @ prefix if present
	username = strings.TrimPrefix(username, "@")

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	result, err := client.ResolveUsername(ctx, username)
	if err != nil {
		respondError(w, http.StatusInternalServerError, "Failed to resolve username: "+err.Error())
		return
	}

	respondJSON(w, http.StatusOK, result)
}

// WebSocket handler
func (r *Router) handleWebSocket(w http.ResponseWriter, req *http.Request) {
	sessionID := getSessionID(req)
	if sessionID == "" {
		respondError(w, http.StatusBadRequest, "X-Session-ID header required (or session_id query param)")
		return
	}

	client := r.getClient(sessionID)
	if client == nil {
		respondError(w, http.StatusUnauthorized, "Invalid session")
		return
	}

	conn, err := r.upgrader.Upgrade(w, req, nil)
	if err != nil {
		log.Printf("WebSocket upgrade error: %v", err)
		return
	}

	wsClient := &WSClient{
		hub:       r.wsHub,
		conn:      conn,
		send:      make(chan []byte, 256),
		sessionID: sessionID,
	}

	r.wsHub.register <- wsClient

	go wsClient.writePump()
	go wsClient.readPump()
}

// Helper methods

func (r *Router) getClient(sessionID string) *telegram.Client {
	r.mu.RLock()
	defer r.mu.RUnlock()
	return r.clients[sessionID]
}

func (r *Router) forwardUpdates(sessionID string, client *telegram.Client) {
	for update := range client.Updates() {
		data, err := json.Marshal(update)
		if err != nil {
			continue
		}
		r.wsHub.broadcast <- WSMessage{
			SessionID: sessionID,
			Data:      data,
		}
	}
}

func generateSessionID() string {
	// Simple session ID generation
	return "sess_" + strconv.FormatInt(time.Now().UnixNano(), 36)
}

// handleDownloadMedia downloads media from a message
// POST /api/v1/download
// Body: {"chat_id": 123, "message_id": 456}
func (r *Router) handleDownloadMedia(w http.ResponseWriter, req *http.Request) {
	if req.Method != http.MethodPost {
		respondError(w, http.StatusMethodNotAllowed, "POST required")
		return
	}

	sessionID := req.Header.Get("X-Session-ID")
	if sessionID == "" {
		respondError(w, http.StatusBadRequest, "X-Session-ID header required")
		return
	}

	client := r.getClient(sessionID)
	if client == nil {
		respondError(w, http.StatusNotFound, "session not found")
		return
	}

	var body struct {
		ChatID    int64 `json:"chat_id"`
		MessageID int   `json:"message_id"`
	}

	if err := json.NewDecoder(req.Body).Decode(&body); err != nil {
		respondError(w, http.StatusBadRequest, "invalid JSON: "+err.Error())
		return
	}

	if body.ChatID == 0 || body.MessageID == 0 {
		respondError(w, http.StatusBadRequest, "chat_id and message_id required")
		return
	}

	ctx, cancel := context.WithTimeout(req.Context(), 2*time.Minute)
	defer cancel()

	result, err := client.DownloadMedia(ctx, body.ChatID, body.MessageID)
	if err != nil {
		respondError(w, http.StatusInternalServerError, "download error: "+err.Error())
		return
	}

	if !result.Success {
		respondError(w, http.StatusBadRequest, result.Error)
		return
	}

	respondJSON(w, http.StatusOK, result)
}

// handleGetMediaInfo gets media info for a specific message
// GET /api/v1/media/{chat_id}/{message_id}
func (r *Router) handleGetMediaInfo(w http.ResponseWriter, req *http.Request) {
	if req.Method != http.MethodGet {
		respondError(w, http.StatusMethodNotAllowed, "GET required")
		return
	}

	sessionID := req.Header.Get("X-Session-ID")
	if sessionID == "" {
		respondError(w, http.StatusBadRequest, "X-Session-ID header required")
		return
	}

	client := r.getClient(sessionID)
	if client == nil {
		respondError(w, http.StatusNotFound, "session not found")
		return
	}

	// Parse path: /api/v1/media/{chat_id}/{message_id}
	path := strings.TrimPrefix(req.URL.Path, "/api/v1/media/")
	parts := strings.Split(path, "/")
	if len(parts) != 2 {
		respondError(w, http.StatusBadRequest, "path must be /api/v1/media/{chat_id}/{message_id}")
		return
	}

	chatID, err := strconv.ParseInt(parts[0], 10, 64)
	if err != nil {
		respondError(w, http.StatusBadRequest, "invalid chat_id")
		return
	}

	messageID, err := strconv.Atoi(parts[1])
	if err != nil {
		respondError(w, http.StatusBadRequest, "invalid message_id")
		return
	}

	ctx, cancel := context.WithTimeout(req.Context(), 30*time.Second)
	defer cancel()

	msg, err := client.GetMessageWithMedia(ctx, chatID, messageID)
	if err != nil {
		respondError(w, http.StatusInternalServerError, "get message error: "+err.Error())
		return
	}

	respondJSON(w, http.StatusOK, msg)
}
