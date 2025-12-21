/**
 * VIBEE Dashboard - Common JavaScript Utilities
 * Shared WebSocket connections, helpers, and chat data
 */

// =============================================================================
// Configuration
// =============================================================================

const VIBEE_CONFIG = {
    // Auto-detect environment
    isLocal: window.location.hostname === 'localhost' || window.location.hostname === '127.0.0.1',

    // WebSocket URLs
    get WS_BASE() {
        return this.isLocal
            ? 'ws://localhost:8080'
            : 'wss://vibee-mcp.fly.dev';
    },

    get WS_LOGS_URL() { return `${this.WS_BASE}/ws/logs`; },
    get WS_AGENTS_URL() { return `${this.WS_BASE}/ws/agents`; },
    get WS_EVENTS_URL() { return `${this.WS_BASE}/ws/events`; },

    // API URLs
    get API_BASE() {
        return this.isLocal
            ? 'http://localhost:8080'
            : 'https://vibee-mcp.fly.dev';
    },

    // Reconnect settings
    RECONNECT_DELAY: 3000,
    MAX_RECONNECT_ATTEMPTS: 10,
};

// =============================================================================
// Chat Names Mapping (shared across dashboards)
// =============================================================================

const CHAT_NAMES = {
    // Users
    '144022504': 'Dmitrii',
    '6579515876': 'vibee_agent',
    '7655182164': 'NeuroBlogger',

    // Groups
    '-1002737186844': 'VIBEE AGENT',
    '-1001978334539': 'NeuroCoder',
    '-1002643951085': 'AiStars',
    '-4744055984': 'TEST',
    '-1002455135768': 'NeuroLesson DEV',
    '-1002379906062': 'Crypto Exchange',

    // Pattaya groups
    '-1001728766323': 'Pattaya Ads',
    '-1001610990403': 'Pattaya Chat',
    '-1002274912525': 'Pattaya Bangsaray',
};

/**
 * Get human-readable name for a chat ID
 */
function getChatName(chatId) {
    const id = String(chatId);
    return CHAT_NAMES[id] || `Chat ${id}`;
}

// =============================================================================
// HTML Escaping
// =============================================================================

/**
 * Escape HTML special characters to prevent XSS
 */
function escapeHtml(text) {
    if (!text) return '';
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

// =============================================================================
// WebSocket Manager
// =============================================================================

class WebSocketManager {
    constructor(name, url, options = {}) {
        this.name = name;
        this.url = url;
        this.ws = null;
        this.reconnectAttempts = 0;
        this.maxReconnectAttempts = options.maxReconnectAttempts || VIBEE_CONFIG.MAX_RECONNECT_ATTEMPTS;
        this.reconnectDelay = options.reconnectDelay || VIBEE_CONFIG.RECONNECT_DELAY;
        this.onMessage = options.onMessage || (() => {});
        this.onStatusChange = options.onStatusChange || (() => {});
        this.pingInterval = null;
    }

    connect() {
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            console.log(`[${this.name}] Already connected`);
            return;
        }

        console.log(`[${this.name}] Connecting to ${this.url}...`);
        this.ws = new WebSocket(this.url);

        this.ws.onopen = () => {
            console.log(`[${this.name}] Connected`);
            this.reconnectAttempts = 0;
            this.onStatusChange('connected');
            this.startPing();
        };

        this.ws.onmessage = (event) => {
            if (event.data === 'pong') return;
            try {
                const data = JSON.parse(event.data);
                this.onMessage(data);
            } catch (e) {
                // Handle non-JSON messages
                this.onMessage(event.data);
            }
        };

        this.ws.onerror = (error) => {
            console.error(`[${this.name}] Error:`, error);
            this.onStatusChange('error');
        };

        this.ws.onclose = () => {
            console.log(`[${this.name}] Disconnected`);
            this.onStatusChange('disconnected');
            this.stopPing();
            this.scheduleReconnect();
        };
    }

    disconnect() {
        this.stopPing();
        if (this.ws) {
            this.ws.close();
            this.ws = null;
        }
    }

    send(message) {
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            this.ws.send(typeof message === 'string' ? message : JSON.stringify(message));
        }
    }

    startPing() {
        this.stopPing();
        this.pingInterval = setInterval(() => {
            this.send('ping');
        }, 30000);
    }

    stopPing() {
        if (this.pingInterval) {
            clearInterval(this.pingInterval);
            this.pingInterval = null;
        }
    }

    scheduleReconnect() {
        if (this.reconnectAttempts >= this.maxReconnectAttempts) {
            console.error(`[${this.name}] Max reconnect attempts reached`);
            return;
        }

        this.reconnectAttempts++;
        console.log(`[${this.name}] Reconnecting in ${this.reconnectDelay}ms (attempt ${this.reconnectAttempts})`);
        setTimeout(() => this.connect(), this.reconnectDelay);
    }
}

// =============================================================================
// Log Parsing
// =============================================================================

/**
 * Parse a Telegram message log line
 * Format: "📨 TG: {chat_id} {sender}: {message}"
 */
function parseTelegramLog(message) {
    const match = message.match(/📨 TG: ([-\d]+) ([^:]+): (.+)/);
    if (match) {
        return {
            chatId: match[1],
            sender: match[2],
            text: match[3],
            chatName: getChatName(match[1]),
        };
    }
    return null;
}

/**
 * Extract log level from message
 */
function getLogLevel(message) {
    const lowerMsg = message.toLowerCase();
    if (lowerMsg.includes('error') || lowerMsg.includes('❌')) return 'error';
    if (lowerMsg.includes('warn') || lowerMsg.includes('⚠')) return 'warn';
    if (lowerMsg.includes('debug')) return 'debug';
    return 'info';
}

// =============================================================================
// Time Formatting
// =============================================================================

/**
 * Format timestamp for display
 */
function formatTime(timestamp) {
    if (!timestamp) return 'N/A';
    const date = typeof timestamp === 'string' ? new Date(timestamp) : new Date(timestamp * 1000);
    return date.toLocaleTimeString('en-US', { hour12: false });
}

/**
 * Format relative time (e.g., "2 minutes ago")
 */
function formatRelativeTime(timestamp) {
    if (!timestamp) return 'N/A';
    const date = typeof timestamp === 'string' ? new Date(timestamp) : new Date(timestamp * 1000);
    const now = new Date();
    const diff = now - date;

    if (diff < 60000) return 'just now';
    if (diff < 3600000) return `${Math.floor(diff / 60000)}m ago`;
    if (diff < 86400000) return `${Math.floor(diff / 3600000)}h ago`;
    return `${Math.floor(diff / 86400000)}d ago`;
}

// =============================================================================
// Time Range Filters
// =============================================================================

const TIME_RANGES = {
    '5m': 5 * 60 * 1000,
    '15m': 15 * 60 * 1000,
    '30m': 30 * 60 * 1000,
    '1h': 60 * 60 * 1000,
    '3h': 3 * 60 * 60 * 1000,
    '6h': 6 * 60 * 60 * 1000,
    '12h': 12 * 60 * 60 * 1000,
    '24h': 24 * 60 * 60 * 1000,
    'all': Infinity,
};

/**
 * Check if timestamp is within time range
 */
function isWithinTimeRange(timestamp, range) {
    if (range === 'all') return true;
    const ms = TIME_RANGES[range];
    if (!ms) return true;

    const date = typeof timestamp === 'string' ? new Date(timestamp) : new Date(timestamp * 1000);
    return (Date.now() - date.getTime()) <= ms;
}

// =============================================================================
// Agent API Client
// =============================================================================

const AgentAPI = {
    async getStatus() {
        const response = await fetch(`${VIBEE_CONFIG.API_BASE}/api/agent/status`);
        return response.json();
    },

    async getConfig() {
        const response = await fetch(`${VIBEE_CONFIG.API_BASE}/api/agent/config`);
        return response.json();
    },

    async updateConfig(config) {
        const response = await fetch(`${VIBEE_CONFIG.API_BASE}/api/agent/config`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(config),
        });
        return response.json();
    },

    async start() {
        const response = await fetch(`${VIBEE_CONFIG.API_BASE}/api/agent/start`, { method: 'POST' });
        return response.json();
    },

    async stop() {
        const response = await fetch(`${VIBEE_CONFIG.API_BASE}/api/agent/stop`, { method: 'POST' });
        return response.json();
    },

    async pause() {
        const response = await fetch(`${VIBEE_CONFIG.API_BASE}/api/agent/pause`, { method: 'POST' });
        return response.json();
    },

    async reset() {
        const response = await fetch(`${VIBEE_CONFIG.API_BASE}/api/agent/reset`, { method: 'POST' });
        return response.json();
    },
};

// =============================================================================
// UI Helpers
// =============================================================================

/**
 * Update connection status indicator
 */
function updateConnectionStatus(dotElement, textElement, status) {
    if (dotElement) {
        dotElement.classList.toggle('disconnected', status !== 'connected');
    }
    if (textElement) {
        textElement.textContent = status === 'connected' ? 'Connected' : 'Disconnected';
    }
}

/**
 * Create an agent card element
 */
function createAgentCard(agent) {
    const statusClass = agent.status.toLowerCase();
    return `
        <div class="card agent-card" data-agent-id="${agent.id}">
            <div class="card-header">
                <span class="agent-name">${escapeHtml(agent.id)}</span>
                <span class="status-badge ${statusClass}">${agent.status}</span>
            </div>
            <div class="agent-stats">
                <div class="stat">
                    <span class="stat-label">Messages</span>
                    <span class="stat-value">${agent.message_count || 0}</span>
                </div>
                <div class="stat">
                    <span class="stat-label">Errors</span>
                    <span class="stat-value">${agent.error_count || 0}</span>
                </div>
            </div>
            <div class="agent-meta text-muted text-sm">
                Last activity: ${formatRelativeTime(agent.last_activity)}
            </div>
        </div>
    `;
}

// =============================================================================
// Export for module use
// =============================================================================

if (typeof module !== 'undefined' && module.exports) {
    module.exports = {
        VIBEE_CONFIG,
        CHAT_NAMES,
        getChatName,
        escapeHtml,
        WebSocketManager,
        parseTelegramLog,
        getLogLevel,
        formatTime,
        formatRelativeTime,
        TIME_RANGES,
        isWithinTimeRange,
        AgentAPI,
        updateConnectionStatus,
        createAgentCard,
    };
}
