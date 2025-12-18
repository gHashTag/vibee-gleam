# Activity Log Panel - Detailed Analysis

## Overview
The Activity Log panel is the **right-side panel** (280px wide) in the P2P page that displays real-time logs and events. This is the "logs on the right side" (логи справа) mentioned by the user.

## Location in Code
**File:** `/workspaces/vibee-gleam/gleam/src/vibee/web/p2p_panel.gleam`
**Function:** `render_activity_log()` (lines 442-455)

## Visual Position

```
┌─────────────────────────────────────────────────────────────┐
│                        HEADER                               │
├──────────────┬──────────────────────────┬───────────────────┤
│   SIDEBAR    │     MAIN CONTENT         │  ACTIVITY LOG ◄── │
│   (250px)    │      (flexible)          │    (280px)        │
│              │                          │                   │
│              │                          │  This is the      │
│              │                          │  "logs on the     │
│              │                          │   right side"     │
└──────────────┴──────────────────────────┴───────────────────┘
```

## HTML Structure

```html
<div class="activity-log">
  <div class="log-header">
    <span class="log-icon">📜</span>
    <span class="log-title" data-i18n="log.liveActivity">Live Activity</span>
    <button class="btn btn-xs" onclick="clearLog()" data-i18n="btn.clear">Clear</button>
  </div>
  <div class="log-body" id="logBody">
    <div class="log-entry system">
      <span class="log-time">--:--:--</span>
      <span class="log-text" data-i18n="log.waitingConnection">Waiting for connection...</span>
    </div>
  </div>
</div>
```

## CSS Styling

### Container
```css
.activity-log {
  position: fixed;
  top: var(--header-height);    /* 50px */
  right: 0;
  bottom: 0;
  width: var(--log-width);      /* 280px */
  background: var(--bg-secondary); /* #111111 */
  border-left: 1px solid var(--border-color); /* #2d3748 */
  display: flex;
  flex-direction: column;
  z-index: 100;
}
```

### Header
```css
.log-header {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.5rem 1rem;
  background: rgba(0, 0, 0, 0.3);
  border-bottom: 1px solid var(--border-color);
  font-size: 0.85rem;
}

.log-icon { font-size: 1rem; }
.log-title { flex: 1; font-weight: 600; }
```

### Body (Scrollable Area)
```css
.log-body {
  flex: 1;
  overflow-y: auto;
  padding: 0.5rem 1rem;
  font-size: 0.8rem;
  font-family: 'JetBrains Mono', monospace;
}
```

### Log Entries
```css
.log-entry {
  display: flex;
  gap: 1rem;
  padding: 0.25rem 0;
  border-bottom: 1px solid rgba(255, 255, 255, 0.03);
  animation: fadeIn 0.3s;
}

@keyframes fadeIn {
  from { opacity: 0; background: rgba(0, 255, 136, 0.1); }
  to { opacity: 1; }
}

.log-time { 
  color: var(--text-muted);  /* #555555 */
  min-width: 70px; 
}

.log-text { 
  flex: 1; 
}
```

## Log Entry Types

### 1. System Logs (Blue)
```css
.log-entry.system .log-text { 
  color: var(--accent-blue);  /* #00d4ff */
}
```
**Examples:**
- `12:34:56  ✓ WebSocket connected`
- `12:35:00  🔄 Scanning markets...`
- `12:36:00  ⚙️ Configuration updated`

### 2. Trade Logs (Green)
```css
.log-entry.trade .log-text { 
  color: var(--accent-green);  /* #00ff88 */
}
```
**Examples:**
- `12:35:15  ✅ Order #abc123 completed`
- `12:35:30  💰 Profit: $42.50`
- `12:36:00  📝 New order created`

### 3. Error Logs (Red)
```css
.log-entry.error .log-text { 
  color: var(--accent-red);  /* #ff4444 */
}
```
**Examples:**
- `12:36:15  ❌ Error: Rate limit exceeded`
- `12:37:00  ⚠️ Connection lost`
- `12:37:30  ❌ Failed to execute order`

### 4. Arbitrage Logs (Orange)
```css
.log-entry.arb .log-text { 
  color: var(--accent-orange);  /* #ff9800 */
}
```
**Examples:**
- `12:35:30  ⚡ Arbitrage opportunity found: 2.5%`
- `12:36:00  💎 Best spread: Binance → Bybit`
- `12:36:30  🎯 Executing arbitrage trade`

## JavaScript Implementation

### Adding Log Entries
```javascript
function addLog(message, level = 'system') {
  const logBody = document.getElementById('logBody');
  const now = new Date();
  const time = now.toTimeString().substring(0, 8);  // HH:MM:SS

  const entry = document.createElement('div');
  entry.className = 'log-entry ' + level;
  entry.innerHTML = `
    <span class="log-time">${time}</span>
    <span class="log-text">${escapeHtml(message)}</span>
  `;

  // Insert at top (newest first)
  logBody.insertBefore(entry, logBody.firstChild);

  // Limit to 100 entries
  while (logBody.children.length > 100) {
    logBody.removeChild(logBody.lastChild);
  }
}
```

### Clearing Logs
```javascript
function clearLog() {
  document.getElementById('logBody').innerHTML = '';
  addLog(t('log.logCleared'), 'system');
}
```

### Loading Activity from Server
```javascript
async function loadActivityLog() {
  try {
    const resp = await fetch('/api/v1/p2p/activity?limit=50');
    const data = await resp.json();

    if (data.success && data.entries) {
      const newEntries = data.entries.filter(e => e.id > lastActivityId);

      if (newEntries.length > 0) {
        lastActivityId = Math.max(...data.entries.map(e => e.id));
        
        newEntries.reverse().forEach(entry => {
          const logType = mapActivityType(entry.type);
          addLog(entry.message, logType);
        });
      }
    }
  } catch (e) {
    console.error('Error loading activity log:', e);
  }
}
```

### Activity Type Mapping
```javascript
function mapActivityType(type) {
  switch (type) {
    case 'maker': return 'trade';
    case 'arbitrage': return 'arb';
    case 'alert': return 'system';
    case 'error': return 'error';
    default: return 'system';
  }
}
```

## WebSocket Integration

### Connection
```javascript
const ws = new WebSocket('ws://localhost:8080/ws/p2p');

ws.onopen = () => {
  addLog(t('log.connected'), 'system');
};

ws.onmessage = (event) => {
  const msg = JSON.parse(event.data);
  
  if (msg.type === 'log') {
    addLog(msg.data.message, msg.data.level || 'system');
  }
};

ws.onerror = (error) => {
  addLog(t('error.connectionFailed'), 'error');
};

ws.onclose = () => {
  addLog(t('log.disconnected'), 'error');
};
```

### Subscribed Channels
The WebSocket subscribes to multiple channels:
```javascript
channels: ['status', 'orders', 'arbitrage', 'logs']
```

## Log Entry Lifecycle

### 1. Event Occurs
- User action (button click)
- WebSocket message received
- API response received
- Timer/interval trigger

### 2. Log Function Called
```javascript
addLog('Order completed successfully', 'trade');
```

### 3. Entry Created
- Current timestamp extracted
- HTML element created
- Styled with appropriate class
- Inserted at top of log

### 4. Auto-cleanup
- If more than 100 entries exist
- Oldest entries removed from bottom

### 5. Animation
- Fade-in animation (0.3s)
- Brief green highlight on new entries

## Example Log Sequence

```
12:34:56  ✓ WebSocket connected                    [system - blue]
12:35:01  🔄 Scanning markets...                    [system - blue]
12:35:05  ⚡ Found opportunity: TON/RUB 2.5%       [arb - orange]
12:35:10  💰 Executing trade...                     [trade - green]
12:35:15  ✅ Order #abc123 created                  [trade - green]
12:35:20  📝 Waiting for payment...                 [system - blue]
12:35:45  ✅ Payment received                       [trade - green]
12:35:50  💵 Profit: $42.50                         [trade - green]
12:36:00  🤖 Maker bot prices updated               [system - blue]
12:36:15  ❌ Error: Rate limit exceeded             [error - red]
12:36:20  ⏸️ Pausing for 60 seconds...             [system - blue]
```

## Internationalization

### English
```javascript
"log.liveActivity": "Live Activity"
"log.connected": "Connected"
"log.disconnected": "Disconnected"
"log.waitingConnection": "Waiting for connection..."
"log.logCleared": "Log cleared"
"btn.clear": "Clear"
```

### Russian
```javascript
"log.liveActivity": "Активность"
"log.connected": "Подключено"
"log.disconnected": "Отключено"
"log.waitingConnection": "Ожидание подключения..."
"log.logCleared": "Лог очищен"
"btn.clear": "Очистить"
```

## Performance Considerations

### 1. Entry Limit
- Maximum 100 entries kept in DOM
- Prevents memory bloat
- Oldest entries auto-removed

### 2. Efficient Insertion
- `insertBefore()` for top insertion
- No full re-render needed
- Only new entries added

### 3. Animation
- CSS-based fade-in
- Hardware-accelerated
- No JavaScript animation loops

### 4. Debouncing
- WebSocket messages can be debounced
- Prevents log spam
- Groups similar messages

## Scrolling Behavior

### Auto-scroll
- New entries appear at top
- User can scroll to view history
- No auto-scroll to bottom (top-insertion pattern)

### Custom Scrollbar
```css
.log-body::-webkit-scrollbar {
  width: 6px;
}

.log-body::-webkit-scrollbar-track {
  background: transparent;
}

.log-body::-webkit-scrollbar-thumb {
  background: var(--border-color);
  border-radius: 3px;
}

.log-body::-webkit-scrollbar-thumb:hover {
  background: var(--accent-blue);
}
```

## Responsive Behavior

### Desktop (>768px)
- Fixed right panel (280px)
- Always visible
- Independent scrolling

### Mobile (<768px)
```css
@media (max-width: 768px) {
  .activity-log {
    display: none;  /* Hidden on mobile */
  }
  
  /* Or could be shown as overlay/modal */
}
```

## Integration with Other Components

### 1. WebSocket Handler
**File:** `/gleam/src/vibee/api/p2p_ws.gleam`
- Sends log messages via WebSocket
- Formats messages for display
- Includes timestamp and level

### 2. API Endpoints
**Endpoint:** `/api/v1/p2p/activity`
- Returns recent activity entries
- Supports pagination (`?limit=50`)
- Used for initial load and refresh

### 3. Event Bus
- Subscribes to event bus
- Receives system-wide events
- Converts events to log entries

## Common Log Messages

### Startup
```
✓ WebSocket connected
🔄 Loading configuration...
✅ Agent initialized
📊 Fetching market data...
```

### Trading
```
💰 Creating order...
✅ Order #abc123 created
📝 Waiting for counterparty...
💵 Trade completed: +$42.50
```

### Arbitrage
```
🔍 Scanning for opportunities...
⚡ Found: Binance → Bybit (2.5%)
🎯 Executing arbitrage...
✅ Arbitrage completed: +$15.30
```

### Errors
```
❌ Error: Insufficient balance
⚠️ Warning: High spread detected
❌ Failed to connect to exchange
⏸️ Rate limit reached, pausing...
```

### System
```
⚙️ Configuration updated
🔄 Refreshing prices...
🤖 Maker bot enabled
🔔 Alert triggered: TON/RUB 3.2%
```

## Debugging

### Enable Verbose Logging
```javascript
// In browser console
localStorage.setItem('debug', 'true');
```

### View Raw WebSocket Messages
```javascript
ws.addEventListener('message', (event) => {
  console.log('WS:', event.data);
});
```

### Export Logs
```javascript
function exportLogs() {
  const entries = Array.from(document.querySelectorAll('.log-entry'));
  const logs = entries.map(e => {
    const time = e.querySelector('.log-time').textContent;
    const text = e.querySelector('.log-text').textContent;
    return `${time} ${text}`;
  });
  
  const blob = new Blob([logs.join('\n')], { type: 'text/plain' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = `p2p-logs-${Date.now()}.txt`;
  a.click();
}
```

## Future Enhancements

### Potential Features
1. **Log Filtering**
   - Filter by type (system/trade/error/arb)
   - Search functionality
   - Date range filter

2. **Log Persistence**
   - Save to localStorage
   - Export to file
   - Email logs

3. **Advanced Display**
   - Collapsible entries
   - Detailed view on click
   - Copy to clipboard

4. **Notifications**
   - Browser notifications for errors
   - Sound alerts
   - Desktop notifications

5. **Analytics**
   - Log statistics
   - Error rate tracking
   - Performance metrics

## Summary

The Activity Log panel is a **280px-wide fixed panel on the right side** of the P2P page that:

✅ Displays real-time logs and events
✅ Shows 4 types of entries (system, trade, error, arbitrage)
✅ Uses color coding for quick identification
✅ Limits to 100 entries for performance
✅ Receives updates via WebSocket
✅ Supports bilingual display (EN/RU)
✅ Features smooth animations
✅ Provides clear button for cleanup
✅ Uses monospace font for readability
✅ Scrolls independently from other panels

This is the "logs on the right side" (логи справа) that provides real-time visibility into all P2P agent activities.
