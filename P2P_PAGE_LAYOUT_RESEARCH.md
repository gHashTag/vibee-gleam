# P2P Page Layout Research Summary

## Overview
The P2P page (P2P страница) is a comprehensive web interface for the VIBEE P2P Earning Agent. It features a **three-column split layout** with a dark theme and real-time updates via WebSocket.

## File Location
**Primary File:** `/workspaces/vibee-gleam/gleam/src/vibee/web/p2p_panel.gleam`
- **Size:** 3,707 lines
- **Language:** Gleam (generates HTML/CSS/JavaScript)
- **Route:** Accessible at `/p2p` and `/earning` (alias)

## Layout Structure

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              HEADER (50px)                                   │
│  💰 VIBEE P2P  │  Dashboard  Messages  Events  P2P Agent  │  EN  ●          │
└─────────────────────────────────────────────────────────────────────────────┘
┌──────────────┬──────────────────────────────────────────┬───────────────────┐
│   SIDEBAR    │           MAIN CONTENT                   │   ACTIVITY LOG    │
│   (250px)    │           (flexible)                     │     (280px)       │
│              │                                          │                   │
│ ┌──────────┐ │ ┌──────────────────────────────────────┐ │ ┌───────────────┐ │
│ │ 🤖 Agent │ │ │  Strategy Selector                   │ │ │ 📜 Live       │ │
│ │  Status  │ │ │  💤 Passive  📈 Market  ⚡ Arb  🔥   │ │ │    Activity   │ │
│ │          │ │ └──────────────────────────────────────┘ │ │               │ │
│ │ ● Status │ │                                          │ │ 12:34:56      │ │
│ │ Strategy │ │ ┌──────────────────────────────────────┐ │ │ ✓ Connected   │ │
│ │ Uptime   │ │ │ 💵 Profit Dashboard                  │ │ │               │ │
│ └──────────┘ │ │  ┌────┬────┬────┬────┐              │ │ │ 12:35:01      │ │
│              │ │  │Today│Week│Month│Total│             │ │ │ 🔄 Scanning   │ │
│ ┌──────────┐ │ │  └────┴────┴────┴────┘              │ │ │               │ │
│ │ 👛 Wallet│ │ │  📈 Charts                           │ │ │ 12:35:15      │ │
│ │          │ │ └──────────────────────────────────────┘ │ │ ✅ Order done │ │
│ │ Address  │ │                                          │ │               │ │
│ │ Balance  │ │ ┌──────────────────────────────────────┐ │ │ 12:35:30      │ │
│ │ Connect  │ │ │ 📋 Active Orders                     │ │ │ ⚡ Arbitrage  │ │
│ └──────────┘ │ │  Table with orders...                │ │ │    found      │ │
│              │ │                                          │ │               │ │
│ ┌──────────┐ │ └──────────────────────────────────────┘ │ │ (scrollable)  │ │
│ │ 📊 Quick │ │                                          │ │               │ │
│ │  Stats   │ │ ┌──────────────────────────────────────┐ │ │               │ │
│ │          │ │ │ ⚡ Arbitrage Opportunities           │ │ │               │ │
│ │ Active   │ │ │  [TON] [RUB] [🔍 Scan]              │ │ │               │ │
│ │ Trades   │ │ │  Grid of opportunities...            │ │ │               │ │
│ │ Pending  │ │ └──────────────────────────────────────┘ │ │               │ │
│ └──────────┘ │                                          │ │               │ │
│              │ ┌──────────────────────────────────────┐ │ │               │ │
│ ┌──────────┐ │ │ 💰 Earning Control                   │ │ │               │ │
│ │ 🎛️ Ctrl │ │ │  🤖 Maker Bot [ON/OFF]              │ │ │               │ │
│ │          │ │ │  🔔 Spread Alerts                   │ │ │               │ │
│ │ ▶ START  │ │ │  ⚡ Auto Executor                   │ │ │               │ │
│ │ ⏹ STOP   │ │ └──────────────────────────────────────┘ │ │               │ │
│ │ ⚙ CONFIG │ │                                          │ │               │ │
│ └──────────┘ │                                          │ └───────────────┘ │
│              │                                          │                   │
│ (scrollable) │ (scrollable)                            │ (scrollable)      │
└──────────────┴──────────────────────────────────────────┴───────────────────┘
```

## Layout Dimensions (CSS Variables)

```css
--header-height: 50px;
--sidebar-width: 250px;
--log-width: 280px;
```

### Positioning
- **Header:** Fixed at top, full width
- **Sidebar:** Fixed left, from header to bottom
- **Main Content:** Flexible width between sidebar and log panel
- **Activity Log:** Fixed right, from header to bottom

### CSS Layout Code
```css
.app-content {
  position: fixed;
  top: var(--header-height);
  left: 0;
  right: var(--log-width);
  bottom: 0;
  display: flex;
}

.sidebar {
  width: var(--sidebar-width);
  height: 100%;
  overflow-y: auto;
}

.main {
  flex: 1;
  height: 100%;
  overflow-y: scroll;
}

.activity-log {
  position: fixed;
  top: var(--header-height);
  right: 0;
  bottom: 0;
  width: var(--log-width);
  overflow-y: auto;
}
```

## Components Breakdown

### 1. Header (render_header)
- **Logo:** 💰 VIBEE P2P
- **Navigation Links:**
  - Dashboard (/)
  - Messages (/app)
  - Events (/events)
  - P2P Agent (/p2p) - active
- **Right Side:**
  - User info (populated by JS)
  - Language toggle (EN/RU)
  - Status indicator dot

### 2. Sidebar (render_sidebar)
Contains 4 cards:

#### a) Agent Status Card 🤖
- Status indicator (active/inactive)
- Current strategy badge
- Uptime display

#### b) Wallet Card 👛
- Wallet address display
- Connect/Disconnect button
- Balance display (USDT, TON)
- TON Connect integration

#### c) Quick Stats Card 📊
- Active Orders count
- Trades Today count
- Pending Arbitrage count

#### d) Controls Card 🎛️
- START button (green)
- STOP button (red)
- CONFIG button (secondary)

### 3. Main Content (render_main)
Contains multiple sections:

#### a) Strategy Selector
4 strategy buttons:
- 💤 Passive Fees
- 📈 Market Making
- ⚡ Arbitrage
- 🔥 Hybrid

#### b) Profit Dashboard 💵
- **Profit Cards Grid:**
  - Today
  - This Week
  - This Month
  - Total Earned
- **Profit Breakdown:**
  - Platform Fees
  - Spread Profit
  - Arbitrage Profit
- **Charts:**
  - 📈 7-Day Profit Line Chart
  - 📊 Profit Breakdown Pie Chart

#### c) Active Orders Table 📋
- Columns: ID, Type, Crypto, Amount, Fiat, Rate, Status, Actions
- Refresh button
- Real-time updates

#### d) Arbitrage Opportunities ⚡
- Currency selectors (TON/USDT/NOT)
- Fiat selector (RUB/THB/USD)
- Scan button
- Grid display of opportunities
- Execute buttons for each opportunity

#### e) Earning Control Panel 💰
Three sections:
1. **Maker Bot 🤖**
   - Enable/disable toggle
   - Buy/Sell prices display
   - Spread display
   - Estimated profit
   - Spread slider (0.5% - 3%)
   - Refresh prices button

2. **Spread Alerts 🔔**
   - Enable/disable toggle
   - Min spread selector
   - Alerts list
   - Scan alerts button

3. **Auto Executor ⚡**
   - Status display
   - Total profit counter
   - Trades counter
   - Simulate trade button
   - "DRY RUN" badge

### 4. Activity Log (render_activity_log)
- **Header:** 📜 Live Activity + Clear button
- **Log Body:** Scrollable list of log entries
- **Log Entry Types:**
  - `system` (blue) - System messages
  - `trade` (green) - Trade activities
  - `error` (red) - Error messages
  - `arb` (orange) - Arbitrage activities
- **Format:** `[HH:MM:SS] Message`
- **Limit:** 100 entries (auto-cleanup)

## Real-time Updates (WebSocket)

### WebSocket Endpoint
`/ws/p2p` - Handled by `p2p_ws.gleam`

### Message Types
1. **StatusUpdate** - Agent status changes
2. **OrderUpdate** - Order state changes
3. **ArbitrageUpdate** - New arbitrage opportunities
4. **LogMessage** - Activity log entries
5. **PriceUpdate** - Price feed updates

### Subscribed Channels
```javascript
channels: ['status', 'orders', 'arbitrage', 'logs']
```

### Log Function
```javascript
function addLog(message, level = 'system') {
  const logBody = document.getElementById('logBody');
  const now = new Date();
  const time = now.toTimeString().substring(0, 8);
  
  const entry = document.createElement('div');
  entry.className = 'log-entry ' + level;
  entry.innerHTML = `<span class="log-time">${time}</span>
                     <span class="log-text">${escapeHtml(message)}</span>`;
  
  logBody.insertBefore(entry, logBody.firstChild);
  
  // Limit to 100 entries
  while (logBody.children.length > 100) {
    logBody.removeChild(logBody.lastChild);
  }
}
```

## Theme & Styling

### Color Palette
```css
--bg-primary: #0a0a0a;      /* Main background */
--bg-secondary: #111111;    /* Header, sidebar */
--bg-card: #1a1a2e;         /* Cards */
--bg-hover: #252550;        /* Hover states */
--text-primary: #e0e0e0;    /* Main text */
--text-secondary: #8b8b8b;  /* Labels */
--text-muted: #555555;      /* Hints */
--accent-green: #00ff88;    /* Success */
--accent-red: #ff4444;      /* Danger */
--accent-blue: #00d4ff;     /* Info */
--accent-orange: #ff9800;   /* Warning */
--accent-purple: #aa00ff;   /* Special */
--border-color: #2d3748;    /* Borders */
```

### Typography
```css
font-family: 'JetBrains Mono', 'Fira Code', 'Monaco', monospace;
font-size: 14px;
```

## Responsive Design

### Desktop (>1200px)
- Full three-column layout
- All navigation visible
- Sidebar: 250px
- Log: 280px

### Tablet (768px - 1024px)
- Three-column layout maintained
- Some text hidden (logo-text, user-name)
- Profit grid: 2 columns

### Mobile (<768px)
- **Stacked layout:**
  - Sidebar: full width, horizontal cards
  - Main: full width
  - Log: hidden or overlay
- Navigation links hidden
- Profit grid: 1 column

```css
@media (max-width: 768px) {
  .app-content { flex-direction: column; }
  .sidebar { 
    width: 100%; 
    max-height: 300px; 
    flex-direction: row; 
    flex-wrap: wrap; 
  }
  .nav-links { display: none; }
  .profit-grid { grid-template-columns: 1fr; }
}
```

## Internationalization (i18n)

### Supported Languages
- **English (EN)** - Default
- **Russian (RU)**

### Implementation
- Data attributes: `data-i18n="key.path"`
- JavaScript translation function: `t(key, params)`
- Language toggle button in header
- Translations stored in `i18n_script()` function

### Example Keys
```javascript
"nav.dashboard": { en: "Dashboard", ru: "Панель" }
"nav.p2p": { en: "P2P Agent", ru: "P2P Агент" }
"log.connected": { en: "Connected", ru: "Подключено" }
"btn.start": { en: "START", ru: "СТАРТ" }
```

## Authentication

### TON Connect Integration
- Wallet connection via TON Connect UI
- Script loaded from CDN: `@tonconnect/ui@2.0.9`
- Functions:
  - `connectWallet()` - Initiate connection
  - `disconnectWallet()` - Disconnect wallet
  - Balance fetching after connection

### Auth Container
- Shown when not authenticated
- Hidden when authenticated
- Dashboard container shown after auth

## Key Features

### 1. Real-time Monitoring
- Live activity log on the right
- WebSocket updates every second
- Status indicators throughout UI

### 2. Multi-Strategy Support
- Passive Fees (💤)
- Market Making (📈)
- Arbitrage (⚡)
- Hybrid (🔥)

### 3. Profit Tracking
- Multiple timeframes (Today, Week, Month, Total)
- Visual charts (Line + Pie)
- Breakdown by source

### 4. Order Management
- Active orders table
- Real-time status updates
- Action buttons per order

### 5. Arbitrage Scanner
- Multi-currency support
- Multi-fiat support
- One-click execution
- Profit calculation

### 6. Maker Bot
- Configurable spread
- Real-time price updates
- Profit estimation
- Enable/disable toggle

### 7. Alert System
- Spread-based alerts
- Configurable thresholds
- Alert history

## Related Files

### Backend
- `/gleam/src/vibee/api/p2p_handlers.gleam` - HTTP API handlers
- `/gleam/src/vibee/api/p2p_ws.gleam` - WebSocket handler
- `/gleam/src/vibee/p2p/*.gleam` - P2P business logic

### Frontend
- All HTML/CSS/JS generated in `p2p_panel.gleam`
- No separate frontend files

### Routing
- `/gleam/src/vibee/api/router.gleam` - Route definitions
  ```gleam
  http.Get, ["p2p"] -> p2p_panel_handler()
  http.Get, ["earning"] -> p2p_panel_handler()  // Alias
  ```

## Technical Details

### Chart.js Integration
- CDN: `chart.js@4.4.1`
- Two charts:
  - `profitLineChart` - 7-day profit trend
  - `profitPieChart` - Profit breakdown

### WebSocket Connection
```javascript
const ws = new WebSocket('ws://localhost:8080/ws/p2p');
ws.onmessage = (event) => {
  const msg = JSON.parse(event.data);
  handleMessage(msg);
};
```

### Modal System
- Accept Order Modal
- Confirmation dialogs
- Overlay with backdrop blur

## User Flow

1. **Landing:** User arrives at `/p2p`
2. **Authentication:** TON Connect wallet connection
3. **Dashboard Display:** Three-column layout appears
4. **Strategy Selection:** Choose earning strategy
5. **Configuration:** Set parameters (spread, alerts, etc.)
6. **Start Agent:** Click START button
7. **Monitoring:** Watch activity log for real-time updates
8. **Profit Tracking:** View dashboard metrics and charts
9. **Manual Actions:** Execute arbitrage, accept orders
10. **Stop Agent:** Click STOP when done

## Performance Considerations

### Optimizations
- Fixed positioning for panels (no reflow)
- CSS transforms for animations
- Log entry limit (100 max)
- Debounced WebSocket messages
- Lazy chart rendering

### Scrolling
- Independent scroll areas:
  - Sidebar (vertical)
  - Main content (vertical)
  - Activity log (vertical)
- Custom scrollbar styling

## Accessibility

### Features
- Semantic HTML structure
- ARIA labels on interactive elements
- Keyboard navigation support
- High contrast dark theme
- Clear visual indicators

## Future Enhancements (Potential)

Based on the code structure, possible improvements:
1. Collapsible sidebar
2. Resizable panels
3. Log filtering/search
4. Export logs functionality
5. Multiple chart views
6. Custom themes
7. Mobile-optimized layout
8. Notification system
9. Historical data views
10. Advanced analytics

## Summary

The P2P page is a sophisticated **three-column split layout** with:
- **Left:** Sidebar (250px) - Agent controls and quick stats
- **Center:** Main content (flexible) - Dashboard, orders, arbitrage
- **Right:** Activity log (280px) - Real-time event stream

The layout is **fixed-position** with independent scrolling areas, providing a professional trading interface with real-time updates via WebSocket. The dark theme with monospace fonts gives it a terminal-like aesthetic suitable for crypto trading.

The "logs on the right side" mentioned by the user refers to the **Activity Log panel** (280px wide) that displays real-time system messages, trades, errors, and arbitrage activities in a scrollable list format.
