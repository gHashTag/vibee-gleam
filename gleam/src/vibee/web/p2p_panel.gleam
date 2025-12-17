// P2P Control Panel - Web UI for P2P Earning Agent
// Dark theme, real-time updates via WebSocket
// Bilingual: English / Russian

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import vibee/earning/types.{
  type AgentStatus, type ArbitrageOpportunity, type EarningConfig, type EarningStats,
  type EarningStrategy, type PriceSource,
  PassiveFees, MarketMaking, Arbitrage, Hybrid,
}
import vibee/p2p/types as p2p_types

// =============================================================================
// PAGE GENERATOR
// =============================================================================

/// Generate the P2P Control Panel HTML page
pub fn render() -> String {
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>VIBEE - P2P Earning Agent</title>
  <script src=\"https://cdn.jsdelivr.net/npm/chart.js@4.4.1/dist/chart.umd.min.js\"></script>
  <script src=\"https://unpkg.com/@tonconnect/ui@2.0.9/dist/tonconnect-ui.min.js\"></script>
  <style>" <> styles() <> auth_styles() <> "</style>
</head>
<body>
  <!-- Auth Container (shown when not authenticated) -->
  " <> render_auth_container() <> "

  <!-- Dashboard Container (shown when authenticated) -->
  <div class=\"app-container\" id=\"dashboardContainer\" style=\"display:none\">
    " <> render_header() <> "
    <div class=\"app-content\">
      " <> render_sidebar() <> "
      " <> render_main() <> "
    </div>
    " <> render_activity_log() <> "
  </div>

  <!-- Accept Order Modal -->
  <div id=\"acceptModal\" class=\"modal-overlay\" style=\"display:none\">
    <div class=\"modal-content\">
      <div class=\"modal-header\">
        <h3 id=\"acceptModalTitle\">Принять ордер</h3>
        <button class=\"modal-close\" onclick=\"closeAcceptModal()\">&times;</button>
      </div>
      <div class=\"modal-body\">
        <p id=\"acceptModalDesc\">Детали сделки</p>
        <div class=\"modal-details\">
          <div class=\"detail-row\">
            <span class=\"detail-label\">Сумма:</span>
            <span class=\"detail-value\" id=\"acceptModalAmount\">--</span>
          </div>
          <div class=\"detail-row\">
            <span class=\"detail-label\">Итого:</span>
            <span class=\"detail-value\" id=\"acceptModalTotal\">--</span>
          </div>
          <div class=\"detail-row\">
            <span class=\"detail-label\">Курс:</span>
            <span class=\"detail-value\" id=\"acceptModalRate\">--</span>
          </div>
        </div>
        <div class=\"modal-info\">
          <span class=\"info-icon\">ℹ️</span>
          <span>После подтверждения будет создан инвойс CryptoBot для оплаты</span>
        </div>
      </div>
      <div class=\"modal-footer\">
        <button class=\"btn btn-secondary\" onclick=\"closeAcceptModal()\">Отмена</button>
        <button class=\"btn btn-success\" onclick=\"confirmAcceptOrder()\">✓ Подтвердить</button>
      </div>
    </div>
  </div>

  <script>" <> i18n_script() <> "</script>
  <script>" <> auth_script() <> "</script>
  <script>" <> client_script() <> "</script>
</body>
</html>"
}

// =============================================================================
// HEADER
// =============================================================================

fn render_header() -> String {
  "<header class=\"header\">
    <div class=\"header-left\">
      <div class=\"logo\">
        <span class=\"logo-icon\">💰</span>
        <span class=\"logo-text\">VIBEE P2P</span>
      </div>
      <nav class=\"nav-links\">
        <a href=\"/\" class=\"nav-link\" data-i18n=\"nav.dashboard\">Dashboard</a>
        <a href=\"/app\" class=\"nav-link\" data-i18n=\"nav.messages\">Messages</a>
        <a href=\"/events\" class=\"nav-link\" data-i18n=\"nav.events\">Events</a>
        <a href=\"/p2p\" class=\"nav-link active\" data-i18n=\"nav.p2p\">P2P Agent</a>
      </nav>
    </div>
    <div class=\"header-right\">
      <!-- User Info (populated by JS) -->
      <div id=\"userInfo\" class=\"user-info\" style=\"display:none\"></div>
      <!-- Language Switcher -->
      <button type=\"button\" class=\"lang-toggle\" id=\"langToggle\">EN</button>
      <span class=\"status-dot\" id=\"statusDot\" title=\"Connecting...\"></span>
    </div>
  </header>"
}

// =============================================================================
// SIDEBAR
// =============================================================================

fn render_sidebar() -> String {
  "<aside class=\"sidebar\">
    <!-- Agent Status Card -->
    <div class=\"card agent-status-card\">
      <div class=\"card-header\">
        <span class=\"card-icon\">🤖</span>
        <span class=\"card-title\" data-i18n=\"sidebar.agentStatus\">Agent Status</span>
      </div>
      <div class=\"card-body\">
        <div class=\"status-indicator\" id=\"agentIndicator\">
          <span class=\"indicator-dot inactive\"></span>
          <span class=\"indicator-text\" id=\"agentStatusText\" data-i18n=\"status.inactive\">Inactive</span>
        </div>
        <div class=\"strategy-badge\" id=\"strategyBadge\">passive_fees</div>
        <div class=\"uptime\" id=\"uptimeDisplay\"><span data-i18n=\"sidebar.uptime\">Uptime</span>: --</div>
      </div>
    </div>

    <!-- Wallet Card -->
    <div class=\"card wallet-card\">
      <div class=\"card-header\">
        <span class=\"card-icon\">👛</span>
        <span class=\"card-title\" data-i18n=\"sidebar.wallet\">Wallet</span>
      </div>
      <div class=\"card-body\">
        <div class=\"wallet-address\" id=\"walletAddress\" data-i18n=\"sidebar.notConnected\">Not connected</div>
        <button class=\"connect-wallet-btn\" id=\"connectWalletBtn\" onclick=\"connectWallet()\" data-i18n=\"sidebar.connectWallet\">Connect Wallet</button>
        <div class=\"wallet-balance\" id=\"walletBalanceSection\" style=\"display:none\">
          <div class=\"balance-row\">
            <span class=\"balance-label\">USDT:</span>
            <span class=\"balance-value\" id=\"balanceUsdt\">0.00</span>
          </div>
          <div class=\"balance-row\">
            <span class=\"balance-label\">TON:</span>
            <span class=\"balance-value\" id=\"balanceTon\">0.00</span>
          </div>
        </div>
        <button class=\"disconnect-wallet-btn\" id=\"disconnectWalletBtn\" onclick=\"disconnectWallet()\" style=\"display:none\" data-i18n=\"sidebar.disconnectWallet\">Disconnect</button>
      </div>
    </div>

    <!-- Quick Stats Card -->
    <div class=\"card stats-card\">
      <div class=\"card-header\">
        <span class=\"card-icon\">📊</span>
        <span class=\"card-title\" data-i18n=\"sidebar.quickStats\">Quick Stats</span>
      </div>
      <div class=\"card-body\">
        <div class=\"stat-row\">
          <span class=\"stat-label\" data-i18n=\"stats.activeOrders\">Active Orders</span>
          <span class=\"stat-value\" id=\"activeOrders\">0</span>
        </div>
        <div class=\"stat-row\">
          <span class=\"stat-label\" data-i18n=\"stats.tradesToday\">Trades Today</span>
          <span class=\"stat-value\" id=\"tradesToday\">0</span>
        </div>
        <div class=\"stat-row\">
          <span class=\"stat-label\" data-i18n=\"stats.pendingArb\">Pending Arb</span>
          <span class=\"stat-value\" id=\"pendingArb\">0</span>
        </div>
      </div>
    </div>

    <!-- Controls Card -->
    <div class=\"card controls-card\">
      <div class=\"card-header\">
        <span class=\"card-icon\">🎛️</span>
        <span class=\"card-title\" data-i18n=\"sidebar.controls\">Controls</span>
      </div>
      <div class=\"card-body controls-body\">
        <button class=\"btn btn-success\" id=\"startBtn\" onclick=\"startAgent()\">
          ▶ <span data-i18n=\"btn.start\">START</span>
        </button>
        <button class=\"btn btn-danger\" id=\"stopBtn\" onclick=\"stopAgent()\" disabled>
          ⏹ <span data-i18n=\"btn.stop\">STOP</span>
        </button>
        <button class=\"btn btn-secondary\" id=\"configBtn\" onclick=\"openConfig()\">
          ⚙ <span data-i18n=\"btn.config\">CONFIG</span>
        </button>
      </div>
    </div>
  </aside>"
}

// =============================================================================
// MAIN CONTENT
// =============================================================================

fn render_main() -> String {
  "<main class=\"main\">
    <!-- Strategy Selector -->
    <div class=\"strategy-selector\">
      <button class=\"strategy-btn active\" data-strategy=\"passive_fees\" onclick=\"selectStrategy('passive_fees')\">
        💤 <span data-i18n=\"strategy.passiveFees\">Passive Fees</span>
      </button>
      <button class=\"strategy-btn\" data-strategy=\"market_making\" onclick=\"selectStrategy('market_making')\">
        📈 <span data-i18n=\"strategy.marketMaking\">Market Making</span>
      </button>
      <button class=\"strategy-btn\" data-strategy=\"arbitrage\" onclick=\"selectStrategy('arbitrage')\">
        ⚡ <span data-i18n=\"strategy.arbitrage\">Arbitrage</span>
      </button>
      <button class=\"strategy-btn\" data-strategy=\"hybrid\" onclick=\"selectStrategy('hybrid')\">
        🔥 <span data-i18n=\"strategy.hybrid\">Hybrid</span>
      </button>
    </div>

    <!-- Profit Dashboard -->
    <div class=\"card profit-dashboard\">
      <div class=\"card-header\">
        <span class=\"card-icon\">💵</span>
        <span class=\"card-title\" data-i18n=\"main.profitDashboard\">Profit Dashboard</span>
      </div>
      <div class=\"card-body\">
        <div class=\"profit-grid\">
          <div class=\"profit-card today\">
            <span class=\"profit-label\" data-i18n=\"profit.today\">Today</span>
            <span class=\"profit-value\" id=\"profitToday\">$0.00</span>
          </div>
          <div class=\"profit-card week\">
            <span class=\"profit-label\" data-i18n=\"profit.thisWeek\">This Week</span>
            <span class=\"profit-value\" id=\"profitWeek\">$0.00</span>
          </div>
          <div class=\"profit-card month\">
            <span class=\"profit-label\" data-i18n=\"profit.thisMonth\">This Month</span>
            <span class=\"profit-value\" id=\"profitMonth\">$0.00</span>
          </div>
          <div class=\"profit-card total\">
            <span class=\"profit-label\" data-i18n=\"profit.totalEarned\">Total Earned</span>
            <span class=\"profit-value\" id=\"profitTotal\">$0.00</span>
          </div>
        </div>
        <div class=\"profit-breakdown\">
          <div class=\"breakdown-item\">
            <span class=\"breakdown-label\" data-i18n=\"profit.platformFees\">Platform Fees:</span>
            <span class=\"breakdown-value\" id=\"feesEarned\">$0.00</span>
          </div>
          <div class=\"breakdown-item\">
            <span class=\"breakdown-label\" data-i18n=\"profit.spreadProfit\">Spread Profit:</span>
            <span class=\"breakdown-value\" id=\"spreadProfit\">$0.00</span>
          </div>
          <div class=\"breakdown-item\">
            <span class=\"breakdown-label\" data-i18n=\"profit.arbitrageProfit\">Arbitrage Profit:</span>
            <span class=\"breakdown-value\" id=\"arbProfit\">$0.00</span>
          </div>
        </div>

        <!-- Charts Section -->
        <div class=\"charts-container\">
          <div class=\"chart-wrapper chart-line\">
            <h4 class=\"chart-title\" data-i18n=\"chart.weeklyProfit\">📈 7-Day Profit</h4>
            <div class=\"chart-canvas-wrapper\">
              <canvas id=\"profitLineChart\"></canvas>
            </div>
          </div>
          <div class=\"chart-wrapper chart-pie\">
            <h4 class=\"chart-title\" data-i18n=\"chart.profitBreakdown\">📊 Profit Breakdown</h4>
            <div class=\"chart-canvas-wrapper pie\">
              <canvas id=\"profitPieChart\"></canvas>
            </div>
          </div>
        </div>
      </div>
    </div>

    <!-- Active Orders Table -->
    <div class=\"card orders-card\">
      <div class=\"card-header\">
        <span class=\"card-icon\">📋</span>
        <span class=\"card-title\" data-i18n=\"main.activeOrders\">Active Orders</span>
        <button class=\"btn btn-sm\" onclick=\"refreshOrders()\">↻ <span data-i18n=\"btn.refresh\">Refresh</span></button>
      </div>
      <div class=\"card-body\">
        <table class=\"orders-table\">
          <thead>
            <tr>
              <th data-i18n=\"table.id\">ID</th>
              <th data-i18n=\"table.type\">Type</th>
              <th data-i18n=\"table.crypto\">Crypto</th>
              <th data-i18n=\"table.amount\">Amount</th>
              <th data-i18n=\"table.fiat\">Fiat</th>
              <th data-i18n=\"table.rate\">Rate</th>
              <th data-i18n=\"table.status\">Status</th>
              <th data-i18n=\"table.actions\">Actions</th>
            </tr>
          </thead>
          <tbody id=\"ordersBody\">
            <tr class=\"empty-row\">
              <td colspan=\"8\" data-i18n=\"table.noOrders\">No active orders</td>
            </tr>
          </tbody>
        </table>
      </div>
    </div>

    <!-- Arbitrage Opportunities -->
    <div class=\"card arbitrage-card\">
      <div class=\"card-header\">
        <span class=\"card-icon\">⚡</span>
        <span class=\"card-title\" data-i18n=\"main.arbitrageOpportunities\">Arbitrage Opportunities</span>
        <select id=\"arbCrypto\" class=\"arb-select\" onchange=\"scanArbitrage()\">
          <option value=\"TON\">TON</option>
          <option value=\"USDT\">USDT</option>
          <option value=\"NOT\">NOT</option>
        </select>
        <select id=\"arbFiat\" class=\"arb-select\" onchange=\"scanArbitrage()\">
          <option value=\"RUB\">RUB</option>
          <option value=\"THB\">THB</option>
          <option value=\"USD\">USD</option>
        </select>
        <button class=\"btn btn-sm\" onclick=\"scanArbitrage()\">🔍 <span data-i18n=\"btn.scan\">Scan</span></button>
      </div>
      <div class=\"card-body\">
        <div class=\"arb-grid\" id=\"arbGrid\">
          <div class=\"arb-empty\" data-i18n=\"arb.clickScan\">
            Click \"Scan\" to find arbitrage opportunities
          </div>
        </div>
      </div>
    </div>

    <!-- Earning Control Panel -->
    <div class=\"card earning-panel\">
      <div class=\"card-header\">
        <span class=\"card-icon\">💰</span>
        <span class=\"card-title\">Earning Control</span>
      </div>
      <div class=\"card-body\">
        <!-- Maker Bot Section -->
        <div class=\"earning-section\">
          <div class=\"section-header\">
            <span class=\"section-icon\">🤖</span>
            <span class=\"section-title\">Maker Bot</span>
            <label class=\"toggle\">
              <input type=\"checkbox\" id=\"makerEnabled\" onchange=\"toggleMaker()\">
              <span class=\"slider\"></span>
            </label>
          </div>
          <div class=\"maker-prices\" id=\"makerPrices\">
            <div class=\"price-row\">
              <span class=\"price-label\">Buy Price:</span>
              <span class=\"price-value buy\" id=\"makerBuyPrice\">--</span>
            </div>
            <div class=\"price-row\">
              <span class=\"price-label\">Sell Price:</span>
              <span class=\"price-value sell\" id=\"makerSellPrice\">--</span>
            </div>
            <div class=\"price-row\">
              <span class=\"price-label\">Spread:</span>
              <span class=\"price-value\" id=\"makerSpread\">--</span>
            </div>
            <div class=\"price-row profit\">
              <span class=\"price-label\">Est. Profit/1K:</span>
              <span class=\"price-value\" id=\"makerProfit\">--</span>
            </div>
          </div>
          <div class=\"maker-controls\">
            <input type=\"range\" id=\"spreadSlider\" min=\"0.5\" max=\"3\" step=\"0.1\" value=\"1.5\" onchange=\"updateSpread()\">
            <span class=\"spread-display\"><span id=\"spreadValue\">1.5</span>%</span>
          </div>
          <button class=\"btn btn-primary btn-block\" onclick=\"refreshMakerPrices()\">🔄 Refresh Prices</button>
        </div>

        <!-- Alerts Section -->
        <div class=\"earning-section\">
          <div class=\"section-header\">
            <span class=\"section-icon\">🔔</span>
            <span class=\"section-title\">Spread Alerts</span>
            <label class=\"toggle\">
              <input type=\"checkbox\" id=\"alertsEnabled\" onchange=\"toggleAlerts()\">
              <span class=\"slider\"></span>
            </label>
          </div>
          <div class=\"alerts-config\">
            <div class=\"config-row\">
              <span>Min Spread:</span>
              <select id=\"minSpreadSelect\" onchange=\"updateMinSpread()\">
                <option value=\"0.3\">0.3%</option>
                <option value=\"0.5\" selected>0.5%</option>
                <option value=\"1.0\">1.0%</option>
                <option value=\"1.5\">1.5%</option>
              </select>
            </div>
          </div>
          <div class=\"alerts-list\" id=\"alertsList\">
            <div class=\"alert-empty\">No alerts yet</div>
          </div>
          <button class=\"btn btn-secondary btn-block\" onclick=\"scanAlerts()\">🔍 Scan Alerts</button>
        </div>

        <!-- Executor Section -->
        <div class=\"earning-section\">
          <div class=\"section-header\">
            <span class=\"section-icon\">⚡</span>
            <span class=\"section-title\">Auto Executor</span>
            <span class=\"badge badge-warning\">DRY RUN</span>
          </div>
          <div class=\"executor-stats\" id=\"executorStats\">
            <div class=\"stat-row\">
              <span>Status:</span>
              <span class=\"status-badge\" id=\"executorStatus\">Ready</span>
            </div>
            <div class=\"stat-row\">
              <span>Total Profit:</span>
              <span id=\"executorProfit\">$0.00</span>
            </div>
            <div class=\"stat-row\">
              <span>Trades:</span>
              <span id=\"executorTrades\">0</span>
            </div>
          </div>
          <button class=\"btn btn-accent btn-block\" onclick=\"simulateTrade()\">🎯 Simulate Trade</button>
        </div>
      </div>
    </div>
  </main>"
}

// =============================================================================
// ACTIVITY LOG
// =============================================================================

fn render_activity_log() -> String {
  "<div class=\"activity-log\">
    <div class=\"log-header\">
      <span class=\"log-icon\">📜</span>
      <span class=\"log-title\" data-i18n=\"log.liveActivity\">Live Activity</span>
      <button class=\"btn btn-xs\" onclick=\"clearLog()\" data-i18n=\"btn.clear\">Clear</button>
    </div>
    <div class=\"log-body\" id=\"logBody\">
      <div class=\"log-entry system\">
        <span class=\"log-time\">--:--:--</span>
        <span class=\"log-text\" data-i18n=\"log.waitingConnection\">Waiting for connection...</span>
      </div>
    </div>
  </div>"
}

// =============================================================================
// CSS STYLES
// =============================================================================

fn styles() -> String {
  ":root {
  --bg-primary: #0a0a0a;
  --bg-secondary: #111111;
  --bg-card: #1a1a2e;
  --bg-hover: #252550;
  --text-primary: #e0e0e0;
  --text-secondary: #8b8b8b;
  --text-muted: #555555;
  --accent-green: #00ff88;
  --accent-red: #ff4444;
  --accent-blue: #00d4ff;
  --accent-orange: #ff9800;
  --accent-purple: #aa00ff;
  --border-color: #2d3748;
  --success: #00ff88;
  --danger: #ff4444;
  --warning: #ffaa00;
  --header-height: 50px;
  --sidebar-width: 250px;
  --log-width: 280px;
}

* { margin: 0; padding: 0; box-sizing: border-box; }

html, body {
  height: 100%;
  overflow: hidden;
}

body {
  font-family: 'JetBrains Mono', 'Fira Code', 'Monaco', monospace;
  background: var(--bg-primary);
  color: var(--text-primary);
  font-size: 14px;
}

.app-container {
  height: 100%;
  position: relative;
}

/* Header - fixed top */
.header {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  height: var(--header-height);
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0 1rem;
  background: var(--bg-secondary);
  border-bottom: 1px solid var(--border-color);
  z-index: 100;
  overflow: hidden;
}

.header-left {
  display: flex;
  align-items: center;
  gap: 1rem;
  flex-shrink: 1;
  min-width: 0;
}

.logo {
  display: flex;
  align-items: center;
  gap: 0.4rem;
  flex-shrink: 0;
}

.logo-icon { font-size: 1.2rem; }
.logo-text { font-size: 1rem; font-weight: bold; color: var(--accent-green); }

.nav-links {
  display: flex;
  gap: 0.25rem;
  flex-shrink: 1;
  min-width: 0;
}

.nav-link {
  color: var(--text-secondary);
  text-decoration: none;
  padding: 0.35rem 0.5rem;
  border-radius: 4px;
  transition: all 0.2s;
  font-size: 0.85rem;
  white-space: nowrap;
}

.nav-link:hover { color: var(--text-primary); background: var(--bg-hover); }
.nav-link.active { color: var(--accent-green); background: rgba(0, 255, 136, 0.1); }

.header-right {
  display: flex;
  align-items: center;
  gap: 1rem;
  flex-shrink: 0;
}

.status-dot {
  width: 12px;
  height: 12px;
  border-radius: 50%;
  background: var(--accent-red);
  animation: pulse 2s infinite;
  cursor: help;
}

.status-dot.connected {
  background: var(--accent-green);
}

@keyframes pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.5; }
}

/* Content Layout */
.app-content {
  position: fixed;
  top: var(--header-height);
  left: 0;
  right: var(--log-width);
  bottom: 0;
  display: flex;
}

/* Sidebar - fixed left */
.sidebar {
  width: var(--sidebar-width);
  height: 100%;
  background: var(--bg-secondary);
  border-right: 1px solid var(--border-color);
  padding: 0.75rem;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

/* Cards */
.card {
  background: var(--bg-card);
  border: 1px solid var(--border-color);
  border-radius: 10px;
  overflow: hidden;
  box-shadow: 0 2px 6px rgba(0, 0, 0, 0.15);
  flex-shrink: 0;
}

.sidebar .card:hover {
  box-shadow: 0 3px 10px rgba(0, 0, 0, 0.25);
}

.card-header {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.6rem 0.85rem;
  background: rgba(0, 0, 0, 0.25);
  border-bottom: 1px solid var(--border-color);
  font-size: 0.85rem;
}

.card-icon { font-size: 0.95rem; }
.card-title { font-weight: 600; flex: 1; }

/* Arbitrage currency selectors */
.arb-select {
  padding: 0.25rem 0.5rem;
  background: var(--bg-primary);
  border: 1px solid var(--border-color);
  border-radius: 4px;
  color: var(--text-primary);
  font-size: 0.75rem;
  font-weight: 600;
  cursor: pointer;
}
.arb-select:hover { border-color: var(--accent-blue); }
.arb-select:focus { outline: none; border-color: var(--accent-gold); }

/* Earning Panel Styles */
.earning-panel { margin-top: 1rem; }
.earning-section {
  padding: 0.75rem;
  margin-bottom: 0.75rem;
  background: rgba(0, 0, 0, 0.2);
  border-radius: 8px;
  border: 1px solid var(--border-color);
}
.earning-section:last-child { margin-bottom: 0; }
.section-header {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin-bottom: 0.75rem;
  padding-bottom: 0.5rem;
  border-bottom: 1px solid var(--border-color);
}
.section-icon { font-size: 1.1rem; }
.section-title { font-weight: 600; flex: 1; }

/* Toggle Switch */
.toggle {
  position: relative;
  width: 40px;
  height: 20px;
}
.toggle input { opacity: 0; width: 0; height: 0; }
.slider {
  position: absolute;
  cursor: pointer;
  top: 0; left: 0; right: 0; bottom: 0;
  background: var(--bg-tertiary);
  border-radius: 20px;
  transition: 0.3s;
}
.slider:before {
  position: absolute;
  content: '';
  height: 16px;
  width: 16px;
  left: 2px;
  bottom: 2px;
  background: white;
  border-radius: 50%;
  transition: 0.3s;
}
.toggle input:checked + .slider { background: var(--accent-green); }
.toggle input:checked + .slider:before { transform: translateX(20px); }

/* Maker Prices */
.maker-prices { margin-bottom: 0.75rem; }
.price-row {
  display: flex;
  justify-content: space-between;
  padding: 0.3rem 0;
  font-size: 0.85rem;
}
.price-label { color: var(--text-secondary); }
.price-value { font-weight: 600; font-family: monospace; }
.price-value.buy { color: var(--accent-green); }
.price-value.sell { color: var(--accent-red); }
.price-row.profit { border-top: 1px solid var(--border-color); padding-top: 0.5rem; margin-top: 0.25rem; }
.price-row.profit .price-value { color: var(--accent-gold); }

/* Spread Slider */
.maker-controls {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  margin-bottom: 0.75rem;
}
.maker-controls input[type='range'] {
  flex: 1;
  height: 4px;
  background: var(--bg-tertiary);
  border-radius: 2px;
  -webkit-appearance: none;
}
.maker-controls input[type='range']::-webkit-slider-thumb {
  -webkit-appearance: none;
  width: 16px;
  height: 16px;
  background: var(--accent-gold);
  border-radius: 50%;
  cursor: pointer;
}
.spread-display {
  font-weight: 600;
  color: var(--accent-gold);
  min-width: 40px;
}

/* Alerts */
.alerts-config { margin-bottom: 0.5rem; }
.config-row {
  display: flex;
  justify-content: space-between;
  align-items: center;
  font-size: 0.85rem;
}
.config-row select {
  padding: 0.25rem 0.5rem;
  background: var(--bg-primary);
  border: 1px solid var(--border-color);
  border-radius: 4px;
  color: var(--text-primary);
  font-size: 0.8rem;
}
.alerts-list {
  max-height: 120px;
  overflow-y: auto;
  margin-bottom: 0.5rem;
}
.alert-empty {
  text-align: center;
  color: var(--text-secondary);
  font-size: 0.8rem;
  padding: 1rem;
}
.alert-item {
  padding: 0.5rem;
  margin-bottom: 0.25rem;
  background: rgba(255, 215, 0, 0.1);
  border: 1px solid var(--accent-gold);
  border-radius: 4px;
  font-size: 0.75rem;
  font-family: monospace;
}

/* Executor Stats */
.executor-stats { margin-bottom: 0.75rem; }
.stat-row {
  display: flex;
  justify-content: space-between;
  padding: 0.25rem 0;
  font-size: 0.85rem;
}
.stat-row .status-badge {
  padding: 0.15rem 0.5rem;
  font-size: 0.7rem;
}

/* Buttons */
.btn-block { width: 100%; }
.btn-primary {
  background: var(--accent-green);
  color: black;
}
.btn-secondary {
  background: var(--accent-blue);
  color: white;
}
.btn-accent {
  background: var(--accent-gold);
  color: black;
}
.badge {
  padding: 0.15rem 0.4rem;
  border-radius: 4px;
  font-size: 0.65rem;
  font-weight: 600;
}
.badge-warning {
  background: rgba(255, 215, 0, 0.2);
  color: var(--accent-gold);
  border: 1px solid var(--accent-gold);
}

.card-body {
  padding: 0.75rem;
}

/* Agent Status Card */
.status-indicator {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin-bottom: 0.5rem;
  padding: 0.5rem;
  background: rgba(0, 0, 0, 0.2);
  border-radius: 6px;
}

.indicator-dot {
  width: 10px;
  height: 10px;
  border-radius: 50%;
  background: var(--accent-red);
  flex-shrink: 0;
}

.indicator-dot.active { background: var(--accent-green); animation: pulse 1.5s infinite; }
.indicator-dot.inactive { background: var(--accent-red); }

.indicator-text {
  font-weight: 600;
  font-size: 0.85rem;
}

.indicator-text.active { color: var(--accent-green); }
.indicator-text.inactive { color: var(--accent-red); }

.strategy-badge {
  display: inline-block;
  padding: 0.25rem 0.5rem;
  background: rgba(0, 212, 255, 0.15);
  border: 1px solid var(--accent-blue);
  color: var(--accent-blue);
  border-radius: 4px;
  font-size: 0.7rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.3px;
  margin-bottom: 0.25rem;
}

.uptime {
  font-size: 0.75rem;
  color: var(--text-secondary);
}

/* Wallet Card */
.wallet-address {
  font-size: 0.75rem;
  font-family: 'SF Mono', 'Monaco', 'Consolas', monospace;
  color: var(--text-secondary);
  word-break: break-all;
  margin-bottom: 0.5rem;
  padding: 0.5rem;
  background: rgba(0, 0, 0, 0.3);
  border-radius: 6px;
  transition: all 0.3s ease;
  text-align: center;
}

.wallet-address.connected {
  color: var(--accent-green);
  border: 1px solid rgba(0, 255, 136, 0.3);
  cursor: pointer;
  background: rgba(0, 255, 136, 0.05);
}

.wallet-address.not-connected {
  color: var(--text-secondary);
  opacity: 0.7;
  font-style: italic;
}

.connect-wallet-btn {
  width: 100%;
  padding: 0.5rem;
  background: linear-gradient(135deg, #0098EA, #00D4FF);
  border: none;
  border-radius: 6px;
  color: white;
  font-weight: 600;
  font-size: 0.8rem;
  cursor: pointer;
  transition: all 0.3s ease;
}

.connect-wallet-btn:hover {
  box-shadow: 0 4px 12px rgba(0, 152, 234, 0.4);
}

.wallet-balance {
  margin: 0.5rem 0;
  padding: 0.5rem;
  background: rgba(0, 0, 0, 0.2);
  border-radius: 6px;
}

.disconnect-wallet-btn {
  width: 100%;
  padding: 0.4rem;
  background: transparent;
  border: 1px solid rgba(255, 100, 100, 0.5);
  border-radius: 6px;
  color: #ff6464;
  font-size: 0.75rem;
  font-weight: 500;
  cursor: pointer;
  transition: all 0.3s ease;
  margin-top: 0.5rem;
}

.disconnect-wallet-btn:hover {
  background: rgba(255, 100, 100, 0.15);
  border-color: #ff6464;
}

.balance-row {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0.25rem 0;
}

.balance-row:not(:last-child) {
  border-bottom: 1px solid rgba(255, 255, 255, 0.05);
}

.balance-label {
  color: var(--text-secondary);
  font-size: 0.8rem;
}
.balance-value {
  color: var(--accent-green);
  font-weight: 600;
  font-size: 0.85rem;
  font-family: 'SF Mono', 'Monaco', 'Consolas', monospace;
}

/* Stats Card */
.stat-row {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0.35rem 0;
  border-bottom: 1px solid rgba(255, 255, 255, 0.06);
}

.stat-row:last-child { border-bottom: none; }
.stat-label {
  color: var(--text-secondary);
  font-size: 0.8rem;
}
.stat-value {
  color: var(--accent-blue);
  font-weight: 700;
  font-size: 0.9rem;
  font-family: 'SF Mono', 'Monaco', 'Consolas', monospace;
}

/* Controls */
.controls-body {
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
}

.btn {
  padding: 0.5rem 0.75rem;
  border: 1px solid var(--border-color);
  background: var(--bg-secondary);
  color: var(--text-primary);
  border-radius: 6px;
  cursor: pointer;
  font-family: inherit;
  font-size: 0.8rem;
  font-weight: 500;
  transition: all 0.2s ease;
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 0.35rem;
}

.btn:hover:not(:disabled) { border-color: var(--accent-blue); background: var(--bg-hover); }
.btn:disabled { opacity: 0.5; cursor: not-allowed; }

.btn-success { border-color: var(--accent-green); color: var(--accent-green); }
.btn-success:hover:not(:disabled) { background: rgba(0, 255, 136, 0.2); }

.btn-danger { border-color: var(--accent-red); color: var(--accent-red); }
.btn-danger:hover:not(:disabled) { background: rgba(255, 68, 68, 0.2); }

.btn-secondary { border-color: var(--text-secondary); color: var(--text-secondary); }
.btn-secondary:hover:not(:disabled) { border-color: var(--accent-blue); color: var(--accent-blue); }

.btn-sm { padding: 0.3rem 0.5rem; font-size: 0.75rem; }
.btn-xs { padding: 0.2rem 0.4rem; font-size: 0.7rem; }

/* Main Content - scrollable */
.main {
  flex: 1;
  height: 100%;
  padding: 1.5rem;
  overflow-y: scroll;
  overflow-x: hidden;
}

.main > * {
  margin-bottom: 1.25rem;
}

.main > *:last-child {
  margin-bottom: 0;
}

/* Custom scrollbar */
.main::-webkit-scrollbar,
.sidebar::-webkit-scrollbar {
  width: 6px;
}

.main::-webkit-scrollbar-track,
.sidebar::-webkit-scrollbar-track {
  background: transparent;
}

.main::-webkit-scrollbar-thumb,
.sidebar::-webkit-scrollbar-thumb {
  background: var(--border-color);
  border-radius: 3px;
}

.main::-webkit-scrollbar-thumb:hover,
.sidebar::-webkit-scrollbar-thumb:hover {
  background: var(--accent-blue);
}

/* Strategy Selector */
.strategy-selector {
  display: flex;
  gap: 0.5rem;
  flex-wrap: wrap;
}

.strategy-btn {
  padding: 0.5rem 1rem;
  background: var(--bg-card);
  border: 1px solid var(--border-color);
  color: var(--text-secondary);
  border-radius: 20px;
  cursor: pointer;
  font-family: inherit;
  font-size: 0.85rem;
  transition: all 0.2s;
}

.strategy-btn:hover { border-color: var(--accent-blue); color: var(--text-primary); }
.strategy-btn.active {
  background: rgba(0, 255, 136, 0.1);
  border-color: var(--accent-green);
  color: var(--accent-green);
}

/* Profit Dashboard */
.profit-grid {
  display: grid;
  grid-template-columns: repeat(4, 1fr);
  gap: 1rem;
  margin-bottom: 1rem;
}

@media (max-width: 1200px) {
  .profit-grid { grid-template-columns: repeat(2, 1fr); }
}

.profit-card {
  background: linear-gradient(135deg, rgba(0, 0, 0, 0.4) 0%, rgba(0, 0, 0, 0.2) 100%);
  padding: 1.25rem 1rem;
  border-radius: 12px;
  text-align: center;
  border: 1px solid var(--border-color);
  border-left: 4px solid var(--border-color);
  transition: all 0.3s ease;
}

.profit-card:hover {
  transform: translateY(-2px);
  box-shadow: 0 8px 25px rgba(0, 0, 0, 0.3);
}

.profit-card.today {
  border-left-color: var(--accent-green);
  box-shadow: 0 0 20px rgba(0, 255, 136, 0.1);
}
.profit-card.week {
  border-left-color: var(--accent-blue);
  box-shadow: 0 0 20px rgba(0, 212, 255, 0.1);
}
.profit-card.month {
  border-left-color: var(--accent-orange);
  box-shadow: 0 0 20px rgba(255, 152, 0, 0.1);
}
.profit-card.total {
  border-left-color: var(--accent-purple);
  box-shadow: 0 0 20px rgba(170, 0, 255, 0.1);
}

.profit-label {
  display: block;
  font-size: 0.7rem;
  color: var(--text-secondary);
  margin-bottom: 0.5rem;
  text-transform: uppercase;
  letter-spacing: 1px;
}

.profit-value {
  font-size: 1.6rem;
  font-weight: bold;
  background: linear-gradient(90deg, var(--accent-green), var(--accent-blue));
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
}

.profit-card.today .profit-value {
  background: var(--accent-green);
  -webkit-background-clip: text;
  background-clip: text;
}
.profit-card.week .profit-value {
  background: var(--accent-blue);
  -webkit-background-clip: text;
  background-clip: text;
}
.profit-card.month .profit-value {
  background: var(--accent-orange);
  -webkit-background-clip: text;
  background-clip: text;
}
.profit-card.total .profit-value {
  background: linear-gradient(90deg, var(--accent-green), var(--accent-purple));
  -webkit-background-clip: text;
  background-clip: text;
}

.profit-breakdown {
  display: flex;
  flex-wrap: wrap;
  gap: 1.5rem;
  padding-top: 1rem;
  margin-bottom: 0.5rem;
}

.breakdown-item {
  display: flex;
  gap: 0.5rem;
}

.breakdown-label { color: var(--text-secondary); font-size: 0.85rem; }
.breakdown-value { color: var(--accent-blue); font-size: 0.85rem; }

/* Charts Container */
.charts-container {
  display: grid;
  grid-template-columns: 1.5fr 1fr;
  gap: 1rem;
  margin-top: 1rem;
}

.chart-wrapper {
  background: rgba(0, 0, 0, 0.3);
  border: 1px solid var(--border-color);
  border-radius: 8px;
  padding: 1rem;
}

.chart-wrapper.chart-line {
  height: 200px;
}

.chart-wrapper.chart-pie {
  height: 200px;
}

.chart-canvas-wrapper {
  position: relative;
  height: 150px;
  width: 100%;
}

.chart-canvas-wrapper.pie {
  height: 150px;
  display: flex;
  align-items: center;
  justify-content: center;
}

.chart-canvas-wrapper canvas {
  max-width: 100%;
  max-height: 100%;
}

.chart-title {
  color: var(--text-primary);
  font-size: 0.75rem;
  font-weight: 600;
  margin-bottom: 0.5rem;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

@media (max-width: 1200px) {
  .charts-container { grid-template-columns: 1fr 1fr; }
}

@media (max-width: 900px) {
  .charts-container { grid-template-columns: 1fr; }
}

/* Orders Table */
.orders-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.85rem;
}

.orders-table th, .orders-table td {
  padding: 0.75rem;
  text-align: left;
  border-bottom: 1px solid var(--border-color);
}

.orders-table th {
  background: rgba(0, 0, 0, 0.3);
  color: var(--text-secondary);
  font-weight: 600;
  text-transform: uppercase;
  font-size: 0.75rem;
}

.orders-table tbody tr:hover { background: var(--bg-hover); }

.empty-row td {
  text-align: center;
  color: var(--text-muted);
  padding: 2rem;
}

/* Order Status Badges */
.order-status {
  padding: 0.2rem 0.5rem;
  border-radius: 4px;
  font-size: 0.75rem;
  text-transform: uppercase;
}

.order-status.open { background: rgba(0, 212, 255, 0.2); color: var(--accent-blue); }
.order-status.matched { background: rgba(255, 152, 0, 0.2); color: var(--accent-orange); }
.order-status.completed { background: rgba(0, 255, 136, 0.2); color: var(--accent-green); }
.order-status.cancelled { background: rgba(255, 68, 68, 0.2); color: var(--accent-red); }

/* Arbitrage Grid */
.arb-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
  gap: 1rem;
}

.arb-empty {
  grid-column: 1 / -1;
  text-align: center;
  color: var(--text-muted);
  padding: 2rem;
}

.arb-card {
  background: rgba(0, 0, 0, 0.3);
  border: 1px solid var(--border-color);
  border-radius: 8px;
  padding: 1rem;
  transition: all 0.2s;
}

.arb-card:hover { border-color: var(--accent-green); }

.arb-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 0.75rem;
}

.arb-crypto {
  font-weight: bold;
  color: var(--accent-blue);
}

.arb-spread {
  padding: 0.2rem 0.5rem;
  border-radius: 4px;
  font-weight: bold;
}

.arb-spread.positive { background: rgba(0, 255, 136, 0.2); color: var(--accent-green); }
.arb-spread.negative { background: rgba(255, 68, 68, 0.2); color: var(--accent-red); }

.arb-flow {
  font-size: 0.85rem;
  color: var(--text-secondary);
  margin-bottom: 0.75rem;
  line-height: 1.5;
}

.arb-profit {
  font-size: 0.9rem;
  color: var(--accent-green);
  margin-bottom: 0.75rem;
}

.arb-execute-btn {
  width: 100%;
  padding: 0.5rem;
  background: rgba(0, 255, 136, 0.1);
  border: 1px solid var(--accent-green);
  color: var(--accent-green);
  border-radius: 4px;
  cursor: pointer;
  font-family: inherit;
  transition: all 0.2s;
}

.arb-execute-btn:hover { background: rgba(0, 255, 136, 0.3); }

/* Activity Log - fixed bottom */
.activity-log {
  position: fixed;
  top: var(--header-height);
  right: 0;
  bottom: 0;
  width: var(--log-width);
  background: var(--bg-secondary);
  border-left: 1px solid var(--border-color);
  display: flex;
  flex-direction: column;
  z-index: 100;
}

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

.log-body {
  flex: 1;
  overflow-y: auto;
  padding: 0.5rem 1rem;
  font-size: 0.8rem;
  font-family: 'JetBrains Mono', monospace;
}

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

.log-time { color: var(--text-muted); min-width: 70px; }
.log-text { flex: 1; }

.log-entry.system .log-text { color: var(--accent-blue); }
.log-entry.trade .log-text { color: var(--accent-green); }
.log-entry.error .log-text { color: var(--accent-red); }
.log-entry.arb .log-text { color: var(--accent-orange); }

/* Language Toggle */
.lang-toggle {
  padding: 0.4rem 0.7rem;
  background: var(--bg-secondary);
  border: 1px solid var(--border-color);
  color: var(--text-primary);
  border-radius: 6px;
  cursor: pointer;
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.8rem;
  font-weight: 600;
  transition: all 0.2s;
  min-width: 40px;
}

.lang-toggle:hover {
  background: var(--bg-hover);
  border-color: var(--accent-green);
}

/* Responsive */
@media (max-width: 1200px) {
  .nav-link { padding: 0.25rem 0.4rem; font-size: 0.8rem; }
  .user-name { display: none; }
}

@media (max-width: 1024px) {
  .profit-grid { grid-template-columns: repeat(2, 1fr); }
  .profit-breakdown { flex-direction: column; gap: 0.5rem; }
  .logo-text { display: none; }
}

@media (max-width: 768px) {
  .app-content { flex-direction: column; }
  .sidebar { width: 100%; max-height: 300px; flex-direction: row; flex-wrap: wrap; }
  .sidebar .card { flex: 1; min-width: 200px; }
  .nav-links { display: none; }
  .profit-grid { grid-template-columns: 1fr; }
  .logout-btn { display: none; }
}

/* Modal Overlay */
.modal-overlay {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background: rgba(0, 0, 0, 0.85);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 1000;
  backdrop-filter: blur(4px);
}

.modal-content {
  background: var(--bg-card);
  border: 1px solid var(--border-color);
  border-radius: 12px;
  width: 90%;
  max-width: 400px;
  box-shadow: 0 20px 60px rgba(0, 0, 0, 0.5);
  animation: modalSlideIn 0.3s ease;
}

@keyframes modalSlideIn {
  from {
    opacity: 0;
    transform: translateY(-20px) scale(0.95);
  }
  to {
    opacity: 1;
    transform: translateY(0) scale(1);
  }
}

.modal-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1rem 1.25rem;
  border-bottom: 1px solid var(--border-color);
}

.modal-header h3 {
  color: var(--accent-green);
  font-size: 1.1rem;
  font-weight: 600;
}

.modal-close {
  background: none;
  border: none;
  color: var(--text-secondary);
  font-size: 1.5rem;
  cursor: pointer;
  padding: 0;
  line-height: 1;
  transition: color 0.2s;
}

.modal-close:hover {
  color: var(--accent-red);
}

.modal-body {
  padding: 1.25rem;
}

.modal-body p {
  color: var(--text-secondary);
  margin-bottom: 1rem;
  font-size: 0.9rem;
}

.modal-details {
  background: rgba(0, 0, 0, 0.3);
  border-radius: 8px;
  padding: 0.75rem 1rem;
  margin-bottom: 1rem;
}

.detail-row {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0.4rem 0;
}

.detail-row:not(:last-child) {
  border-bottom: 1px solid rgba(255, 255, 255, 0.06);
}

.detail-label {
  color: var(--text-secondary);
  font-size: 0.85rem;
}

.detail-value {
  color: var(--accent-blue);
  font-weight: 600;
  font-size: 0.9rem;
  font-family: 'SF Mono', 'Monaco', monospace;
}

.modal-info {
  display: flex;
  gap: 0.5rem;
  align-items: flex-start;
  background: rgba(0, 152, 234, 0.1);
  border: 1px solid rgba(0, 152, 234, 0.3);
  border-radius: 8px;
  padding: 0.75rem;
  font-size: 0.8rem;
  color: var(--text-secondary);
}

.info-icon {
  flex-shrink: 0;
}

.modal-footer {
  display: flex;
  gap: 0.75rem;
  padding: 1rem 1.25rem;
  border-top: 1px solid var(--border-color);
}

.modal-footer .btn {
  flex: 1;
  padding: 0.6rem 1rem;
  font-weight: 600;
}
"
}

// =============================================================================
// I18N SCRIPT - Translations
// =============================================================================

fn i18n_script() -> String {
  "
// Internationalization (i18n) for P2P Control Panel

const translations = {
  en: {
    // Navigation
    'nav.dashboard': 'Dashboard',
    'nav.messages': 'Messages',
    'nav.events': 'Events',
    'nav.p2p': 'P2P Agent',

    // Status
    'status.connecting': 'Connecting...',
    'status.connected': 'Connected',
    'status.disconnected': 'Disconnected',
    'status.active': 'Active',
    'status.inactive': 'Inactive',

    // Sidebar
    'sidebar.agentStatus': 'Agent Status',
    'sidebar.wallet': 'Wallet',
    'sidebar.quickStats': 'Quick Stats',
    'sidebar.controls': 'Controls',
    'sidebar.uptime': 'Uptime',
    'sidebar.notConnected': 'Not connected',
    'sidebar.connectWallet': 'Connect Wallet',
    'sidebar.disconnectWallet': 'Disconnect',

    // Stats
    'stats.activeOrders': 'Active Orders',
    'stats.tradesToday': 'Trades Today',
    'stats.pendingArb': 'Pending Arb',

    // Buttons
    'btn.start': 'START',
    'btn.stop': 'STOP',
    'btn.config': 'CONFIG',
    'btn.refresh': 'Refresh',
    'btn.scan': 'Scan',
    'btn.clear': 'Clear',
    'btn.execute': 'Execute',

    // Auth
    'auth.loginWithTelegram': 'Login with Telegram',
    'auth.enterPhone': 'Enter your phone number to receive a code',
    'auth.sendCode': 'Send Code',
    'auth.verify': 'Verify',
    'auth.changePhone': 'Change phone',
    'auth.loading': 'Processing...',

    // Strategy
    'strategy.passiveFees': 'Passive Fees',
    'strategy.marketMaking': 'Market Making',
    'strategy.arbitrage': 'Arbitrage',
    'strategy.hybrid': 'Hybrid',

    // Main sections
    'main.profitDashboard': 'Profit Dashboard',
    'main.activeOrders': 'Active Orders',
    'main.arbitrageOpportunities': 'Arbitrage Opportunities',

    // Profit
    'profit.today': 'Today',
    'profit.thisWeek': 'This Week',
    'profit.thisMonth': 'This Month',
    'profit.totalEarned': 'Total Earned',
    'profit.platformFees': 'Platform Fees:',
    'profit.spreadProfit': 'Spread Profit:',
    'profit.arbitrageProfit': 'Arbitrage Profit:',

    // Table
    'table.id': 'ID',
    'table.type': 'Type',
    'table.crypto': 'Crypto',
    'table.amount': 'Amount',
    'table.fiat': 'Fiat',
    'table.rate': 'Rate',
    'table.status': 'Status',
    'table.actions': 'Actions',
    'table.noOrders': 'No active orders',

    // Arbitrage
    'arb.clickScan': 'Click \\\"Scan\\\" to find arbitrage opportunities',
    'arb.buyAt': 'Buy @',
    'arb.sellAt': 'Sell @',
    'arb.estProfit': 'Est. profit:',
    'arb.perThousand': 'per $1000',

    // Charts
    'chart.weeklyProfit': '📈 7-Day Profit',
    'chart.profitBreakdown': '📊 Profit Breakdown',
    'chart.fees': 'Fees',
    'chart.spread': 'Spread',
    'chart.arbitrage': 'Arbitrage',

    // Log
    'log.liveActivity': 'Live Activity',
    'log.waitingConnection': 'Waiting for connection...',
    'log.connected': 'Connected to P2P Agent',
    'log.disconnected': 'Disconnected from server',
    'log.agentStarted': 'Agent started successfully',
    'log.agentStopped': 'Agent stopped',
    'log.scanningArb': 'Scanning for arbitrage opportunities...',
    'log.foundOpps': 'Found {count} opportunities',
    'log.strategyChanged': 'Strategy changed to: {strategy}',
    'log.orderCancelled': 'Order cancelled',
    'log.logCleared': 'Log cleared',

    // Errors
    'error.connectionFailed': 'Connection failed',
    'error.startFailed': 'Failed to start agent',
    'error.stopFailed': 'Failed to stop agent',
  },

  ru: {
    // Navigation
    'nav.dashboard': 'Главная',
    'nav.messages': 'Сообщения',
    'nav.events': 'События',
    'nav.p2p': 'P2P Агент',

    // Status
    'status.connecting': 'Подключение...',
    'status.connected': 'Подключен',
    'status.disconnected': 'Отключен',
    'status.active': 'Активен',
    'status.inactive': 'Неактивен',

    // Sidebar
    'sidebar.agentStatus': 'Статус агента',
    'sidebar.wallet': 'Кошелек',
    'sidebar.quickStats': 'Статистика',
    'sidebar.controls': 'Управление',
    'sidebar.uptime': 'Время работы',
    'sidebar.notConnected': 'Не подключен',
    'sidebar.connectWallet': 'Подключить кошелек',
    'sidebar.disconnectWallet': 'Отключить',

    // Stats
    'stats.activeOrders': 'Активные ордера',
    'stats.tradesToday': 'Сделок сегодня',
    'stats.pendingArb': 'Ожидает арбитраж',

    // Buttons
    'btn.start': 'СТАРТ',
    'btn.stop': 'СТОП',
    'btn.config': 'НАСТРОЙКИ',
    'btn.refresh': 'Обновить',
    'btn.scan': 'Поиск',
    'btn.clear': 'Очистить',
    'btn.execute': 'Выполнить',

    // Auth
    'auth.loginWithTelegram': 'Вход через Telegram',
    'auth.enterPhone': 'Введите номер телефона для получения кода',
    'auth.sendCode': 'Отправить код',
    'auth.verify': 'Подтвердить',
    'auth.changePhone': 'Изменить номер',
    'auth.loading': 'Обработка...',

    // Strategy
    'strategy.passiveFees': 'Пассивные комиссии',
    'strategy.marketMaking': 'Маркет-мейкинг',
    'strategy.arbitrage': 'Арбитраж',
    'strategy.hybrid': 'Гибрид',

    // Main sections
    'main.profitDashboard': 'Панель прибыли',
    'main.activeOrders': 'Активные ордера',
    'main.arbitrageOpportunities': 'Арбитражные возможности',

    // Profit
    'profit.today': 'Сегодня',
    'profit.thisWeek': 'За неделю',
    'profit.thisMonth': 'За месяц',
    'profit.totalEarned': 'Всего заработано',
    'profit.platformFees': 'Комиссии платформы:',
    'profit.spreadProfit': 'Прибыль от спреда:',
    'profit.arbitrageProfit': 'Арбитражная прибыль:',

    // Table
    'table.id': 'ID',
    'table.type': 'Тип',
    'table.crypto': 'Крипта',
    'table.amount': 'Сумма',
    'table.fiat': 'Фиат',
    'table.rate': 'Курс',
    'table.status': 'Статус',
    'table.actions': 'Действия',
    'table.noOrders': 'Нет активных ордеров',

    // Arbitrage
    'arb.clickScan': 'Нажмите \\\"Поиск\\\" для поиска арбитражных возможностей',
    'arb.buyAt': 'Купить на',
    'arb.sellAt': 'Продать на',
    'arb.estProfit': 'Прибыль:',
    'arb.perThousand': 'с $1000',

    // Charts
    'chart.weeklyProfit': '📈 Прибыль за 7 дней',
    'chart.profitBreakdown': '📊 Распределение прибыли',
    'chart.fees': 'Комиссии',
    'chart.spread': 'Спред',
    'chart.arbitrage': 'Арбитраж',

    // Log
    'log.liveActivity': 'Журнал событий',
    'log.waitingConnection': 'Ожидание подключения...',
    'log.connected': 'Подключен к P2P агенту',
    'log.disconnected': 'Отключен от сервера',
    'log.agentStarted': 'Агент успешно запущен',
    'log.agentStopped': 'Агент остановлен',
    'log.scanningArb': 'Поиск арбитражных возможностей...',
    'log.foundOpps': 'Найдено {count} возможностей',
    'log.strategyChanged': 'Стратегия изменена на: {strategy}',
    'log.orderCancelled': 'Ордер отменен',
    'log.logCleared': 'Журнал очищен',

    // Errors
    'error.connectionFailed': 'Ошибка подключения',
    'error.startFailed': 'Не удалось запустить агента',
    'error.stopFailed': 'Не удалось остановить агента',
  }
};

let currentLang = localStorage.getItem('vibee_lang') || 'en';

// Get translation
function t(key, params = {}) {
  let text = translations[currentLang][key] || translations['en'][key] || key;
  // Replace parameters like {count}
  Object.keys(params).forEach(k => {
    text = text.replace('{' + k + '}', params[k]);
  });
  return text;
}

// Set language
function setLang(lang) {
  currentLang = lang;
  localStorage.setItem('vibee_lang', lang);

  // Update toggle button text
  const toggle = document.getElementById('langToggle');
  if (toggle) toggle.textContent = lang.toUpperCase();

  // Update all elements with data-i18n (skip wallet if connected)
  document.querySelectorAll('[data-i18n]').forEach(el => {
    // Skip if wallet is connected (marked with data attribute)
    if (el.hasAttribute('data-wallet-connected')) return;
    const key = el.getAttribute('data-i18n');
    el.textContent = t(key);
  });

  // Log language change
  addLog(t('log.strategyChanged', { strategy: lang === 'ru' ? 'Русский' : 'English' }), 'system');
}

// Toggle language
function toggleLang() {
  const newLang = currentLang === 'en' ? 'ru' : 'en';
  setLang(newLang);
}

// Initialize language
document.addEventListener('DOMContentLoaded', () => {
  const toggle = document.getElementById('langToggle');
  if (toggle) toggle.addEventListener('click', toggleLang);
  setLang(currentLang);
});
"
}

// =============================================================================
// CLIENT JAVASCRIPT
// =============================================================================

fn client_script() -> String {
  "
// P2P Control Panel - Client Script

let ws = null;
let reconnectAttempts = 0;
let isAgentActive = false;
let currentStrategy = 'passive_fees';

// Chart instances
let profitLineChart = null;
let profitPieChart = null;

// Initialize
document.addEventListener('DOMContentLoaded', () => {
  connect();
  // Load initial data
  loadStatus();
  loadStats();
  loadOrders();
  loadChartData();
  initCharts();
  loadActivityLog();

  // Poll activity log every 5 seconds
  setInterval(loadActivityLog, 5000);

  // Poll orders every 10 seconds
  setInterval(loadOrders, 10000);

  // Poll status every 15 seconds
  setInterval(loadStatus, 15000);
});

// WebSocket Connection
function connect() {
  const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
  ws = new WebSocket(protocol + '//' + window.location.host + '/ws/p2p');

  ws.onopen = () => {
    updateConnectionStatus(true);
    reconnectAttempts = 0;
    addLog(t('log.connected'), 'system');

    // Subscribe to all updates
    ws.send(JSON.stringify({
      type: 'subscribe',
      channels: ['status', 'orders', 'arbitrage', 'logs']
    }));
  };

  ws.onmessage = (event) => {
    try {
      const msg = JSON.parse(event.data);
      handleMessage(msg);
    } catch (e) {
      console.error('Parse error:', e);
    }
  };

  ws.onclose = () => {
    updateConnectionStatus(false);
    if (reconnectAttempts < 10) {
      reconnectAttempts++;
      setTimeout(connect, 2000 * reconnectAttempts);
    }
  };

  ws.onerror = (error) => {
    console.error('WebSocket error:', error);
    addLog(t('error.connectionFailed'), 'error');
  };
}

// Handle incoming messages
function handleMessage(msg) {
  switch(msg.type) {
    case 'status':
      updateAgentStatus(msg.data);
      break;
    case 'stats':
      updateStats(msg.data);
      break;
    case 'orders':
      updateOrdersTable(msg.data);
      break;
    case 'order_update':
      handleOrderUpdate(msg.data);
      break;
    case 'arbitrage':
      updateArbitrageCards(msg.data);
      break;
    case 'price_update':
      handlePriceUpdate(msg.data);
      break;
    case 'log':
      addLog(msg.data.message, msg.data.level || 'system');
      break;
    case 'error':
      addLog(msg.data.message || 'Unknown error', 'error');
      break;
  }
}

// Update connection status
function updateConnectionStatus(connected) {
  const dot = document.getElementById('statusDot');

  if (connected) {
    dot.classList.add('connected');
    dot.title = t('status.connected');
  } else {
    dot.classList.remove('connected');
    dot.title = t('status.disconnected');
  }
}

// Update agent status
function updateAgentStatus(data) {
  isAgentActive = data.is_active;

  const indicator = document.getElementById('agentIndicator');
  const dot = indicator.querySelector('.indicator-dot');
  const statusText = document.getElementById('agentStatusText');
  const strategyBadge = document.getElementById('strategyBadge');
  const uptimeDisplay = document.getElementById('uptimeDisplay');

  if (data.is_active) {
    dot.classList.add('active');
    dot.classList.remove('inactive');
    statusText.textContent = t('status.active');
    statusText.classList.add('active');
    statusText.classList.remove('inactive');
    document.getElementById('startBtn').disabled = true;
    document.getElementById('stopBtn').disabled = false;
  } else {
    dot.classList.remove('active');
    dot.classList.add('inactive');
    statusText.textContent = t('status.inactive');
    statusText.classList.remove('active');
    statusText.classList.add('inactive');
    document.getElementById('startBtn').disabled = false;
    document.getElementById('stopBtn').disabled = true;
  }

  strategyBadge.textContent = data.strategy || 'passive_fees';
  currentStrategy = data.strategy || 'passive_fees';

  // Update strategy buttons
  document.querySelectorAll('.strategy-btn').forEach(btn => {
    btn.classList.toggle('active', btn.dataset.strategy === currentStrategy);
  });

  if (data.uptime_seconds > 0) {
    uptimeDisplay.textContent = 'Uptime: ' + formatDuration(data.uptime_seconds);
  } else {
    uptimeDisplay.textContent = 'Uptime: --';
  }

  // Update wallet display
  const walletEl = document.getElementById('walletAddress');
  console.log('[updateAgentStatus] Wallet element:', walletEl, 'Wallet data:', data.wallet);
  if (walletEl && data.wallet && data.wallet !== '') {
    const shortWallet = data.wallet.substring(0, 6) + '...' + data.wallet.substring(data.wallet.length - 4);
    walletEl.textContent = shortWallet;
    walletEl.title = data.wallet;  // Full address on hover
    walletEl.classList.add('connected');
    walletEl.classList.remove('not-connected');
    console.log('[updateAgentStatus] Wallet updated to:', shortWallet);
  } else if (walletEl) {
    walletEl.textContent = t('sidebar.notConnected');
    walletEl.classList.remove('connected');
    walletEl.classList.add('not-connected');
  }

  // Quick stats
  document.getElementById('activeOrders').textContent = data.active_orders || 0;
  document.getElementById('tradesToday').textContent = data.trades_today || 0;
  document.getElementById('pendingArb').textContent = data.pending_arbitrage || 0;
}

// Update statistics
function updateStats(data) {
  document.getElementById('profitToday').textContent = '$' + (data.today_profit || 0).toFixed(2);
  document.getElementById('profitWeek').textContent = '$' + (data.this_week_profit || 0).toFixed(2);
  document.getElementById('profitMonth').textContent = '$' + (data.this_month_profit || 0).toFixed(2);

  const total = (data.total_fees_earned || 0) + (data.total_spread_profit || 0) + (data.total_arbitrage_profit || 0);
  document.getElementById('profitTotal').textContent = '$' + total.toFixed(2);

  document.getElementById('feesEarned').textContent = '$' + (data.total_fees_earned || 0).toFixed(2);
  document.getElementById('spreadProfit').textContent = '$' + (data.total_spread_profit || 0).toFixed(2);
  document.getElementById('arbProfit').textContent = '$' + (data.total_arbitrage_profit || 0).toFixed(2);

  // Update charts with new stats data
  updateCharts(data);
}

// Update orders table
function updateOrdersTable(orders) {
  const tbody = document.getElementById('ordersBody');

  if (!orders || orders.length === 0) {
    tbody.innerHTML = '<tr class=\"empty-row\"><td colspan=\"8\">No active orders</td></tr>';
    return;
  }

  // Get current user telegram_id
  const urlParams = new URLSearchParams(window.location.search);
  const currentUserId = parseInt(urlParams.get('telegram_id') || '144022504');

  tbody.innerHTML = orders.map(order => {
    const isMyOrder = order.seller_id === currentUserId;
    const total = (order.amount * order.rate).toFixed(2);

    // For sell orders - other users can buy
    // For buy orders - other users can sell
    const actionBtn = order.status === 'OPEN' || order.status === 'open'
      ? (order.order_type === 'sell'
        ? `<button class=\"btn btn-xs btn-success\" onclick=\"acceptOrder('${order.id}', 'buy', ${order.amount}, ${order.rate}, '${order.crypto}', '${order.fiat}')\">💰 Купить</button>`
        : `<button class=\"btn btn-xs btn-primary\" onclick=\"acceptOrder('${order.id}', 'sell', ${order.amount}, ${order.rate}, '${order.crypto}', '${order.fiat}')\">💵 Продать</button>`)
      : '';

    return `
      <tr>
        <td>${order.id.substring(0, 10)}...</td>
        <td>${order.order_type}</td>
        <td>${order.crypto}</td>
        <td>${order.amount.toFixed(4)}</td>
        <td>${order.fiat}</td>
        <td>${order.rate.toFixed(2)}</td>
        <td><span class=\"order-status ${order.status}\">${order.status.toUpperCase()}</span></td>
        <td>
          ${actionBtn}
          ${isMyOrder ? `<button class=\"btn btn-xs btn-danger\" onclick=\"cancelOrder('${order.id}')\">✕</button>` : ''}
        </td>
      </tr>
    `;
  }).join('');
}

// Accept order - show confirmation modal
function acceptOrder(orderId, action, amount, rate, crypto, fiat) {
  const total = (amount * rate).toFixed(2);
  const actionText = action === 'buy' ? 'КУПИТЬ' : 'ПРОДАТЬ';
  const actionDesc = action === 'buy'
    ? `Вы покупаете ${amount.toFixed(4)} ${crypto} за ${total} ${fiat}`
    : `Вы продаёте ${amount.toFixed(4)} ${crypto} за ${total} ${fiat}`;

  // Show modal
  document.getElementById('acceptModal').style.display = 'flex';
  document.getElementById('acceptModalTitle').textContent = actionText + ' ' + crypto;
  document.getElementById('acceptModalDesc').textContent = actionDesc;
  document.getElementById('acceptModalAmount').textContent = amount.toFixed(4) + ' ' + crypto;
  document.getElementById('acceptModalTotal').textContent = total + ' ' + fiat;
  document.getElementById('acceptModalRate').textContent = rate.toFixed(2) + ' ' + fiat + '/' + crypto;

  // Store order data for confirmation
  window.pendingAccept = { orderId, action, amount, rate, crypto, fiat };
}

// Close accept modal
function closeAcceptModal() {
  document.getElementById('acceptModal').style.display = 'none';
  window.pendingAccept = null;
}

// Confirm accept order
async function confirmAcceptOrder() {
  if (!window.pendingAccept) return;

  const { orderId, action, amount, rate, crypto, fiat } = window.pendingAccept;
  const urlParams = new URLSearchParams(window.location.search);
  const telegramId = urlParams.get('telegram_id') || '144022504';

  addLog(`${action === 'buy' ? 'Покупка' : 'Продажа'} ${amount.toFixed(4)} ${crypto}...`, 'trade');

  try {
    const resp = await fetch('/api/v1/p2p/order/accept', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        order_id: orderId,
        buyer_telegram_id: parseInt(telegramId),
        action: action
      })
    });
    const data = await resp.json();

    if (data.success) {
      if (data.invoice_url) {
        addLog('✅ Инвойс создан! Открываю оплату...', 'trade');
        window.open(data.invoice_url, '_blank');
      } else {
        addLog('✅ Ордер принят!', 'trade');
      }
      closeAcceptModal();
      loadOrders();
    } else {
      addLog('❌ Ошибка: ' + (data.error || 'Unknown'), 'error');
    }
  } catch (e) {
    addLog('❌ Ошибка: ' + e.message, 'error');
  }
}

// Handle single order update
function handleOrderUpdate(data) {
  addLog(`Order ${data.id.substring(0, 8)}: ${data.status}`, 'trade');
  loadOrders(); // Refresh orders
}

// Update arbitrage cards
function updateArbitrageCards(opportunities) {
  const grid = document.getElementById('arbGrid');

  if (!opportunities || opportunities.length === 0) {
    grid.innerHTML = '<div class=\"arb-empty\">No arbitrage opportunities found</div>';
    return;
  }

  grid.innerHTML = opportunities.map(opp => {
    const spreadClass = opp.spread_percent > 0 ? 'positive' : 'negative';
    const profitPer1k = (opp.spread_percent / 100) * 1000;

    return `
      <div class=\"arb-card\">
        <div class=\"arb-header\">
          <span class=\"arb-crypto\">${opp.crypto}/${opp.fiat}</span>
          <span class=\"arb-spread ${spreadClass}\">+${opp.spread_percent.toFixed(2)}%</span>
        </div>
        <div class=\"arb-flow\">
          Buy @ ${opp.buy_source}: ${opp.buy_price.toFixed(2)} ${opp.fiat}<br>
          → Sell @ ${opp.sell_source}: ${opp.sell_price.toFixed(2)} ${opp.fiat}
        </div>
        <div class=\"arb-profit\">Est. profit: $${profitPer1k.toFixed(2)} per $1000</div>
        <button class=\"arb-execute-btn\" onclick=\"executeArbitrage('${opp.id}')\">
          ⚡ Execute
        </button>
      </div>
    `;
  }).join('');
}

// Handle price update
function handlePriceUpdate(data) {
  // Could update price displays if we add them
  console.log('Price update:', data);
}

// Add log entry
function addLog(message, level = 'system') {
  const logBody = document.getElementById('logBody');
  const now = new Date();
  const time = now.toTimeString().substring(0, 8);

  const entry = document.createElement('div');
  entry.className = 'log-entry ' + level;
  entry.innerHTML = `<span class=\"log-time\">${time}</span><span class=\"log-text\">${escapeHtml(message)}</span>`;

  logBody.insertBefore(entry, logBody.firstChild);

  // Limit to 100 entries
  while (logBody.children.length > 100) {
    logBody.removeChild(logBody.lastChild);
  }
}

// Clear log
function clearLog() {
  document.getElementById('logBody').innerHTML = '';
  addLog(t('log.logCleared'), 'system');
}

// Track last activity ID to avoid duplicates
let lastActivityId = 0;

// Load activity log from server
async function loadActivityLog() {
  try {
    const resp = await fetch('/api/v1/p2p/activity?limit=50');
    const data = await resp.json();

    if (data.success && data.entries) {
      // Get entries newer than lastActivityId
      const newEntries = data.entries.filter(e => e.id > lastActivityId);

      if (newEntries.length > 0) {
        // Update lastActivityId
        lastActivityId = Math.max(...data.entries.map(e => e.id));

        // Add new entries (oldest first, so newest appears at top)
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

// Map activity type to log level
function mapActivityType(type) {
  switch (type) {
    case 'maker': return 'trade';
    case 'arbitrage': return 'arb';
    case 'alert': return 'system';
    case 'error': return 'error';
    case 'system': return 'system';
    default: return 'system';
  }
}

// Utility: format duration
function formatDuration(seconds) {
  if (seconds < 60) return seconds + 's';
  if (seconds < 3600) return Math.floor(seconds / 60) + 'm';
  return Math.floor(seconds / 3600) + 'h ' + Math.floor((seconds % 3600) / 60) + 'm';
}

// Utility: escape HTML
function escapeHtml(text) {
  if (!text) return '';
  return text.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

// =============================================================================
// API CALLS
// =============================================================================

// Load initial status
async function loadStatus() {
  try {
    const resp = await fetch('/api/v1/p2p/status');
    const data = await resp.json();
    console.log('[loadStatus] Response:', data);
    if (data.status) {
      console.log('[loadStatus] Wallet:', data.status.wallet);
      updateAgentStatus(data.status);
    }
    // Refresh wallet balance if connected
    if (connectedWallet) {
      loadWalletBalance(connectedWallet);
    }
  } catch (e) {
    console.error('Failed to load status:', e);
  }
}

// Load initial stats
async function loadStats() {
  try {
    const resp = await fetch('/api/v1/p2p/stats');
    const data = await resp.json();
    if (data.stats) updateStats(data.stats);
  } catch (e) {
    console.error('Failed to load stats:', e);
  }
}

// Load orders
async function loadOrders() {
  try {
    const resp = await fetch('/api/v1/p2p/orders');
    const data = await resp.json();
    if (data.orders) updateOrdersTable(data.orders);
  } catch (e) {
    console.error('Failed to load orders:', e);
  }
}

// Refresh orders
function refreshOrders() {
  addLog(t('btn.refresh') + '...', 'system');
  loadOrders();
}

// Start agent
async function startAgent() {
  addLog(t('btn.start') + '...', 'system');
  try {
    const resp = await fetch('/api/v1/p2p/start', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ strategy: currentStrategy })
    });
    const data = await resp.json();
    if (data.success) {
      addLog(t('log.agentStarted'), 'trade');
      loadStatus();
    } else {
      addLog(t('error.startFailed') + ': ' + (data.error || ''), 'error');
    }
  } catch (e) {
    addLog(t('error.startFailed') + ': ' + e.message, 'error');
  }
}

// Stop agent
async function stopAgent() {
  addLog(t('btn.stop') + '...', 'system');
  try {
    const resp = await fetch('/api/v1/p2p/stop', { method: 'POST' });
    const data = await resp.json();
    if (data.success) {
      addLog(t('log.agentStopped'), 'system');
      loadStatus();
    } else {
      addLog(t('error.stopFailed') + ': ' + (data.error || ''), 'error');
    }
  } catch (e) {
    addLog(t('error.stopFailed') + ': ' + e.message, 'error');
  }
}

// Select strategy
function selectStrategy(strategy) {
  currentStrategy = strategy;
  document.querySelectorAll('.strategy-btn').forEach(btn => {
    btn.classList.toggle('active', btn.dataset.strategy === strategy);
  });
  addLog(t('log.strategyChanged', { strategy: strategy }), 'system');

  // If agent is running, update config
  if (isAgentActive) {
    updateConfig({ strategy: strategy });
  }
}

// Update config
async function updateConfig(config) {
  try {
    const resp = await fetch('/api/v1/p2p/config', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(config)
    });
    const data = await resp.json();
    if (!data.success) {
      addLog('Failed to update config: ' + (data.error || 'Unknown error'), 'error');
    }
  } catch (e) {
    addLog('Error updating config: ' + e.message, 'error');
  }
}

// Open config modal (placeholder)
function openConfig() {
  addLog('Config modal not yet implemented', 'system');
  // TODO: Show config modal
}

// Scan for arbitrage
async function scanArbitrage() {
  const crypto = document.getElementById('arbCrypto').value;
  const fiat = document.getElementById('arbFiat').value;
  addLog(t('log.scanningArb') + ' ' + crypto + '/' + fiat, 'arb');
  try {
    const resp = await fetch('/api/v1/p2p/arbitrage?crypto=' + crypto + '&fiat=' + fiat);
    const data = await resp.json();
    if (data.opportunities) {
      updateArbitrageCards(data.opportunities);
      addLog(t('log.foundOpps', { count: data.opportunities.length }) + ' ' + crypto + '/' + fiat, 'arb');
    }
  } catch (e) {
    addLog(t('error.connectionFailed') + ': ' + e.message, 'error');
  }
}

// Execute arbitrage
async function executeArbitrage(oppId) {
  addLog('Executing arbitrage: ' + oppId, 'arb');
  try {
    const resp = await fetch('/api/v1/p2p/arbitrage/execute', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ opportunity_id: oppId })
    });
    const data = await resp.json();
    if (data.success) {
      addLog('Arbitrage executed successfully!', 'trade');
    } else {
      addLog('Arbitrage failed: ' + (data.error || 'Unknown error'), 'error');
    }
  } catch (e) {
    addLog('Error executing arbitrage: ' + e.message, 'error');
  }
}

// Cancel order
async function cancelOrder(orderId) {
  addLog(t('log.orderCancelled') + ': ' + orderId, 'system');
  try {
    const resp = await fetch('/api/v1/p2p/orders/' + orderId + '/cancel', { method: 'POST' });
    const data = await resp.json();
    if (data.success) {
      addLog(t('log.orderCancelled'), 'system');
      loadOrders();
    } else {
      addLog(t('error.connectionFailed') + ': ' + (data.error || ''), 'error');
    }
  } catch (e) {
    addLog(t('error.connectionFailed') + ': ' + e.message, 'error');
  }
}

// =============================================================================
// EARNING PANEL FUNCTIONS
// =============================================================================

// Maker Bot state
let makerEnabled = false;
let alertsEnabled = false;
let currentSpread = 1.5;

// Refresh maker prices
async function refreshMakerPrices() {
  const crypto = document.getElementById('arbCrypto').value;
  const fiat = document.getElementById('arbFiat').value;
  addLog('Fetching maker prices for ' + crypto + '/' + fiat, 'system');

  try {
    const resp = await fetch('/api/v1/p2p/maker/prices?crypto=' + crypto + '&fiat=' + fiat);
    const data = await resp.json();

    if (data.success && data.recommendation) {
      const rec = data.recommendation;
      document.getElementById('makerBuyPrice').textContent = rec.buy_price.toFixed(2) + ' ' + fiat;
      document.getElementById('makerSellPrice').textContent = rec.sell_price.toFixed(2) + ' ' + fiat;
      document.getElementById('makerSpread').textContent = rec.spread_percent.toFixed(1) + '%';
      document.getElementById('makerProfit').textContent = '$' + rec.estimated_profit_per_1000.toFixed(2);
      addLog('Maker prices updated: ' + rec.buy_price.toFixed(2) + ' / ' + rec.sell_price.toFixed(2), 'trade');
    }
  } catch (e) {
    addLog('Error fetching maker prices: ' + e.message, 'error');
  }
}

// Toggle maker bot
async function toggleMaker() {
  makerEnabled = document.getElementById('makerEnabled').checked;

  // Get telegram_id from URL or use default
  const urlParams = new URLSearchParams(window.location.search);
  const telegramId = urlParams.get('telegram_id') || '144022504';

  try {
    // Save state to backend
    const resp = await fetch('/api/v1/p2p/maker/toggle?telegram_id=' + telegramId + '&enabled=' + makerEnabled, {
      method: 'POST'
    });
    const data = await resp.json();

    if (data.success) {
      if (makerEnabled) {
        addLog('Maker Bot enabled (saved)', 'system');
        refreshMakerPrices();
      } else {
        addLog('Maker Bot disabled (saved)', 'system');
      }
    } else {
      addLog('Error saving maker state: ' + data.error, 'error');
    }
  } catch (e) {
    addLog('Error: ' + e.message, 'error');
    // Still update UI
    if (makerEnabled) {
      addLog('Maker Bot enabled (local only)', 'system');
      refreshMakerPrices();
    } else {
      addLog('Maker Bot disabled (local only)', 'system');
    }
  }
}

// Update spread value
function updateSpread() {
  currentSpread = parseFloat(document.getElementById('spreadSlider').value);
  document.getElementById('spreadValue').textContent = currentSpread.toFixed(1);
}

// Toggle alerts
function toggleAlerts() {
  alertsEnabled = document.getElementById('alertsEnabled').checked;
  if (alertsEnabled) {
    addLog('Spread Alerts enabled', 'system');
    scanAlerts();
  } else {
    addLog('Spread Alerts disabled', 'system');
  }
}

// Update min spread
function updateMinSpread() {
  const minSpread = document.getElementById('minSpreadSelect').value;
  addLog('Min spread set to ' + minSpread + '%', 'system');
}

// Scan for alerts
async function scanAlerts() {
  const crypto = document.getElementById('arbCrypto').value;
  const fiat = document.getElementById('arbFiat').value;
  const minSpread = document.getElementById('minSpreadSelect').value;

  addLog('Scanning alerts for ' + crypto + '/' + fiat, 'arb');

  try {
    const resp = await fetch('/api/v1/p2p/alerts/scan?crypto=' + crypto + '&fiat=' + fiat + '&min_spread=' + minSpread);
    const data = await resp.json();

    const alertsList = document.getElementById('alertsList');

    if (data.success && data.alerts && data.alerts.alerts && data.alerts.alerts.length > 0) {
      alertsList.innerHTML = data.alerts.alerts.map(alert =>
        '<div class=\"alert-item\">' + alert.message + '</div>'
      ).join('');
      addLog('Found ' + data.alerts.count + ' alerts', 'arb');
    } else {
      alertsList.innerHTML = '<div class=\"alert-empty\">No alerts found</div>';
      addLog('No alerts found', 'system');
    }
  } catch (e) {
    addLog('Error scanning alerts: ' + e.message, 'error');
  }
}

// Simulate trade
async function simulateTrade() {
  const crypto = document.getElementById('arbCrypto').value;
  const fiat = document.getElementById('arbFiat').value;

  addLog('Simulating trade for ' + crypto + '/' + fiat, 'trade');

  try {
    const resp = await fetch('/api/v1/p2p/executor/simulate?crypto=' + crypto + '&fiat=' + fiat, { method: 'POST' });
    const data = await resp.json();

    if (data.success && data.simulation) {
      const sim = data.simulation;
      document.getElementById('executorProfit').textContent = '$' + sim.net_profit.toFixed(2);
      document.getElementById('executorTrades').textContent = '1';
      addLog('Simulation: ' + sim.amount_traded.toFixed(2) + ' traded, net profit: $' + sim.net_profit.toFixed(2), 'trade');
    } else {
      addLog('Simulation failed: ' + (data.error || 'No opportunities'), 'error');
    }
  } catch (e) {
    addLog('Error simulating trade: ' + e.message, 'error');
  }
}

// Auto-refresh maker prices every 30s if enabled
setInterval(() => {
  if (makerEnabled) {
    refreshMakerPrices();
  }
  if (alertsEnabled) {
    scanAlerts();
  }
}, 30000);

// =============================================================================
// CHART.JS INTEGRATION
// =============================================================================

// Chart color scheme matching dark theme
const chartColors = {
  green: '#00ff88',
  blue: '#00d4ff',
  orange: '#ff9800',
  purple: '#aa00ff',
  red: '#ff4444',
  text: '#8b8b8b',
  grid: '#2d3748'
};

// Initialize charts - shows real data only
function initCharts() {
  if (typeof Chart === 'undefined') {
    console.warn('Chart.js not loaded yet');
    setTimeout(initCharts, 500);
    return;
  }

  // Empty data - will be filled from API
  const emptyLineData = [0, 0, 0, 0, 0, 0, 0];
  const emptyPieData = [0, 0, 0];

  // 7-day profit line chart
  const lineCtx = document.getElementById('profitLineChart');
  if (lineCtx && !profitLineChart) {
    profitLineChart = new Chart(lineCtx.getContext('2d'), {
      type: 'line',
      data: {
        labels: getLast7Days(),
        datasets: [{
          label: 'Daily Profit ($)',
          data: emptyLineData,
          borderColor: chartColors.green,
          backgroundColor: createGradient(lineCtx, chartColors.green),
          tension: 0.4,
          fill: true,
          pointBackgroundColor: chartColors.green,
          pointBorderColor: '#1a1a2e',
          pointBorderWidth: 2,
          pointRadius: 5,
          pointHoverRadius: 7,
          borderWidth: 3
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        interaction: {
          intersect: false,
          mode: 'index'
        },
        plugins: {
          legend: { display: false },
          tooltip: {
            backgroundColor: 'rgba(26, 26, 46, 0.95)',
            titleColor: '#fff',
            bodyColor: chartColors.green,
            borderColor: chartColors.green,
            borderWidth: 1,
            padding: 12,
            cornerRadius: 8,
            displayColors: false,
            callbacks: {
              label: ctx => '💰 $' + ctx.raw.toFixed(2)
            }
          }
        },
        scales: {
          x: {
            grid: { color: 'rgba(45, 55, 72, 0.5)', drawBorder: false },
            ticks: { color: chartColors.text, font: { size: 11 } }
          },
          y: {
            grid: { color: 'rgba(45, 55, 72, 0.5)', drawBorder: false },
            ticks: {
              color: chartColors.text,
              font: { size: 11 },
              callback: val => '$' + val
            },
            beginAtZero: true
          }
        }
      }
    });
  }

  // Profit breakdown doughnut chart
  const pieCtx = document.getElementById('profitPieChart');
  if (pieCtx && !profitPieChart) {
    profitPieChart = new Chart(pieCtx.getContext('2d'), {
      type: 'doughnut',
      data: {
        labels: [t('chart.fees'), t('chart.spread'), t('chart.arbitrage')],
        datasets: [{
          data: emptyPieData,
          backgroundColor: [chartColors.green, chartColors.blue, chartColors.orange],
          borderColor: '#0a0a0a',
          borderWidth: 3,
          hoverBorderColor: '#fff',
          hoverBorderWidth: 2,
          hoverOffset: 8
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        cutout: '55%',
        plugins: {
          legend: {
            position: 'bottom',
            labels: {
              color: '#e0e0e0',
              padding: 12,
              font: { size: 11 },
              usePointStyle: true,
              pointStyle: 'circle'
            }
          },
          tooltip: {
            backgroundColor: 'rgba(26, 26, 46, 0.95)',
            titleColor: '#fff',
            bodyColor: '#fff',
            borderColor: chartColors.grid,
            borderWidth: 1,
            padding: 12,
            cornerRadius: 8,
            callbacks: {
              label: ctx => {
                const total = ctx.dataset.data.reduce((a, b) => a + b, 0);
                const pct = total > 0 ? ((ctx.raw / total) * 100).toFixed(0) : 0;
                return ' $' + ctx.raw.toFixed(2) + ' (' + pct + '%)';
              }
            }
          }
        }
      }
    });
  }
}

// Create gradient for line chart
function createGradient(ctx, color) {
  const canvas = ctx.getContext ? ctx : ctx.canvas;
  const context = canvas.getContext('2d');
  const gradient = context.createLinearGradient(0, 0, 0, 180);
  gradient.addColorStop(0, color.replace(')', ', 0.3)').replace('rgb', 'rgba'));
  gradient.addColorStop(1, color.replace(')', ', 0.0)').replace('rgb', 'rgba'));
  return 'rgba(0, 255, 136, 0.15)';
}

// Get last 7 day labels
function getLast7Days() {
  const days = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
  const result = [];
  for (let i = 6; i >= 0; i--) {
    const d = new Date();
    d.setDate(d.getDate() - i);
    result.push(days[d.getDay()]);
  }
  return result;
}

// Load chart data from API
async function loadChartData() {
  try {
    const resp = await fetch('/api/v1/p2p/stats');
    const data = await resp.json();
    if (data.stats) {
      updateCharts(data.stats);
    }
  } catch (e) {
    console.error('Failed to load chart data:', e);
  }
}

// Update charts with new data
function updateCharts(stats) {
  // Update pie chart with profit breakdown
  if (profitPieChart) {
    const fees = stats.total_fees_earned || 0;
    const spread = stats.total_spread_profit || 0;
    const arb = stats.total_arbitrage_profit || 0;

    profitPieChart.data.datasets[0].data = [fees, spread, arb];
    profitPieChart.update('none');
  }

  // Update line chart with daily history if available
  if (profitLineChart && stats.daily_profits) {
    profitLineChart.data.datasets[0].data = stats.daily_profits.slice(-7);
    profitLineChart.update('none');
  } else if (profitLineChart) {
    // Demo data when no real data available
    const todayProfit = stats.today_profit || 0;
    const weekProfit = stats.this_week_profit || 0;
    const avgDaily = weekProfit > 0 ? weekProfit / 7 : 0;

    // Generate approximate daily values ending with today
    const dailyData = [];
    for (let i = 6; i > 0; i--) {
      dailyData.push(Math.max(0, avgDaily + (Math.random() - 0.5) * avgDaily));
    }
    dailyData.push(todayProfit);

    profitLineChart.data.datasets[0].data = dailyData;
    profitLineChart.update('none');
  }
}

// Refresh charts when stats update
function refreshCharts() {
  loadChartData();
}

// =============================================================================
// TON CONNECT WALLET
// =============================================================================

let tonConnectUI = null;
let connectedWallet = null;

// Initialize TON Connect
function initTonConnect() {
  try {
    if (typeof TON_CONNECT_UI === 'undefined' && typeof TonConnectUI === 'undefined') {
      console.warn('TON Connect UI not loaded');
      return;
    }

    const TonConnect = window.TON_CONNECT_UI || window.TonConnectUI;

    tonConnectUI = new TonConnect.TonConnectUI({
      manifestUrl: 'https://vibee-mcp.fly.dev/tonconnect-manifest.json',
      buttonRootId: null // We'll handle UI manually
    });

    // Listen for wallet connection status changes
    tonConnectUI.onStatusChange(wallet => {
      if (wallet) {
        onWalletConnected(wallet);
      } else {
        onWalletDisconnected();
      }
    });

    // Check if already connected
    if (tonConnectUI.connected) {
      const wallet = tonConnectUI.wallet;
      if (wallet) {
        onWalletConnected(wallet);
      }
    }

    console.log('[TON Connect] Initialized');
  } catch (e) {
    console.error('[TON Connect] Init error:', e);
  }
}

// Connect wallet button handler
async function connectWallet() {
  try {
    if (!tonConnectUI) {
      initTonConnect();
    }

    if (tonConnectUI) {
      await tonConnectUI.openModal();
    } else {
      // Fallback: manual wallet input
      const address = prompt(t('sidebar.enterWalletAddress') || 'Enter your TON wallet address:');
      if (address && address.length > 40) {
        saveWalletToSession(address);
        updateWalletUI(address);
      }
    }
  } catch (e) {
    console.error('[TON Connect] Connect error:', e);
    addLog('Wallet connection failed: ' + e.message, 'error');
  }
}

// Disconnect wallet (user-initiated)
async function disconnectWallet() {
  try {
    if (tonConnectUI && tonConnectUI.connected) {
      await tonConnectUI.disconnect();
    }

    // Always reset UI when user explicitly disconnects
    resetWalletUI();

    // Clear from session
    fetch('/api/v1/auth/web/wallet', {
      method: 'DELETE',
      credentials: 'include'
    });

    addLog(t('log.walletDisconnected') || 'Wallet disconnected', 'system');
  } catch (e) {
    console.error('[TON Connect] Disconnect error:', e);
  }
}

// Called when wallet is connected
function onWalletConnected(wallet) {
  console.log('[Wallet] onWalletConnected, wallet object:', JSON.stringify(wallet, null, 2));

  // Try different ways to get the address
  let address = null;
  if (wallet.account?.address) {
    address = wallet.account.address;
  } else if (wallet.address) {
    address = wallet.address;
  } else if (typeof wallet === 'string') {
    address = wallet;
  }

  console.log('[Wallet] Extracted address:', address);

  if (address && address.length > 10) {
    // Always update to new wallet, even if different from session
    if (connectedWallet && connectedWallet !== address) {
      console.log('[Wallet] Replacing old wallet:', connectedWallet, 'with:', address);
    }
    connectedWallet = address;
    updateWalletUI(address);
    saveWalletToSession(address);
    // Force reload balance for new address
    loadWalletBalance(address);
    addLog(t('log.walletConnected') || 'Wallet connected', 'success');
  } else {
    console.warn('[Wallet] Could not extract valid address from wallet object');
  }
}

// Called when wallet is disconnected
function onWalletDisconnected() {
  console.log('[Wallet] onWalletDisconnected called, current connectedWallet:', connectedWallet);

  // Only reset UI if we actually had a wallet connected via TON Connect
  // Don't reset if wallet was restored from session
  if (!connectedWallet) {
    console.log('[Wallet] No wallet was connected, resetting UI');
    resetWalletUI();
  } else {
    console.log('[Wallet] Wallet exists from session, keeping UI intact');
  }
}

// Reset wallet UI to disconnected state
function resetWalletUI() {
  connectedWallet = null;
  const walletEl = document.getElementById('walletAddress');
  const connectBtn = document.getElementById('connectWalletBtn');
  const disconnectBtn = document.getElementById('disconnectWalletBtn');
  const balanceSection = document.getElementById('walletBalanceSection');

  if (walletEl) {
    walletEl.textContent = t('sidebar.notConnected');
    walletEl.classList.remove('connected');
    walletEl.title = '';
    // Remove connected marker, restore i18n
    walletEl.removeAttribute('data-wallet-connected');
    walletEl.setAttribute('data-i18n', 'sidebar.notConnected');
  }
  if (connectBtn) connectBtn.style.display = 'block';
  if (disconnectBtn) disconnectBtn.style.display = 'none';
  if (balanceSection) balanceSection.style.display = 'none';
}

// Update wallet UI
function updateWalletUI(address) {
  console.log('[Wallet] updateWalletUI called with:', address);

  // Validate address
  if (!address || typeof address !== 'string' || address.length < 10) {
    console.warn('[Wallet] Invalid address, skipping UI update');
    return;
  }

  const walletEl = document.getElementById('walletAddress');
  const connectBtn = document.getElementById('connectWalletBtn');
  const disconnectBtn = document.getElementById('disconnectWalletBtn');
  const balanceSection = document.getElementById('walletBalanceSection');

  if (walletEl) {
    const shortAddr = address.substring(0, 6) + '...' + address.substring(address.length - 4);
    walletEl.textContent = shortAddr;
    walletEl.title = address;
    walletEl.classList.add('connected');
    // Mark as connected so i18n doesn't overwrite
    walletEl.setAttribute('data-wallet-connected', 'true');
    console.log('[Wallet] Address set to:', shortAddr);
  }
  if (connectBtn) connectBtn.style.display = 'none';
  if (disconnectBtn) disconnectBtn.style.display = 'block';
  if (balanceSection) balanceSection.style.display = 'block';

  // Store for later reference
  connectedWallet = address;

  // Load wallet balance
  loadWalletBalance(address);
}

// Save wallet address to backend session
async function saveWalletToSession(address) {
  try {
    await fetch('/api/v1/auth/web/wallet', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      credentials: 'include',
      body: JSON.stringify({ wallet: address })
    });
  } catch (e) {
    console.error('Failed to save wallet:', e);
  }
}

// Load wallet balance from TON API
async function loadWalletBalance(address) {
  const cacheBuster = Date.now();
  try {
    // Load TON balance (with cache buster to get fresh data)
    const tonResp = await fetch('https://tonapi.io/v2/accounts/' + encodeURIComponent(address) + '?_t=' + cacheBuster);
    if (tonResp.ok) {
      const tonData = await tonResp.json();
      const tonBalance = (tonData.balance || 0) / 1e9;
      document.getElementById('balanceTon').textContent = tonBalance.toFixed(4);
      console.log('[Balance] TON:', tonBalance);
    }

    // Load USDT (jetton) balance (with cache buster)
    const jettonsResp = await fetch('https://tonapi.io/v2/accounts/' + encodeURIComponent(address) + '/jettons?_t=' + cacheBuster);
    if (jettonsResp.ok) {
      const jettonsData = await jettonsResp.json();
      // USDT contract address on TON
      const USDT_ADDRESS = 'EQCxE6mUtQJKFnGfaROTKOt1lZbDiiX1kCixRv7Nw2Id_sDs';
      const usdtJetton = jettonsData.balances?.find(j =>
        j.jetton?.address === USDT_ADDRESS
      );
      const usdtBalance = usdtJetton ? (parseInt(usdtJetton.balance) / 1e6) : 0;
      document.getElementById('balanceUsdt').textContent = usdtBalance.toFixed(2);
      console.log('[Balance] USDT:', usdtBalance, 'jettons:', jettonsData.balances?.length || 0);
    }
  } catch (e) {
    console.error('Failed to load balance:', e);
  }
}

// Initialize TON Connect on page load
// Note: wallet is restored from session in checkAuth() (auth_script)
document.addEventListener('DOMContentLoaded', () => {
  // Initialize TON Connect after a delay to let auth complete first
  setTimeout(initTonConnect, 1000);
});
"
}

// =============================================================================
// AUTHENTICATION UI
// =============================================================================

/// Auth container with Telegram phone + OTP login
fn render_auth_container() -> String {
  "<div class=\"auth-container\" id=\"authContainer\">
    <div class=\"auth-card\">
      <div class=\"auth-logo\">
        <span class=\"auth-logo-icon\">💰</span>
        <span class=\"auth-logo-text\">VIBEE P2P</span>
      </div>
      <h2 class=\"auth-title\" data-i18n=\"auth.loginWithTelegram\">Login with Telegram</h2>
      <p class=\"auth-subtitle\" data-i18n=\"auth.enterPhone\">Enter your phone number to receive a code</p>

      <!-- Step 1: Phone -->
      <div id=\"stepPhone\" class=\"auth-step\">
        <div class=\"input-group\">
          <input type=\"tel\" id=\"phoneInput\" class=\"auth-input\" placeholder=\"+66624014170\" autocomplete=\"tel\">
        </div>
        <button class=\"auth-btn\" onclick=\"sendCode()\" id=\"sendCodeBtn\">
          <span data-i18n=\"auth.sendCode\">Send Code</span>
        </button>
      </div>

      <!-- Step 2: Code (hidden initially) -->
      <div id=\"stepCode\" class=\"auth-step\" style=\"display:none\">
        <p class=\"auth-info\" id=\"codeSentInfo\"></p>
        <div class=\"input-group\">
          <input type=\"text\" id=\"codeInput\" class=\"auth-input\" placeholder=\"12345\" maxlength=\"6\" autocomplete=\"one-time-code\">
        </div>
        <button class=\"auth-btn\" onclick=\"verifyCode()\" id=\"verifyCodeBtn\">
          <span data-i18n=\"auth.verify\">Verify</span>
        </button>
        <button class=\"auth-btn-secondary\" onclick=\"goBackToPhone()\">
          <span data-i18n=\"auth.changePhone\">Change phone</span>
        </button>
      </div>

      <!-- Error message -->
      <div id=\"authError\" class=\"auth-error\" style=\"display:none\"></div>

      <!-- Loading indicator -->
      <div id=\"authLoading\" class=\"auth-loading\" style=\"display:none\">
        <div class=\"spinner\"></div>
        <span data-i18n=\"auth.loading\">Processing...</span>
      </div>

      <!-- Dev Login -->
      <div class=\"dev-login-section\">
        <div class=\"dev-divider\"><span>or</span></div>
        <button class=\"dev-login-btn\" onclick=\"devLogin()\">
          🔧 Dev Login (@playra)
        </button>
      </div>
    </div>
  </div>"
}

/// CSS styles for auth container
fn auth_styles() -> String {
  "
/* Auth Container */
.auth-container {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: linear-gradient(135deg, #0a0a0a 0%, #1a1a2e 50%, #0a0a0a 100%);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 1000;
}

.auth-card {
  background: var(--bg-card);
  border: 1px solid var(--border-color);
  border-radius: 16px;
  padding: 2.5rem;
  width: 100%;
  max-width: 380px;
  box-shadow: 0 25px 50px rgba(0, 0, 0, 0.5);
}

.auth-logo {
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 0.75rem;
  margin-bottom: 1.5rem;
}

.auth-logo-icon {
  font-size: 2.5rem;
}

.auth-logo-text {
  font-size: 1.75rem;
  font-weight: bold;
  color: var(--accent-green);
}

.auth-title {
  text-align: center;
  font-size: 1.25rem;
  color: var(--text-primary);
  margin-bottom: 0.5rem;
}

.auth-subtitle {
  text-align: center;
  font-size: 0.9rem;
  color: var(--text-secondary);
  margin-bottom: 2rem;
}

.auth-step {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.input-group {
  position: relative;
}

.auth-input {
  width: 100%;
  padding: 1rem;
  background: var(--bg-secondary);
  border: 1px solid var(--border-color);
  border-radius: 8px;
  color: var(--text-primary);
  font-family: inherit;
  font-size: 1.1rem;
  text-align: center;
  letter-spacing: 2px;
  transition: all 0.2s;
}

.auth-input:focus {
  outline: none;
  border-color: var(--accent-green);
  box-shadow: 0 0 0 3px rgba(0, 255, 136, 0.1);
}

.auth-input::placeholder {
  color: var(--text-muted);
  letter-spacing: normal;
}

.auth-btn {
  width: 100%;
  padding: 1rem;
  background: linear-gradient(135deg, var(--accent-green) 0%, #00cc6a 100%);
  border: none;
  border-radius: 8px;
  color: #000;
  font-family: inherit;
  font-size: 1rem;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.2s;
}

.auth-btn:hover:not(:disabled) {
  transform: translateY(-2px);
  box-shadow: 0 5px 20px rgba(0, 255, 136, 0.4);
}

.auth-btn:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.auth-btn-secondary {
  width: 100%;
  padding: 0.75rem;
  background: transparent;
  border: 1px solid var(--border-color);
  border-radius: 8px;
  color: var(--text-secondary);
  font-family: inherit;
  font-size: 0.9rem;
  cursor: pointer;
  transition: all 0.2s;
}

.auth-btn-secondary:hover {
  border-color: var(--accent-blue);
  color: var(--text-primary);
}

.auth-info {
  text-align: center;
  font-size: 0.9rem;
  color: var(--text-secondary);
  margin-bottom: 0.5rem;
}

.auth-error {
  margin-top: 1rem;
  padding: 0.75rem;
  background: rgba(255, 68, 68, 0.1);
  border: 1px solid var(--accent-red);
  border-radius: 8px;
  color: var(--accent-red);
  font-size: 0.85rem;
  text-align: center;
}

.auth-loading {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 1rem;
  margin-top: 1.5rem;
  color: var(--text-secondary);
}

.spinner {
  width: 32px;
  height: 32px;
  border: 3px solid var(--border-color);
  border-top-color: var(--accent-green);
  border-radius: 50%;
  animation: spin 1s linear infinite;
}

@keyframes spin {
  to { transform: rotate(360deg); }
}

/* User info in header when logged in */
.user-info {
  display: flex;
  align-items: center;
  gap: 0.6rem;
  padding: 0.3rem 0.5rem 0.3rem 0.4rem;
  background: var(--bg-secondary);
  border: 1px solid var(--border-color);
  border-radius: 20px;
  white-space: nowrap;
}

.user-avatar {
  width: 24px;
  height: 24px;
  background: var(--accent-green);
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.75rem;
  font-weight: 600;
  color: var(--bg-primary);
}

.user-name {
  font-size: 0.85rem;
  font-weight: 500;
  color: var(--text-primary);
  max-width: 100px;
  overflow: hidden;
  text-overflow: ellipsis;
}

.logout-btn {
  padding: 0.25rem 0.5rem;
  background: transparent;
  border: 1px solid var(--accent-red);
  border-radius: 6px;
  color: var(--accent-red);
  font-size: 0.75rem;
  font-weight: 500;
  cursor: pointer;
  transition: all 0.2s;
}

.logout-btn:hover {
  background: var(--accent-red);
  color: var(--bg-primary);
}

/* Dev Login */
.dev-login-section {
  margin-top: 1.5rem;
  padding-top: 1rem;
}

.dev-divider {
  display: flex;
  align-items: center;
  text-align: center;
  margin-bottom: 1rem;
  color: var(--text-muted);
  font-size: 0.8rem;
}

.dev-divider::before,
.dev-divider::after {
  content: '';
  flex: 1;
  border-bottom: 1px solid var(--border-color);
}

.dev-divider span {
  padding: 0 0.75rem;
}

.dev-login-btn {
  width: 100%;
  padding: 0.75rem;
  background: transparent;
  border: 1px dashed var(--accent-yellow);
  border-radius: 8px;
  color: var(--accent-yellow);
  font-family: inherit;
  font-size: 0.9rem;
  cursor: pointer;
  transition: all 0.2s;
}

.dev-login-btn:hover {
  background: rgba(255, 204, 0, 0.1);
  border-style: solid;
}
"
}

/// JavaScript for auth flow
fn auth_script() -> String {
  "
// =============================================================================
// TELEGRAM AUTHENTICATION
// =============================================================================

let authPhoneCodeHash = '';
let authSessionId = '';
let currentUser = null;

// Dev mode user data
const DEV_USER = {
  telegram_id: 144022504,
  first_name: 'Dev',
  username: 'playra',
  phone: '+66624014170'
};

// Check if dev mode is enabled via URL param
function isDevMode() {
  const params = new URLSearchParams(window.location.search);
  return params.get('dev') === 'true' || params.get('dev') === '1';
}

// Dev login - bypass auth
function devLogin() {
  currentUser = DEV_USER;
  showDashboard();
  updateUserInfo(DEV_USER);
  addLog('🔧 Dev mode: logged in as @' + DEV_USER.username, 'system');
}

// Check auth on page load
document.addEventListener('DOMContentLoaded', () => {
  // Auto-login in dev mode
  if (isDevMode()) {
    devLogin();
    return;
  }
  checkAuth();
});

// Check if user is authenticated
async function checkAuth() {
  try {
    const resp = await fetch('/api/v1/auth/web/me', { credentials: 'include' });
    if (resp.ok) {
      const data = await resp.json();
      if (data.authenticated && data.user) {
        currentUser = data.user;
        showDashboard();
        updateUserInfo(data.user);
        // Restore wallet from session
        if (data.wallet && data.wallet !== '') {
          console.log('[Auth] Wallet from session:', data.wallet);
          connectedWallet = data.wallet;
          updateWalletUI(data.wallet);
        }
      } else {
        showAuthForm();
      }
    } else {
      showAuthForm();
    }
  } catch (e) {
    console.error('Auth check failed:', e);
    showAuthForm();
  }
}

// Show auth form
function showAuthForm() {
  document.getElementById('authContainer').style.display = 'flex';
  document.getElementById('dashboardContainer').style.display = 'none';
}

// Show dashboard
function showDashboard() {
  document.getElementById('authContainer').style.display = 'none';
  document.getElementById('dashboardContainer').style.display = 'block';
  // Load data after showing dashboard
  loadStatus();
  loadStats();
  loadOrders();
  loadChartData();
  // Load earning control panel data
  refreshMakerPrices();
  scanAlerts();
}

// Send verification code
async function sendCode() {
  const phone = document.getElementById('phoneInput').value.trim();
  if (!phone) {
    showAuthError('Please enter your phone number');
    return;
  }

  // Normalize phone number
  const normalizedPhone = phone.startsWith('+') ? phone : '+' + phone;

  setAuthLoading(true);
  hideAuthError();

  try {
    const resp = await fetch('/api/v1/auth/web/send-code', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ phone: normalizedPhone })
    });

    const data = await resp.json();

    if (data.success) {
      authPhoneCodeHash = data.phone_code_hash;
      authSessionId = data.session_id;

      // Show code step
      document.getElementById('stepPhone').style.display = 'none';
      document.getElementById('stepCode').style.display = 'flex';
      document.getElementById('codeSentInfo').textContent = 'Code sent to ' + normalizedPhone;
      document.getElementById('codeInput').focus();
    } else {
      showAuthError(data.error || 'Failed to send code');
    }
  } catch (e) {
    console.error('Send code error:', e);
    showAuthError('Connection error. Please try again.');
  } finally {
    setAuthLoading(false);
  }
}

// Verify code
async function verifyCode() {
  const phone = document.getElementById('phoneInput').value.trim();
  const code = document.getElementById('codeInput').value.trim();

  if (!code) {
    showAuthError('Please enter the verification code');
    return;
  }

  const normalizedPhone = phone.startsWith('+') ? phone : '+' + phone;

  setAuthLoading(true);
  hideAuthError();

  try {
    const resp = await fetch('/api/v1/auth/web/verify-code', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      credentials: 'include',
      body: JSON.stringify({
        phone: normalizedPhone,
        code: code,
        phone_code_hash: authPhoneCodeHash,
        session_id: authSessionId
      })
    });

    const data = await resp.json();

    if (data.success && data.user) {
      currentUser = data.user;
      showDashboard();
      updateUserInfo(data.user);
    } else {
      showAuthError(data.error || 'Invalid code');
    }
  } catch (e) {
    console.error('Verify code error:', e);
    showAuthError('Connection error. Please try again.');
  } finally {
    setAuthLoading(false);
  }
}

// Go back to phone input
function goBackToPhone() {
  document.getElementById('stepPhone').style.display = 'flex';
  document.getElementById('stepCode').style.display = 'none';
  document.getElementById('codeInput').value = '';
  hideAuthError();
}

// Logout
async function logout() {
  try {
    await fetch('/api/v1/auth/web/logout', {
      method: 'POST',
      credentials: 'include'
    });
  } catch (e) {
    console.error('Logout error:', e);
  }

  currentUser = null;
  // Hide user info
  const userInfoEl = document.getElementById('userInfo');
  if (userInfoEl) userInfoEl.style.display = 'none';

  document.getElementById('phoneInput').value = '';
  document.getElementById('codeInput').value = '';
  goBackToPhone();
  showAuthForm();
}

// Update user info in header
function updateUserInfo(user) {
  const userInfoEl = document.getElementById('userInfo');
  if (!userInfoEl) return;

  const initial = (user.first_name || 'U')[0].toUpperCase();
  const displayName = user.username ? '@' + user.username : user.first_name;

  userInfoEl.innerHTML = `
    <div class=\"user-avatar\">${initial}</div>
    <span class=\"user-name\">${escapeHtml(displayName)}</span>
    <button class=\"logout-btn\" onclick=\"logout()\">Logout</button>
  `;
  userInfoEl.style.display = 'flex';
}

// Show auth error
function showAuthError(message) {
  const errorEl = document.getElementById('authError');
  errorEl.textContent = message;
  errorEl.style.display = 'block';
}

// Hide auth error
function hideAuthError() {
  document.getElementById('authError').style.display = 'none';
}

// Set loading state
function setAuthLoading(loading) {
  document.getElementById('authLoading').style.display = loading ? 'flex' : 'none';
  document.getElementById('sendCodeBtn').disabled = loading;
  document.getElementById('verifyCodeBtn').disabled = loading;
}

// Handle Enter key in inputs
document.addEventListener('keydown', (e) => {
  if (e.key === 'Enter') {
    const stepPhone = document.getElementById('stepPhone');
    const stepCode = document.getElementById('stepCode');

    if (stepPhone.style.display !== 'none' && document.activeElement === document.getElementById('phoneInput')) {
      sendCode();
    } else if (stepCode.style.display !== 'none' && document.activeElement === document.getElementById('codeInput')) {
      verifyCode();
    }
  }
});
"
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

/// Format float to string with 2 decimals
fn format_float(value: Float) -> String {
  let int_part = float.truncate(value)
  let dec_part = float.truncate({ value -. int.to_float(int_part) } *. 100.0)
  int.to_string(int_part) <> "." <> pad_zero(dec_part)
}

fn pad_zero(n: Int) -> String {
  case n < 10 {
    True -> "0" <> int.to_string(n)
    False -> int.to_string(n)
  }
}
