// Unified Layout Component for VIBEE Web UI
// Provides consistent design across all pages

import gleam/list
import gleam/string

/// Navigation item
pub type NavItem {
  NavItem(
    href: String,
    label: String,
    icon: String,
    active: Bool,
  )
}

/// Page configuration
pub type PageConfig {
  PageConfig(
    title: String,
    active_page: String,
    show_sidebar: Bool,
    show_language_toggle: Bool,
  )
}

/// Render full page with unified layout
pub fn render_page(
  config: PageConfig,
  sidebar_content: String,
  main_content: String,
) -> String {
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>" <> config.title <> " - VIBEE</title>
  <style>" <> unified_css() <> "</style>
</head>
<body>
  " <> render_header(config) <> "
  <div class=\"app-container\">
    " <> case config.show_sidebar {
      True -> "<aside class=\"sidebar\">" <> sidebar_content <> "</aside>"
      False -> ""
    } <> "
    <main class=\"main-content\">
      " <> main_content <> "
    </main>
  </div>
  <script>" <> unified_js() <> "</script>
</body>
</html>"
}

/// Render header with navigation
fn render_header(config: PageConfig) -> String {
  let nav_items = [
    NavItem("/", "Dashboard", "📊", config.active_page == "dashboard"),
    NavItem("/leads", "Leads", "🎯", config.active_page == "leads"),
    NavItem("/p2p", "P2P Agent", "💱", config.active_page == "p2p"),
    NavItem("/factory", "Factory", "🏭", config.active_page == "factory"),
    NavItem("/events", "Events", "📡", config.active_page == "events"),
  ]
  
  let nav_html = list.map(nav_items, fn(item) {
    let active_class = case item.active {
      True -> " active"
      False -> ""
    }
    "<a href=\"" <> item.href <> "\" class=\"nav-link" <> active_class <> "\">"
      <> item.icon <> " " <> item.label <> "</a>"
  })
  |> string.join("")
  
  let lang_toggle = case config.show_language_toggle {
    True -> "<button class=\"lang-toggle\" id=\"langToggle\">EN</button>"
    False -> ""
  }
  
  "<header class=\"header\">
    <div class=\"header-left\">
      <div class=\"logo\">
        <span class=\"logo-icon\">⚡</span>
        <span class=\"logo-text\">VIBEE</span>
      </div>
      <nav class=\"nav-links\">
        " <> nav_html <> "
      </nav>
    </div>
    <div class=\"header-right\">
      " <> lang_toggle <> "
      <span class=\"status-dot\" id=\"statusDot\" title=\"Connected\"></span>
    </div>
  </header>"
}

/// Render card component
pub fn render_card(icon: String, title: String, content: String) -> String {
  "<div class=\"card\">
    <div class=\"card-header\">
      <span class=\"card-icon\">" <> icon <> "</span>
      <span class=\"card-title\">" <> title <> "</span>
    </div>
    <div class=\"card-body\">
      " <> content <> "
    </div>
  </div>"
}

/// Render stat card
pub fn render_stat_card(
  icon: String,
  value: String,
  label: String,
  color: String,
) -> String {
  "<div class=\"stat-card stat-" <> color <> "\">
    <div class=\"stat-icon\">" <> icon <> "</div>
    <div class=\"stat-content\">
      <div class=\"stat-value\">" <> value <> "</div>
      <div class=\"stat-label\">" <> label <> "</div>
    </div>
  </div>"
}

/// Render button
pub fn render_button(
  label: String,
  onclick: String,
  style: String,
) -> String {
  "<button class=\"btn btn-" <> style <> "\" onclick=\"" <> onclick <> "\">"
    <> label <> "</button>"
}

/// Render table
pub fn render_table(headers: List(String), rows: List(List(String))) -> String {
  let headers_html = list.map(headers, fn(h) { "<th>" <> h <> "</th>" })
    |> string.join("")
  
  let rows_html = list.map(rows, fn(row) {
    let cells = list.map(row, fn(cell) { "<td>" <> cell <> "</td>" })
      |> string.join("")
    "<tr>" <> cells <> "</tr>"
  })
  |> string.join("")
  
  "<div class=\"table-container\">
    <table class=\"data-table\">
      <thead><tr>" <> headers_html <> "</tr></thead>
      <tbody>" <> rows_html <> "</tbody>
    </table>
  </div>"
}

/// Unified CSS theme
fn unified_css() -> String {
  ":root {
  /* Colors */
  --bg-primary: #0a0a0f;
  --bg-secondary: #12121a;
  --bg-card: #1a1a2e;
  --text-primary: #ffffff;
  --text-secondary: #888888;
  --accent: #00ffaa;
  --accent-hover: #00dd99;
  --border: #2a2a3e;
  --success: #00ff00;
  --warning: #ffaa00;
  --danger: #ff4444;
  --info: #0088cc;
  
  /* Spacing */
  --spacing-xs: 0.25rem;
  --spacing-sm: 0.5rem;
  --spacing-md: 1rem;
  --spacing-lg: 1.5rem;
  --spacing-xl: 2rem;
  
  /* Border radius */
  --radius-sm: 4px;
  --radius-md: 8px;
  --radius-lg: 12px;
  --radius-xl: 16px;
}

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
  background: var(--bg-primary);
  color: var(--text-primary);
  min-height: 100vh;
  line-height: 1.6;
}

/* Header */
.header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: var(--spacing-lg) var(--spacing-xl);
  background: var(--bg-secondary);
  border-bottom: 1px solid var(--border);
  position: sticky;
  top: 0;
  z-index: 100;
  backdrop-filter: blur(10px);
}

.header-left {
  display: flex;
  align-items: center;
  gap: var(--spacing-xl);
}

.logo {
  display: flex;
  align-items: center;
  gap: var(--spacing-sm);
  font-size: 1.5rem;
  font-weight: 700;
  color: var(--accent);
}

.logo-icon {
  font-size: 2rem;
}

.nav-links {
  display: flex;
  gap: var(--spacing-sm);
}

.nav-link {
  padding: var(--spacing-sm) var(--spacing-md);
  color: var(--text-secondary);
  text-decoration: none;
  border-radius: var(--radius-md);
  transition: all 0.2s;
  font-size: 0.95rem;
}

.nav-link:hover {
  color: var(--text-primary);
  background: rgba(255, 255, 255, 0.05);
}

.nav-link.active {
  color: var(--accent);
  background: rgba(0, 255, 170, 0.1);
  border-bottom: 2px solid var(--accent);
}

.header-right {
  display: flex;
  align-items: center;
  gap: var(--spacing-md);
}

.lang-toggle {
  padding: var(--spacing-sm) var(--spacing-md);
  background: var(--bg-card);
  border: 1px solid var(--border);
  color: var(--text-primary);
  border-radius: var(--radius-md);
  cursor: pointer;
  font-size: 0.85rem;
  transition: all 0.2s;
}

.lang-toggle:hover {
  border-color: var(--accent);
  background: rgba(0, 255, 170, 0.1);
}

.status-dot {
  width: 12px;
  height: 12px;
  border-radius: 50%;
  background: var(--success);
  box-shadow: 0 0 10px var(--success);
  animation: pulse 2s infinite;
}

@keyframes pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.5; }
}

/* Layout */
.app-container {
  display: flex;
  min-height: calc(100vh - 70px);
}

.sidebar {
  width: 280px;
  background: var(--bg-secondary);
  border-right: 1px solid var(--border);
  padding: var(--spacing-lg);
  overflow-y: auto;
}

.main-content {
  flex: 1;
  padding: var(--spacing-xl);
  overflow-y: auto;
}

/* Cards */
.card {
  background: var(--bg-card);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  margin-bottom: var(--spacing-lg);
  overflow: hidden;
  transition: all 0.2s;
}

.card:hover {
  border-color: var(--accent);
  box-shadow: 0 4px 20px rgba(0, 255, 170, 0.1);
}

.card-header {
  display: flex;
  align-items: center;
  gap: var(--spacing-sm);
  padding: var(--spacing-md) var(--spacing-lg);
  border-bottom: 1px solid var(--border);
  background: rgba(255, 255, 255, 0.02);
}

.card-icon {
  font-size: 1.5rem;
}

.card-title {
  font-size: 1rem;
  font-weight: 600;
  color: var(--text-primary);
}

.card-body {
  padding: var(--spacing-lg);
}

/* Stat Cards */
.stat-card {
  display: flex;
  align-items: center;
  gap: var(--spacing-md);
  padding: var(--spacing-lg);
  background: var(--bg-card);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  transition: all 0.2s;
}

.stat-card:hover {
  transform: translateY(-2px);
  box-shadow: 0 8px 30px rgba(0, 0, 0, 0.3);
}

.stat-icon {
  font-size: 2.5rem;
}

.stat-content {
  flex: 1;
}

.stat-value {
  font-size: 2rem;
  font-weight: 700;
  color: var(--accent);
  line-height: 1;
}

.stat-label {
  font-size: 0.85rem;
  color: var(--text-secondary);
  margin-top: var(--spacing-xs);
}

.stat-blue .stat-value { color: var(--info); }
.stat-green .stat-value { color: var(--success); }
.stat-yellow .stat-value { color: var(--warning); }
.stat-red .stat-value { color: var(--danger); }

/* Buttons */
.btn {
  padding: var(--spacing-sm) var(--spacing-lg);
  border: 1px solid var(--border);
  background: var(--bg-card);
  color: var(--text-primary);
  border-radius: var(--radius-md);
  cursor: pointer;
  font-size: 0.95rem;
  font-weight: 500;
  transition: all 0.2s;
  display: inline-flex;
  align-items: center;
  gap: var(--spacing-sm);
}

.btn:hover {
  transform: translateY(-1px);
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
}

.btn-primary {
  background: var(--accent);
  border-color: var(--accent);
  color: var(--bg-primary);
}

.btn-primary:hover {
  background: var(--accent-hover);
}

.btn-success {
  background: var(--success);
  border-color: var(--success);
  color: var(--bg-primary);
}

.btn-warning {
  background: var(--warning);
  border-color: var(--warning);
  color: var(--bg-primary);
}

.btn-danger {
  background: var(--danger);
  border-color: var(--danger);
  color: white;
}

.btn-secondary {
  background: transparent;
  border-color: var(--border);
}

.btn:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

/* Tables */
.table-container {
  background: var(--bg-card);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  overflow: hidden;
}

.data-table {
  width: 100%;
  border-collapse: collapse;
}

.data-table th {
  background: var(--bg-secondary);
  padding: var(--spacing-md) var(--spacing-lg);
  text-align: left;
  font-weight: 600;
  color: var(--text-secondary);
  border-bottom: 1px solid var(--border);
  font-size: 0.85rem;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

.data-table td {
  padding: var(--spacing-md) var(--spacing-lg);
  border-bottom: 1px solid var(--border);
}

.data-table tr:last-child td {
  border-bottom: none;
}

.data-table tr:hover {
  background: rgba(255, 255, 255, 0.02);
}

/* Responsive */
@media (max-width: 1024px) {
  .sidebar {
    width: 240px;
  }
  
  .nav-links {
    display: none;
  }
}

@media (max-width: 768px) {
  .app-container {
    flex-direction: column;
  }
  
  .sidebar {
    width: 100%;
    border-right: none;
    border-bottom: 1px solid var(--border);
  }
  
  .header {
    padding: var(--spacing-md);
  }
  
  .main-content {
    padding: var(--spacing-md);
  }
}
"
}

/// Unified JavaScript
fn unified_js() -> String {
  "
// Language toggle
const langToggle = document.getElementById('langToggle');
if (langToggle) {
  langToggle.addEventListener('click', () => {
    const currentLang = langToggle.textContent;
    langToggle.textContent = currentLang === 'EN' ? 'RU' : 'EN';
  });
}

// Status dot animation
const statusDot = document.getElementById('statusDot');
if (statusDot) {
  setInterval(() => {
    statusDot.style.opacity = statusDot.style.opacity === '0.5' ? '1' : '0.5';
  }, 1000);
}

// Smooth scroll
document.querySelectorAll('a[href^=\"#\"]').forEach(anchor => {
  anchor.addEventListener('click', function (e) {
    e.preventDefault();
    const target = document.querySelector(this.getAttribute('href'));
    if (target) {
      target.scrollIntoView({ behavior: 'smooth' });
    }
  });
});
"
}
