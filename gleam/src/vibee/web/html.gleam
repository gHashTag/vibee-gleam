// HTML page generator for VIBEE Web UI

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import nakai
import nakai/attr
import nakai/html.{type Node}
import vibee/config/telegram_config

/// Message data for rendering
pub type MessageData {
  MessageData(
    id: String,
    group_name: String,
    sender_name: String,
    text: String,
    timestamp: Int,
  )
}

/// Group data for sidebar
pub type GroupData {
  GroupData(
    id: String,
    name: String,
    message_count: Int,
    is_active: Bool,
  )
}

/// Dashboard state
pub type DashboardState {
  DashboardState(
    groups: List(GroupData),
    messages: List(MessageData),
    selected_group: Option(String),
    is_connected: Bool,
  )
}

/// Generate the main HTML page with server-rendered dashboard
pub fn index_page() -> String {
  // Default state with sample data
  let state = default_state()
  render_dashboard(state)
}

/// Generate the events page with real-time event stream (PubSub pattern)
pub fn events_page() -> String {
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>VIBEE - Real-time Events</title>
  <style>
:root {
  --bg-primary: #0a0a0a;
  --bg-secondary: #111111;
  --bg-card: #1a1a1a;
  --text-primary: #00ff00;
  --text-secondary: #888888;
  --accent: #00ffaa;
  --border: #333333;
  --telegram: #0088cc;
  --agent: #ff6600;
  --trigger: #ff00ff;
  --llm: #ffaa00;
  --system: #00aaff;
  --error: #ff4444;
}
* { margin: 0; padding: 0; box-sizing: border-box; }
body {
  font-family: 'JetBrains Mono', 'Fira Code', 'Monaco', monospace;
  background: var(--bg-primary);
  color: var(--text-primary);
  min-height: 100vh;
}
.header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1rem 2rem;
  background: var(--bg-secondary);
  border-bottom: 1px solid var(--border);
  position: sticky;
  top: 0;
  z-index: 100;
}
.title { font-size: 1.3rem; color: var(--accent); display: flex; align-items: center; gap: 0.5rem; }
.live-dot { width: 10px; height: 10px; border-radius: 50%; background: #00ff00; animation: pulse 1.5s infinite; }
@keyframes pulse { 0%, 100% { opacity: 1; box-shadow: 0 0 10px #00ff00; } 50% { opacity: 0.5; box-shadow: none; } }
.status { display: flex; align-items: center; gap: 1rem; }
.status-badge { padding: 0.3rem 0.8rem; border-radius: 20px; font-size: 0.8rem; }
.status-connected { background: rgba(0, 255, 0, 0.1); color: #00ff00; border: 1px solid #00ff00; }
.status-disconnected { background: rgba(255, 68, 68, 0.1); color: #ff4444; border: 1px solid #ff4444; }
.stats { display: flex; gap: 1.5rem; font-size: 0.85rem; color: var(--text-secondary); }
.stat { display: flex; align-items: center; gap: 0.3rem; }
.stat-value { color: var(--accent); font-weight: bold; }
.controls { display: flex; gap: 0.5rem; }
.btn {
  padding: 0.5rem 1rem;
  background: transparent;
  border: 1px solid var(--border);
  color: var(--text-primary);
  cursor: pointer;
  border-radius: 4px;
  font-family: inherit;
  font-size: 0.85rem;
  transition: all 0.2s;
}
.btn:hover { border-color: var(--accent); background: rgba(0, 255, 170, 0.1); }
.btn.active { background: var(--accent); color: #000; border-color: var(--accent); }
.container { padding: 1rem 2rem; }
.events-grid { display: flex; flex-direction: column; gap: 0.5rem; }
.event-card {
  background: var(--bg-card);
  border: 1px solid var(--border);
  border-radius: 8px;
  padding: 1rem;
  animation: slideIn 0.3s ease-out;
  border-left: 3px solid var(--border);
}
@keyframes slideIn { from { opacity: 0; transform: translateY(-10px); } to { opacity: 1; transform: translateY(0); } }
.event-card.telegram_message { border-left-color: var(--telegram); }
.event-card.agent_reply { border-left-color: var(--agent); }
.event-card.trigger_detected { border-left-color: var(--trigger); }
.event-card.llm_request, .event-card.llm_response { border-left-color: var(--llm); }
.event-card.system { border-left-color: var(--system); }
.event-card.error { border-left-color: var(--error); }
.event-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.5rem; }
.event-type { font-size: 0.75rem; text-transform: uppercase; padding: 0.2rem 0.5rem; border-radius: 4px; font-weight: bold; }
.event-type.telegram_message { background: rgba(0, 136, 204, 0.2); color: var(--telegram); }
.event-type.agent_reply { background: rgba(255, 102, 0, 0.2); color: var(--agent); }
.event-type.trigger_detected { background: rgba(255, 0, 255, 0.2); color: var(--trigger); }
.event-type.llm_request, .event-type.llm_response { background: rgba(255, 170, 0, 0.2); color: var(--llm); }
.event-type.system { background: rgba(0, 170, 255, 0.2); color: var(--system); }
.event-type.error { background: rgba(255, 68, 68, 0.2); color: var(--error); }
.event-time { font-size: 0.75rem; color: var(--text-secondary); }
.event-content { font-size: 0.9rem; line-height: 1.5; }
.event-meta { display: flex; gap: 1rem; font-size: 0.8rem; color: var(--text-secondary); margin-top: 0.5rem; }
.meta-item { display: flex; align-items: center; gap: 0.3rem; }
.meta-label { color: var(--text-secondary); }
.meta-value { color: var(--accent); }
.chat-type-badge { padding: 0.15rem 0.4rem; border-radius: 4px; font-size: 0.7rem; text-transform: uppercase; font-weight: bold; }
.chat-type-private { background: rgba(255, 107, 107, 0.2); color: #ff6b6b; }
.chat-type-group { background: rgba(78, 205, 196, 0.2); color: #4ecdc4; }
.chat-type-supergroup { background: rgba(69, 183, 209, 0.2); color: #45b7d1; }
.chat-type-channel { background: rgba(150, 111, 214, 0.2); color: #966fd6; }
.empty-state {
  text-align: center;
  padding: 4rem 2rem;
  color: var(--text-secondary);
}
.empty-icon { font-size: 3rem; margin-bottom: 1rem; }
.filter-bar { display: flex; gap: 0.5rem; margin-bottom: 1rem; flex-wrap: wrap; }
.filter-chip {
  padding: 0.3rem 0.8rem;
  border-radius: 20px;
  font-size: 0.8rem;
  cursor: pointer;
  border: 1px solid var(--border);
  background: transparent;
  color: var(--text-secondary);
  transition: all 0.2s;
}
.filter-chip:hover { border-color: var(--accent); }
.filter-chip.active { background: var(--accent); color: #000; border-color: var(--accent); }
  </style>
</head>
<body>
  <div class=\"header\">
    <div class=\"title\">
      <span class=\"live-dot\" id=\"liveDot\"></span>
      VIBEE Events Stream
    </div>
    <div class=\"stats\">
      <div class=\"stat\">Events: <span class=\"stat-value\" id=\"eventCount\">0</span></div>
      <div class=\"stat\">Subscribers: <span class=\"stat-value\" id=\"subCount\">0</span></div>
    </div>
    <div class=\"status\">
      <span class=\"status-badge status-disconnected\" id=\"statusBadge\">Connecting...</span>
    </div>
    <div class=\"controls\">
      <button class=\"btn\" onclick=\"clearEvents()\">Clear</button>
      <button class=\"btn active\" id=\"autoScrollBtn\" onclick=\"toggleAutoScroll()\">Auto-scroll</button>
      <button class=\"btn\" onclick=\"location.href='/logs'\">File Logs</button>
      <button class=\"btn\" onclick=\"location.href='/'\">Dashboard</button>
    </div>
  </div>
  <div class=\"container\">
    <div class=\"filter-bar\" id=\"filterBar\">
      <span class=\"filter-chip active\" data-filter=\"all\">All</span>
      <span class=\"filter-chip\" data-filter=\"telegram_message\">Telegram</span>
      <span class=\"filter-chip\" data-filter=\"private\" style=\"border-color: #ff6b6b;\">Private</span>
      <span class=\"filter-chip\" data-filter=\"group\" style=\"border-color: #4ecdc4;\">Groups</span>
      <span class=\"filter-chip\" data-filter=\"supergroup\" style=\"border-color: #45b7d1;\">Supergroups</span>
      <span class=\"filter-chip\" data-filter=\"agent_reply\">Agent</span>
      <span class=\"filter-chip\" data-filter=\"trigger_detected\">Triggers</span>
      <span class=\"filter-chip\" data-filter=\"llm_request\">LLM</span>
      <span class=\"filter-chip\" data-filter=\"system\">System</span>
      <span class=\"filter-chip\" data-filter=\"error\">Errors</span>
    </div>
    <div class=\"events-grid\" id=\"eventsGrid\">
      <div class=\"empty-state\">
        <div class=\"empty-icon\">📡</div>
        <div>Waiting for events...</div>
        <div style=\"font-size: 0.8rem; margin-top: 0.5rem;\">Events will appear here in real-time</div>
      </div>
    </div>
  </div>
  <script>
let ws = null;
let autoScroll = true;
let eventCount = 0;
let activeFilter = 'all';
let reconnectAttempts = 0;
const seenEvents = new Set();

function connect() {
  const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
  ws = new WebSocket(protocol + '//' + window.location.host + '/ws/events');

  ws.onopen = () => {
    document.getElementById('statusBadge').className = 'status-badge status-connected';
    document.getElementById('statusBadge').textContent = 'Connected';
    document.getElementById('liveDot').style.background = '#00ff00';
    reconnectAttempts = 0;
  };

  ws.onmessage = (event) => {
    try {
      const data = JSON.parse(event.data);
      if (data.type === 'subscriber_count') {
        document.getElementById('subCount').textContent = data.count;
      } else {
        addEvent(data);
      }
    } catch (e) {
      console.error('Parse error:', e, event.data);
    }
  };

  ws.onclose = () => {
    document.getElementById('statusBadge').className = 'status-badge status-disconnected';
    document.getElementById('statusBadge').textContent = 'Disconnected';
    document.getElementById('liveDot').style.background = '#ff4444';
    if (reconnectAttempts < 10) {
      reconnectAttempts++;
      setTimeout(connect, 2000);
    }
  };
}

function addEvent(data) {
  // Deduplicate events by unique ID
  const eventId = data.type === 'telegram_message'
    ? `${data.chat_id}:${data.msg_id}`
    : `${data.type}:${data.timestamp}`;

  if (seenEvents.has(eventId)) return;
  seenEvents.add(eventId);

  const grid = document.getElementById('eventsGrid');

  // Remove empty state
  const empty = grid.querySelector('.empty-state');
  if (empty) empty.remove();

  eventCount++;
  document.getElementById('eventCount').textContent = eventCount;

  const card = document.createElement('div');
  card.className = 'event-card ' + data.type;
  card.dataset.type = data.type;
  card.dataset.chatType = data.chat_type || '';

  const time = new Date(data.timestamp * 1000).toLocaleTimeString();

  let content = '';
  let meta = '';

  switch(data.type) {
    case 'telegram_message':
      content = data.text || '';
      const chatTypeBadge = data.chat_type ? `<span class=\"meta-item\"><span class=\"chat-type-badge chat-type-${data.chat_type}\">${data.chat_type}</span></span>` : '';
      meta = `${chatTypeBadge}<span class=\"meta-item\"><span class=\"meta-label\">From:</span><span class=\"meta-value\">${escapeHtml(data.sender || '')}</span></span>
              <span class=\"meta-item\"><span class=\"meta-label\">Chat:</span><span class=\"meta-value\">${data.chat_id || ''}</span></span>`;
      break;
    case 'agent_reply':
      content = data.text || '';
      meta = `<span class=\"meta-item\"><span class=\"meta-label\">Chat:</span><span class=\"meta-value\">${data.chat_id || ''}</span></span>`;
      break;
    case 'trigger_detected':
      content = 'Trigger: ' + (data.trigger || '');
      meta = `<span class=\"meta-item\"><span class=\"meta-label\">Chat:</span><span class=\"meta-value\">${data.chat_id || ''}</span></span>`;
      break;
    case 'llm_request':
      content = data.prompt_preview || '';
      meta = `<span class=\"meta-item\"><span class=\"meta-label\">Model:</span><span class=\"meta-value\">${data.model || ''}</span></span>`;
      break;
    case 'llm_response':
      content = data.response_preview || '';
      meta = `<span class=\"meta-item\"><span class=\"meta-label\">Model:</span><span class=\"meta-value\">${data.model || ''}</span></span>
              <span class=\"meta-item\"><span class=\"meta-label\">Status:</span><span class=\"meta-value\">${data.status || ''}</span></span>`;
      break;
    case 'system':
      content = data.message || '';
      meta = `<span class=\"meta-item\"><span class=\"meta-label\">Type:</span><span class=\"meta-value\">${data.event_type || ''}</span></span>`;
      break;
    case 'error':
      content = data.message || '';
      meta = `<span class=\"meta-item\"><span class=\"meta-label\">Error:</span><span class=\"meta-value\">${data.error_type || ''}</span></span>`;
      break;
    default:
      content = JSON.stringify(data);
  }

  card.innerHTML = `
    <div class=\"event-header\">
      <span class=\"event-type ${data.type}\">${data.type.replace('_', ' ')}</span>
      <span class=\"event-time\">${time}</span>
    </div>
    <div class=\"event-content\">${escapeHtml(content)}</div>
    <div class=\"event-meta\">${meta}</div>
  `;

  // Apply filter (check both event type and chat type)
  const chatTypeFilters = ['private', 'group', 'supergroup'];
  let shouldShow = true;
  if (activeFilter !== 'all') {
    if (chatTypeFilters.includes(activeFilter)) {
      // Filter by chat type
      shouldShow = data.chat_type === activeFilter;
    } else {
      // Filter by event type
      shouldShow = data.type === activeFilter;
    }
  }
  if (!shouldShow) {
    card.style.display = 'none';
  }

  grid.insertBefore(card, grid.firstChild);

  // Limit to 200 events
  while (grid.children.length > 200) {
    grid.removeChild(grid.lastChild);
  }

  if (autoScroll) {
    window.scrollTo(0, 0);
  }
}

function escapeHtml(text) {
  if (!text) return '';
  return text.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

function clearEvents() {
  document.getElementById('eventsGrid').innerHTML = `
    <div class=\"empty-state\">
      <div class=\"empty-icon\">📡</div>
      <div>Events cleared. Waiting for new events...</div>
    </div>
  `;
  eventCount = 0;
  seenEvents.clear();
  document.getElementById('eventCount').textContent = '0';
}

function toggleAutoScroll() {
  autoScroll = !autoScroll;
  document.getElementById('autoScrollBtn').classList.toggle('active', autoScroll);
}

// Filter handling
document.getElementById('filterBar').addEventListener('click', (e) => {
  if (e.target.classList.contains('filter-chip')) {
    document.querySelectorAll('.filter-chip').forEach(c => c.classList.remove('active'));
    e.target.classList.add('active');
    activeFilter = e.target.dataset.filter;
    const chatTypeFilters = ['private', 'group', 'supergroup'];

    document.querySelectorAll('.event-card').forEach(card => {
      let shouldShow = false;
      if (activeFilter === 'all') {
        shouldShow = true;
      } else if (chatTypeFilters.includes(activeFilter)) {
        // Filter by chat type
        shouldShow = card.dataset.chatType === activeFilter;
      } else {
        // Filter by event type
        shouldShow = card.dataset.type === activeFilter;
      }
      card.style.display = shouldShow ? 'block' : 'none';
    });
  }
});

connect();
  </script>
</body>
</html>"
}

/// Generate the logs page with real-time log viewer
pub fn logs_page() -> String {
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>VIBEE - Real-time Logs (LIVE)</title>
  <style>
:root {
  --bg-primary: #0a0a0a;
  --bg-secondary: #111111;
  --text-primary: #00ff00;
  --text-secondary: #888888;
  --accent: #00ffaa;
  --border: #333333;
}
* { margin: 0; padding: 0; box-sizing: border-box; }
body {
  font-family: 'JetBrains Mono', 'Fira Code', 'Monaco', monospace;
  background: var(--bg-primary);
  color: var(--text-primary);
  min-height: 100vh;
}
.container { max-width: 100%; padding: 1rem; }
.header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1rem;
  background: var(--bg-secondary);
  border-bottom: 1px solid var(--border);
  position: sticky;
  top: 0;
  z-index: 100;
}
.title { font-size: 1.2rem; color: var(--accent); }
.status { display: flex; align-items: center; gap: 0.5rem; }
.status-dot { width: 10px; height: 10px; border-radius: 50%; background: #ff4444; }
.status-dot.connected { background: #00ff00; animation: pulse 2s infinite; }
@keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.5; } }
.controls { display: flex; gap: 0.5rem; flex-wrap: wrap; }
.btn {
  padding: 0.5rem 1rem;
  background: var(--bg-secondary);
  border: 1px solid var(--border);
  color: var(--text-primary);
  cursor: pointer;
  border-radius: 4px;
  font-family: inherit;
}
.btn:hover { border-color: var(--accent); }
.btn.active { background: var(--accent); color: #000; }
.log-container {
  background: var(--bg-primary);
  padding: 1rem;
  font-size: 0.85rem;
  line-height: 1.6;
  white-space: pre-wrap;
  word-break: break-all;
  overflow-y: auto;
  max-height: calc(100vh - 120px);
}
.log-line { padding: 2px 0; border-left: 2px solid transparent; padding-left: 8px; }
.log-line:hover { background: rgba(0, 255, 0, 0.05); }
.log-line.new { border-left-color: var(--accent); animation: fadeIn 0.3s; }
@keyframes fadeIn { from { opacity: 0; background: rgba(0, 255, 170, 0.2); } to { opacity: 1; } }
.highlight-info { color: #00aaff; }
.highlight-warn { color: #ffaa00; }
.highlight-error { color: #ff4444; font-weight: bold; }
.highlight-success { color: #00ff00; }
.highlight-trigger { color: #ff00ff; font-weight: bold; }
.highlight-llm { color: #ffaa00; }
.file-info { color: var(--text-secondary); font-size: 0.75rem; margin-bottom: 1rem; display: flex; justify-content: space-between; }
.line-count { color: var(--accent); }
.live-indicator { display: inline-block; width: 8px; height: 8px; background: #00ff00; border-radius: 50%; margin-right: 6px; animation: blink 1s infinite; }
@keyframes blink { 0%, 100% { opacity: 1; } 50% { opacity: 0.3; } }
  </style>
</head>
<body>
  <div class=\"header\">
    <div class=\"title\"><span class=\"live-indicator\"></span>VIBEE Logs (LIVE)</div>
    <div class=\"status\">
      <span class=\"status-dot\" id=\"statusDot\"></span>
      <span id=\"statusText\">Connecting...</span>
    </div>
    <div class=\"controls\">
      <button class=\"btn\" onclick=\"clearLogs()\">Clear</button>
      <button class=\"btn\" id=\"modeBtn\" onclick=\"toggleMode()\">Show History</button>
      <button class=\"btn active\" id=\"autoScrollBtn\" onclick=\"toggleAutoScroll()\">Auto-scroll</button>
      <button class=\"btn\" onclick=\"location.href='/app'\">Messages UI</button>
    </div>
  </div>
  <div class=\"container\">
    <div class=\"file-info\">
      <span id=\"fileInfo\">Loading...</span>
      <span class=\"line-count\" id=\"lineCount\">0 lines</span>
    </div>
    <div class=\"log-container\" id=\"logContainer\">
      <div class=\"log-line\">Waiting for new log entries...</div>
    </div>
  </div>
  <script>
let ws = null;
let autoScroll = true;
let reconnectAttempts = 0;
let liveOnlyMode = true;
let lineCount = 0;

function connect() {
  const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
  ws = new WebSocket(protocol + '//' + window.location.host + '/ws/logs');

  ws.onopen = () => {
    document.getElementById('statusDot').classList.add('connected');
    document.getElementById('statusText').textContent = 'LIVE';
    document.getElementById('fileInfo').textContent = 'Real-time log stream';
    reconnectAttempts = 0;

    if (!liveOnlyMode) {
      ws.send('tail');
    }
    startPolling();
  };

  ws.onmessage = (event) => {
    if (event.data === 'pong') return;
    appendLog(event.data, true);
  };

  ws.onclose = () => {
    document.getElementById('statusDot').classList.remove('connected');
    document.getElementById('statusText').textContent = 'Disconnected';
    stopPolling();
    if (reconnectAttempts < 10) {
      reconnectAttempts++;
      setTimeout(connect, 2000);
    }
  };
}

let pollInterval = null;
function startPolling() {
  pollInterval = setInterval(() => {
    if (ws && ws.readyState === WebSocket.OPEN) {
      ws.send('poll');
    }
  }, 1000);
}

function stopPolling() {
  if (pollInterval) {
    clearInterval(pollInterval);
    pollInterval = null;
  }
}

function appendLog(text, isNew) {
  if (!text || text.trim() === '') return;
  const container = document.getElementById('logContainer');

  if (lineCount === 0) {
    container.innerHTML = '';
  }

  const lines = text.split('\\n').reverse();

  lines.forEach(line => {
    if (line.trim() === '') return;
    const div = document.createElement('div');
    div.className = isNew ? 'log-line new' : 'log-line';
    div.innerHTML = highlightLine(line);
    container.insertBefore(div, container.firstChild);
    lineCount++;
  });

  document.getElementById('lineCount').textContent = lineCount + ' lines';

  while (container.children.length > 500) {
    container.removeChild(container.lastChild);
  }

  if (autoScroll) {
    container.scrollTop = 0;
  }
}

function highlightLine(line) {
  let html = escapeHtml(line);
  html = html.replace(/(ERROR|error|Error|FAIL|fail|Failed)/g, '<span class=\"highlight-error\">$1</span>');
  html = html.replace(/(WARN|warn|Warning|WARNING)/g, '<span class=\"highlight-warn\">$1</span>');
  html = html.replace(/(INFO|info|OK|Success|SUCCESS|started|Started)/g, '<span class=\"highlight-info\">$1</span>');
  html = html.replace(/(Trigger found|Processing target|auto-reply)/gi, '<span class=\"highlight-trigger\">$1</span>');
  html = html.replace(/(OpenRouter|LLM|Reply generated|Grok)/gi, '<span class=\"highlight-llm\">$1</span>');
  html = html.replace(/(Message sent|Sending message)/gi, '<span class=\"highlight-success\">$1</span>');
  html = html.replace(/(✅|🎯|🔔|⏭️|🎨|📡|🚀|💬|✓)/g, '<span class=\"highlight-success\">$1</span>');
  html = html.replace(/\\[([^\\]]+)\\]/g, '<span class=\"highlight-info\">[$1]</span>');
  return html;
}

function escapeHtml(text) {
  return text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;');
}

function clearLogs() {
  document.getElementById('logContainer').innerHTML = '<div class=\"log-line\">Logs cleared. Waiting for new entries...</div>';
  lineCount = 0;
  document.getElementById('lineCount').textContent = '0 lines';
}

function toggleAutoScroll() {
  autoScroll = !autoScroll;
  const btn = document.getElementById('autoScrollBtn');
  btn.classList.toggle('active', autoScroll);
}

function toggleMode() {
  liveOnlyMode = !liveOnlyMode;
  const btn = document.getElementById('modeBtn');
  if (liveOnlyMode) {
    btn.textContent = 'Show History';
    clearLogs();
  } else {
    btn.textContent = 'Live Only';
    if (ws && ws.readyState === WebSocket.OPEN) {
      ws.send('tail');
    }
  }
}

connect();
  </script>
</body>
</html>"
}

/// Generate the Lustre app page with WebSocket integration
pub fn lustre_app_page() -> String {
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>VIBEE - All Telegram Messages</title>
  <style>" <> lustre_styles() <> "</style>
</head>
<body class=\"dark-theme\">
  <div id=\"app\"></div>
  <script>" <> lustre_app_js() <> "</script>
</body>
</html>"
}

fn lustre_app_js() -> String {
  "
// VIBEE Telegram Message Viewer
// Fetches data from Go bridge or falls back to Gleam API

const GO_BRIDGE_URL = '" <> telegram_config.bridge_url() <> "';
const GLEAM_API_URL = '';  // Same origin
// Session ID will be fetched from session manager or localStorage
let SESSION_ID = localStorage.getItem('vibee_session_id') || '';

// State
let state = {
  dialogs: [],
  messages: [],
  selectedDialog: null,
  isConnected: false,
  isLoading: true,
  dataSource: 'unknown'
};

// Initialize
async function init() {
  render();
  await loadDialogs();
  await loadAllMessages();
  connectWebSocket();
}

// Fetch dialogs - try Go bridge first, then Gleam API
async function loadDialogs() {
  // Try Go bridge first
  try {
    const resp = await fetch(`${GO_BRIDGE_URL}/api/v1/dialogs?limit=30`, {
      headers: { 'X-Session-ID': SESSION_ID }
    });
    const data = await resp.json();
    if (data.dialogs && data.dialogs.length > 0) {
      state.dialogs = data.dialogs;
      state.isConnected = true;
      state.dataSource = 'telegram-live';
      render();
      return;
    }
  } catch (err) {
    console.log('Go bridge unavailable, trying Gleam API...');
  }

  // Fallback to Gleam API
  try {
    const resp = await fetch(`${GLEAM_API_URL}/api/v1/telegram/dialogs`);
    const data = await resp.json();
    state.dialogs = data.dialogs || [];
    state.isConnected = true;
    state.dataSource = 'gleam-cache';
    render();
  } catch (err) {
    console.error('Failed to load dialogs:', err);
    state.isConnected = false;
    render();
  }
}

// Fetch messages from all groups
async function loadAllMessages() {
  state.isLoading = true;
  render();

  // If using Go bridge live data, fetch from each group
  if (state.dataSource === 'telegram-live') {
    try {
      const groupIds = state.dialogs
        .filter(d => d.type === 'supergroup' || d.type === 'group')
        .slice(0, 10)
        .map(d => d.id);

      let allMessages = [];

      for (const groupId of groupIds) {
        try {
          const resp = await fetch(`${GO_BRIDGE_URL}/api/v1/history/${groupId}?limit=10`, {
            headers: { 'X-Session-ID': SESSION_ID }
          });
          const data = await resp.json();
          const dialog = state.dialogs.find(d => d.id === groupId);
          const groupName = dialog ? dialog.title : 'Unknown';

          if (data.messages) {
            allMessages = allMessages.concat(
              data.messages.map(m => ({ ...m, group_name: groupName, group_id: groupId }))
            );
          }
        } catch (e) {
          console.warn('Failed to load messages for group', groupId, e);
        }
      }

      allMessages.sort((a, b) => new Date(b.date) - new Date(a.date));
      state.messages = allMessages;
      state.selectedDialog = null;
      state.isLoading = false;
      render();
      return;
    } catch (err) {
      console.error('Failed to load from Go bridge:', err);
    }
  }

  // Fallback to Gleam API
  try {
    const resp = await fetch(`${GLEAM_API_URL}/api/v1/telegram/all-messages`);
    const data = await resp.json();
    state.messages = data.messages || [];
    state.selectedDialog = null;
    state.isLoading = false;
    render();
  } catch (err) {
    console.error('Failed to load messages:', err);
    state.isLoading = false;
    render();
  }
}

// Fetch messages for a specific dialog
async function loadMessages(dialogId) {
  state.isLoading = true;
  state.selectedDialog = dialogId;
  render();

  // Try Go bridge if available
  if (state.dataSource === 'telegram-live') {
    try {
      const resp = await fetch(`${GO_BRIDGE_URL}/api/v1/history/${dialogId}?limit=50`, {
        headers: { 'X-Session-ID': SESSION_ID }
      });
      const data = await resp.json();
      const dialog = state.dialogs.find(d => d.id === dialogId);
      const groupName = dialog ? dialog.title : 'Unknown';

      state.messages = (data.messages || []).map(m => ({
        ...m,
        group_name: groupName,
        group_id: dialogId
      }));
      state.isLoading = false;
      render();
      return;
    } catch (err) {
      console.error('Failed to load from Go bridge:', err);
    }
  }

  // Fallback to Gleam API history
  try {
    const resp = await fetch(`${GLEAM_API_URL}/api/v1/telegram/history/${dialogId}`);
    const data = await resp.json();
    const dialog = state.dialogs.find(d => d.id === dialogId);
    const groupName = dialog ? dialog.title : 'Unknown';

    state.messages = (data.messages || []).map(m => ({
      ...m,
      group_name: groupName,
      group_id: dialogId
    }));
    state.isLoading = false;
    render();
  } catch (err) {
    console.error('Failed to load messages:', err);
    state.isLoading = false;
    render();
  }
}

// WebSocket for real-time updates
function connectWebSocket() {
  try {
    const ws = new WebSocket(`ws://localhost:8080/ws`);

    ws.onopen = () => {
      console.log('[WS] Connected');
      ws.send('get_messages');
    };

    ws.onmessage = (event) => {
      try {
        const data = JSON.parse(event.data);
        if (data.type === 'new_message') {
          state.messages.unshift(data.data);
          render();
        }
      } catch (e) {
        console.log('[WS]', event.data);
      }
    };

    ws.onclose = () => {
      console.log('[WS] Disconnected, reconnecting...');
      setTimeout(connectWebSocket, 3000);
    };
  } catch (e) {
    console.error('[WS] Failed to connect:', e);
  }
}

// Render the UI
function render() {
  const app = document.getElementById('app');
  if (!app) return;

  app.innerHTML = `
    <div class=\"app-container\">
      ${renderHeader()}
      <div class=\"app-content\">
        ${renderSidebar()}
        ${renderMain()}
      </div>
    </div>
  `;

  // Attach event listeners
  attachEventListeners();
}

function renderHeader() {
  const statusClass = state.isConnected ? 'status-connected' : 'status-disconnected';
  const statusText = state.isConnected ? 'Connected' : 'Disconnected';
  const sourceLabel = state.dataSource === 'telegram-live' ? '🟢 Live Telegram' : '📦 Cached Data';

  return `
    <header class=\"app-header\">
      <div class=\"header-left\">
        <h1 class=\"app-title\">VIBEE Messages</h1>
        <span class=\"subtitle\">All Telegram groups in one place</span>
        <span class=\"data-source\">${sourceLabel}</span>
      </div>
      <div class=\"header-right\">
        <button class=\"btn-refresh\" onclick=\"loadAllMessages()\">↻ Refresh</button>
        <span class=\"connection-status ${statusClass}\">
          <span class=\"status-dot\"></span>
          ${statusText}
        </span>
      </div>
    </header>
  `;
}

function renderSidebar() {
  const groups = state.dialogs.filter(d => d.type === 'supergroup' || d.type === 'group' || d.type === 'channel');

  return `
    <nav class=\"sidebar\">
      <h2 class=\"sidebar-title\">Groups (${groups.length})</h2>
      <button class=\"btn-all\" onclick=\"loadAllMessages()\">📋 All Messages</button>
      <ul class=\"dialog-list\">
        ${groups.map(d => renderDialogItem(d)).join('')}
      </ul>
    </nav>
  `;
}

function renderDialogItem(dialog) {
  const isActive = state.selectedDialog === dialog.id;
  const activeClass = isActive ? 'active' : '';
  const unreadBadge = dialog.unread_count > 0
    ? `<span class=\"unread-badge\">${dialog.unread_count > 999 ? '999+' : dialog.unread_count}</span>`
    : '';

  return `
    <li class=\"dialog-item ${activeClass}\" data-id=\"${dialog.id}\">
      <div class=\"dialog-info\">
        <span class=\"dialog-title\">${escapeHtml(dialog.title)}</span>
        ${unreadBadge}
      </div>
      <span class=\"dialog-type\">${dialog.type}</span>
    </li>
  `;
}

function renderMain() {
  const title = state.selectedDialog
    ? (state.dialogs.find(d => d.id === state.selectedDialog)?.title || 'Messages')
    : 'All Messages';

  return `
    <main class=\"main-content\">
      <div class=\"message-header\">
        <h2 class=\"section-title\">${escapeHtml(title)}</h2>
        <span class=\"message-count\">${state.messages.length} messages</span>
      </div>
      ${state.isLoading ? '<div class=\"loading\">Loading messages from Telegram...</div>' : renderMessageList()}
    </main>
  `;
}

function renderMessageList() {
  if (state.messages.length === 0) {
    return '<div class=\"no-messages\">No messages yet</div>';
  }

  return `
    <ul class=\"message-list\">
      ${state.messages.map(m => renderMessage(m)).join('')}
    </ul>
  `;
}

function renderMessage(msg) {
  const text = msg.text || '';
  const truncated = text.length > 500 ? text.substring(0, 500) + '...' : text;

  return `
    <li class=\"message-item\" id=\"msg-${msg.id}\">
      <div class=\"message-meta\">
        <span class=\"group-badge\">${escapeHtml(msg.group_name || 'Unknown')}</span>
        <span class=\"sender\">${escapeHtml(msg.from_name || 'Unknown')}</span>
        <span class=\"timestamp\">${formatDate(msg.date)}</span>
      </div>
      <div class=\"message-text\">${escapeHtml(truncated)}</div>
    </li>
  `;
}

function formatDate(dateStr) {
  if (!dateStr) return '';
  try {
    const d = new Date(dateStr);
    return d.toLocaleString('ru-RU', {
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit'
    });
  } catch (e) {
    return dateStr;
  }
}

function escapeHtml(text) {
  if (!text) return '';
  return text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/\"/g, '&quot;');
}

function attachEventListeners() {
  document.querySelectorAll('.dialog-item').forEach(el => {
    el.addEventListener('click', () => {
      const id = parseInt(el.dataset.id, 10);
      loadMessages(id);
    });
  });
}

// Start the app
init();
"
}

fn lustre_styles() -> String {
  "
:root {
  --bg-primary: #0f0f0f;
  --bg-secondary: #1a1a1a;
  --bg-tertiary: #252525;
  --text-primary: #ffffff;
  --text-secondary: #a0a0a0;
  --accent: #3b82f6;
  --accent-hover: #2563eb;
  --success: #22c55e;
  --error: #ef4444;
  --border: #333333;
}

* { margin: 0; padding: 0; box-sizing: border-box; }

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  background: var(--bg-primary);
  color: var(--text-primary);
  min-height: 100vh;
}

.app-container {
  display: flex;
  flex-direction: column;
  min-height: 100vh;
}

.app-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1rem 2rem;
  background: var(--bg-secondary);
  border-bottom: 1px solid var(--border);
}

.app-title { font-size: 1.5rem; font-weight: 600; }
.subtitle { color: var(--text-secondary); font-size: 0.9rem; margin-left: 1rem; }
.data-source { color: var(--accent); font-size: 0.75rem; margin-left: 1rem; padding: 0.25rem 0.5rem; background: rgba(59, 130, 246, 0.1); border-radius: 0.25rem; }

.header-right { display: flex; align-items: center; gap: 1rem; }

.btn-refresh {
  padding: 0.5rem 1rem;
  background: var(--accent);
  color: white;
  border: none;
  border-radius: 0.5rem;
  cursor: pointer;
  font-size: 0.875rem;
}
.btn-refresh:hover { background: var(--accent-hover); }

.connection-status {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.5rem 1rem;
  border-radius: 9999px;
  font-size: 0.875rem;
}
.status-connected { background: rgba(34, 197, 94, 0.1); color: var(--success); }
.status-disconnected { background: rgba(239, 68, 68, 0.1); color: var(--error); }
.status-dot { width: 8px; height: 8px; border-radius: 50%; background: currentColor; }

.app-content { display: flex; flex: 1; }

.sidebar {
  width: 320px;
  background: var(--bg-secondary);
  border-right: 1px solid var(--border);
  padding: 1.5rem;
  overflow-y: auto;
  max-height: calc(100vh - 70px);
}

.sidebar-title {
  font-size: 0.875rem;
  text-transform: uppercase;
  color: var(--text-secondary);
  margin-bottom: 1rem;
  letter-spacing: 0.05em;
}

.btn-all {
  width: 100%;
  padding: 0.75rem 1rem;
  background: var(--bg-tertiary);
  color: var(--text-primary);
  border: 1px solid var(--border);
  border-radius: 0.5rem;
  cursor: pointer;
  text-align: left;
  margin-bottom: 1rem;
  font-size: 0.9rem;
}
.btn-all:hover { background: var(--accent); }

.dialog-list { list-style: none; }

.dialog-item {
  display: flex;
  flex-direction: column;
  padding: 0.75rem 1rem;
  border-radius: 0.5rem;
  cursor: pointer;
  margin-bottom: 0.25rem;
  transition: background 0.2s;
}
.dialog-item:hover { background: var(--bg-tertiary); }
.dialog-item.active { background: var(--accent); }

.dialog-info { display: flex; justify-content: space-between; align-items: center; }
.dialog-title { font-size: 0.9rem; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 200px; }
.dialog-type { font-size: 0.75rem; color: var(--text-secondary); margin-top: 0.25rem; }

.unread-badge {
  background: var(--accent);
  padding: 0.125rem 0.5rem;
  border-radius: 9999px;
  font-size: 0.75rem;
  min-width: 24px;
  text-align: center;
}
.dialog-item.active .unread-badge { background: rgba(255,255,255,0.2); }

.main-content {
  flex: 1;
  padding: 1.5rem 2rem;
  overflow-y: auto;
  max-height: calc(100vh - 70px);
}

.message-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1.5rem;
}
.section-title { font-size: 1.25rem; font-weight: 600; }
.message-count { color: var(--text-secondary); font-size: 0.875rem; }

.loading, .no-messages {
  text-align: center;
  padding: 3rem;
  color: var(--text-secondary);
}

.message-list {
  list-style: none;
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.message-item {
  background: var(--bg-secondary);
  border: 1px solid var(--border);
  border-radius: 0.75rem;
  padding: 1rem 1.25rem;
}

.message-meta {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  margin-bottom: 0.5rem;
  flex-wrap: wrap;
}

.group-badge {
  background: var(--accent);
  color: white;
  padding: 0.125rem 0.5rem;
  border-radius: 0.25rem;
  font-size: 0.75rem;
}

.sender { font-weight: 500; color: var(--text-primary); }
.timestamp { color: var(--text-secondary); font-size: 0.75rem; margin-left: auto; }

.message-text {
  color: var(--text-secondary);
  line-height: 1.5;
  white-space: pre-wrap;
  word-break: break-word;
}

@media (max-width: 768px) {
  .app-content { flex-direction: column; }
  .sidebar { width: 100%; max-height: 300px; border-right: none; border-bottom: 1px solid var(--border); }
  .app-header { padding: 1rem; }
  .main-content { padding: 1rem; }
  .subtitle { display: none; }
}
"
}

/// Generate dashboard with custom state
pub fn render_dashboard(state: DashboardState) -> String {
  html.Html([], [
    html.Head([
      html.meta([attr.charset("utf-8")]),
      html.meta([
        attr.name("viewport"),
        attr.content("width=device-width, initial-scale=1"),
      ]),
      html.Element("title", [], [html.Text("VIBEE - Message Dashboard")]),
      html.Element("style", [], [html.Text(styles())]),
    ]),
    html.Body([attr.class("dark-theme")], [
      html.div([attr.class("app-container")], [
        // Header
        render_header(state.is_connected),
        // Content
        html.div([attr.class("app-content")], [
          // Sidebar
          render_sidebar(state.groups),
          // Main content
          render_main_content(state.messages, state.selected_group),
        ]),
      ]),
      // Auto-refresh script
      html.Element("script", [], [html.Text(refresh_script())]),
    ]),
  ])
  |> nakai.to_string()
}

fn render_header(is_connected: Bool) -> Node {
  let status_class = case is_connected {
    True -> "connection-status status-connected"
    False -> "connection-status status-disconnected"
  }
  let status_text = case is_connected {
    True -> "Connected"
    False -> "Disconnected"
  }

  html.Element("header", [attr.class("app-header")], [
    html.div([attr.class("header-left")], [
      html.h1([attr.class("app-title")], [html.Text("VIBEE Messages")]),
    ]),
    html.div([attr.class("header-right")], [
      html.Element("button", [
        attr.class("refresh-btn"),
        attr.Attr("onclick", "refreshPage()"),
      ], [html.Text("Refresh")]),
      html.span([attr.class(status_class)], [
        html.span([attr.class("status-dot")], []),
        html.Text(status_text),
      ]),
    ]),
  ])
}

fn render_sidebar(groups: List(GroupData)) -> Node {
  html.Element("nav", [attr.class("sidebar")], [
    html.h2([attr.class("sidebar-title")], [html.Text("Groups")]),
    html.ul([attr.class("group-list")],
      list.map(groups, render_group_item)
    ),
  ])
}

fn render_group_item(group: GroupData) -> Node {
  let class = case group.is_active {
    True -> "group-item active"
    False -> "group-item"
  }

  html.li([attr.class(class)], [
    html.Element("a", [
      attr.href("/?group=" <> group.id),
      attr.class("group-link"),
    ], [
      html.span([attr.class("group-name")], [html.Text(group.name)]),
      html.span([attr.class("message-count")], [html.Text(int.to_string(group.message_count))]),
    ]),
  ])
}

fn render_main_content(messages: List(MessageData), selected_group: Option(String)) -> Node {
  let title = case selected_group {
    Some(group) -> group
    None -> "All Messages"
  }

  html.Element("main", [attr.class("main-content")], [
    html.Element("section", [attr.class("message-header")], [
      html.h2([attr.class("section-title")], [html.Text(title)]),
      html.span([attr.class("message-total")], [
        html.Text(int.to_string(list.length(messages)) <> " messages"),
      ]),
    ]),
    case messages {
      [] -> html.div([attr.class("no-messages")], [
        html.Text("No messages yet"),
      ])
      _ -> html.ul([attr.class("message-list")],
        list.map(messages, render_message_item)
      )
    },
  ])
}

fn render_message_item(msg: MessageData) -> Node {
  html.li([attr.class("message-item")], [
    html.div([attr.class("message-meta")], [
      html.span([attr.class("group-badge")], [html.Text(msg.group_name)]),
      html.span([attr.class("sender")], [html.Text(msg.sender_name)]),
      html.span([attr.class("timestamp")], [html.Text(format_timestamp(msg.timestamp))]),
    ]),
    html.div([attr.class("message-text")], [html.Text(msg.text)]),
  ])
}

/// Default state with sample data
pub fn default_state() -> DashboardState {
  DashboardState(
    groups: [
      GroupData(id: "all", name: "All Messages", message_count: 5, is_active: True),
      GroupData(id: "dev", name: "Development Team", message_count: 2, is_active: False),
      GroupData(id: "ai", name: "AI Research", message_count: 2, is_active: False),
      GroupData(id: "crypto", name: "Crypto Trading", message_count: 1, is_active: False),
    ],
    messages: [
      MessageData(id: "msg-1", group_name: "Development Team", sender_name: "Alice", text: "Just deployed the new VIBEE update!", timestamp: 1733650000),
      MessageData(id: "msg-2", group_name: "AI Research", sender_name: "Bob", text: "The new Claude model is impressive", timestamp: 1733649500),
      MessageData(id: "msg-3", group_name: "Development Team", sender_name: "Charlie", text: "Great work on the Gleam integration", timestamp: 1733649000),
      MessageData(id: "msg-4", group_name: "Crypto Trading", sender_name: "Dave", text: "Market looking interesting today", timestamp: 1733648500),
      MessageData(id: "msg-5", group_name: "AI Research", sender_name: "Eve", text: "Anyone tried the new RAG implementation?", timestamp: 1733648000),
    ],
    selected_group: None,
    is_connected: True,
  )
}

/// Format Unix timestamp to human-readable string
fn format_timestamp(ts: Int) -> String {
  // Simple formatting - just show relative time
  let _now = 1733650100  // Approximate current time
  let diff = 1733650100 - ts
  case diff {
    d if d < 60 -> "Just now"
    d if d < 3600 -> int.to_string(d / 60) <> " min ago"
    d if d < 86400 -> int.to_string(d / 3600) <> " hours ago"
    _ -> int.to_string(diff / 86400) <> " days ago"
  }
}

/// JavaScript for WebSocket and interactions
fn refresh_script() -> String {
  "
    let ws = null;
    let reconnectAttempts = 0;
    const maxReconnectAttempts = 5;

    function connectWebSocket() {
      const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
      const wsUrl = protocol + '//' + window.location.host + '/ws';

      ws = new WebSocket(wsUrl);

      ws.onopen = function() {
        console.log('[WS] Connected');
        reconnectAttempts = 0;
        updateConnectionStatus(true);
        // Request initial messages
        ws.send('get_messages');
      };

      ws.onmessage = function(event) {
        try {
          const data = JSON.parse(event.data);
          if (data.type === 'messages') {
            console.log('[WS] Received messages:', data.data.length);
            // Could update DOM here if we had client-side rendering
          } else if (data.type === 'new_message') {
            console.log('[WS] New message:', data.data);
            // Reload page to show new message (simple approach)
            window.location.reload();
          }
        } catch (e) {
          console.log('[WS] Message:', event.data);
        }
      };

      ws.onclose = function() {
        console.log('[WS] Disconnected');
        updateConnectionStatus(false);
        // Try to reconnect
        if (reconnectAttempts < maxReconnectAttempts) {
          reconnectAttempts++;
          setTimeout(connectWebSocket, 2000 * reconnectAttempts);
        }
      };

      ws.onerror = function(error) {
        console.error('[WS] Error:', error);
      };
    }

    function updateConnectionStatus(connected) {
      const statusEl = document.querySelector('.connection-status');
      if (statusEl) {
        if (connected) {
          statusEl.className = 'connection-status status-connected';
          statusEl.innerHTML = '<span class=\"status-dot\"></span>Connected';
        } else {
          statusEl.className = 'connection-status status-disconnected';
          statusEl.innerHTML = '<span class=\"status-dot\"></span>Disconnected';
        }
      }
    }

    function refreshPage() {
      if (ws && ws.readyState === WebSocket.OPEN) {
        ws.send('get_messages');
      }
      window.location.reload();
    }

    // Initialize WebSocket connection
    connectWebSocket();

    // Ping to keep connection alive
    setInterval(function() {
      if (ws && ws.readyState === WebSocket.OPEN) {
        ws.send('ping');
      }
    }, 30000);

    // Add click handlers to group items
    document.querySelectorAll('.group-link').forEach(link => {
      link.addEventListener('click', function(e) {
        document.querySelectorAll('.group-item').forEach(i => i.classList.remove('active'));
        this.parentElement.classList.add('active');
      });
    });
  "
}

/// CSS styles
fn styles() -> String {
  "
    :root {
      --bg-primary: #0f0f0f;
      --bg-secondary: #1a1a1a;
      --bg-tertiary: #252525;
      --text-primary: #ffffff;
      --text-secondary: #a0a0a0;
      --accent: #3b82f6;
      --accent-hover: #2563eb;
      --success: #22c55e;
      --error: #ef4444;
      --border: #333333;
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
    }

    .loading-screen {
      display: flex;
      align-items: center;
      justify-content: center;
      min-height: 100vh;
      font-size: 1.5rem;
      color: var(--text-secondary);
    }

    .app-container {
      display: flex;
      flex-direction: column;
      min-height: 100vh;
    }

    .app-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 1rem 2rem;
      background: var(--bg-secondary);
      border-bottom: 1px solid var(--border);
    }

    .app-title {
      font-size: 1.5rem;
      font-weight: 600;
    }

    .header-right {
      display: flex;
      align-items: center;
      gap: 1rem;
    }

    .connection-status {
      display: flex;
      align-items: center;
      gap: 0.5rem;
      padding: 0.5rem 1rem;
      border-radius: 9999px;
      font-size: 0.875rem;
    }

    .status-connected {
      background: rgba(34, 197, 94, 0.1);
      color: var(--success);
    }

    .status-disconnected {
      background: rgba(239, 68, 68, 0.1);
      color: var(--error);
    }

    .status-dot {
      width: 8px;
      height: 8px;
      border-radius: 50%;
      background: currentColor;
    }

    .refresh-btn {
      padding: 0.5rem 1rem;
      background: var(--accent);
      color: white;
      border: none;
      border-radius: 0.5rem;
      cursor: pointer;
      font-size: 0.875rem;
      transition: background 0.2s;
    }

    .refresh-btn:hover {
      background: var(--accent-hover);
    }

    .app-content {
      display: flex;
      flex: 1;
    }

    .sidebar {
      width: 280px;
      background: var(--bg-secondary);
      border-right: 1px solid var(--border);
      padding: 1.5rem;
    }

    .sidebar-title {
      font-size: 0.875rem;
      text-transform: uppercase;
      color: var(--text-secondary);
      margin-bottom: 1rem;
      letter-spacing: 0.05em;
    }

    .group-list {
      list-style: none;
    }

    .group-item {
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 0.75rem 1rem;
      border-radius: 0.5rem;
      cursor: pointer;
      transition: background 0.2s;
      margin-bottom: 0.25rem;
    }

    .group-item:hover {
      background: var(--bg-tertiary);
    }

    .group-item.active {
      background: var(--accent);
    }

    .group-name {
      font-size: 0.9rem;
    }

    .message-count {
      background: var(--bg-tertiary);
      padding: 0.125rem 0.5rem;
      border-radius: 9999px;
      font-size: 0.75rem;
      color: var(--text-secondary);
    }

    .group-item.active .message-count {
      background: rgba(255, 255, 255, 0.2);
      color: white;
    }

    .group-link {
      display: flex;
      justify-content: space-between;
      align-items: center;
      width: 100%;
      text-decoration: none;
      color: inherit;
    }

    .message-total {
      color: var(--text-secondary);
      font-size: 0.875rem;
    }

    .main-content {
      flex: 1;
      padding: 1.5rem 2rem;
      overflow-y: auto;
    }

    .message-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 1.5rem;
    }

    .section-title {
      font-size: 1.25rem;
      font-weight: 600;
    }

    .loading {
      color: var(--text-secondary);
      font-size: 0.875rem;
    }

    .message-list {
      list-style: none;
      display: flex;
      flex-direction: column;
      gap: 1rem;
    }

    .message-item {
      background: var(--bg-secondary);
      border: 1px solid var(--border);
      border-radius: 0.75rem;
      padding: 1rem 1.25rem;
    }

    .message-meta {
      display: flex;
      align-items: center;
      gap: 0.75rem;
      margin-bottom: 0.5rem;
    }

    .group-badge {
      background: var(--accent);
      color: white;
      padding: 0.125rem 0.5rem;
      border-radius: 0.25rem;
      font-size: 0.75rem;
    }

    .sender {
      font-weight: 500;
      color: var(--text-primary);
    }

    .timestamp {
      color: var(--text-secondary);
      font-size: 0.75rem;
      margin-left: auto;
    }

    .message-text {
      color: var(--text-secondary);
      line-height: 1.5;
    }

    .no-messages {
      text-align: center;
      padding: 3rem;
      color: var(--text-secondary);
    }

    @media (max-width: 768px) {
      .app-content {
        flex-direction: column;
      }

      .sidebar {
        width: 100%;
        border-right: none;
        border-bottom: 1px solid var(--border);
      }

      .app-header {
        padding: 1rem;
      }

      .main-content {
        padding: 1rem;
      }
    }
  "
}
