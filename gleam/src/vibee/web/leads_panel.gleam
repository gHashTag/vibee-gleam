// Leads Management Panel (Lustre UI)
// Admin interface for managing leads

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/web/layout

/// Lead status
pub type LeadStatus {
  New
  Contacted
  Qualified
  Converted
  Lost
}

/// Lead priority
pub type LeadPriority {
  Low
  Medium
  High
  Urgent
}

/// Lead data
pub type Lead {
  Lead(
    id: Int,
    telegram_user_id: Int,
    username: Option(String),
    first_name: Option(String),
    last_name: Option(String),
    phone: Option(String),
    first_message: String,
    first_message_date: String,
    source_chat_name: String,
    status: LeadStatus,
    priority: LeadPriority,
    intent: Option(String),
    crypto_interest: List(String),
    trigger_words: List(String),
    agent_response: String,
    last_activity: String,
    message_count: Int,
  )
}

/// Lead message
pub type LeadMessage {
  LeadMessage(
    id: Int,
    message_text: String,
    message_date: String,
    direction: String, // "incoming" or "outgoing"
    sentiment: Option(String),
  )
}

/// Lead note
pub type LeadNote {
  LeadNote(
    id: Int,
    note: String,
    note_type: String,
    created_by: String,
    created_at: String,
  )
}

/// Render leads panel HTML using unified layout
pub fn render_leads_panel(leads: List(Lead)) -> String {
  let config = layout.PageConfig(
    title: "Leads Management",
    active_page: "leads",
    show_sidebar: False,
    show_language_toggle: True,
  )
  
  let main_content = "
    <div class=\"leads-header\">
      <h1>🎯 Leads Management</h1>
      <div class=\"leads-actions\">
        <button class=\"btn btn-primary\">📊 Export CSV</button>
        <button class=\"btn btn-secondary\">🔄 Refresh</button>
      </div>
    </div>
    
    <div class=\"stats-grid\">
      " <> render_stats_html(leads) <> "
    </div>
    
    " <> render_filters_html() <> "
    " <> render_leads_table_html(leads) <> "
    
    <style>" <> leads_specific_css() <> "</style>
    <script>" <> leads_js() <> "</script>
  "
  
  layout.render_page(config, "", main_content)
}

/// Render stats HTML
fn render_stats_html(leads: List(Lead)) -> String {
  let total = list.length(leads)
  let new_count = list.count(leads, fn(l) { l.status == New })
  let contacted = list.count(leads, fn(l) { l.status == Contacted })
  let converted = list.count(leads, fn(l) { l.status == Converted })
  
  layout.render_stat_card("🎯", int.to_string(total), "Total Leads", "blue")
  <> layout.render_stat_card("🆕", int.to_string(new_count), "New", "green")
  <> layout.render_stat_card("💬", int.to_string(contacted), "Contacted", "yellow")
  <> layout.render_stat_card("✅", int.to_string(converted), "Converted", "blue")
}

/// Render filters HTML
fn render_filters_html() -> String {
  "<div class=\"filters\">
    <select class=\"filter-select\">
      <option value=\"all\">All Status</option>
      <option value=\"new\">New</option>
      <option value=\"contacted\">Contacted</option>
      <option value=\"qualified\">Qualified</option>
      <option value=\"converted\">Converted</option>
      <option value=\"lost\">Lost</option>
    </select>
    <select class=\"filter-select\">
      <option value=\"all\">All Priority</option>
      <option value=\"urgent\">🔴 Urgent</option>
      <option value=\"high\">🟠 High</option>
      <option value=\"medium\">🟡 Medium</option>
      <option value=\"low\">🟢 Low</option>
    </select>
    <input type=\"search\" class=\"search-input\" placeholder=\"Search leads...\">
  </div>"
}

/// Render leads table HTML
fn render_leads_table_html(leads: List(Lead)) -> String {
  let rows = list.map(leads, render_lead_row_html)
  let rows_html = string.join(rows, "\n")
  
  layout.render_table(
    ["Priority", "Contact", "First Message", "Intent", "Crypto", "Source", "Status", "Last Activity", "Actions"],
    []
  ) |> string.replace("</tbody>", rows_html <> "</tbody>")
}

fn render_lead_row_html(lead: Lead) -> String {
  let name = case lead.first_name, lead.last_name {
    Some(first), Some(last) -> first <> " " <> last
    Some(first), None -> first
    None, Some(last) -> last
    None, None -> "Unknown"
  }
  
  let username_html = case lead.username {
    Some(u) -> "<div class=\"contact-username\">@" <> u <> "</div>"
    None -> ""
  }
  
  let priority_badge = case lead.priority {
    Urgent -> "<span class=\"badge priority-urgent\">🔴 Urgent</span>"
    High -> "<span class=\"badge priority-high\">🟠 High</span>"
    Medium -> "<span class=\"badge priority-medium\">🟡 Medium</span>"
    Low -> "<span class=\"badge priority-low\">🟢 Low</span>"
  }
  
  let status_badge = case lead.status {
    New -> "<span class=\"badge status-new\">🆕 New</span>"
    Contacted -> "<span class=\"badge status-contacted\">💬 Contacted</span>"
    Qualified -> "<span class=\"badge status-qualified\">✨ Qualified</span>"
    Converted -> "<span class=\"badge status-converted\">✅ Converted</span>"
    Lost -> "<span class=\"badge status-lost\">❌ Lost</span>"
  }
  
  let intent_badge = case lead.intent {
    Some("buy") -> "<span class=\"badge intent-buy\">💰 Buy</span>"
    Some("sell") -> "<span class=\"badge intent-sell\">💸 Sell</span>"
    Some("exchange") -> "<span class=\"badge intent-exchange\">🔄 Exchange</span>"
    Some("info") -> "<span class=\"badge intent-info\">ℹ️ Info</span>"
    _ -> "-"
  }
  
  let crypto_tags = list.map(lead.crypto_interest, fn(crypto) {
    "<span class=\"crypto-tag\">" <> crypto <> "</span>"
  })
  let crypto_html = string.join(crypto_tags, " ")
  
  "<tr class=\"lead-row\">
    <td>" <> priority_badge <> "</td>
    <td>
      <div class=\"contact-info\">
        <div class=\"contact-name\">" <> name <> "</div>
        " <> username_html <> "
        <div class=\"contact-id\">ID: " <> int.to_string(lead.telegram_user_id) <> "</div>
      </div>
    </td>
    <td>
      <div class=\"message-preview\">" <> string.slice(lead.first_message, 0, 100) <> "</div>
      <div class=\"message-date\">" <> lead.first_message_date <> "</div>
    </td>
    <td>" <> intent_badge <> "</td>
    <td><div class=\"crypto-tags\">" <> crypto_html <> "</div></td>
    <td><div class=\"source-chat\">" <> lead.source_chat_name <> "</div></td>
    <td>" <> status_badge <> "</td>
    <td>
      <div class=\"last-activity\">" <> lead.last_activity <> "</div>
      <div class=\"message-count\">" <> int.to_string(lead.message_count) <> " messages</div>
    </td>
    <td>
      <div class=\"actions\">
        <button class=\"btn-icon\" title=\"View Details\" onclick=\"viewLead(" <> int.to_string(lead.id) <> ")\">👁️</button>
        <button class=\"btn-icon\" title=\"Send Message\" onclick=\"messageLead(" <> int.to_string(lead.id) <> ")\">💬</button>
        <button class=\"btn-icon\" title=\"Change Status\" onclick=\"changeStatus(" <> int.to_string(lead.id) <> ")\">🔄</button>
      </div>
    </td>
  </tr>"
}

/// Leads-specific CSS
fn leads_specific_css() -> String {
  ".leads-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 2rem;
}

.leads-header h1 {
  font-size: 2rem;
  color: var(--accent);
}

.leads-actions {
  display: flex;
  gap: 0.5rem;
}

.stats-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1rem;
  margin-bottom: 2rem;
}

.filters {
  display: flex;
  gap: 1rem;
  margin-bottom: 1.5rem;
  flex-wrap: wrap;
}

.filter-select, .search-input {
  padding: 0.5rem 1rem;
  background: var(--bg-card);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  color: var(--text-primary);
  font-size: 0.9rem;
}

.search-input {
  flex: 1;
  max-width: 400px;
}

.leads-table {
  width: 100%;
  border-collapse: collapse;
}

.lead-row:hover {
  background: rgba(255, 255, 255, 0.02);
}

.contact-info {
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
}

.contact-name {
  font-weight: 600;
}

.contact-username {
  color: var(--info);
  font-size: 0.85rem;
}

.contact-id {
  color: var(--text-secondary);
  font-size: 0.8rem;
}

.message-preview {
  max-width: 300px;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.message-date {
  color: var(--text-secondary);
  font-size: 0.8rem;
  margin-top: 0.25rem;
}

.badge {
  display: inline-block;
  padding: 0.25rem 0.75rem;
  border-radius: 12px;
  font-size: 0.85rem;
  font-weight: 500;
}

.priority-urgent { background: rgba(255, 68, 68, 0.2); color: var(--danger); }
.priority-high { background: rgba(255, 170, 0, 0.2); color: var(--warning); }
.priority-medium { background: rgba(255, 255, 0, 0.2); color: #ffff00; }
.priority-low { background: rgba(0, 255, 0, 0.2); color: var(--success); }

.status-new { background: rgba(0, 255, 0, 0.2); color: var(--success); }
.status-contacted { background: rgba(255, 170, 0, 0.2); color: var(--warning); }
.status-qualified { background: rgba(170, 0, 255, 0.2); color: #aa00ff; }
.status-converted { background: rgba(0, 255, 170, 0.2); color: var(--accent); }
.status-lost { background: rgba(255, 68, 68, 0.2); color: var(--danger); }

.intent-buy { background: rgba(0, 255, 0, 0.2); color: var(--success); }
.intent-sell { background: rgba(255, 68, 68, 0.2); color: var(--danger); }
.intent-exchange { background: rgba(0, 136, 204, 0.2); color: var(--info); }
.intent-info { background: rgba(136, 136, 136, 0.2); color: var(--text-secondary); }

.crypto-tags {
  display: flex;
  flex-wrap: wrap;
  gap: 0.25rem;
}

.crypto-tag {
  padding: 0.2rem 0.5rem;
  background: var(--bg-secondary);
  border: 1px solid var(--border);
  border-radius: 4px;
  font-size: 0.75rem;
  color: var(--accent);
}

.actions {
  display: flex;
  gap: 0.5rem;
}

.btn-icon {
  background: var(--bg-secondary);
  border: 1px solid var(--border);
  border-radius: 4px;
  padding: 0.4rem 0.6rem;
  cursor: pointer;
  font-size: 1.2rem;
  transition: all 0.2s;
}

.btn-icon:hover {
  background: var(--bg-primary);
  transform: scale(1.1);
}
"
}

/// JavaScript for leads
fn leads_js() -> String {
  "
function viewLead(leadId) {
  window.location.href = '/leads/' + leadId;
}

function messageLead(leadId) {
  alert('Send message to lead #' + leadId);
}

function changeStatus(leadId) {
  const status = prompt('Enter new status (new/contacted/qualified/converted/lost):');
  if (status) {
    fetch('/api/v1/leads/' + leadId + '/status', {
      method: 'PUT',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ status: status })
    })
    .then(response => response.json())
    .then(data => {
      alert('Status updated!');
      location.reload();
    })
    .catch(error => {
      alert('Error: ' + error);
    });
  }
}

// Auto-refresh every 30 seconds
setInterval(() => {
  if (window.location.pathname === '/leads') {
    location.reload();
  }
}, 30000);
"
}

