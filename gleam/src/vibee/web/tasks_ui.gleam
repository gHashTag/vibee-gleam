// TaskFlow UI - Web Interface for Task Management
// SSR HTML generation with HTMX support
// Dark theme, real-time updates, bilingual (EN/RU)

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/db/tasks.{
  type Task, type TaskCategory, type TaskStats, type TaskStatus,
  type TaskComment, type StatusHistoryEntry,
  TaskPending, TaskInProgress, TaskWaiting, TaskCompleted, TaskCancelled,
  CategoryConversation, CategoryMeeting, CategoryProject, CategoryPromise, CategoryOther,
}

// =============================================================================
// MAIN PAGE RENDERERS
// =============================================================================

/// Render Dashboard page with stats, overdue and today's tasks
pub fn render_dashboard(
  stats: TaskStats,
  overdue: List(Task),
  today: List(Task),
) -> String {
  render_page("dashboard", render_dashboard_content(stats, overdue, today))
}

/// Render Tasks List page with filters
pub fn render_tasks_list(
  tasks: List(Task),
  filter_status: Option(String),
  filter_priority: Option(Int),
  view_mode: String,
) -> String {
  render_page("list", render_tasks_list_content(tasks, filter_status, filter_priority, view_mode))
}

/// Render Task Create form
pub fn render_task_create_form(contacts: List(#(Int, String))) -> String {
  render_page("new", render_create_form_content(contacts))
}

/// Render Task Detail page
pub fn render_task_detail(
  task: Task,
  comments: List(TaskComment),
  history: List(StatusHistoryEntry),
) -> String {
  render_page("detail", render_detail_content(task, comments, history))
}

// =============================================================================
// PAGE TEMPLATE
// =============================================================================

fn render_page(active_page: String, content: String) -> String {
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>VIBEE TaskFlow</title>
  <script src=\"https://unpkg.com/htmx.org@1.9.10\"></script>
  <style>" <> styles() <> "</style>
</head>
<body>
  <div class=\"app-container\">
    " <> render_header() <> "
    <div class=\"app-content\">
      " <> render_sidebar(active_page) <> "
      <main class=\"main\">
        " <> content <> "
      </main>
    </div>
  </div>
  <script>" <> i18n_script() <> "</script>
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
        <span class=\"logo-icon\">✅</span>
        <span class=\"logo-text\">TaskFlow</span>
      </div>
      <nav class=\"nav-links\">
        <a href=\"/\" class=\"nav-link\" data-i18n=\"nav.home\">Home</a>
        <a href=\"/tasks\" class=\"nav-link active\" data-i18n=\"nav.tasks\">Tasks</a>
        <a href=\"/p2p\" class=\"nav-link\" data-i18n=\"nav.p2p\">P2P</a>
      </nav>
    </div>
    <div class=\"header-right\">
      <div class=\"lang-switcher\">
        <button class=\"lang-btn active\" data-lang=\"en\" onclick=\"setLang('en')\">EN</button>
        <button class=\"lang-btn\" data-lang=\"ru\" onclick=\"setLang('ru')\">RU</button>
      </div>
      <a href=\"/tasks/new\" class=\"btn btn-success\">
        + <span data-i18n=\"btn.newTask\">New Task</span>
      </a>
    </div>
  </header>"
}

// =============================================================================
// SIDEBAR
// =============================================================================

fn render_sidebar(active_page: String) -> String {
  let nav_item = fn(href: String, icon: String, label: String, key: String, page: String) {
    let class = case page == active_page {
      True -> "sidebar-link active"
      False -> "sidebar-link"
    }
    "<a href=\"" <> href <> "\" class=\"" <> class <> "\">
      <span class=\"sidebar-icon\">" <> icon <> "</span>
      <span data-i18n=\"" <> key <> "\">" <> label <> "</span>
    </a>"
  }

  "<aside class=\"sidebar\">
    <nav class=\"sidebar-nav\">
      " <> nav_item("/tasks", "📊", "Dashboard", "sidebar.dashboard", "dashboard") <> "
      " <> nav_item("/tasks/list", "📋", "All Tasks", "sidebar.allTasks", "list") <> "
      " <> nav_item("/tasks/list?filter=today", "📅", "Today", "sidebar.today", "today") <> "
      " <> nav_item("/tasks/list?filter=overdue", "⚠️", "Overdue", "sidebar.overdue", "overdue") <> "
      " <> nav_item("/tasks/contacts", "👥", "By Contact", "sidebar.byContact", "contacts") <> "
      " <> nav_item("/tasks/reports", "📈", "Reports", "sidebar.reports", "reports") <> "
    </nav>

    <div class=\"sidebar-section\">
      <div class=\"sidebar-title\" data-i18n=\"sidebar.quickFilters\">Quick Filters</div>
      <div class=\"filter-badges\">
        <a href=\"/tasks/list?status=pending\" class=\"filter-badge pending\">
          <span class=\"badge-dot\"></span>
          <span data-i18n=\"status.pending\">Pending</span>
        </a>
        <a href=\"/tasks/list?status=in_progress\" class=\"filter-badge in-progress\">
          <span class=\"badge-dot\"></span>
          <span data-i18n=\"status.inProgress\">In Progress</span>
        </a>
        <a href=\"/tasks/list?status=waiting\" class=\"filter-badge waiting\">
          <span class=\"badge-dot\"></span>
          <span data-i18n=\"status.waiting\">Waiting</span>
        </a>
        <a href=\"/tasks/list?status=completed\" class=\"filter-badge completed\">
          <span class=\"badge-dot\"></span>
          <span data-i18n=\"status.completed\">Completed</span>
        </a>
      </div>
    </div>

    <div class=\"sidebar-section\">
      <div class=\"sidebar-title\" data-i18n=\"sidebar.priority\">Priority</div>
      <div class=\"filter-badges\">
        <a href=\"/tasks/list?priority=1\" class=\"filter-badge priority-high\">🔴 <span data-i18n=\"priority.high\">High</span></a>
        <a href=\"/tasks/list?priority=2\" class=\"filter-badge priority-medium\">🟡 <span data-i18n=\"priority.medium\">Medium</span></a>
        <a href=\"/tasks/list?priority=3\" class=\"filter-badge priority-low\">🟢 <span data-i18n=\"priority.low\">Low</span></a>
      </div>
    </div>
  </aside>"
}

// =============================================================================
// DASHBOARD CONTENT
// =============================================================================

fn render_dashboard_content(
  stats: TaskStats,
  overdue: List(Task),
  today: List(Task),
) -> String {
  "<div class=\"dashboard\">
    <!-- Stats Cards -->
    <div class=\"stats-grid\">
      " <> render_stat_card("📋", "Total Tasks", int.to_string(stats.total), "stat.total", "blue") <> "
      " <> render_stat_card("⏳", "Pending", int.to_string(stats.pending), "stat.pending", "gray") <> "
      " <> render_stat_card("🔄", "In Progress", int.to_string(stats.in_progress), "stat.inProgress", "cyan") <> "
      " <> render_stat_card("✅", "Completed", int.to_string(stats.completed), "stat.completed", "green") <> "
      " <> render_stat_card("⚠️", "Overdue", int.to_string(stats.overdue), "stat.overdue", "red") <> "
      " <> render_completion_card(stats) <> "
    </div>

    <!-- Overdue Tasks -->
    <div class=\"card\">
      <div class=\"card-header\">
        <span class=\"card-icon\">⚠️</span>
        <span class=\"card-title\" data-i18n=\"dashboard.overdueTasks\">Overdue Tasks</span>
        <span class=\"badge badge-red\">" <> int.to_string(list.length(overdue)) <> "</span>
      </div>
      <div class=\"card-body\">
        " <> render_task_list_compact(overdue, "overdue") <> "
      </div>
    </div>

    <!-- Today's Tasks -->
    <div class=\"card\">
      <div class=\"card-header\">
        <span class=\"card-icon\">📅</span>
        <span class=\"card-title\" data-i18n=\"dashboard.todayTasks\">Today's Tasks</span>
        <span class=\"badge badge-blue\">" <> int.to_string(list.length(today)) <> "</span>
      </div>
      <div class=\"card-body\">
        " <> render_task_list_compact(today, "today") <> "
      </div>
    </div>
  </div>"
}

fn render_stat_card(icon: String, label: String, value: String, i18n_key: String, color: String) -> String {
  "<div class=\"stat-card " <> color <> "\">
    <div class=\"stat-icon\">" <> icon <> "</div>
    <div class=\"stat-info\">
      <div class=\"stat-value\">" <> value <> "</div>
      <div class=\"stat-label\" data-i18n=\"" <> i18n_key <> "\">" <> label <> "</div>
    </div>
  </div>"
}

fn render_completion_card(stats: TaskStats) -> String {
  let total = stats.total
  let completed = stats.completed
  let percentage = case total {
    0 -> "0"
    _ -> int.to_string(completed * 100 / total)
  }

  "<div class=\"stat-card completion\">
    <div class=\"completion-ring\">
      <svg viewBox=\"0 0 36 36\">
        <path class=\"ring-bg\" d=\"M18 2.0845 a 15.9155 15.9155 0 0 1 0 31.831 a 15.9155 15.9155 0 0 1 0 -31.831\" />
        <path class=\"ring-progress\" stroke-dasharray=\"" <> percentage <> ", 100\" d=\"M18 2.0845 a 15.9155 15.9155 0 0 1 0 31.831 a 15.9155 15.9155 0 0 1 0 -31.831\" />
      </svg>
      <span class=\"completion-value\">" <> percentage <> "%</span>
    </div>
    <div class=\"stat-label\" data-i18n=\"stat.completionRate\">Completion Rate</div>
  </div>"
}

// =============================================================================
// TASK LIST CONTENT
// =============================================================================

fn render_tasks_list_content(
  tasks: List(Task),
  _filter_status: Option(String),
  _filter_priority: Option(Int),
  view_mode: String,
) -> String {
  let view_buttons = "<div class=\"view-switcher\">
    <button class=\"view-btn" <> case view_mode { "list" -> " active" _ -> "" } <> "\" onclick=\"setView('list')\" title=\"List View\">☰</button>
    <button class=\"view-btn" <> case view_mode { "kanban" -> " active" _ -> "" } <> "\" onclick=\"setView('kanban')\" title=\"Kanban\">▦</button>
    <button class=\"view-btn" <> case view_mode { "matrix" -> " active" _ -> "" } <> "\" onclick=\"setView('matrix')\" title=\"Eisenhower Matrix\">⊞</button>
  </div>"

  let content = case view_mode {
    "kanban" -> render_kanban_view(tasks)
    "matrix" -> render_eisenhower_view(tasks)
    _ -> render_list_view(tasks)
  }

  "<div class=\"tasks-page\">
    <div class=\"page-header\">
      <h1 class=\"page-title\" data-i18n=\"page.allTasks\">All Tasks</h1>
      " <> view_buttons <> "
    </div>

    <div id=\"tasks-container\" hx-get=\"/api/v1/tasks/list-partial\" hx-trigger=\"taskUpdated from:body\">
      " <> content <> "
    </div>
  </div>"
}

fn render_list_view(tasks: List(Task)) -> String {
  case list.length(tasks) {
    0 -> "<div class=\"empty-state\">
      <span class=\"empty-icon\">📭</span>
      <span class=\"empty-text\" data-i18n=\"empty.noTasks\">No tasks found</span>
      <a href=\"/tasks/new\" class=\"btn btn-success\" data-i18n=\"btn.createFirst\">Create your first task</a>
    </div>"
    _ -> "<div class=\"task-list\">" <> string.join(list.map(tasks, render_task_card), "") <> "</div>"
  }
}

fn render_kanban_view(tasks: List(Task)) -> String {
  let pending = list.filter(tasks, fn(t) { t.status == TaskPending })
  let in_progress = list.filter(tasks, fn(t) { t.status == TaskInProgress })
  let waiting = list.filter(tasks, fn(t) { t.status == TaskWaiting })
  let completed = list.filter(tasks, fn(t) { t.status == TaskCompleted })

  "<div class=\"kanban-board\">
    " <> render_kanban_column("pending", "Pending", "⏳", pending) <> "
    " <> render_kanban_column("in_progress", "In Progress", "🔄", in_progress) <> "
    " <> render_kanban_column("waiting", "Waiting", "⏸️", waiting) <> "
    " <> render_kanban_column("completed", "Completed", "✅", completed) <> "
  </div>"
}

fn render_kanban_column(status: String, title: String, icon: String, tasks: List(Task)) -> String {
  "<div class=\"kanban-column " <> status <> "\">
    <div class=\"kanban-header\">
      <span class=\"kanban-icon\">" <> icon <> "</span>
      <span class=\"kanban-title\">" <> title <> "</span>
      <span class=\"kanban-count\">" <> int.to_string(list.length(tasks)) <> "</span>
    </div>
    <div class=\"kanban-cards\">
      " <> string.join(list.map(tasks, render_task_card_mini), "") <> "
    </div>
  </div>"
}

fn render_eisenhower_view(tasks: List(Task)) -> String {
  // Quadrant 1: Urgent + Important (priority 1, has due date soon)
  // Quadrant 2: Important (priority 1-2, no urgent due)
  // Quadrant 3: Urgent (priority 2-3, due soon)
  // Quadrant 4: Neither (priority 3, no due)

  let q1 = list.filter(tasks, fn(t) { t.priority == 1 })
  let q2 = list.filter(tasks, fn(t) { t.priority == 2 })
  let q3 = list.filter(tasks, fn(t) { t.priority == 3 && option.is_some(t.due_date) })
  let q4 = list.filter(tasks, fn(t) { t.priority == 3 && option.is_none(t.due_date) })

  "<div class=\"eisenhower-matrix\">
    <div class=\"matrix-row\">
      " <> render_matrix_quadrant("q1", "Do First", "🔥", "Urgent & Important", q1) <> "
      " <> render_matrix_quadrant("q2", "Schedule", "📅", "Important", q2) <> "
    </div>
    <div class=\"matrix-row\">
      " <> render_matrix_quadrant("q3", "Delegate", "👥", "Urgent", q3) <> "
      " <> render_matrix_quadrant("q4", "Eliminate", "🗑️", "Neither", q4) <> "
    </div>
  </div>"
}

fn render_matrix_quadrant(id: String, title: String, icon: String, subtitle: String, tasks: List(Task)) -> String {
  "<div class=\"matrix-quadrant " <> id <> "\">
    <div class=\"quadrant-header\">
      <span class=\"quadrant-icon\">" <> icon <> "</span>
      <div class=\"quadrant-info\">
        <span class=\"quadrant-title\">" <> title <> "</span>
        <span class=\"quadrant-subtitle\">" <> subtitle <> "</span>
      </div>
      <span class=\"quadrant-count\">" <> int.to_string(list.length(tasks)) <> "</span>
    </div>
    <div class=\"quadrant-tasks\">
      " <> string.join(list.map(tasks, render_task_card_mini), "") <> "
    </div>
  </div>"
}

// =============================================================================
// CREATE FORM CONTENT
// =============================================================================

fn render_create_form_content(contacts: List(#(Int, String))) -> String {
  let contact_options = string.join(
    list.map(contacts, fn(c) {
      "<option value=\"" <> int.to_string(c.0) <> "\">" <> c.1 <> "</option>"
    }),
    ""
  )

  "<div class=\"create-page\">
    <h1 class=\"page-title\" data-i18n=\"page.createTask\">Create Task</h1>

    <form class=\"task-form\" hx-post=\"/api/v1/tasks\" hx-redirect=\"/tasks\">
      <div class=\"form-group\">
        <label for=\"title\" data-i18n=\"form.title\">Title *</label>
        <input type=\"text\" id=\"title\" name=\"title\" required class=\"form-input\" placeholder=\"What needs to be done?\">
      </div>

      <div class=\"form-group\">
        <label for=\"description\" data-i18n=\"form.description\">Description</label>
        <textarea id=\"description\" name=\"description\" class=\"form-textarea\" rows=\"3\" placeholder=\"Add details...\"></textarea>
      </div>

      <div class=\"form-row\">
        <div class=\"form-group\">
          <label for=\"contact_id\" data-i18n=\"form.contact\">Contact</label>
          <select id=\"contact_id\" name=\"contact_id\" class=\"form-select\">
            <option value=\"\">-- No contact --</option>
            " <> contact_options <> "
          </select>
        </div>

        <div class=\"form-group\">
          <label for=\"category\" data-i18n=\"form.category\">Category</label>
          <select id=\"category\" name=\"category\" class=\"form-select\">
            <option value=\"other\">📋 Other</option>
            <option value=\"conversation\">💬 Conversation</option>
            <option value=\"meeting\">📞 Meeting</option>
            <option value=\"project\">👥 Project</option>
            <option value=\"promise\">✅ Promise</option>
          </select>
        </div>
      </div>

      <div class=\"form-row\">
        <div class=\"form-group\">
          <label data-i18n=\"form.priority\">Priority</label>
          <div class=\"radio-group\">
            <label class=\"radio-label priority-high\">
              <input type=\"radio\" name=\"priority\" value=\"1\">
              <span>🔴 High</span>
            </label>
            <label class=\"radio-label priority-medium\">
              <input type=\"radio\" name=\"priority\" value=\"2\" checked>
              <span>🟡 Medium</span>
            </label>
            <label class=\"radio-label priority-low\">
              <input type=\"radio\" name=\"priority\" value=\"3\">
              <span>🟢 Low</span>
            </label>
          </div>
        </div>

        <div class=\"form-group\">
          <label for=\"due_date\" data-i18n=\"form.dueDate\">Due Date</label>
          <input type=\"datetime-local\" id=\"due_date\" name=\"due_date\" class=\"form-input\">
        </div>
      </div>

      <div class=\"form-group\">
        <label data-i18n=\"form.responsibility\">Responsibility</label>
        <div class=\"radio-group\">
          <label class=\"radio-label\">
            <input type=\"radio\" name=\"responsibility\" value=\"owner\" checked>
            <span>👤 Me</span>
          </label>
          <label class=\"radio-label\">
            <input type=\"radio\" name=\"responsibility\" value=\"contact\">
            <span>👥 Contact</span>
          </label>
          <label class=\"radio-label\">
            <input type=\"radio\" name=\"responsibility\" value=\"both\">
            <span>🤝 Both</span>
          </label>
        </div>
      </div>

      <div class=\"form-actions\">
        <a href=\"/tasks\" class=\"btn btn-secondary\" data-i18n=\"btn.cancel\">Cancel</a>
        <button type=\"submit\" class=\"btn btn-success\" data-i18n=\"btn.createTask\">Create Task</button>
      </div>
    </form>
  </div>"
}

// =============================================================================
// DETAIL CONTENT
// =============================================================================

fn render_detail_content(
  task: Task,
  comments: List(TaskComment),
  history: List(StatusHistoryEntry),
) -> String {
  "<div class=\"detail-page\">
    <div class=\"detail-header\">
      <div class=\"detail-meta\">
        " <> render_priority_badge(task.priority) <> "
        " <> render_status_badge(task.status) <> "
        " <> render_category_badge(task.category) <> "
      </div>
      <h1 class=\"detail-title\">" <> task.title <> "</h1>
      " <> case task.contact_name {
        Some(name) -> "<div class=\"detail-contact\">👤 " <> name <> "</div>"
        None -> ""
      } <> "
    </div>

    <div class=\"detail-body\">
      <!-- Info Section -->
      <div class=\"card\">
        <div class=\"card-header\">
          <span class=\"card-icon\">ℹ️</span>
          <span class=\"card-title\" data-i18n=\"detail.info\">Information</span>
          <a href=\"/tasks/" <> int.to_string(task.id) <> "/edit\" class=\"btn btn-sm\">✏️ Edit</a>
        </div>
        <div class=\"card-body\">
          " <> case task.description {
            Some(desc) -> "<div class=\"detail-description\">" <> desc <> "</div>"
            None -> "<div class=\"detail-description muted\" data-i18n=\"detail.noDescription\">No description</div>"
          } <> "
          <div class=\"detail-info-grid\">
            <div class=\"info-item\">
              <span class=\"info-label\" data-i18n=\"detail.dueDate\">Due Date</span>
              <span class=\"info-value\">" <> option.unwrap(task.due_date, "Not set") <> "</span>
            </div>
            <div class=\"info-item\">
              <span class=\"info-label\" data-i18n=\"detail.responsibility\">Responsibility</span>
              <span class=\"info-value\">" <> task.responsibility <> "</span>
            </div>
            <div class=\"info-item\">
              <span class=\"info-label\" data-i18n=\"detail.created\">Created</span>
              <span class=\"info-value\">" <> task.created_at <> "</span>
            </div>
          </div>
        </div>
      </div>

      <!-- Status Change -->
      <div class=\"card\">
        <div class=\"card-header\">
          <span class=\"card-icon\">🔄</span>
          <span class=\"card-title\" data-i18n=\"detail.changeStatus\">Change Status</span>
        </div>
        <div class=\"card-body\">
          <div class=\"status-buttons\">
            " <> render_status_button(task.id, TaskPending, task.status) <> "
            " <> render_status_button(task.id, TaskInProgress, task.status) <> "
            " <> render_status_button(task.id, TaskWaiting, task.status) <> "
            " <> render_status_button(task.id, TaskCompleted, task.status) <> "
            " <> render_status_button(task.id, TaskCancelled, task.status) <> "
          </div>
        </div>
      </div>

      <!-- Comments -->
      <div class=\"card\">
        <div class=\"card-header\">
          <span class=\"card-icon\">💬</span>
          <span class=\"card-title\" data-i18n=\"detail.comments\">Comments</span>
          <span class=\"badge\">" <> int.to_string(list.length(comments)) <> "</span>
        </div>
        <div class=\"card-body\">
          <div class=\"comments-list\">
            " <> string.join(list.map(comments, render_comment), "") <> "
          </div>
          <form class=\"comment-form\" hx-post=\"/api/v1/tasks/" <> int.to_string(task.id) <> "/comments\" hx-swap=\"beforeend\" hx-target=\".comments-list\">
            <textarea name=\"comment\" class=\"form-textarea\" rows=\"2\" placeholder=\"Add a comment...\"></textarea>
            <button type=\"submit\" class=\"btn btn-success btn-sm\">Add</button>
          </form>
        </div>
      </div>

      <!-- History -->
      <div class=\"card\">
        <div class=\"card-header\">
          <span class=\"card-icon\">📜</span>
          <span class=\"card-title\" data-i18n=\"detail.history\">Status History</span>
        </div>
        <div class=\"card-body\">
          <div class=\"history-timeline\">
            " <> string.join(list.map(history, render_history_entry), "") <> "
          </div>
        </div>
      </div>
    </div>

    <!-- Actions -->
    <div class=\"detail-actions\">
      <button class=\"btn btn-danger\" onclick=\"archiveTask(" <> int.to_string(task.id) <> ")\" data-i18n=\"btn.archive\">🗑️ Archive</button>
    </div>
  </div>"
}

fn render_status_button(task_id: Int, status: TaskStatus, current: TaskStatus) -> String {
  let #(icon, label, class) = case status {
    TaskPending -> #("⏳", "Pending", "pending")
    TaskInProgress -> #("🔄", "In Progress", "in-progress")
    TaskWaiting -> #("⏸️", "Waiting", "waiting")
    TaskCompleted -> #("✅", "Completed", "completed")
    TaskCancelled -> #("❌", "Cancelled", "cancelled")
  }
  let active = case status == current { True -> " active" False -> "" }
  let status_str = tasks.status_to_string(status)

  "<button class=\"status-btn " <> class <> active <> "\"
          hx-patch=\"/api/v1/tasks/" <> int.to_string(task_id) <> "/status\"
          hx-vals='{\"status\": \"" <> status_str <> "\"}'
          hx-swap=\"none\">
    " <> icon <> " " <> label <> "
  </button>"
}

fn render_comment(comment: TaskComment) -> String {
  "<div class=\"comment\">
    <div class=\"comment-header\">
      <span class=\"comment-author\">User #" <> int.to_string(comment.author_telegram_id) <> "</span>
      <span class=\"comment-date\">" <> comment.created_at <> "</span>
    </div>
    <div class=\"comment-text\">" <> comment.comment_text <> "</div>
  </div>"
}

fn render_history_entry(entry: StatusHistoryEntry) -> String {
  let old = option.unwrap(entry.old_status, "created")
  let arrow = case old { "created" -> "" _ -> old <> " → " }

  "<div class=\"history-entry\">
    <div class=\"history-dot\"></div>
    <div class=\"history-content\">
      <div class=\"history-change\">" <> arrow <> entry.new_status <> "</div>
      <div class=\"history-meta\">
        <span class=\"history-by\">" <> option.unwrap(entry.changed_by, "System") <> "</span>
        <span class=\"history-date\">" <> entry.changed_at <> "</span>
      </div>
      " <> case entry.comment {
        Some(c) -> "<div class=\"history-comment\">" <> c <> "</div>"
        None -> ""
      } <> "
    </div>
  </div>"
}

// =============================================================================
// TASK CARD COMPONENTS
// =============================================================================

/// Full task card for list view
pub fn render_task_card(task: Task) -> String {
  "<a href=\"/tasks/" <> int.to_string(task.id) <> "\" class=\"task-card\">
    <div class=\"task-card-header\">
      " <> render_priority_badge(task.priority) <> "
      " <> render_status_badge(task.status) <> "
    </div>
    <div class=\"task-card-title\">" <> task.title <> "</div>
    <div class=\"task-card-footer\">
      " <> case task.contact_name {
        Some(name) -> "<span class=\"task-contact\">👤 " <> name <> "</span>"
        None -> ""
      } <> "
      " <> case task.due_date {
        Some(date) -> "<span class=\"task-due\">📅 " <> date <> "</span>"
        None -> ""
      } <> "
      " <> render_category_badge(task.category) <> "
    </div>
  </a>"
}

/// Mini task card for kanban/matrix views
fn render_task_card_mini(task: Task) -> String {
  "<a href=\"/tasks/" <> int.to_string(task.id) <> "\" class=\"task-card-mini\">
    <div class=\"mini-priority\">" <> priority_dot(task.priority) <> "</div>
    <div class=\"mini-content\">
      <div class=\"mini-title\">" <> task.title <> "</div>
      " <> case task.contact_name {
        Some(name) -> "<div class=\"mini-contact\">👤 " <> name <> "</div>"
        None -> ""
      } <> "
    </div>
  </a>"
}

/// Compact task list for dashboard
fn render_task_list_compact(tasks: List(Task), context: String) -> String {
  case list.length(tasks) {
    0 -> "<div class=\"empty-compact\" data-i18n=\"empty." <> context <> "\">No tasks</div>"
    _ -> "<div class=\"task-list-compact\">" <> string.join(list.map(tasks, render_task_card_mini), "") <> "</div>"
  }
}

// =============================================================================
// BADGE COMPONENTS
// =============================================================================

pub fn render_priority_badge(priority: Int) -> String {
  let #(class, icon, label) = case priority {
    1 -> #("priority-high", "🔴", "High")
    2 -> #("priority-medium", "🟡", "Medium")
    _ -> #("priority-low", "🟢", "Low")
  }
  "<span class=\"badge " <> class <> "\">" <> icon <> " " <> label <> "</span>"
}

fn priority_dot(priority: Int) -> String {
  case priority {
    1 -> "🔴"
    2 -> "🟡"
    _ -> "🟢"
  }
}

pub fn render_status_badge(status: TaskStatus) -> String {
  let #(class, icon, label) = case status {
    TaskPending -> #("status-pending", "⏳", "Pending")
    TaskInProgress -> #("status-in-progress", "🔄", "In Progress")
    TaskWaiting -> #("status-waiting", "⏸️", "Waiting")
    TaskCompleted -> #("status-completed", "✅", "Completed")
    TaskCancelled -> #("status-cancelled", "❌", "Cancelled")
  }
  "<span class=\"badge " <> class <> "\">" <> icon <> " " <> label <> "</span>"
}

pub fn render_category_badge(category: TaskCategory) -> String {
  let #(icon, label) = case category {
    CategoryConversation -> #("💬", "Chat")
    CategoryMeeting -> #("📞", "Meeting")
    CategoryProject -> #("👥", "Project")
    CategoryPromise -> #("✅", "Promise")
    CategoryOther -> #("📋", "Other")
  }
  "<span class=\"badge category\">" <> icon <> " " <> label <> "</span>"
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
  --accent-cyan: #00d4ff;
  --border-color: #2d3748;
  --priority-high: #ff4444;
  --priority-medium: #ffaa00;
  --priority-low: #00ff88;
  --status-pending: #888888;
  --status-in-progress: #00d4ff;
  --status-waiting: #ff9800;
  --status-completed: #00ff88;
  --status-cancelled: #ff4444;
}

* { margin: 0; padding: 0; box-sizing: border-box; }

body {
  font-family: 'JetBrains Mono', 'Fira Code', monospace;
  background: var(--bg-primary);
  color: var(--text-primary);
  min-height: 100vh;
  font-size: 14px;
}

a { color: inherit; text-decoration: none; }

.app-container {
  display: flex;
  flex-direction: column;
  min-height: 100vh;
}

/* Header */
.header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0.75rem 1.5rem;
  background: var(--bg-secondary);
  border-bottom: 1px solid var(--border-color);
  position: sticky;
  top: 0;
  z-index: 100;
}

.header-left, .header-right { display: flex; align-items: center; gap: 1.5rem; }

.logo { display: flex; align-items: center; gap: 0.5rem; }
.logo-icon { font-size: 1.5rem; }
.logo-text { font-size: 1.2rem; font-weight: bold; color: var(--accent-green); }

.nav-links { display: flex; gap: 0.5rem; }
.nav-link {
  color: var(--text-secondary);
  padding: 0.5rem 0.75rem;
  border-radius: 4px;
  transition: all 0.2s;
}
.nav-link:hover { color: var(--text-primary); background: var(--bg-hover); }
.nav-link.active { color: var(--accent-green); background: rgba(0, 255, 136, 0.1); }

.lang-switcher { display: flex; gap: 0.25rem; }
.lang-btn {
  padding: 0.3rem 0.5rem;
  background: transparent;
  border: 1px solid var(--border-color);
  color: var(--text-secondary);
  border-radius: 4px;
  cursor: pointer;
  font-size: 0.75rem;
}
.lang-btn.active { border-color: var(--accent-green); color: var(--accent-green); }

/* Content Layout */
.app-content { display: flex; flex: 1; overflow: hidden; }

/* Sidebar */
.sidebar {
  width: 240px;
  background: var(--bg-secondary);
  border-right: 1px solid var(--border-color);
  padding: 1rem;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
  gap: 1.5rem;
}

.sidebar-nav { display: flex; flex-direction: column; gap: 0.25rem; }
.sidebar-link {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  padding: 0.6rem 0.75rem;
  border-radius: 6px;
  color: var(--text-secondary);
  transition: all 0.2s;
}
.sidebar-link:hover { color: var(--text-primary); background: var(--bg-hover); }
.sidebar-link.active { color: var(--accent-green); background: rgba(0, 255, 136, 0.1); }
.sidebar-icon { font-size: 1.1rem; }

.sidebar-section { margin-top: 0.5rem; }
.sidebar-title {
  font-size: 0.7rem;
  text-transform: uppercase;
  color: var(--text-muted);
  padding: 0 0.5rem;
  margin-bottom: 0.5rem;
}

.filter-badges { display: flex; flex-wrap: wrap; gap: 0.4rem; }
.filter-badge {
  display: flex;
  align-items: center;
  gap: 0.3rem;
  padding: 0.3rem 0.6rem;
  background: var(--bg-card);
  border: 1px solid var(--border-color);
  border-radius: 12px;
  font-size: 0.75rem;
  color: var(--text-secondary);
  transition: all 0.2s;
}
.filter-badge:hover { border-color: var(--accent-blue); color: var(--text-primary); }
.badge-dot { width: 6px; height: 6px; border-radius: 50%; background: currentColor; }
.filter-badge.pending .badge-dot { background: var(--status-pending); }
.filter-badge.in-progress .badge-dot { background: var(--status-in-progress); }
.filter-badge.waiting .badge-dot { background: var(--status-waiting); }
.filter-badge.completed .badge-dot { background: var(--status-completed); }

/* Main Content */
.main {
  flex: 1;
  padding: 1.5rem;
  overflow-y: auto;
}

/* Cards */
.card {
  background: var(--bg-card);
  border: 1px solid var(--border-color);
  border-radius: 8px;
  margin-bottom: 1rem;
}
.card-header {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.75rem 1rem;
  background: rgba(0, 0, 0, 0.2);
  border-bottom: 1px solid var(--border-color);
}
.card-icon { font-size: 1rem; }
.card-title { font-weight: 600; flex: 1; }
.card-body { padding: 1rem; }

/* Stats Grid */
.stats-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
  gap: 1rem;
  margin-bottom: 1.5rem;
}

.stat-card {
  background: var(--bg-card);
  border: 1px solid var(--border-color);
  border-radius: 8px;
  padding: 1rem;
  display: flex;
  align-items: center;
  gap: 1rem;
}
.stat-card.blue { border-left: 3px solid var(--accent-blue); }
.stat-card.green { border-left: 3px solid var(--accent-green); }
.stat-card.red { border-left: 3px solid var(--accent-red); }
.stat-card.gray { border-left: 3px solid var(--text-muted); }
.stat-card.cyan { border-left: 3px solid var(--accent-cyan); }

.stat-icon { font-size: 1.5rem; }
.stat-value { font-size: 1.5rem; font-weight: bold; color: var(--text-primary); }
.stat-label { font-size: 0.8rem; color: var(--text-secondary); }

/* Completion Ring */
.stat-card.completion {
  flex-direction: column;
  align-items: center;
  justify-content: center;
}
.completion-ring { position: relative; width: 60px; height: 60px; }
.completion-ring svg { width: 100%; height: 100%; transform: rotate(-90deg); }
.ring-bg { fill: none; stroke: var(--border-color); stroke-width: 3; }
.ring-progress { fill: none; stroke: var(--accent-green); stroke-width: 3; stroke-linecap: round; }
.completion-value {
  position: absolute;
  top: 50%; left: 50%;
  transform: translate(-50%, -50%);
  font-size: 0.9rem;
  font-weight: bold;
}

/* Task Cards */
.task-list { display: flex; flex-direction: column; gap: 0.5rem; }

.task-card {
  display: block;
  background: var(--bg-card);
  border: 1px solid var(--border-color);
  border-radius: 8px;
  padding: 1rem;
  transition: all 0.2s;
}
.task-card:hover { border-color: var(--accent-blue); transform: translateY(-2px); }

.task-card-header { display: flex; gap: 0.5rem; margin-bottom: 0.5rem; }
.task-card-title { font-weight: 600; margin-bottom: 0.5rem; }
.task-card-footer { display: flex; gap: 0.75rem; font-size: 0.8rem; color: var(--text-secondary); }

.task-card-mini {
  display: flex;
  gap: 0.5rem;
  padding: 0.6rem;
  background: var(--bg-secondary);
  border: 1px solid var(--border-color);
  border-radius: 6px;
  margin-bottom: 0.4rem;
  transition: all 0.2s;
}
.task-card-mini:hover { border-color: var(--accent-blue); }
.mini-priority { font-size: 0.7rem; }
.mini-title { font-size: 0.85rem; font-weight: 500; }
.mini-contact { font-size: 0.75rem; color: var(--text-secondary); }

/* Badges */
.badge {
  display: inline-flex;
  align-items: center;
  gap: 0.25rem;
  padding: 0.2rem 0.5rem;
  border-radius: 4px;
  font-size: 0.75rem;
  font-weight: 500;
}
.badge.priority-high { background: rgba(255, 68, 68, 0.15); color: var(--priority-high); }
.badge.priority-medium { background: rgba(255, 170, 0, 0.15); color: var(--priority-medium); }
.badge.priority-low { background: rgba(0, 255, 136, 0.15); color: var(--priority-low); }
.badge.status-pending { background: rgba(136, 136, 136, 0.15); color: var(--status-pending); }
.badge.status-in-progress { background: rgba(0, 212, 255, 0.15); color: var(--status-in-progress); }
.badge.status-waiting { background: rgba(255, 152, 0, 0.15); color: var(--status-waiting); }
.badge.status-completed { background: rgba(0, 255, 136, 0.15); color: var(--status-completed); }
.badge.status-cancelled { background: rgba(255, 68, 68, 0.15); color: var(--status-cancelled); }
.badge.category { background: rgba(170, 0, 255, 0.1); color: var(--accent-purple); }
.badge-red { background: var(--accent-red); color: white; }
.badge-blue { background: var(--accent-blue); color: white; }

/* Kanban Board */
.kanban-board { display: flex; gap: 1rem; overflow-x: auto; padding-bottom: 1rem; }
.kanban-column {
  flex: 1;
  min-width: 250px;
  background: var(--bg-secondary);
  border: 1px solid var(--border-color);
  border-radius: 8px;
}
.kanban-header {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.75rem;
  border-bottom: 1px solid var(--border-color);
}
.kanban-column.pending .kanban-header { border-top: 3px solid var(--status-pending); border-radius: 8px 8px 0 0; }
.kanban-column.in_progress .kanban-header { border-top: 3px solid var(--status-in-progress); border-radius: 8px 8px 0 0; }
.kanban-column.waiting .kanban-header { border-top: 3px solid var(--status-waiting); border-radius: 8px 8px 0 0; }
.kanban-column.completed .kanban-header { border-top: 3px solid var(--status-completed); border-radius: 8px 8px 0 0; }
.kanban-title { flex: 1; font-weight: 600; }
.kanban-count {
  background: var(--bg-card);
  padding: 0.2rem 0.5rem;
  border-radius: 10px;
  font-size: 0.75rem;
}
.kanban-cards { padding: 0.75rem; max-height: 500px; overflow-y: auto; }

/* Eisenhower Matrix */
.eisenhower-matrix { display: flex; flex-direction: column; gap: 1rem; }
.matrix-row { display: flex; gap: 1rem; }
.matrix-quadrant {
  flex: 1;
  min-height: 200px;
  background: var(--bg-secondary);
  border: 1px solid var(--border-color);
  border-radius: 8px;
}
.matrix-quadrant.q1 { border-top: 3px solid var(--accent-red); }
.matrix-quadrant.q2 { border-top: 3px solid var(--accent-blue); }
.matrix-quadrant.q3 { border-top: 3px solid var(--accent-orange); }
.matrix-quadrant.q4 { border-top: 3px solid var(--text-muted); }
.quadrant-header {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.75rem;
  border-bottom: 1px solid var(--border-color);
}
.quadrant-icon { font-size: 1.2rem; }
.quadrant-info { flex: 1; }
.quadrant-title { font-weight: 600; display: block; }
.quadrant-subtitle { font-size: 0.75rem; color: var(--text-secondary); }
.quadrant-count {
  background: var(--bg-card);
  padding: 0.2rem 0.5rem;
  border-radius: 10px;
  font-size: 0.75rem;
}
.quadrant-tasks { padding: 0.75rem; max-height: 300px; overflow-y: auto; }

/* Page Headers */
.page-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 1.5rem; }
.page-title { font-size: 1.5rem; font-weight: bold; }

/* View Switcher */
.view-switcher { display: flex; gap: 0.25rem; }
.view-btn {
  padding: 0.5rem 0.75rem;
  background: var(--bg-card);
  border: 1px solid var(--border-color);
  color: var(--text-secondary);
  border-radius: 4px;
  cursor: pointer;
  font-size: 1rem;
}
.view-btn:hover { border-color: var(--accent-blue); }
.view-btn.active { border-color: var(--accent-green); color: var(--accent-green); background: rgba(0, 255, 136, 0.1); }

/* Form Styles */
.task-form { max-width: 600px; }
.form-group { margin-bottom: 1.25rem; }
.form-group label {
  display: block;
  margin-bottom: 0.5rem;
  font-weight: 500;
  color: var(--text-secondary);
}
.form-row { display: flex; gap: 1rem; }
.form-row .form-group { flex: 1; }

.form-input, .form-select, .form-textarea {
  width: 100%;
  padding: 0.75rem;
  background: var(--bg-secondary);
  border: 1px solid var(--border-color);
  border-radius: 6px;
  color: var(--text-primary);
  font-family: inherit;
  font-size: 0.9rem;
}
.form-input:focus, .form-select:focus, .form-textarea:focus {
  outline: none;
  border-color: var(--accent-blue);
}
.form-textarea { resize: vertical; min-height: 80px; }

.radio-group { display: flex; gap: 1rem; flex-wrap: wrap; }
.radio-label {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.5rem 0.75rem;
  background: var(--bg-secondary);
  border: 1px solid var(--border-color);
  border-radius: 6px;
  cursor: pointer;
  transition: all 0.2s;
}
.radio-label:hover { border-color: var(--accent-blue); }
.radio-label input[type=\"radio\"] { display: none; }
.radio-label:has(input:checked) { border-color: var(--accent-green); background: rgba(0, 255, 136, 0.1); }

.form-actions { display: flex; gap: 1rem; margin-top: 2rem; }

/* Buttons */
.btn {
  display: inline-flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.6rem 1rem;
  border: 1px solid var(--border-color);
  background: var(--bg-secondary);
  color: var(--text-primary);
  border-radius: 6px;
  cursor: pointer;
  font-family: inherit;
  font-size: 0.9rem;
  transition: all 0.2s;
}
.btn:hover { border-color: var(--accent-blue); }
.btn-success { border-color: var(--accent-green); color: var(--accent-green); }
.btn-success:hover { background: rgba(0, 255, 136, 0.15); }
.btn-danger { border-color: var(--accent-red); color: var(--accent-red); }
.btn-danger:hover { background: rgba(255, 68, 68, 0.15); }
.btn-secondary { color: var(--text-secondary); }
.btn-sm { padding: 0.4rem 0.6rem; font-size: 0.8rem; }

/* Detail Page */
.detail-header { margin-bottom: 1.5rem; }
.detail-meta { display: flex; gap: 0.5rem; margin-bottom: 0.75rem; }
.detail-title { font-size: 1.5rem; font-weight: bold; margin-bottom: 0.5rem; }
.detail-contact { color: var(--text-secondary); }
.detail-description { margin-bottom: 1rem; line-height: 1.6; }
.detail-description.muted { color: var(--text-muted); font-style: italic; }
.detail-info-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 1rem; }
.info-item { }
.info-label { display: block; font-size: 0.75rem; color: var(--text-secondary); margin-bottom: 0.25rem; }
.info-value { font-weight: 500; }

/* Status Buttons */
.status-buttons { display: flex; flex-wrap: wrap; gap: 0.5rem; }
.status-btn {
  padding: 0.5rem 1rem;
  background: var(--bg-secondary);
  border: 1px solid var(--border-color);
  border-radius: 20px;
  color: var(--text-secondary);
  cursor: pointer;
  font-family: inherit;
  font-size: 0.85rem;
  transition: all 0.2s;
}
.status-btn:hover { border-color: var(--accent-blue); color: var(--text-primary); }
.status-btn.active { border-color: var(--accent-green); color: var(--accent-green); background: rgba(0, 255, 136, 0.1); }
.status-btn.pending.active { border-color: var(--status-pending); color: var(--status-pending); background: rgba(136, 136, 136, 0.1); }
.status-btn.in-progress.active { border-color: var(--status-in-progress); color: var(--status-in-progress); background: rgba(0, 212, 255, 0.1); }
.status-btn.waiting.active { border-color: var(--status-waiting); color: var(--status-waiting); background: rgba(255, 152, 0, 0.1); }
.status-btn.completed.active { border-color: var(--status-completed); color: var(--status-completed); background: rgba(0, 255, 136, 0.1); }
.status-btn.cancelled.active { border-color: var(--status-cancelled); color: var(--status-cancelled); background: rgba(255, 68, 68, 0.1); }

/* Comments */
.comments-list { margin-bottom: 1rem; }
.comment {
  padding: 0.75rem;
  background: var(--bg-secondary);
  border-radius: 6px;
  margin-bottom: 0.5rem;
}
.comment-header { display: flex; justify-content: space-between; margin-bottom: 0.5rem; }
.comment-author { font-weight: 500; color: var(--accent-blue); }
.comment-date { font-size: 0.75rem; color: var(--text-muted); }
.comment-text { line-height: 1.5; }
.comment-form { display: flex; gap: 0.5rem; }
.comment-form textarea { flex: 1; }

/* History Timeline */
.history-timeline { padding-left: 1rem; border-left: 2px solid var(--border-color); }
.history-entry { position: relative; padding: 0.5rem 0 0.5rem 1rem; }
.history-dot {
  position: absolute;
  left: -1.35rem;
  top: 0.75rem;
  width: 10px;
  height: 10px;
  background: var(--accent-blue);
  border-radius: 50%;
}
.history-change { font-weight: 500; margin-bottom: 0.25rem; }
.history-meta { font-size: 0.75rem; color: var(--text-secondary); }
.history-comment { font-size: 0.85rem; color: var(--text-muted); margin-top: 0.25rem; font-style: italic; }

/* Detail Actions */
.detail-actions {
  margin-top: 2rem;
  padding-top: 1rem;
  border-top: 1px solid var(--border-color);
}

/* Empty States */
.empty-state {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 3rem;
  text-align: center;
}
.empty-icon { font-size: 3rem; margin-bottom: 1rem; }
.empty-text { color: var(--text-secondary); margin-bottom: 1rem; }
.empty-compact { color: var(--text-muted); font-size: 0.85rem; text-align: center; padding: 1rem; }

/* Responsive */
@media (max-width: 768px) {
  .sidebar { display: none; }
  .header { padding: 0.5rem 1rem; }
  .main { padding: 1rem; }
  .form-row { flex-direction: column; }
  .kanban-board { flex-direction: column; }
  .matrix-row { flex-direction: column; }
  .stats-grid { grid-template-columns: repeat(2, 1fr); }
}
"
}

// =============================================================================
// I18N SCRIPT
// =============================================================================

fn i18n_script() -> String {
  "const i18n = {
  en: {
    'nav.home': 'Home',
    'nav.tasks': 'Tasks',
    'nav.p2p': 'P2P',
    'btn.newTask': 'New Task',
    'btn.createTask': 'Create Task',
    'btn.cancel': 'Cancel',
    'btn.archive': 'Archive',
    'sidebar.dashboard': 'Dashboard',
    'sidebar.allTasks': 'All Tasks',
    'sidebar.today': 'Today',
    'sidebar.overdue': 'Overdue',
    'sidebar.byContact': 'By Contact',
    'sidebar.reports': 'Reports',
    'sidebar.quickFilters': 'Quick Filters',
    'sidebar.priority': 'Priority',
    'status.pending': 'Pending',
    'status.inProgress': 'In Progress',
    'status.waiting': 'Waiting',
    'status.completed': 'Completed',
    'priority.high': 'High',
    'priority.medium': 'Medium',
    'priority.low': 'Low',
    'stat.total': 'Total Tasks',
    'stat.pending': 'Pending',
    'stat.inProgress': 'In Progress',
    'stat.completed': 'Completed',
    'stat.overdue': 'Overdue',
    'stat.completionRate': 'Completion Rate',
    'dashboard.overdueTasks': 'Overdue Tasks',
    'dashboard.todayTasks': \"Today's Tasks\",
    'page.allTasks': 'All Tasks',
    'page.createTask': 'Create Task',
    'empty.noTasks': 'No tasks found',
    'empty.overdue': 'No overdue tasks',
    'empty.today': 'No tasks for today',
    'form.title': 'Title',
    'form.description': 'Description',
    'form.contact': 'Contact',
    'form.category': 'Category',
    'form.priority': 'Priority',
    'form.dueDate': 'Due Date',
    'form.responsibility': 'Responsibility',
    'detail.info': 'Information',
    'detail.changeStatus': 'Change Status',
    'detail.comments': 'Comments',
    'detail.history': 'Status History',
    'detail.noDescription': 'No description',
    'detail.dueDate': 'Due Date',
    'detail.responsibility': 'Responsibility',
    'detail.created': 'Created',
  },
  ru: {
    'nav.home': 'Главная',
    'nav.tasks': 'Задачи',
    'nav.p2p': 'P2P',
    'btn.newTask': 'Новая задача',
    'btn.createTask': 'Создать задачу',
    'btn.cancel': 'Отмена',
    'btn.archive': 'Архивировать',
    'sidebar.dashboard': 'Дашборд',
    'sidebar.allTasks': 'Все задачи',
    'sidebar.today': 'Сегодня',
    'sidebar.overdue': 'Просроченные',
    'sidebar.byContact': 'По контактам',
    'sidebar.reports': 'Отчёты',
    'sidebar.quickFilters': 'Быстрые фильтры',
    'sidebar.priority': 'Приоритет',
    'status.pending': 'Ожидает',
    'status.inProgress': 'В работе',
    'status.waiting': 'На паузе',
    'status.completed': 'Выполнено',
    'priority.high': 'Высокий',
    'priority.medium': 'Средний',
    'priority.low': 'Низкий',
    'stat.total': 'Всего задач',
    'stat.pending': 'Ожидают',
    'stat.inProgress': 'В работе',
    'stat.completed': 'Выполнено',
    'stat.overdue': 'Просрочено',
    'stat.completionRate': 'Выполнение',
    'dashboard.overdueTasks': 'Просроченные задачи',
    'dashboard.todayTasks': 'Задачи на сегодня',
    'page.allTasks': 'Все задачи',
    'page.createTask': 'Создать задачу',
    'empty.noTasks': 'Задачи не найдены',
    'empty.overdue': 'Нет просроченных задач',
    'empty.today': 'Нет задач на сегодня',
    'form.title': 'Название',
    'form.description': 'Описание',
    'form.contact': 'Контакт',
    'form.category': 'Категория',
    'form.priority': 'Приоритет',
    'form.dueDate': 'Дедлайн',
    'form.responsibility': 'Ответственность',
    'detail.info': 'Информация',
    'detail.changeStatus': 'Изменить статус',
    'detail.comments': 'Комментарии',
    'detail.history': 'История статусов',
    'detail.noDescription': 'Нет описания',
    'detail.dueDate': 'Дедлайн',
    'detail.responsibility': 'Ответственность',
    'detail.created': 'Создано',
  }
};

let currentLang = localStorage.getItem('lang') || 'en';

function setLang(lang) {
  currentLang = lang;
  localStorage.setItem('lang', lang);
  document.querySelectorAll('.lang-btn').forEach(btn => {
    btn.classList.toggle('active', btn.dataset.lang === lang);
  });
  applyTranslations();
}

function applyTranslations() {
  document.querySelectorAll('[data-i18n]').forEach(el => {
    const key = el.dataset.i18n;
    if (i18n[currentLang][key]) {
      el.textContent = i18n[currentLang][key];
    }
  });
}

// Apply translations on load
document.addEventListener('DOMContentLoaded', () => {
  setLang(currentLang);
});
"
}

// =============================================================================
// CLIENT SCRIPT
// =============================================================================

fn client_script() -> String {
  "// View mode switching
function setView(mode) {
  const url = new URL(window.location.href);
  url.searchParams.set('view', mode);
  window.location.href = url.toString();
}

// Archive task
function archiveTask(taskId) {
  if (confirm('Are you sure you want to archive this task?')) {
    fetch('/api/v1/tasks/' + taskId, { method: 'DELETE' })
      .then(() => window.location.href = '/tasks')
      .catch(err => alert('Error: ' + err));
  }
}

// WebSocket for real-time updates
let ws;
function connectWebSocket() {
  const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
  ws = new WebSocket(protocol + '//' + location.host + '/ws/tasks');

  ws.onopen = () => console.log('TaskFlow WebSocket connected');
  ws.onclose = () => setTimeout(connectWebSocket, 3000);
  ws.onerror = (err) => console.error('WebSocket error:', err);

  ws.onmessage = (event) => {
    const data = JSON.parse(event.data);
    if (data.type === 'task_updated' || data.type === 'task_created') {
      // Trigger HTMX refresh
      document.body.dispatchEvent(new CustomEvent('taskUpdated'));
    }
  };
}

// Connect on load
document.addEventListener('DOMContentLoaded', connectWebSocket);
"
}
