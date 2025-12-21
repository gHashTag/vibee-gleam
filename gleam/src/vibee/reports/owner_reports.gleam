// Owner Reports Module
// Sends activity reports and growth strategy recommendations to the owner

import gleam/float
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/string

// ============================================================
// Types
// ============================================================

pub type ActivityStats {
  ActivityStats(
    total_messages: Int,
    total_generations: Int,
    new_users: Int,
    active_users: Int,
    total_revenue_stars: Int,
    period: String,
  )
}

pub type GrowthMetrics {
  GrowthMetrics(
    user_growth_percent: Float,
    revenue_growth_percent: Float,
    conversion_rate: Float,
    top_features: List(String),
    top_prompts: List(String),
  )
}

pub type GrowthStrategy {
  GrowthStrategy(title: String, description: String, priority: Int, action: String)
}

pub type AlertType {
  ErrorAlert
  HighLoadAlert
  UnusualActivityAlert
  PaymentAlert
  LowBalanceAlert
}

pub type OwnerReport {
  DailyActivityReport(stats: ActivityStats)
  WeeklyGrowthReport(metrics: GrowthMetrics, strategies: List(GrowthStrategy))
  AlertReport(alert_type: AlertType, message: String)
}

// ============================================================
// Report Generation
// ============================================================

/// Generate daily activity summary
pub fn generate_daily_summary() -> Result(ActivityStats, String) {
  io.println("[REPORTS] Generating daily summary...")

  // TODO: Integrate with actual database queries
  // For now, return placeholder stats
  Ok(ActivityStats(
    total_messages: 0,
    total_generations: 0,
    new_users: 0,
    active_users: 0,
    total_revenue_stars: 0,
    period: "24h",
  ))
}

/// Generate weekly growth report
pub fn generate_weekly_report() -> Result(#(GrowthMetrics, List(GrowthStrategy)), String) {
  io.println("[REPORTS] Generating weekly growth report...")

  // Analyze trends
  let metrics = GrowthMetrics(
    user_growth_percent: 0.0,
    revenue_growth_percent: 0.0,
    conversion_rate: 0.0,
    top_features: [],
    top_prompts: [],
  )

  // Generate strategies based on data
  let strategies = analyze_growth_opportunities()

  Ok(#(metrics, strategies))
}

/// Analyze data and suggest growth strategies
pub fn analyze_growth_opportunities() -> List(GrowthStrategy) {
  [
    GrowthStrategy(
      title: "Promote NeuroPhoto",
      description: "NeuroPhoto has high engagement. Consider promoting it more.",
      priority: 1,
      action: "Create promotional posts showcasing best generations",
    ),
    GrowthStrategy(
      title: "Increase conversion with trials",
      description: "Offer 3 free generations to new users to increase conversion.",
      priority: 2,
      action: "Implement free trial system",
    ),
    GrowthStrategy(
      title: "Add referral program",
      description: "Users who invite friends get bonus stars.",
      priority: 3,
      action: "Create referral tracking and rewards",
    ),
    GrowthStrategy(
      title: "Expand LoRA collection",
      description: "Add new LoRA models based on popular prompt themes.",
      priority: 4,
      action: "Train new LoRAs on trending styles",
    ),
  ]
}

// ============================================================
// Report Formatting
// ============================================================

/// Format report for Telegram message
pub fn format_report_message(report: OwnerReport) -> String {
  case report {
    DailyActivityReport(stats) -> format_daily_report(stats)
    WeeklyGrowthReport(metrics, strategies) -> format_weekly_report(metrics, strategies)
    AlertReport(alert_type, message) -> format_alert(alert_type, message)
  }
}

fn format_daily_report(stats: ActivityStats) -> String {
  "Daily Activity Report (" <> stats.period <> ")\n\n"
  <> "Messages: " <> int.to_string(stats.total_messages) <> "\n"
  <> "Active Users: " <> int.to_string(stats.active_users) <> "\n"
  <> "Generations: " <> int.to_string(stats.total_generations) <> "\n"
  <> "New Users: " <> int.to_string(stats.new_users) <> "\n"
  <> "Revenue (Stars): " <> int.to_string(stats.total_revenue_stars)
}

fn format_weekly_report(metrics: GrowthMetrics, strategies: List(GrowthStrategy)) -> String {
  let base = "Weekly Growth Report\n\n"
    <> "User Growth: " <> float_to_percent(metrics.user_growth_percent) <> "\n"
    <> "Revenue Growth: " <> float_to_percent(metrics.revenue_growth_percent) <> "\n"
    <> "Conversion Rate: " <> float_to_percent(metrics.conversion_rate) <> "\n\n"
    <> "Growth Strategies:\n"

  let strategy_text = list.map(strategies, fn(s) {
    "- [P" <> int.to_string(s.priority) <> "] " <> s.title <> ": " <> s.description
  })
  |> string.join("\n")

  base <> strategy_text
}

fn format_alert(alert_type: AlertType, message: String) -> String {
  let type_emoji = case alert_type {
    ErrorAlert -> "ERROR"
    HighLoadAlert -> "HIGH LOAD"
    UnusualActivityAlert -> "UNUSUAL"
    PaymentAlert -> "PAYMENT"
    LowBalanceAlert -> "LOW BALANCE"
  }
  "[" <> type_emoji <> "] " <> message
}

// ============================================================
// Report Sending
// ============================================================

/// Send report to owner via Telegram
pub fn send_owner_report(
  owner_telegram_id: String,
  report: OwnerReport,
  bridge_url: String,
  session_id: String,
) -> Result(Nil, String) {
  let message = format_report_message(report)

  io.println("[REPORTS] Sending report to owner: " <> owner_telegram_id)
  io.println("[REPORTS] Message: " <> string.slice(message, 0, 100) <> "...")

  // Use Telegram API to send message
  case send_telegram_message(bridge_url, session_id, owner_telegram_id, message) {
    Ok(_) -> {
      io.println("[REPORTS] Report sent successfully")
      Ok(Nil)
    }
    Error(err) -> {
      io.println("[REPORTS] Failed to send report: " <> err)
      Error(err)
    }
  }
}

/// Create and send daily report
pub fn send_daily_report(
  owner_telegram_id: String,
  bridge_url: String,
  session_id: String,
) -> Result(Nil, String) {
  case generate_daily_summary() {
    Ok(stats) -> {
      let report = DailyActivityReport(stats)
      send_owner_report(owner_telegram_id, report, bridge_url, session_id)
    }
    Error(err) -> Error("Failed to generate daily report: " <> err)
  }
}

/// Create and send weekly growth report
pub fn send_weekly_report(
  owner_telegram_id: String,
  bridge_url: String,
  session_id: String,
) -> Result(Nil, String) {
  case generate_weekly_report() {
    Ok(#(metrics, strategies)) -> {
      let report = WeeklyGrowthReport(metrics, strategies)
      send_owner_report(owner_telegram_id, report, bridge_url, session_id)
    }
    Error(err) -> Error("Failed to generate weekly report: " <> err)
  }
}

/// Send alert to owner
pub fn send_alert(
  owner_telegram_id: String,
  alert_type: AlertType,
  message: String,
  bridge_url: String,
  session_id: String,
) -> Result(Nil, String) {
  let report = AlertReport(alert_type, message)
  send_owner_report(owner_telegram_id, report, bridge_url, session_id)
}

// ============================================================
// MCP Tool Interface
// ============================================================

/// Get daily stats as JSON for MCP tool
pub fn get_daily_stats_json() -> String {
  case generate_daily_summary() {
    Ok(stats) -> {
      json.object([
        #("total_messages", json.int(stats.total_messages)),
        #("total_generations", json.int(stats.total_generations)),
        #("new_users", json.int(stats.new_users)),
        #("active_users", json.int(stats.active_users)),
        #("total_revenue_stars", json.int(stats.total_revenue_stars)),
        #("period", json.string(stats.period)),
      ])
      |> json.to_string()
    }
    Error(err) -> {
      json.object([#("error", json.string(err))])
      |> json.to_string()
    }
  }
}

/// Get growth strategies as JSON for MCP tool
pub fn get_growth_strategies_json() -> String {
  let strategies = analyze_growth_opportunities()

  json.object([
    #("strategies", json.array(strategies, fn(s) {
      json.object([
        #("title", json.string(s.title)),
        #("description", json.string(s.description)),
        #("priority", json.int(s.priority)),
        #("action", json.string(s.action)),
      ])
    })),
  ])
  |> json.to_string()
}

// ============================================================
// Helper Functions
// ============================================================


fn float_to_percent(f: Float) -> String {
  // Simple conversion using gleam/float
  case f {
    0.0 -> "0%"
    _ -> {
      let percent = float.truncate(f *. 100.0)
      int.to_string(percent) <> "%"
    }
  }
}

fn send_telegram_message(
  _bridge_url: String,
  _session_id: String,
  chat_id: String,
  text: String,
) -> Result(String, String) {
  // Implementation would use httpc to call bridge API
  // For now, just log
  io.println("[REPORTS] Would send to " <> chat_id <> ": " <> text)
  Ok("sent")
}
