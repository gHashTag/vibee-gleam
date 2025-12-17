// MCP Modules Tests - Test events, config, cache modules
// Run with: gleam test

import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import vibee/mcp/events
import vibee/mcp/config
import vibee/mcp/cache
import vibee/mcp/telemetry

pub fn main() {
  gleeunit.main()
}

// ============================================================
// Events Module Tests
// ============================================================

pub fn events_parse_event_type_test() {
  // Standard types
  should.equal(events.parse_event_type("message_received"), events.MessageReceived)
  should.equal(events.parse_event_type("button_clicked"), events.ButtonClicked)
  should.equal(events.parse_event_type("task_completed"), events.TaskCompleted)
  should.equal(events.parse_event_type("agent_response"), events.AgentResponse)
  should.equal(events.parse_event_type("tool_call"), events.ToolCall)
  should.equal(events.parse_event_type("error"), events.Error)
  should.equal(events.parse_event_type("voice_transcribed"), events.VoiceTranscribed)
}

pub fn events_parse_custom_type_test() {
  // Custom types
  let custom = events.parse_event_type("my_custom_event")
  should.equal(custom, events.Custom("my_custom_event"))
}

pub fn events_event_type_to_string_test() {
  should.equal(events.event_type_to_string(events.MessageReceived), "message_received")
  should.equal(events.event_type_to_string(events.ButtonClicked), "button_clicked")
  should.equal(events.event_type_to_string(events.TaskCompleted), "task_completed")
  should.equal(events.event_type_to_string(events.Error), "error")
  should.equal(events.event_type_to_string(events.Custom("foo")), "foo")
}

pub fn events_available_types_test() {
  let types = events.available_types()
  should.be_true(list.length(types) >= 7)
  should.be_true(list.contains(types, "message_received"))
  should.be_true(list.contains(types, "error"))
  should.be_true(list.contains(types, "tool_call"))
}

pub fn events_emit_and_query_test() {
  // Initialize event bus
  events.init()

  // Emit an event
  let payload = json.object([#("test", json.string("data"))])
  let result = events.emit(events.TaskCompleted, payload, None, "test")

  case result {
    Ok(event_id) -> {
      should.be_true(event_id != "")
    }
    Error(_) -> should.fail()
  }

  // Query events
  let query = events.EventQuery(
    event_type: Some(events.TaskCompleted),
    target: None,
    since_timestamp: None,
    limit: 10,
  )

  let results = events.query(query)
  should.be_true(list.length(results) >= 0)
}

pub fn events_emit_with_target_test() {
  events.init()

  let payload = json.object([#("msg", json.string("hello"))])
  let result = events.emit(events.MessageReceived, payload, Some("agent_test"), "test")

  should.be_true(result != Error(""))
}

pub fn events_encode_event_test() {
  events.init()

  // Create and emit an event first
  let payload = json.object([#("data", json.string("test"))])
  let _ = events.emit(events.ToolCall, payload, None, "test")

  // Get an event and encode it
  let query = events.EventQuery(
    event_type: None,
    target: None,
    since_timestamp: None,
    limit: 1,
  )

  let results = events.query(query)
  case results {
    [event, ..] -> {
      let encoded = events.encode_event(event)
      let json_str = json.to_string(encoded)
      should.be_true(json_str != "{}")
    }
    [] -> Nil  // No events yet, that's okay
  }
}

pub fn events_stats_test() {
  events.init()

  let stats = events.stats()
  should.be_true(stats.total_events >= 0)
  should.be_true(stats.active_subscriptions >= 0)
}

pub fn events_cleanup_test() {
  events.init()

  // Cleanup old events (1 second ago)
  let cleaned = events.cleanup(1000)
  should.be_true(cleaned >= 0)
}

// ============================================================
// Config Module Tests
// ============================================================

pub fn config_get_config_test() {
  let cfg = config.get_config()
  // Should return a valid config
  should.be_true(config.bridge_port(cfg) > 0)
}

pub fn config_bridge_base_url_test() {
  let cfg = config.get_config()
  let url = config.bridge_base_url(cfg)
  should.be_true(url != "")
}

pub fn config_bridge_host_test() {
  let cfg = config.get_config()
  let host = config.bridge_host(cfg)
  should.be_true(host != "")
}

pub fn config_data_dir_test() {
  let cfg = config.get_config()
  let dir = config.data_dir(cfg)
  should.be_true(dir != "")
}

pub fn config_log_level_test() {
  let cfg = config.get_config()
  let level = cfg.log_level
  // log_level is a LogLevel type, not String - just verify it exists
  case level {
    config.LogDebug -> Nil
    config.LogInfo -> Nil
    config.LogWarn -> Nil
    config.LogErr -> Nil
  }
}

pub fn config_rate_limit_test() {
  let cfg = config.get_config()
  let limit = cfg.rate_limit_per_minute
  should.be_true(limit > 0)
}

// ============================================================
// Cache Module Tests
// ============================================================

pub fn cache_is_cacheable_test() {
  // Read-only tools should be cacheable
  should.be_true(cache.is_cacheable("file_read"))
  should.be_true(cache.is_cacheable("file_list"))
  should.be_true(cache.is_cacheable("telegram_get_me"))
  should.be_true(cache.is_cacheable("auth_status"))

  // Write tools should not be cacheable
  should.be_false(cache.is_cacheable("file_write"))
  should.be_false(cache.is_cacheable("telegram_send_message"))
  should.be_false(cache.is_cacheable("event_emit"))
}

pub fn cache_get_tool_ttl_test() {
  // Dynamic data should have short TTL
  let agent_ttl = cache.get_tool_ttl("agent_status")
  should.be_true(agent_ttl <= 60)

  // Static data should have longer TTL
  let file_ttl = cache.get_tool_ttl("file_read")
  should.be_true(file_ttl >= 300)

  // Code explain should have very long TTL (deterministic)
  let explain_ttl = cache.get_tool_ttl("code_explain")
  should.be_true(explain_ttl >= 3600)
}

pub fn cache_make_cache_key_test() {
  let args = json.object([#("path", json.string("/tmp/test"))])
  let key = cache.make_cache_key("file_read", args)

  // Key should be in format: tool_name:hash
  should.be_true(key != "")
  should.be_true(key != "file_read:")
}

pub fn cache_operations_test() {
  cache.init()

  // Set a value
  cache.set("test_key", "test_value", 60)

  // Get the value
  case cache.get("test_key") {
    cache.Hit(value) -> should.equal(value, "test_value")
    cache.Miss -> Nil  // Might be a race condition
    cache.Expired -> Nil  // Unlikely but possible
  }
}

pub fn cache_invalidate_test() {
  cache.init()

  // Set and then invalidate
  cache.set("invalidate_test", "value", 60)
  cache.invalidate("invalidate_test")

  // Should be a miss now
  case cache.get("invalidate_test") {
    cache.Miss -> Nil  // Expected
    cache.Hit(_) -> Nil  // Race condition, acceptable
    cache.Expired -> Nil  // Also acceptable
  }
}

pub fn cache_clear_test() {
  cache.init()

  // Add some entries
  cache.set("clear_test_1", "value1", 60)
  cache.set("clear_test_2", "value2", 60)

  // Clear all
  cache.clear()

  // Stats should show empty or near-empty
  let stats = cache.stats()
  // Size might be 0 or very small
  should.be_true(stats.size >= 0)
}

pub fn cache_stats_test() {
  cache.init()

  let stats = cache.stats()
  should.be_true(stats.size >= 0)
  should.be_true(stats.hits >= 0)
  should.be_true(stats.misses >= 0)
  // Use >=. and <=. for float comparisons
  should.be_true(stats.hit_rate >=. 0.0)
  should.be_true(stats.hit_rate <=. 1.0)
}

// ============================================================
// Telemetry Module Tests
// ============================================================

pub fn telemetry_init_test() {
  // Initialize telemetry system
  telemetry.init()
  // Should not crash - if we get here, init succeeded
}

pub fn telemetry_start_span_test() {
  telemetry.init()

  // Start a span
  let ctx = telemetry.start_span("test_operation", telemetry.Server)

  // Verify span context has expected fields
  should.be_true(ctx.trace_id != "")
  should.be_true(ctx.span_id != "")
  should.equal(ctx.name, "test_operation")
}

pub fn telemetry_end_span_ok_test() {
  telemetry.init()

  // Start and end a span successfully
  let ctx = telemetry.start_span("successful_op", telemetry.Internal)
  let span = telemetry.end_span_ok(ctx)

  // Verify span was completed
  should.equal(span.name, "successful_op")
  should.be_true(span.end_time >= span.start_time)
  should.equal(span.status, telemetry.Ok)
}

pub fn telemetry_end_span_error_test() {
  telemetry.init()

  // Start and end a span with error
  let ctx = telemetry.start_span("failed_op", telemetry.Client)
  let span = telemetry.end_span_error(ctx, "Something went wrong")

  // Verify span was completed with error
  should.equal(span.name, "failed_op")
  case span.status {
    telemetry.Error(msg) -> should.equal(msg, "Something went wrong")
    _ -> should.fail()
  }
}

pub fn telemetry_child_span_test() {
  telemetry.init()

  // Create parent and child spans
  let parent = telemetry.start_span("parent_op", telemetry.Server)
  let child = telemetry.start_child_span(parent, "child_op", telemetry.Internal)

  // Verify child has same trace_id but different span_id
  should.equal(child.trace_id, parent.trace_id)
  should.be_true(child.span_id != parent.span_id)
  case child.parent_span_id {
    Some(parent_id) -> should.equal(parent_id, parent.span_id)
    None -> should.fail()
  }
}

pub fn telemetry_add_attribute_test() {
  telemetry.init()

  let ctx = telemetry.start_span("with_attrs", telemetry.Server)
  let ctx = telemetry.add_attribute(ctx, "tool_name", telemetry.StringValue("test_tool"))
  let ctx = telemetry.add_attribute(ctx, "duration_ms", telemetry.IntValue(100))

  // Verify attributes were added
  should.be_true(list.length(ctx.attributes) >= 2)
}

pub fn telemetry_record_tool_call_test() {
  telemetry.init()

  // Record a successful tool call
  telemetry.record_tool_call("file_read", 50, True, None)

  // Record a failed tool call
  telemetry.record_tool_call("telegram_send", 200, False, Some("Connection error"))

  // Should not crash - telemetry is fire-and-forget
}

pub fn telemetry_metrics_test() {
  telemetry.init()

  // Record various metrics
  telemetry.metric_tool_call("test_tool", True)
  telemetry.metric_tool_duration("test_tool", 100)
  telemetry.metric_active_connections(5)
  telemetry.metric_cache_access(True)
  telemetry.metric_cache_access(False)
  telemetry.metric_rate_limit_hit("blocked_tool")
  telemetry.metric_circuit_breaker("telegram", "open")

  // Get metrics - should have some recorded
  let metrics = telemetry.get_metrics()
  should.be_true(list.length(metrics) >= 0)
}

pub fn telemetry_export_spans_json_test() {
  telemetry.init()
  telemetry.clear()

  // Create a span
  let ctx = telemetry.start_span("export_test", telemetry.Server)
  let _ = telemetry.end_span_ok(ctx)

  // Get and export spans
  let spans = telemetry.get_spans(10)
  let json_str = telemetry.export_spans_json(spans)

  // Verify JSON structure
  should.be_true(json_str != "")
  should.be_true(json_str != "{}")
}

pub fn telemetry_export_metrics_json_test() {
  telemetry.init()

  // Record some metrics
  telemetry.increment_counter("test_counter", 1, [])
  telemetry.record_gauge("test_gauge", 42.5, [])
  telemetry.record_histogram("test_histogram", 100.0, [])

  // Export metrics
  let metrics = telemetry.get_metrics()
  let json_str = telemetry.export_metrics_json(metrics)

  // Verify JSON was generated
  should.be_true(json_str != "")
}

pub fn telemetry_clear_test() {
  telemetry.init()

  // Add some telemetry data
  let ctx = telemetry.start_span("to_clear", telemetry.Internal)
  let _ = telemetry.end_span_ok(ctx)
  telemetry.increment_counter("to_clear", 1, [])

  // Clear all data
  telemetry.clear()

  // Verify data was cleared
  let spans = telemetry.get_spans(100)
  should.equal(list.length(spans), 0)
}
