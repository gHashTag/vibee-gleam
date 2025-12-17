// Logging E2E Integration Tests
// Full flow tests for the vibe logging system

import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import vibee/log_aggregator.{LogEntry}
import vibee/vibe_logger

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// E2E Flow Tests
// =============================================================================

/// Test full flow: Logger → Aggregator → Subscriber
pub fn logging_full_flow_test() {
  // Start aggregator
  case log_aggregator.start() {
    Ok(aggregator) -> {
      // Create subscriber
      let subscriber = process.new_subject()
      log_aggregator.subscribe(aggregator, subscriber)

      // Create log entry directly (bypassing global)
      let entry = LogEntry(
        timestamp: "2024-01-01T00:00:00.000Z",
        level: "INFO",
        logger: "e2e-test",
        message: "full flow test message",
        trace_id: Some("trace-e2e"),
        request_id: Some("req-e2e"),
        session_id: Some("sess-e2e"),
        span_id: None,
        tool: Some("test_tool"),
        extra: [],
      )

      // Send to aggregator
      log_aggregator.log(aggregator, entry)

      // Subscriber should receive JSON broadcast
      case process.receive(subscriber, 1000) {
        Ok(json_str) -> {
          // Verify JSON contains all fields
          should.be_true(string.contains(json_str, "full flow test message"))
          should.be_true(string.contains(json_str, "INFO"))
          should.be_true(string.contains(json_str, "e2e-test"))
          should.be_true(string.contains(json_str, "trace-e2e"))
          should.be_true(string.contains(json_str, "req-e2e"))
          should.be_true(string.contains(json_str, "sess-e2e"))
          should.be_true(string.contains(json_str, "test_tool"))
        }
        Error(_) -> should.fail()
      }

      // Also verify stored in aggregator
      let recent = log_aggregator.get_recent(aggregator, 1)
      should.equal(list.length(recent), 1)

      case list.first(recent) {
        Ok(stored) -> {
          should.equal(stored.message, "full flow test message")
          should.equal(stored.trace_id, Some("trace-e2e"))
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

/// Test MCP logger creates proper context chain
pub fn logging_mcp_context_test() {
  case log_aggregator.start_global() {
    Ok(_) -> {
      // Simulate MCP request flow
      let mcp_logger = vibe_logger.mcp_logger("mcp-request-123")

      // Log at MCP level
      vibe_logger.info(mcp_logger, "Processing MCP request")

      // Create tool logger inheriting context
      let tool_logger = vibe_logger.tool_logger("telegram_send_message", mcp_logger)

      // Log at tool level
      vibe_logger.debug(tool_logger, "Sending message to Telegram")

      process.sleep(50)

      case log_aggregator.get_global() {
        Some(aggregator) -> {
          let recent = log_aggregator.get_recent(aggregator, 10)

          // Find MCP log
          let mcp_log = list.find(recent, fn(e) {
            e.message == "Processing MCP request"
          })

          case mcp_log {
            Ok(entry) -> {
              should.equal(entry.logger, "mcp")
              should.equal(entry.request_id, Some("mcp-request-123"))
              // Should have auto-generated trace_id
              case entry.trace_id {
                Some(_) -> should.be_true(True)
                None -> should.fail()
              }
            }
            Error(_) -> should.fail()
          }

          // Find tool log
          let tool_log = list.find(recent, fn(e) {
            e.message == "Sending message to Telegram"
          })

          case tool_log {
            Ok(entry) -> {
              should.equal(entry.logger, "tool")
              should.equal(entry.tool, Some("telegram_send_message"))
              // Should inherit request_id from parent
              should.equal(entry.request_id, Some("mcp-request-123"))
            }
            Error(_) -> should.fail()
          }
        }
        None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

/// Test tool logger inherits full parent context
pub fn logging_tool_context_test() {
  case log_aggregator.start_global() {
    Ok(_) -> {
      // Create parent with full context
      let parent =
        vibe_logger.new("parent")
        |> vibe_logger.with_trace_id("trace-parent-123")
        |> vibe_logger.with_request_id("req-parent-456")
        |> vibe_logger.with_session_id("sess-parent-789")
        |> vibe_logger.with_span_id("span-parent-abc")

      // Create child tool logger
      let tool = vibe_logger.tool_logger("child_tool", parent)

      // Log with child
      vibe_logger.info(tool, "child tool execution")

      process.sleep(50)

      case log_aggregator.get_global() {
        Some(aggregator) -> {
          let recent = log_aggregator.get_recent(aggregator, 5)

          let child_log = list.find(recent, fn(e) {
            e.message == "child tool execution"
          })

          case child_log {
            Ok(entry) -> {
              // Tool logger should inherit ALL context from parent
              should.equal(entry.trace_id, Some("trace-parent-123"))
              should.equal(entry.request_id, Some("req-parent-456"))
              should.equal(entry.session_id, Some("sess-parent-789"))
              should.equal(entry.span_id, Some("span-parent-abc"))
              // Plus have its own tool name
              should.equal(entry.tool, Some("child_tool"))
            }
            Error(_) -> should.fail()
          }
        }
        None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

/// Test multiple concurrent subscribers receive broadcasts
pub fn logging_concurrent_subscribers_test() {
  case log_aggregator.start() {
    Ok(aggregator) -> {
      // Create 3 subscribers
      let sub1 = process.new_subject()
      let sub2 = process.new_subject()
      let sub3 = process.new_subject()

      log_aggregator.subscribe(aggregator, sub1)
      log_aggregator.subscribe(aggregator, sub2)
      log_aggregator.subscribe(aggregator, sub3)

      // Send a log
      let entry = LogEntry(
        timestamp: "2024-01-01T00:00:00.000Z",
        level: "INFO",
        logger: "concurrent-test",
        message: "broadcast to all",
        trace_id: None,
        request_id: None,
        session_id: None,
        span_id: None,
        tool: None,
        extra: [],
      )

      log_aggregator.log(aggregator, entry)

      // All 3 subscribers should receive the message
      case process.receive(sub1, 1000) {
        Ok(json1) -> {
          should.be_true(string.contains(json1, "broadcast to all"))

          case process.receive(sub2, 1000) {
            Ok(json2) -> {
              should.be_true(string.contains(json2, "broadcast to all"))

              case process.receive(sub3, 1000) {
                Ok(json3) -> {
                  should.be_true(string.contains(json3, "broadcast to all"))
                  // All 3 should be identical
                  should.equal(json1, json2)
                  should.equal(json2, json3)
                }
                Error(_) -> should.fail()
              }
            }
            Error(_) -> should.fail()
          }
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

/// Test extra data is preserved in log entries
pub fn logging_extra_data_test() {
  case log_aggregator.start_global() {
    Ok(_) -> {
      let logger =
        vibe_logger.new("extra-test")
        |> vibe_logger.with_data("user_count", json.int(42))
        |> vibe_logger.with_data("status", json.string("active"))
        |> vibe_logger.with_data("metrics", json.object([
          #("latency_ms", json.int(123)),
          #("success", json.bool(True)),
        ]))

      vibe_logger.info(logger, "log with extra data")

      process.sleep(50)

      case log_aggregator.get_global() {
        Some(aggregator) -> {
          let recent = log_aggregator.get_recent(aggregator, 5)

          let log_entry = list.find(recent, fn(e) {
            e.message == "log with extra data"
          })

          case log_entry {
            Ok(entry) -> {
              // Should have 3 extra fields
              should.equal(list.length(entry.extra), 3)

              // Verify extra data keys exist
              let keys = list.map(entry.extra, fn(pair) { pair.0 })
              should.be_true(list.contains(keys, "user_count"))
              should.be_true(list.contains(keys, "status"))
              should.be_true(list.contains(keys, "metrics"))
            }
            Error(_) -> should.fail()
          }
        }
        None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}
