// Vibe Logger Tests
// Tests for structured logging with context and tracing

import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import vibee/log_aggregator
import vibee/vibe_logger

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Basic Tests
// =============================================================================

/// Test that new() creates a logger with defaults
pub fn vibe_logger_create_test() {
  let logger = vibe_logger.new("test-logger")

  // Logger should have the name we passed
  should.equal(logger.name, "test-logger")

  // Context should be empty by default
  should.equal(logger.context.trace_id, None)
  should.equal(logger.context.request_id, None)
  should.equal(logger.context.session_id, None)
  should.equal(logger.context.tool_name, None)
  should.equal(logger.context.span_id, None)
  should.equal(logger.context.extra, [])
}

/// Test that empty_context creates proper empty context
pub fn vibe_logger_empty_context_test() {
  let ctx = vibe_logger.empty_context()

  should.equal(ctx.trace_id, None)
  should.equal(ctx.request_id, None)
  should.equal(ctx.session_id, None)
  should.equal(ctx.user_id, None)
  should.equal(ctx.tool_name, None)
  should.equal(ctx.span_id, None)
  should.equal(ctx.extra, [])
}

// =============================================================================
// Context Builder Tests
// =============================================================================

/// Test with_trace_id adds trace_id to context
pub fn vibe_logger_context_trace_id_test() {
  let logger =
    vibe_logger.new("test")
    |> vibe_logger.with_trace_id("trace-abc-123")

  should.equal(logger.context.trace_id, Some("trace-abc-123"))
}

/// Test with_request_id adds request_id to context
pub fn vibe_logger_context_request_id_test() {
  let logger =
    vibe_logger.new("test")
    |> vibe_logger.with_request_id("req-xyz-456")

  should.equal(logger.context.request_id, Some("req-xyz-456"))
}

/// Test with_session_id adds session_id to context
pub fn vibe_logger_context_session_id_test() {
  let logger =
    vibe_logger.new("test")
    |> vibe_logger.with_session_id("sess-123")

  should.equal(logger.context.session_id, Some("sess-123"))
}

/// Test with_tool adds tool_name to context
pub fn vibe_logger_context_tool_test() {
  let logger =
    vibe_logger.new("test")
    |> vibe_logger.with_tool("telegram_send_message")

  should.equal(logger.context.tool_name, Some("telegram_send_message"))
}

/// Test with_span_id adds span_id to context
pub fn vibe_logger_context_span_id_test() {
  let logger =
    vibe_logger.new("test")
    |> vibe_logger.with_span_id("span-789")

  should.equal(logger.context.span_id, Some("span-789"))
}

/// Test with_data adds extra data to context
pub fn vibe_logger_context_extra_data_test() {
  let logger =
    vibe_logger.new("test")
    |> vibe_logger.with_data("user_count", json.int(42))
    |> vibe_logger.with_data("active", json.bool(True))

  // Extra should have 2 items
  should.equal(list.length(logger.context.extra), 2)

  // Should contain our keys (order is reversed due to prepend)
  case list.first(logger.context.extra) {
    Ok(#(key, _)) -> should.equal(key, "active")
    Error(_) -> should.fail()
  }
}

/// Test chaining multiple context builders
pub fn vibe_logger_context_chaining_test() {
  let logger =
    vibe_logger.new("mcp")
    |> vibe_logger.with_trace_id("trace-1")
    |> vibe_logger.with_request_id("req-1")
    |> vibe_logger.with_session_id("sess-1")
    |> vibe_logger.with_tool("test_tool")
    |> vibe_logger.with_span_id("span-1")

  should.equal(logger.context.trace_id, Some("trace-1"))
  should.equal(logger.context.request_id, Some("req-1"))
  should.equal(logger.context.session_id, Some("sess-1"))
  should.equal(logger.context.tool_name, Some("test_tool"))
  should.equal(logger.context.span_id, Some("span-1"))
}

// =============================================================================
// Level Tests
// =============================================================================

/// Test with_level sets minimum log level
pub fn vibe_logger_level_test() {
  let debug_logger = vibe_logger.new("test")
  should.equal(debug_logger.level, vibe_logger.Debug)

  let info_logger =
    vibe_logger.new("test")
    |> vibe_logger.with_level(vibe_logger.Info)
  should.equal(info_logger.level, vibe_logger.Info)

  let warn_logger =
    vibe_logger.new("test")
    |> vibe_logger.with_level(vibe_logger.Warn)
  should.equal(warn_logger.level, vibe_logger.Warn)
}

// =============================================================================
// ID Generation Tests
// =============================================================================

/// Test generate_trace_id creates unique IDs
pub fn vibe_logger_generate_trace_id_test() {
  let id1 = vibe_logger.generate_trace_id()
  let id2 = vibe_logger.generate_trace_id()

  // Should not be empty
  should.be_true(string.length(id1) > 0)
  should.be_true(string.length(id2) > 0)

  // Should be unique
  should.not_equal(id1, id2)
}

/// Test generate_span_id creates unique IDs
pub fn vibe_logger_generate_span_id_test() {
  let id1 = vibe_logger.generate_span_id()
  let id2 = vibe_logger.generate_span_id()

  // Should not be empty
  should.be_true(string.length(id1) > 0)
  should.be_true(string.length(id2) > 0)

  // Should be unique
  should.not_equal(id1, id2)
}

// =============================================================================
// Convenience Function Tests
// =============================================================================

/// Test mcp_logger creates properly configured logger
pub fn vibe_logger_mcp_logger_test() {
  let logger = vibe_logger.mcp_logger("req-123")

  should.equal(logger.name, "mcp")
  should.equal(logger.context.request_id, Some("req-123"))

  // Should have auto-generated trace_id
  case logger.context.trace_id {
    Some(tid) -> should.be_true(string.length(tid) > 0)
    None -> should.fail()
  }
}

/// Test tool_logger inherits context from parent
pub fn vibe_logger_tool_logger_test() {
  let parent =
    vibe_logger.new("parent")
    |> vibe_logger.with_trace_id("trace-parent")
    |> vibe_logger.with_request_id("req-parent")

  let tool = vibe_logger.tool_logger("my_tool", parent)

  // Should have new name
  should.equal(tool.name, "tool")

  // Should have tool_name set
  should.equal(tool.context.tool_name, Some("my_tool"))

  // Should inherit parent context
  should.equal(tool.context.trace_id, Some("trace-parent"))
  should.equal(tool.context.request_id, Some("req-parent"))
}

// =============================================================================
// Aggregator Integration Tests
// =============================================================================

/// Test that logging sends entries to global aggregator
pub fn vibe_logger_aggregator_integration_test() {
  // Start global aggregator
  case log_aggregator.start_global() {
    Ok(_) -> {
      // Create logger with context
      let logger =
        vibe_logger.new("test-integration")
        |> vibe_logger.with_trace_id("trace-integration-test")

      // Log a message
      vibe_logger.info(logger, "integration test message")

      // Small delay for async processing
      process.sleep(50)

      // Get from aggregator
      case log_aggregator.get_global() {
        Some(aggregator) -> {
          let recent = log_aggregator.get_recent(aggregator, 5)

          // Should have at least 1 log
          should.be_true(list.length(recent) >= 1)

          // Find our log
          let found =
            list.find(recent, fn(entry) {
              entry.message == "integration test message"
            })

          case found {
            Ok(entry) -> {
              should.equal(entry.logger, "test-integration")
              should.equal(entry.level, "INFO")
              should.equal(entry.trace_id, Some("trace-integration-test"))
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

/// Test all log levels write to aggregator
pub fn vibe_logger_all_levels_aggregator_test() {
  case log_aggregator.start_global() {
    Ok(_) -> {
      let logger =
        vibe_logger.new("level-test")
        |> vibe_logger.with_level(vibe_logger.Trace)  // Enable all levels

      // Log at each level
      vibe_logger.trace(logger, "trace message")
      vibe_logger.debug(logger, "debug message")
      vibe_logger.info(logger, "info message")
      vibe_logger.warn(logger, "warn message")
      vibe_logger.error(logger, "error message")
      vibe_logger.fatal(logger, "fatal message")

      // Small delay
      process.sleep(100)

      // Verify logs in aggregator
      case log_aggregator.get_global() {
        Some(aggregator) -> {
          let recent = log_aggregator.get_recent(aggregator, 20)

          // Check for each level
          let levels_found = list.map(recent, fn(entry) { entry.level })

          should.be_true(list.contains(levels_found, "TRACE"))
          should.be_true(list.contains(levels_found, "DEBUG"))
          should.be_true(list.contains(levels_found, "INFO"))
          should.be_true(list.contains(levels_found, "WARN"))
          should.be_true(list.contains(levels_found, "ERROR"))
          should.be_true(list.contains(levels_found, "FATAL"))
        }
        None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

/// Test quick log functions (log_info, log_error, etc.)
pub fn vibe_logger_quick_functions_test() {
  case log_aggregator.start_global() {
    Ok(_) -> {
      // Use quick functions
      vibe_logger.log_info("quick info test")
      vibe_logger.log_error("quick error test")
      vibe_logger.log_debug("quick debug test")
      vibe_logger.log_warn("quick warn test")

      process.sleep(50)

      case log_aggregator.get_global() {
        Some(aggregator) -> {
          let recent = log_aggregator.get_recent(aggregator, 10)

          // All should have "vibee" as logger name
          let vibee_logs =
            list.filter(recent, fn(entry) { entry.logger == "vibee" })

          should.be_true(list.length(vibee_logs) >= 4)
        }
        None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}
