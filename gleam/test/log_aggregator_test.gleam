// Log Aggregator Tests
// Tests for OTP actor log_aggregator

import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import vibee/log_aggregator.{type LogEntry, LogEntry}

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Helper Functions
// =============================================================================

fn create_test_entry(msg: String) -> LogEntry {
  LogEntry(
    timestamp: "2024-01-01T00:00:00.000Z",
    level: "INFO",
    logger: "test",
    message: msg,
    trace_id: None,
    request_id: None,
    session_id: None,
    span_id: None,
    tool: None,
    extra: [],
  )
}

fn create_test_entry_with_trace(msg: String, trace_id: String) -> LogEntry {
  LogEntry(
    timestamp: "2024-01-01T00:00:00.000Z",
    level: "INFO",
    logger: "test",
    message: msg,
    trace_id: Some(trace_id),
    request_id: None,
    session_id: None,
    span_id: None,
    tool: None,
    extra: [],
  )
}

// =============================================================================
// Tests
// =============================================================================

/// Test that log_aggregator actor starts successfully
pub fn log_aggregator_start_test() {
  case log_aggregator.start() {
    Ok(_aggregator) -> {
      should.be_true(True)
    }
    Error(_) -> should.fail()
  }
}

/// Test subscribe functionality
pub fn log_aggregator_subscribe_test() {
  case log_aggregator.start() {
    Ok(aggregator) -> {
      let subscriber = process.new_subject()
      log_aggregator.subscribe(aggregator, subscriber)
      // If we get here without error, subscribe worked
      should.be_true(True)
    }
    Error(_) -> should.fail()
  }
}

/// Test broadcast - subscriber receives logs
pub fn log_aggregator_broadcast_test() {
  case log_aggregator.start() {
    Ok(aggregator) -> {
      let subscriber = process.new_subject()
      log_aggregator.subscribe(aggregator, subscriber)

      // Send a log entry
      let entry = create_test_entry("broadcast test message")
      log_aggregator.log(aggregator, entry)

      // Verify we received the broadcast
      case process.receive(subscriber, 1000) {
        Ok(json_str) -> {
          should.be_true(string.contains(json_str, "broadcast test message"))
          should.be_true(string.contains(json_str, "INFO"))
          should.be_true(string.contains(json_str, "test"))
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

/// Test get_recent returns logged entries
pub fn log_aggregator_get_recent_test() {
  case log_aggregator.start() {
    Ok(aggregator) -> {
      // Add some logs
      log_aggregator.log(aggregator, create_test_entry("message 1"))
      log_aggregator.log(aggregator, create_test_entry("message 2"))
      log_aggregator.log(aggregator, create_test_entry("message 3"))

      // Small delay to let actor process
      process.sleep(50)

      // Get recent logs
      let recent = log_aggregator.get_recent(aggregator, 10)

      // Should have 3 logs
      should.equal(list.length(recent), 3)

      // Most recent first (message 3)
      case list.first(recent) {
        Ok(first) -> should.equal(first.message, "message 3")
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

/// Test max_logs limit (1000 logs)
pub fn log_aggregator_max_logs_limit_test() {
  case log_aggregator.start() {
    Ok(aggregator) -> {
      // Add more than 1000 logs (add 1010 to test limit)
      list.range(1, 1010)
      |> list.each(fn(i) {
        let msg = "log message " <> int_to_string(i)
        log_aggregator.log(aggregator, create_test_entry(msg))
      })

      // Small delay
      process.sleep(100)

      // Get all recent (request 2000 to see what we actually have)
      let recent = log_aggregator.get_recent(aggregator, 2000)

      // Should be capped at 1000
      should.equal(list.length(recent), 1000)
    }
    Error(_) -> should.fail()
  }
}

/// Test unsubscribe - subscriber stops receiving logs
pub fn log_aggregator_unsubscribe_test() {
  case log_aggregator.start() {
    Ok(aggregator) -> {
      let subscriber = process.new_subject()
      log_aggregator.subscribe(aggregator, subscriber)

      // Verify we receive logs
      log_aggregator.log(aggregator, create_test_entry("before unsubscribe"))
      case process.receive(subscriber, 500) {
        Ok(_) -> {
          // Good, we received it
          // Now unsubscribe
          log_aggregator.unsubscribe(aggregator, subscriber)

          // Send another log
          log_aggregator.log(aggregator, create_test_entry("after unsubscribe"))

          // Should NOT receive this one (timeout expected)
          case process.receive(subscriber, 500) {
            Ok(_) -> should.fail()  // Should not receive after unsubscribe
            Error(_) -> should.be_true(True)  // Timeout is expected
          }
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

/// Test global registry - start_global and get_global
pub fn log_aggregator_global_registry_test() {
  // First, start global aggregator
  case log_aggregator.start_global() {
    Ok(_) -> {
      // Now get_global should return Some
      case log_aggregator.get_global() {
        Some(aggregator) -> {
          // Should be able to use it
          log_aggregator.log(aggregator, create_test_entry("global test"))
          let recent = log_aggregator.get_recent(aggregator, 1)
          should.equal(list.length(recent), 1)
        }
        None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// =============================================================================
// Helper: Int to String
// =============================================================================

fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ -> do_int_to_string(n, "")
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        9 -> "9"
        _ -> "?"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}
