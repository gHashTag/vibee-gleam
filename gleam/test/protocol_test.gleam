// Test for MCP protocol parsing

import gleam/option.{None, Some}
import gleeunit/should
import vibee/mcp/protocol

pub fn extract_int_field_test() {
  let input = "{\"jsonrpc\":\"2.0\",\"id\":42,\"method\":\"initialize\"}"
  let result = protocol.parse_request(input)

  case result {
    Ok(req) -> {
      case req.id {
        Some(42) -> should.be_true(True)
        Some(_other) -> {
          // Show what we got
          should.fail()
        }
        None -> {
          // ID was not parsed
          should.fail()
        }
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn extract_id_at_end_test() {
  let input = "{\"method\":\"test\",\"jsonrpc\":\"2.0\",\"id\":123}"
  let result = protocol.parse_request(input)

  case result {
    Ok(req) -> {
      should.equal(req.id, Some(123))
    }
    Error(_) -> should.fail()
  }
}
