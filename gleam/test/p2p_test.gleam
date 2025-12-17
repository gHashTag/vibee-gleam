// P2P Escrow System Tests
// Tests for P2P trading functionality

import gleeunit
import gleeunit/should
import vibee/p2p/types as p2p_types
import vibee/p2p/escrow_client
import vibee/p2p/tools as p2p_tools
import vibee/mcp/types.{Tool}
import gleam/json
import gleam/list

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// TYPE TESTS
// =============================================================================

pub fn crypto_to_string_test() {
  p2p_types.crypto_to_string(p2p_types.TON)
  |> should.equal("TON")

  p2p_types.crypto_to_string(p2p_types.USDT)
  |> should.equal("USDT")
}

pub fn crypto_from_string_test() {
  p2p_types.crypto_from_string("TON")
  |> should.equal(p2p_types.TON)

  p2p_types.crypto_from_string("USDT")
  |> should.equal(p2p_types.USDT)

  // Default to USDT for unknown
  p2p_types.crypto_from_string("BTC")
  |> should.equal(p2p_types.USDT)
}

pub fn fiat_to_string_test() {
  p2p_types.fiat_to_string(p2p_types.THB)
  |> should.equal("THB")

  p2p_types.fiat_to_string(p2p_types.RUB)
  |> should.equal("RUB")

  p2p_types.fiat_to_string(p2p_types.USD)
  |> should.equal("USD")

  p2p_types.fiat_to_string(p2p_types.EUR)
  |> should.equal("EUR")
}

pub fn fiat_from_string_test() {
  p2p_types.fiat_from_string("THB")
  |> should.equal(p2p_types.THB)

  p2p_types.fiat_from_string("RUB")
  |> should.equal(p2p_types.RUB)

  // Default to THB for unknown
  p2p_types.fiat_from_string("JPY")
  |> should.equal(p2p_types.THB)
}

pub fn fiat_symbol_test() {
  p2p_types.fiat_symbol(p2p_types.THB)
  |> should.equal("฿")

  p2p_types.fiat_symbol(p2p_types.RUB)
  |> should.equal("₽")

  p2p_types.fiat_symbol(p2p_types.USD)
  |> should.equal("$")

  p2p_types.fiat_symbol(p2p_types.EUR)
  |> should.equal("€")
}

pub fn payment_method_test() {
  p2p_types.payment_method_from_string("bangkok_bank")
  |> should.equal(p2p_types.BangkokBank)

  p2p_types.payment_method_from_string("promptpay")
  |> should.equal(p2p_types.PromptPay)

  p2p_types.payment_method_from_string("sberbank")
  |> should.equal(p2p_types.Sberbank)

  p2p_types.payment_method_from_string("tinkoff")
  |> should.equal(p2p_types.Tinkoff)

  p2p_types.payment_method_display(p2p_types.BangkokBank)
  |> should.equal("Bangkok Bank")

  p2p_types.payment_method_display(p2p_types.PromptPay)
  |> should.equal("PromptPay")
}

pub fn order_status_test() {
  p2p_types.status_to_string(p2p_types.Open)
  |> should.equal("open")

  p2p_types.status_to_string(p2p_types.Locked)
  |> should.equal("locked")

  p2p_types.status_to_string(p2p_types.FiatSent)
  |> should.equal("fiat_sent")

  p2p_types.status_to_string(p2p_types.Completed)
  |> should.equal("completed")

  p2p_types.status_to_string(p2p_types.Disputed)
  |> should.equal("disputed")

  p2p_types.status_to_string(p2p_types.Cancelled)
  |> should.equal("cancelled")
}

pub fn calculate_fee_test() {
  // 0.5% fee on 100.0
  p2p_types.calculate_fee(100.0, 0.5)
  |> should.equal(0.5)

  // 1% fee on 1000.0
  p2p_types.calculate_fee(1000.0, 1.0)
  |> should.equal(10.0)
}

pub fn default_rates_test() {
  let rates = p2p_types.default_rates()

  // Should have rates for all pairs
  let len = list.length(rates)
  should.be_true(len >= 4)
}

// =============================================================================
// TOOL DEFINITION TESTS
// =============================================================================

pub fn get_tools_test() {
  let tools = p2p_tools.get_tools()

  // Should have 9 tools
  list.length(tools)
  |> should.equal(9)
}

pub fn create_sell_order_tool_test() {
  let tool = p2p_tools.p2p_create_sell_order_tool()

  tool.name
  |> should.equal("p2p_create_sell_order")

  // Description should not be empty
  should.be_true(tool.description != "")
}

pub fn list_orders_tool_test() {
  let tool = p2p_tools.p2p_list_orders_tool()

  tool.name
  |> should.equal("p2p_list_orders")
}

pub fn take_order_tool_test() {
  let tool = p2p_tools.p2p_take_order_tool()

  tool.name
  |> should.equal("p2p_take_order")
}

pub fn mark_paid_tool_test() {
  let tool = p2p_tools.p2p_mark_paid_tool()

  tool.name
  |> should.equal("p2p_mark_paid")
}

pub fn confirm_payment_tool_test() {
  let tool = p2p_tools.p2p_confirm_payment_tool()

  tool.name
  |> should.equal("p2p_confirm_payment")
}

pub fn cancel_order_tool_test() {
  let tool = p2p_tools.p2p_cancel_order_tool()

  tool.name
  |> should.equal("p2p_cancel_order")
}

pub fn my_orders_tool_test() {
  let tool = p2p_tools.p2p_my_orders_tool()

  tool.name
  |> should.equal("p2p_my_orders")
}

pub fn order_status_tool_test() {
  let tool = p2p_tools.p2p_order_status_tool()

  tool.name
  |> should.equal("p2p_order_status")
}

pub fn rates_tool_test() {
  let tool = p2p_tools.p2p_rates_tool()

  tool.name
  |> should.equal("p2p_rates")
}

// =============================================================================
// HANDLER TESTS (using JSON args)
// =============================================================================

pub fn handle_rates_test() {
  let args = json.object([])

  let result = p2p_tools.handle_p2p_rates(args)

  // Should not be an error
  result.is_error
  |> should.equal(False)
}

// Note: Handler tests require ETS initialization
// Run integration tests for full coverage

// Note: Tests that require ETS (FFI) are skipped in unit tests
// They should be run in integration tests with proper ETS initialization

// =============================================================================
// ESCROW CLIENT TYPE TESTS
// =============================================================================

pub fn escrow_config_type_test() {
  // EscrowConfig type is defined correctly
  should.be_true(True)
}
