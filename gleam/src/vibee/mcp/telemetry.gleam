// Telemetry Module for MCP Server
// OpenTelemetry-compatible metrics and tracing

import gleam/json
import gleam/option.{type Option, None, Some}

/// Span represents a unit of work (trace segment)
pub type Span {
  Span(
    trace_id: String,
    span_id: String,
    parent_span_id: Option(String),
    name: String,
    kind: SpanKind,
    start_time: Int,
    end_time: Int,
    status: SpanStatus,
    attributes: List(#(String, AttributeValue)),
  )
}

/// Span kind (OpenTelemetry standard)
pub type SpanKind {
  Internal
  Server
  Client
  Producer
  Consumer
}

/// Span status
pub type SpanStatus {
  Unset
  Ok
  Error(String)
}

/// Attribute value types
pub type AttributeValue {
  StringValue(String)
  IntValue(Int)
  FloatValue(Float)
  BoolValue(Bool)
  StringArrayValue(List(String))
}

/// Metric types
pub type Metric {
  Counter(name: String, value: Int, attributes: List(#(String, String)))
  Gauge(name: String, value: Float, attributes: List(#(String, String)))
  Histogram(name: String, value: Float, attributes: List(#(String, String)))
}

/// Initialize telemetry system
@external(erlang, "vibee_telemetry_ffi", "init")
pub fn init() -> Nil

/// Start a new span
pub fn start_span(name: String, kind: SpanKind) -> SpanContext {
  let trace_id = generate_trace_id()
  let span_id = generate_span_id()
  let start_time = system_time_ns()

  SpanContext(
    trace_id: trace_id,
    span_id: span_id,
    parent_span_id: None,
    name: name,
    kind: kind,
    start_time: start_time,
    attributes: [],
  )
}

/// Start a child span
pub fn start_child_span(parent: SpanContext, name: String, kind: SpanKind) -> SpanContext {
  let span_id = generate_span_id()
  let start_time = system_time_ns()

  SpanContext(
    trace_id: parent.trace_id,
    span_id: span_id,
    parent_span_id: Some(parent.span_id),
    name: name,
    kind: kind,
    start_time: start_time,
    attributes: [],
  )
}

/// End a span with success
pub fn end_span_ok(ctx: SpanContext) -> Span {
  let end_time = system_time_ns()
  let span = Span(
    trace_id: ctx.trace_id,
    span_id: ctx.span_id,
    parent_span_id: ctx.parent_span_id,
    name: ctx.name,
    kind: ctx.kind,
    start_time: ctx.start_time,
    end_time: end_time,
    status: Ok,
    attributes: ctx.attributes,
  )
  record_span(span)
  span
}

/// End a span with error
pub fn end_span_error(ctx: SpanContext, error_msg: String) -> Span {
  let end_time = system_time_ns()
  let span = Span(
    trace_id: ctx.trace_id,
    span_id: ctx.span_id,
    parent_span_id: ctx.parent_span_id,
    name: ctx.name,
    kind: ctx.kind,
    start_time: ctx.start_time,
    end_time: end_time,
    status: Error(error_msg),
    attributes: ctx.attributes,
  )
  record_span(span)
  span
}

/// Add attribute to span context
pub fn add_attribute(ctx: SpanContext, key: String, value: AttributeValue) -> SpanContext {
  SpanContext(
    ..ctx,
    attributes: [#(key, value), ..ctx.attributes]
  )
}

/// Span context (in-progress span)
pub type SpanContext {
  SpanContext(
    trace_id: String,
    span_id: String,
    parent_span_id: Option(String),
    name: String,
    kind: SpanKind,
    start_time: Int,
    attributes: List(#(String, AttributeValue)),
  )
}

// ============================================================
// MCP-specific telemetry helpers
// ============================================================

/// Record a tool call span
pub fn record_tool_call(
  tool_name: String,
  duration_ms: Int,
  success: Bool,
  error_msg: Option(String),
) -> Nil {
  let span = start_span("mcp.tool_call", Server)
  let span = add_attribute(span, "mcp.tool_name", StringValue(tool_name))
  let span = add_attribute(span, "mcp.duration_ms", IntValue(duration_ms))
  let span = add_attribute(span, "mcp.success", BoolValue(success))

  case error_msg {
    Some(err) -> {
      let span = add_attribute(span, "mcp.error", StringValue(err))
      let _ = end_span_error(span, err)
      Nil
    }
    None -> {
      let _ = end_span_ok(span)
      Nil
    }
  }
}

/// Record a request span
pub fn record_request(
  method: String,
  duration_ms: Int,
  status_code: Int,
) -> Nil {
  let span = start_span("mcp.request", Server)
  let span = add_attribute(span, "mcp.method", StringValue(method))
  let span = add_attribute(span, "mcp.duration_ms", IntValue(duration_ms))
  let span = add_attribute(span, "mcp.status_code", IntValue(status_code))

  case status_code {
    code if code >= 0 -> {
      let _ = end_span_ok(span)
      Nil
    }
    _ -> {
      let _ = end_span_error(span, "Request failed")
      Nil
    }
  }
}

/// Increment a counter metric
pub fn increment_counter(name: String, value: Int, attributes: List(#(String, String))) -> Nil {
  record_metric(Counter(name, value, attributes))
}

/// Record a gauge metric
pub fn record_gauge(name: String, value: Float, attributes: List(#(String, String))) -> Nil {
  record_metric(Gauge(name, value, attributes))
}

/// Record a histogram observation
pub fn record_histogram(name: String, value: Float, attributes: List(#(String, String))) -> Nil {
  record_metric(Histogram(name, value, attributes))
}

// ============================================================
// MCP Metrics
// ============================================================

/// Increment tool call counter
pub fn metric_tool_call(tool_name: String, success: Bool) -> Nil {
  let status = case success {
    True -> "success"
    False -> "failure"
  }
  increment_counter("mcp.tool_calls_total", 1, [
    #("tool_name", tool_name),
    #("status", status),
  ])
}

/// Record tool call duration
pub fn metric_tool_duration(tool_name: String, duration_ms: Int) -> Nil {
  record_histogram("mcp.tool_call_duration_ms", int_to_float(duration_ms), [
    #("tool_name", tool_name),
  ])
}

/// Record active connections gauge
pub fn metric_active_connections(count: Int) -> Nil {
  record_gauge("mcp.active_connections", int_to_float(count), [])
}

/// Record cache hit/miss
pub fn metric_cache_access(hit: Bool) -> Nil {
  let result = case hit {
    True -> "hit"
    False -> "miss"
  }
  increment_counter("mcp.cache_accesses_total", 1, [
    #("result", result),
  ])
}

/// Record rate limit hit
pub fn metric_rate_limit_hit(tool_name: String) -> Nil {
  increment_counter("mcp.rate_limit_hits_total", 1, [
    #("tool_name", tool_name),
  ])
}

/// Record circuit breaker state change
pub fn metric_circuit_breaker(service: String, state: String) -> Nil {
  increment_counter("mcp.circuit_breaker_state_changes_total", 1, [
    #("service", service),
    #("state", state),
  ])
}

// ============================================================
// Export functions
// ============================================================

/// Get all recorded spans (for export)
pub fn get_spans(limit: Int) -> List(Span) {
  get_recorded_spans(limit)
}

/// Get all recorded metrics (for export)
pub fn get_metrics() -> List(Metric) {
  get_recorded_metrics()
}

/// Export spans as JSON (OTLP-compatible format)
pub fn export_spans_json(spans: List(Span)) -> String {
  json.object([
    #("resourceSpans", json.array([
      json.object([
        #("scopeSpans", json.array([
          json.object([
            #("spans", json.array(spans, encode_span))
          ])
        ], fn(x) { x }))
      ])
    ], fn(x) { x }))
  ]) |> json.to_string()
}

/// Export metrics as JSON (OTLP-compatible format)
pub fn export_metrics_json(metrics: List(Metric)) -> String {
  json.object([
    #("resourceMetrics", json.array([
      json.object([
        #("scopeMetrics", json.array([
          json.object([
            #("metrics", json.array(metrics, encode_metric))
          ])
        ], fn(x) { x }))
      ])
    ], fn(x) { x }))
  ]) |> json.to_string()
}

/// Clear all recorded telemetry data
pub fn clear() -> Nil {
  clear_telemetry()
}

// ============================================================
// Encoding helpers
// ============================================================

fn encode_span(span: Span) -> json.Json {
  json.object([
    #("traceId", json.string(span.trace_id)),
    #("spanId", json.string(span.span_id)),
    #("parentSpanId", case span.parent_span_id {
      Some(id) -> json.string(id)
      None -> json.null()
    }),
    #("name", json.string(span.name)),
    #("kind", json.int(span_kind_to_int(span.kind))),
    #("startTimeUnixNano", json.int(span.start_time)),
    #("endTimeUnixNano", json.int(span.end_time)),
    #("status", encode_status(span.status)),
    #("attributes", json.array(span.attributes, encode_attribute)),
  ])
}

fn encode_status(status: SpanStatus) -> json.Json {
  case status {
    Unset -> json.object([#("code", json.int(0))])
    Ok -> json.object([#("code", json.int(1))])
    Error(msg) -> json.object([
      #("code", json.int(2)),
      #("message", json.string(msg)),
    ])
  }
}

fn encode_attribute(attr: #(String, AttributeValue)) -> json.Json {
  let #(key, value) = attr
  json.object([
    #("key", json.string(key)),
    #("value", encode_attribute_value(value)),
  ])
}

fn encode_attribute_value(value: AttributeValue) -> json.Json {
  case value {
    StringValue(s) -> json.object([#("stringValue", json.string(s))])
    IntValue(i) -> json.object([#("intValue", json.int(i))])
    FloatValue(f) -> json.object([#("doubleValue", json.float(f))])
    BoolValue(b) -> json.object([#("boolValue", json.bool(b))])
    StringArrayValue(arr) -> json.object([
      #("arrayValue", json.object([
        #("values", json.array(arr, fn(s) { json.object([#("stringValue", json.string(s))]) }))
      ]))
    ])
  }
}

fn encode_metric(metric: Metric) -> json.Json {
  case metric {
    Counter(name, value, attrs) -> json.object([
      #("name", json.string(name)),
      #("sum", json.object([
        #("dataPoints", json.array([
          json.object([
            #("asInt", json.int(value)),
            #("attributes", encode_metric_attributes(attrs)),
          ])
        ], fn(x) { x })),
        #("isMonotonic", json.bool(True)),
      ])),
    ])
    Gauge(name, value, attrs) -> json.object([
      #("name", json.string(name)),
      #("gauge", json.object([
        #("dataPoints", json.array([
          json.object([
            #("asDouble", json.float(value)),
            #("attributes", encode_metric_attributes(attrs)),
          ])
        ], fn(x) { x })),
      ])),
    ])
    Histogram(name, value, attrs) -> json.object([
      #("name", json.string(name)),
      #("histogram", json.object([
        #("dataPoints", json.array([
          json.object([
            #("sum", json.float(value)),
            #("count", json.int(1)),
            #("attributes", encode_metric_attributes(attrs)),
          ])
        ], fn(x) { x })),
      ])),
    ])
  }
}

fn encode_metric_attributes(attrs: List(#(String, String))) -> json.Json {
  json.array(attrs, fn(attr) {
    let #(key, value) = attr
    json.object([
      #("key", json.string(key)),
      #("value", json.object([#("stringValue", json.string(value))])),
    ])
  })
}

fn span_kind_to_int(kind: SpanKind) -> Int {
  case kind {
    Internal -> 1
    Server -> 2
    Client -> 3
    Producer -> 4
    Consumer -> 5
  }
}

fn int_to_float(n: Int) -> Float {
  case n {
    0 -> 0.0
    _ -> int_to_float_ffi(n)
  }
}

// ============================================================
// FFI declarations
// ============================================================

@external(erlang, "vibee_telemetry_ffi", "generate_trace_id")
fn generate_trace_id() -> String

@external(erlang, "vibee_telemetry_ffi", "generate_span_id")
fn generate_span_id() -> String

@external(erlang, "vibee_telemetry_ffi", "system_time_ns")
fn system_time_ns() -> Int

@external(erlang, "vibee_telemetry_ffi", "record_span")
fn record_span(span: Span) -> Nil

@external(erlang, "vibee_telemetry_ffi", "record_metric")
fn record_metric(metric: Metric) -> Nil

@external(erlang, "vibee_telemetry_ffi", "get_recorded_spans")
fn get_recorded_spans(limit: Int) -> List(Span)

@external(erlang, "vibee_telemetry_ffi", "get_recorded_metrics")
fn get_recorded_metrics() -> List(Metric)

@external(erlang, "vibee_telemetry_ffi", "clear_telemetry")
fn clear_telemetry() -> Nil

@external(erlang, "vibee_telemetry_ffi", "int_to_float")
fn int_to_float_ffi(n: Int) -> Float
