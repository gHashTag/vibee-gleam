// Vibe Trace - Span-based distributed tracing
// Compatible with OpenTelemetry concepts

import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/vibe_logger.{type VibeLogger}

// =============================================================================
// Types
// =============================================================================

/// Span status
pub type SpanStatus {
  StatusOk
  StatusError(message: String)
  StatusUnset
}

/// Span kind (like OpenTelemetry)
pub type SpanKind {
  KindInternal
  KindServer
  KindClient
  KindProducer
  KindConsumer
}

/// A span represents a single operation within a trace
pub type Span {
  Span(
    trace_id: String,
    span_id: String,
    parent_id: Option(String),
    name: String,
    kind: SpanKind,
    start_time: Int,
    end_time: Option(Int),
    status: SpanStatus,
    attributes: List(#(String, String)),
  )
}

/// Trace context for propagation
pub type TraceContext {
  TraceContext(trace_id: String, span_id: String, sampled: Bool)
}

// =============================================================================
// Span Operations
// =============================================================================

/// Start a new root span (no parent)
pub fn start_span(name: String) -> Span {
  Span(
    trace_id: vibe_logger.generate_trace_id(),
    span_id: vibe_logger.generate_span_id(),
    parent_id: None,
    name: name,
    kind: KindInternal,
    start_time: get_timestamp_ms(),
    end_time: None,
    status: StatusUnset,
    attributes: [],
  )
}

/// Start a child span
pub fn start_child_span(name: String, parent: Span) -> Span {
  Span(
    trace_id: parent.trace_id,
    span_id: vibe_logger.generate_span_id(),
    parent_id: Some(parent.span_id),
    name: name,
    kind: KindInternal,
    start_time: get_timestamp_ms(),
    end_time: None,
    status: StatusUnset,
    attributes: [],
  )
}

/// Start a span from trace context (for distributed tracing)
pub fn start_span_from_context(name: String, ctx: TraceContext) -> Span {
  Span(
    trace_id: ctx.trace_id,
    span_id: vibe_logger.generate_span_id(),
    parent_id: Some(ctx.span_id),
    name: name,
    kind: KindServer,
    start_time: get_timestamp_ms(),
    end_time: None,
    status: StatusUnset,
    attributes: [],
  )
}

/// Set span kind
pub fn with_kind(span: Span, kind: SpanKind) -> Span {
  Span(..span, kind: kind)
}

/// Add attribute to span
pub fn with_attribute(span: Span, key: String, value: String) -> Span {
  Span(..span, attributes: [#(key, value), ..span.attributes])
}

/// Add multiple attributes
pub fn with_attributes(span: Span, attrs: List(#(String, String))) -> Span {
  Span(..span, attributes: list.flatten([attrs, span.attributes]))
}

/// Set span status to OK
pub fn set_ok(span: Span) -> Span {
  Span(..span, status: StatusOk)
}

/// Set span status to Error
pub fn set_error(span: Span, message: String) -> Span {
  Span(..span, status: StatusError(message))
}

/// End the span and record duration
pub fn end_span(span: Span) -> Span {
  Span(..span, end_time: Some(get_timestamp_ms()))
}

/// End span with OK status
pub fn end_span_ok(span: Span) -> Span {
  span
  |> set_ok
  |> end_span
}

/// End span with Error status
pub fn end_span_error(span: Span, message: String) -> Span {
  span
  |> set_error(message)
  |> end_span
}

// =============================================================================
// Logging Integration
// =============================================================================

/// Log span start
pub fn log_span_start(span: Span, logger: VibeLogger) -> Nil {
  let logger_with_span =
    logger
    |> vibe_logger.with_trace_id(span.trace_id)
    |> vibe_logger.with_span_id(span.span_id)
    |> vibe_logger.with_data("span.name", json.string(span.name))
    |> vibe_logger.with_data("span.kind", json.string(kind_to_string(span.kind)))

  logger_with_span
  |> vibe_logger.debug("Span started: " <> span.name)
}

/// Log span end with duration
pub fn log_span_end(span: Span, logger: VibeLogger) -> Nil {
  let duration = case span.end_time {
    Some(end) -> end - span.start_time
    None -> 0
  }

  let logger_with_span =
    logger
    |> vibe_logger.with_trace_id(span.trace_id)
    |> vibe_logger.with_span_id(span.span_id)
    |> vibe_logger.with_data("span.name", json.string(span.name))
    |> vibe_logger.with_data("span.duration_ms", json.int(duration))
    |> vibe_logger.with_data("span.status", json.string(status_to_string(span.status)))

  let level_fn = case span.status {
    StatusError(_) -> vibe_logger.error
    _ -> vibe_logger.info
  }

  level_fn(
    logger_with_span,
    "Span completed: " <> span.name <> " (" <> int.to_string(duration) <> "ms)",
  )
}

/// Create logger from span context
pub fn logger_from_span(span: Span, base_logger: VibeLogger) -> VibeLogger {
  base_logger
  |> vibe_logger.with_trace_id(span.trace_id)
  |> vibe_logger.with_span_id(span.span_id)
}

// =============================================================================
// Context Propagation
// =============================================================================

/// Extract trace context from span
pub fn to_context(span: Span) -> TraceContext {
  TraceContext(trace_id: span.trace_id, span_id: span.span_id, sampled: True)
}

/// Serialize context to W3C traceparent format
pub fn to_traceparent(ctx: TraceContext) -> String {
  let sampled_flag = case ctx.sampled {
    True -> "01"
    False -> "00"
  }
  "00-" <> ctx.trace_id <> "-" <> ctx.span_id <> "-" <> sampled_flag
}

/// Parse W3C traceparent format
pub fn from_traceparent(header: String) -> Result(TraceContext, Nil) {
  case string.split(header, "-") {
    ["00", trace_id, span_id, flags] -> {
      let sampled = flags == "01"
      Ok(TraceContext(trace_id: trace_id, span_id: span_id, sampled: sampled))
    }
    _ -> Error(Nil)
  }
}

// =============================================================================
// Serialization
// =============================================================================

/// Convert span to JSON for export
pub fn span_to_json(span: Span) -> json.Json {
  let duration = case span.end_time {
    Some(end) -> end - span.start_time
    None -> 0
  }

  let base = [
    #("trace_id", json.string(span.trace_id)),
    #("span_id", json.string(span.span_id)),
    #("name", json.string(span.name)),
    #("kind", json.string(kind_to_string(span.kind))),
    #("start_time", json.int(span.start_time)),
    #("duration_ms", json.int(duration)),
    #("status", json.string(status_to_string(span.status))),
    #(
      "attributes",
      json.object(
        list.map(span.attributes, fn(a) { #(a.0, json.string(a.1)) }),
      ),
    ),
  ]

  let with_parent = case span.parent_id {
    Some(pid) -> [#("parent_id", json.string(pid)), ..base]
    None -> base
  }

  let with_end = case span.end_time {
    Some(end) -> [#("end_time", json.int(end)), ..with_parent]
    None -> with_parent
  }

  json.object(with_end)
}

// =============================================================================
// Helpers
// =============================================================================

fn kind_to_string(kind: SpanKind) -> String {
  case kind {
    KindInternal -> "internal"
    KindServer -> "server"
    KindClient -> "client"
    KindProducer -> "producer"
    KindConsumer -> "consumer"
  }
}

fn status_to_string(status: SpanStatus) -> String {
  case status {
    StatusOk -> "ok"
    StatusError(msg) -> "error: " <> msg
    StatusUnset -> "unset"
  }
}

@external(erlang, "vibee_vibe_trace_ffi", "get_timestamp_ms")
fn get_timestamp_ms() -> Int

// =============================================================================
// High-Level API
// =============================================================================

/// Execute function with automatic span tracking
pub fn with_span(
  name: String,
  logger: VibeLogger,
  f: fn(Span, VibeLogger) -> a,
) -> a {
  let span = start_span(name)
  log_span_start(span, logger)

  let span_logger = logger_from_span(span, logger)
  let result = f(span, span_logger)

  let final_span = end_span_ok(span)
  log_span_end(final_span, logger)

  result
}

/// Execute function with automatic span tracking (child span)
pub fn with_child_span(
  name: String,
  parent: Span,
  logger: VibeLogger,
  f: fn(Span, VibeLogger) -> a,
) -> a {
  let span = start_child_span(name, parent)
  log_span_start(span, logger)

  let span_logger = logger_from_span(span, logger)
  let result = f(span, span_logger)

  let final_span = end_span_ok(span)
  log_span_end(final_span, logger)

  result
}
