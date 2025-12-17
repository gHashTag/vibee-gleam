-module(vibee_telemetry_ffi).

%% API
-export([
    init/0,
    generate_trace_id/0,
    generate_span_id/0,
    system_time_ns/0,
    record_span/1,
    record_metric/1,
    get_recorded_spans/1,
    get_recorded_metrics/0,
    clear_telemetry/0,
    int_to_float/1
]).

%% ETS table names
-define(SPANS_TABLE, vibee_telemetry_spans).
-define(METRICS_TABLE, vibee_telemetry_metrics).
-define(MAX_SPANS, 1000).
-define(MAX_METRICS, 10000).

%% Initialize ETS tables
init() ->
    case ets:info(?SPANS_TABLE) of
        undefined ->
            ets:new(?SPANS_TABLE, [
                named_table,
                ordered_set,
                public,
                {keypos, 1}
            ]);
        _ -> ok
    end,
    case ets:info(?METRICS_TABLE) of
        undefined ->
            ets:new(?METRICS_TABLE, [
                named_table,
                bag,
                public,
                {keypos, 1}
            ]);
        _ -> ok
    end,
    nil.

%% Generate a 32-character hex trace ID
generate_trace_id() ->
    Bytes = crypto:strong_rand_bytes(16),
    list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B:8>> <= Bytes])).

%% Generate a 16-character hex span ID
generate_span_id() ->
    Bytes = crypto:strong_rand_bytes(8),
    list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B:8>> <= Bytes])).

%% Get current time in nanoseconds
system_time_ns() ->
    erlang:system_time(nanosecond).

%% Record a span
record_span(Span) ->
    init(),
    %% Extract span fields
    {span, TraceId, SpanId, _ParentSpanId, Name, _Kind, StartTime, EndTime, _Status, _Attrs} = Span,

    %% Store with timestamp key for ordering
    Key = {EndTime, SpanId},
    ets:insert(?SPANS_TABLE, {Key, Span}),

    %% Log span info
    DurationMs = (EndTime - StartTime) div 1000000,
    io:format("[TELEMETRY] Span: ~s (~s) duration=~pms~n", [Name, TraceId, DurationMs]),

    %% Cleanup if too many spans
    case ets:info(?SPANS_TABLE, size) of
        Size when Size > ?MAX_SPANS ->
            cleanup_old_spans(Size div 10);
        _ -> ok
    end,
    nil.

%% Record a metric
record_metric(Metric) ->
    init(),
    %% Get metric name
    Name = case Metric of
        {counter, N, _, _} -> N;
        {gauge, N, _, _} -> N;
        {histogram, N, _, _} -> N
    end,

    Timestamp = erlang:system_time(millisecond),
    Key = {Name, Timestamp},
    ets:insert(?METRICS_TABLE, {Key, Metric}),

    %% Cleanup if too many metrics
    case ets:info(?METRICS_TABLE, size) of
        Size when Size > ?MAX_METRICS ->
            cleanup_old_metrics(Size div 10);
        _ -> ok
    end,
    nil.

%% Get recorded spans (most recent first)
get_recorded_spans(Limit) ->
    init(),
    Spans = ets:tab2list(?SPANS_TABLE),
    Sorted = lists:reverse(lists:sort(fun(A, B) ->
        {KeyA, _} = A,
        {KeyB, _} = B,
        KeyA >= KeyB
    end, Spans)),
    Limited = lists:sublist(Sorted, Limit),
    [Span || {_, Span} <- Limited].

%% Get recorded metrics
get_recorded_metrics() ->
    init(),
    Metrics = ets:tab2list(?METRICS_TABLE),
    [Metric || {_, Metric} <- Metrics].

%% Clear all telemetry data
clear_telemetry() ->
    init(),
    ets:delete_all_objects(?SPANS_TABLE),
    ets:delete_all_objects(?METRICS_TABLE),
    nil.

%% Convert integer to float
int_to_float(N) ->
    N * 1.0.

%% Internal helpers

cleanup_old_spans(Count) ->
    Spans = ets:tab2list(?SPANS_TABLE),
    Sorted = lists:sort(fun(A, B) ->
        {KeyA, _} = A,
        {KeyB, _} = B,
        KeyA =< KeyB
    end, Spans),
    ToDelete = lists:sublist(Sorted, Count),
    lists:foreach(fun({Key, _}) ->
        ets:delete(?SPANS_TABLE, Key)
    end, ToDelete),
    ok.

cleanup_old_metrics(Count) ->
    Metrics = ets:tab2list(?METRICS_TABLE),
    Sorted = lists:sort(fun(A, B) ->
        {KeyA, _} = A,
        {KeyB, _} = B,
        KeyA =< KeyB
    end, Metrics),
    ToDelete = lists:sublist(Sorted, Count),
    lists:foreach(fun({Key, _}) ->
        ets:delete(?METRICS_TABLE, Key)
    end, ToDelete),
    ok.
