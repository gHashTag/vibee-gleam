-module(vibee_rate_limit_ffi).
-export([
    init/0,
    get_request_count/1,
    increment_request_count/1,
    get_window_reset_seconds/1,
    get_circuit_state/1,
    get_circuit_retry_seconds/1,
    increment_failure_count/1,
    open_circuit/1,
    close_circuit/1
]).

%% Window size in seconds (1 minute)
-define(WINDOW_SIZE, 60).
%% Circuit breaker timeout in seconds
-define(CIRCUIT_TIMEOUT, 30).

%% Initialize ETS tables (call once at startup)
init() ->
    %% Rate limit table: {tool_name, count, window_start_timestamp}
    case ets:whereis(vibee_rate_limits) of
        undefined ->
            ets:new(vibee_rate_limits, [named_table, public, set]);
        _ -> ok
    end,
    %% Circuit breaker table: {service_name, state, failure_count, opened_at}
    case ets:whereis(vibee_circuits) of
        undefined ->
            ets:new(vibee_circuits, [named_table, public, set]);
        _ -> ok
    end,
    ok.

%% Get current request count for a tool (within current window)
get_request_count(ToolName) ->
    ensure_tables(),
    Now = erlang:system_time(second),
    case ets:lookup(vibee_rate_limits, ToolName) of
        [{ToolName, Count, WindowStart}] ->
            case Now - WindowStart < ?WINDOW_SIZE of
                true -> Count;
                false ->
                    %% Window expired, reset
                    ets:insert(vibee_rate_limits, {ToolName, 0, Now}),
                    0
            end;
        [] ->
            %% First request for this tool
            ets:insert(vibee_rate_limits, {ToolName, 0, Now}),
            0
    end.

%% Increment request count and return new count
increment_request_count(ToolName) ->
    ensure_tables(),
    Now = erlang:system_time(second),
    case ets:lookup(vibee_rate_limits, ToolName) of
        [{ToolName, Count, WindowStart}] ->
            case Now - WindowStart < ?WINDOW_SIZE of
                true ->
                    NewCount = Count + 1,
                    ets:insert(vibee_rate_limits, {ToolName, NewCount, WindowStart}),
                    NewCount;
                false ->
                    %% Window expired, reset to 1
                    ets:insert(vibee_rate_limits, {ToolName, 1, Now}),
                    1
            end;
        [] ->
            ets:insert(vibee_rate_limits, {ToolName, 1, Now}),
            1
    end.

%% Get seconds until window resets
get_window_reset_seconds(ToolName) ->
    ensure_tables(),
    Now = erlang:system_time(second),
    case ets:lookup(vibee_rate_limits, ToolName) of
        [{ToolName, _Count, WindowStart}] ->
            Elapsed = Now - WindowStart,
            case Elapsed < ?WINDOW_SIZE of
                true -> ?WINDOW_SIZE - Elapsed;
                false -> 0
            end;
        [] -> 0
    end.

%% Get circuit breaker state for a service
%% Returns: closed | open | half_open (as atoms for Gleam)
get_circuit_state(ServiceName) ->
    ensure_tables(),
    Now = erlang:system_time(second),
    case ets:lookup(vibee_circuits, ServiceName) of
        [{ServiceName, open, _FailCount, OpenedAt}] ->
            case Now - OpenedAt > ?CIRCUIT_TIMEOUT of
                true ->
                    %% Transition to half-open
                    ets:insert(vibee_circuits, {ServiceName, half_open, 0, OpenedAt}),
                    half_open;
                false ->
                    open
            end;
        [{ServiceName, State, _FailCount, _OpenedAt}] ->
            State;
        [] ->
            closed
    end.

%% Get seconds until circuit breaker retry
get_circuit_retry_seconds(ServiceName) ->
    ensure_tables(),
    Now = erlang:system_time(second),
    case ets:lookup(vibee_circuits, ServiceName) of
        [{ServiceName, open, _FailCount, OpenedAt}] ->
            Elapsed = Now - OpenedAt,
            case Elapsed < ?CIRCUIT_TIMEOUT of
                true -> ?CIRCUIT_TIMEOUT - Elapsed;
                false -> 0
            end;
        _ -> 0
    end.

%% Increment failure count and return new count
increment_failure_count(ServiceName) ->
    ensure_tables(),
    Now = erlang:system_time(second),
    case ets:lookup(vibee_circuits, ServiceName) of
        [{ServiceName, State, FailCount, OpenedAt}] ->
            NewCount = FailCount + 1,
            ets:insert(vibee_circuits, {ServiceName, State, NewCount, OpenedAt}),
            NewCount;
        [] ->
            ets:insert(vibee_circuits, {ServiceName, closed, 1, Now}),
            1
    end.

%% Open circuit breaker
open_circuit(ServiceName) ->
    ensure_tables(),
    Now = erlang:system_time(second),
    ets:insert(vibee_circuits, {ServiceName, open, 0, Now}),
    nil.

%% Close circuit breaker (reset to normal)
close_circuit(ServiceName) ->
    ensure_tables(),
    Now = erlang:system_time(second),
    ets:insert(vibee_circuits, {ServiceName, closed, 0, Now}),
    nil.

%% Ensure tables exist
ensure_tables() ->
    case ets:whereis(vibee_rate_limits) of
        undefined -> init();
        _ -> ok
    end.
