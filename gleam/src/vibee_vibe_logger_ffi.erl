-module(vibee_vibe_logger_ffi).
-export([print_stderr/1, get_iso_timestamp/0, generate_trace_id/0, generate_span_id/0]).

%% Print message to stderr (MCP-safe!)
print_stderr(Message) when is_binary(Message) ->
    io:format(standard_error, "~s~n", [Message]),
    nil.

%% Get current timestamp in ISO 8601 format
get_iso_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    MicroSec = erlang:system_time(microsecond) rem 1000000,
    MilliSec = MicroSec div 1000,
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
        [Year, Month, Day, Hour, Min, Sec, MilliSec])).

%% Generate unique trace ID (16 hex chars)
generate_trace_id() ->
    Bytes = crypto:strong_rand_bytes(8),
    list_to_binary(binary_to_hex(Bytes)).

%% Generate unique span ID (8 hex chars)
generate_span_id() ->
    Bytes = crypto:strong_rand_bytes(4),
    list_to_binary(binary_to_hex(Bytes)).

%% Helper: Convert binary to hex string
binary_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Bin]).
