-module(vibee_vibe_trace_ffi).
-export([get_timestamp_ms/0]).

%% Get current timestamp in milliseconds (for span timing)
get_timestamp_ms() ->
    erlang:system_time(millisecond).
