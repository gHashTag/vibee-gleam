-module(vibee_timestamp_ffi).
-export([get_timestamp_seconds/0, get_timestamp_milliseconds/0]).

%% Get current timestamp in seconds
get_timestamp_seconds() ->
    erlang:system_time(second).

%% Get current timestamp in milliseconds
get_timestamp_milliseconds() ->
    erlang:system_time(millisecond).
