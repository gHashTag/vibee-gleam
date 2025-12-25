-module(vibee_owner_notifier_ffi).
-export([get_timestamp/0, format_timestamp/1]).

%% Get current Unix timestamp in seconds
get_timestamp() ->
    erlang:system_time(second).

%% Format timestamp as HH:MM:SS
format_timestamp(Timestamp) ->
    DateTime = calendar:system_time_to_universal_time(Timestamp, second),
    {{_Y, _M, _D}, {H, Min, S}} = DateTime,
    % Добавляем 3 часа для MSK
    HMsk = (H + 3) rem 24,
    list_to_binary(io_lib:format("~2..0B:~2..0B:~2..0B", [HMsk, Min, S])).
