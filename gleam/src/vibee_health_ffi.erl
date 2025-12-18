-module(vibee_health_ffi).
-export([get_start_time/0]).

-define(START_TIME_KEY, vibee_start_time).

get_start_time() ->
    case persistent_term:get(?START_TIME_KEY, undefined) of
        undefined ->
            Now = erlang:system_time(second),
            persistent_term:put(?START_TIME_KEY, Now),
            Now;
        StartTime ->
            StartTime
    end.
