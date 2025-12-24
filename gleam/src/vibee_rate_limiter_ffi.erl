-module(vibee_rate_limiter_ffi).
-export([init/0, get_bucket/1, set_bucket/2, get_time_ms/0]).

-define(TABLE, vibee_rate_limiter_buckets).

%% Initialize ETS table
init() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}]);
        _ ->
            ok
    end,
    nil.

%% Get bucket for IP
get_bucket(Ip) ->
    init(),
    case ets:lookup(?TABLE, Ip) of
        [{Ip, Tokens, LastRefill}] ->
            {some, {token_bucket, Tokens, LastRefill}};
        [] ->
            none
    end.

%% Set bucket for IP
set_bucket(Ip, {token_bucket, Tokens, LastRefill}) ->
    init(),
    ets:insert(?TABLE, {Ip, Tokens, LastRefill}),
    nil.

%% Get current time in milliseconds
get_time_ms() ->
    erlang:system_time(millisecond).
