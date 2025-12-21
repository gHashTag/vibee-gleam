-module(vibee_dynamic_config_ffi).
-export([
    cache_init/0,
    cache_get/1,
    cache_set/3,
    cache_delete/1,
    cache_clear/0
]).

%% ETS table for dynamic config cache
-define(TABLE, vibee_dynamic_config_cache).

%% Initialize cache table
cache_init() ->
    case ets:whereis(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set]);
        _ -> ok
    end,
    nil.

%% Get cached value with TTL check
%% Returns: {ok, Value} | {error, nil}
cache_get(Key) ->
    ensure_init(),
    case ets:lookup(?TABLE, Key) of
        [{Key, Value, CreatedAt, TTL}] ->
            Now = erlang:system_time(second),
            case Now - CreatedAt < TTL of
                true ->
                    {ok, Value};
                false ->
                    %% Expired, delete entry
                    ets:delete(?TABLE, Key),
                    {error, nil}
            end;
        [] ->
            {error, nil}
    end.

%% Set cached value with TTL (in seconds)
cache_set(Key, Value, TTL) ->
    ensure_init(),
    Now = erlang:system_time(second),
    ets:insert(?TABLE, {Key, Value, Now, TTL}),
    nil.

%% Delete single entry
cache_delete(Key) ->
    ensure_init(),
    ets:delete(?TABLE, Key),
    nil.

%% Clear entire cache
cache_clear() ->
    ensure_init(),
    ets:delete_all_objects(?TABLE),
    nil.

%% Internal: ensure table is initialized
ensure_init() ->
    case ets:whereis(?TABLE) of
        undefined -> cache_init();
        _ -> ok
    end.
