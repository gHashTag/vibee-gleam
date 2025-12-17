-module(vibee_db_pool_ffi).

%% Database Pool Cache FFI
%% Caches pog connection pool in ETS for singleton access

-export([init/0, get_cached/0, cache_pool/1, clear_cache/0, set_global_pool/1, get_global_pool/0]).

-define(TABLE, vibee_db_pool_cache).

%% Initialize ETS table
init() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set, {keypos, 1}]);
        _ ->
            ok
    end,
    nil.

%% Get cached pool connection
%% Returns: {some, Connection} | none
get_cached() ->
    init(),
    case ets:lookup(?TABLE, pool) of
        [{pool, Connection}] -> {some, Connection};
        [] -> none
    end.

%% Cache pool connection
cache_pool(Connection) ->
    init(),
    ets:insert(?TABLE, {pool, Connection}),
    nil.

%% Clear cached pool (for reconnection on errors)
clear_cache() ->
    init(),
    ets:delete(?TABLE, pool),
    nil.

%% Alias: Set global pool (same as cache_pool)
set_global_pool(Connection) ->
    cache_pool(Connection).

%% Alias: Get global pool (same as get_cached)
get_global_pool() ->
    get_cached().
