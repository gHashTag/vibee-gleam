-module(vibee_cache_ffi).
-export([
    init/0,
    get/1,
    set/3,
    delete/1,
    delete_prefix/1,
    clear/0,
    stats/0,
    hash_string/1
]).

%% Cache table name
-define(CACHE_TABLE, vibee_cache).
%% Stats table for hit/miss tracking
-define(STATS_TABLE, vibee_cache_stats).

%% Initialize cache tables
init() ->
    %% Main cache: {Key, Value, CreatedAt, TTL}
    case ets:whereis(?CACHE_TABLE) of
        undefined ->
            ets:new(?CACHE_TABLE, [named_table, public, set]);
        _ -> ok
    end,
    %% Stats: {hits, Hits}, {misses, Misses}
    case ets:whereis(?STATS_TABLE) of
        undefined ->
            Tab = ets:new(?STATS_TABLE, [named_table, public, set]),
            ets:insert(Tab, {hits, 0}),
            ets:insert(Tab, {misses, 0}),
            Tab;
        _ -> ok
    end,
    nil.

%% Get cached value
%% Returns: {cache_found, Value} | cache_not_found | cache_expired
get(Key) ->
    ensure_init(),
    case ets:lookup(?CACHE_TABLE, Key) of
        [{Key, Value, CreatedAt, TTL}] ->
            Now = erlang:system_time(second),
            case Now - CreatedAt < TTL of
                true ->
                    inc_stat(hits),
                    {cache_found, Value};
                false ->
                    %% Expired, delete entry
                    ets:delete(?CACHE_TABLE, Key),
                    inc_stat(misses),
                    cache_expired
            end;
        [] ->
            inc_stat(misses),
            cache_not_found
    end.

%% Set cached value with TTL
set(Key, Value, TTL) ->
    ensure_init(),
    Now = erlang:system_time(second),
    ets:insert(?CACHE_TABLE, {Key, Value, Now, TTL}),
    nil.

%% Delete single entry
delete(Key) ->
    ensure_init(),
    ets:delete(?CACHE_TABLE, Key),
    nil.

%% Delete entries by prefix
delete_prefix(Prefix) ->
    ensure_init(),
    %% Find and delete all keys starting with prefix
    PrefixLen = byte_size(Prefix),
    ets:foldl(
        fun({Key, _, _, _}, Acc) ->
            case binary:longest_common_prefix([Key, Prefix]) of
                PrefixLen ->
                    ets:delete(?CACHE_TABLE, Key);
                _ -> ok
            end,
            Acc
        end,
        ok,
        ?CACHE_TABLE
    ),
    nil.

%% Clear entire cache
clear() ->
    ensure_init(),
    ets:delete_all_objects(?CACHE_TABLE),
    ets:insert(?STATS_TABLE, {hits, 0}),
    ets:insert(?STATS_TABLE, {misses, 0}),
    nil.

%% Get cache statistics
%% Returns: {cache_stats, Size, Hits, Misses, HitRate}
stats() ->
    ensure_init(),
    Size = ets:info(?CACHE_TABLE, size),
    [{hits, Hits}] = ets:lookup(?STATS_TABLE, hits),
    [{misses, Misses}] = ets:lookup(?STATS_TABLE, misses),
    Total = Hits + Misses,
    HitRate = case Total of
        0 -> 0.0;
        _ -> Hits / Total
    end,
    {cache_stats, Size, Hits, Misses, HitRate}.

%% SHA256 hash of string
hash_string(Binary) when is_binary(Binary) ->
    Hash = crypto:hash(sha256, Binary),
    bin_to_hex(Hash).

%% Internal: convert binary to hex string
bin_to_hex(Bin) ->
    << <<(hex_char(N div 16)), (hex_char(N rem 16))>> || <<N>> <= Bin >>.

hex_char(N) when N < 10 -> $0 + N;
hex_char(N) -> $a + N - 10.

%% Internal: ensure tables are initialized
ensure_init() ->
    case ets:whereis(?CACHE_TABLE) of
        undefined -> init();
        _ -> ok
    end.

%% Internal: increment stat counter
inc_stat(Stat) ->
    case ets:whereis(?STATS_TABLE) of
        undefined -> ok;
        _ ->
            ets:update_counter(?STATS_TABLE, Stat, 1)
    end.
