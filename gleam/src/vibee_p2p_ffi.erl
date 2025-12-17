%% VIBEE P2P FFI - Erlang FFI for P2P Escrow System
%% ETS storage for orders, traders, and disputes

-module(vibee_p2p_ffi).
-export([
    init/0,
    generate_order_id/0,
    current_timestamp/0,
    current_timestamp_ms/0,
    % Dynamic conversion
    to_dynamic/1,
    decode_order_map/1,
    get_map_field_string/2,
    get_map_field_int/2,
    get_map_field_float/2,
    get_map_field_option_string/2,
    get_map_field_option_int/2,
    % Orders
    create_order/1,
    get_order/1,
    update_order/1,
    update_order_status/3,
    list_orders/1,
    list_orders_by_status/1,
    list_orders_by_seller/1,
    list_orders_by_buyer/1,
    delete_order/1,
    % Traders
    get_trader/1,
    update_trader/1,
    create_trader/1,
    % Disputes
    create_dispute/1,
    get_dispute/1,
    update_dispute/1,
    list_disputes_by_order/1,
    % Map getters for Gleam
    get_map_string/2,
    get_map_float/2,
    get_map_int/2,
    get_map_field_atom_as_string/2,
    % Helpers
    order_to_map/20,
    trader_to_map/12,
    % Environment
    get_env_var/1
]).

-define(ORDERS_TABLE, vibee_p2p_orders).
-define(TRADERS_TABLE, vibee_p2p_traders).
-define(DISPUTES_TABLE, vibee_p2p_disputes).
-define(COUNTER_TABLE, vibee_p2p_counters).

%% =============================================================================
%% INITIALIZATION
%% =============================================================================

init() ->
    %% Create ETS tables if they don't exist
    case ets:info(?ORDERS_TABLE) of
        undefined ->
            ets:new(?ORDERS_TABLE, [named_table, public, set, {keypos, 1}]);
        _ -> ok
    end,
    case ets:info(?TRADERS_TABLE) of
        undefined ->
            ets:new(?TRADERS_TABLE, [named_table, public, set, {keypos, 1}]);
        _ -> ok
    end,
    case ets:info(?DISPUTES_TABLE) of
        undefined ->
            ets:new(?DISPUTES_TABLE, [named_table, public, set, {keypos, 1}]);
        _ -> ok
    end,
    case ets:info(?COUNTER_TABLE) of
        undefined ->
            ets:new(?COUNTER_TABLE, [named_table, public, set]),
            ets:insert(?COUNTER_TABLE, {order_counter, 0}),
            ets:insert(?COUNTER_TABLE, {dispute_counter, 0});
        _ -> ok
    end,
    nil.

%% =============================================================================
%% DYNAMIC CONVERSION (for Gleam interop)
%% =============================================================================

%% Convert Gleam json.Json (iolist/binary) to Erlang map
%% json.Json is an iolist meant for serialization, not a structured map
%% So we serialize it to binary and decode back to a map
to_dynamic(Value) when is_binary(Value) ->
    %% Already a binary, decode it
    try
        json:decode(Value)
    catch
        _:_ -> #{}
    end;
to_dynamic(Value) when is_list(Value) ->
    %% It's an iolist from gleam_json, convert to binary and decode
    try
        Bin = iolist_to_binary(Value),
        json:decode(Bin)
    catch
        _:_ -> #{}
    end;
to_dynamic(Value) when is_map(Value) ->
    %% Already a map
    Value;
to_dynamic(_Value) ->
    #{}.

%% Decode order map back to Gleam-compatible tuple
%% Returns {ok, OrderTuple} or {error, reason}
decode_order_map(OrderMap) when is_map(OrderMap) ->
    try
        Id = maps:get(<<"id">>, OrderMap),
        SellerTelegramId = maps:get(<<"seller_telegram_id">>, OrderMap),
        SellerWallet = maps:get(<<"seller_wallet">>, OrderMap),
        BuyerTelegramId = get_optional(<<"buyer_telegram_id">>, OrderMap),
        BuyerWallet = get_optional(<<"buyer_wallet">>, OrderMap),
        Crypto = maps:get(<<"crypto">>, OrderMap),
        CryptoAmount = ensure_float(maps:get(<<"crypto_amount">>, OrderMap)),
        Fiat = maps:get(<<"fiat">>, OrderMap),
        FiatAmount = ensure_float(maps:get(<<"fiat_amount">>, OrderMap)),
        PaymentMethod = maps:get(<<"payment_method">>, OrderMap),
        PaymentDetails = maps:get(<<"payment_details">>, OrderMap),
        Status = maps:get(<<"status">>, OrderMap),
        EscrowTxHash = get_optional(<<"escrow_tx_hash">>, OrderMap),
        ReleaseTxHash = get_optional(<<"release_tx_hash">>, OrderMap),
        EscrowAddress = get_optional(<<"escrow_address">>, OrderMap),
        CreatedAt = maps:get(<<"created_at">>, OrderMap),
        LockedAt = get_optional(<<"locked_at">>, OrderMap),
        CompletedAt = get_optional(<<"completed_at">>, OrderMap),
        ExpiresInMinutes = maps:get(<<"expires_in_minutes">>, OrderMap, 1440),
        SellerRating = ensure_float(maps:get(<<"seller_rating">>, OrderMap, 5.0)),
        SellerTrades = maps:get(<<"seller_trades">>, OrderMap, 0),
        {ok, #{
            id => Id,
            seller_telegram_id => SellerTelegramId,
            seller_wallet => SellerWallet,
            buyer_telegram_id => BuyerTelegramId,
            buyer_wallet => BuyerWallet,
            crypto => Crypto,
            crypto_amount => CryptoAmount,
            fiat => Fiat,
            fiat_amount => FiatAmount,
            payment_method => PaymentMethod,
            payment_details => PaymentDetails,
            status => Status,
            escrow_tx_hash => EscrowTxHash,
            release_tx_hash => ReleaseTxHash,
            escrow_address => EscrowAddress,
            created_at => CreatedAt,
            locked_at => LockedAt,
            completed_at => CompletedAt,
            expires_in_minutes => ExpiresInMinutes,
            seller_rating => SellerRating,
            seller_trades => SellerTrades
        }}
    catch
        _:Reason -> {error, Reason}
    end;
decode_order_map(_) ->
    {error, not_a_map}.

get_optional(Key, Map) ->
    case maps:get(Key, Map, nil) of
        nil -> none;
        undefined -> none;
        null -> none;
        Value -> {some, Value}
    end.

ensure_float(Value) when is_float(Value) -> Value;
ensure_float(Value) when is_integer(Value) -> float(Value);
ensure_float(_) -> 0.0.

%% Map field getters for Gleam decoding
get_map_field_string(Map, Key) when is_map(Map) ->
    BinKey = to_atom_key(Key),
    case maps:get(BinKey, Map, <<"">>) of
        V when is_binary(V) -> V;
        V when is_list(V) -> list_to_binary(V);
        _ -> <<"">>
    end;
get_map_field_string(_, _) -> <<"">>.

get_map_field_int(Map, Key) when is_map(Map) ->
    BinKey = to_atom_key(Key),
    case maps:get(BinKey, Map, 0) of
        V when is_integer(V) -> V;
        _ -> 0
    end;
get_map_field_int(_, _) -> 0.

get_map_field_float(Map, Key) when is_map(Map) ->
    BinKey = to_atom_key(Key),
    case maps:get(BinKey, Map, 0.0) of
        V when is_float(V) -> V;
        V when is_integer(V) -> float(V);
        _ -> 0.0
    end;
get_map_field_float(_, _) -> 0.0.

get_map_field_atom_as_string(Map, Key) when is_map(Map) ->
    BinKey = to_atom_key(Key),
    case maps:get(BinKey, Map, unknown) of
        V when is_atom(V) -> atom_to_binary(V, utf8);
        V when is_binary(V) -> V;
        V when is_list(V) -> list_to_binary(V);
        _ -> <<"unknown">>
    end;
get_map_field_atom_as_string(_, _) -> <<"unknown">>.

get_map_field_option_string(Map, Key) when is_map(Map) ->
    BinKey = to_atom_key(Key),
    case maps:get(BinKey, Map, none) of
        none -> none;
        {some, V} when is_binary(V) -> {some, V};
        {some, V} when is_list(V) -> {some, list_to_binary(V)};
        V when is_binary(V) -> {some, V};
        V when is_list(V) -> {some, list_to_binary(V)};
        _ -> none
    end;
get_map_field_option_string(_, _) -> none.

get_map_field_option_int(Map, Key) when is_map(Map) ->
    BinKey = to_atom_key(Key),
    case maps:get(BinKey, Map, none) of
        none -> none;
        {some, V} when is_integer(V) -> {some, V};
        V when is_integer(V) -> {some, V};
        _ -> none
    end;
get_map_field_option_int(_, _) -> none.

to_atom_key(Key) when is_list(Key) -> list_to_atom(Key);
to_atom_key(Key) when is_binary(Key) -> binary_to_atom(Key);
to_atom_key(Key) when is_atom(Key) -> Key.

%% =============================================================================
%% MAP GETTERS FOR GLEAM
%% =============================================================================

%% Convert key to binary (Gleam strings are lists)
to_bin_key(Key) when is_binary(Key) -> Key;
to_bin_key(Key) when is_list(Key) -> list_to_binary(Key);
to_bin_key(Key) -> Key.

%% Get string value from Erlang map, key can be binary or list
get_map_string(Map, Key) when is_map(Map) ->
    BinKey = to_bin_key(Key),
    maps:get(BinKey, Map, <<>>);
get_map_string(_Map, _Key) ->
    <<>>.

%% Get float value from Erlang map
get_map_float(Map, Key) when is_map(Map) ->
    BinKey = to_bin_key(Key),
    case maps:get(BinKey, Map, 0.0) of
        V when is_float(V) -> V;
        V when is_integer(V) -> float(V);
        _ -> 0.0
    end;
get_map_float(_Map, _Key) ->
    0.0.

%% Get integer value from Erlang map
get_map_int(Map, Key) when is_map(Map) ->
    BinKey = to_bin_key(Key),
    case maps:get(BinKey, Map, 0) of
        V when is_integer(V) -> V;
        V when is_float(V) -> trunc(V);
        _ -> 0
    end;
get_map_int(_Map, _Key) ->
    0.

%% =============================================================================
%% HELPERS
%% =============================================================================

generate_order_id() ->
    init(),
    Counter = ets:update_counter(?COUNTER_TABLE, order_counter, 1),
    Timestamp = erlang:system_time(second),
    %% Format: P2P-TIMESTAMP-COUNTER
    iolist_to_binary([
        <<"P2P-">>,
        integer_to_binary(Timestamp rem 1000000),
        <<"-">>,
        integer_to_binary(Counter)
    ]).

current_timestamp() ->
    erlang:system_time(second).

current_timestamp_ms() ->
    erlang:system_time(millisecond).

%% =============================================================================
%% ORDERS CRUD
%% =============================================================================

%% Order is stored as a map with all fields
create_order(OrderMap) when is_map(OrderMap) ->
    init(),
    Id = maps:get(<<"id">>, OrderMap),
    true = ets:insert(?ORDERS_TABLE, {Id, OrderMap}),
    {ok, Id}.

get_order(OrderId) ->
    init(),
    case ets:lookup(?ORDERS_TABLE, OrderId) of
        [{_Id, OrderMap}] -> {ok, OrderMap};
        [] -> {error, not_found}
    end.

update_order(OrderMap) when is_map(OrderMap) ->
    init(),
    Id = maps:get(<<"id">>, OrderMap),
    true = ets:insert(?ORDERS_TABLE, {Id, OrderMap}),
    {ok, Id}.

%% Update order status and buyer (called when accepting an order)
update_order_status(OrderId, BuyerTelegramId, Status) ->
    init(),
    case ets:lookup(?ORDERS_TABLE, OrderId) of
        [{_Id, OrderMap}] ->
            Now = erlang:system_time(second),
            UpdatedOrder = OrderMap#{
                <<"buyer_telegram_id">> => BuyerTelegramId,
                <<"status">> => Status,
                <<"locked_at">> => Now,
                <<"updated_at">> => Now
            },
            true = ets:insert(?ORDERS_TABLE, {OrderId, UpdatedOrder}),
            io:format("[P2P FFI] Order ~s updated: buyer=~p, status=~s~n",
                      [OrderId, BuyerTelegramId, Status]),
            {ok, OrderId};
        [] ->
            {error, not_found}
    end.

delete_order(OrderId) ->
    init(),
    true = ets:delete(?ORDERS_TABLE, OrderId),
    ok.

list_orders(Limit) ->
    init(),
    All = ets:tab2list(?ORDERS_TABLE),
    Orders = [OrderMap || {_Id, OrderMap} <- All],
    %% Sort by created_at descending
    Sorted = lists:sort(
        fun(A, B) ->
            maps:get(<<"created_at">>, A, 0) >= maps:get(<<"created_at">>, B, 0)
        end,
        Orders
    ),
    lists:sublist(Sorted, Limit).

list_orders_by_status(Status) ->
    init(),
    All = ets:tab2list(?ORDERS_TABLE),
    [OrderMap || {_Id, OrderMap} <- All,
                 maps:get(<<"status">>, OrderMap) =:= Status].

list_orders_by_seller(SellerId) ->
    init(),
    All = ets:tab2list(?ORDERS_TABLE),
    [OrderMap || {_Id, OrderMap} <- All,
                 maps:get(<<"seller_telegram_id">>, OrderMap) =:= SellerId].

list_orders_by_buyer(BuyerId) ->
    init(),
    All = ets:tab2list(?ORDERS_TABLE),
    [OrderMap || {_Id, OrderMap} <- All,
                 maps:get(<<"buyer_telegram_id">>, OrderMap) =:= BuyerId].

%% =============================================================================
%% TRADERS CRUD
%% =============================================================================

create_trader(TraderMap) when is_map(TraderMap) ->
    init(),
    TelegramId = maps:get(<<"telegram_id">>, TraderMap),
    true = ets:insert(?TRADERS_TABLE, {TelegramId, TraderMap}),
    {ok, TelegramId}.

get_trader(TelegramId) ->
    init(),
    case ets:lookup(?TRADERS_TABLE, TelegramId) of
        [{_Id, TraderMap}] -> {ok, TraderMap};
        [] -> {error, not_found}
    end.

update_trader(TraderMap) when is_map(TraderMap) ->
    init(),
    TelegramId = maps:get(<<"telegram_id">>, TraderMap),
    true = ets:insert(?TRADERS_TABLE, {TelegramId, TraderMap}),
    {ok, TelegramId}.

%% =============================================================================
%% DISPUTES CRUD
%% =============================================================================

create_dispute(DisputeMap) when is_map(DisputeMap) ->
    init(),
    Counter = ets:update_counter(?COUNTER_TABLE, dispute_counter, 1),
    Id = iolist_to_binary([<<"DISPUTE-">>, integer_to_binary(Counter)]),
    DisputeWithId = DisputeMap#{<<"id">> => Id},
    true = ets:insert(?DISPUTES_TABLE, {Id, DisputeWithId}),
    {ok, Id}.

get_dispute(DisputeId) ->
    init(),
    case ets:lookup(?DISPUTES_TABLE, DisputeId) of
        [{_Id, DisputeMap}] -> {ok, DisputeMap};
        [] -> {error, not_found}
    end.

update_dispute(DisputeMap) when is_map(DisputeMap) ->
    init(),
    Id = maps:get(<<"id">>, DisputeMap),
    true = ets:insert(?DISPUTES_TABLE, {Id, DisputeMap}),
    {ok, Id}.

list_disputes_by_order(OrderId) ->
    init(),
    All = ets:tab2list(?DISPUTES_TABLE),
    [DisputeMap || {_Id, DisputeMap} <- All,
                   maps:get(<<"order_id">>, DisputeMap) =:= OrderId].

%% =============================================================================
%% MAP BUILDERS
%% =============================================================================

order_to_map(Id, SellerTelegramId, SellerWallet, BuyerTelegramId, BuyerWallet,
             Crypto, CryptoAmount, Fiat, FiatAmount, PaymentMethod, PaymentDetails,
             Status, EscrowTxHash, ReleaseTxHash, CreatedAt, LockedAt, CompletedAt,
             ExpiresInMinutes, SellerRating, SellerTrades) ->
    #{
        <<"id">> => Id,
        <<"seller_telegram_id">> => SellerTelegramId,
        <<"seller_wallet">> => SellerWallet,
        <<"buyer_telegram_id">> => BuyerTelegramId,
        <<"buyer_wallet">> => BuyerWallet,
        <<"crypto">> => Crypto,
        <<"crypto_amount">> => CryptoAmount,
        <<"fiat">> => Fiat,
        <<"fiat_amount">> => FiatAmount,
        <<"payment_method">> => PaymentMethod,
        <<"payment_details">> => PaymentDetails,
        <<"status">> => Status,
        <<"escrow_tx_hash">> => EscrowTxHash,
        <<"release_tx_hash">> => ReleaseTxHash,
        <<"created_at">> => CreatedAt,
        <<"locked_at">> => LockedAt,
        <<"completed_at">> => CompletedAt,
        <<"expires_in_minutes">> => ExpiresInMinutes,
        <<"seller_rating">> => SellerRating,
        <<"seller_trades">> => SellerTrades
    }.

trader_to_map(TelegramId, Username, WalletAddress, CompletedTrades, TotalVolumeUsd,
              Rating, RatingCount, DisputesAsSeller, DisputesAsBuyer,
              CreatedAt, LastActive, IsVerified) ->
    #{
        <<"telegram_id">> => TelegramId,
        <<"username">> => Username,
        <<"wallet_address">> => WalletAddress,
        <<"completed_trades">> => CompletedTrades,
        <<"total_volume_usd">> => TotalVolumeUsd,
        <<"rating">> => Rating,
        <<"rating_count">> => RatingCount,
        <<"disputes_as_seller">> => DisputesAsSeller,
        <<"disputes_as_buyer">> => DisputesAsBuyer,
        <<"created_at">> => CreatedAt,
        <<"last_active">> => LastActive,
        <<"is_verified">> => IsVerified
    }.

%% =============================================================================
%% ENVIRONMENT HELPERS
%% =============================================================================

%% Get environment variable, return empty string if not set
get_env_var(Key) when is_binary(Key) ->
    get_env_var(binary_to_list(Key));
get_env_var(Key) when is_list(Key) ->
    case os:getenv(Key) of
        false -> <<>>;
        Value -> list_to_binary(Value)
    end.
