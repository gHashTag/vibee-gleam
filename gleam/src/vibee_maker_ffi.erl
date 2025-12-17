%% VIBEE Maker Bot FFI
%% Wrapper for P2P order operations used by maker bot

-module(vibee_maker_ffi).
-export([create_order/1, list_orders_by_seller/1, cancel_order/1, get_map_string/2]).

%% Create order from Gleam JSON
create_order(JsonOrder) ->
    %% Convert Gleam json.Json to Erlang map
    OrderMap = vibee_p2p_ffi:to_dynamic(JsonOrder),
    case vibee_p2p_ffi:create_order(OrderMap) of
        {ok, Id} -> {ok, Id};
        {error, Reason} -> {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% List orders by seller (returns list of maps)
list_orders_by_seller(SellerId) ->
    vibee_p2p_ffi:list_orders_by_seller(SellerId).

%% Cancel an order
cancel_order(OrderId) ->
    case vibee_p2p_ffi:get_order(OrderId) of
        {ok, OrderMap} ->
            UpdatedOrder = OrderMap#{<<"status">> => <<"cancelled">>},
            vibee_p2p_ffi:update_order(UpdatedOrder),
            {ok, nil};
        {error, _} ->
            {error, <<"Order not found">>}
    end.

%% Get string from map
get_map_string(Map, Key) when is_map(Map), is_binary(Key) ->
    maps:get(Key, Map, <<>>);
get_map_string(_, _) ->
    <<>>.
