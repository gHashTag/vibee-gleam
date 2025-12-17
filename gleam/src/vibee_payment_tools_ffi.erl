%% Payment Tools FFI helpers
%% Helper functions for payment tools

-module(vibee_payment_tools_ffi).

-export([
    create_payment_map/8,
    get_map_field/2,
    get_float_value/1,
    get_string_value/1,
    get_int_value/1,
    get_bool_value/1
]).

%% Create payment map for storage
create_payment_map(InvId, TelegramId, Amount, Stars, Currency, Method, InvoiceUrl, Description) ->
    #{
        <<"inv_id">> => InvId,
        <<"telegram_id">> => TelegramId,
        <<"amount">> => Amount,
        <<"stars">> => Stars,
        <<"currency">> => Currency,
        <<"method">> => Method,
        <<"invoice_url">> => case InvoiceUrl of
            {some, Url} -> Url;
            none -> undefined
        end,
        <<"description">> => case Description of
            {some, Desc} -> Desc;
            none -> undefined
        end
    }.

%% Get field from map
%% Supports both binary keys (from json:decode) and list keys (from Gleam strings)
get_map_field(Map, Key) when is_map(Map) ->
    BinKey = if
        is_binary(Key) -> Key;
        is_list(Key) -> list_to_binary(Key);
        true -> Key
    end,
    case maps:find(BinKey, Map) of
        {ok, Value} -> {ok, Value};
        error -> {error, not_found}
    end;
get_map_field(_NotMap, _Key) ->
    {error, not_a_map}.

%% Get string value from dynamic
get_string_value(Value) when is_binary(Value) ->
    {ok, Value};
get_string_value(Value) when is_list(Value) ->
    {ok, list_to_binary(Value)};
get_string_value(_) ->
    {error, nil}.

%% Get int value from dynamic
get_int_value(Value) when is_integer(Value) ->
    {ok, Value};
get_int_value(_) ->
    {error, nil}.

%% Get float value from dynamic, converts int to float if needed
get_float_value(Value) when is_float(Value) ->
    {ok, Value};
get_float_value(Value) when is_integer(Value) ->
    {ok, float(Value)};
get_float_value(_) ->
    {error, nil}.

%% Get bool value from dynamic
get_bool_value(Value) when is_boolean(Value) ->
    {ok, Value};
get_bool_value(_) ->
    {error, nil}.
