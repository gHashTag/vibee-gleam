-module(vibee_json_ffi).
-export([decode_map/1, get_string/2, get_string_list/2, raw/1]).

%% Pass through JSON string without escaping
%% Used when the string is already valid JSON and we don't want to double-encode
raw(JsonString) when is_binary(JsonString) -> JsonString.

%% Decode erlang map to list of {key, value} pairs
decode_map(Term) when is_map(Term) ->
    Pairs = maps:to_list(Term),
    {ok, lists:map(fun({K, V}) ->
        Key = if
            is_binary(K) -> K;
            is_atom(K) -> atom_to_binary(K, utf8);
            is_list(K) -> list_to_binary(K);
            true -> iolist_to_binary(io_lib:format("~p", [K]))
        end,
        {Key, V}
    end, Pairs)};
decode_map(_) ->
    {error, nil}.

%% Get string value from JSON by key
%% Handles gleam/json representation
get_string(Json, Key) when is_binary(Key) ->
    try
        %% gleam/json object is represented as a list of tuples
        case Json of
            List when is_list(List) ->
                find_string_in_list(List, Key);
            Map when is_map(Map) ->
                case maps:get(Key, Map, undefined) of
                    undefined -> <<>>;
                    Value when is_binary(Value) -> Value;
                    Value when is_list(Value) -> list_to_binary(Value);
                    _ -> <<>>
                end;
            _ -> <<>>
        end
    catch
        _:_ -> <<>>
    end;
get_string(_, _) ->
    <<>>.

%% Find string in gleam json list
find_string_in_list([], _Key) ->
    <<>>;
find_string_in_list([{K, V} | Rest], Key) when is_binary(K) ->
    case K of
        Key when is_binary(V) -> V;
        Key when is_list(V) -> list_to_binary(V);
        Key -> <<>>;
        _ -> find_string_in_list(Rest, Key)
    end;
find_string_in_list([_ | Rest], Key) ->
    find_string_in_list(Rest, Key).

%% Get list of strings from JSON array by key
get_string_list(Json, Key) when is_binary(Key) ->
    try
        case Json of
            List when is_list(List) ->
                find_list_in_json(List, Key);
            Map when is_map(Map) ->
                case maps:get(Key, Map, undefined) of
                    undefined -> [];
                    Value when is_list(Value) ->
                        lists:filtermap(fun
                            (V) when is_binary(V) -> {true, V};
                            (V) when is_list(V) -> {true, list_to_binary(V)};
                            (_) -> false
                        end, Value);
                    _ -> []
                end;
            _ -> []
        end
    catch
        _:_ -> []
    end;
get_string_list(_, _) ->
    [].

%% Find list in gleam json
find_list_in_json([], _Key) ->
    [];
find_list_in_json([{K, V} | Rest], Key) when is_binary(K) ->
    case K of
        Key when is_list(V) ->
            lists:filtermap(fun
                (Item) when is_binary(Item) -> {true, Item};
                (Item) when is_list(Item) -> {true, list_to_binary(Item)};
                (_) -> false
            end, V);
        Key -> [];
        _ -> find_list_in_json(Rest, Key)
    end;
find_list_in_json([_ | Rest], Key) ->
    find_list_in_json(Rest, Key).
