-module(test_json_ffi).
-export([test/0]).

test() ->
    %% Simulate what happens when gleam json.object is passed to to_dynamic
    %% gleam/json uses thoas library internally
    JsonStr = <<"{\"crypto\":\"USDT\",\"fiat\":\"THB\"}">>,
    case thoas:decode(JsonStr) of
        {ok, Decoded} ->
            io:format("Decoded type: ~p~n", [Decoded]),
            io:format("Is map: ~p~n", [is_map(Decoded)]),
            case is_map(Decoded) of
                true ->
                    io:format("Keys: ~p~n", [maps:keys(Decoded)]);
                false ->
                    io:format("Not a map~n")
            end;
        {error, E} ->
            io:format("Error: ~p~n", [E])
    end.
