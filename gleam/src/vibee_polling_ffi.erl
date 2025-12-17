-module(vibee_polling_ffi).
-export([get_api_key/0]).

%% Get VIBEE_API_KEY from environment (for bridge authentication)
get_api_key() ->
    case os:getenv("VIBEE_API_KEY") of
        false -> <<"">>;
        Value when is_list(Value) -> list_to_binary(Value);
        Value when is_binary(Value) -> Value;
        _ -> <<"">>
    end.
