-module(vibee_remotion_ffi).
-export([get_env_or/2]).

%% Get environment variable with default value
get_env_or(Key, Default) when is_binary(Key), is_binary(Default) ->
    case os:getenv(binary_to_list(Key)) of
        false -> Default;
        Value -> list_to_binary(Value)
    end.
