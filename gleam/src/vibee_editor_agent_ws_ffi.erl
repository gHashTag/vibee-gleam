-module(vibee_editor_agent_ws_ffi).
-export([get_env_or/2]).

%% Get environment variable with default value
get_env_or(Key, Default) ->
    case os:getenv(binary_to_list(Key)) of
        false -> Default;
        Value -> list_to_binary(Value)
    end.
