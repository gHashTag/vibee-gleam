-module(vibee_config_ffi).
-export([get_env/1, get_env/2, log_error/1]).

%% Get environment variable (returns Result)
get_env(Name) when is_binary(Name) ->
    case os:getenv(binary_to_list(Name)) of
        false -> {error, nil};
        Value -> {ok, list_to_binary(Value)}
    end.

%% Get environment variable with default (returns String directly)
get_env(Name, Default) when is_binary(Name), is_binary(Default) ->
    case os:getenv(binary_to_list(Name)) of
        false -> Default;
        Value -> list_to_binary(Value)
    end;
get_env(Name, Default) when is_binary(Name) ->
    case os:getenv(binary_to_list(Name)) of
        false -> Default;
        Value -> list_to_binary(Value)
    end.

%% Log error message to stderr
log_error(Msg) when is_binary(Msg) ->
    io:format(standard_error, "[CONFIG ERROR] ~s~n", [Msg]),
    nil.
