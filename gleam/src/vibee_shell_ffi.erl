-module(vibee_shell_ffi).
-export([safe_cmd/1]).

%% Safe wrapper around os:cmd that catches exceptions
safe_cmd(Command) when is_binary(Command) ->
    safe_cmd(binary_to_list(Command));
safe_cmd(Command) when is_list(Command) ->
    try
        Result = os:cmd(Command),
        {ok, list_to_binary(Result)}
    catch
        error:badarg ->
            {error, <<"Command execution failed: badarg">>};
        error:Reason ->
            {error, list_to_binary(io_lib:format("Command execution failed: ~p", [Reason]))};
        _:Exception ->
            {error, list_to_binary(io_lib:format("Command failed with exception: ~p", [Exception]))}
    end.
