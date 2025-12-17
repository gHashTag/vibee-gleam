-module(vibee_ffi).
-export([get_unix_timestamp/0, get_formatted_timestamp/0, env_to_string/1, get_env/1]).

get_unix_timestamp() ->
    os:system_time(second).

get_formatted_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                                  [Year, Month, Day, Hour, Min, Sec])).

%% Convert environment variable value to binary string
%% os:getenv returns charlist or false
env_to_string(false) -> <<>>;
env_to_string(Value) when is_list(Value) -> list_to_binary(Value);
env_to_string(Value) when is_binary(Value) -> Value;
env_to_string(_) -> <<>>.

%% Get environment variable - handles binary to charlist conversion
%% Gleam strings are binaries, but os:getenv expects charlist
get_env(Name) when is_binary(Name) ->
    env_to_string(os:getenv(binary_to_list(Name)));
get_env(Name) when is_list(Name) ->
    env_to_string(os:getenv(Name));
get_env(_) ->
    <<>>.
