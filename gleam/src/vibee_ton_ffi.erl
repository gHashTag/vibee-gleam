%% TON Client FFI for VIBEE MCP
%% Environment variable access and HTTP helpers

-module(vibee_ton_ffi).

-export([
    get_env/2,
    http_get/2
]).

%% Get environment variable with default
get_env(Key, Default) when is_binary(Key), is_binary(Default) ->
    case os:getenv(binary_to_list(Key)) of
        false -> Default;
        Value -> list_to_binary(Value)
    end.

%% Simple HTTP GET request (for TON Center API)
%% Returns: {ok, Body} | {error, Reason}
http_get(Url, Headers) when is_binary(Url), is_list(Headers) ->
    %% Ensure inets is started
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),

    %% Convert headers to httpc format
    HttpcHeaders = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],

    Request = {binary_to_list(Url), HttpcHeaders},

    case httpc:request(get, Request, [{timeout, 30000}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _RespHeaders, Body}} ->
            {ok, Body};
        {ok, {{_, StatusCode, _}, _RespHeaders, Body}} ->
            {error, {StatusCode, Body}};
        {error, Reason} ->
            {error, Reason}
    end.
