-module(vibee_web_auth_ffi).
-export([
    init_ets/0,
    store_session/2,
    get_session/1,
    delete_session/1,
    generate_token/0,
    current_timestamp/0
]).

-define(TABLE, vibee_web_sessions).

%% Initialize ETS table for web sessions
init_ets() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set]),
            nil;
        _ ->
            nil
    end.

%% Store session in ETS
%% Token -> JSON string with session data
store_session(Token, Data) ->
    init_ets(),
    ets:insert(?TABLE, {Token, Data}),
    nil.

%% Get session from ETS
%% Returns {ok, Data} or {error, nil}
get_session(Token) ->
    init_ets(),
    case ets:lookup(?TABLE, Token) of
        [{Token, Data}] -> {ok, Data};
        [] -> {error, nil}
    end.

%% Delete session from ETS
delete_session(Token) ->
    init_ets(),
    ets:delete(?TABLE, Token),
    nil.

%% Generate a random session token
%% Returns binary (UTF-8 string for Gleam)
generate_token() ->
    Bytes = crypto:strong_rand_bytes(32),
    Base64 = base64:encode(Bytes),
    %% Remove special characters for URL safety
    %% Return as binary (Gleam string), NOT list!
    binary:replace(Base64, [<<"+">>, <<"/">>, <<"=">>], <<>>, [global]).

%% Get current UNIX timestamp
current_timestamp() ->
    os:system_time(second).
