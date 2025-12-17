-module(vibee_session_ffi).

%% Session Manager FFI for VIBEE
%% ETS-backed session storage for multi-account Telegram support
%% With file persistence for session recovery after restart

-export([
    init/0,
    get_active/0,
    set_active/1,
    clear_active/0,
    upsert_session/5,
    get_session/1,
    list_sessions/0,
    remove_session/1
]).

-define(SESSION_TABLE, vibee_sessions).
-define(ACTIVE_KEY, '__active_session__').
%% Session file path - uses persistent volume on Fly.io
session_file() ->
    case os:getenv("VIBEE_SESSION_FILE") of
        false -> "/app/sessions/sessions.dat";
        Path -> Path
    end.

%% Initialize ETS table and load from disk
init() ->
    try
        case ets:info(?SESSION_TABLE) of
            undefined ->
                ets:new(?SESSION_TABLE, [named_table, public, set, {keypos, 1}]),
                io:format("[FFI] ETS table ~p created~n", [?SESSION_TABLE]),
                %% Load sessions from disk
                load_from_disk();
            _ ->
                io:format("[FFI] ETS table ~p already exists~n", [?SESSION_TABLE]),
                ok
        end
    catch
        _Type:_Error ->
            io:format("[FFI] ETS table creation failed, recreating~n"),
            catch ets:delete(?SESSION_TABLE),
            ets:new(?SESSION_TABLE, [named_table, public, set, {keypos, 1}]),
            load_from_disk()
    end,
    nil.

%% Load sessions from disk file
load_from_disk() ->
    File = session_file(),
    io:format("[FFI] Loading sessions from: ~s~n", [File]),
    case file:read_file(File) of
        {ok, Binary} ->
            try
                Data = binary_to_term(Binary),
                lists:foreach(fun(Record) ->
                    ets:insert(?SESSION_TABLE, Record)
                end, Data),
                io:format("[FFI] Loaded ~p sessions from disk~n", [length(Data)])
            catch
                _:_ -> io:format("[FFI] Failed to parse session file, starting fresh~n")
            end;
        {error, enoent} ->
            io:format("[FFI] No session file found, starting fresh~n");
        {error, Reason} ->
            io:format("[FFI] Failed to read session file: ~p~n", [Reason])
    end.

%% Save sessions to disk
save_to_disk() ->
    File = session_file(),
    AllRecords = ets:tab2list(?SESSION_TABLE),
    Binary = term_to_binary(AllRecords),
    %% Ensure directory exists
    filelib:ensure_dir(File),
    case file:write_file(File, Binary) of
        ok -> ok;
        {error, Reason} ->
            io:format("[FFI] Failed to save sessions: ~p~n", [Reason])
    end.

%% Get active session ID
get_active() ->
    case ets:info(?SESSION_TABLE) of
        undefined -> none;
        _ ->
            case ets:lookup(?SESSION_TABLE, ?ACTIVE_KEY) of
                [{?ACTIVE_KEY, SessionId}] -> {some, SessionId};
                [] -> none
            end
    end.

%% Set active session
set_active(SessionId) ->
    case ets:info(?SESSION_TABLE) of
        undefined -> init();
        _ -> ok
    end,
    ets:insert(?SESSION_TABLE, {?ACTIVE_KEY, SessionId}),
    save_to_disk(),
    nil.

%% Clear active session
clear_active() ->
    case ets:info(?SESSION_TABLE) of
        undefined -> nil;
        _ ->
            ets:delete(?SESSION_TABLE, ?ACTIVE_KEY),
            save_to_disk(),
            nil
    end.

%% Upsert session info
%% SessionId, Phone, Username, Authorized, CreatedAt
upsert_session(SessionId, Phone, Username, Authorized, CreatedAt) ->
    %% Ensure table exists before insert
    case ets:info(?SESSION_TABLE) of
        undefined ->
            io:format("[FFI] Table not found, initializing...~n"),
            init();
        _ ->
            ok
    end,
    Record = {SessionId, Phone, Username, Authorized, CreatedAt},
    ets:insert(?SESSION_TABLE, Record),
    save_to_disk(),
    nil.

%% Get session by ID
get_session(SessionId) ->
    init(),
    case ets:lookup(?SESSION_TABLE, SessionId) of
        [{SessionId, Phone, Username, Authorized, CreatedAt}] ->
            {some, {SessionId, Phone, Username, Authorized, CreatedAt}};
        [] ->
            none
    end.

%% List all sessions (excluding active key)
list_sessions() ->
    init(),
    AllRecords = ets:tab2list(?SESSION_TABLE),
    %% Filter out the active key marker
    Sessions = lists:filter(fun
        ({?ACTIVE_KEY, _}) -> false;
        (_) -> true
    end, AllRecords),
    Sessions.

%% Remove session by ID
remove_session(SessionId) ->
    init(),
    %% Also clear active if this was the active session
    case get_active() of
        {some, SessionId} -> clear_active();
        _ -> ok
    end,
    case ets:lookup(?SESSION_TABLE, SessionId) of
        [{_, _, _, _, _}] ->
            ets:delete(?SESSION_TABLE, SessionId),
            save_to_disk(),
            true;
        [] ->
            false
    end.
