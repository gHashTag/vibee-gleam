-module(vibee_agent_registry_ffi).
-export([
    init/0,
    register_agent/8,
    unregister_agent/1,
    update_status/2,
    update_activity/1,
    increment_messages/1,
    increment_errors/1,
    reset_counts/1,
    list_all/0,
    get_agent/1,
    get_unix_timestamp/0,
    get_iso_timestamp_string/0
]).

-define(TABLE, vibee_agent_registry).

%% Record structure: {id, agent_type, status, started_at, last_activity, msg_count, err_count, session_id}

init() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}]);
        _ ->
            ok
    end,
    nil.

register_agent(Id, AgentType, Status, StartedAt, LastActivity, MsgCount, ErrCount, SessionId) ->
    init(),
    ets:insert(?TABLE, {Id, AgentType, Status, StartedAt, LastActivity, MsgCount, ErrCount, SessionId}),
    nil.

unregister_agent(Id) ->
    init(),
    ets:delete(?TABLE, Id),
    nil.

update_status(Id, Status) ->
    init(),
    case ets:lookup(?TABLE, Id) of
        [{Id, AgentType, _OldStatus, StartedAt, _LastActivity, MsgCount, ErrCount, SessionId}] ->
            Timestamp = get_iso_timestamp(),
            ets:insert(?TABLE, {Id, AgentType, Status, StartedAt, Timestamp, MsgCount, ErrCount, SessionId});
        [] ->
            ok
    end,
    nil.

update_activity(Id) ->
    init(),
    case ets:lookup(?TABLE, Id) of
        [{Id, AgentType, Status, StartedAt, _LastActivity, MsgCount, ErrCount, SessionId}] ->
            Timestamp = get_iso_timestamp(),
            ets:insert(?TABLE, {Id, AgentType, Status, StartedAt, Timestamp, MsgCount, ErrCount, SessionId});
        [] ->
            ok
    end,
    nil.

increment_messages(Id) ->
    init(),
    case ets:lookup(?TABLE, Id) of
        [{Id, AgentType, Status, StartedAt, _LastActivity, MsgCount, ErrCount, SessionId}] ->
            Timestamp = get_iso_timestamp(),
            ets:insert(?TABLE, {Id, AgentType, Status, StartedAt, Timestamp, MsgCount + 1, ErrCount, SessionId});
        [] ->
            ok
    end,
    nil.

increment_errors(Id) ->
    init(),
    case ets:lookup(?TABLE, Id) of
        [{Id, AgentType, Status, StartedAt, _LastActivity, MsgCount, ErrCount, SessionId}] ->
            Timestamp = get_iso_timestamp(),
            ets:insert(?TABLE, {Id, AgentType, Status, StartedAt, Timestamp, MsgCount, ErrCount + 1, SessionId});
        [] ->
            ok
    end,
    nil.

reset_counts(Id) ->
    init(),
    case ets:lookup(?TABLE, Id) of
        [{Id, AgentType, Status, StartedAt, _LastActivity, _MsgCount, _ErrCount, SessionId}] ->
            Timestamp = get_iso_timestamp(),
            ets:insert(?TABLE, {Id, AgentType, Status, StartedAt, Timestamp, 0, 0, SessionId});
        [] ->
            ok
    end,
    nil.

list_all() ->
    init(),
    Agents = ets:tab2list(?TABLE),
    [to_tuple(Agent) || Agent <- Agents].

get_agent(Id) ->
    init(),
    case ets:lookup(?TABLE, Id) of
        [{Id, AgentType, Status, StartedAt, LastActivity, MsgCount, ErrCount, SessionId}] ->
            {some, {Id, AgentType, Status, StartedAt, LastActivity, MsgCount, ErrCount, SessionId}};
        [] ->
            none
    end.

%% Internal helpers

to_tuple({Id, AgentType, Status, StartedAt, LastActivity, MsgCount, ErrCount, SessionId}) ->
    {Id, AgentType, Status, StartedAt, LastActivity, MsgCount, ErrCount, SessionId}.

get_iso_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    MilliSec = erlang:system_time(millisecond) rem 1000,
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
                                 [Year, Month, Day, Hour, Min, Sec, MilliSec])).

%% Public timestamp functions for WebSocket
get_unix_timestamp() ->
    erlang:system_time(second).

get_iso_timestamp_string() ->
    get_iso_timestamp().
