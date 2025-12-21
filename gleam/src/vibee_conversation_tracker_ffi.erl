-module(vibee_conversation_tracker_ffi).
-export([init/0, upsert/3, get/2, delete/2, list_all/0]).

-define(TABLE, vibee_active_conversations).

%% Initialize ETS table
init() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set, {keypos, 1}]);
        _ ->
            ok
    end,
    nil.

%% Upsert conversation
%% Key: {ChatId, UserId}
upsert(ChatId, UserId, Conv) ->
    init(),
    Key = {ChatId, UserId},
    ets:insert(?TABLE, {Key, Conv}),
    nil.

%% Get conversation
get(ChatId, UserId) ->
    init(),
    Key = {ChatId, UserId},
    case ets:lookup(?TABLE, Key) of
        [{Key, Conv}] -> {some, Conv};
        [] -> none
    end.

%% Delete conversation
delete(ChatId, UserId) ->
    init(),
    Key = {ChatId, UserId},
    ets:delete(?TABLE, Key),
    nil.

%% List all active conversations
list_all() ->
    init(),
    case ets:tab2list(?TABLE) of
        [] -> [];
        Entries -> [Conv || {_Key, Conv} <- Entries]
    end.
