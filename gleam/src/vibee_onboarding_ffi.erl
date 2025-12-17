-module(vibee_onboarding_ffi).

%% Onboarding Store FFI for VIBEE MCP
%% ETS-backed onboarding state storage with progress tracking

-export([
    init/0,
    generate_id/0,
    get_timestamp/0,
    create/3,
    get/1,
    update_status/2,
    update_progress/4,
    set_selected_dialogs/2,
    set_dialogs_list/2,
    complete/2,
    fail/2,
    list_active/0,
    delete/1
]).

-define(TABLE, vibee_onboarding).

%% Initialize ETS table
init() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set, {keypos, 1}]);
        _ ->
            ok
    end,
    nil.

%% Generate unique onboarding ID
generate_id() ->
    Bytes = crypto:strong_rand_bytes(6),
    Hex = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B:8>> <= Bytes]),
    list_to_binary("onb_" ++ Hex).

%% Get current timestamp
get_timestamp() ->
    erlang:system_time(millisecond).

%% Create new onboarding session
%% Returns: {ok, OnboardingId} | {error, Reason}
create(SessionId, Phone, AutoSelect) ->
    init(),
    Id = generate_id(),
    Now = get_timestamp(),
    State = #{
        id => Id,
        session_id => SessionId,
        phone => Phone,
        status => pending,
        auto_select => AutoSelect,
        dialogs => [],
        selected_dialog_ids => [],
        progress => #{
            dialogs_found => 0,
            dialogs_scanned => 0,
            messages_parsed => 0,
            embeddings_generated => 0,
            current_dialog => none
        },
        started_at => Now,
        completed_at => none,
        error => none
    },
    ets:insert(?TABLE, {Id, State}),
    {ok, Id}.

%% Get onboarding by ID
get(Id) ->
    init(),
    case ets:lookup(?TABLE, Id) of
        [{_, State}] -> {some, State};
        [] -> none
    end.

%% Update status
update_status(Id, NewStatus) ->
    init(),
    case ets:lookup(?TABLE, Id) of
        [{_, State}] ->
            Updated = State#{status => NewStatus},
            ets:insert(?TABLE, {Id, Updated}),
            ok;
        [] ->
            {error, not_found}
    end.

%% Update progress
update_progress(Id, DialogsScanned, MessagesParsed, CurrentDialog) ->
    init(),
    case ets:lookup(?TABLE, Id) of
        [{_, State}] ->
            Progress = maps:get(progress, State),
            UpdatedProgress = Progress#{
                dialogs_scanned => DialogsScanned,
                messages_parsed => MessagesParsed,
                current_dialog => CurrentDialog
            },
            Updated = State#{progress => UpdatedProgress},
            ets:insert(?TABLE, {Id, Updated}),
            ok;
        [] ->
            {error, not_found}
    end.

%% Set dialogs list (after scanning)
set_dialogs_list(Id, Dialogs) ->
    init(),
    case ets:lookup(?TABLE, Id) of
        [{_, State}] ->
            Progress = maps:get(progress, State),
            UpdatedProgress = Progress#{dialogs_found => length(Dialogs)},
            Updated = State#{
                dialogs => Dialogs,
                progress => UpdatedProgress,
                status => waiting_selection
            },
            ets:insert(?TABLE, {Id, Updated}),
            ok;
        [] ->
            {error, not_found}
    end.

%% Set selected dialogs for processing
set_selected_dialogs(Id, DialogIds) ->
    init(),
    case ets:lookup(?TABLE, Id) of
        [{_, State}] ->
            Updated = State#{
                selected_dialog_ids => DialogIds,
                status => embedding
            },
            ets:insert(?TABLE, {Id, Updated}),
            ok;
        [] ->
            {error, not_found}
    end.

%% Mark as completed
complete(Id, EmbeddingsCount) ->
    init(),
    case ets:lookup(?TABLE, Id) of
        [{_, State}] ->
            Progress = maps:get(progress, State),
            UpdatedProgress = Progress#{embeddings_generated => EmbeddingsCount},
            Updated = State#{
                status => completed,
                progress => UpdatedProgress,
                completed_at => get_timestamp()
            },
            ets:insert(?TABLE, {Id, Updated}),
            ok;
        [] ->
            {error, not_found}
    end.

%% Mark as failed
fail(Id, ErrorMsg) ->
    init(),
    case ets:lookup(?TABLE, Id) of
        [{_, State}] ->
            Updated = State#{
                status => failed,
                error => ErrorMsg,
                completed_at => get_timestamp()
            },
            ets:insert(?TABLE, {Id, Updated}),
            ok;
        [] ->
            {error, not_found}
    end.

%% List active onboarding sessions (not completed/failed)
list_active() ->
    init(),
    AllItems = ets:tab2list(?TABLE),
    ActiveStates = [pending, scanning, waiting_selection, embedding],
    [State || {_, State} <- AllItems,
              lists:member(maps:get(status, State), ActiveStates)].

%% Delete onboarding session
delete(Id) ->
    init(),
    case ets:lookup(?TABLE, Id) of
        [{_, _}] ->
            ets:delete(?TABLE, Id),
            true;
        [] ->
            false
    end.
