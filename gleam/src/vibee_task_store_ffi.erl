-module(vibee_task_store_ffi).

%% Task Store FFI for Rainbow Bridge
%% ETS-backed task storage with subscriptions

-export([
    init/0,
    generate_task_id/0,
    get_timestamp/0,
    store_task/1,
    get_task/1,
    list_tasks/1,
    delete_task/1,
    subscribe/2,
    unsubscribe/2,
    notify_subscribers/3
]).

-define(TASK_TABLE, vibee_tasks).
-define(SUBS_TABLE, vibee_task_subscriptions).

%% Initialize ETS tables
init() ->
    case ets:info(?TASK_TABLE) of
        undefined ->
            ets:new(?TASK_TABLE, [named_table, public, set, {keypos, 1}]);
        _ ->
            ok
    end,
    case ets:info(?SUBS_TABLE) of
        undefined ->
            ets:new(?SUBS_TABLE, [named_table, public, bag, {keypos, 1}]);
        _ ->
            ok
    end,
    nil.

%% Generate unique task ID
generate_task_id() ->
    Bytes = crypto:strong_rand_bytes(8),
    Hex = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B:8>> <= Bytes]),
    list_to_binary("task_" ++ Hex).

%% Get current timestamp in milliseconds
get_timestamp() ->
    erlang:system_time(millisecond).

%% Store task in ETS
store_task(Task) ->
    init(),
    %% Task is a Gleam record, extract ID for key
    TaskId = extract_task_id(Task),
    ets:insert(?TASK_TABLE, {TaskId, Task}),
    nil.

%% Get task by ID
get_task(TaskId) ->
    init(),
    case ets:lookup(?TASK_TABLE, TaskId) of
        [{_, Task}] -> {some, Task};
        [] -> none
    end.

%% List tasks, optionally filtered by state
list_tasks(StateFilter) ->
    init(),
    AllTasks = ets:tab2list(?TASK_TABLE),
    Tasks = [Task || {_, Task} <- AllTasks],
    case StateFilter of
        none -> Tasks;
        {some, State} ->
            lists:filter(fun(Task) ->
                task_state_matches(Task, State)
            end, Tasks)
    end.

%% Delete task by ID
delete_task(TaskId) ->
    init(),
    case ets:lookup(?TASK_TABLE, TaskId) of
        [{_, _}] ->
            ets:delete(?TASK_TABLE, TaskId),
            ets:delete(?SUBS_TABLE, TaskId),
            true;
        [] ->
            false
    end.

%% Subscribe to task updates
subscribe(TaskId, SubscriberId) ->
    init(),
    ets:insert(?SUBS_TABLE, {TaskId, SubscriberId}),
    nil.

%% Unsubscribe from task updates
unsubscribe(TaskId, SubscriberId) ->
    init(),
    ets:delete_object(?SUBS_TABLE, {TaskId, SubscriberId}),
    nil.

%% Notify all subscribers of a task
notify_subscribers(TaskId, EventType, Data) ->
    init(),
    Subscribers = ets:lookup(?SUBS_TABLE, TaskId),
    lists:foreach(fun({_, SubscriberId}) ->
        %% Send notification to subscriber
        %% This could be via process message, webhook, etc.
        notify_subscriber(SubscriberId, TaskId, EventType, Data)
    end, Subscribers),
    nil.

%% Internal: Notify a single subscriber
notify_subscriber(SubscriberId, TaskId, EventType, Data) ->
    %% Log the notification for now
    %% In production, this would send to WebSocket, event bus, etc.
    io:format("[TASK_STORE] Notify ~s: task=~s event=~s data=~s~n",
              [SubscriberId, TaskId, EventType, Data]),
    ok.

%% Internal: Extract task ID from Gleam Task record
extract_task_id(Task) ->
    %% Gleam record: {task, Id, Description, State, Context, Metrics, History, CreatedAt, UpdatedAt}
    case Task of
        {task, Id, _, _, _, _, _, _, _} -> Id;
        _ -> <<"unknown">>
    end.

%% Internal: Check if task state matches filter
task_state_matches(Task, StateStr) ->
    %% Extract state from task record
    case Task of
        {task, _, _, State, _, _, _, _, _} ->
            state_to_string(State) =:= StateStr;
        _ ->
            false
    end.

%% Convert state atom to string
state_to_string(pending) -> <<"pending">>;
state_to_string(in_progress) -> <<"in_progress">>;
state_to_string(waiting_feedback) -> <<"waiting_feedback">>;
state_to_string(healing) -> <<"healing">>;
state_to_string(completed) -> <<"completed">>;
state_to_string(failed) -> <<"failed">>;
state_to_string(_) -> <<"unknown">>.
