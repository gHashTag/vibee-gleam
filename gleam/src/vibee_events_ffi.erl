-module(vibee_events_ffi).

%% API
-export([
    init/0,
    store_event/1,
    get_all_events/0,
    store_subscription/1,
    remove_subscription/1,
    get_all_subscriptions/0,
    cleanup_old_events/1,
    get_event_stats/0,
    generate_event_id/0,
    generate_subscription_id/0,
    system_time_ms/0,
    invoke_callback/2
]).

%% ETS table names
-define(EVENTS_TABLE, vibee_events).
-define(SUBSCRIPTIONS_TABLE, vibee_subscriptions).
-define(CALLBACKS_TABLE, vibee_callbacks).
-define(MAX_EVENTS, 10000).

%% Initialize ETS tables
init() ->
    case ets:info(?EVENTS_TABLE) of
        undefined ->
            ets:new(?EVENTS_TABLE, [
                named_table,
                ordered_set,
                public,
                {keypos, 1}
            ]);
        _ -> ok
    end,
    case ets:info(?SUBSCRIPTIONS_TABLE) of
        undefined ->
            ets:new(?SUBSCRIPTIONS_TABLE, [
                named_table,
                set,
                public,
                {keypos, 1}
            ]);
        _ -> ok
    end,
    case ets:info(?CALLBACKS_TABLE) of
        undefined ->
            ets:new(?CALLBACKS_TABLE, [
                named_table,
                set,
                public,
                {keypos, 1}
            ]);
        _ -> ok
    end,
    nil.

%% Store an event in ETS
%% Event record: {event, Id, EventType, Payload, Target, Timestamp, Source}
store_event(Event) ->
    init(),
    %% Extract fields from Gleam record
    {event, Id, EventType, Payload, Target, Timestamp, Source} = Event,

    %% Store with timestamp as key for ordering
    Key = {Timestamp, Id},
    ets:insert(?EVENTS_TABLE, {Key, EventType, Payload, Target, Source}),

    %% Cleanup if too many events
    case ets:info(?EVENTS_TABLE, size) of
        Size when Size > ?MAX_EVENTS ->
            %% Remove oldest 10%
            cleanup_oldest(Size div 10);
        _ -> ok
    end,
    nil.

%% Get all events (most recent first)
get_all_events() ->
    init(),
    Events = ets:tab2list(?EVENTS_TABLE),
    %% Sort by timestamp descending and convert to Gleam records
    Sorted = lists:reverse(lists:sort(fun(A, B) ->
        {KeyA, _, _, _, _} = A,
        {KeyB, _, _, _, _} = B,
        KeyA >= KeyB
    end, Events)),
    lists:map(fun({{Timestamp, Id}, EventType, Payload, Target, Source}) ->
        {event, Id, EventType, Payload, Target, Timestamp, Source}
    end, Sorted).

%% Store a subscription
store_subscription(Sub) ->
    init(),
    {subscription, Id, EventTypes, TargetFilter, CallbackId} = Sub,
    ets:insert(?SUBSCRIPTIONS_TABLE, {Id, EventTypes, TargetFilter, CallbackId}),
    nil.

%% Remove a subscription
remove_subscription(Id) ->
    init(),
    ets:delete(?SUBSCRIPTIONS_TABLE, Id),
    nil.

%% Get all subscriptions
get_all_subscriptions() ->
    init(),
    Subs = ets:tab2list(?SUBSCRIPTIONS_TABLE),
    lists:map(fun({Id, EventTypes, TargetFilter, CallbackId}) ->
        {subscription, Id, EventTypes, TargetFilter, CallbackId}
    end, Subs).

%% Cleanup events older than cutoff timestamp
cleanup_old_events(CutoffTimestamp) ->
    init(),
    Events = ets:tab2list(?EVENTS_TABLE),
    OldEvents = lists:filter(fun({{Timestamp, _}, _, _, _, _}) ->
        Timestamp < CutoffTimestamp
    end, Events),
    lists:foreach(fun({Key, _, _, _, _}) ->
        ets:delete(?EVENTS_TABLE, Key)
    end, OldEvents),
    length(OldEvents).

%% Get event statistics
get_event_stats() ->
    init(),
    Events = ets:tab2list(?EVENTS_TABLE),
    Subs = ets:tab2list(?SUBSCRIPTIONS_TABLE),

    TotalEvents = length(Events),
    ActiveSubs = length(Subs),

    %% Count events by type
    TypeCounts = lists:foldl(fun({{_, _}, EventType, _, _, _}, Acc) ->
        TypeStr = event_type_to_string(EventType),
        maps:update_with(TypeStr, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, Events),

    EventsByType = maps:to_list(TypeCounts),

    %% Find oldest and newest timestamps
    Timestamps = [Ts || {{Ts, _}, _, _, _, _} <- Events],
    {Oldest, Newest} = case Timestamps of
        [] -> {0, 0};
        _ -> {lists:min(Timestamps), lists:max(Timestamps)}
    end,

    {event_stats, TotalEvents, EventsByType, Oldest, Newest, ActiveSubs}.

%% Generate unique event ID
generate_event_id() ->
    Ts = erlang:system_time(millisecond),
    Rand = rand:uniform(1000000),
    iolist_to_binary(io_lib:format("evt_~p_~p", [Ts, Rand])).

%% Generate unique subscription ID
generate_subscription_id() ->
    Ts = erlang:system_time(millisecond),
    Rand = rand:uniform(1000000),
    iolist_to_binary(io_lib:format("sub_~p_~p", [Ts, Rand])).

%% Get current time in milliseconds
system_time_ms() ->
    erlang:system_time(millisecond).

%% Invoke a callback (currently just logs)
invoke_callback(CallbackId, Event) ->
    %% For now, just log the callback invocation
    %% In the future, this could send a message to a registered process
    {event, Id, EventType, _Payload, _Target, _Timestamp, _Source} = Event,
    TypeStr = event_type_to_string(EventType),
    io:format("[EVENT BUS] Callback ~s invoked for event ~s (type: ~s)~n",
              [CallbackId, Id, TypeStr]),
    nil.

%% Internal helpers

cleanup_oldest(Count) ->
    Events = ets:tab2list(?EVENTS_TABLE),
    Sorted = lists:sort(fun(A, B) ->
        {KeyA, _, _, _, _} = A,
        {KeyB, _, _, _, _} = B,
        KeyA =< KeyB
    end, Events),
    ToDelete = lists:sublist(Sorted, Count),
    lists:foreach(fun({Key, _, _, _, _}) ->
        ets:delete(?EVENTS_TABLE, Key)
    end, ToDelete),
    ok.

event_type_to_string(EventType) ->
    case EventType of
        message_received -> <<"message_received">>;
        button_clicked -> <<"button_clicked">>;
        task_completed -> <<"task_completed">>;
        agent_response -> <<"agent_response">>;
        tool_call -> <<"tool_call">>;
        error -> <<"error">>;
        voice_transcribed -> <<"voice_transcribed">>;
        {custom, S} -> S;
        _ -> <<"unknown">>
    end.
