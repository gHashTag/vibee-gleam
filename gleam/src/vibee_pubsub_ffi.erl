-module(vibee_pubsub_ffi).
-export([subscribe/4, unsubscribe/2, unsubscribe_client/1, publish/2,
         get_pending_events/2, clear_pending_events/2]).

-define(SUBS_TABLE, graphql_subscriptions).
-define(EVENTS_TABLE, graphql_events).

%% Initialize ETS tables
init() ->
    case ets:info(?SUBS_TABLE) of
        undefined ->
            ets:new(?SUBS_TABLE, [named_table, public, bag]),
            ets:new(?EVENTS_TABLE, [named_table, public, bag]);
        _ -> ok
    end,
    nil.

%% Subscribe to a topic
subscribe(ClientId, SubscriptionId, Topic, Filter) ->
    init(),
    ets:insert(?SUBS_TABLE, {{ClientId, SubscriptionId}, Topic, Filter}),
    nil.

%% Unsubscribe from a specific subscription
unsubscribe(ClientId, SubscriptionId) ->
    init(),
    ets:delete(?SUBS_TABLE, {ClientId, SubscriptionId}),
    ets:delete(?EVENTS_TABLE, {ClientId, SubscriptionId}),
    nil.

%% Unsubscribe all subscriptions for a client
unsubscribe_client(ClientId) ->
    init(),
    ets:match_delete(?SUBS_TABLE, {{ClientId, '_'}, '_', '_'}),
    ets:match_delete(?EVENTS_TABLE, {{ClientId, '_'}, '_'}),
    nil.

%% Publish an event to a topic
publish(Topic, Event) ->
    init(),
    %% Find all subscribers for this topic
    Subs = ets:match_object(?SUBS_TABLE, {'_', Topic, '_'}),
    lists:foreach(fun({{ClientId, SubId}, _Topic, _Filter}) ->
        ets:insert(?EVENTS_TABLE, {{ClientId, SubId}, Event})
    end, Subs),
    nil.

%% Get pending events for a subscription
get_pending_events(ClientId, SubscriptionId) ->
    init(),
    Events = ets:lookup(?EVENTS_TABLE, {ClientId, SubscriptionId}),
    [Event || {_, Event} <- Events].

%% Clear pending events after delivery
clear_pending_events(ClientId, SubscriptionId) ->
    init(),
    ets:delete(?EVENTS_TABLE, {ClientId, SubscriptionId}),
    nil.
