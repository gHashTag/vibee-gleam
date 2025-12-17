-module(vibee_log_aggregator_ffi).
-export([register_aggregator/1, lookup_aggregator/0]).

%% Registry key for log aggregator
-define(REGISTRY_KEY, vibee_log_aggregator).

%% Register the aggregator subject globally
register_aggregator(Subject) ->
    persistent_term:put(?REGISTRY_KEY, Subject),
    nil.

%% Lookup the registered aggregator
lookup_aggregator() ->
    try
        Subject = persistent_term:get(?REGISTRY_KEY),
        {some, Subject}
    catch
        error:badarg -> none
    end.
