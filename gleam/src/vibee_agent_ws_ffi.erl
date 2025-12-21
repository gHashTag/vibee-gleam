-module(vibee_agent_ws_ffi).
-export([spawn_ticker/1]).

%% Spawn a ticker process that sends Nil to the subject every 5 seconds
spawn_ticker(Subject) ->
    spawn(fun() -> ticker_loop(Subject) end),
    nil.

ticker_loop(Subject) ->
    timer:sleep(5000),
    gleam@erlang@process:send(Subject, nil),
    ticker_loop(Subject).
