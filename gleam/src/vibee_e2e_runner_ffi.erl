%% E2E Test Runner FFI - ETS storage for async test runs
%% Allows Claude Code to poll for E2E test results without HTTP timeout

-module(vibee_e2e_runner_ffi).

-export([
    init/0,
    save_test_run/2,
    get_test_run/1,
    get_test_run_unsafe/1,
    delete_test_run/1,
    generate_id/0,
    current_time_ms/0,
    spawn_async/1,
    is_none/1,
    extract_some/1
]).

-define(TABLE, vibee_e2e_runs).

%% Initialize ETS table (with race condition protection)
init() ->
    case ets:info(?TABLE) of
        undefined ->
            try
                ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}])
            catch
                error:badarg ->
                    %% Table was created by another process between check and create
                    ok
            end;
        _ ->
            ok
    end,
    nil.

%% Save test run to ETS
%% TestRun is the Gleam E2ETestRun record encoded as tuple
save_test_run(TestRunId, TestRun) ->
    error_logger:info_msg("[E2E-ETS] save_test_run called: ~p~n", [TestRunId]),
    init(),
    error_logger:info_msg("[E2E-ETS] init() done, inserting~n"),
    try
        ets:insert(?TABLE, {TestRunId, TestRun}),
        error_logger:info_msg("[E2E-ETS] insert SUCCESS for ~p~n", [TestRunId])
    catch
        Class:Reason ->
            error_logger:error_msg("[E2E-ETS] insert FAILED: ~p:~p~n", [Class, Reason])
    end,
    nil.

%% Get test run from ETS
get_test_run(TestRunId) ->
    error_logger:info_msg("[E2E-ETS] get_test_run called: ~p~n", [TestRunId]),
    init(),
    Result = case ets:lookup(?TABLE, TestRunId) of
        [{TestRunId, TestRun}] ->
            error_logger:info_msg("[E2E-ETS] FOUND: ~p~n", [TestRunId]),
            {some, TestRun};
        [] ->
            error_logger:info_msg("[E2E-ETS] NOT FOUND: ~p~n", [TestRunId]),
            none
    end,
    Result.

%% Delete test run from ETS (cleanup)
delete_test_run(TestRunId) ->
    init(),
    ets:delete(?TABLE, TestRunId),
    nil.

%% Generate unique test run ID
generate_id() ->
    %% Format: e2e_YYYYMMDD_HHMMSS_RAND
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    Rand = rand:uniform(9999),
    Id = io_lib:format("e2e_~4..0B~2..0B~2..0B_~2..0B~2..0B~2..0B_~4..0B",
                       [Year, Month, Day, Hour, Min, Sec, Rand]),
    iolist_to_binary(Id).

%% Current timestamp in milliseconds
current_time_ms() ->
    erlang:system_time(millisecond).

%% Get test run unsafely (returns the run directly or crashes)
get_test_run_unsafe(TestRunId) ->
    init(),
    case ets:lookup(?TABLE, TestRunId) of
        [{TestRunId, TestRun}] -> TestRun;
        [] -> error(not_found)
    end.

%% Spawn async function with error handling
spawn_async(Fun) ->
    io:format("[E2E-FFI] spawn_async called~n"),
    Pid = spawn(fun() ->
        io:format("[E2E-FFI] spawned process started, calling Fun()~n"),
        try
            Result = Fun(),
            io:format("[E2E-FFI] Fun() completed successfully: ~p~n", [Result])
        catch
            Class:Reason:Stacktrace ->
                io:format("[E2E-SPAWN-ERROR] ~p:~p~n~p~n", [Class, Reason, Stacktrace])
        end
    end),
    io:format("[E2E-FFI] spawned pid: ~p~n", [Pid]),
    nil.

%% Check if value is 'none' atom
is_none(none) -> true;
is_none(_) -> false.

%% Extract value from {some, Value} tuple
extract_some({some, Value}) -> Value;
extract_some(_) -> error(not_a_some_tuple).
