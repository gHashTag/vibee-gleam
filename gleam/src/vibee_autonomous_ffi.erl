-module(vibee_autonomous_ffi).

%% Autonomous Debug Cycle FFI
%% Provides build execution, file operations, and utilities

-export([
    run_build/1,
    read_file/1,
    write_file/2,
    generate_fix_id/0,
    get_timestamp/0,
    float_to_int/1,
    int_to_float/1
]).

%% Run gleam build and capture output
run_build(Path) ->
    %% Change to project directory and run build
    Cmd = "cd " ++ binary_to_list(Path) ++ " && gleam build 2>&1",
    Result = os:cmd(Cmd),
    Output = list_to_binary(Result),
    %% Check for success by looking for error patterns
    ExitCode = case binary:match(Output, <<"error:">>) of
        nomatch -> 0;
        _ -> 1
    end,
    {build_output, ExitCode, Output}.

%% Read file contents
read_file(Path) ->
    case file:read_file(Path) of
        {ok, Content} -> {ok, Content};
        {error, Reason} -> {error, atom_to_binary(Reason, utf8)}
    end.

%% Write file contents
write_file(Path, Content) ->
    case file:write_file(Path, Content) of
        ok -> {ok, nil};
        {error, Reason} -> {error, atom_to_binary(Reason, utf8)}
    end.

%% Generate unique fix ID
generate_fix_id() ->
    Bytes = crypto:strong_rand_bytes(6),
    Hex = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B:8>> <= Bytes]),
    list_to_binary("fix_" ++ Hex).

%% Get current timestamp in milliseconds
get_timestamp() ->
    erlang:system_time(millisecond).

%% Convert float to integer
float_to_int(F) ->
    trunc(F).

%% Convert integer to float
int_to_float(I) ->
    float(I).
