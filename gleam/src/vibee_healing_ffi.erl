-module(vibee_healing_ffi).

%% Healing FFI for Rainbow Bridge
%% File operations and build execution for self-healing

-export([
    init/0,
    read_file/1,
    write_file/2,
    run_build/1,
    get_timestamp/0
]).

-define(BACKUP_DIR, "/tmp/vibee_healing_backups").

%% Initialize healing system
init() ->
    %% Create backup directory
    file:make_dir(?BACKUP_DIR),
    nil.

%% Read file contents
read_file(Path) ->
    case file:read_file(Path) of
        {ok, Content} -> {ok, Content};
        {error, Reason} -> {error, atom_to_binary(Reason, utf8)}
    end.

%% Write file contents with backup
write_file(Path, Content) ->
    %% Create backup first
    BackupPath = create_backup(Path),
    case file:write_file(Path, Content) of
        ok -> {ok, nil};
        {error, Reason} ->
            %% Restore from backup on failure
            restore_backup(BackupPath, Path),
            {error, atom_to_binary(Reason, utf8)}
    end.

%% Run gleam build
run_build(Path) ->
    Cmd = "cd " ++ binary_to_list(Path) ++ " && gleam build 2>&1",
    Result = os:cmd(Cmd),
    Output = list_to_binary(Result),
    ExitCode = case binary:match(Output, <<"error:">>) of
        nomatch -> 0;
        _ -> 1
    end,
    {build_output, ExitCode, Output}.

%% Get timestamp
get_timestamp() ->
    erlang:system_time(millisecond).

%% Internal: Create backup of file
create_backup(Path) ->
    Timestamp = integer_to_list(erlang:system_time(second)),
    Filename = filename:basename(binary_to_list(Path)),
    BackupPath = ?BACKUP_DIR ++ "/" ++ Filename ++ "." ++ Timestamp ++ ".bak",
    case file:read_file(Path) of
        {ok, Content} ->
            file:write_file(BackupPath, Content),
            BackupPath;
        _ ->
            ""
    end.

%% Internal: Restore from backup
restore_backup(BackupPath, OriginalPath) ->
    case BackupPath of
        "" -> ok;
        _ ->
            case file:read_file(BackupPath) of
                {ok, Content} -> file:write_file(OriginalPath, Content);
                _ -> ok
            end
    end.
