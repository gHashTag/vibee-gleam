-module(vibee_audit_ffi).
-export([
    system_time_ms/0,
    get_session_id/0,
    hash_string/1,
    append_to_audit_log/1
]).

%% Audit log file path
-define(AUDIT_LOG, "/tmp/vibee_audit.jsonl").

%% Get current timestamp in milliseconds
system_time_ms() ->
    erlang:system_time(millisecond).

%% Get or generate session ID
%% Uses process dictionary to maintain session across calls
get_session_id() ->
    case get(vibee_session_id) of
        undefined ->
            %% Generate new session ID: timestamp + random
            Now = erlang:system_time(second),
            Rand = rand:uniform(999999),
            SessionId = iolist_to_binary([
                integer_to_binary(Now),
                <<"-">>,
                integer_to_binary(Rand)
            ]),
            put(vibee_session_id, SessionId),
            SessionId;
        SessionId ->
            SessionId
    end.

%% SHA256 hash of string (returns hex)
hash_string(Binary) when is_binary(Binary) ->
    Hash = crypto:hash(sha256, Binary),
    bin_to_hex(Hash).

%% Convert binary to hex string
bin_to_hex(Bin) ->
    << <<(hex_char(N div 16)), (hex_char(N rem 16))>> || <<N>> <= Bin >>.

hex_char(N) when N < 10 -> $0 + N;
hex_char(N) -> $a + N - 10.

%% Append line to audit log file (JSONL format)
append_to_audit_log(Line) when is_binary(Line) ->
    %% Ensure directory exists
    filelib:ensure_dir(?AUDIT_LOG),

    %% Append to file with newline
    case file:open(?AUDIT_LOG, [append, raw, binary]) of
        {ok, Fd} ->
            file:write(Fd, <<Line/binary, "\n">>),
            file:close(Fd),
            nil;
        {error, _Reason} ->
            %% Log error but don't crash
            nil
    end.
