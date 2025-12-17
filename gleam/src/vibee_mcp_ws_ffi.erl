-module(vibee_mcp_ws_ffi).

%% API
-export([
    generate_session_id/0
]).

%% Generate unique session ID
generate_session_id() ->
    Bytes = crypto:strong_rand_bytes(16),
    list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B:8>> <= Bytes])).
