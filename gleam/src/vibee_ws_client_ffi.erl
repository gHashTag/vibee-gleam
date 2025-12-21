%% WebSocket Client FFI for Gleam
%% Uses gun library for WebSocket connections to telegram-bridge
%% Supports both WS and WSS (TLS) connections

-module(vibee_ws_client_ffi).
-export([
    connect/3,
    connect_tls/4,
    send/2,
    close/1,
    receive_message/2,
    send_ping/1
]).

%% Connect to WebSocket server (plain HTTP - for localhost testing)
%% Returns {ok, ConnPid, StreamRef} or {error, Reason}
connect(Host, Port, Path) ->
    connect_internal(Host, Port, Path, #{protocols => [http]}, []).

%% Connect to WebSocket server with TLS (for production wss://)
%% Headers is a list of {Name, Value} tuples for Authorization etc
connect_tls(Host, Port, Path, Headers) ->
    TlsOpts = #{
        protocols => [http],
        transport => tls,
        tls_opts => [
            {verify, verify_peer},
            {cacerts, public_key:cacerts_get()},
            {customize_hostname_check, [
                {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
            ]}
        ]
    },
    connect_internal(Host, Port, Path, TlsOpts, Headers).

%% Internal connection function
connect_internal(Host, Port, Path, Opts, Headers) ->
    HostStr = binary_to_list(Host),
    PathStr = binary_to_list(Path),

    case gun:open(HostStr, Port, Opts) of
        {ok, ConnPid} ->
            case gun:await_up(ConnPid, 10000) of
                {ok, _Protocol} ->
                    %% Build headers for upgrade request
                    UpgradeHeaders = lists:map(
                        fun({Name, Value}) ->
                            {binary_to_list(Name), binary_to_list(Value)}
                        end,
                        Headers
                    ),

                    StreamRef = gun:ws_upgrade(ConnPid, PathStr, UpgradeHeaders),
                    receive
                        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _RespHeaders} ->
                            {ok, {ConnPid, StreamRef}};
                        {gun_response, ConnPid, _, _, Status, _RespHeaders} ->
                            gun:close(ConnPid),
                            {error, {upgrade_failed, Status}};
                        {gun_error, ConnPid, StreamRef, Reason} ->
                            gun:close(ConnPid),
                            {error, Reason};
                        {gun_error, ConnPid, Reason} ->
                            gun:close(ConnPid),
                            {error, Reason}
                    after 15000 ->
                        gun:close(ConnPid),
                        {error, timeout}
                    end;
                {error, Reason} ->
                    gun:close(ConnPid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Send text message through WebSocket
send({ConnPid, StreamRef}, Text) ->
    gun:ws_send(ConnPid, StreamRef, {text, Text}),
    ok.

%% Send ping frame for keepalive
send_ping({ConnPid, StreamRef}) ->
    gun:ws_send(ConnPid, StreamRef, ping),
    ok.

%% Close WebSocket connection
close({ConnPid, _StreamRef}) ->
    gun:close(ConnPid),
    ok.

%% Receive message with timeout (in milliseconds)
%% Returns {ok, Data} | {error, Reason}
receive_message({ConnPid, StreamRef}, Timeout) ->
    receive
        {gun_ws, ConnPid, StreamRef, {text, Data}} ->
            {ok, Data};
        {gun_ws, ConnPid, StreamRef, {binary, Data}} ->
            {ok, Data};
        {gun_ws, ConnPid, StreamRef, ping} ->
            %% Auto-respond to ping with pong
            gun:ws_send(ConnPid, StreamRef, pong),
            {ok, <<"ping">>};
        {gun_ws, ConnPid, StreamRef, pong} ->
            {ok, <<"pong">>};
        {gun_ws, ConnPid, StreamRef, close} ->
            {error, closed};
        {gun_ws, ConnPid, StreamRef, {close, Code, Reason}} ->
            {error, {closed, Code, Reason}};
        {gun_down, ConnPid, _, Reason, _} ->
            {error, {connection_down, Reason}};
        {gun_error, ConnPid, StreamRef, Reason} ->
            {error, Reason};
        {gun_error, ConnPid, Reason} ->
            {error, {gun_error, Reason}}
    after Timeout ->
        {error, timeout}
    end.
