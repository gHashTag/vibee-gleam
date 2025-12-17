%% WebSocket Client FFI for Gleam
%% Uses gun library for WebSocket connections to telegram-bridge

-module(vibee_ws_client_ffi).
-export([
    connect/3,
    send/2,
    close/1,
    receive_message/2
]).

%% Connect to WebSocket server
%% Returns {ok, ConnPid, StreamRef} or {error, Reason}
connect(Host, Port, Path) ->
    case gun:open(binary_to_list(Host), Port, #{protocols => [http]}) of
        {ok, ConnPid} ->
            case gun:await_up(ConnPid, 5000) of
                {ok, _Protocol} ->
                    StreamRef = gun:ws_upgrade(ConnPid, binary_to_list(Path)),
                    receive
                        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
                            {ok, {ConnPid, StreamRef}};
                        {gun_response, ConnPid, _, _, Status, _Headers} ->
                            gun:close(ConnPid),
                            {error, {upgrade_failed, Status}};
                        {gun_error, ConnPid, StreamRef, Reason} ->
                            gun:close(ConnPid),
                            {error, Reason}
                    after 10000 ->
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

%% Close WebSocket connection
close({ConnPid, _StreamRef}) ->
    gun:close(ConnPid),
    ok.

%% Receive message with timeout (in milliseconds)
receive_message({ConnPid, StreamRef}, Timeout) ->
    receive
        {gun_ws, ConnPid, StreamRef, {text, Data}} ->
            {ok, Data};
        {gun_ws, ConnPid, StreamRef, {binary, Data}} ->
            {ok, Data};
        {gun_ws, ConnPid, StreamRef, close} ->
            {error, closed};
        {gun_ws, ConnPid, StreamRef, {close, Code, Reason}} ->
            {error, {closed, Code, Reason}};
        {gun_down, ConnPid, _, Reason, _} ->
            {error, {connection_down, Reason}};
        {gun_error, ConnPid, StreamRef, Reason} ->
            {error, Reason}
    after Timeout ->
        {error, timeout}
    end.
