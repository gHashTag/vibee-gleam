%% TaskFlow Reminder Actor FFI
%% Erlang helper functions for reminder actor

-module(vibee_task_reminder_ffi).
-export([http_post/3, schedule_check/1]).

%% Make HTTP POST request to Telegram bridge
http_post(Url, ApiKey, Body) ->
    Headers = [
        {"Content-Type", "application/json"},
        {"Authorization", iolist_to_binary([<<"Bearer ">>, ApiKey])}
    ],
    Request = {binary_to_list(Url), Headers, "application/json", Body},
    case httpc:request(post, Request, [{timeout, 30000}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, ResponseBody};
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            {ok, ResponseBody};
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, iolist_to_binary([
                <<"HTTP ">>,
                integer_to_binary(StatusCode),
                <<": ">>,
                ResponseBody
            ])};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Schedule CheckReminders message after delay
%% Note: This requires the actor to be registered or passed as a parameter
%% For now, returns nil as the actor loop handles scheduling via process.send_after
schedule_check(_DelayMs) ->
    %% The actor should use process.send_after from Gleam side
    %% This is a placeholder - actual implementation should use OTP timer
    nil.
