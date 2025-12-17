-module(vibee_parser_ffi).
-export([spawn_full_parse/3, spawn_dialog_parse/4]).

%% Spawn full parse process in background
%% Calls vibee@telegram@parser:parse_all_dialogs through Gleam FFI
spawn_full_parse(Pool, SessionId, JobId) ->
    spawn(fun() ->
        io:format("[PARSER:BG] Starting full parse job ~p~n", [JobId]),
        
        %% Create config
        Cfg = {'vibee@telegram@parser:ParserConfig', 100, 500, 0, true},
        
        %% Call parser
        case 'vibee@telegram@parser':parse_all_dialogs(Pool, SessionId, Cfg, JobId) of
            {ok, Result} ->
                io:format("[PARSER:BG] Completed! ~p messages~n", [element(4, Result)]),
                ResultJson = 'vibee@telegram@parser':result_to_json(Result),
                'vibee@db@postgres':complete_job(Pool, JobId, ResultJson);
            {error, Err} ->
                ErrMsg = parser_error_to_string(Err),
                io:format("[PARSER:BG] Failed: ~s~n", [ErrMsg]),
                'vibee@db@postgres':fail_job(Pool, JobId, ErrMsg)
        end
    end),
    nil.

%% Spawn single dialog parse in background
spawn_dialog_parse(Pool, SessionId, DialogId, JobId) ->
    spawn(fun() ->
        io:format("[PARSER:BG] Starting dialog parse job ~p for dialog ~p~n", [JobId, DialogId]),
        
        %% Create config
        Cfg = {'vibee@telegram@parser:ParserConfig', 100, 500, 0, true},
        
        %% Call parser
        case 'vibee@telegram@parser':parse_single_dialog(Pool, SessionId, DialogId, Cfg) of
            {ok, Result} ->
                io:format("[PARSER:BG] Dialog complete! ~p messages~n", [element(2, Result)]),
                ResultJson = 'vibee@telegram@parser':dialog_result_to_json(Result),
                'vibee@db@postgres':complete_job(Pool, JobId, ResultJson);
            {error, Err} ->
                ErrMsg = parser_error_to_string(Err),
                io:format("[PARSER:BG] Dialog parse failed: ~s~n", [ErrMsg]),
                'vibee@db@postgres':fail_job(Pool, JobId, ErrMsg)
        end
    end),
    nil.

%% Convert parser error to string
parser_error_to_string({parser_api_error, Msg}) -> Msg;
parser_error_to_string({parser_db_error, Msg}) -> Msg;
parser_error_to_string({parser_connection_error, Msg}) -> Msg;
parser_error_to_string(parser_rate_limited) -> <<"Rate limited by Telegram">>;
parser_error_to_string(parser_cancelled) -> <<"Cancelled">>;
parser_error_to_string(Other) -> iolist_to_binary(io_lib:format("~p", [Other])).
