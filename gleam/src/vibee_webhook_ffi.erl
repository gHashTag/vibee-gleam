-module(vibee_webhook_ffi).
-export([parse_webhook/1]).

%% Parse webhook JSON and return WebhookInfo record
%% WebhookInfo is defined in Gleam as:
%% WebhookInfo(id: String, status: String, output_url: String, error: String)
%% In Erlang this becomes a tuple: {webhook_info, Id, Status, OutputUrl, Error}
parse_webhook(Body) when is_binary(Body) ->
    try
        Map = jiffy:decode(Body, [return_maps]),
        Id = get_id(Map),
        Status = get_status(Map),
        OutputUrl = get_output_url(Map),
        Error = get_error(Map),
        {webhook_info, Id, Status, OutputUrl, Error}
    catch
        _:_ ->
            {webhook_info, <<"unknown">>, <<"parse_error">>, <<>>, <<"Failed to parse JSON">>}
    end;
parse_webhook(_) ->
    {webhook_info, <<"unknown">>, <<"invalid_input">>, <<>>, <<"Body is not binary">>}.

%% Extract ID from various field names
get_id(Map) ->
    Fields = [<<"id">>, <<"task_id">>, <<"job_id">>, <<"jobId">>, <<"video_id">>, <<"voice_id">>],
    get_first_string(Map, Fields, <<"unknown">>).

%% Extract status from various field names
get_status(Map) ->
    Fields = [<<"status">>, <<"task_status">>, <<"state">>],
    get_first_string(Map, Fields, <<"unknown">>).

%% Extract output URL from various field names
get_output_url(Map) ->
    Fields = [<<"output">>, <<"video_url">>, <<"videoUrl">>, <<"output_url">>, <<"result">>, <<"url">>],
    case get_first_value(Map, Fields) of
        undefined -> <<>>;
        Value when is_binary(Value) -> Value;
        Value when is_list(Value) ->
            % Output might be a list (Replicate returns array)
            case Value of
                [First | _] when is_binary(First) -> First;
                _ -> <<>>
            end;
        _ -> <<>>
    end.

%% Extract error from various field names
get_error(Map) ->
    Fields = [<<"error">>, <<"error_message">>, <<"task_status_msg">>, <<"message">>],
    get_first_string(Map, Fields, <<>>).

%% Helper: get first matching string field
get_first_string(Map, Fields, Default) ->
    case get_first_value(Map, Fields) of
        undefined -> Default;
        Value when is_binary(Value) -> Value;
        Value when is_list(Value) -> list_to_binary(Value);
        Value when is_atom(Value) -> atom_to_binary(Value, utf8);
        _ -> Default
    end.

%% Helper: get first matching field value
get_first_value(_, []) ->
    undefined;
get_first_value(Map, [Field | Rest]) ->
    case maps:get(Field, Map, undefined) of
        undefined -> get_first_value(Map, Rest);
        Value -> Value
    end.
