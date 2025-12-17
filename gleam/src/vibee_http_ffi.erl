-module(vibee_http_ffi).
-export([get/1, get_with_headers/2, post/3, post_json/2, clone_voice/4, download_file/1]).

%% HTTP GET request (no headers)
get(Url) ->
    get_with_headers(Url, []).

%% HTTP GET request with headers
get_with_headers(Url, Headers) ->
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets),
    UrlStr = binary_to_list(Url),
    HeadersList = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],
    SslOpts = [{ssl, [{verify, verify_none}]}],
    case httpc:request(get, {UrlStr, HeadersList}, [{timeout, 30000} | SslOpts], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, Body};
        {ok, {{_, StatusCode, _}, _, Body}} ->
            {error, iolist_to_binary([<<"HTTP ">>, integer_to_binary(StatusCode), <<": ">>, Body])};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% HTTP POST request with headers
post(Url, Body, Headers) ->
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets),
    UrlStr = binary_to_list(Url),
    HeadersList = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],
    ContentType = proplists:get_value("content-type", HeadersList, "application/json"),
    FinalHeaders = [{K, V} || {K, V} <- HeadersList, K =/= "content-type"],
    BodyBin = if is_binary(Body) -> Body; true -> list_to_binary(Body) end,
    SslOpts = [{ssl, [{verify, verify_none}]}],
    case httpc:request(post, {UrlStr, FinalHeaders, ContentType, BodyBin}, [{timeout, 30000} | SslOpts], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            {ok, RespBody};
        {ok, {{_, 201, _}, _, RespBody}} ->
            {ok, RespBody};
        {ok, {{_, StatusCode, _}, _, RespBody}} ->
            {error, iolist_to_binary([<<"HTTP ">>, integer_to_binary(StatusCode), <<": ">>, RespBody])};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Simple JSON POST (for Telegram Bot API)
post_json(Url, Body) ->
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets),
    UrlStr = binary_to_list(Url),
    BodyBin = if is_binary(Body) -> Body; true -> list_to_binary(Body) end,
    SslOpts = [{ssl, [{verify, verify_none}]}],
    case httpc:request(post, {UrlStr, [], "application/json", BodyBin}, [{timeout, 30000} | SslOpts], [{body_format, binary}]) of
        {ok, {{_, StatusCode, _}, _, RespBody}} when StatusCode >= 200, StatusCode < 300 ->
            {ok, RespBody};
        {ok, {{_, StatusCode, _}, _, RespBody}} ->
            {error, iolist_to_binary([<<"HTTP ">>, integer_to_binary(StatusCode), <<": ">>, RespBody])};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Download file from URL (for voice cloning)
download_file(Url) ->
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets),
    UrlStr = binary_to_list(Url),
    SslOpts = [{ssl, [{verify, verify_none}]}],
    case httpc:request(get, {UrlStr, []}, [{timeout, 60000} | SslOpts], [{body_format, binary}]) of
        {ok, {{_, StatusCode, _}, _, Body}} when StatusCode >= 200, StatusCode < 300 ->
            {ok, Body};
        {ok, {{_, StatusCode, _}, _, Body}} ->
            {error, iolist_to_binary([<<"HTTP ">>, integer_to_binary(StatusCode), <<": ">>, Body])};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Clone voice using ElevenLabs API with multipart/form-data
%% ApiKey: ElevenLabs API key
%% Name: Voice name
%% Description: Voice description
%% AudioData: Binary audio data
clone_voice(ApiKey, Name, Description, AudioData) ->
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets),
    Url = "https://api.elevenlabs.io/v1/voices/add",
    Boundary = <<"----VibeeVoiceClone", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    %% Build multipart body
    Body = build_multipart_body(Boundary, [
        {<<"name">>, Name},
        {<<"description">>, Description},
        {<<"files">>, AudioData, <<"audio.mp3">>, <<"audio/mpeg">>}
    ]),

    ContentType = "multipart/form-data; boundary=" ++ binary_to_list(Boundary),
    Headers = [{"xi-api-key", binary_to_list(ApiKey)}],
    SslOpts = [{ssl, [{verify, verify_none}]}],

    case httpc:request(post, {Url, Headers, ContentType, Body}, [{timeout, 120000} | SslOpts], [{body_format, binary}]) of
        {ok, {{_, StatusCode, _}, _, RespBody}} when StatusCode >= 200, StatusCode < 300 ->
            {ok, RespBody};
        {ok, {{_, StatusCode, _}, _, RespBody}} ->
            {error, iolist_to_binary([<<"HTTP ">>, integer_to_binary(StatusCode), <<": ">>, RespBody])};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Build multipart/form-data body
build_multipart_body(Boundary, Parts) ->
    iolist_to_binary([build_parts(Boundary, Parts), <<"--">>, Boundary, <<"--\r\n">>]).

build_parts(_Boundary, []) ->
    [];
build_parts(Boundary, [{Name, Value} | Rest]) ->
    %% Text field
    Part = [
        <<"--">>, Boundary, <<"\r\n">>,
        <<"Content-Disposition: form-data; name=\"">>, Name, <<"\"\r\n\r\n">>,
        Value, <<"\r\n">>
    ],
    [Part | build_parts(Boundary, Rest)];
build_parts(Boundary, [{Name, FileData, FileName, ContentType} | Rest]) ->
    %% File field
    Part = [
        <<"--">>, Boundary, <<"\r\n">>,
        <<"Content-Disposition: form-data; name=\"">>, Name, <<"\"; filename=\"">>, FileName, <<"\"\r\n">>,
        <<"Content-Type: ">>, ContentType, <<"\r\n\r\n">>,
        FileData, <<"\r\n">>
    ],
    [Part | build_parts(Boundary, Rest)].
