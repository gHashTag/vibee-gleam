-module(vibee_storage_ffi).
-export([upload_to_s3/3, list_assets/0, get_config/0, get_extension/1]).

%% Get storage configuration from environment
get_config() ->
    RenderServerUrl = get_env("RENDER_SERVER_URL", "https://vibee-remotion.fly.dev"),
    S3Endpoint = get_env("AWS_ENDPOINT_URL_S3", "https://fly.storage.tigris.dev"),
    S3Bucket = get_env("BUCKET_NAME", "vibee-assets"),
    #{
        render_server_url => list_to_binary(RenderServerUrl),
        s3_endpoint => list_to_binary(S3Endpoint),
        s3_bucket => list_to_binary(S3Bucket),
        s3_public_url => iolist_to_binary([S3Endpoint, "/", S3Bucket])
    }.

get_env(Key, Default) ->
    case os:getenv(Key) of
        false -> Default;
        "" -> Default;
        Value -> Value
    end.

%% Upload file to S3 via render-server proxy
upload_to_s3(FileData, Filename, ContentType) ->
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets),

    Config = get_config(),
    RenderServerUrl = maps:get(render_server_url, Config),
    UploadUrl = <<RenderServerUrl/binary, "/upload">>,

    %% Decode base64 if needed
    Data = try
        base64:decode(FileData)
    catch
        _:_ -> FileData
    end,

    Headers = [
        {"x-filename", binary_to_list(Filename)},
        {"content-type", binary_to_list(ContentType)}
    ],

    SslOpts = [{ssl, [{verify, verify_none}]}],
    Request = {binary_to_list(UploadUrl), Headers, binary_to_list(ContentType), Data},

    case httpc:request(post, Request, [{timeout, 120000} | SslOpts], [{body_format, binary}]) of
        {ok, {{_, StatusCode, _}, _, Body}} when StatusCode >= 200, StatusCode < 300 ->
            %% Parse JSON response manually
            case extract_json_field(Body, <<"url">>) of
                {ok, Url} ->
                    Key = case extract_json_field(Body, <<"key">>) of
                        {ok, K} -> K;
                        _ -> <<>>
                    end,
                    {ok, #{url => Url, key => Key}};
                _ ->
                    case extract_json_field(Body, <<"error">>) of
                        {ok, Error} -> {error, Error};
                        _ -> {ok, #{url => Body, key => <<>>}}
                    end
            end;
        {ok, {{_, StatusCode, _}, _, Body}} ->
            {error, iolist_to_binary([<<"HTTP ">>, integer_to_binary(StatusCode), <<": ">>, Body])};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Upload failed: ~p", [Reason]))}
    end.

%% List assets from S3 via render-server
list_assets() ->
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets),

    Config = get_config(),
    RenderServerUrl = maps:get(render_server_url, Config),
    AssetsUrl = <<RenderServerUrl/binary, "/assets">>,

    SslOpts = [{ssl, [{verify, verify_none}]}],

    case httpc:request(get, {binary_to_list(AssetsUrl), []}, [{timeout, 30000} | SslOpts], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            %% Return raw JSON for Gleam to parse
            {ok, Body};
        {ok, {{_, StatusCode, _}, _, Body}} ->
            {error, iolist_to_binary([<<"HTTP ">>, integer_to_binary(StatusCode), <<": ">>, Body])};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("List failed: ~p", [Reason]))}
    end.

%% Simple JSON field extractor (no dependencies)
extract_json_field(Json, Field) ->
    Pattern = <<"\"", Field/binary, "\":\"">>,
    case binary:match(Json, Pattern) of
        {Start, Len} ->
            ValueStart = Start + Len,
            RestJson = binary:part(Json, ValueStart, byte_size(Json) - ValueStart),
            case binary:match(RestJson, <<"\"">>) of
                {EndPos, _} ->
                    Value = binary:part(RestJson, 0, EndPos),
                    {ok, Value};
                nomatch ->
                    error
            end;
        nomatch ->
            error
    end.

%% Get file extension (lowercase)
get_extension(Filename) when is_binary(Filename) ->
    case filename:extension(binary_to_list(Filename)) of
        [] -> <<"">>;
        Ext -> list_to_binary(string:lowercase(Ext))
    end;
get_extension(Filename) when is_list(Filename) ->
    case filename:extension(Filename) of
        [] -> <<"">>;
        Ext -> list_to_binary(string:lowercase(Ext))
    end.
