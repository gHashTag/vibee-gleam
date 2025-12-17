-module(vibee_ffmpeg_ffi).
-export([
    concat_videos/2,
    add_audio/4,
    add_watermark/4,
    trim_video/4,
    get_video_info/1,
    extract_audio/2,
    convert_format/3
]).

%% Concatenate multiple video files
%% InputFiles: list of file paths
%% OutputPath: output file path
concat_videos(InputFiles, OutputPath) when is_list(InputFiles), is_binary(OutputPath) ->
    try
        % Create concat file list
        TempList = "/tmp/ffmpeg_concat_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".txt",
        ListContent = lists:foldl(fun(File, Acc) ->
            FileStr = case is_binary(File) of
                true -> binary_to_list(File);
                false -> File
            end,
            Acc ++ "file '" ++ FileStr ++ "'\n"
        end, "", InputFiles),
        ok = file:write_file(TempList, ListContent),

        % Run FFmpeg concat
        Cmd = io_lib:format(
            "ffmpeg -y -f concat -safe 0 -i ~s -c copy ~s 2>&1",
            [TempList, binary_to_list(OutputPath)]
        ),
        Result = os:cmd(lists:flatten(Cmd)),

        % Cleanup temp file
        file:delete(TempList),

        % Check if output exists
        case filelib:is_file(binary_to_list(OutputPath)) of
            true -> {ok, OutputPath};
            false -> {error, list_to_binary(Result)}
        end
    catch
        _:Error ->
            {error, list_to_binary(io_lib:format("FFmpeg concat error: ~p", [Error]))}
    end.

%% Add audio track to video
%% VideoPath: input video path
%% AudioPath: audio file path
%% OutputPath: output file path
%% Options: #{volume => 0.0-1.0, replace => true/false}
add_audio(VideoPath, AudioPath, OutputPath, Options)
  when is_binary(VideoPath), is_binary(AudioPath), is_binary(OutputPath) ->
    try
        Volume = maps:get(volume, Options, 1.0),
        Replace = maps:get(replace, Options, false),

        VolumeFilter = io_lib:format("volume=~.2f", [Volume]),

        Cmd = case Replace of
            true ->
                % Replace original audio completely
                io_lib:format(
                    "ffmpeg -y -i ~s -i ~s -c:v copy -map 0:v:0 -map 1:a:0 -af \"~s\" -shortest ~s 2>&1",
                    [binary_to_list(VideoPath), binary_to_list(AudioPath),
                     VolumeFilter, binary_to_list(OutputPath)]
                );
            false ->
                % Mix audio tracks
                io_lib:format(
                    "ffmpeg -y -i ~s -i ~s -filter_complex \"[1:a]~s[a1];[0:a][a1]amix=inputs=2:duration=first[aout]\" -map 0:v -map \"[aout]\" -c:v copy ~s 2>&1",
                    [binary_to_list(VideoPath), binary_to_list(AudioPath),
                     VolumeFilter, binary_to_list(OutputPath)]
                )
        end,

        Result = os:cmd(lists:flatten(Cmd)),

        case filelib:is_file(binary_to_list(OutputPath)) of
            true -> {ok, OutputPath};
            false -> {error, list_to_binary(Result)}
        end
    catch
        _:Error ->
            {error, list_to_binary(io_lib:format("FFmpeg add_audio error: ~p", [Error]))}
    end.

%% Add watermark image to video
%% VideoPath: input video path
%% WatermarkPath: watermark image path (PNG with transparency recommended)
%% OutputPath: output file path
%% Options: #{position => "topright"|"topleft"|"bottomright"|"bottomleft"|"center",
%%           opacity => 0.0-1.0, scale => 0.1-1.0, margin => pixels}
add_watermark(VideoPath, WatermarkPath, OutputPath, Options)
  when is_binary(VideoPath), is_binary(WatermarkPath), is_binary(OutputPath) ->
    try
        Position = maps:get(position, Options, <<"bottomright">>),
        Opacity = maps:get(opacity, Options, 0.7),
        Scale = maps:get(scale, Options, 0.15),
        Margin = maps:get(margin, Options, 20),

        % Calculate position overlay
        PosStr = case Position of
            <<"topleft">> ->
                io_lib:format("x=~p:y=~p", [Margin, Margin]);
            <<"topright">> ->
                io_lib:format("x=W-w-~p:y=~p", [Margin, Margin]);
            <<"bottomleft">> ->
                io_lib:format("x=~p:y=H-h-~p", [Margin, Margin]);
            <<"bottomright">> ->
                io_lib:format("x=W-w-~p:y=H-h-~p", [Margin, Margin]);
            <<"center">> ->
                "x=(W-w)/2:y=(H-h)/2";
            _ ->
                io_lib:format("x=W-w-~p:y=H-h-~p", [Margin, Margin])
        end,

        % Build filter complex
        FilterComplex = io_lib:format(
            "[1:v]scale=iw*~.2f:ih*~.2f,format=rgba,colorchannelmixer=aa=~.2f[wm];[0:v][wm]overlay=~s",
            [Scale, Scale, Opacity, PosStr]
        ),

        Cmd = io_lib:format(
            "ffmpeg -y -i ~s -i ~s -filter_complex \"~s\" -c:a copy ~s 2>&1",
            [binary_to_list(VideoPath), binary_to_list(WatermarkPath),
             FilterComplex, binary_to_list(OutputPath)]
        ),

        Result = os:cmd(lists:flatten(Cmd)),

        case filelib:is_file(binary_to_list(OutputPath)) of
            true -> {ok, OutputPath};
            false -> {error, list_to_binary(Result)}
        end
    catch
        _:Error ->
            {error, list_to_binary(io_lib:format("FFmpeg watermark error: ~p", [Error]))}
    end.

%% Trim video to specific duration
%% VideoPath: input video path
%% OutputPath: output file path
%% StartTime: start time in seconds or "HH:MM:SS" format
%% Duration: duration in seconds or "HH:MM:SS" format
trim_video(VideoPath, OutputPath, StartTime, Duration)
  when is_binary(VideoPath), is_binary(OutputPath) ->
    try
        StartStr = format_time(StartTime),
        DurationStr = format_time(Duration),

        Cmd = io_lib:format(
            "ffmpeg -y -i ~s -ss ~s -t ~s -c copy ~s 2>&1",
            [binary_to_list(VideoPath), StartStr, DurationStr, binary_to_list(OutputPath)]
        ),

        Result = os:cmd(lists:flatten(Cmd)),

        case filelib:is_file(binary_to_list(OutputPath)) of
            true -> {ok, OutputPath};
            false -> {error, list_to_binary(Result)}
        end
    catch
        _:Error ->
            {error, list_to_binary(io_lib:format("FFmpeg trim error: ~p", [Error]))}
    end.

%% Get video information (duration, resolution, codec, etc.)
get_video_info(VideoPath) when is_binary(VideoPath) ->
    try
        Cmd = io_lib:format(
            "ffprobe -v quiet -print_format json -show_format -show_streams ~s 2>&1",
            [binary_to_list(VideoPath)]
        ),
        Result = os:cmd(lists:flatten(Cmd)),
        {ok, list_to_binary(Result)}
    catch
        _:Error ->
            {error, list_to_binary(io_lib:format("FFprobe error: ~p", [Error]))}
    end.

%% Extract audio from video
extract_audio(VideoPath, OutputPath)
  when is_binary(VideoPath), is_binary(OutputPath) ->
    try
        Cmd = io_lib:format(
            "ffmpeg -y -i ~s -vn -acodec libmp3lame -q:a 2 ~s 2>&1",
            [binary_to_list(VideoPath), binary_to_list(OutputPath)]
        ),

        Result = os:cmd(lists:flatten(Cmd)),

        case filelib:is_file(binary_to_list(OutputPath)) of
            true -> {ok, OutputPath};
            false -> {error, list_to_binary(Result)}
        end
    catch
        _:Error ->
            {error, list_to_binary(io_lib:format("FFmpeg extract error: ~p", [Error]))}
    end.

%% Convert video format
%% Options: #{codec => "libx264", quality => "medium", resolution => "1080x1920"}
convert_format(VideoPath, OutputPath, Options)
  when is_binary(VideoPath), is_binary(OutputPath) ->
    try
        Codec = maps:get(codec, Options, <<"libx264">>),
        Quality = maps:get(quality, Options, <<"medium">>),
        Resolution = maps:get(resolution, Options, undefined),

        % Build codec options
        QualityPreset = case Quality of
            <<"fast">> -> "-preset fast -crf 23";
            <<"medium">> -> "-preset medium -crf 20";
            <<"high">> -> "-preset slow -crf 18";
            _ -> "-preset medium -crf 20"
        end,

        % Build resolution filter
        ScaleFilter = case Resolution of
            undefined -> "";
            Res ->
                ResStr = binary_to_list(Res),
                io_lib:format("-vf scale=~s", [ResStr])
        end,

        Cmd = io_lib:format(
            "ffmpeg -y -i ~s -c:v ~s ~s ~s -c:a aac ~s 2>&1",
            [binary_to_list(VideoPath), binary_to_list(Codec),
             QualityPreset, ScaleFilter, binary_to_list(OutputPath)]
        ),

        Result = os:cmd(lists:flatten(Cmd)),

        case filelib:is_file(binary_to_list(OutputPath)) of
            true -> {ok, OutputPath};
            false -> {error, list_to_binary(Result)}
        end
    catch
        _:Error ->
            {error, list_to_binary(io_lib:format("FFmpeg convert error: ~p", [Error]))}
    end.

%% Helper: format time value
format_time(Time) when is_integer(Time) ->
    integer_to_list(Time);
format_time(Time) when is_float(Time) ->
    io_lib:format("~.2f", [Time]);
format_time(Time) when is_binary(Time) ->
    binary_to_list(Time);
format_time(Time) when is_list(Time) ->
    Time.
