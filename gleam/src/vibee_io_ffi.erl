-module(vibee_io_ffi).
-export([get_line/0, read_file/1]).

get_line() ->
    case io:get_line("") of
        eof -> {error, nil};
        {error, _} -> {error, nil};
        Line when is_list(Line) -> {ok, unicode:characters_to_binary(Line)};
        Line when is_binary(Line) -> {ok, Line}
    end.

read_file(Path) when is_binary(Path) ->
    case file:read_file(Path) of
        {ok, Content} -> {ok, Content};
        {error, _} -> {error, nil}
    end.
