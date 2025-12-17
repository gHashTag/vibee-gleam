#!/bin/bash
set -e
echo "=== VIBEE MCP Server Starting ==="
echo "Contents of /app:"
ls -la /app/
echo ""
echo "Looking for beam files:"
find /app -name "mcp_ws_server.beam" 2>/dev/null || echo "Not found"
echo ""
echo "EBIN directories:"
find /app -type d -name ebin 2>/dev/null
echo ""

# Build -pa arguments for each ebin directory
PA_ARGS=""
for dir in $(find /app -type d -name ebin 2>/dev/null); do
    PA_ARGS="$PA_ARGS -pa $dir"
done

echo "Starting Erlang with code paths:"
echo "EBIN_DIRS: $PA_ARGS"

# Try loading module info before calling main
exec erl $PA_ARGS -noshell -eval "
    io:format(\"Testing module load...~n\"),
    case code:ensure_loaded(mcp_ws_server) of
        {module, _} ->
            io:format(\"Module loaded successfully~n\"),
            io:format(\"Calling main...~n\"),
            mcp_ws_server:main();
        {error, Reason} ->
            io:format(\"Failed to load module: ~p~n\", [Reason]),
            io:format(\"Code path: ~p~n\", [code:get_path()]),
            halt(1)
    end.
"
