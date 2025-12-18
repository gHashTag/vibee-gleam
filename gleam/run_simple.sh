#!/bin/bash
# Простой запуск без MCP сервера
export VIBEE_MODE=telegram
export DATABASE_URL=""
export OPENROUTER_API_KEY="${OPENROUTER_API_KEY:-dummy}"
export TELEGRAM_API_ID="${TELEGRAM_API_ID:-12345}"
export TELEGRAM_API_HASH="${TELEGRAM_API_HASH:-dummy}"

echo "Starting VIBEE in Telegram mode (no MCP server)..."
erl -pa build/dev/erlang/*/ebin -noshell -eval "
    io:format('VIBEE Agent Framework~n'),
    io:format('Mode: Telegram (simplified)~n'),
    io:format('Erlang/OTP: ~s~n', [erlang:system_info(otp_release)]),
    timer:sleep(2000),
    io:format('~nAgents would run here with proper configuration~n'),
    io:format('Current limitations:~n'),
    io:format('  - Erlang 25 (need 27 for full features)~n'),
    io:format('  - Missing env vars for Telegram~n'),
    io:format('  - Database not configured~n'),
    halt(0).
"
