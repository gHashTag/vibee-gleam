%% VIBEE Credentials FFI
%% Secure storage for exchange API keys in ETS

-module(vibee_credentials_ffi).
-export([init/0, store_credentials/5, get_credentials/1, has_credentials/1, remove_credentials/1, list_configured/0]).

-define(TABLE, vibee_credentials).

%% Initialize credentials ETS table
init() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set, {keypos, 1}]);
        _ -> ok
    end,
    nil.

%% Store credentials for an exchange
%% Credentials are stored as {exchange, api_key, api_secret, passphrase, enabled}
store_credentials(Exchange, ApiKey, ApiSecret, Passphrase, Enabled) ->
    init(),
    %% Basic validation
    case byte_size(ApiKey) > 0 andalso byte_size(ApiSecret) > 0 of
        true ->
            %% Store credentials (in production, should be encrypted)
            ets:insert(?TABLE, {Exchange, #{
                api_key => ApiKey,
                api_secret => ApiSecret,
                passphrase => Passphrase,
                enabled => Enabled,
                stored_at => erlang:system_time(second)
            }}),
            io:format("[CREDENTIALS] Stored credentials for ~s~n", [Exchange]),
            true;
        false ->
            false
    end.

%% Get credentials for an exchange
%% Returns {ok, {ApiKey, ApiSecret, Passphrase, Enabled}} or {error, "not_found"}
get_credentials(Exchange) ->
    init(),
    case ets:lookup(?TABLE, Exchange) of
        [{_, Creds}] ->
            ApiKey = maps:get(api_key, Creds, <<"">>),
            ApiSecret = maps:get(api_secret, Creds, <<"">>),
            Passphrase = maps:get(passphrase, Creds, <<"">>),
            Enabled = maps:get(enabled, Creds, false),
            {ok, {ApiKey, ApiSecret, Passphrase, Enabled}};
        [] ->
            {error, <<"not_found">>}
    end.

%% Check if exchange has credentials
has_credentials(Exchange) ->
    init(),
    case ets:lookup(?TABLE, Exchange) of
        [{_, _}] -> true;
        [] -> false
    end.

%% Remove credentials for an exchange
remove_credentials(Exchange) ->
    init(),
    case ets:lookup(?TABLE, Exchange) of
        [{_, _}] ->
            ets:delete(?TABLE, Exchange),
            io:format("[CREDENTIALS] Removed credentials for ~s~n", [Exchange]),
            true;
        [] ->
            false
    end.

%% List all configured exchanges
%% Returns list of PriceSource atoms
list_configured() ->
    init(),
    Exchanges = ets:foldl(fun({Exchange, Creds}, Acc) ->
        case maps:get(enabled, Creds, false) of
            true ->
                %% Convert exchange key to PriceSource atom
                Source = exchange_to_source(Exchange),
                [Source | Acc];
            false ->
                Acc
        end
    end, [], ?TABLE),
    Exchanges.

%% Convert exchange key to Gleam PriceSource
%% NOTE: Gleam compiles BinanceP2P -> binance_p2_p, OKXP2P -> o_k_x_p2_p
exchange_to_source(<<"binance">>) -> binance_p2_p;
exchange_to_source(<<"okx">>) -> o_k_x_p2_p;
exchange_to_source(<<"bybit">>) -> bybit_p2_p;
exchange_to_source(<<"cryptobot">>) -> crypto_bot;
exchange_to_source(<<"tonapi">>) -> ton_a_p_i;
exchange_to_source(<<"garantex">>) -> garantex;
exchange_to_source(<<"vibee">>) -> vibee_p2_p;
exchange_to_source(_) -> vibee_p2_p.
