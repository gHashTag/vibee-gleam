%% VIBEE Arbitrage Execution FFI
%% Handles real order placement on exchanges

-module(vibee_arb_ffi).
-export([place_order/6, get_balance/2, cancel_order/2]).

%% =============================================================================
%% ORDER PLACEMENT
%% =============================================================================

%% Place order on exchange
%% Returns {ok, OrderId} or {error, Reason}
place_order(Exchange, Side, Crypto, Fiat, Amount, Price) ->
    io:format("[ARB_FFI] Placing ~s order on ~s: ~p ~s @ ~p ~s~n",
              [Side, Exchange, Amount, Crypto, Price, Fiat]),

    %% Get credentials
    case vibee_credentials_ffi:get_credentials(Exchange) of
        {error, _} ->
            {error, <<"No credentials configured for ", Exchange/binary>>};
        {ok, {ApiKey, ApiSecret, Passphrase, Enabled}} ->
            case Enabled of
                false ->
                    {error, <<"Exchange ", Exchange/binary, " is disabled">>};
                true ->
                    %% Route to appropriate exchange handler
                    case Exchange of
                        <<"binance">> ->
                            place_binance_order(ApiKey, ApiSecret, Side, Crypto, Fiat, Amount, Price);
                        <<"okx">> ->
                            place_okx_order(ApiKey, ApiSecret, Passphrase, Side, Crypto, Fiat, Amount, Price);
                        <<"vibee">> ->
                            place_vibee_order(Side, Crypto, Fiat, Amount, Price);
                        _ ->
                            {error, <<"Unsupported exchange: ", Exchange/binary>>}
                    end
            end
    end.

%% =============================================================================
%% BINANCE SPOT API
%% =============================================================================

%% Place order on Binance Spot (not P2P - P2P requires manual interaction)
place_binance_order(ApiKey, ApiSecret, Side, Crypto, Fiat, Amount, Price) ->
    %% Binance Spot API endpoint
    BaseUrl = "https://api.binance.com/api/v3/order",

    %% Build symbol (e.g., TONUSDT, USDTTHB)
    Symbol = <<Crypto/binary, Fiat/binary>>,

    %% Build order parameters
    Timestamp = erlang:system_time(millisecond),
    Params = [
        {<<"symbol">>, Symbol},
        {<<"side">>, Side},
        {<<"type">>, <<"LIMIT">>},
        {<<"timeInForce">>, <<"GTC">>},
        {<<"quantity">>, float_to_binary(Amount, [{decimals, 8}])},
        {<<"price">>, float_to_binary(Price, [{decimals, 8}])},
        {<<"timestamp">>, integer_to_binary(Timestamp)}
    ],

    %% Create query string
    QueryString = build_query_string(Params),

    %% Sign the request
    Signature = hmac_sha256_hex(ApiSecret, QueryString),
    SignedQuery = <<QueryString/binary, "&signature=", Signature/binary>>,

    %% Make POST request
    Url = <<(list_to_binary(BaseUrl))/binary, "?", SignedQuery/binary>>,
    Headers = [{<<"X-MBX-APIKEY">>, ApiKey}],

    case http_post(Url, Headers, <<>>) of
        {ok, #{<<"orderId">> := OrderId}} ->
            OrderIdStr = integer_to_binary(OrderId),
            io:format("[BINANCE] ✓ Order placed: ~s~n", [OrderIdStr]),
            {ok, OrderIdStr};
        {ok, #{<<"msg">> := Msg}} ->
            io:format("[BINANCE] ✗ Order failed: ~s~n", [Msg]),
            {error, Msg};
        {error, Reason} ->
            io:format("[BINANCE] ✗ Request failed: ~p~n", [Reason]),
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% =============================================================================
%% OKX SPOT API
%% =============================================================================

place_okx_order(ApiKey, ApiSecret, Passphrase, Side, Crypto, Fiat, Amount, Price) ->
    %% OKX Trade API endpoint
    BaseUrl = "https://www.okx.com/api/v5/trade/order",

    %% Build instrument ID (e.g., TON-USDT)
    InstId = <<Crypto/binary, "-", Fiat/binary>>,

    %% Convert side
    OkxSide = case Side of
        <<"BUY">> -> <<"buy">>;
        <<"SELL">> -> <<"sell">>
    end,

    %% Build request body
    Body = jsx:encode(#{
        <<"instId">> => InstId,
        <<"tdMode">> => <<"cash">>,
        <<"side">> => OkxSide,
        <<"ordType">> => <<"limit">>,
        <<"sz">> => float_to_binary(Amount, [{decimals, 8}]),
        <<"px">> => float_to_binary(Price, [{decimals, 8}])
    }),

    %% Create signature
    Timestamp = iso8601_timestamp(),
    Method = <<"POST">>,
    RequestPath = <<"/api/v5/trade/order">>,
    PreSign = <<Timestamp/binary, Method/binary, RequestPath/binary, Body/binary>>,
    Signature = base64:encode(crypto:mac(hmac, sha256, ApiSecret, PreSign)),

    %% Headers
    Headers = [
        {<<"OK-ACCESS-KEY">>, ApiKey},
        {<<"OK-ACCESS-SIGN">>, Signature},
        {<<"OK-ACCESS-TIMESTAMP">>, Timestamp},
        {<<"OK-ACCESS-PASSPHRASE">>, Passphrase},
        {<<"Content-Type">>, <<"application/json">>}
    ],

    case http_post(list_to_binary(BaseUrl), Headers, Body) of
        {ok, #{<<"data">> := [#{<<"ordId">> := OrderId} | _]}} ->
            io:format("[OKX] ✓ Order placed: ~s~n", [OrderId]),
            {ok, OrderId};
        {ok, #{<<"msg">> := Msg}} ->
            io:format("[OKX] ✗ Order failed: ~s~n", [Msg]),
            {error, Msg};
        {error, Reason} ->
            io:format("[OKX] ✗ Request failed: ~p~n", [Reason]),
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% =============================================================================
%% VIBEE P2P (LOCAL)
%% =============================================================================

place_vibee_order(Side, Crypto, _Fiat, Amount, Price) ->
    %% Create order on VibeeP2P
    OrderId = vibee_p2p_ffi:generate_order_id(),
    Timestamp = erlang:system_time(second),

    OrderType = case Side of
        <<"BUY">> -> <<"buy">>;
        <<"SELL">> -> <<"sell">>
    end,

    Order = #{
        <<"id">> => OrderId,
        <<"seller_telegram_id">> => 144022504,  % Default user
        <<"buyer_telegram_id">> => null,
        <<"side">> => OrderType,
        <<"crypto">> => Crypto,
        <<"fiat">> => <<"RUB">>,
        <<"amount">> => Amount,
        <<"price">> => Price,
        <<"total_fiat">> => Amount * Price,
        <<"status">> => <<"open">>,
        <<"payment_method">> => <<"any">>,
        <<"created_at">> => Timestamp,
        <<"is_arb_order">> => true
    },

    case vibee_p2p_ffi:create_order(Order) of
        {ok, Id} ->
            io:format("[VIBEE] ✓ Order created: ~s~n", [Id]),
            {ok, Id};
        {error, Reason} ->
            io:format("[VIBEE] ✗ Order failed: ~p~n", [Reason]),
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% =============================================================================
%% BALANCE CHECK
%% =============================================================================

get_balance(Exchange, Asset) ->
    case vibee_credentials_ffi:get_credentials(Exchange) of
        {error, _} ->
            {error, <<"No credentials">>};
        {ok, {ApiKey, ApiSecret, Passphrase, _}} ->
            case Exchange of
                <<"binance">> ->
                    get_binance_balance(ApiKey, ApiSecret, Asset);
                <<"okx">> ->
                    get_okx_balance(ApiKey, ApiSecret, Passphrase, Asset);
                _ ->
                    {error, <<"Unsupported exchange">>}
            end
    end.

get_binance_balance(_ApiKey, _ApiSecret, _Asset) ->
    %% TODO: Implement Binance balance check
    {ok, 0.0}.

get_okx_balance(_ApiKey, _ApiSecret, _Passphrase, _Asset) ->
    %% TODO: Implement OKX balance check
    {ok, 0.0}.

%% =============================================================================
%% ORDER CANCELLATION
%% =============================================================================

cancel_order(Exchange, OrderId) ->
    io:format("[ARB_FFI] Cancelling order ~s on ~s~n", [OrderId, Exchange]),
    %% TODO: Implement order cancellation
    {ok, cancelled}.

%% =============================================================================
%% HELPERS
%% =============================================================================

build_query_string(Params) ->
    Parts = lists:map(fun({K, V}) ->
        <<K/binary, "=", V/binary>>
    end, Params),
    iolist_to_binary(lists:join(<<"&">>, Parts)).

hmac_sha256_hex(Secret, Data) ->
    Mac = crypto:mac(hmac, sha256, Secret, Data),
    bin_to_hex(Mac).

bin_to_hex(Bin) ->
    << <<(hex_char(N div 16)), (hex_char(N rem 16))>> || <<N>> <= Bin >>.

hex_char(N) when N < 10 -> $0 + N;
hex_char(N) -> $a + N - 10.

iso8601_timestamp() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.000Z",
                                   [Y, M, D, H, Mi, S])).

http_post(Url, Headers, Body) ->
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets),

    UrlStr = binary_to_list(Url),
    HeaderList = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],
    BodyBin = case Body of
        B when is_binary(B) -> B;
        _ -> <<>>
    end,

    SslOpts = [{ssl, [{verify, verify_none}]}],

    case httpc:request(post,
                       {UrlStr, HeaderList, "application/json", BodyBin},
                       [{timeout, 30000} | SslOpts],
                       [{body_format, binary}]) of
        {ok, {{_, StatusCode, _}, _, RespBody}} when StatusCode >= 200, StatusCode < 300 ->
            try
                {ok, jsx:decode(RespBody, [return_maps])}
            catch
                _:_ -> {ok, #{<<"raw">> => RespBody}}
            end;
        {ok, {{_, StatusCode, _}, _, RespBody}} ->
            try
                {ok, jsx:decode(RespBody, [return_maps])}
            catch
                _:_ -> {error, {StatusCode, RespBody}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
