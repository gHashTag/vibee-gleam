%% VIBEE Arbitrage Executor FFI
%% Bridge between Gleam tools and arbitrage execution

-module(vibee_arb_executor_ffi).
-export([execute_arbitrage/3]).

%% Execute arbitrage trade
%% Returns {ok, JsonResult} or {error, Reason}
execute_arbitrage(OpportunityId, Amount, DryRun) ->
    io:format("[ARB_EXEC] Executing arbitrage: ~s, amount: ~p, dry_run: ~p~n",
              [OpportunityId, Amount, DryRun]),

    %% Parse opportunity ID to extract details
    %% Format: ARB-{crypto}-{fiat}-{buy_source}-{sell_source}-{timestamp}
    case parse_opportunity_id(OpportunityId) of
        {error, Reason} ->
            {error, Reason};
        {ok, #{crypto := Crypto, fiat := Fiat, buy_source := BuySource,
               sell_source := SellSource, buy_price := BuyPrice, sell_price := SellPrice}} ->

            case DryRun of
                true ->
                    %% Simulate execution
                    simulate_execution(OpportunityId, Crypto, Fiat, BuySource, SellSource,
                                       BuyPrice, SellPrice, Amount);
                false ->
                    %% Real execution
                    real_execution(OpportunityId, Crypto, Fiat, BuySource, SellSource,
                                   BuyPrice, SellPrice, Amount)
            end
    end.

%% Parse opportunity ID - for real opportunities from scan
parse_opportunity_id(OpportunityId) ->
    %% For testing, generate mock data based on ID
    %% In production, look up from cache
    Now = erlang:system_time(second),
    {ok, #{
        crypto => <<"USDT">>,
        fiat => <<"THB">>,
        buy_source => <<"binance">>,
        sell_source => <<"vibee">>,
        buy_price => 34.5,
        sell_price => 35.2,
        opportunity_id => OpportunityId,
        detected_at => Now
    }}.

%% Simulate arbitrage execution (dry run)
simulate_execution(OpportunityId, Crypto, Fiat, BuySource, SellSource,
                   BuyPrice, SellPrice, Amount) ->
    io:format("[ARB_EXEC] DRY RUN: Would execute ~p ~s~n", [Amount, Crypto]),
    io:format("  Buy @ ~s: ~p ~s~n", [BuySource, BuyPrice, Fiat]),
    io:format("  Sell @ ~s: ~p ~s~n", [SellSource, SellPrice, Fiat]),

    GrossProfit = Amount * (SellPrice - BuyPrice),
    Fees = Amount * BuyPrice * 0.005 * 2,  % 0.5% fee per side
    NetProfit = GrossProfit - Fees,

    Timestamp = erlang:system_time(second),
    BuyOrderId = <<"DRY-BUY-", (integer_to_binary(Timestamp))/binary>>,
    SellOrderId = <<"DRY-SELL-", (integer_to_binary(Timestamp))/binary>>,

    Result = jsx:encode(#{
        <<"success">> => true,
        <<"dry_run">> => true,
        <<"opportunity_id">> => OpportunityId,
        <<"buy_order_id">> => BuyOrderId,
        <<"sell_order_id">> => SellOrderId,
        <<"buy_source">> => BuySource,
        <<"sell_source">> => SellSource,
        <<"buy_price">> => BuyPrice,
        <<"sell_price">> => SellPrice,
        <<"amount">> => Amount,
        <<"crypto">> => Crypto,
        <<"fiat">> => Fiat,
        <<"gross_profit">> => GrossProfit,
        <<"fees">> => Fees,
        <<"net_profit">> => NetProfit,
        <<"executed_at">> => Timestamp,
        <<"message">> => <<"Dry run completed successfully">>
    }),

    io:format("[ARB_EXEC] DRY RUN complete. Net profit: ~.2f ~s~n", [NetProfit, Fiat]),
    {ok, Result}.

%% Real arbitrage execution
real_execution(OpportunityId, Crypto, Fiat, BuySource, SellSource,
               BuyPrice, SellPrice, Amount) ->
    io:format("[ARB_EXEC] LIVE: Executing real arbitrage...~n"),

    %% Step 1: Place buy order
    case vibee_arb_ffi:place_order(BuySource, <<"BUY">>, Crypto, Fiat, Amount, BuyPrice) of
        {error, BuyError} ->
            ErrorResult = jsx:encode(#{
                <<"success">> => false,
                <<"error">> => <<"Buy order failed: ", BuyError/binary>>,
                <<"opportunity_id">> => OpportunityId
            }),
            {ok, ErrorResult};

        {ok, BuyOrderId} ->
            io:format("[ARB_EXEC] Buy order placed: ~s~n", [BuyOrderId]),

            %% Step 2: Place sell order
            case vibee_arb_ffi:place_order(SellSource, <<"SELL">>, Crypto, Fiat, Amount, SellPrice) of
                {error, SellError} ->
                    %% Sell failed - need to cancel buy order
                    io:format("[ARB_EXEC] Sell failed, buy succeeded. Manual intervention needed!~n"),
                    ErrorResult = jsx:encode(#{
                        <<"success">> => false,
                        <<"error">> => <<"Sell order failed (buy succeeded): ", SellError/binary>>,
                        <<"opportunity_id">> => OpportunityId,
                        <<"buy_order_id">> => BuyOrderId,
                        <<"warning">> => <<"Buy order succeeded but sell failed. Manual intervention required!">>
                    }),
                    {ok, ErrorResult};

                {ok, SellOrderId} ->
                    io:format("[ARB_EXEC] Sell order placed: ~s~n", [SellOrderId]),

                    GrossProfit = Amount * (SellPrice - BuyPrice),
                    Fees = Amount * BuyPrice * 0.005 * 2,
                    NetProfit = GrossProfit - Fees,
                    Timestamp = erlang:system_time(second),

                    SuccessResult = jsx:encode(#{
                        <<"success">> => true,
                        <<"dry_run">> => false,
                        <<"opportunity_id">> => OpportunityId,
                        <<"buy_order_id">> => BuyOrderId,
                        <<"sell_order_id">> => SellOrderId,
                        <<"buy_source">> => BuySource,
                        <<"sell_source">> => SellSource,
                        <<"buy_price">> => BuyPrice,
                        <<"sell_price">> => SellPrice,
                        <<"amount">> => Amount,
                        <<"crypto">> => Crypto,
                        <<"fiat">> => Fiat,
                        <<"gross_profit">> => GrossProfit,
                        <<"fees">> => Fees,
                        <<"net_profit">> => NetProfit,
                        <<"executed_at">> => Timestamp,
                        <<"message">> => <<"Arbitrage executed successfully!">>
                    }),

                    io:format("[ARB_EXEC] ✓ Trade complete! Net profit: ~.2f ~s~n", [NetProfit, Fiat]),
                    {ok, SuccessResult}
            end
    end.
