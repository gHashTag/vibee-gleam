%% VIBEE Earning FFI
%% Erlang FFI for Gleam earning module

-module(vibee_earning_ffi).
-export([start_earning/5, stop_earning/1, get_status/1, get_stats/1, update_strategy/2]).
-export([get_active_config/0, get_wallet/1]).
-export([get_last_alert_time/1, set_last_alert_time/2]).
-export([set_maker_enabled/2, is_maker_enabled/1]).
-export([init/0]).

%% ETS table for storing earning configs and state
-define(CONFIG_TABLE, vibee_earning_configs).
-define(STATS_TABLE, vibee_earning_stats).
-define(ALERTS_TABLE, vibee_alert_cooldowns).

%% Initialize ETS tables
init() ->
    case ets:whereis(?CONFIG_TABLE) of
        undefined ->
            ets:new(?CONFIG_TABLE, [named_table, public, set, {keypos, 1}]);
        _ -> ok
    end,
    case ets:whereis(?STATS_TABLE) of
        undefined ->
            ets:new(?STATS_TABLE, [named_table, public, set, {keypos, 1}]);
        _ -> ok
    end,
    case ets:whereis(?ALERTS_TABLE) of
        undefined ->
            ets:new(?ALERTS_TABLE, [named_table, public, set, {keypos, 1}]);
        _ -> ok
    end,
    ok.

%% Start earning for a user
-spec start_earning(integer(), binary(), binary(), float(), float()) -> boolean().
start_earning(TelegramId, Wallet, Strategy, MaxPosition, Spread) ->
    init(),
    Now = erlang:system_time(second),

    Config = #{
        telegram_id => TelegramId,
        wallet => Wallet,
        strategy => binary_to_atom(Strategy, utf8),
        max_position_usdt => MaxPosition,
        spread_percent => Spread,
        is_active => true,
        started_at => Now,
        updated_at => Now
    },

    %% Check if already active
    case ets:lookup(?CONFIG_TABLE, TelegramId) of
        [{_, #{is_active := true}}] ->
            false;  % Already active
        _ ->
            ets:insert(?CONFIG_TABLE, {TelegramId, Config}),

            %% Initialize stats if not exists
            case ets:lookup(?STATS_TABLE, TelegramId) of
                [] ->
                    ets:insert(?STATS_TABLE, {TelegramId, initial_stats()});
                _ -> ok
            end,

            %% Try to start worker if available
            try
                vibee_earning_worker:start_earning(TelegramId, Config)
            catch
                _:_ -> ok  % Worker not started, still store config
            end,

            io:format("[EarningFFI] Started earning for user ~p with strategy ~s~n",
                      [TelegramId, Strategy]),
            true
    end.

%% Update strategy for a user
-spec update_strategy(integer(), binary()) -> boolean().
update_strategy(TelegramId, Strategy) ->
    init(),
    case ets:lookup(?CONFIG_TABLE, TelegramId) of
        [{_, Config}] ->
            UpdatedConfig = Config#{
                strategy => binary_to_atom(Strategy, utf8),
                updated_at => erlang:system_time(second)
            },
            ets:insert(?CONFIG_TABLE, {TelegramId, UpdatedConfig}),

            %% Update worker if running
            try
                vibee_earning_worker:start_earning(TelegramId, UpdatedConfig)
            catch
                _:_ -> ok
            end,

            io:format("[EarningFFI] Updated strategy for user ~p to ~s~n",
                      [TelegramId, Strategy]),
            true;
        [] ->
            false
    end.

%% Stop earning for a user
-spec stop_earning(integer()) -> boolean().
stop_earning(TelegramId) ->
    init(),
    case ets:lookup(?CONFIG_TABLE, TelegramId) of
        [{_, Config}] ->
            UpdatedConfig = Config#{is_active => false, updated_at => erlang:system_time(second)},
            ets:insert(?CONFIG_TABLE, {TelegramId, UpdatedConfig}),

            %% Try to stop worker
            try
                vibee_earning_worker:stop_earning(TelegramId)
            catch
                _:_ -> ok
            end,

            io:format("[EarningFFI] Stopped earning for user ~p~n", [TelegramId]),
            true;
        [] ->
            false
    end.

%% Get status for a user (returns Gleam AgentStatus record)
-spec get_status(integer()) -> tuple().
get_status(TelegramId) ->
    init(),
    Now = erlang:system_time(second),

    case ets:lookup(?CONFIG_TABLE, TelegramId) of
        [{_, Config}] ->
            IsActive = maps:get(is_active, Config, false),
            Strategy = maps:get(strategy, Config, passive_fees),
            StartedAt = maps:get(started_at, Config, Now),
            Uptime = case IsActive of
                true -> Now - StartedAt;
                false -> 0
            end,

            %% Count active orders from P2P FFI
            ActiveOrders = count_active_orders(TelegramId),

            %% Return Gleam AgentStatus record
            %% AgentStatus(telegram_id, is_active, strategy, uptime_seconds, active_orders,
            %%             pending_arbitrage, current_position_usdt, current_position_ton,
            %%             trades_today, profit_today, last_error, error_count,
            %%             started_at, last_activity_at)
            {agent_status,
             TelegramId,
             IsActive,
             Strategy,
             Uptime,
             ActiveOrders,  % active_orders from P2P
             0,  % pending_arbitrage
             0.0,  % current_position_usdt
             0.0,  % current_position_ton
             0,  % trades_today
             0.0,  % profit_today
             none,  % last_error
             0,  % error_count
             StartedAt,
             Now  % last_activity_at
            };
        [] ->
            %% Return inactive status
            {agent_status,
             TelegramId,
             false,
             passive_fees,
             0, 0, 0, 0.0, 0.0, 0, 0.0, none, 0, 0, 0}
    end.

%% Get stats for a user (returns Gleam EarningStats record)
-spec get_stats(integer()) -> tuple().
get_stats(TelegramId) ->
    init(),
    case ets:lookup(?STATS_TABLE, TelegramId) of
        [{_, Stats}] ->
            %% Return Gleam EarningStats record
            %% EarningStats(telegram_id, total_trades, successful_trades, failed_trades,
            %%              total_volume_usdt, total_fees_earned, total_spread_profit,
            %%              total_arbitrage_profit, today_profit, this_week_profit,
            %%              this_month_profit, best_trade_profit, avg_trade_profit, updated_at)
            {earning_stats,
             TelegramId,
             maps:get(total_trades, Stats, 0),
             maps:get(successful_trades, Stats, 0),
             maps:get(failed_trades, Stats, 0),
             maps:get(total_volume_usdt, Stats, 0.0),
             maps:get(total_fees_earned, Stats, 0.0),
             maps:get(total_spread_profit, Stats, 0.0),
             maps:get(total_arbitrage_profit, Stats, 0.0),
             maps:get(today_profit, Stats, 0.0),
             maps:get(this_week_profit, Stats, 0.0),
             maps:get(this_month_profit, Stats, 0.0),
             maps:get(best_trade_profit, Stats, 0.0),
             maps:get(avg_trade_profit, Stats, 0.0),
             maps:get(updated_at, Stats, erlang:system_time(second))
            };
        [] ->
            %% Return empty stats
            {earning_stats,
             TelegramId,
             0, 0, 0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
             erlang:system_time(second)}
    end.

%% Internal: initial stats
initial_stats() ->
    #{
        total_trades => 0,
        successful_trades => 0,
        failed_trades => 0,
        total_volume_usdt => 0.0,
        total_fees_earned => 0.0,
        total_spread_profit => 0.0,
        total_arbitrage_profit => 0.0,
        today_profit => 0.0,
        this_week_profit => 0.0,
        this_month_profit => 0.0,
        best_trade_profit => 0.0,
        avg_trade_profit => 0.0,
        updated_at => erlang:system_time(second)
    }.

%% Get currently active config (first active user)
%% Returns {ok, {TelegramId, Wallet}} or {error, nil} - Gleam Result compatible
-spec get_active_config() -> {ok, {integer(), binary()}} | {error, nil}.
get_active_config() ->
    init(),
    %% Find any active config
    case ets:foldl(fun({TelegramId, Config}, Acc) ->
        case Acc of
            none ->
                case maps:get(is_active, Config, false) of
                    true ->
                        Wallet = maps:get(wallet, Config, <<"">>),
                        {found, TelegramId, Wallet};
                    false -> none
                end;
            Found -> Found
        end
    end, none, ?CONFIG_TABLE) of
        {found, TelegramId, Wallet} -> {ok, {TelegramId, Wallet}};
        _ -> {error, nil}
    end.

%% Get wallet for a user
-spec get_wallet(integer()) -> {some, binary()} | none.
get_wallet(TelegramId) ->
    init(),
    case ets:lookup(?CONFIG_TABLE, TelegramId) of
        [{_, Config}] ->
            case maps:get(wallet, Config, undefined) of
                undefined -> none;
                Wallet -> {some, Wallet}
            end;
        [] -> none
    end.

%% =============================================================================
%% ALERT COOLDOWN TRACKING
%% =============================================================================

%% Get last alert time for a key (returns 0 if not found)
-spec get_last_alert_time(binary()) -> integer().
get_last_alert_time(Key) ->
    init(),
    case ets:lookup(?ALERTS_TABLE, Key) of
        [{_, Timestamp}] -> Timestamp;
        [] -> 0
    end.

%% Set last alert time for a key
-spec set_last_alert_time(binary(), integer()) -> nil.
set_last_alert_time(Key, Timestamp) ->
    init(),
    ets:insert(?ALERTS_TABLE, {Key, Timestamp}),
    nil.

%% =============================================================================
%% MAKER BOT TOGGLE
%% =============================================================================

%% Set maker bot enabled/disabled for a user
-spec set_maker_enabled(integer(), boolean()) -> nil.
set_maker_enabled(TelegramId, Enabled) ->
    init(),
    case ets:lookup(?CONFIG_TABLE, TelegramId) of
        [{_, Config}] ->
            UpdatedConfig = Config#{
                maker_enabled => Enabled,
                updated_at => erlang:system_time(second)
            },
            ets:insert(?CONFIG_TABLE, {TelegramId, UpdatedConfig});
        [] ->
            %% Create minimal config with maker_enabled
            Config = #{
                telegram_id => TelegramId,
                maker_enabled => Enabled,
                is_active => false,
                strategy => passive_fees,
                updated_at => erlang:system_time(second)
            },
            ets:insert(?CONFIG_TABLE, {TelegramId, Config})
    end,
    io:format("[EarningFFI] Set maker_enabled=~p for user ~p~n", [Enabled, TelegramId]),
    nil.

%% Check if maker bot is enabled for a user
-spec is_maker_enabled(integer()) -> boolean().
is_maker_enabled(TelegramId) ->
    init(),
    case ets:lookup(?CONFIG_TABLE, TelegramId) of
        [{_, Config}] ->
            maps:get(maker_enabled, Config, false);
        [] ->
            false
    end.

%% Count active orders for a user from P2P ETS
-spec count_active_orders(integer()) -> integer().
count_active_orders(TelegramId) ->
    try
        %% Get orders by seller (maker bot creates sell orders)
        SellerOrders = vibee_p2p_ffi:list_orders_by_seller(TelegramId),
        %% Count only open/active/pending orders
        ActiveSeller = lists:filter(fun(Order) ->
            Status = maps:get(<<"status">>, Order, <<>>),
            Status =:= <<"open">> orelse Status =:= <<"active">> orelse Status =:= <<"pending">>
        end, SellerOrders),

        %% Also count buy orders (buyer side)
        BuyerOrders = vibee_p2p_ffi:list_orders_by_buyer(TelegramId),
        ActiveBuyer = lists:filter(fun(Order) ->
            Status = maps:get(<<"status">>, Order, <<>>),
            Status =:= <<"open">> orelse Status =:= <<"active">> orelse Status =:= <<"pending">>
        end, BuyerOrders),

        %% Also check all orders where user is involved
        AllOrders = vibee_p2p_ffi:list_orders(100),
        UserOrders = lists:filter(fun(Order) ->
            SellerId = maps:get(<<"seller_telegram_id">>, Order, 0),
            BuyerId = maps:get(<<"buyer_telegram_id">>, Order, 0),
            Status = maps:get(<<"status">>, Order, <<>>),
            (SellerId =:= TelegramId orelse BuyerId =:= TelegramId) andalso
            (Status =:= <<"open">> orelse Status =:= <<"active">> orelse Status =:= <<"pending">>)
        end, AllOrders),

        length(UserOrders)
    catch
        _:_ -> 0
    end.
