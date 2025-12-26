%% VIBEE P2P Earning Worker
%% Background GenServer for automated P2P trading

-module(vibee_earning_worker).
-behaviour(gen_server).

%% API
-export([start_link/0, start_earning/2, stop_earning/1, get_status/1]).
-export([get_stats/1, scan_arbitrage/2]).
-export([get_activity_log/0, get_activity_log/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Intervals in milliseconds
-define(PRICE_CHECK_INTERVAL, 30000).     % 30 seconds
-define(ORDER_CHECK_INTERVAL, 60000).     % 1 minute
-define(ARBITRAGE_SCAN_INTERVAL, 120000). % 2 minutes
-define(STATS_UPDATE_INTERVAL, 300000).   % 5 minutes

%% State record
-record(state, {
    active_users = #{} :: #{integer() => user_state()},
    price_cache = #{} :: #{binary() => price_data()},
    arbitrage_cache = [] :: [arbitrage_opportunity()],
    stats = #{} :: #{integer() => earning_stats()}
}).

-type user_state() :: #{
    telegram_id := integer(),
    wallet := binary(),
    strategy := atom(),
    config := map(),
    started_at := integer(),
    active_orders := [binary()],
    is_active := boolean()
}.

-type price_data() :: #{
    crypto := atom(),
    fiat := atom(),
    price := float(),
    updated_at := integer()
}.

-type arbitrage_opportunity() :: #{
    id := binary(),
    crypto := atom(),
    fiat := atom(),
    buy_source := atom(),
    buy_price := float(),
    sell_source := atom(),
    sell_price := float(),
    spread_percent := float(),
    detected_at := integer()
}.

-type earning_stats() :: #{
    total_trades := integer(),
    total_profit := float(),
    today_profit := float()
}.

%% =============================================================================
%% API Functions
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Start earning for a user
-spec start_earning(integer(), map()) -> {ok, started} | {error, term()}.
start_earning(TelegramId, Config) ->
    gen_server:call(?MODULE, {start_earning, TelegramId, Config}).

%% Stop earning for a user
-spec stop_earning(integer()) -> {ok, stopped} | {error, term()}.
stop_earning(TelegramId) ->
    gen_server:call(?MODULE, {stop_earning, TelegramId}).

%% Get current status for a user
-spec get_status(integer()) -> {ok, map()} | {error, not_found}.
get_status(TelegramId) ->
    gen_server:call(?MODULE, {get_status, TelegramId}).

%% Get earning stats for a user
-spec get_stats(integer()) -> {ok, earning_stats()} | {error, not_found}.
get_stats(TelegramId) ->
    gen_server:call(?MODULE, {get_stats, TelegramId}).

%% Manually trigger arbitrage scan
-spec scan_arbitrage(atom(), atom()) -> {ok, [arbitrage_opportunity()]}.
scan_arbitrage(Crypto, Fiat) ->
    gen_server:call(?MODULE, {scan_arbitrage, Crypto, Fiat}).

%% =============================================================================
%% gen_server Callbacks
%% =============================================================================

init([]) ->
    %% Schedule periodic tasks
    erlang:send_after(?PRICE_CHECK_INTERVAL, self(), check_prices),
    erlang:send_after(?ORDER_CHECK_INTERVAL, self(), check_orders),
    erlang:send_after(?ARBITRAGE_SCAN_INTERVAL, self(), scan_arbitrage),
    erlang:send_after(?STATS_UPDATE_INTERVAL, self(), update_stats),

    io:format("[EarningWorker] Started~n"),

    %% Auto-enable earning for default user if VIBEE_AUTO_EARN env is set
    State = case os:getenv("VIBEE_AUTO_EARN") of
        false -> #state{};
        "" -> #state{};
        TelegramIdStr ->
            try
                TelegramId = list_to_integer(TelegramIdStr),
                Wallet = case os:getenv("VIBEE_AUTO_WALLET") of
                    false -> <<>>;
                    "" -> <<>>;
                    W -> list_to_binary(W)
                end,
                Strategy = case os:getenv("VIBEE_AUTO_STRATEGY") of
                    false -> market_making;
                    "" -> market_making;
                    "passive_fees" -> passive_fees;
                    "arbitrage" -> arbitrage;
                    "hybrid" -> hybrid;
                    _ -> market_making
                end,
                Now = erlang:system_time(second),
                UserState = #{
                    telegram_id => TelegramId,
                    wallet => Wallet,
                    strategy => Strategy,
                    config => #{},
                    started_at => Now,
                    active_orders => [],
                    is_active => true
                },
                %% Also enable maker bot for this user
                vibee_earning_ffi:set_maker_enabled(TelegramId, true),
                io:format("[EarningWorker] ✓ Auto-enabled earning for user ~p with strategy ~p~n",
                          [TelegramId, Strategy]),
                %% Log to activity log
                log_system(io_lib:format("Agent started with ~s strategy", [Strategy])),
                #state{
                    active_users = #{TelegramId => UserState},
                    stats = #{TelegramId => initial_stats()}
                }
            catch
                _:_ ->
                    io:format("[EarningWorker] Failed to auto-enable earning~n"),
                    #state{}
            end
    end,
    {ok, State}.

handle_call({start_earning, TelegramId, Config}, _From, State) ->
    case maps:is_key(TelegramId, State#state.active_users) of
        true ->
            {reply, {error, already_active}, State};
        false ->
            Now = erlang:system_time(second),
            Strategy = maps:get(strategy, Config, passive_fees),
            UserState = #{
                telegram_id => TelegramId,
                wallet => maps:get(wallet, Config, <<>>),
                strategy => Strategy,
                config => Config,
                started_at => Now,
                active_orders => [],
                is_active => true
            },
            NewUsers = maps:put(TelegramId, UserState, State#state.active_users),
            NewStats = maps:put(TelegramId, initial_stats(), State#state.stats),
            io:format("[EarningWorker] Started earning for user ~p~n", [TelegramId]),
            log_system(io_lib:format("Earning started for user ~p with ~p strategy", [TelegramId, Strategy])),
            {reply, {ok, started}, State#state{active_users = NewUsers, stats = NewStats}}
    end;

handle_call({stop_earning, TelegramId}, _From, State) ->
    case maps:take(TelegramId, State#state.active_users) of
        {_UserState, NewUsers} ->
            io:format("[EarningWorker] Stopped earning for user ~p~n", [TelegramId]),
            {reply, {ok, stopped}, State#state{active_users = NewUsers}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_status, TelegramId}, _From, State) ->
    case maps:get(TelegramId, State#state.active_users, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        UserState ->
            Now = erlang:system_time(second),
            StartedAt = maps:get(started_at, UserState),
            Status = #{
                is_active => maps:get(is_active, UserState),
                strategy => maps:get(strategy, UserState),
                uptime_seconds => Now - StartedAt,
                active_orders => length(maps:get(active_orders, UserState)),
                wallet => maps:get(wallet, UserState)
            },
            {reply, {ok, Status}, State}
    end;

handle_call({get_stats, TelegramId}, _From, State) ->
    case maps:get(TelegramId, State#state.stats, undefined) of
        undefined ->
            {reply, {ok, initial_stats()}, State};
        Stats ->
            {reply, {ok, Stats}, State}
    end;

handle_call({scan_arbitrage, Crypto, Fiat}, _From, State) ->
    %% Perform arbitrage scan
    Opportunities = do_scan_arbitrage(Crypto, Fiat, State#state.price_cache),
    {reply, {ok, Opportunities}, State#state{arbitrage_cache = Opportunities}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_prices, State) ->
    %% Update price cache
    spawn(fun() -> update_price_cache() end),
    erlang:send_after(?PRICE_CHECK_INTERVAL, self(), check_prices),
    {noreply, State};

handle_info(check_orders, State) ->
    %% Check and manage active orders for all users
    NewState = process_active_orders(State),
    erlang:send_after(?ORDER_CHECK_INTERVAL, self(), check_orders),
    {noreply, NewState};

handle_info(scan_arbitrage, State) ->
    %% Scan for REAL arbitrage opportunities across all pairs
    %% and send Telegram notifications
    spawn(fun() ->
        scan_and_notify_all_pairs()
    end),
    erlang:send_after(?ARBITRAGE_SCAN_INTERVAL, self(), scan_arbitrage),
    {noreply, State};

handle_info(update_stats, State) ->
    %% Update all user stats
    NewStats = update_all_stats(State#state.active_users, State#state.stats),
    erlang:send_after(?STATS_UPDATE_INTERVAL, self(), update_stats),
    {noreply, State#state{stats = NewStats}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[EarningWorker] Terminated~n"),
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================

initial_stats() ->
    #{
        total_trades => 0,
        successful_trades => 0,
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

update_price_cache() ->
    %% Call Gleam price_feed module to get current prices
    %% For now, we'll just log
    io:format("[EarningWorker] Price cache updated~n").

process_active_orders(State) ->
    %% Process orders for each active user
    maps:fold(fun(TelegramId, UserState, AccState) ->
        case maps:get(is_active, UserState) of
            true ->
                Strategy = maps:get(strategy, UserState),
                process_user_strategy(TelegramId, Strategy, AccState);
            false ->
                AccState
        end
    end, State, State#state.active_users).

process_user_strategy(TelegramId, passive_fees, State) ->
    %% Passive fees - check if maker bot is enabled via toggle
    case is_maker_bot_enabled(TelegramId) of
        true ->
            io:format("[EarningWorker] Maker bot enabled for passive_fees user ~p~n", [TelegramId]),
            %% Run maker bot logic same as market_making
            Now = erlang:system_time(second),
            LastUpdate = get_last_maker_update(TelegramId),
            case Now - LastUpdate > 300 of
                true ->
                    io:format("[EarningWorker] Refreshing maker orders for passive_fees user ~p~n", [TelegramId]),
                    run_maker_bot(TelegramId),
                    set_last_maker_update(TelegramId, Now);
                false ->
                    ok
            end;
        false ->
            ok
    end,
    State;

process_user_strategy(TelegramId, market_making, State) ->
    %% Active market making - place buy/sell orders with spread
    io:format("[EarningWorker] Processing market making for user ~p~n", [TelegramId]),
    log_system(io_lib:format("Processing market making for user ~p", [TelegramId])),

    %% Check if we need to refresh orders (every 5 minutes)
    Now = erlang:system_time(second),
    LastUpdate = get_last_maker_update(TelegramId),

    case Now - LastUpdate > 300 of
        true ->
            %% Time to refresh orders
            io:format("[EarningWorker] Refreshing maker orders for user ~p~n", [TelegramId]),
            run_maker_bot(TelegramId),
            set_last_maker_update(TelegramId, Now);
        false ->
            ok
    end,
    State;

process_user_strategy(TelegramId, arbitrage, State) ->
    %% Check for arbitrage opportunities and execute if profitable
    case State#state.arbitrage_cache of
        [] ->
            State;
        [Opp | _] ->
            SpreadPercent = maps:get(spread_percent, Opp),
            case SpreadPercent > 1.5 of
                true ->
                    io:format("[EarningWorker] Found arbitrage opportunity for user ~p: ~.2f%~n",
                              [TelegramId, SpreadPercent]),
                    %% TODO: Execute arbitrage trade
                    State;
                false ->
                    State
            end
    end;

process_user_strategy(TelegramId, hybrid, State) ->
    %% Combine all strategies
    State1 = process_user_strategy(TelegramId, market_making, State),
    process_user_strategy(TelegramId, arbitrage, State1).

do_scan_arbitrage(Crypto, Fiat, _PriceCache) ->
    %% Simulate arbitrage scanning between platforms
    Now = erlang:system_time(second),

    %% Generate some sample opportunities based on current timestamp
    %% In production, this would fetch real prices from multiple sources
    BasePrice = case {Crypto, Fiat} of
        {usdt, thb} -> 35.0;
        {usdt, rub} -> 100.0;
        {ton, thb} -> 192.0;
        {ton, rub} -> 550.0;
        _ -> 1.0
    end,

    %% Generate pseudo-random spread based on timestamp
    Noise = (Now rem 100) / 1000.0,  % 0-10% variation

    %% Create opportunity if spread is significant
    case Noise > 0.015 of
        true ->
            [#{
                id => list_to_binary(io_lib:format("ARB-~p-~p", [Now, Now rem 10000])),
                crypto => Crypto,
                fiat => Fiat,
                buy_source => binance_p2p,
                buy_price => BasePrice * (1.0 - Noise/2),
                sell_source => vibee_p2p,
                sell_price => BasePrice * (1.0 + Noise/2),
                spread_percent => Noise * 100,
                potential_profit_percent => (Noise * 100) - 0.5,
                max_trade_size => 1000.0,
                detected_at => Now,
                expires_at => Now + 300
            }];
        false ->
            []
    end.

update_all_stats(ActiveUsers, Stats) ->
    maps:fold(fun(TelegramId, _UserState, AccStats) ->
        case maps:get(TelegramId, AccStats, undefined) of
            undefined ->
                maps:put(TelegramId, initial_stats(), AccStats);
            UserStats ->
                %% Update timestamp
                maps:put(TelegramId, maps:put(updated_at, erlang:system_time(second), UserStats), AccStats)
        end
    end, Stats, ActiveUsers).

%% =============================================================================
%% REAL ARBITRAGE SCANNING WITH TELEGRAM NOTIFICATIONS
%% =============================================================================

%% Scan all configured pairs and send Telegram alerts
scan_and_notify_all_pairs() ->
    %% Check if P2P alerts are enabled (default: disabled)
    case os:getenv("VIBEE_P2P_ALERTS_ENABLED") of
        "true" -> do_scan_and_notify_all_pairs();
        "1" -> do_scan_and_notify_all_pairs();
        _ ->
            %% Alerts disabled - skip silently
            ok
    end.

do_scan_and_notify_all_pairs() ->
    io:format("[EarningWorker] Scanning all pairs for arbitrage...~n"),
    log_system("Scanning for arbitrage opportunities..."),

    %% Configure pairs to scan
    %% NOTE: Gleam compiles THB -> t_h_b, RUB -> r_u_b, TON -> t_o_n, USDT -> u_s_d_t
    Pairs = [
        {t_o_n, r_u_b},
        {u_s_d_t, r_u_b},
        {t_o_n, t_h_b},
        {u_s_d_t, t_h_b}
    ],

    %% Scan each pair and collect all alerts
    AllAlerts = lists:flatmap(fun({Crypto, Fiat}) ->
        scan_pair_real(Crypto, Fiat)
    end, Pairs),

    %% Send notifications for alerts that pass cooldown
    case AllAlerts of
        [] ->
            io:format("[EarningWorker] No arbitrage opportunities found~n"),
            log_system("Scan complete: no opportunities found");
        _ ->
            io:format("[EarningWorker] Found ~p opportunities, sending notifications...~n", [length(AllAlerts)]),
            log_arbitrage_scan(length(AllAlerts)),
            send_alert_notifications(AllAlerts)
    end.

%% Scan a single pair using real Gleam alerts module
scan_pair_real(Crypto, Fiat) ->
    try
        %% Call Gleam alerts module - scan_spreads returns List(SpreadAlert)
        %% Gleam modules use @ as separator: vibee/earning/alerts -> vibee@earning@alerts
        MinSpread = 0.5,  % 0.5% minimum spread for production alerts
        'vibee@earning@alerts':scan_spreads(Crypto, Fiat, MinSpread)
    catch
        Error:Reason ->
            io:format("[EarningWorker] Error scanning ~p/~p: ~p:~p~n", [Crypto, Fiat, Error, Reason]),
            []
    end.

%% Send notifications for alerts
send_alert_notifications(Alerts) ->
    %% Get chat ID from env or use default
    ChatId = case os:getenv("VIBEE_ALERT_CHAT_ID") of
        false -> 144022504;
        "" -> 144022504;
        ChatIdStr ->
            try list_to_integer(ChatIdStr)
            catch _:_ -> 144022504
            end
    end,
    CooldownSeconds = 300,  % 5 minutes cooldown per alert type

    try
        %% Call Gleam function to process and notify
        %% Gleam modules use @ as separator: vibee/earning/alerts -> vibee@earning@alerts
        SentCount = 'vibee@earning@alerts':process_and_notify_alerts(Alerts, ChatId, CooldownSeconds),
        io:format("[EarningWorker] Sent ~p Telegram notifications~n", [SentCount]),
        case SentCount > 0 of
            true -> log_alert_sent(ChatId, io_lib:format("~p alerts", [SentCount]));
            false -> ok
        end
    catch
        Error:Reason ->
            io:format("[EarningWorker] Error sending notifications: ~p:~p~n", [Error, Reason]),
            log_error("alerts", Reason)
    end.

%% =============================================================================
%% MAKER BOT INTEGRATION
%% =============================================================================

%% ETS table for maker bot state
-define(MAKER_STATE_TABLE, vibee_maker_state).

ensure_maker_state_table() ->
    case ets:info(?MAKER_STATE_TABLE) of
        undefined ->
            ets:new(?MAKER_STATE_TABLE, [named_table, public, set]);
        _ -> ok
    end.

get_last_maker_update(TelegramId) ->
    ensure_maker_state_table(),
    case ets:lookup(?MAKER_STATE_TABLE, {maker_update, TelegramId}) of
        [{_, Timestamp}] -> Timestamp;
        [] -> 0
    end.

set_last_maker_update(TelegramId, Timestamp) ->
    ensure_maker_state_table(),
    ets:insert(?MAKER_STATE_TABLE, {{maker_update, TelegramId}, Timestamp}).

%% Check if maker bot is enabled for user (uses vibee_earning_ffi)
is_maker_bot_enabled(TelegramId) ->
    vibee_earning_ffi:is_maker_enabled(TelegramId).

%% Run maker bot for a user with error tracking and retry
run_maker_bot(TelegramId) ->
    run_maker_bot(TelegramId, 0).

run_maker_bot(TelegramId, RetryCount) when RetryCount >= 3 ->
    io:format("[MAKER] Max retries reached for user ~p, will try again next cycle~n", [TelegramId]),
    track_error(TelegramId, max_retries);

run_maker_bot(TelegramId, RetryCount) ->
    io:format("[MAKER] Running maker bot for user ~p (attempt ~p)~n", [TelegramId, RetryCount + 1]),

    %% First, cancel old maker orders
    try
        Cancelled = 'vibee@earning@maker_bot':cancel_old_orders(TelegramId),
        io:format("[MAKER] Cancelled ~p old orders~n", [Cancelled])
    catch
        _:_ -> ok
    end,

    %% Create default config for TON/RUB (most liquid pair)
    %% MakerConfig record in Gleam compiles to tuple
    Config = {maker_config,
        t_o_n,          % crypto: TON
        r_u_b,          % fiat: RUB
        1.5,            % spread_percent
        10.0,           % min_order_size
        1000.0,         % max_order_size
        100000.0,       % total_capital
        0.5,            % rebalance_threshold
        true            % enabled
    },

    %% Place new orders with retry logic
    try
        case 'vibee@earning@maker_bot':place_maker_orders(Config, TelegramId) of
            {ok, {BuyId, SellId}} ->
                io:format("[MAKER] ✓ Orders placed: buy=~s, sell=~s~n", [BuyId, SellId]),
                log_maker_order(buy, BuyId, 0.0),
                log_maker_order(sell, SellId, 0.0),
                clear_errors(TelegramId);
            {error, <<"No market data available">>} ->
                %% Market data issue - retry after delay
                io:format("[MAKER] No market data, retrying in 5s...~n"),
                log_system("Waiting for market data..."),
                timer:sleep(5000),
                run_maker_bot(TelegramId, RetryCount + 1);
            {error, Reason} ->
                io:format("[MAKER] Failed to place orders: ~p~n", [Reason]),
                log_error("maker_bot", Reason),
                track_error(TelegramId, Reason)
        end
    catch
        error:undef ->
            %% Module not loaded - common after deploy, retry
            io:format("[MAKER] Module not loaded, retrying in 2s...~n"),
            log_system("Loading maker module..."),
            timer:sleep(2000),
            run_maker_bot(TelegramId, RetryCount + 1);
        Error:Reason2 ->
            io:format("[MAKER] Error running maker bot: ~p:~p~n", [Error, Reason2]),
            log_error("maker_bot", Reason2),
            track_error(TelegramId, {Error, Reason2})
    end.

%% Track errors for a user
track_error(TelegramId, Reason) ->
    ensure_maker_state_table(),
    Key = {maker_errors, TelegramId},
    Count = case ets:lookup(?MAKER_STATE_TABLE, Key) of
        [{_, N}] -> N + 1;
        [] -> 1
    end,
    ets:insert(?MAKER_STATE_TABLE, {Key, Count}),
    ets:insert(?MAKER_STATE_TABLE, {{maker_last_error, TelegramId}, Reason}),
    io:format("[MAKER] Error count for user ~p: ~p~n", [TelegramId, Count]).

%% Clear error count on success
clear_errors(TelegramId) ->
    ensure_maker_state_table(),
    ets:delete(?MAKER_STATE_TABLE, {maker_errors, TelegramId}),
    ets:delete(?MAKER_STATE_TABLE, {maker_last_error, TelegramId}).

%% =============================================================================
%% ACTIVITY LOG
%% =============================================================================

-define(ACTIVITY_LOG_TABLE, vibee_activity_log).
-define(MAX_LOG_ENTRIES, 100).

ensure_activity_log_table() ->
    case ets:info(?ACTIVITY_LOG_TABLE) of
        undefined ->
            ets:new(?ACTIVITY_LOG_TABLE, [named_table, public, ordered_set]);
        _ -> ok
    end.

%% Add activity log entry
add_activity(Type, Message) ->
    add_activity(Type, Message, #{}).

add_activity(Type, Message, Data) ->
    ensure_activity_log_table(),
    Now = erlang:system_time(millisecond),
    Entry = #{
        id => Now,
        type => Type,
        message => Message,
        data => Data,
        timestamp => Now
    },
    ets:insert(?ACTIVITY_LOG_TABLE, {Now, Entry}),
    %% Keep only last MAX_LOG_ENTRIES
    prune_activity_log(),
    Entry.

prune_activity_log() ->
    case ets:info(?ACTIVITY_LOG_TABLE, size) of
        N when N > ?MAX_LOG_ENTRIES ->
            %% Get oldest keys and delete them
            Keys = ets:select(?ACTIVITY_LOG_TABLE, [{{'$1', '_'}, [], ['$1']}]),
            SortedKeys = lists:sort(Keys),
            ToDelete = lists:sublist(SortedKeys, N - ?MAX_LOG_ENTRIES),
            lists:foreach(fun(K) -> ets:delete(?ACTIVITY_LOG_TABLE, K) end, ToDelete);
        _ -> ok
    end.

%% Get activity log (last N entries)
get_activity_log() ->
    get_activity_log(50).

get_activity_log(Limit) ->
    ensure_activity_log_table(),
    AllEntries = ets:tab2list(?ACTIVITY_LOG_TABLE),
    SortedEntries = lists:sort(fun({K1, _}, {K2, _}) -> K1 > K2 end, AllEntries),
    LimitedEntries = lists:sublist(SortedEntries, Limit),
    [Entry || {_, Entry} <- LimitedEntries].

%% Log activity wrapper functions
log_maker_order(Side, OrderId, Price) ->
    add_activity(maker, io_lib:format("~s order ~s @ ~.2f", [Side, OrderId, Price]),
                 #{side => Side, order_id => OrderId, price => Price}).

log_arbitrage_found(Crypto, Fiat, Spread) when is_float(Spread) ->
    add_activity(arbitrage, io_lib:format("Found ~p/~p spread: ~.2f%", [Crypto, Fiat, Spread]),
                 #{crypto => Crypto, fiat => Fiat, spread => Spread});
log_arbitrage_found(Crypto, Fiat, Spread) when is_integer(Spread) ->
    add_activity(arbitrage, io_lib:format("Found ~p/~p spread: ~p%", [Crypto, Fiat, Spread]),
                 #{crypto => Crypto, fiat => Fiat, spread => Spread}).

log_arbitrage_scan(Count) ->
    add_activity(arbitrage, io_lib:format("Scan found ~p opportunities", [Count]),
                 #{count => Count}).

log_alert_sent(ChatId, Message) ->
    add_activity(alert, io_lib:format("Alert sent to ~p", [ChatId]),
                 #{chat_id => ChatId, message => Message}).

log_error(Context, Reason) ->
    add_activity(error, io_lib:format("Error in ~s: ~p", [Context, Reason]),
                 #{context => Context, reason => Reason}).

log_system(Message) ->
    add_activity(system, Message, #{}).
