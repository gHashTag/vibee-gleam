%% Payment ETS Store for VIBEE MCP
%% Stores payment records in-memory with ETS

-module(vibee_payment_ffi).

-export([
    init/0,
    generate_inv_id/0,
    get_timestamp/0,
    create/1,
    get/1,
    get_by_telegram_id/1,
    update_status/2,
    update_status_completed/1,
    list_pending/0,
    list_by_telegram_id/1,
    delete/1,
    get_balance/1,
    update_balance/2
]).

-define(PAYMENTS_TABLE, vibee_payments).
-define(BALANCES_TABLE, vibee_balances).

%% Initialize ETS tables
init() ->
    %% Create payments table if not exists
    case ets:info(?PAYMENTS_TABLE) of
        undefined ->
            ets:new(?PAYMENTS_TABLE, [
                set,
                public,
                named_table,
                {keypos, 1},
                {read_concurrency, true},
                {write_concurrency, true}
            ]);
        _ ->
            ok
    end,
    %% Create balances table if not exists
    case ets:info(?BALANCES_TABLE) of
        undefined ->
            ets:new(?BALANCES_TABLE, [
                set,
                public,
                named_table,
                {keypos, 1},
                {read_concurrency, true},
                {write_concurrency, true}
            ]);
        _ ->
            ok
    end,
    ok.

%% Generate unique invoice ID
generate_inv_id() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(999999),
    list_to_binary(io_lib:format("pay_~p_~6..0B", [Timestamp rem 10000000000, Random])).

%% Get current timestamp in seconds
get_timestamp() ->
    erlang:system_time(second).

%% Create a new payment record
%% Args: Map with payment data
%% Returns: {ok, InvId} | {error, Reason}
create(PaymentMap) when is_map(PaymentMap) ->
    init(),
    InvId = case maps:get(<<"inv_id">>, PaymentMap, undefined) of
        undefined -> generate_inv_id();
        Id -> Id
    end,

    TelegramId = maps:get(<<"telegram_id">>, PaymentMap, 0),
    Amount = maps:get(<<"amount">>, PaymentMap, 0.0),
    Stars = maps:get(<<"stars">>, PaymentMap, 0),
    Currency = maps:get(<<"currency">>, PaymentMap, <<"XTR">>),
    Method = maps:get(<<"method">>, PaymentMap, <<"telegram">>),
    InvoiceUrl = maps:get(<<"invoice_url">>, PaymentMap, undefined),
    Description = maps:get(<<"description">>, PaymentMap, undefined),
    CreatedAt = get_timestamp(),

    Payment = #{
        inv_id => InvId,
        telegram_id => TelegramId,
        amount => Amount,
        stars => Stars,
        currency => Currency,
        status => <<"PENDING">>,
        method => Method,
        invoice_url => InvoiceUrl,
        description => Description,
        created_at => CreatedAt,
        completed_at => undefined
    },

    case ets:insert_new(?PAYMENTS_TABLE, {InvId, Payment}) of
        true -> {ok, InvId};
        false -> {error, <<"Payment with this inv_id already exists">>}
    end.

%% Get payment by inv_id
%% Returns: {ok, PaymentMap} | {error, not_found}
get(InvId) when is_binary(InvId) ->
    init(),
    case ets:lookup(?PAYMENTS_TABLE, InvId) of
        [{InvId, Payment}] -> {ok, Payment};
        [] -> {error, not_found}
    end.

%% Get payment by telegram_id (returns first found)
get_by_telegram_id(TelegramId) when is_integer(TelegramId) ->
    init(),
    Pattern = {'_', #{telegram_id => TelegramId, '_' => '_'}},
    case ets:match_object(?PAYMENTS_TABLE, Pattern) of
        [{InvId, Payment} | _] -> {ok, Payment#{inv_id => InvId}};
        [] -> {error, not_found}
    end.

%% Update payment status
%% Returns: {ok, UpdatedPayment} | {error, not_found}
update_status(InvId, NewStatus) when is_binary(InvId), is_binary(NewStatus) ->
    case get(InvId) of
        {ok, Payment} ->
            UpdatedPayment = Payment#{status => NewStatus},
            ets:insert(?PAYMENTS_TABLE, {InvId, UpdatedPayment}),
            {ok, UpdatedPayment};
        Error ->
            Error
    end.

%% Mark payment as completed with timestamp
update_status_completed(InvId) when is_binary(InvId) ->
    case get(InvId) of
        {ok, Payment} ->
            UpdatedPayment = Payment#{
                status => <<"COMPLETED">>,
                completed_at => get_timestamp()
            },
            ets:insert(?PAYMENTS_TABLE, {InvId, UpdatedPayment}),
            {ok, UpdatedPayment};
        Error ->
            Error
    end.

%% List all pending payments
list_pending() ->
    init(),
    AllPayments = ets:tab2list(?PAYMENTS_TABLE),
    PendingPayments = lists:filter(
        fun({_InvId, Payment}) ->
            maps:get(status, Payment, undefined) =:= <<"PENDING">>
        end,
        AllPayments
    ),
    [Payment#{inv_id => InvId} || {InvId, Payment} <- PendingPayments].

%% List payments by telegram_id
list_by_telegram_id(TelegramId) when is_integer(TelegramId) ->
    init(),
    AllPayments = ets:tab2list(?PAYMENTS_TABLE),
    UserPayments = lists:filter(
        fun({_InvId, Payment}) ->
            maps:get(telegram_id, Payment, undefined) =:= TelegramId
        end,
        AllPayments
    ),
    [Payment#{inv_id => InvId} || {InvId, Payment} <- UserPayments].

%% Delete payment by inv_id
delete(InvId) when is_binary(InvId) ->
    init(),
    ets:delete(?PAYMENTS_TABLE, InvId),
    ok.

%% Get user balance (stars)
%% Returns: Balance (integer, default 0)
get_balance(TelegramId) when is_integer(TelegramId) ->
    init(),
    case ets:lookup(?BALANCES_TABLE, TelegramId) of
        [{TelegramId, Balance}] -> Balance;
        [] -> 0  % Default to 0 if not found
    end.

%% Update user balance - ATOMIC using ets:update_counter
%% Returns: {ok, NewBalance}
%% SECURITY: Uses ets:update_counter for atomic updates to prevent race conditions
update_balance(TelegramId, DeltaStars) when is_integer(TelegramId), is_integer(DeltaStars) ->
    init(),
    try
        %% Atomic increment/decrement with minimum floor of 0
        %% The UpdateOp tuple: {Position, Increment, Threshold, SetValue}
        %% Position 2 is the balance value, floor is 0, set to 0 if below
        NewBalance = ets:update_counter(?BALANCES_TABLE, TelegramId,
            {2, DeltaStars, 0, 0}),  %% Pos 2, incr by Delta, min 0, set to 0 if below
        {ok, NewBalance}
    catch
        error:badarg ->
            %% Key doesn't exist, initialize with the delta (or 0 if negative)
            InitBalance = max(0, DeltaStars),
            ets:insert(?BALANCES_TABLE, {TelegramId, InitBalance}),
            {ok, InitBalance}
    end.
