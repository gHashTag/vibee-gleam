%% VIBEE Oracle FFI - Erlang FFI for Oracle functions
%% SMS parsing, amount extraction, reference detection

-module(vibee_oracle_ffi).
-export([
    extract_amount/2,
    extract_p2p_reference/1,
    sign_message/2,
    verify_signature/3
]).

%% =============================================================================
%% AMOUNT EXTRACTION
%% =============================================================================

%% Extract amount from text based on currency
%% Returns {ok, Float} or {error, Reason}
extract_amount(Text, Currency) ->
    %% Convert to binary if needed
    TextBin = ensure_binary(Text),

    %% Remove commas and spaces from numbers
    CleanText = re:replace(TextBin, <<"[,\\s]">>, <<"">>, [global, {return, binary}]),

    %% Pattern based on currency
    Pattern = case Currency of
        <<"thb">> ->
            %% Match Thai Baht patterns: 3500.00, 3500, followed by optional บาท
            <<"([0-9]+\\.?[0-9]*)">>;
        <<"rub">> ->
            %% Match Russian Ruble patterns: 5000р, 5000.00, +5000
            <<"\\+?([0-9]+\\.?[0-9]*)">>;
        <<"usd">> ->
            %% Match USD patterns: $100, 100.00
            <<"\\$?([0-9]+\\.?[0-9]*)">>;
        _ ->
            <<"([0-9]+\\.?[0-9]*)">>;
        _ ->
            <<"([0-9]+\\.?[0-9]*)">>
    end,

    case re:run(CleanText, Pattern, [{capture, [1], binary}]) of
        {match, [AmountBin]} ->
            try
                Amount = case binary:match(AmountBin, <<".">>) of
                    nomatch ->
                        float(binary_to_integer(AmountBin));
                    _ ->
                        binary_to_float(AmountBin)
                end,
                {ok, Amount}
            catch
                _:_ -> {error, <<"parse_error">>}
            end;
        nomatch ->
            {error, <<"no_match">>}
    end.

%% =============================================================================
%% REFERENCE EXTRACTION
%% =============================================================================

%% Extract P2P order reference from text
%% Returns {some, Reference} or none
extract_p2p_reference(Text) ->
    TextBin = ensure_binary(Text),

    %% Pattern for P2P order ID: P2P-TIMESTAMP-COUNTER
    Pattern = <<"(P2P-[0-9]+-[0-9]+)">>,

    case re:run(TextBin, Pattern, [{capture, [1], binary}]) of
        {match, [Reference]} ->
            {some, Reference};
        nomatch ->
            none
    end.

%% =============================================================================
%% SIGNING (for Oracle authentication)
%% =============================================================================

%% Sign a message with private key (ED25519)
%% For production, use proper crypto library
sign_message(Message, PrivateKey) ->
    %% Simple HMAC for demo (use ED25519 in production)
    MessageBin = ensure_binary(Message),
    KeyBin = ensure_binary(PrivateKey),

    Mac = crypto:mac(hmac, sha256, KeyBin, MessageBin),
    base64:encode(Mac).

%% Verify signature
verify_signature(Message, Signature, PublicKey) ->
    %% For demo, just check if signature starts with expected prefix
    SigBin = ensure_binary(Signature),
    case binary:match(SigBin, <<"oracle_sig_">>) of
        {0, _} -> true;
        _ ->
            %% Try proper verification (simplified)
            ExpectedSig = sign_message(Message, PublicKey),
            ExpectedSig =:= SigBin
    end.

%% =============================================================================
%% HELPERS
%% =============================================================================

ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(_) -> <<>>.
