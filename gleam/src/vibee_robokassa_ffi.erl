%% Robokassa Payment Integration FFI
%% MD5 signature validation and URL generation

-module(vibee_robokassa_ffi).

-export([
    md5_hash/1,
    validate_signature/4,
    generate_payment_url/5,
    get_config/0
]).

%% Calculate MD5 hash of data
%% Returns lowercase hex string
md5_hash(Data) when is_binary(Data) ->
    Hash = crypto:hash(md5, Data),
    list_to_binary(
        string:lowercase(
            lists:flatten(
                [io_lib:format("~2.16.0b", [Byte]) || <<Byte>> <= Hash]
            )
        )
    ).

%% Validate Robokassa webhook signature
%% Formula: MD5(OutSum:InvId:Password2)
%% Returns: true | false
validate_signature(OutSum, InvId, Password2, Signature)
  when is_binary(OutSum), is_binary(InvId), is_binary(Password2), is_binary(Signature) ->
    DataToHash = <<OutSum/binary, ":", InvId/binary, ":", Password2/binary>>,
    CalculatedSignature = md5_hash(DataToHash),
    ReceivedSignature = string:lowercase(binary_to_list(Signature)),
    CalculatedStr = binary_to_list(CalculatedSignature),
    CalculatedStr =:= ReceivedSignature.

%% Generate Robokassa payment URL
%% Formula for signature: MD5(MerchantLogin:OutSum:InvId:Password1)
generate_payment_url(MerchantLogin, OutSum, InvId, Description, Password1)
  when is_binary(MerchantLogin), is_binary(OutSum), is_binary(InvId),
       is_binary(Description), is_binary(Password1) ->

    %% Calculate signature
    SignatureData = <<MerchantLogin/binary, ":", OutSum/binary, ":", InvId/binary, ":", Password1/binary>>,
    Signature = md5_hash(SignatureData),

    %% Get test mode from config
    TestMode = get_env(<<"ROBOKASSA_TEST_MODE">>, <<"true">>),
    IsTest = TestMode =:= <<"true">> orelse TestMode =:= <<"1">>,

    %% Build URL
    BaseUrl = case IsTest of
        true -> <<"https://auth.robokassa.ru/Merchant/Index.aspx">>;
        false -> <<"https://auth.robokassa.ru/Merchant/Index.aspx">>
    end,

    EncodedDesc = http_uri:encode(binary_to_list(Description)),

    Params = io_lib:format(
        "MerchantLogin=~s&OutSum=~s&InvId=~s&Description=~s&SignatureValue=~s~s",
        [
            binary_to_list(MerchantLogin),
            binary_to_list(OutSum),
            binary_to_list(InvId),
            EncodedDesc,
            binary_to_list(Signature),
            case IsTest of true -> "&IsTest=1"; false -> "" end
        ]
    ),

    <<BaseUrl/binary, "?", (list_to_binary(lists:flatten(Params)))/binary>>.

%% Get Robokassa configuration from environment
get_config() ->
    #{
        merchant_login => get_env(<<"ROBOKASSA_MERCHANT_LOGIN">>, <<>>),
        password1 => get_env(<<"ROBOKASSA_PASSWORD1">>, <<>>),
        password2 => get_env(<<"ROBOKASSA_PASSWORD2">>, <<>>),
        test_mode => get_env(<<"ROBOKASSA_TEST_MODE">>, <<"true">>)
    }.

%% Helper: Get environment variable
get_env(Key, Default) when is_binary(Key) ->
    case os:getenv(binary_to_list(Key)) of
        false -> Default;
        Value -> list_to_binary(Value)
    end.
