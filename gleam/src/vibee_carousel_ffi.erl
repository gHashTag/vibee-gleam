-module(vibee_carousel_ffi).
-export([random_string/0]).

%% Generate a random hex string for IDs
random_string() ->
    Bytes = crypto:strong_rand_bytes(8),
    list_to_binary([io_lib:format("~2.16.0b", [B]) || <<B:8>> <= Bytes]).
