-module(avm_timer).

-export([sleep/1]).

-spec sleep(non_neg_integer()) -> ok.
sleep(MSecs) ->
    receive
        after MSecs -> ok
    end.
