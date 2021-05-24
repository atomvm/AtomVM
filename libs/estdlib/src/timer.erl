-module(timer).

-export([sleep/1]).

-define(MAX_INT, 4294967295).

-spec sleep(non_neg_integer() | infinity) -> ok.
sleep(infinity) ->
    receive
        '$atomvm_timer_interrupt' ->
            {error, unexpected_interrupt}
    after ?MAX_INT ->
        sleep(infinity)
    end;
sleep(MSecs) ->
    receive
        '$atomvm_timer_interrupt' ->
            {error, unexpected_interrupt}
    after MSecs -> ok
    end.
