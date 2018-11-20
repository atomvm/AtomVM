-module(timer).

-export([sleep/1]).

sleep(MSecs) ->
    receive
        after MSecs -> ok
    end.
