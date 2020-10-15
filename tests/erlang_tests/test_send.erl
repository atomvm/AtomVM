-module(test_send).

-export([start/0, send2/2]).

start() ->
    send2(5, 1) + send2(self(), -1).

send2(A, B) ->
    try erlang:send(A, B) of
        B -> -1;
        Any -> Any
    catch
        error:badarg -> -2;
        _:_ -> -4
    end.
