-module(pid_to_list_test).

-export([start/0, check/1, g/1]).

start() ->
    [A | T] = g(self()),
    A + check(T).

check([$>]) ->
    3;
check([$. | T]) ->
    check(T);
check([H | T]) when H >= $0 andalso H =< $9 ->
    check(T).

g(X) ->
    try erlang:pid_to_list(X) of
        Res -> Res
    catch
        error:badarg -> 0;
        _:_ -> 10
    end.
