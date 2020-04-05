-module(ref_to_list_test).

-export([start/0, check/1, g/1]).

start() ->
    [A1, A2, A3, A4, A5 | T] = g(h()),
    A1 + A2 + A3 + A4 + A5 + check(T).

check([$>]) ->
    3;
check([$. | T]) ->
    1 + check(T);
check([H | T]) when H >= $0 andalso H =< $9 ->
    check(T).

g(X) ->
    try erlang:ref_to_list(X) of
        Res -> Res
    catch
        error:badarg -> 0;
        _:_ -> 10
    end.

h() ->
    erlang:make_ref().
