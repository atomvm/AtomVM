-module(test_concat_badarg).
-export([start/0, f/2]).

start() ->
    f("Hello", "World") + f(5, "List").

f(A, B) ->
    try A ++ B of
        L when is_list(L) -> 1;
        _L -> 2
    catch
        error:badarg -> 3;
        _:_ -> 4
    end.
