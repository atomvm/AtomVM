-module(long_atoms).

-export([start/0, make_long_list/1, f/1]).

start() ->
    A = "aaaaaaaaaaaaaaaaa",
    L = make_long_list(A),
    f(L ++ A) + f(L).

make_long_list(A) ->
    A ++ A ++ A ++ A ++ A ++ A ++ A ++ A ++ A ++ A ++ A ++ A ++ A ++ A ++ A.

f(L) ->
    try erlang:list_to_atom(L) of
        At when is_atom(At) -> 1;
        _At -> 2
    catch
        error:system_limit -> 3;
        _:_ -> 4
    end.
