-module(test_try_case_end).

-export([start/0, id/1, add_handle_badarith/2, try_add_plus_2/2, try_add/2]).

start() ->
    add_handle_badarith(id(-1), id(1)).

id(X) ->
    X.

add_handle_badarith(A, B) ->
    try try_add_plus_2(A, B) of
        Result -> Result
    catch
        error:badarith -> -16;
        error:{try_clause, N} -> 256 - N;
        _:_ -> 2048
    end.

try_add_plus_2(A, B) ->
    try_add(A, B) + 2.

try_add(A, B) ->
    try A + B of
        1 -> 0;
        2 -> 1
    catch
        error:badarg -> -1;
        _:_ -> -1024
    end.
