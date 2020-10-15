-module(improper_literal).

-export([start/0, id/1, add/2, check/3]).

start() ->
    L1 = [1, 2, 3, 4 | 5],
    L2 = [1 | 2],
    bool_to_int(check(id(L1), 0, 5)) + bool_to_int(check(id(L2), 0, 2)) * 2.

id(I) ->
    I.

add(A, B) ->
    id(A) + id(B).

check(T, Last, 1) when not is_list(T) ->
    T > Last;
check([H | T], Last, N) ->
    if
        H > Last -> check(T, H, N - 1);
        H =< Last -> false
    end.

bool_to_int(true) ->
    1;
bool_to_int(false) ->
    0;
bool_to_int(_) ->
    -1.
