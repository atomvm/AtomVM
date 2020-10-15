-module(improper_concat).

-export([start/0, id/1, add/2, check/3]).

start() ->
    L1 = [id(add(id(1), id(0))), id(add(id(1), id(1)))] ++ id(add(id(1), id(2))),
    L2 = [-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0] ++ L1,
    try_concat([id(1), add(id(1), id(1)) | id(3)], [4, id(5)]) * 4 +
        bool_to_int(check(L1, 0, 3)) + bool_to_int(check(L2, -11, 14)) * 2.

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

try_concat(A, B) ->
    try id(A) ++ id(B) of
        Res -> Res
    catch
        error:badarg -> 1;
        _:_ -> 2048
    end.

bool_to_int(true) ->
    1;
bool_to_int(false) ->
    0;
bool_to_int(_) ->
    -1.
