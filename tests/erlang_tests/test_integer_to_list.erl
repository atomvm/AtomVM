-module(test_integer_to_list).

-export([start/0, some_calculation/2, concat_integers/2, compare_list/2]).

start() ->
    NewList =
        concat_integers(some_calculation(100, 1), some_calculation(100, hello)) ++
            concat_integers(a, []),
    compare_list(NewList, "6,-1").

some_calculation(N, A) when is_integer(N) and is_integer(A) ->
    N div 20 + A;
some_calculation(_N, _A) ->
    -1.

concat_integers(A, B) ->
    ListA =
        try integer_to_list(A) of
            ListValue -> ListValue
        catch
            error:badarg -> "";
            _:_ -> "error"
        end,
    ListB =
        try integer_to_list(B) of
            AListValue -> "," ++ AListValue
        catch
            error:badarg -> "";
            _:_ -> "error"
        end,
    ListA ++ ListB.

compare_list([], []) ->
    1;
compare_list([H_A | T_A], [H_B | T_B]) when H_A == H_B ->
    compare_list(T_A, T_B);
compare_list(_A, _B) ->
    0.
