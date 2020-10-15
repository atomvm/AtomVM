-module(test_list_match).

-export([start/0]).

start() ->
    Res1 =
        case f(foo) of
            foo -> 1;
            _ -> 0
        end,
    Res2 =
        case f(1342) of
            1342 -> 2;
            _ -> 0
        end,
    Res3 =
        case f({foo, bar}) of
            {foo, bar} -> 4;
            _ -> 0
        end,
    Res4 =
        case f([]) of
            [] -> 8;
            _ -> 0
        end,
    Res5 =
        case f([foo, bar]) of
            [foo, bar] -> 16;
            _ -> 0
        end,
    Res1 + Res2 + Res3 + Res4 + Res5.

f(X) when is_atom(X) -> id(X);
f(X) when is_integer(X) -> id(X);
f(X) when is_tuple(X) -> id(X);
f(X) when is_list(X) -> id(X).

id(X) -> X.
