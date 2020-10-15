-module(test_bigint_eq).

-export([start/0, id/1, make_list/1, pow/2]).

start() ->
    N = factorial(id(0)) + 62,
    bool_to_n(
        make_list(N) ==
            [
                pow(-2, 62),
                {5, pow(-2, 63)},
                [1, test],
                [{}, []],
                [5, {}]
            ]
    ) +
        bool_to_n(
            make_list(N) ==
                [
                    pow(-2, 63),
                    {5, pow(-2, 62)},
                    [1, test],
                    [{}, []],
                    [5, {}]
                ]
        ) * 2 +
        bool_to_n(
            make_list(N) ==
                [
                    pow(-2, 62),
                    {5, {pow(-2, 62)}},
                    [1, test],
                    [{}, []],
                    [5, {}]
                ]
        ) * 4.

make_list(N) ->
    [
        pow(-2, N - 1),
        {5, pow(-2, N)},
        [1, test],
        [{}, []],
        [5, {}]
    ].

pow(_N, 0) ->
    1;
pow(N, M) ->
    N * pow(N, M - 1).

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

id(N) ->
    N.

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.
