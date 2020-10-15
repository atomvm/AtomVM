-module(nested_list_size3).

-export([start/0]).

start() ->
    erts_debug:flat_size(make_nested()).

make_nested() ->
    [
        [
            [1, 2],
            [4, 5, 6],
            [7, 8, 9, 10]
        ],
        [
            [11, 12, 13, 14],
            [15, 16, 17, 18]
        ],
        [
            [],
            foo,
            [],
            bar,
            [19],
            []
        ],
        20,
        test
    ].
