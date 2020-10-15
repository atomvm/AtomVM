-module(test_ref_eq).

-export([start/0, id/1, make_list/2, factorial/1]).

start() ->
    N = factorial(id(0)) - 1,
    R = make_ref(),
    bool_to_n(make_list(R, N) == [{1,R},
                        {R,1},
                        [0,R],
                        [R,0],
                        [0,0]]) +
    bool_to_n(make_list(R, N) == [{1,R},
                        {R,1},
                        [0,make_ref()],
                        [R,0],
                        [0,0]]) * 2 +
    bool_to_n(make_list(R, N) == [{1,R},
                        {R,1},
                        [0,1,R],
                        [R,0],
                        [0,0]]) * 4.
make_list(R, N) ->
    [
        {1, R},
        {R, factorial(N)},
        [N, R],
        [R, N],
        [N, N]
    ].

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

id(N) ->
    N.

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.
