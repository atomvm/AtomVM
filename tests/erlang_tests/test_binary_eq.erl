-module(test_binary_eq).
-export([start/0, id/1, make_list/2, factorial/1]).

start() ->
    N = factorial(id(0)) - 1,
    bool_to_n(make_list(<<"test">>, N) == [{1,<<"es">>},
                                 {<<"es">>,<<"1">>},
                                 [0,<<"es">>],
                                 [<<"es">>,0],
                                 [0,0]]) +
    bool_to_n(make_list(<<"test">>, N) == [{1,<<"es">>},
                                 {<<"es">>,<<"1">>},
                                 [0,<<"ez">>],
                                 [<<"es">>,0],
                                 [0,0]]) * 2 +
    bool_to_n(make_list(<<"test">>, N) == [{1,<<"es">>},
                                 {<<"es">>,<<"1">>},
                                 [0,<<"es">>],
                                 [0,<<"es">>,0],
                                 [0,0]]) * 4.
make_list(Bin, N) ->
    B = binary:part(Bin, 1, 2),
    [
     {1, B},
     {B, integer_to_binary(factorial(N))},
     [N, B],
     [B, N],
     [N, N]
    ].

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

id(N) ->
    N.

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.
