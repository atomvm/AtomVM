-module(make_garbage7).
-export([start/0, det/1]).

start() ->
    det({{2, 2, 3, 9}, {1, 1, 3, 4}, {2, 0, 1, 7}, {11, 3, 4, 8}}).

det({A}) ->
    A;

det({{A, B}, {C, D}}) ->
    A * det({D}) - C * det({B});

det({{A, B, C}, {D, E, F}, {G, H, I}}) ->
    A * det({{E, F}, {H, I}}) - B * det({{D, F}, {G, I}}) + C * det({{D, E}, {G, H}});

det({{A, B, C, D}, {E, F, G, H}, {I, J, K, L}, {M, N, O, P}}) ->
    A * det({{F, G, H}, {J, K, L}, {N, O, P}}) - B * det({{E, G, H}, {I, K, L}, {M, O, P}})
    + C * det({{E, F, H}, {I, J, L}, {M, N, P}}) - D * det({{E, F, G}, {I, J, K}, {M, N, O}}).
