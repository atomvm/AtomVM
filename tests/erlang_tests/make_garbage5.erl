-module(make_garbage5).

-export([start/0, det/1]).

start() ->
    det({{2, 2, 3}, {1, 1, 3}, {2, 0, 1}}).

det({{A, B}, {C, D}}) ->
    A * D - B * C;
det({{A, B, C}, {D, E, F}, {G, H, I}}) ->
    A * det({{E, F}, {H, I}}) - B * det({{D, F}, {G, I}}) + C * det({{D, E}, {G, H}}).
