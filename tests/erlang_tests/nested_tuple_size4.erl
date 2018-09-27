-module(nested_tuple_size4).
-export([start/0, make_nested_tuple/0]).

start() ->
    erts_debug:flat_size(make_nested_tuple()).

make_nested_tuple() ->
    [{[{{1}, {2}}, {{2}, {4}}, {{3}, {6}}], [{{1}, {2}}, {{3}, {6}}, {{5}, {10}}, {{7}, {14}}], [{{0}, {0}}]}, 2].
