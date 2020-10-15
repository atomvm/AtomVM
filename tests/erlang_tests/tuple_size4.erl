-module(tuple_size4).

-export([start/0, make_tuple/0]).

start() ->
    erts_debug:flat_size(make_tuple()).

make_tuple() ->
    {{a}, {1, 2}, {[], [], []}}.
