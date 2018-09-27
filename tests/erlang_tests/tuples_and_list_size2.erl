-module(tuples_and_list_size2).
-export([start/0, make_tuples_and_list/0]).

start() ->
    erts_debug:flat_size(make_tuples_and_list()).

make_tuples_and_list() ->
    [{a, 10}, {b, 20}].
