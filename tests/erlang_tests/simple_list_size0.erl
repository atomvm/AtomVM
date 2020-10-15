-module(simple_list_size0).

-export([start/0, make_simple_list/0]).

start() ->
    erts_debug:flat_size(make_simple_list()).

make_simple_list() ->
    [1].
