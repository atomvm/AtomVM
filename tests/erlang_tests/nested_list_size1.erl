-module(nested_list_size1).
-export([start/0]).

start() ->
    erts_debug:flat_size(make_nested()).

make_nested() ->
    [[]].
