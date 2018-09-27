-module(nested_tuple_size0).
-export([start/0, make_nested_tuple/0]).

start() ->
    erts_debug:flat_size(make_nested_tuple()).

make_nested_tuple() ->
    {[{1, 2}, {2, 4}]}.
