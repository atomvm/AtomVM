-module(complex_struct_size3).
-export([start/0, a/0, b/0, c/0]).

start() ->
    erts_debug:flat_size(c()).

a() ->
    [1, 2, 3, 4, 5].

b() ->
    [5, 4, 3, 2, 1].

c() ->
    {a(), b()}.
