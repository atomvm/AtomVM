-module(complex_struct_size0).

-export([start/0, abcdefg/0]).

start() ->
    erts_debug:flat_size(abcdefg()).

a() ->
    {a, 0}.

b() ->
    {b, 1}.

c() ->
    {c, 2}.

d() ->
    {d, 3}.

e() ->
    {e, 4}.

f() ->
    {f, 5}.

g() ->
    {g, 6}.

ab() ->
    [a(), b()].

cde() ->
    [c(), d(), e()].

fg() ->
    [f(), g()].

cdefg() ->
    [cde(), fg()].

abcdefg() ->
    [ab(), cdefg()].
