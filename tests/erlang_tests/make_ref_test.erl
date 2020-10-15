-module(make_ref_test).

-export([start/0, compare/2, do_make_ref/0, to_tuple/1]).

start() ->
    ARef = do_make_ref(),
    ARefT = to_tuple(ARef),
    compare(ARefT, ARef) + compare(to_tuple(make_ref()), ARef) * 32.

do_make_ref() ->
    make_ref().

to_tuple(Something) ->
    {Something}.

compare(Something1, {Something2}) when Something1 == Something2 ->
    1;
compare({Something1}, Something2) when Something1 == Something2 ->
    2;
compare(_Any1, _Any2) ->
    4.
