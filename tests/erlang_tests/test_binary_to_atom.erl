-module(test_binary_to_atom).

-export([start/0, f/1, g/1, h/1, i/1]).

start() ->
    f(i(h(g(2)))) + f(i(h(g(10)))) + f(i(h(g(20)))).

f(not_an_atom) ->
    0;
f(hello) ->
    1;
f(world) ->
    2;
f(test) ->
    4;
f(AnyAtom) when is_atom(AnyAtom) ->
    8;
f(_Any) ->
    16.

g(N) ->
    (N div 2) - 1.

h(0) ->
    <<"hello">>;
h(4) ->
    <<"this_will_be_a_new_atom">>;
h(9) ->
    5;
h(_) ->
    [].

i(A) ->
    try binary_to_atom(A, latin1) of
        AnyAtom -> AnyAtom
    catch
        _:_ -> not_an_atom
    end.
