-module(test_binary_to_existing_atom).
-export([start/0, f/1, g/1, h/1, i/1, g/0]).

start() ->
    f(i(h(g(2)))) + f(i(h(g(10)))) + f(i(h(g(20)))).

f(missing_atom) ->
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
    <<"a_missing_atom">>;

h(_) ->
    [].

i(A) when not is_binary(A) ->
    error;

i(A) ->
    try binary_to_existing_atom(A, latin1) of
        AnyAtom -> AnyAtom
    catch
        error:badarg -> missing_atom;
        _:_ -> -1024
    end.

g() ->
    this_will_be_a_new_atom.
