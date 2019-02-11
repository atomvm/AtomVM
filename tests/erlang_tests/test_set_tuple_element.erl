-module(test_set_tuple_element).

-export([start/0, test/2]).

-record(
    big, {
        a = foo,
        b = 1,
        c = {e, f},
        e = [3,1,4,1,5,2,6,9,5],
        f = [],
        g = undefined,
        h = [have, i ,created, enough, elements, yet],
        i = [und, now, for, something, completely, different],
        j = {ha, ha}
    }
).


start() ->
    test(thing1, {lets, try_this}).

test(G, J) ->
    Big = make_a_change(#big{}, G, J),
    case {Big#big.g, Big#big.j} of
        {G, J} ->
            0;
        X -> erlang:display(X), 1
    end.

make_a_change(Big, Thing1, Thing2) ->
    Big#big{g=Thing1, j=Thing2}.
