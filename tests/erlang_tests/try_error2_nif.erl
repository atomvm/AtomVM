-module (try_error2_nif).
-export([start/0, f/1, g/1, h/1, factorial/1]).

start() ->
    f(g(factorial(3))).

f({_V1, V2, V3}) ->
    try h(V2) of
        AnyVal -> AnyVal * V3
    catch
        error:N when is_integer(N) ->
            N + 1;
        error:_ ->
            0;
        _:_ ->
            -1
    end.

g(I) ->
    {I, I * 2, I * 4}.

h(I) when I > 4 ->
    error(I, [a, b, c]);
h(I) ->
    I.

factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N - 1).
