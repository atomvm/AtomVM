-module(catch_badmatch).

-export([start/0, f/1, do_badmatch/1]).

start() ->
    is_error(f(0)).

f(Value) ->
    try do_badmatch(Value) of
        AnyVal -> AnyVal
    catch
        _:_ -> error
    end.

do_badmatch(V) ->
    4 = V * 2.

is_error(error) ->
    1;
is_error(Value) when is_integer(Value) ->
    0;
is_error(_Value) ->
    2.
