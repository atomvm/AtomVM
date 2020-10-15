-module(catch_noifmatch).

-export([start/0, f/1, do_noifmatch/1]).

start() ->
    is_error(f(0)).

f(Value) ->
    try do_noifmatch(Value) of
        AnyVal -> AnyVal
    catch
        _:_ -> error
    end.

do_noifmatch(Value) ->
    if
        (Value >= 1) and (Value =< 2) -> unexpected1;
        Value == 3 -> ok;
        Value > 3 -> unexpected3
    end.

is_error(error) ->
    1;
is_error(ok) ->
    0;
is_error(_Value) ->
    2.
