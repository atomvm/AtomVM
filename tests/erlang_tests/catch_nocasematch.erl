-module(catch_nocasematch).

-export([start/0, f/1, do_nocasematch/1]).

start() ->
    is_error(f(0)).

f(Value) ->
    try do_nocasematch(Value) of
        AnyVal -> AnyVal
    catch
        _:_ -> error
    end.

do_nocasematch(Value) ->
    case Value of
        1 -> unexpected1;
        2 -> ok;
        3 -> unexpected3
    end.

is_error(error) ->
    1;
is_error(ok) ->
    0;
is_error(_Value) ->
    2.
