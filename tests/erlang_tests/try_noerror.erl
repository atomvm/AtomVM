-module(try_noerror).

-export([start/0, f/1]).

start() ->
    f(3).

f(Value) ->
    try 5 div Value of
        AnyVal -> AnyVal
    catch
        _:_ -> error
    end.
