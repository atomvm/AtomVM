-module(test_open_port_badargs).
-export([start/0]).

start() ->
    safe_open_port({spawn, fail, "test"}, []) + safe_open_port({spawn, "echo"}, nil) * 4
    + safe_open_port({spawn}, []) * 16.
    %TODO: safe_open_port({notspawn, "echo"}, []) * 64.

safe_open_port(A, B) ->
    try open_port(A, B) of
        Any -> Any
    catch
        error:badarg -> -1;
        _:_ -> -2
    end.
