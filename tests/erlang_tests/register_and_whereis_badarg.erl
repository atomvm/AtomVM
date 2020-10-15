-module(register_and_whereis_badarg).

-export([start/0, f/1, do_register/2, do_whereis/1]).

start() ->
    do_register(fail, fail) + do_register(fail, self()) * 10 + do_whereis(fail) * 100.

f(Pid) when is_pid(Pid) ->
    Pid;
f(fail) ->
    5;
f(good) ->
    good.

do_register(A, B) ->
    try register(f(A), f(B)) of
        true -> 1;
        _Any -> 2
    catch
        error:badarg -> 3;
        _:_ -> 4
    end.

do_whereis(A) ->
    try whereis(f(A)) of
        APid when is_pid(APid) -> 1;
        _Any -> 2
    catch
        error:badarg -> 3;
        _:_ -> 4
    end.
