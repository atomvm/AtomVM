-module(test_is_process_alive).

-export([start/0, fact/1, no_loop/0]).

start() ->
    Pid = spawn(?MODULE, no_loop, []),
    A = g(is_process_alive(Pid)),
    Pid ! {self(), 5},
    Fact =
        receive
            Result ->
                Result
        end,
    sleep(50),
    case is_process_alive(Pid) of
        false ->
            Fact + A;
        true ->
            A
    end.

no_loop() ->
    receive
        {Pid, AnyInteger} when is_integer(AnyInteger) ->
            Pid ! fact(AnyInteger);
        {Pid, _AnyVal} ->
            Pid ! error
    end.

fact(0) ->
    1;
fact(N) ->
    N * fact(N - 1).

sleep(MSecs) ->
    receive
    after MSecs -> ok
    end.

g(true) ->
    1;
g(false) ->
    0.
