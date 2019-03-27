-module(spawn_fun3).
-export([start/0]).

start() ->
    L = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    Fun = fun() ->
            receive
                {Pid, sum} ->
                    Pid ! sum(L)
            end
          end,
    Pid = spawn(Fun),
    Pid ! {self(), sum},
    receive
        N -> N
    end.

sum([]) ->
    0;

sum([H | T]) ->
    H + sum(T).
