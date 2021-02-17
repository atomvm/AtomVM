-module(spawn_opt_demonitor_normal).
-export([start/0, sum/1, proc/1]).

start() ->
    L = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    {Pid, Ref} = spawn_opt(?MODULE, proc, [L], [monitor]),
    true = erlang:demonitor(Ref),
    Pid ! {self(), sum},
    receive
        N when is_integer(N) -> N
    end,
    CM =
        receive
            N2 -> N2
        after 200 ->
            exit
        end,
    case CM of
        exit ->
            1;
        {'DOWN',Ref,process,Pid,normal} when is_reference(Ref) andalso is_pid(Pid) ->
            2;
        {'DOWN',_,process,_,normal} ->
            3;
        {'DOWN',_,process,_,_} ->
            4;
        T when is_tuple(T) ->
            5;
        _ ->
            6
    end.

proc(L) ->
    receive
        {Pid, sum} ->
            Pid ! sum(L)
    end.

sum([]) ->
    0;

sum([H | T]) ->
    H + sum(T).
