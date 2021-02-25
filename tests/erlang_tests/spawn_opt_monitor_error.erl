-module(spawn_opt_monitor_error).
-export([start/0, sum/1, proc/1]).

start() ->
    L = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    {Pid, Ref} = spawn_opt(?MODULE, proc, [L], [monitor]),
    Pid ! {self(), sum},
    receive
        N when is_integer(N) -> N
    end,
    CM =
        receive
            N2 -> N2
        end,
    case CM of
        {'DOWN',Ref,process,Pid,{5,EL}} when is_reference(Ref) andalso is_pid(Pid)
                                                        andalso is_list(EL) ->
            1;
        {'DOWN',_,process,_,{{nocatch,test}, _}} ->
            2;
        {'DOWN',_,process,_,_} ->
            3;
        T when is_tuple(T) ->
            4;
        _ ->
            5
    end.

proc(L) ->
    receive
        {Pid, sum} ->
            Pid ! sum(L),
            erlang:error(5)
    end.

sum([]) ->
    0;

sum([H | T]) ->
    H + sum(T).
