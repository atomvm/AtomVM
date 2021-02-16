-module(link_kill_parent).
-export([start/0, start2/0, sum/1, proc/1]).

start() ->
    Pid = spawn(?MODULE, start2, []),
    Pid ! {monitor, self()},
    CP =
        receive
            N2 -> N2
    end,
    CPRef = erlang:monitor(process, CP),
    Pid ! go,
    CM =
        receive
            Smth -> Smth
        end,
    case CM of
        {'DOWN',CPRef,process,CP,{{nocatch,test}, EL}} when is_list(EL) ->
            1;
        {'DOWN',Ref,process,Pid,normal} when is_reference(Ref) andalso is_pid(Pid) ->
            2;
        {'DOWN',_,process,_,_} ->
            3;
        T when is_tuple(T) ->
            4;
        _ ->
            5
    end.

start2() ->
    L = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    Pid = spawn_opt(?MODULE, proc, [L], [link]),
    receive
        {monitor, Resp} ->
            Resp ! Pid
    end,
    Pid ! {self(), sum},
    receive
        N when is_integer(N) -> N
    end,
    receive
        go -> throw(test)
    end.

proc(L) ->
    receive
        {Pid, sum} ->
            Pid ! sum(L),
            l()
    end.

l() ->
    l().

sum([]) ->
    0;

sum([H | T]) ->
    H + sum(T).
