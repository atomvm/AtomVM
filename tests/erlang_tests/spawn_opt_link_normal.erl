-module(spawn_opt_link_normal).
-export([start/0, start2/0, sum/1, proc/1]).

start() ->
    {Pid, Ref} = spawn_opt(?MODULE, start2, [], [monitor]),
    Pid ! {self(), sum},
    CM =
        receive
            N2 -> N2
        end,
    case CM of
        {'DOWN',Ref,process,Pid,normal} when is_reference(Ref) andalso is_pid(Pid) ->
            1;
        {'DOWN',_,process,_,normal} ->
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
    Pid ! {self(), sum},
    receive
        N when is_integer(N) -> N
    end,
    CM =
        receive
            N2 -> N2
        end,
    case CM of
        {'DOWN',Ref,process,Pid,normal} when is_reference(Ref) andalso is_pid(Pid) ->
            1;
        {'DOWN',_,process,_,normal} ->
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
            Pid ! sum(L)
    end.

sum([]) ->
    0;

sum([H | T]) ->
    H + sum(T).
