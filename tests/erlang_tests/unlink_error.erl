-module(unlink_error).
-export([start/0, start2/0, sum/1, proc/1]).

start() ->
    {Pid, Ref} = spawn_opt(?MODULE, start2, [], [monitor]),
    Pid ! {self(), sum},
    CM =
        receive
            N2 -> N2
        after 150 ->
            ok
        end,
    case CM of
        ok ->
            1;
        {'DOWN',Ref,process,Pid,{{nocatch,test}, L}} when is_reference(Ref) andalso is_pid(Pid)
                                                         andalso is_list(L) ->
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

start2() ->
    L = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    Pid = spawn(?MODULE, proc, [L]),
    erlang:link(Pid),
    erlang:unlink(Pid),
    Pid ! {self(), sum},
    receive
        N when is_integer(N) -> N
    end,
    loop().

loop() ->
    loop().

proc(L) ->
    receive
        {Pid, sum} ->
            Pid ! sum(L),
            error(test)
    end.

sum([]) ->
    0;

sum([H | T]) ->
    H + sum(T).
