-module(trap_exit_flag).
-export([start/0, sum/1, proc/1]).

start() ->
    L = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    Pid = spawn(?MODULE, proc, [L]),
    false = erlang:process_flag(trap_exit, true),
    erlang:link(Pid),
    Pid ! {self(), sum},
    CM =
        receive
            N2 -> N2
        after 2000 ->
            err
        end,
    case CM of
        {'EXIT',Pid,{{nocatch,test}, EL}} when is_pid(Pid) andalso is_list(EL) ->
            1;
        {'EXIT',_,normal} ->
            2;
        {'EXIT',_,_} ->
            3;
        T when is_tuple(T) ->
            4;
        _ ->
            5
    end.

proc(_L) ->
    receive
        {_Pid, sum} ->
            throw(test)
    end.

sum([]) ->
    0;

sum([H | T]) ->
    H + sum(T).
