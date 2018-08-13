-module(just_receive_test).
-export([start/0]).

start() ->
    Pid = self(),
    Pid ! 10,
    Rec =
        receive
            {_Something, Any} -> Any;
            Any -> Any
        end,
    Rec + is_pid_a_pid(Pid).


is_pid_a_pid(Pid) ->
    true_to_one(is_pid(Pid)).

true_to_one(true) ->
    1;
true_to_one(false) ->
    0.
