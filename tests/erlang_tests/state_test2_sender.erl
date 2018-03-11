-module (state_test2_sender).
-export([send_msgs/1]).

send_msgs(Pid) ->
    send_integer(Pid, 1),
    send_integer(Pid, 2),
    send_integer(Pid, 3),
    Pid ! {get, self()},
    Value =
        receive
            Any -> hd(Any)
        end,
    Pid ! terminate,
    Value.

send_integer(Pid, Index) ->
    Pid ! {put, Index}.
