-module(test_timestamp).

-export([start/0]).

start() ->
    {M1, S1, Mic1} = erlang:timestamp(),
    receive after 100 -> ok end,
    {M2, S2, Mic2} = erlang:timestamp(),
    case leq({M1, S1, Mic1}, {M2, S2, Mic2}) of
        true ->
            1;
        false ->
            0
    end.


leq({M1, S1, Mic1}, {M1, S1, Mic1}) ->
    true;
leq({M1, S1, Mic1}, {M1, S1, Mic2}) when  Mic1 < Mic2 ->
    true;
leq({M1, S1, _}, {M1, S2, _}) when  S1 < S2 ->
    true;
leq({M1, _, _}, {M2, _, _}) when  M1 < M2 ->
    true;
leq(_, _) ->
    false.
