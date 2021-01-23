-module(test_selective_receive).

-export([start/0]).

start() ->
    test_selective_receive(),
    test_selective_receive_with_timeout(),
    0.

test_selective_receive() ->
    Self = self(),
    spawn(fun() ->
        Self ! three
    end),
    spawn(fun() ->
        receive after 250 -> ok end,
        Self ! two
    end),
    spawn(fun() ->
        receive after 500 -> ok end,
        Self ! one
    end),
    receive
        one ->
            ok
    end,
    receive
        two ->
            ok
    end,
    receive
        three ->
            ok
    end.

test_selective_receive_with_timeout() ->
    Self = self(),
    spawn(fun() ->
        Self ! two
    end),
    spawn(fun() ->
        receive after 500 -> ok end,
        Self ! one
    end),
    ok = receive
        one ->
            expected_timeout
    after 250 ->
        ok
    end,
    receive
        two ->
            ok
    end.
