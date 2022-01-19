-module(test_timer).

-export([test/0]).

test() ->
    ok = test_timer(),
    ok = test_timer_interrupt(),
    ok = test_timer_loop(),
    ok.

-include("etest.hrl").

test_timer() ->
    T0 = erlang:timestamp(),
    ok = timer:sleep(101),
    T1 = erlang:timestamp(),
    ok = etest:assert_true((to_ms(T1) - to_ms(T0)) >= 101),
    ok.

test_timer_interrupt() ->
    Self = self(),
    Pid = spawn(fun() -> do_test_interrupt(Self) end),
    receive ready -> ok end,

    %% this message should not interrupt the timer
    Pid ! try_to_interrupt,

    Pid ! '$atomvm_timer_interrupt',
    ?ASSERT_MATCH(pop_mailbox(), ok),
    ok.

pop_mailbox() ->
    receive
        X -> X
    end.

do_test_interrupt(Pid) ->
    Pid ! ready,
    case timer:sleep(infinity) of
        {error, unexpected_interrupt} ->
            Pid ! ok;
        _ ->
            Pid ! error
    end.

test_timer_loop() ->
    Self = self(),
    spawn(fun() -> timer:sleep(220), Self ! ping end),
    ok = timer_loop(5).

timer_loop(0) ->
    ok;
timer_loop(I) ->
    T0 = erlang:timestamp(),
    ok = timer:sleep(101),
    T1 = erlang:timestamp(),
    ok = etest:assert_true((to_ms(T1) - to_ms(T0)) >= 101),
    timer_loop(I - 1).

to_ms({MegaSecs, Secs, MicroSecs}) ->
    ((MegaSecs * 1000000 + Secs) * 1000 + MicroSecs div 1000).
