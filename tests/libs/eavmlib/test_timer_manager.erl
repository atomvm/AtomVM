-module(test_timer_manager).

-export([test/0]).

-include("estdlib.hrl").

test() ->
    ok = test_start_timer(),
    ok = test_cancel_timer(),
    ok.

-include("etest.hrl").

test_start_timer() ->
    ?ASSERT_MATCH(timer_manager:get_timer_refs(), []),
    timer_manager:start_timer(100, self(), test_start_timer),
    wait_for_timeout(test_start_timer, 5000).

test_cancel_timer() ->
    ?ASSERT_MATCH(timer_manager:get_timer_refs(), []),
    TimerRef = timer_manager:start_timer(60000, self(), test_cancel_timer),
    ?ASSERT_MATCH(timer_manager:get_timer_refs(), [TimerRef]),
    timer_manager:cancel_timer(TimerRef),
    %?TIMER:sleep(100),
    ?ASSERT_MATCH(timer_manager:get_timer_refs(), []),
    ok.

test_send_after() ->
    timer_manager:send_after(100, self(), ping),
    pong = wait_for_timeout(ping, 5000).


wait_for_timeout(Msg, Timeout) ->
    receive
        {timeout, _TimerRef, Msg} ->
            ok;
        Msg ->
            pong
    after Timeout ->
        fail
    end.
