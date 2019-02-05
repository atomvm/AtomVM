-module(test_timer).

-export([test/0]).

-include("estdlib.hrl").

test() ->
    ok = test_start_timer(),
    ok = test_cancel_timer(),
    ok = test_timer(),
    ok.

-include("etest.hrl").

test_start_timer() ->
    timer_manager:start(),
    ?ASSERT_MATCH(timer_manager:get_timer_refs(), []),
    ?ERLANG:start_timer(100, self(), test_start_timer),
    wait_for_timeout(test_start_timer, 5000).

test_cancel_timer() ->
    ?ASSERT_MATCH(timer_manager:get_timer_refs(), []),
    TimerRef = ?ERLANG:start_timer(60000, self(), test_cancel_timer),
    ?ASSERT_MATCH(timer_manager:get_timer_refs(), [TimerRef]),
    ?ERLANG:cancel_timer(TimerRef),
    ?TIMER:sleep(100),
    ?ASSERT_MATCH(timer_manager:get_timer_refs(), []),
    ok.

test_timer() ->
    %% hack. until we can convert an erlang timestamp to a big integer
    %% this will fail if run when UNIX epoch divides 1m secs
    {_M0, S0, _Mi0} = erlang:timestamp(),
    ok = ?TIMER:sleep(1001),
    {_M1, S1, _Mi1} = erlang:timestamp(),
    ok = etest:assert_true(S1 > S0),
    ok.

%% TODO Add support for opcode 96
% current_ms() ->
%     {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
%     MegaSecs * 1000000 + Secs * 1000 + MicroSecs / 1000.



wait_for_timeout(Msg, Timeout) ->
    receive
        {timeout, _TimerRef, Msg} ->
            ok
    after Timeout ->
        fail
    end.
