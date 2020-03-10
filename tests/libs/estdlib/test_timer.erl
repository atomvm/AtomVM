-module(test_timer).

-export([test/0]).

test() ->
    ok = test_timer(),
    ok.

-include("etest.hrl").

test_timer() ->
    %% hack. until we can convert an erlang timestamp to a big integer
    %% this will fail if run when UNIX epoch divides 1m secs
    {_M0, S0, _Mi0} = erlang:timestamp(),
    ok = timer:sleep(1001),
    {_M1, S1, _Mi1} = erlang:timestamp(),
    ok = etest:assert_true(S1 > S0),
    ok.
