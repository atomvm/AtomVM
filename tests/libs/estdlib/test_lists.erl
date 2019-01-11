-module(test_lists).

-export([test/0]).

test() ->
    ok = test_nth(),
    ok.

test_nth() ->
    ok = etest:assert_match(lists:nth(1, [a,b,c]), a),
    ok = etest:assert_match(lists:nth(2, [a,b,c]), b),
    ok = etest:assert_match(lists:nth(3, [a,b,c]), c),
    % try
    %     lists:nth(-1, [a,b,c]),
    %     throw(failure)
    % catch
    %     _:_ -> ok
    % end,
    ok.
