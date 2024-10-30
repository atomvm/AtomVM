-module(test_lists_member).

-export([test/0, start/0]).

start() ->
    test().

test() ->
    true = test_member_with_existing_element(),
    true = test_member_with_existing_element_on_heterogenous_list(),
    false = test_member_with_non_existing_element(),
    false = test_member_in_empty_list(),
    ok.

test_member_with_existing_element() ->
    Element = 5,
    List = [1, 2, 3, 4, 5, 6, 7, 8, 9],
    lists:member(Element, List).

test_member_with_existing_element_on_heterogenous_list() ->
    Element = key,
    List = [1, "a", 3, "b", 5, 6, "cd", 8, key],
    lists:member(Element, List).

test_member_with_non_existing_element() ->
    Element = 10,
    List = [1, 2, 3, 4, 5, 6, 7, 8, 9],
    lists:member(Element, List).

test_member_in_empty_list() ->
    Element = element,
    List = [],
    lists:member(Element, List).
