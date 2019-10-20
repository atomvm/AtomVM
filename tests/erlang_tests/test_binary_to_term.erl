-module(test_binary_to_term).

-export([start/0]).

start() ->
    test_reverse(foo,           <<131,100,0,3,102,111,111>>),
    test_reverse(bar,           <<131,100,0,3,98,97,114>>),
    test_reverse(128,           <<131,97,128>>),
    test_reverse(257,           <<131,98,0,0,1,1>>),
    test_reverse(0,             <<131,97,0>>),
    test_reverse(-1,            <<131,98,255,255,255,255>>),
    test_reverse(32768,         <<131,98,0,0,128,0>>),
    test_reverse(-32768,        <<131,98,255,255,128,0>>),
    test_reverse({foo, bar},    <<131,104,2,100,0,3,102,111,111,100,0,3,98,97,114>>),
    test_reverse({foo, 0},      <<131,104,2,100,0,3,102,111,111,97,0>>),
    test_reverse([],            <<131,106>>),
    test_reverse([{foo, 0}, {bar, 1}], <<131,108,0,0,0,2,104,2,100,0,3,102,111,111,97,0,104,2,100,0,3,98,97,114,97,1,106>>),
    test_reverse([improper|list], <<131,108,0,0,0,1,100,0,8,105,109,112,114,111,112,101,114,100,0,4,108,105,115,116>>),
    test_reverse(<<"foobar">>,  <<131,109,0,0,0,6,102,111,111,98,97,114>>),
    test_reverse(<<":アトムＶＭ">>, <<131,109,0,0,0,6,58,162,200,224,54,45>>),
    test_reverse("",            <<131,106>>),
    test_reverse("foobar",      <<131,107,0,6,102,111,111,98,97,114>>),
    test_reverse(":アトムＶＭ",  <<131,108,0,0,0,6,97,58,98,0,0,48,162,98,0,0,48,200,98,0,0,48,224,98,0,0,255,54,98,0,0,255,45,106>>),
    0.

test_reverse(T, Interop) ->
    Bin = erlang:term_to_binary(T),
    %% erlang:display(Bin),
    Bin = Interop,
    X = erlang:binary_to_term(Bin),
    %% erlang:display(X),
    X = T.
