-module(test_io_lib).

-export([test/0]).

-include("etest.hrl").
-include("estdlib.hrl").

test() ->
    ?ASSERT_MATCH(?IO_LIB:format("", []), ""),
    ?ASSERT_MATCH(?IO_LIB:format("foo", []), "foo"),
    ?ASSERT_MATCH(?IO_LIB:format("foo~n", []), "foo\n"),
    ?ASSERT_MATCH(?IO_LIB:format("foo: ~p~n", [bar]), "foo: bar\n"),
    ?ASSERT_MATCH(?IO_LIB:format("foo: ~p~n", ["bar"]), "foo: \"bar\"\n"),
    ?ASSERT_MATCH(?IO_LIB:format("foo: ~p~n", [123]), "foo: 123\n"),
    ?ASSERT_MATCH(?IO_LIB:format("foo: ~p~n", [-123]), "foo: -123\n"),
    ?ASSERT_MATCH(?IO_LIB:format("foo: ~p~n", [[1, 2, 3]]), "foo: [1,2,3]\n"),
    ?ASSERT_MATCH(?IO_LIB:format("foo: ~p~n", [[1, 2, [3]]]), "foo: [1,2,[3]]\n"),
    ?ASSERT_MATCH(?IO_LIB:format("foo: ~p~n", [[65, 116, 111, 109, 86, 77]]), "foo: \"AtomVM\"\n"),
    ?ASSERT_MATCH(?IO_LIB:format("foo: ~p~n", [<<"bar">>]), "foo: <<\"bar\">>\n"),
    ?ASSERT_MATCH(?IO_LIB:format("foo: ~p~n", [<<1, 2, 3>>]), "foo: <<1,2,3>>\n"),
    ?ASSERT_MATCH(?IO_LIB:format("foo: ~p~n", [{bar, tapas}]), "foo: {bar,tapas}\n"),
    ?ASSERT_MATCH(?IO_LIB:format("foo: ~p~n", [{bar, "tapas"}]), "foo: {bar,\"tapas\"}\n"),

    ?ASSERT_MATCH(?IO_LIB:format("~p", [foo]), "foo"),
    ?ASSERT_MATCH(?IO_LIB:format("\t~p", [bar]), "\tbar"),

    ?ASSERT_MATCH(?IO_LIB:format("a ~p ~p of ~p patterns", [small, number, interesting]), "a small number of interesting patterns"),
    ?ASSERT_MATCH(?IO_LIB:format("escape ~~p~n", []), "escape ~p\n"),

    ?ASSERT_FAILURE(?IO_LIB:format("no pattern", [foo]), bad_format),
    ?ASSERT_FAILURE(?IO_LIB:format("too ~p many ~p patterns", [foo]), bad_format),
    ?ASSERT_FAILURE(?IO_LIB:format("not enough ~p patterns", [foo, bar]), bad_format),

    ok.
