-module(test_io_lib).

-export([test/0]).

-include("etest.hrl").

test() ->
    ?ASSERT_MATCH(io_lib:format("", []), ""),
    ?ASSERT_MATCH(io_lib:format("foo", []), "foo"),
    ?ASSERT_MATCH(io_lib:format("foo~n", []), "foo\n"),
    ?ASSERT_MATCH(io_lib:format("foo: ~p~n", [bar]), "foo: bar\n"),
    ?ASSERT_MATCH(io_lib:format("foo: ~p~n", ["bar"]), "foo: \"bar\"\n"),
    ?ASSERT_MATCH(io_lib:format("foo: ~s~n", ["bar"]), "foo: bar\n"),
    ?ASSERT_MATCH(io_lib:format("foo: ~p~n", [123]), "foo: 123\n"),
    ?ASSERT_MATCH(io_lib:format("foo: ~p~n", [-123]), "foo: -123\n"),
    ?ASSERT_MATCH(io_lib:format("foo: ~p~n", [[1, 2, 3]]), "foo: [1,2,3]\n"),
    ?ASSERT_MATCH(io_lib:format("foo: ~p~n", [[1, 2, [3]]]), "foo: [1,2,[3]]\n"),
    ?ASSERT_MATCH(io_lib:format("foo: ~p~n", [[65, 116, 111, 109, 86, 77]]), "foo: \"AtomVM\"\n"),
    ?ASSERT_MATCH(io_lib:format("foo: ~s~n", [[65, 116, 111, 109, 86, 77]]), "foo: AtomVM\n"),
    ?ASSERT_MATCH(io_lib:format("foo: ~p~n", [<<"bar">>]), "foo: <<\"bar\">>\n"),
    ?ASSERT_MATCH(io_lib:format("foo: ~p~n", [<<1, 2, 3>>]), "foo: <<1,2,3>>\n"),
    ?ASSERT_MATCH(io_lib:format("foo: ~p~n", [{bar, tapas}]), "foo: {bar,tapas}\n"),
    ?ASSERT_MATCH(io_lib:format("foo: ~p~n", [{bar, "tapas"}]), "foo: {bar,\"tapas\"}\n"),
    ?ASSERT_MATCH(io_lib:format("foo: ~p~n", [#{}]), "foo: #{}\n"),
    ?ASSERT_MATCH(io_lib:format("foo: ~p~n", [#{a => 1}]), "foo: #{a => 1}\n"),
    ?ASSERT_MATCH(io_lib:format("foo: ~p", [#{a => 1, b => 2}]), "foo: #{a => 1,b => 2}"),
    ?ASSERT_MATCH(io_lib:format("foo: ~p", [#{b => 2, a => 1}]), "foo: #{a => 1,b => 2}"),
    ?ASSERT_MATCH(io_lib:format("foo: ~p", [#{{x, y} => z}]), "foo: #{{x,y} => z}"),
    ?ASSERT_MATCH(io_lib:format("foo: ~p", [#{"foo" => "bar"}]), "foo: #{\"foo\" => \"bar\"}"),

    ?ASSERT_MATCH(io_lib:format("~p", [foo]), "foo"),
    ?ASSERT_MATCH(io_lib:format("\t~p", [bar]), "\tbar"),

    ?ASSERT_MATCH(io_lib:format("a ~p ~p of ~p patterns", [small, number, interesting]), "a small number of interesting patterns"),
    ?ASSERT_MATCH(io_lib:format("escape ~~p~n", []), "escape ~p\n"),

    ?ASSERT_FAILURE(io_lib:format("no pattern", [foo]), bad_format),
    ?ASSERT_FAILURE(io_lib:format("too ~p many ~p patterns", [foo]), bad_format),
    ?ASSERT_FAILURE(io_lib:format("not enough ~p patterns", [foo, bar]), bad_format),

    ok.
