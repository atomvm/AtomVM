%
% This file is part of AtomVM.
%
% Copyright 2019-2021 Fred Dushin <fred@dushin.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_io_lib).

-export([test/0]).

-include("etest.hrl").

-define(FLT(L), lists:flatten(L)).

test() ->
    ?ASSERT_MATCH(?FLT(io_lib:format("", [])), ""),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo", [])), "foo"),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo~n", [])), "foo\n"),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo: ~p~n", [bar])), "foo: bar\n"),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo: ~p~n", ["bar"])), "foo: \"bar\"\n"),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo: ~s~n", ["bar"])), "foo: bar\n"),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo: ~p~n", [123])), "foo: 123\n"),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo: ~p~n", [-123])), "foo: -123\n"),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo: ~p~n", [[1, 2, 3]])), "foo: [1,2,3]\n"),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo: ~p~n", [[1, 2, [3]]])), "foo: [1,2,[3]]\n"),
    ?ASSERT_MATCH(
        ?FLT(io_lib:format("foo: ~p~n", [[65, 116, 111, 109, 86, 77]])), "foo: \"AtomVM\"\n"
    ),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo: ~s~n", [[65, 116, 111, 109, 86, 77]])), "foo: AtomVM\n"),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo: ~p~n", [<<"bar">>])), "foo: <<\"bar\">>\n"),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo: ~p~n", [<<1, 2, 3>>])), "foo: <<1,2,3>>\n"),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo: ~p~n", [{bar, tapas}])), "foo: {bar,tapas}\n"),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo: ~p~n", [{bar, "tapas"}])), "foo: {bar,\"tapas\"}\n"),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo: ~p~n", [#{}])), "foo: #{}\n"),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo: ~p~n", [#{a => 1}])), "foo: #{a => 1}\n"),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo: ~p", [#{a => 1, b => 2}])), "foo: #{a => 1,b => 2}"),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo: ~p", [#{b => 2, a => 1}])), "foo: #{a => 1,b => 2}"),
    ?ASSERT_MATCH(?FLT(io_lib:format("foo: ~p", [#{{x, y} => z}])), "foo: #{{x,y} => z}"),
    ?ASSERT_MATCH(
        ?FLT(io_lib:format("foo: ~p", [#{"foo" => "bar"}])), "foo: #{\"foo\" => \"bar\"}"
    ),

    ?ASSERT_MATCH(?FLT(io_lib:format("~p", [foo])), "foo"),
    ?ASSERT_MATCH(?FLT(io_lib:format("\t~p", [bar])), "\tbar"),

    ?ASSERT_MATCH(
        ?FLT(io_lib:format("a ~p ~p of ~p patterns", [small, number, interesting])),
        "a small number of interesting patterns"
    ),
    ?ASSERT_MATCH(?FLT(io_lib:format("escape ~~p~n", [])), "escape ~p\n"),

    ?ASSERT_FAILURE(io_lib:format("no pattern", id([foo])), badarg),
    ?ASSERT_FAILURE(io_lib:format("too ~p many ~p patterns", id([foo])), badarg),
    ?ASSERT_FAILURE(io_lib:format("not enough ~p patterns", id([foo, bar])), badarg),

    ok.

id(X) ->
    X.
