%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
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

-module(test_display_string).

-export([start/0, id/1]).

start() ->
    true = erlang:display_string(stderr, ?MODULE:id("hello\n")),
    true = erlang:display_string(?MODULE:id("wrapper\n")),
    true = erlang:display_string(stdout, ?MODULE:id("ascii\n")),
    true = erlang:display_string(stdout, ?MODULE:id("漢字")),
    true = erlang:display_string(stdout, ?MODULE:id([16#0100, $\n])),
    true = erlang:display_string(stdout, ?MODULE:id(<<0>>)),
    true = erlang:display_string(stdout, ?MODULE:id(<<"binary\n">>)),
    true = erlang:display_string(stdout, ?MODULE:id(<<230, 188, 162, 229, 173, 151>>)),
    true = erlang:display_string(stderr, ?MODULE:id([])),

    ok = assert_badarg(fun() -> erlang:display_string(standard_io, ?MODULE:id("bad")) end),
    ok = assert_badarg(fun() -> erlang:display_string(stdout, ?MODULE:id(["bad"])) end),
    ok = assert_badarg(fun() -> erlang:display_string(stdout, ?MODULE:id([<<"io">> | "data"])) end),
    ok = assert_badarg(fun() -> erlang:display_string(stdout, ?MODULE:id([16#110000])) end),
    ok = assert_badarg(fun() -> erlang:display_string(stdout, ?MODULE:id([1 | 2])) end),

    0.

assert_badarg(F) ->
    try
        R = F(),
        {fail_no_ex, R}
    catch
        error:badarg -> ok
    end.

id(T) ->
    T.
