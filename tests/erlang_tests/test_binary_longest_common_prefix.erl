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

-module(test_binary_longest_common_prefix).

-export([start/0, id/1, fail_with_badarg/1]).
-define(ID(Arg), ?MODULE:id(Arg)).

start() ->
    % Basic cases: two binaries with a common prefix
    2 = binary:longest_common_prefix(?ID([<<"erlang">>, <<"ergonomy">>])),
    0 = binary:longest_common_prefix(?ID([<<"erlang">>, <<"foo">>])),
    6 = binary:longest_common_prefix(?ID([<<"erlang">>, <<"erlang">>])),

    % One binary shorter than prefix
    3 = binary:longest_common_prefix(?ID([<<"foobar">>, <<"foo">>])),
    3 = binary:longest_common_prefix(?ID([<<"foo">>, <<"foobar">>])),

    % Single-element list: returns length of that binary
    6 = binary:longest_common_prefix(?ID([<<"erlang">>])),
    0 = binary:longest_common_prefix(?ID([<<>>])),

    % Empty binaries in the list
    0 = binary:longest_common_prefix(?ID([<<>>, <<"erlang">>])),
    0 = binary:longest_common_prefix(?ID([<<"erlang">>, <<>>])),
    0 = binary:longest_common_prefix(?ID([<<>>, <<>>])),

    % More than two binaries
    2 = binary:longest_common_prefix(?ID([<<"erlang">>, <<"ergonomy">>, <<"ermine">>])),
    0 = binary:longest_common_prefix(?ID([<<"erlang">>, <<"ergonomy">>, <<"foo">>])),
    5 = binary:longest_common_prefix(?ID([<<"hello">>, <<"hello">>, <<"hello">>])),

    % All identical
    4 = binary:longest_common_prefix(?ID([<<"test">>, <<"test">>, <<"test">>])),

    % Prefix is full match for some but not others
    3 = binary:longest_common_prefix(?ID([<<"abc">>, <<"abcdef">>])),
    3 = binary:longest_common_prefix(?ID([<<"abcdef">>, <<"abc">>])),

    % Error cases: empty list is badarg
    ok = fail_with_badarg(fun() -> binary:longest_common_prefix(?ID([])) end),

    % Error cases: not a list
    ok = fail_with_badarg(fun() -> binary:longest_common_prefix(?ID(not_a_list)) end),
    ok = fail_with_badarg(fun() -> binary:longest_common_prefix(?ID(42)) end),

    % Error cases: list contains non-binary
    ok = fail_with_badarg(fun() -> binary:longest_common_prefix(?ID([<<"ok">>, not_a_binary])) end),
    ok = fail_with_badarg(fun() -> binary:longest_common_prefix(?ID([not_a_binary, <<"ok">>])) end),
    ok = fail_with_badarg(fun() -> binary:longest_common_prefix(?ID([42])) end),

    % Error cases: improper list
    ok = fail_with_badarg(fun() -> binary:longest_common_prefix(?ID([<<"a">> | <<"b">>])) end),

    0.

id(X) -> X.

fail_with_badarg(Fun) ->
    try Fun() of
        Ret -> {unexpected, Ret}
    catch
        error:badarg -> ok;
        C:E -> {unexpected, C, E}
    end.
