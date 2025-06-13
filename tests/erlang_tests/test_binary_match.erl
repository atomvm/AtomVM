%
% This file is part of AtomVM.
%
% Copyright 2025 Jakub Gonet <jakub.gonet@swmansion.com>
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

-module(test_binary_match).

-export([start/0, id/1, fail_with_badarg/1]).
-define(ID(Arg), ?MODULE:id(Arg)).

start() ->
    ok = fail_with_badarg(fun() -> binary:match(?ID(<<"a">>), ?ID(<<"">>)) end),
    {0, 1} = binary:match(?ID(<<"a">>), ?ID(<<"a">>)),
    {0, 1} = binary:match(?ID(<<"aa">>), ?ID(<<"a">>)),
    {0, 2} = binary:match(?ID(<<"aba">>), ?ID(<<"ab">>)),

    % empty subject
    case get_otp_version() of
        OTP when OTP =< 26 ->
            nomatch = binary:match(?ID(<<"">>), ?ID(<<"">>)),
            nomatch = binary:match(?ID(<<"">>), ?ID(<<"a">>)),
            nomatch = binary:match(?ID(<<"">>), ?ID([])),
            % for /3, nomatch only if empty subject + empty options
            nomatch = binary:match(?ID(<<"">>), ?ID(not_binary), ?ID([]));
        _AVM_or_newer_OTP ->
            ok = fail_with_badarg(fun() -> binary:match(?ID(<<"">>), ?ID(<<"">>)) end),
            nomatch = binary:match(?ID(<<"">>), ?ID(<<"a">>)),
            ok = fail_with_badarg(fun() -> binary:match(?ID(<<"">>), ?ID([])) end),
            ok = fail_with_badarg(fun() -> binary:match(?ID(<<"">>), ?ID(not_binary), ?ID([])) end)
    end,

    % list of patterns
    ok = fail_with_badarg(fun() -> binary:match(?ID(<<"a">>), ?ID([])) end),
    ok = fail_with_badarg(fun() -> binary:match(?ID(<<"a">>), ?ID([<<"">>])) end),
    {0, 1} = binary:match(?ID(<<"a">>), ?ID([<<"a">>])),
    {0, 1} = binary:match(?ID(<<"a">>), ?ID([<<"a">>, <<"a">>])),
    {0, 2} = binary:match(?ID(<<"aa">>), ?ID([<<"a">>, <<"aa">>])),

    % scope opt
    nomatch = binary:match(?ID(<<"a">>), ?ID(<<"a">>), ?ID([{scope, {0, 0}}])),
    nomatch = binary:match(?ID(<<"a">>), ?ID(<<"a">>), ?ID([{scope, {1, 0}}])),
    nomatch = binary:match(?ID(<<"bab">>), ?ID(<<"b">>), ?ID([{scope, {1, 0}}])),
    {2, 1} = binary:match(?ID(<<"bab">>), ?ID(<<"b">>), ?ID([{scope, {1, 2}}])),
    % {scope, {1, -1}}: starts at 0, 1 byte long
    {0, 1} = binary:match(?ID(<<"bab">>), ?ID(<<"b">>), ?ID([{scope, {1, -1}}])),

    % bad inputs, subjects must be non-empty to not short-circuit
    ok = fail_with_badarg(fun() -> binary:match(?ID(not_binary), ?ID(<<"a">>)) end),
    ok = fail_with_badarg(fun() -> binary:match(?ID(<<"a">>), ?ID(not_binary)) end),
    ok = fail_with_badarg(fun() -> binary:match(?ID(<<"a">>), ?ID([<<"a">> | <<"a">>])) end),
    ok = fail_with_badarg(fun() -> binary:match(?ID(not_binary), ?ID(<<"a">>), ?ID([])) end),
    ok = fail_with_badarg(fun() -> binary:match(?ID(<<"a">>), ?ID(not_binary), ?ID([])) end),
    ok = fail_with_badarg(fun() -> binary:match(?ID(<<"a">>), ?ID(<<"a">>), ?ID(not_list)) end),

    % bad scope
    ok = fail_with_badarg(fun() ->
        binary:match(?ID(<<"a">>), ?ID(<<"a">>), ?ID([{scope, {0, 2}}]))
    end),
    ok = fail_with_badarg(fun() ->
        binary:match(?ID(<<"a">>), ?ID(<<"a">>), ?ID([{scope, {0, -1}}]))
    end),
    ok = fail_with_badarg(fun() ->
        binary:match(?ID(<<"a">>), ?ID(<<"a">>), ?ID([{scope, {-1, 1}}]))
    end),
    0.

id(X) ->
    X.

fail_with_badarg(Fun) ->
    try Fun() of
        Ret -> {unexpected, Ret}
    catch
        error:badarg -> ok;
        C:E -> {unexpected, C, E}
    end.

get_otp_version() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            list_to_integer(erlang:system_info(otp_release));
        _ ->
            atomvm
    end.
