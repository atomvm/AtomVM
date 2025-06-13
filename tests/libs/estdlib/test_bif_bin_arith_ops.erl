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
-module(test_bif_bin_arith_ops).

-define(ID(X), ?MODULE:id(X)).
-export([test/0, id/1]).

test() ->
    Ops = ['+', '-', '*', '/'],
    ValidInputs = [[2, 3], [0, 2], [2.0, 3], [2, 3.0], [2.0, 3.0]],
    InvalidInputs = [[not_int, 1], [1, not_int]],
    [{ok, _} = call_op(Name, Args) || Name <- Ops, Args <- ValidInputs],
    [{error, badarith} = call_op(Name, Args) || Name <- Ops, Args <- InvalidInputs],

    {ok, 0} = call_op('div', [2, 3]),
    {ok, 2} = call_op('rem', [2, 3]),
    {error, badarith} = call_op('div', [2.0, 3]),
    {error, badarith} = call_op('div', [2, 3.0]),
    {error, badarith} = call_op('rem', [2.0, 3]),
    {error, badarith} = call_op('rem', [2, 3.0]),
    ok.

call_op(Name, Args) ->
    try apply(erlang, ?ID(Name), ?ID(Args)) of
        V -> {ok, V}
    catch
        C:E -> {C, E}
    end.

id(X) -> X.
