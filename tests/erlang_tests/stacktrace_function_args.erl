%
% This file is part of AtomVM.
%
% Copyright 2026 Davide Bettio <davide@uninstall.it>
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

% Do not move code
% Line numbers are relevant for our tests

-module(stacktrace_function_args).

-export([start/0, id/1]).

f(N) when is_integer(N) ->
    N + 1.

func_info_stacktrace() ->
    try f(?MODULE:id(noninteger1)) of
        X -> X
    catch
        Class:Reason:Stacktrace -> {info, Class, Reason, Stacktrace}
    end.

id(X) ->
    X.

% Only relative line numbers (using ?LINE) from here

start() ->
    {info, error, function_clause, [
        {stacktrace_function_args, f, [noninteger1], [
            {file, ?FILE}, {line, 28}
        ]},
        {stacktrace_function_args, func_info_stacktrace, 0, [
            {file, ?FILE}, {line, 32}
        ]},
        {stacktrace_function_args, start, 0, [
            {file, ?FILE}, {line, ?LINE + 3}
        ]}
        | Rest
    ]} = func_info_stacktrace(),

    case erlang:system_info(machine) of
        "BEAM" -> true = is_list(Rest);
        "ATOM" -> [] = Rest
    end,

    0.
