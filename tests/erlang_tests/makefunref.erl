%
% This file is part of AtomVM.
%
% Copyright 2020 Davide Bettio <davide@uninstall.it>
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

-module(makefunref).

-export([start/0, mfa2fun/3, id/1]).

start() ->
    ok = test_make_fun_boundaries(),
    F = mfa2fun(erlang, integer_to_list, id(0)),
    L = F(id(123)),
    id(length(L)).

mfa2fun(Module, FunctionName, TheArity) ->
    Arity = TheArity + 1,
    fun Module:FunctionName/Arity.

test_make_fun_boundaries() ->
    [
        begin
            Fun = erlang:make_fun(?MODULE, missing, Arity),
            true = is_function(Fun, Arity),
            true = query_arity(Fun, Arity),
            {arity, Arity} = erlang:fun_info(Fun, arity),
            false = query_arity(Fun, 254),
            Closure = fun(X) -> {Fun, X} end,
            true = query_arity(Closure, 1),
            false = query_arity(Closure, 0),
            false = query_arity(Fun, (1 bsl 32) + Arity),
            false = query_arity(Closure, (1 bsl 32) + 1)
        end
     || Arity <- [0, 1, 255]
    ],
    [
        badarg =
            try
                erlang:make_fun(?MODULE, missing, Arity)
            catch
                error:R -> R
            end
     || Arity <- [-1, 256, 1 bsl 100, 1.0, bad]
    ],
    ok.

query_arity(Fun, Arity) ->
    Result = is_function(Fun, Arity),
    Result = (guard_arity(Fun, Arity) =:= matched).

%% Distinct atoms keep OTP from rewriting this guard as the is_function/2 BIF.
guard_arity(Fun, Arity) when is_integer(Arity), Arity >= 0, is_function(Fun, Arity) -> matched;
guard_arity(_, _) -> rejected.

id(X) when is_integer(X) ->
    X * 1.
