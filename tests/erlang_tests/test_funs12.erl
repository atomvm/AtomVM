%
% This file is part of AtomVM.
%
% Copyright 2025 Davide Bettio <davide@uninstall.it>
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

-module(test_funs12).
-export([start/0, check_guard/3, check_bool/3, discard/1, get_fun/0]).

start() ->
    CheckGuardFun = fun ?MODULE:check_guard/3,
    SelfFun = fun erlang:self/0,
    {A, F} = ?MODULE:get_fun(),
    true = ?MODULE:check_guard(3, CheckGuardFun, 3),
    false = ?MODULE:check_guard([], CheckGuardFun, []),
    true = ?MODULE:check_bool(SelfFun, F, 1),
    false = ?MODULE:check_bool([], {}, 1),
    ok = expect_error(fun() -> ?MODULE:check_bool({}, [], not_integer) end, error, badarg),
    ok = expect_error(fun() -> ?MODULE:check_bool(SelfFun, F, not_integer) end, error, badarg),
    ok = expect_error(fun() -> ?MODULE:check_bool(SelfFun, F, -20) end, error, badarg),
    1 = ?MODULE:check_guard(A, F, A),
    ?MODULE:check_guard(0, SelfFun, 0).

check_guard(A, B, A) when A > 1 andalso is_function(B, A) ->
    discard(B);
check_guard(A, B, A) when is_function(B, A) ->
    A;
check_guard(A, _B, A) when A < 0 ->
    error;
check_guard(_, _, _) ->
    false.

discard(_X) ->
    true.

check_bool(A, B, C) ->
    is_function(A, C) or is_function(B, C).

get_fun() ->
    {1, fun(X) -> X + 1 end}.

expect_error(Fun, A, B) ->
    try Fun() of
        Result -> {error, Result}
    catch
        A:B -> ok
    end.
