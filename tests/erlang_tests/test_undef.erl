%
% This file is part of AtomVM.
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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

-module(test_undef).

-export([
    start/0, test_undef_apply/1, test_undef_apply_last/1, test_undef_call/1, apply_last/2, call/1
]).

start() ->
    ok = test_undef_apply(test_undef_module),
    ok = test_undef_apply(?MODULE),
    ok = test_undef_apply_last(test_undef_module),
    ok = test_undef_apply_last(?MODULE),
    ok = test_undef_call(test_undef_module),
    ok = test_undef_call(?MODULE),
    0.

test_undef_apply(Module) ->
    % At least with OTP27 compiler, this compiles to:
    % {apply,0}.
    try
        Module:undef_function(),
        {unexpected, ok}
    catch
        error:undef -> ok;
        T:V -> {unexpected, T, V}
    end.

test_undef_apply_last(Module) ->
    try
        apply_last(Module, undef_function),
        {unexpected, ok}
    catch
        error:undef -> ok;
        T:V -> {unexpected, T, V}
    end.

apply_last(Module, Func) ->
    % At least with OTP27 compiler, this compiles to:
    % {apply_last,0,0}.
    Module:Func().

test_undef_call(Module) ->
    try
        call(fun Module:undef_function/0),
        {unexpected, ok}
    catch
        error:undef -> ok;
        T:V -> {unexpected, T, V}
    end.

call(Fun) ->
    % At least with OTP27 compiler, this compiles to:
    % {apply_last,0,0}.
    Fun().
