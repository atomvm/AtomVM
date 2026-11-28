%
% This file is part of AtomVM.
%
% Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
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

-module(interpreted_calls).

-export([run/1]).

run(Parent) ->
    % In mixed-mode builds this caller is raw BEAM and the target is native.
    % Check all three external call shapes and that each returns to the
    % correct continuation. Interpreter-only and JIT-only builds are controls.
    24 = tail(4),
    {wrapped, 120} = non_tail(5),
    720 = framed_tail(3),
    Reason = {reason, 23},
    ok = check_exception(arithmetic, Reason, {error, badarith}),
    ok = check_exception(error, Reason, {error, Reason}),
    ok = check_exception(throw, Reason, {throw, Reason}),
    ok = check_exception(exit, Reason, {exit, Reason}),
    Parent ! {self(), ok}.

tail(N) ->
    export_test_module:exported_func(N).

non_tail(N) ->
    {wrapped, export_test_module:exported_func(N)}.

framed_tail(N) ->
    Value = export_test_module:exported_func(N),
    export_test_module:exported_func(Value).

check_exception(Kind, Value, Expected) ->
    % Catching inside native code is a control. The same exception must also
    % reach the bytecode catch, survive re-raising, and leave its frame usable.
    Expected = export_test_module:catch_exception(Kind, Value),
    Result =
        try
            try
                export_test_module:raise_exception(Kind, Value)
            catch
                InnerClass:InnerReason:InnerStack ->
                    ok = check_stacktrace(InnerStack),
                    erlang:raise(InnerClass, InnerReason, InnerStack)
            end
        of
            Unexpected -> {unexpected, Unexpected}
        catch
            Class:Reason:Stack ->
                ok = check_stacktrace(Stack),
                {Class, Reason}
        end,
    Expected = Result,
    24 = tail(4),
    ok.

check_stacktrace(undefined) ->
    % AVM_CREATE_STACKTRACES=OFF
    ok;
check_stacktrace([{export_test_module, raise_exception, 2, _} | _]) ->
    ok;
check_stacktrace([_ | Rest]) ->
    check_stacktrace(Rest).
