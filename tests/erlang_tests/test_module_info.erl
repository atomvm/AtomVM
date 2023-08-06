%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_module_info).

-export([start/0]).

start() ->
    ok = test_module_info0(),
    ok = test_module_info1(),
    0.

test_module_info0() ->
    ModuleInfo = ?MODULE:module_info(),
    ?MODULE = proplists_get_value(module, ModuleInfo),
    CompileList = proplists_get_value(compile, ModuleInfo),
    true = is_list(CompileList),
    AttributesList = proplists_get_value(attributes, ModuleInfo),
    true = is_list(AttributesList),
    Exports = proplists_get_value(exports, ModuleInfo),
    true = lists_member({start, 0}, Exports),
    true = lists_member({module_info, 0}, Exports),
    true = lists_member({module_info, 1}, Exports),
    ok.

test_module_info1() ->
    ?MODULE = ?MODULE:module_info(module),
    CompileList = ?MODULE:module_info(compile),
    true = is_list(CompileList),
    AttributesList = ?MODULE:module_info(attributes),
    true = is_list(AttributesList),
    Exports = ?MODULE:module_info(exports),
    true = lists_member({start, 0}, Exports),
    true = lists_member({module_info, 0}, Exports),
    true = lists_member({module_info, 1}, Exports),
    ok =
        try
            ?MODULE:module_info(unknown_module_info),
            fail
        catch
            error:badarg -> ok
        end,
    ok.

proplists_get_value(_Key, []) -> undefined;
proplists_get_value(Key, [{Key, Value} | _Tail]) -> Value;
proplists_get_value(Key, [_Pair | Tail]) -> proplists_get_value(Key, Tail).

lists_member(_Elem, []) -> false;
lists_member(Elem, [Elem | _Tail]) -> true;
lists_member(Elem, [_Other | Tail]) -> lists_member(Elem, Tail).
