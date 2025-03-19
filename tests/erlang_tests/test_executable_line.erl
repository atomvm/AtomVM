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

-module(test_executable_line).
-compile([line_coverage]).
-export([start/0]).

start() ->
    ok = test_executable_line(),
    0.

test_executable_line() ->
    ModuleInfo = ?MODULE:module_info(),
    CompileList = proplists_get_value(compile, ModuleInfo),
    true = is_list(CompileList),
    OptionsList = proplists_get_value(options, CompileList),
    case erlang:system_info(machine) of
        "BEAM" ->
            case erlang:system_info(otp_release) >= "27" of
                true ->
                    [line_coverage] = OptionsList;
                false ->
                    ok
            end;
        _ ->
            ok
    end,
    ok.

proplists_get_value(_Key, []) -> undefined;
proplists_get_value(Key, [{Key, Value} | _Tail]) -> Value;
proplists_get_value(Key, [_Pair | Tail]) -> proplists_get_value(Key, Tail).
