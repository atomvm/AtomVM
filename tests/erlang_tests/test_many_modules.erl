%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

-module(test_many_modules).

-export([start/0]).

-include("code_load/export_test_module_data.hrl").

-define(MODULE_COUNT, 300).
-define(BASE_NAME, <<"export_test_module">>).

start() ->
    Base = ?EXPORT_TEST_MODULE_DATA,
    %% Load MODULE_COUNT (> 256) distinct copies of the same module under
    %% different names and call each one.
    ok = load_and_call_all(Base, 1),
    ok = catch_all(1),
    ok = trace_all(1),
    0.

%% 18-char name matching the length of "export_test_module":
%% "export_test_md_" (15 chars) ++ 3-digit zero-padded index (1..999).
mod_name(I) when I < 10 ->
    list_to_atom("export_test_md_00" ++ integer_to_list(I));
mod_name(I) when I < 100 ->
    list_to_atom("export_test_md_0" ++ integer_to_list(I));
mod_name(I) ->
    list_to_atom("export_test_md_" ++ integer_to_list(I)).

load_and_call_all(_Base, I) when I > ?MODULE_COUNT ->
    ok;
load_and_call_all(Base, I) ->
    Name = mod_name(I),
    NameBin = atom_to_binary(Name, latin1),
    Bin = binary:replace(Base, ?BASE_NAME, NameBin, [global]),
    {module, Name} = code:load_binary(Name, atom_to_list(Name) ++ ".beam", Bin),
    24 = Name:exported_func(4),
    load_and_call_all(Base, I + 1).

catch_all(I) when I > ?MODULE_COUNT ->
    ok;
catch_all(I) ->
    Catcher = mod_name(I),
    Raiser = mod_name(?MODULE_COUNT + 1 - I),
    {Catcher, Raiser, 8} = Catcher:catching_func(Raiser, 2),
    catch_all(I + 1).

trace_all(I) when I > ?MODULE_COUNT ->
    ok;
trace_all(I) ->
    Catcher = mod_name(I),
    Raiser = mod_name(?MODULE_COUNT + 1 - I),
    case Catcher:tracing_func(Raiser) of
        {Catcher, Raiser, undefined} ->
            %% Built without AVM_CREATE_STACKTRACES, so stacktrace_build/3
            %% yields 'undefined'. The cross-module raise is still exercised.
            ok;
        {Catcher, Raiser, Stacktrace} ->
            Modules = [M || {M, _F, _A, _L} <- Stacktrace],
            true = has_module(Modules, Catcher),
            true = has_module(Modules, Raiser),
            [] = [M || M <- Modules, is_loaded_copy(M), M =/= Catcher, M =/= Raiser]
    end,
    trace_all(I + 1).

has_module([], _Module) -> false;
has_module([Module | _T], Module) -> true;
has_module([_H | T], Module) -> has_module(T, Module).

is_loaded_copy(Module) ->
    is_loaded_copy0(atom_to_list(Module)).

is_loaded_copy0("export_test_md_" ++ _Rest) -> true;
is_loaded_copy0(_Other) -> false.
