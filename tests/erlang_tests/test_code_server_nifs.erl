%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

-module(test_code_server_nifs).

-export([start/0, test_literals/0]).

start() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            OTPRelease = erlang:system_info(otp_release),
            if
                OTPRelease >= "26" ->
                    ok = test_is_loaded();
                true ->
                    ok
            end,
            ok;
        "ATOM" ->
            ok = test_is_loaded(),
            case erlang:system_info(emu_flavor) of
                jit ->
                    ok = test_atom_resolver(),
                    ok = test_literal_resolver(),
                    ok = test_type_resolver(),
                    ok = test_error_cases();
                emu ->
                    ok
            end
    end,
    0.

%% Test code_server:is_loaded/1
test_is_loaded() ->
    M = code_server:is_loaded(?MODULE),
    % On BEAM, this undocumented function returns {file, _Path}, not true
    true = M =/= false,
    false = code_server:is_loaded(non_existent_module_12345),
    ok.

%% Test code_server:atom_resolver/2
test_atom_resolver() ->
    % The first atom (index 1) should be the module name
    ModuleName = code_server:atom_resolver(?MODULE, 1),
    true = is_atom(ModuleName),
    ?MODULE = ModuleName,

    % Test some other atoms that should exist in this module
    Atom2 = code_server:atom_resolver(?MODULE, 2),
    true = is_atom(Atom2),
    start = Atom2,
    ok.

%% Test code_server:literal_resolver/2
test_literal_resolver() ->
    try
        Literal0 = code_server:literal_resolver(?MODULE, 0),
        Literal1 = code_server:literal_resolver(?MODULE, 1),
        true = Literal0 =/= Literal1,
        ok
    catch
        % If there are no literals, that's also acceptable
        error:badarg -> ok
    end,
    ok.

%% Test code_server:type_resolver/2
test_type_resolver() ->
    any = code_server:type_resolver(?MODULE, 0),
    t_atom = code_server:type_resolver(?MODULE, 1),
    true = test_type_resolver0(2),
    ok.

test_type_resolver0(N) ->
    case code_server:type_resolver(?MODULE, N) of
        any -> false;
        % We know N >= 2
        {t_integer, {2, '+inf'}} -> true;
        _Other -> test_type_resolver0(N + 1)
    end.

%% Test error cases
test_error_cases() ->
    % Test with invalid module
    try
        code_server:atom_resolver(non_existent_module, 1),
        error(should_have_failed)
    catch
        error:badarg -> ok
    end,

    try
        code_server:literal_resolver(non_existent_module, 0),
        error(should_have_failed)
    catch
        error:badarg -> ok
    end,

    try
        code_server:type_resolver(non_existent_module, 0),
        error(should_have_failed)
    catch
        error:badarg -> ok
    end,
    ok.

%% Function with literals for testing
test_literals() ->
    List = [1, 2, 3, atom, "string"],
    Tuple = {hello, world, 42, 3.14159},
    Map = #{key => value, number => 123},
    {List, Tuple, Map}.
