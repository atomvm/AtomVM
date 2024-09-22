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

-module(test_code_ensure_loaded).

-export([start/0]).

start() ->
    test_self_module(),
    test_code(),
    test_non_existing_module(),
    test_badarg(),
    0.

test_self_module() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

test_code() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            {module, code} = code:ensure_loaded(code),
            ok;
        "ATOM" ->
            % This isn't supported for now as this test is ran without the
            % Erlang module being loaded.
            ok
    end,
    ok.

test_non_existing_module() ->
    {error, _} = code:ensure_loaded(non_existing_module),
    ok.

test_badarg() ->
    ok =
        try
            code:ensure_loaded("non_existing_module"),
            failure
        catch
            _:_ -> ok
        end.
