%
% This file is part of AtomVM.
%
% Copyright 2025 Franciszek Kubis <franciszek.kubis@swmansion.com>
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

-module(test_code_get_object_code).

-export([start/0, get_object_wrong_argument/1]).

-include("code_load/export_test_module_data.hrl").

start() ->
    ok = get_object_from_export_test_module(),
    ok = get_object_from_already_loaded_test_module(),
    ok = get_object_from_non_existing_module(),
    ok = ?MODULE:get_object_wrong_argument("a string"),
    ok = ?MODULE:get_object_wrong_argument(123),
    ok = ?MODULE:get_object_wrong_argument({1, "a"}),
    ok = ?MODULE:get_object_wrong_argument([1, b, 3]),
    0.

get_object_from_already_loaded_test_module() ->
    {test_code_get_object_code, Bin, _Filename} = code:get_object_code(?MODULE),
    {module, ?MODULE} = code:load_binary(
        ?MODULE, atom_to_list(?MODULE) ++ ".beam", Bin
    ),
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

get_object_from_export_test_module() ->
    Bin = ?EXPORT_TEST_MODULE_DATA,
    error = code:get_object_code(export_test_module),
    {module, export_test_module} = code:load_binary(
        export_test_module, "export_test_module.beam", Bin
    ),
    {module, export_test_module} = code:ensure_loaded(export_test_module),
    error = code:get_object_code(export_test_module),
    24 = export_test_module:exported_func(4),
    ok.

get_object_from_non_existing_module() ->
    error = code:get_object_code(non_existing_module),
    ok.

get_object_wrong_argument(Argument) ->
    try code:get_object_code(Argument) of
        _ -> not_raised
    catch
        _:_ -> ok
    end.
