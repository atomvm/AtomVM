%
% This file is part of AtomVM.
%
% Copyright 2023 Davide Bettio <davide@uninstall.it>
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

-module(test_add_avm_pack_file).

-export([start/0]).

-ifdef(AVM_DISABLE_JIT).
path() ->
    "code_load/code_load_pack.avm".
-else.
path() ->
    "../code_load/code_load_pack-" ++ atom_to_list(?AVM_JIT_TARGET_ARCH) ++ ".avm".
-endif.

start() ->
    AVM = path(),
    erlang:display(atomvm:add_avm_pack_file(AVM, [])),
    export_test_module:exported_func(4).
