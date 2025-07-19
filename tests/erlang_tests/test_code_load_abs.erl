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

-module(test_code_load_abs).

-export([start/0]).

start() ->
    Path =
        case erlang:system_info(emu_flavor) of
            emu -> "code_load/export_test_module";
            jit -> "../code_load/x86_64/export_test_module"
        end,
    {module, export_test_module} = code:load_abs(Path),
    export_test_module:exported_func(4).
