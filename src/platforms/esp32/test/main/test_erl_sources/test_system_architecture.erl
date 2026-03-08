%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M. <petermm@gmail.com>
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

-module(test_system_architecture).
-export([start/0]).

start() ->
    SystemArchitecture = erlang:system_info(system_architecture),
    ok =
        case SystemArchitecture of
            <<"xtensa-esp-esp_idf">> ->
                ok;
            <<"riscv32-esp-esp_idf">> ->
                ok
        end,
    nomatch = binary:match(SystemArchitecture, <<"esp_idf-">>),
    ok.
