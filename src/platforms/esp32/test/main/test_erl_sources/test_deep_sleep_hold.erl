%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
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

-module(test_deep_sleep_hold).
-export([start/0]).

start() ->
    Sysinfo = erlang:system_info(esp32_chip_info),
    Model =
        if
            is_map(Sysinfo) -> maps:get(model, Sysinfo);
            true -> undefined
        end,
    case Model of
        % esp32_p4 has no support on earlier revisions, and thus left out here.
        M when M =:= esp32; M =:= esp32_s2; M =:= esp32_c3; M =:= esp32_s3; M =:= esp32_c2 ->
            io:format("Testing: deep_sleep_hold_en/dis\n"),
            ok = gpio:deep_sleep_hold_en(),
            ok = gpio:deep_sleep_hold_dis();
        _ ->
            io:format("Not testing: deep_sleep_hold_en/dis\n"),
            ok
    end,
    ok.
