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

-module(test_esp_partition).
-export([start/0]).

start() ->
    [
        {<<"nvs">>, 1, 2, 16#9000, 16#6000, []},
        {<<"phy_init">>, 1, 1, 16#f000, 16#1000, []},
        {<<"factory">>, 0, 0, 16#10000, 16#200000, []},
        {<<"lib.avm">>, 1, 1, 16#210000, 16#40000, []},
        {<<"main.avm">>, 1, 1, 16#250000, 16#100000, []}
    ] = esp:partition_list(),
    0.
